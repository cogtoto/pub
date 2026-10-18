#!/usr/bin/env python3
"""A two-layer MNIST net, from scratch, standard library only.

This file is the PRD theory in code: 784 pixels -> 32 hidden neurons (ReLU)
-> 10 digit scores (sigmoid). No NumPy, no ML libraries, no biases.

Run:
    python3 two_layer_mnist.py           # train, save, then draw
    python3 two_layer_mnist.py --train   # train and save weights
    python3 two_layer_mnist.py --run     # load weights and draw

Training prints the loss after each epoch. Run mode does not. For one
test image of each digit it draws the picture, then layer 2 (ten
sigmoid bars). Foot gets sixel pictures. At the end it counts how many
test guesses match the true digit. Accuracy is not a goal.
"""

from __future__ import annotations

import gzip
import json
import math
import os
import random
import shutil
import sys

# --- shapes locked in the PRD (so X · W¹ and LAYER¹ · W² are valid) ---
N_PIXELS = 784  # 28×28 flattened
N_HIDDEN = 32  # layer 1
N_OUTPUT = 10  # layer 2, one score per digit 0..9
BATCH = 150  # observations per training step (X is 150 × 784)
EPOCHS = 80  # the test count was still rising at 50 epochs
ETA = 1.0  # learning rate η
SUBSET_NAME = "mnist_subset.json.gz"
WEIGHTS_NAME = "mnist_weights.json.gz"


# ---------------------------------------------------------------------------
# Tiny matrix helpers (lists of lists). Named after the PRD symbols.
# ---------------------------------------------------------------------------


def matmul(A, B):
    """A · B. A is m×n, B is n×p, result is m×p."""
    n = len(A[0])
    p = len(B[0])
    out = []
    for Ai in A:
        row = [0.0] * p
        for k in range(n):
            aik = Ai[k]
            Bk = B[k]
            for j in range(p):
                row[j] += aik * Bk[j]
        out.append(row)
    return out


def transpose(A):
    """Aᵀ."""
    return [list(col) for col in zip(*A)]


def hadamard(A, B):
    """A ⊙ B (element-wise multiply)."""
    return [[a * b for a, b in zip(ra, rb)] for ra, rb in zip(A, B)]


def random_matrix(rows, cols, scale):
    """Small Gaussian weights. Random start; training will nudge them."""
    return [[random.gauss(0.0, scale) for _ in range(cols)] for _ in range(rows)]


# ---------------------------------------------------------------------------
# Activations
# ---------------------------------------------------------------------------


def relu(Z):
    """ReLU(z) = max(0, z). Hidden layer: negatives off, positives through."""
    return [[z if z > 0.0 else 0.0 for z in row] for row in Z]


def relu_prime(Z):
    """ReLU′(z) = 1 if z > 0 else 0."""
    return [[1.0 if z > 0.0 else 0.0 for z in row] for row in Z]


def _sigmoid_one(z):
    """sigmoid(z) = 1 / (1 + e^(−z)), written stably for large |z|."""
    if z >= 0.0:
        return 1.0 / (1.0 + math.exp(-z))
    ez = math.exp(z)
    return ez / (1.0 + ez)


def sigmoid(Z):
    """Squash each output sum into (0, 1). Not softmax — scores need not sum to 1."""
    return [[_sigmoid_one(z) for z in row] for row in Z]


def sigmoid_prime_from_sigma(S):
    """sigmoid′(z) = σ(z) · (1 − σ(z)). S is already σ(Z)."""
    return [[s * (1.0 - s) for s in row] for row in S]


def argmax(row):
    """Guess = column with the largest score."""
    best_i = 0
    best = row[0]
    for i, v in enumerate(row):
        if v > best:
            best, best_i = v, i
    return best_i


# ---------------------------------------------------------------------------
# Forward, loss, backward, SGD — the teaching core
# ---------------------------------------------------------------------------


def forward(X, W1, W2):
    """One guess for a batch.

    X → ⊗ W¹ → Z¹ → ReLU → LAYER¹ → ⊗ W² → Z² → sigmoid → Ŷ
    """
    Z1 = matmul(X, W1)  # Z¹ = X · W¹          (batch × 32)
    layer1 = relu(Z1)  # LAYER¹ = ReLU(Z¹)     (batch × 32)
    Z2 = matmul(layer1, W2)  # Z² = LAYER¹ · W²    (batch × 10)
    Yhat = sigmoid(Z2)  # Ŷ = sigmoid(Z²)       (batch × 10)
    return Z1, layer1, Z2, Yhat


def mse_sum(Yhat, Y):
    """LOSS = (Ŷ − Y)² summed over the batch (every score vs every one-hot entry)."""
    total = 0.0
    for pred_row, true_row in zip(Yhat, Y):
        for pred, true in zip(pred_row, true_row):
            err = pred - true
            total += err * err
    return total


def backward(X, Y, Z1, layer1, Yhat, W2):
    """Send the output error back and get ∂LOSS/∂W¹ and ∂LOSS/∂W²."""
    # E = 2(Ŷ − Y) ⊙ sigmoid′(Z²)
    two_diff = [[2.0 * (pred - true) for pred, true in zip(pr, tr)] for pr, tr in zip(Yhat, Y)]
    E = hadamard(two_diff, sigmoid_prime_from_sigma(Yhat))

    # ∂LOSS/∂W² = (LAYER¹)ᵀ · E
    # "for each of the 32 hidden values, how much did it push each of the 10 output errors?"
    dW2 = matmul(transpose(layer1), E)

    # ∂LOSS/∂W¹ = Xᵀ · ((E · (W²)ᵀ) ⊙ ReLU′(Z¹))
    # "for each of the 784 pixels, how much did it push each of the 32 hidden neurons?"
    hidden_error = hadamard(matmul(E, transpose(W2)), relu_prime(Z1))
    dW1 = matmul(transpose(X), hidden_error)
    return dW1, dW2


def sgd(W1, W2, dW1, dW2, eta):
    """W ← W − η · ∂LOSS/∂W. No momentum, Adam, or regularization."""
    W1 = [[w - eta * g for w, g in zip(rw, rg)] for rw, rg in zip(W1, dW1)]
    W2 = [[w - eta * g for w, g in zip(rw, rg)] for rw, rg in zip(W2, dW2)]
    return W1, W2


# ---------------------------------------------------------------------------
# Data: small MNIST subset on disk (not the full 60k/10k)
# ---------------------------------------------------------------------------


def one_hot(labels):
    """Digit 3 → [0,0,0,1,0,0,0,0,0,0]."""
    rows = []
    for lab in labels:
        row = [0.0] * N_OUTPUT
        row[lab] = 1.0
        rows.append(row)
    return rows


def load_subset(path):
    """Pixels in [0, 1], one row of 784 per image. Pure Python JSON/gzip."""
    with gzip.open(path, "rt", encoding="utf-8") as f:
        data = json.load(f)
    def pack(split):
        images = [[px / 255.0 for px in row] for row in split["pixels"]]
        labels = list(split["labels"])
        return images, labels
    train_x, train_y = pack(data["train"])
    test_x, test_y = pack(data["test"])
    return train_x, train_y, test_x, test_y


def batches(X, Y, size):
    """Walk the training set in chunks of 150 (last chunk may be shorter)."""
    for start in range(0, len(X), size):
        yield X[start : start + size], Y[start : start + size]


# Pictures are RGB buffers, row-major. Foot draws them as sixel.
_BG = (14, 17, 24)
_INK = (236, 232, 224)
_VIOLET = (167, 139, 250)
_CORAL = (251, 146, 60)
_TRACK = (42, 48, 64)
_MUTED = (120, 138, 160)

# 5×7 glyphs, so the bar chart can label digits without falling back to text art.
_GLYPH = {
    "0": ("01110", "10001", "10001", "10001", "10001", "10001", "01110"),
    "1": ("00100", "01100", "00100", "00100", "00100", "00100", "01110"),
    "2": ("01110", "10001", "00001", "00010", "00100", "01000", "11111"),
    "3": ("11110", "00001", "00001", "01110", "00001", "00001", "11110"),
    "4": ("00010", "00110", "01010", "10010", "11111", "00010", "00010"),
    "5": ("11111", "10000", "11110", "00001", "00001", "10001", "01110"),
    "6": ("00110", "01000", "10000", "11110", "10001", "10001", "01110"),
    "7": ("11111", "00001", "00010", "00100", "01000", "01000", "01000"),
    "8": ("01110", "10001", "10001", "01110", "10001", "10001", "01110"),
    "9": ("01110", "10001", "10001", "01111", "00001", "00010", "01100"),
    ".": ("00000", "00000", "00000", "00000", "00000", "01100", "01100"),
    "*": ("00100", "10101", "01110", "11111", "01110", "10101", "00100"),
}


def _clamp01(t):
    if t < 0.0:
        return 0.0
    if t > 1.0:
        return 1.0
    return t


def _blank(width, height, color):
    return [color] * (width * height)


def _fill_rect(img, width, height, x0, y0, w, h, color):
    x1 = min(width, x0 + w)
    y1 = min(height, y0 + h)
    x0 = max(0, x0)
    y0 = max(0, y0)
    for y in range(y0, y1):
        row = y * width
        for x in range(x0, x1):
            img[row + x] = color


def _blit(img, width, height, x, y, glyph, color, scale):
    for gy, row in enumerate(glyph):
        for gx, bit in enumerate(row):
            if bit != "1":
                continue
            _fill_rect(img, width, height, x + gx * scale, y + gy * scale, scale, scale, color)


def _in_foot():
    """Foot draws sixel even when TERM is xterm-256color."""
    pid = os.getpid()
    seen = set()
    while pid and pid not in seen:
        seen.add(pid)
        try:
            comm = open(f"/proc/{pid}/comm", encoding="utf-8").read().strip()
            stat = open(f"/proc/{pid}/stat", encoding="utf-8").read()
        except OSError:
            return False
        if comm == "foot":
            return True
        try:
            ppid = int(stat[stat.rfind(")") + 2 :].split()[1])
        except (ValueError, IndexError):
            return False
        pid = ppid
    return False


def _sixel_rgb(width, height, pixels):
    """An RGB picture as sixel. Colors are quantized so the palette stays small."""
    palette = []
    index = {}

    def pid(color):
        key = tuple((c // 16) * 16 for c in color)
        slot = index.get(key)
        if slot is None:
            slot = len(palette)
            index[key] = slot
            palette.append(key)
        return slot

    ids = [pid(px) for px in pixels]
    parts = [f'\033P0;1;0q"1;1;{width};{height}']
    for i, (r, g, b) in enumerate(palette):
        parts.append(f"#{i};2;{r * 100 // 255};{g * 100 // 255};{b * 100 // 255}")
    for y in range(0, height, 6):
        used = []
        for color_i in range(len(palette)):
            runs = []
            any_bit = False
            for x in range(width):
                bits = 0
                for bit in range(6):
                    yy = y + bit
                    if yy < height and ids[yy * width + x] == color_i:
                        bits |= 1 << bit
                        any_bit = True
                runs.append(chr(63 + bits))
            if any_bit:
                used.append((color_i, "".join(runs)))
        for color_i, runs in used:
            parts.append(f"#{color_i}{runs}$")
        parts.append("-")
    parts.append("\033\\")
    return "".join(parts)


def _print_pixels(width, height, pixels):
    """Same picture, two pixel rows per line, when the terminal is not foot."""
    cols = max(8, shutil.get_terminal_size((80, 24)).columns - 2)
    new_w = min(width, cols)
    new_h = max(2, int(round(height * new_w / width)))
    if new_h % 2:
        new_h += 1
    lines = []
    for y in range(0, new_h, 2):
        chars = []
        for x in range(new_w):
            sx = min(width - 1, int(x * width / new_w))
            sy0 = min(height - 1, int(y * height / new_h))
            sy1 = min(height - 1, int((y + 1) * height / new_h))
            top = pixels[sy0 * width + sx]
            bot = pixels[sy1 * width + sx]
            chars.append(
                f"\033[38;2;{top[0]};{top[1]};{top[2]}m"
                f"\033[48;2;{bot[0]};{bot[1]};{bot[2]}m▀"
            )
        lines.append("".join(chars) + "\033[0m")
    print("\n".join(lines))


def show_image(width, height, pixels):
    """Draw one RGB picture. Foot gets sixel; another terminal gets the pixels."""
    if not sys.stdout.isatty():
        return
    if _in_foot():
        sys.stdout.write(_sixel_rgb(width, height, pixels))
        sys.stdout.write("\n")
    else:
        _print_pixels(width, height, pixels)


def _digit_image(pixels, scale=8):
    """Deflatten the 784-vector into a 28×28 picture, then scale each pixel up."""
    side = 28 * scale
    img = _blank(side, side, _BG)
    for y in range(side):
        src_y = y // scale
        row = y * side
        for x in range(side):
            v = pixels[src_y * 28 + (x // scale)]
            t = _clamp01(v)
            ink = int(t * 255 + 0.5)
            img[row + x] = (ink, ink, ink)
    return side, side, img


def show_digit(pixels):
    """The test picture, as an image."""
    show_image(*_digit_image(pixels))


def _layer2_image(scores, guess, truth):
    """Ten sigmoid bars as a picture. A star marks the guess.

    Bar length is the score itself (0 to 1). The guess bar is violet.
    The true digit's label, and its bar when it is not the guess, is coral.
    """
    scale = 2
    row_h, gap = 22, 8
    bar_w = 200
    star_w, label_w, score_w = 16, 16, 52
    pad = 4
    width = pad + star_w + label_w + 8 + bar_w + 10 + score_w + pad
    height = 10 * row_h + 9 * gap
    img = _blank(width, height, _BG)
    for digit, score in enumerate(scores):
        t = _clamp01(score)
        y = digit * (row_h + gap)
        is_guess = digit == guess
        is_true = digit == truth
        if is_guess:
            bar_color = _VIOLET
        elif is_true:
            bar_color = _CORAL
        else:
            bar_color = _MUTED
        label_color = _CORAL if is_true else _INK
        x = pad
        if is_guess:
            _blit(img, width, height, x, y + 4, _GLYPH["*"], _VIOLET, scale)
        x += star_w
        _blit(img, width, height, x, y + 4, _GLYPH[str(digit)], label_color, scale)
        x += label_w + 8
        _fill_rect(img, width, height, x, y + 4, bar_w, 14, _TRACK)
        _fill_rect(img, width, height, x, y + 4, int(t * bar_w + 0.5), 14, bar_color)
        x += bar_w + 10
        text = f"{t:.2f}"
        for ch in text:
            _blit(img, width, height, x, y + 4, _GLYPH[ch], _INK, scale)
            x += 6 * scale
    return width, height, img


def show_layer2(scores, guess, truth):
    """Draw the ten sigmoid scores for one image."""
    print("layer 2   10 sigmoid scores    * guess")
    show_image(*_layer2_image(scores, guess, truth))


def save_weights(path, W1, W2):
    """mnist_weights.json.gz — so run mode can skip training."""
    with gzip.open(path, "wt", encoding="utf-8") as f:
        json.dump({"W1": W1, "W2": W2}, f)


def load_weights(path):
    with gzip.open(path, "rt", encoding="utf-8") as f:
        payload = json.load(f)
    return payload["W1"], payload["W2"]


# ---------------------------------------------------------------------------
# Train by running this file, then draw a few digits and layer 2
# ---------------------------------------------------------------------------


def train(train_x, train_y):
    """Train for EPOCHS. Print the summed loss after each one. Return W¹, W²."""
    train_Y = one_hot(train_y)
    # Random knobs. Scale ~ 1/√fan-in keeps the first sums from exploding.
    W1 = random_matrix(N_PIXELS, N_HIDDEN, 1.0 / math.sqrt(N_PIXELS))
    W2 = random_matrix(N_HIDDEN, N_OUTPUT, 1.0 / math.sqrt(N_HIDDEN))

    n_train = len(train_x)
    order = list(range(n_train))

    for epoch in range(1, EPOCHS + 1):
        random.shuffle(order)
        X = [train_x[i] for i in order]
        Y = [train_Y[i] for i in order]

        epoch_loss = 0.0
        n_seen = 0
        for xb, yb in batches(X, Y, BATCH):
            # forward: make a guess
            Z1, layer1, _Z2, Yhat = forward(xb, W1, W2)
            epoch_loss += mse_sum(Yhat, yb)
            n_seen += len(xb)
            # backward: measure error, send it back, nudge every weight
            dW1, dW2 = backward(xb, yb, Z1, layer1, Yhat, W2)
            # The PRD writes ∂LOSS/∂W from *summed* MSE. That sum over a 150×10
            # batch is far too big for η = 1 (sigmoid saturates to 0 in one
            # step). Divide by batch×10 so the update is mean MSE, η = 1.
            # Printed loss stays the summed number so you can watch it move.
            scale = len(xb) * N_OUTPUT
            dW1 = [[g / scale for g in row] for row in dW1]
            dW2 = [[g / scale for g in row] for row in dW2]
            W1, W2 = sgd(W1, W2, dW1, dW2, ETA)

        # one number per epoch so you can watch the loss move (no accuracy %)
        mean_loss = epoch_loss / n_seen
        print(f"epoch {epoch:2d}/{EPOCHS}  loss {epoch_loss:.1f}  (mean per image {mean_loss:.4f})")

    return W1, W2


def print_guess_counts(test_x, test_y, W1, W2):
    """How many test guesses match the true digit, and which digit the misses became."""
    _Z1, _layer1, _Z2, Yhat = forward(test_x, W1, W2)
    right = [0] * N_OUTPUT
    total = [0] * N_OUTPUT
    wrong = [[0] * N_OUTPUT for _ in range(N_OUTPUT)]
    for scores, lab in zip(Yhat, test_y):
        guess = argmax(scores)
        total[lab] += 1
        if guess == lab:
            right[lab] += 1
        else:
            wrong[lab][guess] += 1
    n_right = sum(right)
    n = len(test_y)
    print()
    print(f"test   guess = true   {n_right} of {n}")
    for digit in range(N_OUTPUT):
        line = f"  {digit}   {right[digit]} of {total[digit]}"
        misses = sorted(range(N_OUTPUT), key=lambda g: (-wrong[digit][g], g))
        parts = [f"{g}×{wrong[digit][g]}" for g in misses if wrong[digit][g]]
        if parts:
            line += "    wrong " + "  ".join(parts)
        print(line)


def run_demo(test_x, test_y, W1, W2):
    """One test image per digit. Picture, layer 2, then guess vs truth."""
    print("one test image per digit   picture, then layer 2")
    seen_digit = set()
    for img, lab in zip(test_x, test_y):
        if lab in seen_digit:
            continue
        _Z1, _layer1, _Z2, Yhat = forward([img], W1, W2)
        pred = argmax(Yhat[0])
        print()
        show_digit(img)
        print()
        show_layer2(Yhat[0], pred, lab)
        print(f"guess {pred}   true {lab}")
        seen_digit.add(lab)
        if len(seen_digit) == N_OUTPUT:
            break
    print_guess_counts(test_x, test_y, W1, W2)


def parse_mode(argv):
    """No flag: train, save, then draw. --train and --run select one half."""
    train_flag = False
    run_flag = False
    for arg in argv[1:]:
        if arg == "--train":
            train_flag = True
        elif arg == "--run":
            run_flag = True
        else:
            print(f"unknown argument: {arg}")
            print("usage: python3 two_layer_mnist.py [--train] [--run]")
            sys.exit(2)
    if not train_flag and not run_flag:
        return True, True
    return train_flag, run_flag


def main():
    random.seed(0)
    do_train, do_run = parse_mode(sys.argv)

    here = os.path.dirname(os.path.abspath(__file__))
    subset_path = os.path.join(here, SUBSET_NAME)
    weights_path = os.path.join(here, WEIGHTS_NAME)
    train_x, train_y, test_x, test_y = load_subset(subset_path)

    print("two-layer MNIST net  (pure Python, no NumPy)")
    print("structure: 784 → 32 (ReLU) → 10 (sigmoid)   no biases")
    print("weights:   W¹ is 784×32,  W² is 32×10")
    print(f"data:      {len(train_x)} train, {len(test_x)} test, batch {BATCH}, {EPOCHS} epochs, η={ETA:g}")
    print(f"subset:    {subset_path}")
    print()

    if do_train:
        W1, W2 = train(train_x, train_y)
        save_weights(weights_path, W1, W2)
        print(f"saved weights  {weights_path}")
        print()
    else:
        if not os.path.isfile(weights_path):
            print(f"no weights at {weights_path}")
            print("train first:  python3 two_layer_mnist.py --train")
            sys.exit(1)
        W1, W2 = load_weights(weights_path)

    if do_run:
        run_demo(test_x, test_y, W1, W2)


if __name__ == "__main__":
    main()
