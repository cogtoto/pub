#!/usr/bin/env python3
"""A two-layer MNIST net, from scratch, standard library only.

This file is the PRD theory in code: 784 pixels -> 32 hidden neurons (ReLU)
-> 10 digit scores (sigmoid). No NumPy, no ML libraries, no biases.

Modes:
    python3 two_layer_mnist.py           # train, save weights, then run
    python3 two_layer_mnist.py --train   # train only (save weights)
    python3 two_layer_mnist.py --run     # run mode: load weights, show
                                         # digit + both layers graphically

Training prints loss each epoch. Run mode skips that noise and draws a
beautiful view of LAYER¹ (32 hidden) and Ŷ (10 outputs) for each guess.
Accuracy is not a goal — watching the loop / the activations is.
"""

from __future__ import annotations

import gzip
import json
import math
import os
import random
import sys

# --- shapes locked in the PRD (so X · W¹ and LAYER¹ · W² are valid) ---
N_PIXELS = 784  # 28×28 flattened
N_HIDDEN = 32  # layer 1 (hidden)
N_OUTPUT = 10  # layer 2 (output), one score per digit 0..9
BATCH = 150  # observations per training step (X is 150 × 784)
EPOCHS = 50
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


# Darker glyph = more ink. Pixels are 0 (empty) … 1 (ink), same numbers as X.
_INK = " .:-=+*#%@"


def _digit_bitmap(pixels, scale=8):
    """28×28 picture scaled up. Each value is 0 (empty) … 255 (ink)."""
    side = 28 * scale
    rows = []
    for y in range(side):
        src_y = y // scale
        row = []
        for x in range(side):
            v = pixels[src_y * 28 + (x // scale)]
            if v < 0.0:
                v = 0.0
            elif v > 1.0:
                v = 1.0
            row.append(int(v * 255 + 0.5))
        rows.append(row)
    return side, side, rows


def print_digit(pixels):
    """The 28×28 picture as text, when the terminal cannot draw an image."""
    for r in range(28):
        row = pixels[r * 28 : (r + 1) * 28]
        chars = []
        for px in row:
            i = int(px * (len(_INK) - 1) + 0.5)
            if i < 0:
                i = 0
            elif i >= len(_INK):
                i = len(_INK) - 1
            chars.append(_INK[i] * 2)
        print("".join(chars))


def _sixel(pixels, scale=8):
    """The same picture as a sixel image (what foot draws inline)."""
    width, height, rows = _digit_bitmap(pixels, scale)
    # #0 black background, #1 white ink — the stored MNIST picture.
    parts = [f'\033P0;1;0q"1;1;{width};{height}#0;2;0;0;0#1;2;100;100;100']
    for y in range(0, height, 6):
        for color, bright in ((0, False), (1, True)):
            parts.append(f"#{color}")
            for x in range(width):
                bits = 0
                for bit in range(6):
                    yy = y + bit
                    if yy < height:
                        ink = rows[yy][x] > 40
                        if ink == bright:
                            bits |= 1 << bit
                parts.append(chr(63 + bits))
            parts.append("$")
        parts.append("-")
    parts.append("\033\\")
    return "".join(parts)


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


def show_digit(pixels):
    """Draw the test picture. Foot gets the real image; otherwise text."""
    if sys.stdout.isatty() and _in_foot():
        sys.stdout.write(_sixel(pixels))
        sys.stdout.write("\n")
    else:
        print_digit(pixels)


# ---------------------------------------------------------------------------
# Weights on disk — so --run can skip training
# ---------------------------------------------------------------------------


def save_weights(path, W1, W2):
    """Persist W¹ and W² as gzipped JSON (pure Python, no pickle)."""
    payload = {"W1": W1, "W2": W2, "n_hidden": N_HIDDEN, "n_output": N_OUTPUT}
    with gzip.open(path, "wt", encoding="utf-8") as f:
        json.dump(payload, f)


def load_weights(path):
    """Load W¹ (784×32) and W² (32×10) saved by save_weights."""
    with gzip.open(path, "rt", encoding="utf-8") as f:
        data = json.load(f)
    return data["W1"], data["W2"]


# ---------------------------------------------------------------------------
# Run-mode graphics: both layers as a terminal composition
#
# Cells and bars are colored spaces (always one column wide). Block glyphs
# like ▁█ are ambiguous-width and shove the columns out of line.
# ---------------------------------------------------------------------------

# Soft ANSI (only when stdout is a TTY).
_RST = "\033[0m"
_DIM = "\033[2m"
_BOLD = "\033[1m"
_CYAN = "\033[38;5;80m"
_VIOLET = "\033[38;5;141m"
_GOLD = "\033[38;5;221m"
_MINT = "\033[38;5;114m"
_ROSE = "\033[38;5;168m"
_LINE = "\033[38;5;240m"
# Dark → bright. 256-color backgrounds, so every cell stays one column.
_CYAN_RAMP = (236, 23, 24, 30, 31, 37, 44, 51)
_VIOLET_RAMP = (236, 53, 54, 90, 91, 127, 128, 141)
_MINT_RAMP = (236, 22, 28, 29, 35, 41, 47, 84)
_GOLD_RAMP = (236, 58, 94, 136, 178, 214, 220, 221)
_ROSE_RAMP = (236, 52, 88, 124, 160, 168, 174, 210)
_CELL = 6  # columns per hidden neuron, index and value share this width


def _use_color():
    return sys.stdout.isatty() and os.environ.get("NO_COLOR") is None


def _c(code, text):
    return f"{code}{text}{_RST}" if _use_color() else text


def _paint(fg, bg, text):
    """Text on a solid background. `text` is a fixed number of spaces/digits."""
    if not _use_color():
        return text
    return f"\033[38;5;{fg}m\033[48;5;{bg}m{text}{_RST}"


def _ramp_at(t, ramp):
    """Pick a ramp color for t in [0, 1]."""
    if t <= 0.0:
        return ramp[0]
    if t >= 1.0:
        return ramp[-1]
    return ramp[int(t * (len(ramp) - 1) + 1e-9)]


def _heat_cell(value, t):
    """One hidden neuron: exactly _CELL columns, brighter when ReLU is stronger."""
    label = "  ·   " if value <= 1e-4 else f"{value:5.2f} "
    bg = _ramp_at(t, _CYAN_RAMP)
    fg = 16 if t > 0.55 else 255
    return _paint(fg, bg, label)


def _bar(value, width, ramp):
    """Horizontal score bar made of single-column colored spaces."""
    v = 0.0 if value < 0.0 else (1.0 if value > 1.0 else value)
    filled = int(v * width + 0.5)
    if not _use_color():
        return "#" * filled + "." * (width - filled)
    bg = _ramp_at(max(v, 0.08), ramp)
    return _paint(15, bg, " " * filled) + _paint(240, 236, " " * (width - filled))


def _rule(width, title=""):
    """Hairline rule with an optional centered title."""
    if not title:
        return _c(_LINE, "─" * width)
    pad = max(0, width - len(title) - 2)
    left = pad // 2
    right = pad - left
    return _c(_LINE, "─" * left) + f" {title} " + _c(_LINE, "─" * right)


def show_layers(layer1_row, yhat_row, pred, lab):
    """Beautiful run-mode view of the two layers for one image.

    LAYER¹ — 32 ReLU activations as a 4×8 heat grid (fixed-width cells).
    LAYER² — 10 sigmoid scores as labeled bars (winner highlighted).
    """
    # Normalize hidden activations to [0, 1] for the heat grid.
    peak = max(layer1_row) if layer1_row else 1.0
    if peak <= 0.0:
        peak = 1.0
    h_norm = [max(0.0, v) / peak for v in layer1_row]

    width = 56
    print()
    print(_c(_BOLD, _rule(width, "run · both layers")))
    print(
        _c(_DIM, "  784 pixels")
        + "  →  "
        + _c(_CYAN, "LAYER¹  32 × ReLU")
        + "  →  "
        + _c(_VIOLET, "LAYER²  10 × sigmoid")
    )
    print()

    # --- LAYER¹: 4 rows × 8 neurons, one fixed-width cell each ------
    print(_c(_BOLD + _CYAN, "  LAYER¹  hidden (32)"))
    print(_c(_DIM, "  one cell per neuron; brighter = stronger ReLU; · = off"))
    print()
    cols = 8
    for r in range(0, N_HIDDEN, cols):
        idxs = list(range(r, min(r + cols, N_HIDDEN)))
        print("  " + _c(_DIM, "".join(f"{i:^{_CELL}d}" for i in idxs)))
        print("  " + "".join(_heat_cell(layer1_row[i], h_norm[i]) for i in idxs))
        print()

    if _use_color():
        spark = "".join(_paint(15, _ramp_at(t, _CYAN_RAMP), " ") for t in h_norm)
    else:
        spark = "".join("#" if t > 0.12 else "." for t in h_norm)
    print("  " + spark + _c(_DIM, f"  peak ReLU {peak:.3f}"))
    print()

    # --- LAYER²: 10 digit score bars --------------------------------
    print(_c(_BOLD + _VIOLET, "  LAYER²  output (10)"))
    print(_c(_DIM, "  each bar = sigmoid score for digit 0…9; * = guess"))
    print()
    bar_w = 32
    for d, score in enumerate(yhat_row):
        is_pred = d == pred
        is_true = d == lab
        mark = "*" if is_pred else " "
        if is_pred and is_true:
            tag = _c(_MINT, "  guess = true")
            ramp = _MINT_RAMP
        elif is_pred:
            ramp = _GOLD_RAMP
            tag = _c(_GOLD, "  guess")
        elif is_true:
            ramp = _ROSE_RAMP
            tag = _c(_ROSE, "  true")
        else:
            ramp = _VIOLET_RAMP
            tag = ""
        print(f"  {mark} {d}  {_bar(score, bar_w, ramp)}  {score:.3f}{tag}")

    print()
    ok = pred == lab
    verdict = _c(_MINT, "ok") if ok else _c(_ROSE, "miss")
    print(f"  →  {verdict}   guess {_c(_BOLD, str(pred))}   true {_c(_BOLD, str(lab))}")
    print(_c(_LINE, "─" * width))


def show_run_guesses(test_x, test_y, W1, W2):
    """Run mode: one test image per digit, with the two-layer graphic."""
    print()
    print(_c(_BOLD, "run mode  ·  graphical view of both layers"))
    print(_c(_DIM, "one test image per digit 0…9  ·  no training noise"))
    seen_digit = set()
    for img, lab in zip(test_x, test_y):
        if lab in seen_digit:
            continue
        _Z1, layer1, _Z2, Yhat = forward([img], W1, W2)
        pred = argmax(Yhat[0])
        print()
        print(_c(_BOLD, f"digit sample  true={lab}"))
        show_digit(img)
        show_layers(layer1[0], Yhat[0], pred, lab)
        seen_digit.add(lab)
        if len(seen_digit) == N_OUTPUT:
            break


# ---------------------------------------------------------------------------
# Train / run modes
# ---------------------------------------------------------------------------


def train(train_x, train_Y, quiet=False):
    """50 epochs of batch SGD. Returns W¹, W²."""
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

        if not quiet:
            mean_loss = epoch_loss / n_seen
            print(f"epoch {epoch:2d}/{EPOCHS}  loss {epoch_loss:.1f}  (mean per image {mean_loss:.4f})")

    return W1, W2


def parse_mode(argv):
    """Default: train then run. --train / --run select one phase."""
    mode = "both"
    for arg in argv[1:]:
        if arg in ("-h", "--help"):
            print(__doc__.strip())
            print()
            print("options:")
            print("  (none)    train, save weights, then run-mode graphics")
            print("  --train   train only and save weights")
            print("  --run     load weights and show both layers (no training)")
            print("  --help    this text")
            sys.exit(0)
        elif arg == "--train":
            mode = "train"
        elif arg == "--run":
            mode = "run"
        else:
            print(f"unknown option: {arg}", file=sys.stderr)
            print("try --help", file=sys.stderr)
            sys.exit(2)
    return mode


def main():
    random.seed(0)
    mode = parse_mode(sys.argv)

    here = os.path.dirname(os.path.abspath(__file__))
    subset_path = os.path.join(here, SUBSET_NAME)
    weights_path = os.path.join(here, WEIGHTS_NAME)
    train_x, train_y, test_x, test_y = load_subset(subset_path)
    train_Y = one_hot(train_y)

    print("two-layer MNIST net  (pure Python, no NumPy)")
    print("structure: 784 → 32 (ReLU) → 10 (sigmoid)   no biases")
    print("weights:   W¹ is 784×32,  W² is 32×10")
    print(f"data:      {len(train_x)} train, {len(test_x)} test, batch {BATCH}, {EPOCHS} epochs, η={ETA:g}")
    print(f"subset:    {subset_path}")
    print(f"mode:      {mode}")
    print()

    if mode in ("train", "both"):
        print("training mode  ·  watching the loss")
        print()
        W1, W2 = train(train_x, train_Y, quiet=False)
        save_weights(weights_path, W1, W2)
        print()
        print(f"saved weights → {weights_path}")

    if mode == "run":
        if not os.path.isfile(weights_path):
            print(f"no weights at {weights_path}", file=sys.stderr)
            print("run once with --train (or no flags) first.", file=sys.stderr)
            sys.exit(1)
        W1, W2 = load_weights(weights_path)
        print(f"loaded weights ← {weights_path}")

    if mode in ("run", "both"):
        show_run_guesses(test_x, test_y, W1, W2)


if __name__ == "__main__":
    main()
