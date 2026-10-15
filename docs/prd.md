# PRD: Simple neural network (Python)

Status: **implemented** — see `two_layer_mnist.py`.

User stories: [user-stories.md](user-stories.md) (four groups: network structure, MNIST, learning, usage).

Formulas below are plain text (no LaTeX), so they show in Cursor **Preview**. The typeset edition, with the TikZ structure figure, is [prd.tex](prd.tex).

## Problem

Vincent wants a very simple neural network in Python so he can learn how it works: a from-scratch two-layer MNIST network. In **run mode** he wants a beautiful graphical view of **both layers** (the 32 hidden neurons and the 10 output scores) — not the training loss spam.

## Goals

- A small, self-explanatory neural network in **Python** that makes the mechanics visible
- Follow the definition Vincent quoted from his notes (below)
- Train on a small MNIST subset file and print a few digit guesses
- **Run mode:** after weights exist, show each test digit with a polished terminal graphic of **LAYER¹** (32) and **LAYER²** (10)
- **Priority: keep it simple and self-explanatory** — not accuracy
- Spec-driven delivery (this PRD is the source of truth)
- Implementation and tests run on Vincent’s local machine

## Non-goals (until the PRD says otherwise)

- Any language other than Python
- Any Python ML library (PyTorch, TensorFlow, Keras, scikit-learn, JAX, and similar)
- NumPy or other numeric libraries — pure Python only
- Full 60k/10k MNIST training
- User-story decomposition
- Production OCR, GPUs, or a full deep-learning framework
- Any accuracy target, hyperparameter search, or tuning for a score

## Users

- Primary: Vincent, learning how a neural network works by building one

## Reference

- Vincent’s notes for the two-layer MNIST definition quoted below (math and schema only)

## Theory

This section is the teaching core of the PRD. The later Python file should be readable as a walk through of this theory.

### What the network is

A neural network here is **not** a black box. It is two tables of numbers (**weights**) plus two simple math steps (**activations**).

We show it a picture of a handwritten digit. It outputs **10 scores**, one per digit `0`…`9`. The largest score is its guess.

It does not “see” a picture the way we do. The picture is turned into **784 numbers**: each pixel of the 28×28 grid is one number between 0 (white) and 1 (black), laid out in a single row.

So one image is a row vector of length 784. A batch of 150 images is the matrix **X** of shape **150 × 784** from the notes.

### What we want to recognize

The goal is **not** to read a full page or a printed font. It is: given one small handwritten numeral, **name the digit** (`0`…`9`).

Below are a few **real MNIST** examples (originally 28×28 pixels, shown larger). The caption is the true label — what we want the network to output after it looks at that picture.

![MNIST example: digit 0](mnist-examples/digit-0.png)

**True label: 0** — the network should guess `0`.

![MNIST example: digit 1](mnist-examples/digit-1.png)

**True label: 1** — the network should guess `1`.

![MNIST example: digit 2](mnist-examples/digit-2.png)

**True label: 2** — the network should guess `2`.

![MNIST example: digit 3](mnist-examples/digit-3.png)

**True label: 3** — the network should guess `3`.

![MNIST example: digit 7](mnist-examples/digit-7.png)

**True label: 7** — the network should guess `7`.

![MNIST example: digit 8](mnist-examples/digit-8.png)

**True label: 8** — the network should guess `8`.

Handwriting varies (a sloppy `7` vs a crossed `7`, an `8` that looks like a `0`). That is why we train on many examples. After training, we print a few guesses vs these kinds of labels. We do **not** require a score; we want the loop to be obvious.

### A neuron

One **neuron** does two things:

1. **Weighted sum** — multiply each incoming number by a weight, then add those products. That sum is **z**.
2. **Activation** — pass **z** through a small function σ(z) so the output stays in a useful range (not a huge raw sum).

The **weights** are the knobs we will learn. At the start they are random. After training they should make the digit scores match the labels.

This Project has **no bias terms** (no extra “+ b” per neuron), so the theory stays aligned with the two weight matrices only.

### Two layers

- **Layer 1 (hidden):** 32 neurons. Each one looks at all 784 pixels. Together they are a 32-number “internal sketch” of the image: not a digit yet, just features the second layer can use. In **run mode** these 32 values are drawn as a heat grid (4×8) plus a sparkline.
- **Layer 2 (output):** 10 neurons. Each one looks at those 32 numbers and votes for one digit class. In **run mode** these 10 sigmoid scores are drawn as labeled bars (guess ★ / true highlighted).

There is **one** hidden layer and **one** output layer — two layers total. “Both layers” in the UI means LAYER¹ + LAYER², not two hidden layers.

So the shapes that actually fit MNIST are:

| object | meaning | shape |
| --- | --- | --- |
| X | 150 images, 784 pixels each | 150 × 784 |
| W¹ | every hidden neuron’s 784 weights | 784 × 32 |
| Z¹ | hidden weighted sums | 150 × 32 |
| LAYER¹ | hidden activations | 150 × 32 |
| W² | every output neuron’s 32 weights | 32 × 10 |
| Z² | output weighted sums | 150 × 10 |
| Ŷ | 10 scores per image | 150 × 10 |
| Y | true labels, one-hot (a 1 in the true digit column) | 150 × 10 |

The notes label the weights W¹ as 32×150 and W² as 10×150. Those sizes do not multiply with X (150 × 784). We keep the **32 and 10 neurons**, and use **784×32** and **32×10** so the multiplications are valid.

### Forward pass (how a guess is made)

From the notes, with σ meaning “an activation” in general:

```
X → ⊗ W¹ → Z¹ → σ → LAYER¹ → ⊗ W² → Z² → σ → Ŷ  >>  LOSS(Ŷ, Y)
```

In this Project the two σ are **different** functions:

1. Z¹ = X · W¹
2. LAYER¹ = ReLU(Z¹)
3. Z² = LAYER¹ · W²
4. Ŷ = sigmoid(Z²)

**ReLU** (hidden layer): ReLU(z) = max(0, z). Negative sums become 0; positive sums pass through. That is a cheap “on/off” that is easy to differentiate (slope 1 if z > 0, else 0).

**Sigmoid** (output layer): sigmoid(z) = 1 / (1 + e^(−z)). Each of the 10 numbers is squashed into (0, 1), which we treat as a soft score for that digit. We do **not** force them to sum to 1 (that would be softmax — out of scope). The guess is simply **argmax**: the column with the largest score.

### Loss (how wrong a guess is)

The true label for an image is a **one-hot** row: digit `3` is `[0,0,0,1,0,0,0,0,0,0]`.

**Mean squared error** compares every score to every one-hot entry:

```
LOSS = (Ŷ − Y)²
```

(summed over the batch in the code). If the network is sure about the right digit, Ŷ is close to Y and the loss is small. If it is sure about the wrong digit, the loss is large.

We **do not** chase an accuracy percentage. Loss is only there so the backward pass has a number to reduce.

### Backward pass (how it learns)

Learning is **backpropagation** plus **SGD**. Idea in one sentence: *measure the error at the output, send it backward through each layer, and nudge every weight a little so the next forward pass is less wrong.*

Derivatives we need:

- sigmoid′(z) = σ(z) · (1 − σ(z))
- ReLU′(z) = 1 if z > 0, else 0

⊙ is element-wise multiply (Hadamard). · is matrix multiply. ᵀ is transpose.

For one batch:

1. Error at the output (MSE + sigmoid):

```
E = 2(Ŷ − Y) ⊙ sigmoid′(Z²)
```

2. How to change the second-layer weights (closer to the answer):

```
∂LOSS/∂W² = (LAYER¹)ᵀ · E
```

   Read: “for each of the 32 hidden values, how much did it push each of the 10 output errors?”

3. Send that error back to the hidden layer, through W² and ReLU:

```
∂LOSS/∂W¹ = Xᵀ · ((E · (W²)ᵀ) ⊙ ReLU′(Z¹))
```

   Read: “for each of the 784 pixels, how much did it push each of the 32 hidden neurons, given the error that came from the output?”

4. **SGD update** (take a small step downhill; η = 1):

```
W¹ ← W¹ − η · ∂LOSS/∂W¹
W² ← W² − η · ∂LOSS/∂W²
```

No momentum, Adam, or regularization. Repeat: **batch** of 150 images → **epoch** = one full walk through the ~1,000 training images (~7 batches) → **50 epochs**.

That is the whole learning theory for this Project.

## Model definition (from Vincent)

150 observations in a matrix X of shape 150 × 784 (2D tensor): 150 rows (observations), 784 columns (features).

Two weight matrices. The notes label them W¹ as 32×150 and W² as 10×150. **Locked** implementable shapes: W¹ is **784 × 32**, W² is **32 × 10**.

Layers: 1st layer of 32 neurons, 2nd layer of 10 neurons.

Output OUTPUT of shape 150 × 10: 150 rows, 10 columns (the 10 digit classes).

Schema: X → ⊗ W¹ → Z¹ → σ → LAYER¹ → ⊗ W² → Z² → σ → Ŷ >> LOSS(Ŷ, Y), with hidden ReLU and output sigmoid.

## Decisions

- **Language:** Python only (standard library). No other languages.
- **Purpose:** learning how a neural network works
- **Stack:** pure Python, from scratch. No ML libraries, no NumPy.
- **Task:** handwritten digit recognition (OCR) on MNIST
- **Done:** one well-commented Python file that trains, saves weights, and in **run mode** shows digit + both layers graphically. **No accuracy objective.** Priority is that a reader can follow the learning loop and *see* the activations.
- **Architecture:** 32 neurons then 10 neurons on 784-pixel inputs → **784 → 32 → 10**
- **Weight matrices:** W¹ is **784×32**, W² is **32×10**
- **Forward / activations / loss / backprop:** as in **Theory** above
- **Prediction:** argmax over the 10 outputs → digit 0–9
- **Subset file:** ~1,000 train / ~200 test
- **Batch:** **150 images per training step**
- **Epochs:** 50, print the loss after each epoch (**training mode** only)
- **Learning rate:** η = 1
- **Modes:** `--train` (loss only + save weights), `--run` (load weights + layer graphics, no training), default = train then run
- **Weights file:** `mnist_weights.json.gz` next to the script (so `--run` skips training)
- **Deliverable:** **one Python file**, well commented so it reads as this theory in code

### Run mode — graphical view of both layers

Training mode is for watching the loss. **Run mode is for seeing the network think.**

For each sample digit 0…9 (one test image per class):

1. Draw the 28×28 digit (sixel in Foot, text art otherwise).
2. Draw **LAYER¹** (32 ReLU activations): a 4×8 heat grid. Each neuron is one fixed-width cell (the ReLU value on a brightness ramp) plus a one-line sparkline under the grid. Cells are scaled to the peak ReLU for that image.
3. Draw **LAYER²** (10 sigmoid scores): horizontal bars labeled 0…9; ★ marks the guess; true label and guess use distinct highlights.
4. Print a one-line verdict: guess vs true.

No epoch lines in `--run`. Soft ANSI color when stdout is a TTY (`NO_COLOR` disables it). Pure Unicode + ANSI — no GUI toolkit.

## Requirements

- Implement from scratch in pure Python (explicit forward pass, loss, backprop).
- Comments should retell the theory above, next to the matching lines.
- No ML libraries, no NumPy, no other languages.
- MNIST subset file for train/eval.
- After training (or via `--run`), show digit guesses with the **both-layers** graphic (no score to hit).
- Two layers, 32 then 10 neurons.
- Ship as a single well-commented `.py` file.
- `--train` / `--run` / default (both) as above.

## Success criteria

- The single `.py` file is simple and self-explanatory (comments carry the forward / loss / backward / update story).
- Training mode prints loss each epoch and saves weights.
- Run mode loads weights and shows each sample digit with a clear graphic of LAYER¹ and LAYER².
- **No accuracy number is required.** We do not tune to hit a %.

## Constraints

- Language: Python (standard library only)
- Code on Vincent’s local machine
- Process: PRD (here) → [user stories](user-stories.md) → `two_layer_mnist.py`

## Open questions

None.
