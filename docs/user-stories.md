# User stories

Decomposed from the [PRD](/cursor/stores/self/docs/prd.md) as Vincent asked. Four groups: **network structure**, **MNIST data**, **learning algorithm**, **using the network**. Do not write Python until Vincent asks.

Each story is for Vincent, learning how a tiny neural net works.

---

## 1. Structure of the neural network

### US-1.1 — Two-layer layout

**As** Vincent, **I want** a network with 32 hidden neurons then 10 output neurons on 784 pixel inputs, **so that** the shape matches the notes (784 → 32 → 10) and MNIST digits.

**Acceptance**

- Input size is 784 (flattened 28×28).
- Hidden layer size is 32; output size is 10 (one score per digit 0–9).
- Weight matrices are $W^{1}$ of shape 784×32 and $W^{2}$ of shape 32×10.
- No bias vectors.

### US-1.2 — Activations

**As** Vincent, **I want** ReLU on the hidden layer and sigmoid on the output, **so that** the forward pass matches the PRD theory.

**Acceptance**

- Hidden: $\mathrm{ReLU}(z)=\max(0,z)$.
- Output: $\mathrm{sigmoid}(z)=1/(1+e^{-z})$.
- A guess is argmax over the 10 sigmoid scores.

### US-1.3 — One readable Python file

**As** Vincent, **I want** the whole net in one well-commented Python file using only the standard library, **so that** I can read the math next to the code.

**Acceptance**

- Single `.py` file.
- Comments retell the PRD theory (shapes, forward, loss, backward, update).
- No NumPy, no ML libraries, no other languages.

---

## 2. MNIST dataset and its description

### US-2.1 — What the data is

**As** Vincent, **I want** MNIST explained as 28×28 handwritten digits 0–9, **so that** I know what the 784 numbers and the 10 labels mean.

**Acceptance**

- PRD (and later comments) describe: one image = 784 pixels in $[0,1]$; label = which digit; labels stored one-hot for the loss.
- A few real MNIST pictures sit in the PRD with their true labels (what “recognition” means).
- Scope is isolated handwritten numerals, not full-page OCR or printed fonts.

### US-2.2 — Small subset file

**As** Vincent, **I want** a small MNIST subset on disk, **so that** training stays simple and does not use the full 60k/10k set.

**Acceptance**

- Subset file with about **1,000** training images and **200** test images, digits 0–9 represented.
- Loaded in pure Python (no ML dataset helpers).
- A training **batch** is 150 rows, i.e. $X_{150,784}$ as in the notes (150 is not the whole subset).

---

## 3. Learning algorithm

### US-3.1 — Forward pass and loss

**As** Vincent, **I want** an explicit forward pass and MSE loss, **so that** I can see how a guess is scored.

**Acceptance**

- $Z^{1} = X \cdot W^{1}$, $LAYER^{1} = \mathrm{ReLU}(Z^{1})$, $Z^{2} = LAYER^{1} \cdot W^{2}$, $\hat{Y} = \mathrm{sigmoid}(Z^{2})$.
- $LOSS = (\hat{Y} - Y)^{2}$ (summed over the batch).
- No softmax, no extra losses.

### US-3.2 — Backpropagation and SGD

**As** Vincent, **I want** the error sent backward and the weights nudged with SGD, **so that** I can follow the learning step.

**Acceptance**

- Output error $E = 2(\hat{Y}-Y) \odot \mathrm{sigmoid}'(Z^{2})$.
- $\partial LOSS/\partial W^{2} = (LAYER^{1})^{\top} \cdot E$.
- $\partial LOSS/\partial W^{1} = X^{\top} \cdot ((E \cdot (W^{2})^{\top}) \odot \mathrm{ReLU}'(Z^{1}))$.
- Update: $W \leftarrow W - \eta\,\partial LOSS/\partial W$ with $\eta = 1$.
- No momentum, Adam, or regularization.

### US-3.3 — Training loop

**As** Vincent, **I want** a simple loop over batches and epochs, **so that** I can watch the loss move.

**Acceptance**

- Batch size 150; **50 epochs** (one epoch = one pass over the ~1,000 training images).
- Print the loss after each epoch.
- No accuracy target and no hyperparameter search.

---

## 4. Usage of the neural network

### US-4.1 — Train by running the file

**As** Vincent, **I want** to run the Python file and have it train, **so that** I see the learning loop happen.

**Acceptance**

- Running the file loads the subset, trains 50 epochs, prints loss each epoch.
- No extra CLI framework required (a simple `python thefile.py` is enough).

### US-4.2 — Read a digit guess

**As** Vincent, **I want** the trained net to name a few handwritten digits, **so that** I see what “usage” means: image in, digit out.

**Acceptance**

- After training, run forward on a few test (or held-out) images.
- Print predicted digit (argmax) next to the true label.
- No accuracy percentage required; a handful of guesses is enough.
