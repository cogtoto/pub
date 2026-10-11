> This is the Project PRD for the simple Python neural net (living copy also in Project Context).

# PRD: Simple neural network (Python)

Status: **draft** — iterating with Vincent. Do not decompose into user stories or write code until Vincent asks.

## Problem

Vincent wants a very simple neural network in Python so he can learn how it works: a from-scratch implementation of the two-layer MNIST network from his notes.

## Goals

- A small, self-explanatory neural network in Python that makes the mechanics visible
- Follow the definition Vincent quoted from his notes (below)
- Train on a small MNIST subset file and print a few digit guesses
- **Priority: keep it simple and self-explanatory** — not accuracy
- Spec-driven delivery (this PRD is the source of truth until stories and code are requested)
- Implementation and tests run on Vincent’s local machine

## Non-goals (until the PRD says otherwise)

- Any Python ML library (PyTorch, TensorFlow, Keras, scikit-learn, JAX, and similar)
- NumPy or other numeric libraries — pure Python only
- Full 60k/10k MNIST training
- Keeping the implementation in R or OCaml
- User-story decomposition
- Production OCR, GPUs, or a full deep-learning framework
- Any accuracy target, hyperparameter search, or tuning for a score

## Users

- Primary: Vincent, learning how a neural network works by building one

## Reference

- [NN chapter](/workspace/BOOK/chapter8.tex) — “Machine Learning and Neural Networks”
- [R sketch](/workspace/R/NN.R)

## Model definition (from Vincent)

150 observations in a matrix \(X_{150,784}\) (2D tensor): 150 rows (observations), 784 columns (features).

Two weight matrices. The notes label them \(W^{1}_{32,150}\) and \(W^{2}_{10,150}\). Those subscripts do not multiply with \(X_{150,784}\). **Locked** (fits MNIST 28×28 and 32 then 10 neurons):

- \(W^{1}\): \(784 \times 32\)
- \(W^{2}\): \(32 \times 10\)

Layers:

- 1st layer: 32 neurons
- 2nd layer: 10 neurons

Output \(OUTPUT_{150,10}\): 150 rows, 10 columns (the 10 digit classes to recognize).

Simplified 2-layer schema:

\[
X \longrightarrow \otimes W^{1} \rightarrow Z^{1} \rightarrow \sigma \rightarrow LAYER^{1} \longrightarrow \otimes W^{2} \rightarrow Z^{2} \rightarrow \sigma \rightarrow \hat{Y} \gg LOSS(\hat{Y}, Y)
\]

Vincent’s schema uses \(\sigma\) as a generic activation. **Chosen activations:** hidden **ReLU**, output **sigmoid** (as in the later chapter paragraph and the R sketch).

## Decisions

- **Purpose:** learning how a neural network works
- **Stack:** pure Python, from scratch. No ML libraries, no NumPy.
- **Task:** handwritten digit recognition (OCR) on MNIST
- **Done:** one well-commented Python file that trains and can emit digit guesses. **No accuracy objective.** Priority is that a reader can follow the learning loop.
- **Architecture:** 32 neurons then 10 neurons on 784-pixel inputs → **784 → 32 → 10**
- **Weight matrices:** \(W^{1}\) is **784×32**, \(W^{2}\) is **32×10** (Vincent deferred; this is what matches MNIST)
- **Forward:** \(X \to \otimes W^{1} \to Z^{1} \to \mathrm{ReLU} \to LAYER^{1} \to \otimes W^{2} \to Z^{2} \to \mathrm{sigmoid} \to \hat{Y} \gg LOSS(\hat{Y}, Y)\)
- **Activations:** hidden **ReLU** \(\max(0,x)\); output **sigmoid** \(1/(1+e^{-x})\)
- **Loss:** MSE, \(LOSS = (\hat{Y} - Y)^{2}\) (from the same chapter)
- **Prediction:** argmax over the 10 outputs → digit 0–9
- **Subset file:** ~1,000 train / ~200 test
- **Batch:** **150 images per training step** (\(X_{150,784}\) as in the notes). That is option **B** — 150 is not the whole dataset.
- **Epoch (definition):** one full pass over the training subset. With ~1,000 train images and batches of 150, one epoch is about seven steps (the last batch may be smaller). 50 epochs means we walk through the 1,000 images 50 times. Not a score — just how long we let the loop run.
- **Deliverable:** **one Python file**, well commented so the forward pass, loss, and backprop can be read as a lesson
- **Learning:** backpropagation + SGD (detailed below)
- **Learning rate:** \(\eta = 1\)
- **Epochs:** 50, print the loss after each epoch (so the loop is visible, not to chase a score)

## Learning mechanism (backpropagation)

Very simple, two-layer version of “guess, measure the error, send it backward, nudge the weights.”

Each training step on a batch of images:

1. **Forward** — compute \(\hat{Y}\) with ReLU then sigmoid.
2. **Loss** — mean squared error: how far \(\hat{Y}\) is from the true labels \(Y\) (one-hot digits).
   \[
   LOSS = (\hat{Y} - Y)^{2}
   \]
3. **Backward** — from that error, how much each weight contributed:
   - Output-side error (MSE + sigmoid):
     \[
     E = 2(\hat{Y}-Y) \odot \mathrm{sigmoid}'(Z^{2})
     \]
   - First update the second layer (closer to the answer):
     \[
     \frac{\partial LOSS}{\partial W^{2}} = (LAYER^{1})^{\top} \cdot E
     \]
   - Then send \(E\) back through \(W^{2}\) and ReLU, and update the first layer:
     \[
     \frac{\partial LOSS}{\partial W^{1}} = X^{\top} \cdot \bigl( (E \cdot (W^{2})^{\top}) \odot \mathrm{ReLU}'(Z^{1}) \bigr)
     \]
     \(\mathrm{ReLU}'(z)\) is \(1\) if \(z>0\), else \(0\). \(\mathrm{sigmoid}'(z)=\sigma(z)(1-\sigma(z))\). \(\odot\) is element-wise multiply; \(\cdot\) is matrix multiply.
4. **Update** — take a small step downhill (SGD, nothing fancier):
   \[
   W^{1} \leftarrow W^{1} - \eta\,\frac{\partial LOSS}{\partial W^{1}}, \quad
   W^{2} \leftarrow W^{2} - \eta\,\frac{\partial LOSS}{\partial W^{2}}
   \]

No extra tricks: no momentum, Adam, or regularization. Repeat over batches until training stops.

## Requirements

- Implement from scratch in pure Python (explicit forward pass, loss, backprop).
- Keep the code readable next to the schema above.
- No ML libraries, no NumPy.
- MNIST subset file for train/eval.
- After training, print digit guesses for some images (no score to hit).
- Two layers, 32 then 10 neurons.
- Ship as a single well-commented `.py` file (comments explain the schema, matrix shapes, and the backward step).
- Learning is the four-step loop above (forward, MSE, backprop, SGD).

## Success criteria

- The single `.py` file is simple and self-explanatory (comments carry the forward / loss / backward / update story).
- Running it trains the net and prints a few predicted digits vs labels.
- **No accuracy number is required.** We do not tune to hit a %.

## Constraints

- Language: Python (standard library only)
- Code on Vincent’s local machine when we reach implementation
- Process: PRD (here) → user stories (on request) → code and tests (on request)

## Open questions

None on the model. PRD stays **draft** until Vincent says it is mature enough for user stories.
