
# MNIST DATA PREPARATION

# predictor variables
X <- matrix(c(
  0,0,0,0,1,
  0,0,0,1,0,
  0,0,1,1,0,
  0,0,1,1,0,
  0,1,1,0,0,
  1,1,0,0,0,
  0,0,0,1,1,
  0,0,0,1,0,
  0,0,1,1,0,
  0,0,1,1,0,
  0,1,1,0,0,
  1,0,1,0,0,
  0,0,0,1,0,
  0,0,0,1,1,
  0,0,1,1,0,
  0,0,1,1,0,
  0,1,1,0,0,
  1,0,1,0,0
),
  ncol = 5,
  byrow = TRUE
)

# observed outcomes
y <- matrix(c(
  0,0,1,
  0,0,1,
  0,1,0,
  0,1,0,
  1,0,0,
  1,0,0,
  0,0,1,
  0,0,1,
  0,1,0,
  0,1,0,
  1,0,0,
  1,0,0,
  0,0,1,
  0,0,1,
  0,1,0,
  0,1,0,
  1,0,0,
  1,0,0), ncol=3, byrow = TRUE)
              

my_nn <- list(
  input = X,
  weights1 = matrix(runif(6*5),nrow = 5, ncol=6),
  z1 = matrix(rep(0,6*5), nrow = 6 , ncol= 5),
  weights2 = matrix(runif(6*3),nrow=6, ncol = 3),
  z2 = matrix(rep(0,6*3), nrow = 6 , ncol= 3),
  y = y,
  output = matrix(rep(0, times = 6*3), ncol = 3)
)

# the activation function
sigmoid <- function(x) {
  1.0 / (1.0 + exp(-x))
}

x=seq(0,20,0.1)
plot(x, sigmoid(x), type="l") 

# the derivative of the activation function
sigmoid_derivative <- function(x) {
  sigmoid(x) * (1.0 - sigmoid(x))
}

x=seq(0,20,0.1)
plot(x, sigmoid_derivative(x), type="l") 


# the relu function and its derivative
relu <- function(x) {
  if (x<=0) 0 else x
}

relumatrix <- function(m) {
  apply(m, c(1,2), relu)
}

relu_derivative <- function(x) {
  if (x<=0) 0 else 1
}

relumatrix_derivative <- function(m) {
  apply(m, c(1,2), relu_derivative)
}

loss_function <- function(nn, b) {
  sum((nn$y[(((b-1)*batch_size+1):(b*batch_size)),] - nn$output) ^ 2)
}

# batch_size. doit être un multiple du sample en input
batch_size <- 6

feedforward <- function(nn, b) {
  nn$z1 <- nn$input[(((b-1)*batch_size+1):(b*batch_size)),] %*% nn$weights1
  nn$layer1 <- relumatrix(nn$z1)
  nn$z2 <- nn$layer1 %*% nn$weights2
  nn$output <- sigmoid(nn$z2)
  nn
}


backprop <- function(nn, b, learning_rate) {
  d_weights2 <- ( t(nn$layer1) %*%  (2 * (nn$y[(((b-1)*batch_size+1):(b*batch_size)),] - nn$output) *  sigmoid_derivative(nn$z2))) 

  d_weights1 <- ( 2 * (nn$y[(((b-1)*batch_size+1):(b*batch_size)),] - nn$output) * sigmoid_derivative(nn$z2)) %*% t(nn$weights2)
  d_weights1 <- d_weights1 * relumatrix_derivative(nn$z1)
  d_weights1 <- t(nn$input[(((b-1)*batch_size+1):(b*batch_size)),]) %*% d_weights1
  nn$weights1 <- nn$weights1 + (d_weights1 * learning_rate)
  nn$weights2 <- nn$weights2 + (d_weights2 * learning_rate)

  nn
}


# number of times to perform feedforward and backpropagation, and the learning rate
n <- 100
learning_rate <- 1

loss_df <- matrix(ncol=nrow(my_nn$input)/batch_size, nrow = n) 

for (i in (1:n)) {
  for (b in (1:(nrow(my_nn$input)/batch_size)))
  {
   my_nn <- feedforward(my_nn, b)
   my_nn <- backprop(my_nn, b, learning_rate)
   loss_df[i,b] <- loss_function(my_nn, b)
  }
  
}

print(loss_df)


test_nn <- function(nn, x) {
  resultats <- sigmoid(sigmoid(x %*% nn$weights1) %*% nn$weights2)
  sortie <-  max.col(resultats) - 1
  comparaison <- matrix(data = c(y_test, sortie), byrow= FALSE, ncol=2)
  comparaison <- cbind(comparaison, comparaison[,1]== comparaison[,2])
  return(colSums(comparaison)[3]/length(x[,1]))
  }


