# Author: Adam Rich
# Date:   July 11, 2019
#         March 20, 2021
#
# Description:
#   Trying to figure out LDA using FLD
# 



# Setup
rm(list = ls(all = TRUE))
library(tidyverse)
library(alrtools)




# Data
penguins <- palmerpenguins::penguins %>% 
  drop_na()

features <- c(
  'bill_length_mm', 'bill_depth_mm', 'flipper_length_mm', 'body_mass_g')

X <- penguins[, features] %>% as.matrix
klass <- penguins$species

k_factor <- klass %>% as.character %>% factor
k_integer <- k_factor %>% as.integer
k_labels <- levels(k_factor)


# Dimensions
K <- length(k_labels)
P <- ncol(X)
N <- nrow(X)
N_k <- k_integer %>% table %>% as.numeric
pi <- N_k / N


# Objects to store parameters
mu <- matrix(nrow = K, ncol = P)
X_k <- list()
Xc_k <- list()
sigma_k <- list()
sigma <- matrix(0, nrow = P, ncol = P)



# X_k and Xc_k
for (i in 1:K) {
  X_k[[i]] <- X[k_integer == i, ]
  mu[i, ] <- apply(X_k[[i]], 2, mean)
  mui_rep <- matrix(
    mu[i, ], nrow = N_k[i], ncol = P, byrow = TRUE)
  Xc_k[[i]] <- X_k[[i]] - mui_rep
  sigma_k[[i]] <- (t(Xc_k[[i]]) %*% Xc_k[[i]]) / (N - K)
  sigma <- sigma + sigma_k[[i]]
}


# MASS::lda defines f1
# define f2 to show how it compares
f1 <- sqrt(diag(var(X - mu[k_integer, ])))
f2 <- (sigma * (N - K) / (N - 1)) %>% diag %>% sqrt

f1
f2

# scaling?
diag(1/f1, P, P)






sigma_inv <- solve(sigma)


ldf <- function(x) {
  t1 <- X %*% sigma_inv %*% t(mu) 
  t2 <- diag(-0.5 * mu %*% sigma_inv %*% t(mu))
  t3 <- log(pi)
  t1 + rep(t2, each = nrow(t1)) + rep(t3, each = nrow(t1))
}

LDF_X <- ldf(X)

pred_X <- apply(LDF_X, 1, function(r) {which(r == max(r))})






mass_lda <- MASS::lda(
  species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  data = penguins)

mass_lda %>% names
mass_lda %>% str

pred_mass <- predict(mass_lda)$class %>% as.integer

data.frame(
  pred_X, 
  k_integer, 
  pred_mass,
  pred_X == k_integer,
  pred_mass == k_integer)

all(pred_X == pred_mass)


