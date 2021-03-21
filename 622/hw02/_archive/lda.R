# lda <- function (x, grouping, prior = proportions, tol = 1e-04, method = c("moment", 
#     "mle", "mve", "t"), CV = FALSE, nu = 5, ...) {

penguins <- palmerpenguins::penguins %>% 
    drop_na()

features <- c(
    'bill_length_mm', 'bill_depth_mm', 'flipper_length_mm', 'body_mass_g')

x <- penguins[, features] %>% as.matrix
grouping <- penguins$species

method <- 'moment'
CV <- FALSE
nu <- 5
tol <- 1e-04


    


n <- nrow(x)
p <- ncol(x)

g <- as.factor(grouping)
lev <- lev1 <- levels(g)
counts <- as.vector(table(g))

proportions <- counts/n
ng <- length(proportions)
prior <- proportions
names(prior) <- names(counts) <- lev1


# method <- match.arg(method)
group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
f1 <- sqrt(diag(var(x - group.means[g, ])))




scaling <- diag(1/f1, , p)

fac <- if (method == "moment") 1/(n - ng) else 1/n
X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol)
if (rank == 0L) 
    stop("rank = 0: variables are numerically constant")
if (rank < p) 
    warning("variables are collinear")
scaling <- scaling %*% 
    X.s$v[, 1L:rank] %*% 
    diag(1/X.s$d[1L:rank], , rank)





xbar <- colSums(prior %*% group.means)
fac <- if (method == "mle") 1/ng else 1/(ng - 1)

X <- sqrt((n * prior) * fac) * 
    scale(group.means, center = xbar, scale = FALSE) %*% 
    scaling

X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol * X.s$d[1L])
if (rank == 0L) 
    stop("group means are numerically identical")
scaling <- scaling %*% X.s$v[, 1L:rank]
if (is.null(dimnames(x))) 
    dimnames(scaling) <- list(NULL, paste("LD", 1L:rank, 
                                          sep = ""))
else {
    dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank, 
                                                 sep = ""))
    dimnames(group.means)[[2L]] <- colnames(x)
}


cl <- match.call()
cl[[1L]] <- as.name("lda")
structure(list(prior = prior, counts = counts, means = group.means, 
               scaling = scaling, lev = lev, svd = X.s$d[1L:rank], N = n, 
               call = cl), class = "lda")


