


bos <- MASS::Boston


names(bos)


m1 <- lm(medv ~ rm, data = bos)

plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 4)


