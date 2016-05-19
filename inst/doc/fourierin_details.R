## ---- fig.height= 5, fig.width= 7----------------------------------------
library(fourierin)

                                        # Compute integral
out <- fourierin(f = function(t) exp(-t^2/2),
                 a = -5, b = 5, c = -3, d = 3,
                 r = -1, s = -1, resol = 64)
grid <- out$w                           # Extract grid and values
values <- Re(out$values)

plot(grid, values, type = "l", col = 3, xlab = "x", ylab = "f(x)")
lines(grid, dnorm(grid), col = 4)
legend("topleft", legend = c("True", "Recovered"), col = c(4, 3),
      lwd = 1)

## ---- fig.height= 5, fig.width= 7----------------------------------------

library(fourierin)
                                        # Compute integral
shape <- 5
rate <- 3
out <- fourierin(f = function(t) dgamma(t, shape, rate),
                 a = -0, b = 8, c = -5, d = 5,
                 r = 1, s = 1, resol = 64)
grid <- out$w                           # Extract grid
re_values <- Re(out$values)             # Real values
im_values <- Im(out$values)             # Imag values

                                        # Now compute the real and
                                        # imaginary true values of the
                                        # characteric function.
true_cf <- function(t, shape, rate) (1 - 1i*t/rate)^-shape
true_re <- Re(true_cf(grid, shape, rate))
true_im <- Im(true_cf(grid, shape, rate))

                                        # Compare them. We can see a
                                        # slight discrepancy on the
                                        # tails, but that is fixed
                                        # when resulution is
                                        # increased.
plot(grid, re_values, type = "l", col = 3, xlab = "t",
     ylab = expression(paste("Re ", phi(t))))
lines(grid, true_re, col = 4)
legend("topright", legend = c("True", "Recovered"), col = c(4, 3),
      lwd = 1)

                                        # Same here
plot(grid, im_values, type = "l", col = 3, xlab = "t",
     ylab = expression(paste("Im ", phi(t))))
lines(grid, true_im, col = 4)
legend("topright", legend = c("True", "Recovered"), col = c(4, 3),
      lwd = 1)

