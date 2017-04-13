#shadow line plot of logistic equation
a = seq(from = 0, to = 4, by = .001)
x = .5
v = a*x*(1-x)
plot(a, v, type = 'l', col = 1, xlim = c(2,4), ylim = c(0,1))
for (i in 1:4) {
  v = a*v*(1-v)
  lines(a, v, col = i + 1)
}

#logistic equation invariant plot
a = 4
x = seq(0, 1, .001)
f = matrix(NA, nrow = 4, ncol = length(x))
f[1, ] = a*x*(1-x)
plot(x, f[1, ], type = "l", col = 2)
lines(x, x)
for (i in 2:ncol(f)) {
  f[i, ] = a*f[i-1,]*(1-f[i-1, ])
  lines(x, f[i, ], col = i+1)
}
hist(f, breaks = 1000)

#collatz
collatz = function(x) {
  count = matrix(x, nrow = length(x), ncol = 2)
  count[, 2] = 1
  for(i in 1:length(x)) {
    while(x[i] != 1) {
      if(x[i] %% 2 == 0) {
        x[i] = x[i] / 2
      } else {
        x[i] = 3 * x[i] + 1
      }
      count[i, 2] = count[i, 2] + 1
    }
  }
  count
}

#plot of x^#+ax at varying a
a = -1:1
x = seq(from = -5, to = 5, by = 0.01)
plot(x, x, type = "l")
lines(x, matrix(0, ncol = length(x)))
lines(matrix(0, ncol = length(x)), x)
for (i in 1:(length(a))) {
  f = x^3 + a[i]*x
  fk = f^3 + a[i]*f
  lines(x, f, col = i + 1)
  lines(x, fk, col = i + 1)
}