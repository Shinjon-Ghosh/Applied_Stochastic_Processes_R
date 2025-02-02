
##3

set.seed(1234)
result=rep(NA,10000)
for(i in 1: 10000){
  u=runif(1,0,1)
  result [i] = -0.25*log(u)
}
print(result[1:30])
hist(result, main = "Z has an exponential distribution Exp(4)",
     xlab = "Values",
     ylab = "Frequency")



##4
set.seed(1234)
result=rep(NA,10000)
for(i in 1: 10000){
  u=runif(3,0,1)
  result [i] = -0.25*sum(log(u))
}
print(result[1:30])
hist(result, main = "T has a gamma distribution G(3,4)",
     xlab = "Values",
     ylab = "Frequency")

##1
set.seed(1234)
X = c(1, 2, 3, 4)
p = c(1/6, 1/3, 1/4, 1/4)
result = rep(NA, 10000)

for (i in 1:10000) {
  U = runif(1, 0, 1)
  if (U < p[1]) {
    result[i] = X[1]
  } else if (U > p[1] & U < sum(p[1:2])) {
    result[i] = X[2]
  } else if (U > sum(p[1:2]) & U < sum(p[1:3])) {
    result[i] = X[3]
  } else {
    result[i] = X[4]
  }
}
print(result[1:30])
hist(result,
     breaks = seq(min(result) - 0.5, max(result) + 0.5, by = 1),
     main = "Distribution of Discrete Random Variable X",
     xlab = "Values",
     ylab = "Frequency")

##2
set.seed(1234)
result = rep(NA, 10000)
for (i in 1:10000) {
  u = runif(4, 0, 1)
  result[i] = sum(u < 1/3)
}
print(result[1:30])

hist(result, 
breaks = seq(min(result) - 0.5, max(result) + 0.5, by = 1), 
main = "Y has a Binomial Distribution Bin(4, 1/3)", 
xlab = "Number of Successes", ylab = "Frequency")

