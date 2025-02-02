## 1
set.seed(123)
N <- 50000
U=runif(N, 0, 1)
print(U[1:40])

X <- c(0, 1, 2, 3)
P <- c(1/4, 1/4, 1/4, 1/4)
State <- rep(NA, N)
Random_N <- rep(NA, N)

U <- runif(1, 0, 1)
if (U < P[1]) {
  discret_N <- X[1]
} else if (U < sum(P[1:2])) {
  discret_N <- X[2]
} else if (U < sum(P[1:3])) {
  discret_N <- X[3]
} else {
  discret_N <- X[4]
}


State[1] <- discret_N 
Random_N[1] <- U
print(paste("The Initial state is: ", State[1]))

P_0 <- c(0.7, 0, 0.3, 0)
P_1 <- c(0.5, 0, 0.5, 0)
P_2 <- c(0, 0.4, 0, 0.6)
P_3 <- c(0, 0.2, 0, 0.8)

for (i in 2:N) {
  U <- runif(1, 0, 1)
  if (State[i-1] == 0) {
    if (U < P_0[1]) {
      discret_N <- X[1]
    } else if (U < sum(P_0[1:2])) {
      discret_N <- X[2]
    } else if (U < sum(P_0[1:3])) {
      discret_N <- X[3]
    } else {
      discret_N <- X[4]
    }
    State[i] <- discret_N
    Random_N[i] <- U
  } else if (State[i-1] == 1) {
    if (U < P_1[1]) {
      discret_N <- X[1]
    } else if (U < sum(P_1[1:2])) {
      discret_N <- X[2]
    } else if (U < sum(P_1[1:3])) {
      discret_N <- X[3]
    } else {
      discret_N <- X[4]
    }
    State[i] <- discret_N
    Random_N[i] <- U
  } else if (State[i-1] == 2) {
    if (U < P_2[1]) {
      discret_N <- X[1]
    } else if (U < sum(P_2[1:2])) {
      discret_N <- X[2]
    } else if (U < sum(P_2[1:3])) {
      discret_N <- X[3]
    } else {
      discret_N <- X[4]
    }
    State[i] <- discret_N
    Random_N[i] <- U
  } else {
    if (U < P_3[1]) {
      discret_N <- X[1]
    } else if (U < sum(P_3[1:2])) {
      discret_N <- X[2]
    } else if (U < sum(P_3[1:3])) {
      discret_N <- X[3]
    } else {
      discret_N <- X[4]
    }
    State[i] <- discret_N
    Random_N[i] <- U
  }
}

print(State[1])

print(Random_N[1:40])



Manual <- c(.25, .15, .15, .45)
simulated <- c(sum(State == 0) / 50000, sum(State == 1) / 50000, sum(State == 2) / 50000, sum(State == 3) / 50000)
Difference <- abs(real-simulated)


# print the long run of each state which is generated in the program
print(paste("The difference between real long run proportion and simulated proportion in state 0 is:", Difference[1]))
print(paste("The difference between real long run proportion and simulated proportion in state 1 is:", Difference[2]))
print(paste("The difference between real long run proportion and simulated proportion in state 2 is:", Difference[3]))
print(paste("The difference between real long run proportion and simulated proportion in state 3 is:", Difference[4]))

