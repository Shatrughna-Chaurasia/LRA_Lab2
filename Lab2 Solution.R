#P1
data1 = read.csv("C:/Users/Shatrughna/Desktop/dataset.csv")
data1 <- read.csv("C:\\Users\\Shatrughna\\Desktop\\dataset.csv")

data1
summary(lm(data1$Y ~ data1$X))
#P2
data2 = read.csv("C:/Users/Shatrughna/Desktop/Delivery.csv")
data2
model = summary(lm(data2$Y ~ data2$X1 + data2$X2))
#P3
#  Value of the unbiased estimator of σ^2

x = cbind(1,data1$X);x
beta = matrix(c(2627.822, -37.154),2,1);beta
sigma = sum((data1$Y - x%*%(beta))^2)/18;sigma
#P4
beta = coef(lm(data1$Y ~ data1$X))
T1 = 2627.82/44.184 ;T1
T2 = -37.154/2.889;T2
Tj = c(T1,T2);Tj
#P4
n = 5000
z_samples = matrix(rnorm(n*3,mean = 0, sd = 1),ncol = 3)
hist(z_samples)
z_squared = z_samples^2
w_samples = rowSums((z_squared))
hist(w_samples)
mean(w_samples)
var(w_samples)
#5.
x = matrix(print(w_samples[1:40]),8,5);x
Px = x%*%solve((t(x)%*%x))%*%t(x);Px
Px%*%Px == Px
sample = rnorm(mean = matrix(0,8,1), sd = matrix(1,8,8))
library(MASS)
n = 5000
mean_vec = matrix(0,8,1)
cov = matrix(1,8,8)
u = mvrnorm(n,mean_vec,cov);u
mean = colMeans(u);mean
var = cov(u);var
Px2 = matrix(1,8,8) - Px;Px2
Y = MASS::mvrnorm(n,mean_vec,cov);Y
df1 = rank(Px);df1
df2 = rank(Px2);df2
part1 = diag((Y%*% Px2%*%t(Y)))/df1
part2 = diag((t(Y)%*% Px%*%Y))/df2


#####################################
# Load required libraries
library(MASS)

# Set the number of samples
n <- 5000

# Define parameters
mu <- rep(0, 8)
I8 <- diag(8)

# Generate Y from a multivariate normal distribution
Y <- MASS::mvrnorm(n, mu, I8)

# Calculate PX1 and PX2 matrices
X <- MASS::mvrnorm(n, mu, I8)
PX1 <- X[, 1:4];PX1
PX2 <- I8 - PX1;PX2

# Calculate degrees of freedom
df1 <- min(rank(PX2), ncol(PX2))
df2 <- min(rank(PX1), ncol(PX1))

# Calculate the two parts of the formula
part1 <- sum(diag(Y %*% PX2 %*% t(Y))) / df1
part2 <- sum(diag(Y %*% PX1 %*% t(Y))) / df2

# Calculate the F-distributed samples
f_samples <- part1 / part2

# Print the first few samples for demonstration
print(f_samples[1:10])
