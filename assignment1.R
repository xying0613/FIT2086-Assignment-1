setwd("C:/Users/User/Documents/YEAR 2 SEM 2/FIT2086/Assignment")
heart <- read.csv("heart.disease.csv", header = TRUE)
nrow(heart[,])
nrow(heart[heart$H == 0 & heart$T == 0,])
nrow(heart[heart$H == 0 & heart$T == 1,])
nrow(heart[heart$H == 1 & heart$T == 0,])
nrow(heart[heart$H == 1 & heart$T == 1,])

#xx <- seq(from=-3, to=3, length.out=100)
#plot(x=xx, y=dnorm(x=xx, mean=0, sd=sqrt(1/6)), xlim = c(-3,3), ylim = c(0,1), type = "l", xlab="x", ylab="P(X=x|s)", 
#     main="Probability density function of X", col="red")
#lines(x=xx, y=dnorm(x=xx, mean=0, sd=sqrt(2/3)), type = "l", col = "blue")
#legend(x=-3, y=1, legend=c("s=1","s=2"), col=c("red","blue"), lwd=2, lty=c(1, 1))

f1 <- function(x){
  (1-abs(x))/(1^2)
}

f2 <- function(x){
  (2-abs(x))/(2^2)
}

curve(f1, from=-3, to=3, xlab="x", ylab="P(X=x|s)", main="Probability density function of X", col="red")

curve(f2, from=-3, to=3, col="blue", add=TRUE)

legend(x=-3, y=1, legend=c("s=1","s=2"), col=c("red","blue"), lwd=2, lty=c(1, 1))



height <- read.csv("heights.hk.csv", header = TRUE)

my_estimates <- function(X)
{
  n = length(X)
  
  retval = list()
  
  # Calculate the sample mean
  retval$mu_ml = sum(X)/n
  
  # Calculate the squared deviations around the mean
  e2 = (X - retval$m)^2
  
  # Calculate the two estimates of variance
  retval$var_ml = sum(e2)/n
  retval$var_u  = sum(e2)/(n-1)
  
  return(retval)
}

est <- my_estimates(height$Height)

est$mu_ml
est$var_u

xseq <- seq(from=1, to=2, length.out=100)
plot(x=xseq, y=dnorm(x=xseq, mean=est$mu_ml, sd=sqrt(est$var_u)), type = "l", xlab="x (Height in m)", 
     ylab="P(X=x)", main="Normal Distribution to the height data", col="blue")


pnorm(1.65,est$mu_ml,sqrt(est$var_u))
pnorm(1.75,est$mu_ml,sqrt(est$var_u))-pnorm(1.65,est$mu_ml,sqrt(est$var_u))
1-pnorm(1.75,est$mu_ml,sqrt(est$var_u))-(1-pnorm(1.85,est$mu_ml,sqrt(est$var_u)))
1-pnorm(1.85,est$mu_ml,sqrt(est$var_u))

zero <- dbinom(0,18,1-pnorm(1.8,est$mu_ml,sqrt(est$var_u)))
one <- dbinom(1,18,1-pnorm(1.8,est$mu_ml,sqrt(est$var_u)))
two <- dbinom(2,18,1-pnorm(1.8,est$mu_ml,sqrt(est$var_u)))
three <- dbinom(3,18,1-pnorm(1.8,est$mu_ml,sqrt(est$var_u)))
1-(zero+one+two+three)

xseq <- seq(from=1, to=2, length.out=100)
hist(height$Height, prob=TRUE, ylim=c(0,9), xlab="Height(m)", ylab="Probability density",
     main="Probability of different heights against probability density")
lines(x=xseq, y=dnorm(x=xseq, mean=est$mu_ml, sd=sqrt(est$var_u)), type = "l", col="blue")
