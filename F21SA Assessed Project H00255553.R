# Setting working directory to where .csv file is stored
setwd("C:/Users/jonat/OneDrive - Heriot-Watt University/Own Uni/PGY2/F21SA Statistical Modelling and Analysis/CW")
library(VGAM)
# Reading and storing csv
wind_data <- read.csv("wind.csv", header = TRUE, row.names = 1)
n <- length(wind_data$x)

# Question 1
# Max speed
max_speed <- max(wind_data$x)
# Histogram
par(mfrow=c(1,1), col.main ="black", col.lab='black')
hist(wind_data$x, 
     main = "Histogram of Wind Speeds", 
     xlab = "Wind Speeds (MPH)",
     breaks = seq(from = 0, to = round(max_speed) + 5, by = 3))
# Summary statistics
summary(wind_data)
sd_ori <- sd(wind_data$x); sd_ori
IQR(wind_data$x)
quantile(wind_data$x, c(0.025, 0.975))

# Question 2
# (1/2n * (sum of (x_i^2)))^(0.5)
sigma_hat_mle <- sqrt(sum(wind_data$x^2)/(2*n))

# Question 3
sqrt_inverse_i <- sqrt((sigma_hat_mle^2)/(4))

# Question 4
ci <- c(0,0)
z_025 <- qnorm(p = 0.025, lower.tail = FALSE)
var_change <- z_025*(sqrt_inverse_i)
ci[1] <- sigma_hat_mle - var_change
ci[2] <- sigma_hat_mle + var_change
ci_range <- ci[2] - ci[1]
ci

# Question 5, 6, 7
# Simulation by 10,000 times
m = 10000
# Y is an array of m number, where it stores the 
### simulated mean value of wind speeds for the next
### 1000 days for each iteration (Q5)
Y <- numeric(m)
Y_CI_L <- numeric(m)
Y_CI_U <- numeric(m)
# SD_Y stores the binary values of 1, and 0, where 1,
### if the standard deviation of simulated values of
### 
P_SD_Y <- numeric(m)
P_CI_L <- numeric(m)
P_CI_U <- numeric(m)
for (i in 1:m){
  X_sigma <- rrayleigh(n, scale = sigma_hat_mle)
  X_CI_L <- rrayleigh(n, scale = ci[1])
  X_CI_U <- rrayleigh(n, scale = ci[2])
  Y[i] <- mean(X_sigma)
  Y_CI_L[i] <- mean(X_CI_L)
  Y_CI_U[i] <- mean(X_CI_U)
  if (sd(X_sigma) > sd_ori){
    P_SD_Y[i] <- 1
  }
  if (sd(X_CI_L) > sd_ori){
    P_CI_L[i] <- 1
  }
  if (sd(Y_CI_U) > sd_ori){
    P_CI_U[i] <- 1
  }
}
p_sd_greater <- sum(P_SD_Y)/length(P_SD_Y)
P_CI_L_greater <- sum(P_CI_L)/length(P_CI_L)
P_CI_U_greater <- sum(P_CI_U)/length(P_CI_U)

# Plotting stacked/overlapped histograms for all three Y, Y_CI_L, and Y_CI_U
par(mfrow=c(1,1), col.main ="white", col.lab='white')
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
opar <- par(lwd=0.01)
hgY <- hist(Y, plot = FALSE)
hgCIL <- hist(Y_CI_L, plot = FALSE)
hgCIU <- hist(Y_CI_U, plot = FALSE)
plot(hgY, col = 'red', xlim=c(0,30))
plot(hgCIL, add=TRUE, col=rgb(0,0,1,1/4), xlim=c(0,30))
plot(hgCIU, add=TRUE, col=rgb(1,0,0,1/4), xlim=c(0,30))
title(main = "Histogram of SD of Mean, lower CI, and Upper CI", xlab = "Wind SPeeds (MPH)",
      col.main="black", col.lab="black", ylab = "frequency")
legend ( "bottomleft", legend =c("mean", "lower CI" , "upper CI"), col=c('red', rgb(0,0,1,1/4), rgb(1,0,0,1/4)), lty = 1, lwd =2 )
par(opar)

# Individual Plots for the previous
par(mfrow=c(1,3), col.lab = "black", col.main = 'black')
# text(1,1,"First title",cex=2,font=2)
hist(Y_CI_L, main = "lower CI")
hist(Y, main = "mean")
hist(Y_CI_U, main = "upper CI")
