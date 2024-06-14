# Zadanie 1
# Load necessary library
library(MASS)

# Define the parameters
mu_x <- 1/2
mu_y <- 1/4
sigma_x <- 1
sigma_y <- 1.5
rho <- 0.3

x <- seq(-5, 5, length.out = 50)
y <- seq(-5, 5, length.out = 50)

grid <- expand.grid(x = x, y = y)

bivariate_normal_density <- function(x, y, mu_x, mu_y, sigma_x, sigma_y, rho) {
  z <- (1 / (2 * pi * sigma_x * sigma_y)) *
    exp(-1 / (2 * (1 - rho^2)) *
          (((x - mu_x)^2 / sigma_x^2) -
             (2 * rho * (x - mu_x) * (y - mu_y) / (sigma_x * sigma_y)) +
             ((y - mu_y)^2 / sigma_y^2)))
  return(z)
}

grid$z <- mapply(bivariate_normal_density, grid$x, grid$y, 
                 MoreArgs = list(mu_x = mu_x, mu_y = mu_y, sigma_x = sigma_x, sigma_y = sigma_y, rho = rho))

z_matrix <- matrix(grid$z, nrow = 50, ncol = 50)

# Plot the surface
persp(x, y, z_matrix, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      xlab = "X", ylab = "Y", zlab = "Density", main = "Bivariate Normal Distribution")

# Zadanie 2

men <- c(9.88, 9.97, 10, 10.02, 10.04, 10.06, 10.07, 10.08, 10.09, 10.09, 10.11, 10.11, 10.12, 10.13, 10.13, 10.14, 10.16, 10.16, 10.19, 10.20, 10.21, 10.22, 10.22, 10.69, 10.90)
women <- c(10.83, 10.93, 10.94, 10.96, 10.97, 10.97, 10.99, 11, 11.01, 11.04, 11.06, 11.07, 11.07, 11.08, 11.12, 11.13, 11.14, 11.18, 11.41, 11.43, 11.46, 11.48, 11.52, 11.56, 11.48, 11.62, 11.70, 11.86, 11.98, 12.06, 12.35)

shapiro.test(men)
shapiro.test(women)

# JAK NIE P > 0.05 TO ZAMIAST VAR.TEST TEST LEVENA
library(lawstat)

ramka = data.frame(
  values = c(men, women),
  flags = c(rep(1, length(men)), rep(2,length(women)))
)
levene.test(ramka$values, ramka$flags)

boxplot(men, women, boxwex=0.6, horizontal=T, col='green', cex.axis=0.8, names=c('men', 'women'), las=1, main='bieg na 100m, Londyn 2012')

# Zadanie 3

Tech1 <- c(63, 39, 66, 65, 60, 43, 37)
Tech2 <- c(35, 54, 38, 25, 24, 22, 37)
Tech3 <- c(75, 62, 42, 43, 27, 81, 66)
Tech4 <- c(69, 58, 40, 68, 51, 25, 23)
Tech5 <- c(38, 32, 32, 59, 25, 38, 32)

# Jesli przejda test shapira to bartlett
shapiro.test(Tech1)
shapiro.test(Tech2)
shapiro.test(Tech3)
shapiro.test(Tech4)
shapiro.test(Tech5)

# jesli chociaz 1 nie przechodzi to fligner.test

fligner.test(list(Tech1, Tech2, Tech3, Tech4, Tech5))

# Combine data into a data frame
# data <- data.frame(value = c(Tech1, Tech2, Tech3, Tech4, Tech5),
# group = factor(rep(1:5, each = 7)))

# Bartlett test for equal variances
# bartlett.test(value ~ group, data = data)


# Zadanie 4
public <- c(4.2, 6.1, 4.9, 8.5, 4.6, 9.1, 7.7, 6.5, 6.2, 10.2, 11.6, 10.4, 5, 10.4, 8.1)
private <- c(13, 18.8, 13.2, 14.4, 17.7, 17.7, 17.6, 19.8, 16.8, 16.1)

# jak porównujemy średnie to trzeba najpierw porównać wariancje
var.test(public, private) # domyślne wartości ratio = 1, alternative = 'two.sided', conf.level = 0.95

library(PASWR2)
# t.test(private, public, alternative = "greater", mu = 10)

tsum.test(mean.x = mean(public), s.x = sd(public), n.x = length(public),
          mean.y = mean(private), s.y = sd(private), n.y = length(private))


# zad 5
poZapyleniu = c(0.78, 0.76, 0.43, 0.92, 0.86, 0.59, 0.68)
bezZapylenia = c(0.21, 0.62, 0.42, 0.29, 0.30, 0.60, 0.14)

# H0: mi2 = mi1
# H1: mi2 > mi1 <=> mi1-mi2 < 0

t.test(poZapyleniu, bezZapylenia,
       mu = 0,
       alternative = 'less',
       paired = T)


# zad 7
# H0: O1 = O2 = O3 = O4
# H1: ~H0

prop.test(
  c(16,23,35,27), # wektor liczb sukcesów
  c(28,42,60,51) # wektor liczebności serii doświadczeń
)
# p-value = 0.9476 > 0.05 -> nie ma podstaw do odrzucenia H0


# zad 6
# H0: O1 = 0.5 && O2 = 0.4 && O3 = 0.55
# H1: ~H0

prop.test(
  c(21, 17, 30),
  c(36, 45, 52),
  p = c(0.5, 0.4, 0.55)
)
# p-value = 0.7423 > 0.05 -> nie ma podstaw do odrzucenia H0



# proba losowa:   (X1, X2, ..., Xn) -> i.i.d.
# statystyka:     dowolna funkcja proby losowej f(X1, ..., Xn)
# O - (teta) nieznany parametr w rozkładzie
# 
# estymator nieznanego parametru teta - dowolna statystyka ^On(X1,...,Xn), której wartosci pozwalają ocenić/przybliżyc nieznaną wartość parametru teta
#
# zgodność estymatora
# nieobciążoność estymatora (nieobciążoność asymptotyczna)
#
#
# przedział ufności dla nieznanego parametru teta (na poziomie ufności 1-alfa)
#   jest to losowy przedział (O1(X1,..,Xn), O2(X1,..,Xn)) spełniający warunki:
#   1. końce przedziału są zm.losowymi niezależącymi od teta
#   2. P(O1(X1,..,Xn) < O < O2(X1,..,Xn)) = 1-alfa

# zad 9
# X ~ N(mu, sigma)
# przedział ufności dla nieznanej średniej SS





# zad 8 - w domu



















