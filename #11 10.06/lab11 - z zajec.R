# Laboratorium 13

# zad 9
library(gplots)

# n liczba pobranych prób
# k liczebnosc kazdej proby
# m srednia w rozkladzie z którego pobrano próbe
# s odchylenie standardowew rozkładzie z którego pobrano próbke
puSrednia = function(n, k, m, s) {
  matr = t(replicate(n, rnorm(k, mean=m,sd=s)))
  means = rowMeans(matr)
  sds = c()
  for(i in 1:n) {
    sds[i] = sd(matr[i, ])
  }
  upper = means + qt(0.975, n-1) * sds / sqrt(n-1)
  lower = means - qt(0.975, n-1) * sds / sqrt(n-1)
  
  # rysowanie wykresu
  plotCI(
    1:n,
    means,
    ui = upper,
    li = lower,
    las = 1,
  )
  abline(h = m, col='red')
}

puSrednia(100, 200, 3, 1.5)
# na 100 probach spodziewamy się 5 słupków, które będą poza przedziałem ufności, ze względu na poziom ufności = 0.95



# Laboratorium 14

# zad 1
# H0: teta1 = teta2
# H1: ~H0

prop.test(
  c(705-450, 1320-517), # wektor liczb sukcesów
  c(700, 1320) # wektor liczebności prób
)
# p-value < 0.05 -> odrzucamy hipotezę podstawową H0 na rzecz alternatywnej H1


# zad 2

#    drogie          regularne      tanie     bardzo tanie
#      24                38           70          68
#
# 1/3 P = 1/30      2/3 P = 2/30    P=1/10      P=1/10
#
# H0:
#  O1 = 1/30         O2 = 2/30     O3 = 1/10    O4 = 1/10
# H1: ~H0

braki = c(24, 38, 70, 68)
prop.test(
  braki,
  rep(225, 4),
  p = c(1/30, 2/30, 1/10, 1/10)
)
# p-value < 0.05 -> odrzucamy hipotezę podstawową H0 na rzecz alternatywnej H1


# zad 3
library(gplots)

# n liczba pobranych prób
# k liczebnosc kazdej proby
# m srednia w rozkladzie z którego pobrano próbe
# s odchylenie standardowew rozkładzie z którego pobrano próbke
puWariancja = function(n, k, m, s) {
  matr = t(replicate(n, rnorm(k, mean=m,sd=s)))
  vars = c()
  for(i in 1:n) {
    vars[i] = var(matr[i, ])
  }
  # 0.975 -> 1-a/2 -> dla standardowego poziomu ufności 0.95
  lower = vars * (n-1) / qchisq(0.975, n-1)
  upper = vars * (n-1) / qchisq(0.025, n-1)
  
  # rysowanie wykresu
  plotCI(
    1:n,
    vars,
    ui = upper,
    li = lower,
  )
  abline(h = s^2, col='red')
}

puWariancja(20, 100, 3, 1.5)


# zad 4
# ANALIZA WARIANCJI
# H0: mu1 = mu2 = mu3 = ... = mun
# H1: ~H0

polska = c(23.5, 25, 24, 27, 29, 22.5, 28, 30.5, 31)
niemcy = c(22, 24.5, 23.5, 28, 32, 30.5, 29.5)
usa = c(28, 26.5, 24, 25.5, 23.5, 29, 30.5, 26, 26, 32.5)
norwegia = c(25, 26.5, 30, 27, 24.5, 25, 23, 30, 29.5)

ramka = data.frame(
  czas = c(polska, niemcy, usa, norwegia),
  kraj = c(rep('polska', length(polska)),
           rep('niemcy', length(niemcy)), 
           rep('usa', length(usa)), 
           rep('norwegia', length(norwegia)))
)

# narysowanie wykresu pudełkowego
library(ggplot2)
qplot(kraj,
      czas,
      data = ramka,
      geom = 'boxplot',
      fill = 'kraj')

# analiza wariancji
analiza = aov(
  czas ~ kraj,
  data = ramka
)
summary(analiza)
# Pr(>F) = 0.982 > 0.05 -> nie ma podstaw do odrzucenia H0


# zad 5
I = c(24, 56, 34, 25, 42, 20, 68, 53, 34)
II = c(35, 76, 56, 44, 38, 25, 51, 36, 72, 29, 33)
III = c(40, 51, 63, 39, 54, 49, 50, 62)
IV = c(50, 80, 38, 48, 58, 74, 75, 49, 83)
V = c(40, 80, 44, 75, 64, 65, 37, 61, 38, 47, 55, 29)
ramka = data.frame(
  dane = c(I, II, III, IV, V),
  flagi = c(rep('I', length(I)),
            rep('II', length(II)),
            rep('III', length(III)),
            rep('IV', length(IV)),
            rep('V', length(V)))
)

# narysowanie wykresu pudełkowego
qplot(flagi,
      dane,
      data = ramka,
      geom = 'boxplot',
      fill = 'dane')

# analiza wariancji
analiza = aov(
  dane ~ flagi,
  data = ramka
)
summary(analiza)
# Pr(>F) = 0.04 < 0.05 -> odrzucamy H0 na rzecz H1

# test porównań wielokrotnych
pairwise.t.test(
  ramka$dane, ramka$flagi,
  p.adjust.method = p.adjust.methods
)


# zad 6
# TODO





