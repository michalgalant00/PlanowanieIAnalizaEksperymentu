# wstęp
# ====

# testy zgodnosci - H0: próba ma jakiś rozkład
#       chi-kwadrat Pearsona
#       Kołmogrowa-Smirnova
#       Shapiro-Wilka
#       Kruskal-Wallis - dla liczby prób >= 3
#
# testy losowosci - H0: próba ma charakter losowy
#       serii Walda-Wolfowitza
#
# testy niezależności dwoch cech - H0: cecha X jest niezależna od cechy Y
#       chi-kwadrat Pearsona (zwykły)
#       chi-kwadrat Pearsona (dokładny, z pakietu TeachingDemos)
#       Fishera
#
# testy istotnosci - H0: np wariancja ma jakąś wartość 

# ====


# zadania
# ====

# Zadanie 1
library(randtests)

# H0: próba jest losowa
a = c(5, 17, 21, 29, 33, 45, 56, 66, 72, 88)
b = c(45, 12, 77, 64, 4, 93, 21, 37, 90, 95)
c = c(63, 90, 47, 16, 86, 74, 97, 13, 26, 3)
d = c(1, 81, 11, 21, 91, 71, 31, 61, 41, 51)

runs.test(a) # p-value = 0.00729 < 0.05 -> odrzucamy H0
runs.test(b) # p-value = 1 > 0.05 -> nie ma podstaw do odrzucenia H0
runs.test(c) # p-value = 0.1797 > 0.05 -> nie ma podstaw do odrzucenia H0
runs.test(d) # p-value = 0.1797 > 0.05 -> nie ma podstaw do odrzucenia H0


# Zadanie 2

# H0: próbki mają ten sam rozkład
wzr_1 = c(176, 182.5, 166, 175, 175.5, 161.5, 173, 165, 186, 170.5, 158, 163.5)
wzr_2 = c(168, 172, 163, 171.5, 177, 190, 172.5, 164, 183.5, 171, 157.5, 166)

# walidacyjnie
ks.test(wzr_1, wzr_2) # p-value = 0.869 > 0.05 -> nie ma podstaw do odrzucenia H0

# utworzenie ramki danych z połączonymi danymi i oflagowanie ich
frame_ = data.frame(
  height = c(wzr_1, wzr_2),
  year = c(rep(1, length(wzr_1)),
           rep(2, length(wzr_2)))
)

# posortowanie danych po wzroście
frame_ = frame_[order(frame_$height), ] 

runs.test(frame_$year) # p-value = 0.4038 > 0.05 -> nie ma podstaw do odrzucenia H0


# Zadanie 3

# H0: częstotliwość chodzenia do teatru jest niezależna od wykształcenia
# macierz kontyngencji
#             N Ś  W
m = matrix(c(23,15,8 ,  # 0
             18,30,22,  # 1
             28,11,31), # 2
           nrow = 3, ncol = 3)

chisq.test(m) # p-value = 0.0006137 < 0.05 -> odrzucamy H0

library(TeachingDemos)
chisq.detail(m) # P-value = 0.001 < 0.05 -> odrzucamy H0


# Zadanie 4

# H0: częstotliwość mycia zębów jest niezależna od płci
m = matrix(c(4,2,5,
             6,4,1),
           nrow = 2, ncol = 3)

fisher.test(m) # p-value = 0.5356 > 0.05 -> nie ma podstaw do odrzucenia H0


# Zadanie 5

fun_ = function(x) {
  return(x * sin(x))
}

# rysowanie funkcją curve
curve(fun, n = 500,
      from = -30, to = 30,
      type = 'l',
      ylim = c(-50, 50),
      las = 1, # legend axis
      bty = 'o', # border type
      lty = 1, # line type
      main = 'y = x*sin(x)',
      xlab = 'x', ylab = 'y')
abline(0, 1, col = 'red', lty = 2)
abline(0, -1, col = 'red', lty = 2)

# rysowanie przez ggplot2
library(ggplot2)

args_ = seq(-30, 30, by = 0.01)
frame_ = data.frame(args = args_,
                    vals = fun_(args_))

base = ggplot(frame_, aes(x = args, y = vals)) # podstawa wykresu
plot = base +
  geom_line() +
  ggtitle('y = x*sin(x)')
plot # wyświetlenie wykresu

# TODO dokończyć zad5


# Zadanie 6

# H0: F1 = F2 = F3 = F4

x1 = c(11.2, 5, 45, 3.42, 24, 54, 33.5, 18)
x2 = c(72.5, 6, 22.4, 70, 101, 23, 42, 33)
x3 = c(32, 44.6, 44.9, 64, 2.3, 8, 93, 100, 22, 34)
x4 = c(20, 19, 18, 2, 3.6, 4.6, 8)

list_ = list(x1, x2, x3, x4)
kruskal.test(list_) # p-value = 0.02251 < 0.05 -> odrzucamy H0

boxplot(list_, boxwex=0.8, las=1, col=4)




