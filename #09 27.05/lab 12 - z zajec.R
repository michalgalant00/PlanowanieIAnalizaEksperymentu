# lab 12

# zad 1
library(lawstat) # do levene.test

# 18-25 lat
x = c(285, 290, 311, 296, 240, 245, 283, 256, 270, 285, 230, 195, 210)
# 26-30 lat
y = c(295, 307, 301, 280, 275, 280, 285, 285, 305, 275, 308, 300, 310, 282, 315, 300, 304, 293, 296)
# 31-40 lat
z = c(180, 320, 280, 225, 305, 15, 290, 215, 220, 400, 0, 332, 198, 200, 255, 270, 310, 280, 10, 328, 430)

# H0: wariancja x ^2 = wariancja y ^2 = wariancja z ^2

shapiro.test(x) # p-value > 0.05 -> OK
shapiro.test(y) # p-value > 0.05 -> OK
shapiro.test(z) # p-value < 0.05 -> NIE PRZESZŁO

# skoro przynajmniej jedna proba nie przeszła testu shapiro-wilka na sprawdzenie, czy proba pochodzi z rozkladu normalnego to stosowany jest test Flignera
fligner.test(list(x,y,z))
# p-value < 0.05 -> odrzucamy H0

# przeprowadzamy test Levene'a
ramka = data.frame(
  values = c(x,y,z),
  flags = c(rep(1,length(x)), rep(2,length(y)), rep(3,length(z)))
)

levene.test(ramka$values, ramka$flags)
# p-value < 0.05 -> odrzucamy H0



# testy istotności dla średnich
# Populacja 1  , Populacja 2
#   cecha X    ,   cecha Y
# 
# badamy niezależność cech X i Y
# H0: średnie są takie same: mi1 = mi2
# H1: mi1 > mi2  greater
#     mi1 < mi2  less
#     mi1 != mi2 two.sided
# 
# próby muszą przejść test na pochodzenie z r.normalnego (Shapiro-Wilka)
#
#
#
# Model 1: znane są obie wariancje (nie zbiorów - to można policzyć - a rozkładów, z których pochodzą)
# zsum.test(mean.x = mean(x),
#           sigma.x = sqrt(wariancja1), # odchylenie std w rozkładzie 1
#           n.x = liczebnosc zbioru 1,
#           mean.y = mean(y),
#           sigma.y = sqrt(wariancja1), # odchylenie std w rozkładzie 2
#           n.y = liczebnosc zbioru 2,
#           conf.level = ...,
#           alternative = greater/less/two.sided,
#           mu = roznica miedzy srednia1 a srednia2 (domyslnie 0)
#             )

# zad 2
library(PASWR2)

x = c(176, 168, 181, 157, 180, 172, 176, 169, 190, 183, 170, 182, 173, 175, 182, 166)
y = c(164, 182, 170, 171, 181, 191, 173, 169, 184, 181, 175, 177, 169, 178)

shapiro.test(x)
shapiro.test(y)
# obie próby pochodzą z r.norm

# H0: mi1 = mi2+3
# H1: mi1 - mi2 > 3 -> 'greater'

# wariancjaX = 10.6 -> odchylenie std = sqrt(10.6)
# wariancjaY = 8.2 -> odchylenie std = sqrt(8.2)
zsum.test(mean.x = mean(x), sigma.x = sqrt(10.6), n.x = length(x),
          mean.y = mean(y), sigma.y = sqrt(8.2), n.y = length(y),
          conf.level = 0.99,
          alternative = 'greater', mu = 3)
# p-value = 0.9999 > 0.01 -> nie ma podstaw by odrzucić H0



# Model 2: nie są znane wariancje, ale są równe
# tsum.test(mean.x = mean(x),
#           s.x = sd(x), # nie jest znana wariancja -> liczymy odchylenie z próby 1
#           n.x = length(x),
#           mean.y = mean(y),
#           s.y = sd(y), # nie jest znana wariancja -> liczymy odchylenie z próby 2
#           n.y = length(y),
#           conf.level = ...,
#           alternative = ...,
#           mu = ...,
#           var.equal = TRUE # USTAWIANE NA TRUE
# )

# zad 3
x = c(176, 168, 181, 157, 180, 172, 176, 169, 190, 183, 170, 182, 173, 175, 182, 166)
y = c(164, 182, 170, 171, 181, 191, 173, 169, 184, 181, 175, 177, 169, 178)

# H0: mi1 = mi2+3
# H1: mi1 != mi2 -> 'two.sided'

# wcześniej trzeba sprawdzić, czy wariancje są równe:
# próby muszą pochodzić z r.norm -> pochodzą
shapiro.test(x)
shapiro.test(y)
# test równości wariancji
var.test(x,y,
         conf.level = 0.95, # wartość domyślna
         alternative = 'two.sided' # wartość domyślna
         )
# p-value > 0.05 -> nie ma powodu do odrzucenia H pomocniczej (wariancje są równe)

# teraz przejście do właściwego testu
tsum.test(mean.x = mean(x), s.x = sd(x), n.x = length(x),
          mean.y = mean(y), s.y = sd(y), n.y = length(y),
          conf.level = 0.95,
          alternative = 'two.sided', mu = 3, var.equal = TRUE)
# p-value = 0.1609 > 0.05 -> nie ma podstaw by odrzucić H0



# Model 3: nie znamy wariancji i są różne
# tsum.test(mean.x = mean(x),
#           s.x = sd(x), # nie jest znana wariancja -> liczymy odchylenie z próby 1
#           n.x = length(x),
#           mean.y = mean(y),
#           s.y = sd(y), # nie jest znana wariancja -> liczymy odchylenie z próby 2
#           n.y = length(y),
#           conf.level = ...,
#           alternative = ...,
#           mu = ...,
#           var.equal = FALSE # USTAWIANE NA FALSE
# )

# zad 4
# niezaszczepieni
x = c(4, 6, 8, 7, 6, 5, 4, 6, 9, 9, 10, 2, 11, 12, 9, 9, 7, 9, 6, 6, 6, 14, 8, 16, 9, 11, 20, 2)
# zaszczepieni
y = c(4, 7, 5, 3, 11, 4, 8, 7, 5, 7, 4, 3, 2, 1, 5, 3, 10, 2, 10, 3, 5, 4, 8, 9, 5, 6, 4)

# H0: mi1 = mi2+1.5
# H1: mi1 - mi2 > 1.5

shapiro.test(x)
shapiro.test(y)
# obie próby przechodzą test Shapiro-Wilka
# test równości wariancji
var.test(x,y)
# p-value < 0.05 -> odrzucamy H pomocniczą (wariancje są rowne), na korzyść H pomocniczej alternatywnej, że wariancje nie są sobie równe

# teraz przejście do właściwego testu
tsum.test(mean.x = mean(x), s.x = sd(x), n.x = length(x),
          mean.y = mean(y), s.y = sd(y), n.y = length(y),
          conf.level = 0.95,
          alternative = 'greater', mu = 1.5, var.equal = FALSE)
# p-value = 0.06772 > 0.05 -> nie ma podstaw by odrzucić H0



# Model 4: próby pobrane są z tej samej populacji
# t.test(x, # populacja 'przed'
#        y, # populacja 'po'
#        conf.level = ...,
#        alternative = ...,
#        mu = mi1-mi2,
#        paired = TRUE
#          )

# zad 5
x = c(23, 45, 34, 35, 24, 44, 65, 45, 29, 63, 40, 28, 30, 19, 21)
y = c(34, 50, 24, 33, 22, 67, 51, 49, 48, 51, 18, 17, 30, 28, 20)

# H0: mi1 = mi2
# H1: mi1 < mi2 -> less

shapiro.test(x)
shapiro.test(y)

t.test(
  x, y,
  conf.level = 0.95,
  alternative = 'less',
  mu = 0,
  paired = TRUE
)
# p-value = 0.52 > 0.5 -> nie ma podstaw do odrzucenia H0



# zad 7
startX = -2
endX = 2
v1 = seq(startX, endX, length = 20)
v2 = seq(startX, endX, length = 20)

f = function(x, y) {
  exp(x^2 - sqrt(y^2 + 4)) - log(abs(x)^2 + 1)
}

m = outer(v1, v2, FUN = f)

persp(
  v1, v2, m,
  theta = 35, # obrót prawo-lewo
  phi = 30, # obrót góra-dół
  expand = 0.5, # wysokość wykresu
  col = 7, # kolor
  ticktype = 'simple', # simple/detail -> podpisy na wykresie
  box = T # T/F -> czy jest zamknięty
)


















