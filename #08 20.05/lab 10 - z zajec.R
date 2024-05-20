# zad 1
library(TeachingDemos)
# Dane
low = c(1004, 832, 545)
medium = c(880, 732, 904)
high = c(928, 801, 1014)

# Tworzenie tabeli kontyngencji
data = matrix(c(low, medium, high),
              nrow = 3, byrow = TRUE,
              dimnames = list(Stężenie = c("Niskie", "Średnie", "Wysokie"),
                               Choroba = c("A", "B", "C")))
# Wyświetlanie tabeli kontyngencji
print(data)

chisq.detail(data) # p-value < 0.05 -> odrzucamy H0 -> są zależne


# zad 2
a = rchisq(n = 1020, df = 15)
b = rchisq(n = 1100, df = 5)
c = rchisq(n = 1180, df = 10)
d = rchisq(n = 1200, df = 15)

list_ = list(a, b, c, d)
kruskal.test(list_) # p-value < 0.05 -> odrzucamy H0


# zad 4
frame_ = data.frame(
  Miasto = c('Wrocław', 'Poznań', 'Szczecin', 'Gdańsk', 'Warszawa', 'Kraków'),
  Kobiety = c(350, 484, 196, 320, 560, 731),
  Mężczyźni = c(370, 492, 174, 350, 530, 710))

frame_$Razem = apply(
  frame_[, c(2, 3)], # zastosuj funkcję na kolumnie 2 i 3
  1, # 1 - idź po wierszach, 2 - idź po kolumnach
  sum) # funkcja jaka ma być wykonana

frame_

library(plotrix)

attach(frame_)
pyramid.plot(Kobiety, Mężczyźni,
             labels = Miasto,
             gap = 250,
             top.labels = c('Kobiety', 'Miasto', 'Mężczyźni'),
             main = 'Wykres piramidowy',
             space = 0.3,
             laxlab = c(0, 400, 800),
             raxlab = c(0, 400, 800),
             show.values = T,
             unit = '')
detach(frame_)


# zad 7
par(pty = 's')
plot(
  c(1,-1,-1,1),
  c(1,1,-1,-1),
  xlim = c(-2,2),
  ylim = c(-2,2),
  pch = 19,
  las = 1
)
abline(h = 0)
abline(v = 0)

arrows(0, 0, 0.93, -0.93, length = 0.1)
arrows(0, 0, 0.93, 0.93, length = 0.1)
arrows(0, 0, -0.93, 0.93, length = 0.1)
arrows(0, 0, -0.93, -0.93, length = 0.1)

text(1,1, 'p = 1/4', pos = 4)
text(-1,-1, 'p = 1/4', pos = 4)
text(-1,1, 'p = 1/4', pos = 4)
text(1,-1, 'p = 1/4', pos = 4)



