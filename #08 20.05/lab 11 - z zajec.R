# zad 1
generujTrajektorie = function(n) {
  h = sample(c(-1,1), n, replace=T)
  v = sample(c(-1,1), n, replace=T)
  fc = cumsum(h)
  sc = cumsum(v)
  
  xlim = c(min(fc)-1, max(fc)+1)
  ylim = c(min(sc)-1, max(sc)+1)
  
  par(pty = 's')
  plot(
    fc,
    sc,
    xlim = xlim,
    ylim = ylim,
    type = 'o',
    pch = 19,
    las = 1
  )
  abline(v=0)
  abline(h=0)
}

generujTrajektorie(50)
generujTrajektorie(150)
generujTrajektorie(500)


# zad 2
frame_ = data.frame(
  wojewodztwo = c('dolnośląskie', 'kujawsko-pomorskie', 'lubelskie', 'lubuskie', 'łódzkie', 'małopolskie', 'mazowieckie'),
  powierzchnia = c(19947, 17972, 25122, 13988, 18219, 15183, 35558), # tys. km2
  ludnosc = c(2.9, 2.1, 2.1, 1.0, 2.5, 3.4, 5.4), # mln
  liczbaMiast = c(91, 52, 48, 43, 46, 62, 89)
)

library(scales)
frame_$powierzchniaProcentowo = percent(round(frame_$powierzchnia/sum(frame_$powierzchnia), 2))
frame_$ludnoscProcentowo = percent(round(frame_$ludnosc/sum(frame_$ludnosc), 2))
frame_$liczbaMiastProcentowo = percent(round(frame_$liczbaMiast/sum(frame_$liczbaMiast), 2))

library(plotrix)
attach(frame_)
par(pty='s')
pie3D(powierzchnia,
      radius = 0.96, theta = 0.6,
      main = 'województwa, powierzchnia',
      cex.main = 1.2,
      col = rainbow(length(powierzchnia)),
      labels = powierzchniaProcentowo,
      labelcex = 0.7,
      explode = 0.06
      )

pie3D(ludnosc,
      radius = 0.96, theta = 0.6,
      main = 'województwa, ludność',
      cex.main = 1.2,
      col = rainbow(length(ludnosc)),
      labels = ludnoscProcentowo,
      labelcex = 0.7,
      explode = 0.06
      )

pie3D(liczbaMiast,
      radius = 0.96, theta = 0.6,
      main = 'województwa, liczba miast',
      cex.main = 1.2,
      col = rainbow(length(liczbaMiast)),
      labels = liczbaMiastProcentowo,
      labelcex = 0.7,
      explode = 0.06
      )

legend(-2, -1.6,
       legend = wojewodztwo,
       cex = 0.6,
       title = 'województwo',
       fill = rainbow(length(powierzchnia)),
       xpd = T,
       xjust = -1.7,
       yjust = -0.2,
       bty = 'n')
detach(frame_)


# zad 3
# dolnoslaskie
d = c(122, 98, 101, 74, 94, 130, 121, 128, 69, 92, 133, 105, 89, 93)
# mazowieckie
m = c(99, 102, 84, 77, 84, 138, 112, 72, 67, 93, 100, 124, 120, 88, 75, 95, 109, 80, 114)

# sprawdzenie, czy próby pochodzą z rozkładu normalnego
shapiro.test(d) # p-value > 0.05 -> nie ma podstaw, żeby odrzucić H0
shapiro.test(m) # p-value > 0.05 -> nie ma podstaw, żeby odrzucić H0

# jeżeli obie próby pochodzą z r. normalnego to można zastosować var.test

# a
var.test(d, m, ratio = 1,
         alternative = 'greater',
         conf.level = 0.9)
# przy zmianie conf.level p-value też się zmienia: tu przy conf.level = 0.9, p-value = 1-0.9 = 0.1 -> wyniki mniejsze niz 0.1 będą odrzucane
# p-value = 0.4 > 0.1 -> nie ma podstaw by odrzucić H0

# b
var.test(d, m, ratio = 4,
         alternative = 'two.sided', # domyślna
         conf.level = 0.99)
# p-value = 0.024 > 0.01 -> nie ma podstaw by odrzucić H0

# c
var.test(m, d, # kolejnosc wektorów ma znaczenie, więc albo zamieniamy kolejność m i d
         ratio = 3, # albo zmiana ratio na 1/3
         # tu: zmiana kolejności parametrów (m <-> d), nie ma potrzeby zmiany ratio
         alternative = 'greater',
         conf.level = 0.975)
# p-value = 0.99 > 0.025 -> nie ma podstaw do odrzucenia H0



# zad 4
m1 = c(47, 75, 98, 23, 19, 35, 64, 57, 86, 91, 47, 86, 46, 57, 38, 86, 59, 66, 75, 45, 68, 92, 100, 44, 72, 39)
m2 = c(83, 46, 33, 67, 46, 46, 11, 46, 87, 46, 16, 46, 33, 46, 23, 46, 59, 28, 57, 21, 7, 92, 34, 56, 8, 13, 62, 10, 7,7, 7)

shapiro.test(m1) # pochodzi z rozkładu normalnego
shapiro.test(m2) # nie pochodzi z rozkładu normalnego

# ramka do oflagowania danych
frame_ = data.frame(
  values = c(m1, m2),
  flags = c(rep(1, length(m1)), rep(2, length(m2)))
)

library(lawstat)
levene.test(frame_$values, frame_$flags)
# p-value = 0.82 > 0.05 -> nie ma podstaw do odrzucenia H0


# zad 5
lt5 = c(5.5, 4.8, 5.0, 4.7, 5.4, 6.2, 4.4, 6.1, 5.9, 6.0)
gt5lt10 = c(6.7, 7.0, 6.4, 5.9, 6.8, 7.1, 7.0, 7.6, 6.8, 8.1, 6.8, 5.7)
gt10lt20 = c(8.9, 9.3, 8.5, 9.0, 8.8, 10.0, 9.9, 10.2)
gt20 = c(12.4, 13.0, 10.5, 13.4, 12.6, 12.0, 14.0, 12.5, 13.1, 14.0, 10.8, 10.9, 11.0)

# wszystkie proby pochodza z r.norm -> bartlett.test(list(x1,x2,..))
  
# co najmniej 1 proba nie pochodzi z r.norm -> fligner.test(list(x1,x2,..))












