# ramka danych
ramka_ = data.frame(lp = 1:10,
                    indeks = sample(10000:19999, 10),
                    kol1 = sample(1:50, 10, replace=T),
                    kol2 = sample(1:50, 10, replace=T))

ramka_$kol2[3] # pobranie odpowiedniej wartosci z jakiejs kolumny
ramka_$aktywnosc = sample(1:10, 10, replace=T) # utworzenie nowej kolumny z jakimis danymi
ramka_$suma = rowSums(ramka_[ ,c(3,4,5)]) # utworzenie nowej kolumny, ktora bedzie sumą kolumny 3, 4 i 5

# umozliwia odnoszenie się do nazw poszczegolnych kolumn bez potrzeby łańcuchowania ramka_$kolumna - wystarczy kolumna
attach(ramka_) # ON
# detach(ramka_) # OFF

# przefiltrowana ramka
ramkaFiltr_ = subset(ramka_, kol1 > 25 & kol2 > 25 & aktywnosc >= 3)


# operacje na plikach
ramka_ = read.table('grupa.txt',
                    header = T, # domyslnie = FALSE
                    sep = ' ') # domyslnie = ' '


# ================================
# rozklady zmiennych losowych

# P(X = x) - gestosc (density) w argumencie x - przedr. d
# P(X <= x) - dystrybuanta (probability distribution function) w argumencie x - przedr. p
# qnorm(mean = 3, sd = 1, alpha = 0.3), kwantyle - przedr. q
# wylosowanie próby z jakiegoś rozkładu - przedr. r
rnorm(mean = 0, sd = 1, 100)
# runif() = sample()

# zad 3
# a
p = 0.7 # skutecznosc - pr-stwo sukcesu
n = 20 # podejmie 20 prob
k = 11 # trafi 11 razy
a = dbinom(k, n, p)

# b
# P(X >= 11) = 1 - P(X <= 10) - trafi co najmniej 11 razy
b = 1 - pbinom(10, 20, 0.7)

# c 
# P(X = 2)
c = dpois(2, 0.1)

# d
# P(X<2) = P(X<=1)
d = ppois(1, 0.1)

# e
e = qt(p = 0.995, df = 5)

# f
f_d = dnorm(0)
f_p = pnorm(0)

# g
g = qchisq(0.95, 15)

# h
h = rnorm(mean = 50, sd = 5)

# i
i = rexp(100, rate = 2)


# zad 5
bez_powtorzen = sample(seq(23, 131, by = 6), size = 14, replace = F)
z_powtorzeniami = sample(seq(23, 131, by = 6), size = 14, replace = T)


# zad 6
tv_ = round(rnorm(mean = 4, sd = 1.5, n = 80))
pc_ = round(rnorm(mean = 6, sd = 2, n = 80))

# a
barplot(table(tv_), main = "Czas spędzany przed telewizorem")
barplot(table(pc_), main = "Czas spędzany przed komputerem")

# b
barplot(sort(table(tv_)), main = "Posortowany czas spędzany przed telewizorem")
barplot(sort(table(pc_)), main = "Posortowany czas spędzany przed komputerem")

# c
barplot(table(tv_, tv_), main = "Częstotliwość czasu spędzanego przed telewizorem")
barplot(table(pc_, pc_), main = "Częstotliwość czasu spędzanego przed komputerem")

# d
barplot(table(pc_, tv_), legend.text = TRUE, col = rainbow(12), main = "Czas spędzany przed telewizorem i komputerem", ylim = c(0, 15))

# e
barplot(table(pc_, tv_), legend.text = TRUE, col = rainbow(12), main = "Czas spędzany przed telewizorem i komputerem", ylim = c(0, 15))
axis(2, at = seq(0, 15, by = 1))

# f
barplot(table(pc_, tv_), legend.text = TRUE, col = rainbow(12), main = "Czas spędzany przed telewizorem i komputerem", ylim = c(0, 15))
axis(2, at = seq(0, 15, by = 1))
abline(h = seq(0, 15, by = 1), col = "gray")


# zad 7
# generowanie z zad 6
tv_ = round(rnorm(mean = 4, sd = 1.5, n = 80))
pc_ = round(rnorm(mean = 6, sd = 2, n = 80))

pie(tv_)
pie(pc_)

f_tv = factor(tv_)
labels_ = levels(f_tv)
weights_ = as.vector(table(f_tv))
percentage_ = weights_ / sum(weights_)

labels_ = paste(labels_, '_', percentage_)
labels_ = paste(labels_, '%', sep='')

pie(weights_, labels = labels_, col = rainbow(8))




















