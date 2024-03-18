# ============================ FAKTORY
# zad 1
cars = c('VW', 'Toyota', 'Renault', 'VW', 'Audi', 'Citroen', 'Fiat', 'Mazda', 'Toyota', 'Renault',
          'Renault', 'Audi', 'VW', 'VW', 'Audi', 'Mazda', 'Skoda', 'VW', 'Skoda')
# faktor
faktor = factor(cars)
# wartosci unikatowe
levels(faktor)
# zestawienie wartości unikatowych z ich wagami
table(faktor)
 

# zad 2
# i) Generowanie przykładowych wektorów odpowiedzi
set.seed(123)  # Ustawienie seeda
a_ <- sample(c('T', 'N', 'X'), 10, replace = TRUE)
b_ <- sample(c('T', 'N', 'X'), 10, replace = TRUE)

# ii) Zestawienie danych używając funkcji table()
table(a_, b_)

# iii) Obliczenie liczby osób, które na oba pytania odpowiedziały 'nie wiem'
persons_both_X <- sum(a_ == 'X' & b_ == 'X')

# Obliczenie liczby osób, które odpowiedziały na I 'nie' a na drugie 'tak'
persons_N_T <- sum(a_ == 'N' & b_ == 'T')


# zad 3
# Generowanie przykładowego wektora wieku
set.seed(123)  # Ustawienie ziarna dla powtarzalności
age_ <- sample(18:80, 10, replace = TRUE)

# Przykładowe wektory odpowiedzi na pytania
a_ <- sample(c('T', 'N', 'X'), 10, replace = TRUE)
b_ <- sample(c('T', 'N', 'X'), 10, replace = TRUE)

# i) Średni wiek tych, którzy na I pytanie odpowiedzieli 'tak'
mean_age_T <- tapply(age_, a_, function(x) mean(x[a_ == 'T']))

# ii) Odchylenie standardowe wieku osób, które na II pytanie odpowiedziały 'nie'
sd_age_N <- tapply(age_, b_, function(x) sd(x[b_ == 'N']))


# zad 4
# Utworzenie wektora wieku
age_ <- c(18, 25, 30, 40, 55, 60, 70, 80, 20, 35)

# Utworzenie wektora przedziałów wiekowych
ageIntervals_ <- cut(age_, breaks = c(0, 20, 30, 50, 75), labels = c("(0, 20]", "(20, 30]", "(30, 50]", "(50, 75]"))

# Zestawienie ilości osób w poszczególnych przedziałach wiekowych i odpowiedziach
table(ageIntervals_, a_, b_)


# ============================ MACIERZE
# zad 5





























































