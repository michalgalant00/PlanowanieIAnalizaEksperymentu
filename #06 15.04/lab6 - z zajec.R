# test zgodnosci chi-kwadrat Pearsona - porównanie liczności teoretycznych i empirycznych

# H0 - hipoteza podstawowa
# H1 - hipoteza alternatywna  H1: ~H0
# 
# H0                    T                     F
# accept               ok                błąd II rodzaju (B = P(bII))
# reject  błąd I rodzaju (a = (P(bI)))       ok
# 
# a - poziom istotności testu (a = 0,05)
# 1-a - poziom ufności testu
# 1-B - moc testu

# * a - alfa, B - beta


#====
# lab 6 - zad 3

# H0: F = Funif (rozkład jednostajny)
# H1: ~H0

# wektor liczności empirycznych, z podejrzewanym rozkladem prawdopodobienstw (per przypadek)
chisq.test(c(19,13,21,12,12,13),
           p=rep(1/6, 6))
# p-value = 0.392 >= 0.05 -> nie ma podstaw do odrzucenia hipotezy podstawowej


#====
# lab 6 - zad 4

ludnoscPath = "C:/0_studia/planowanie i analiza eksperymentu/lab/#04 25.03/2017_ludnosc.csv"
dta = read.csv(ludnoscPath, header = T, sep = ';')

dta$FC = substr(dta$Ludnosc, 1, 1) # kolumna z pierwszą cyfrą (FirstCipher)
# H0: F = Fbenf (rozkład Benforda)
# H1: ~H0

countCiphers = function(v) {
  a = c()
  for (i in 1:9) {
    a[i] = dim(subset(dta, dta$FC == i))[1] # wektor a na i-tej pozycji bedzie mial informacje ile danych zaczęło się od cyfry i
  }
  return(a)
}

emp = countCiphers(dta$FC)

# wektor z prawdopodobieństwami rozkładu Benforda
generateBenfordProbs = function() {
  v = c()
  for (i in 1:9) {
    v[i] = log10(1+1/i)
  }
  return(v)
}

probs = generateBenfordProbs()


chisq.test(emp, p = probs)
# p-value = 0.00195 < 0.05 -> odrzucamy hipotezę podstawową


#====
# lab 6 - zad 5

#  i   0   1   2   3   4   5
# ni  10  27  29  16  11   7

# H0: F = Fpoiss (rozkład Poissona P(X=k)=lambda^k/k! * e^-lambda) 
# H1: ~H0

proba = c(10,27,29,16,11,7)

# przybliżenie lambdy średnią ważoną (wektor częstości)
lambda = weighted.mean(c(0, 1, 2, 3, 4, 5), proba)

probs = dpois(0:5, lambda = lambda)

sum(probs) # prawdopodobieństwa nie sumują się do 1
probs = probs/sum(probs) # 'uzupełnienie' niepełnego rozkładu

chisq.test(proba,
           p = probs)
# p-value = 0.7355 >= 0.05 -> nie ma podstaw do odrzucenia hipotezy podstawowej


# ===================================================================
#  
#                          LABORATORIUM 7
#  
# ===================================================================

#====
# zad 1 (Test chi kwadrat Pearsona)
# H0: F = Fdwumianowy 
# H1: ~H0
proba = c(7, 5, 4, 3, 3, 4, 2, 3, 3, 3, 6, 5, 2, 4, 1, 4, 5)

probs = dbinom(sort(proba), 10, 0.4)
probs = probs/sum(probs)
chisq.test(proba,
           p = probs)
# p-value = 3.191e-10 (~0,0000000003191) < 0.05 -> odrzucamy hipotezę podstawową


#====
# zad 2 (Test zgodności Kołmogorowa-Smirnova)
# H0: F = Fnorm (rozkład normalny jest rozkładem ciągłym) (srednia, odchylenie)
# H1: ~H0

proba = c(62, 57, 70, 58, 59, 67, 65, 69, 55, 57, 60, 54, 72, 66, 74)

# wyprowadzenie parametrów pod rozkład normalny
mi = mean(proba)
sigma = sd(proba)

ks.test(x = proba,
        'pnorm', # nazwa dystrybuanty rozkładu z którym testujemy zgodność
        mean = mi, sd = sigma) # parametry rozkładu
# p-value = 0.9085 >= 0.05 -> nie ma podstaw do odrzucenia hipotezy podstawowej


# Test Shapiro-Wilka
# czy proba ma szansę pochodzić z rozkładu normalnego (bez znaczenia z jakimi parametrami)?
shapiro.test(x = proba)
# p-value = 0.4246 >= 0.05 -> nie ma podstaw do odrzucenia hipotezy podstawowej


#====
# zad 3
plucaPath = 'C:/0_studia/planowanie i analiza eksperymentu/lab/#06 15.04/pluca.txt'
dta = read.table(file = plucaPath, header = T, sep = ' ')

# H0: F1 = F2 (rozkład pierwszej próby jest taki sam jak drugiej próby)
# H1: ~H0


# ks.test(proba1, proba2)


#====
# zad 4 (Test chi kwadrat Pearsona)
# H0: Fprobki1(rozk. Cauchyego) = Fprobki2(rozk. normalny) 
# H1: ~H0

# rozkład Cauchyego
proba1 = rcauchy(60, location = 0, scale = 1)
# rozkład normalny
proba2 = rnorm(55, mean = 0, sd = 1)

ks.test(proba1, proba2)
# p-value = 0.2945 >= 0.05 -> nie ma podstaw do odrzucenia hipotezy podstawowej


#====
# zad 5 (Test chi kwadrat Pearsona)
# H0: F = Fdwumianowy 
# H1: ~H0

ramka_ = data.frame(A = c( 1, 4, 8),
                    B = c( 2, 1,-6),
                    C = c(-7, 0,-3),
                    D = c( 3,-2, 4),
                    E = c( 5,-9, 1))

ramka_$Suma = apply(ramka_[,1:5], 1, sum)
ramka_$`Suma(A, C, E)` = apply(ramka_[c("A", "C", "E")], 1, sum)
ramka_$Max = apply(ramka_[,1:5], 1, max)
ramka_$MaxAbsValue = apply(ramka_[,1:5], 1, function(x) max(abs(x)))

ramka_
