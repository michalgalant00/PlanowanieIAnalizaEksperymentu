#====
# lab5 - zad1
n = 1000 # liczba prób
coins_ = sample(c(0,1), n, replace = T) # wygenerowanie próbki
coinsFrame_ = data.frame(Lp = 1:n,
                         Wynik = coins_)
coinsFrame_$SumaCzesciowa = cumsum(coinsFrame_$Wynik) # cumsum(wektor) - wektor sum częściowych danego wektora
coinsFrame_$Srednia = coinsFrame_$SumaCzesciowa / coinsFrame_$Lp

plot(coinsFrame_$Lp, coinsFrame_$Srednia,
     type = 'o', ylim = c(0,1), xlim = c(1,n))
abline(h = 1/2, col = 'red')


#==== 
# lab5 - zad2
curve(pnorm(x, mean=0, sd=4),
      xlim = c(-20, 20), ylim = c(0, 1),
      lwd = 3, lty = 1, col = 'green')

proba = rnorm(n = 25, mean = 0, sd = 4)
proba = rnorm(n = 250, mean = 0, sd = 4)
proba = rnorm(n = 2500, mean = 0, sd = 4) # zwiekszanie liczebnosci proby zwieksza zageszczenie wykresu
lines(ecdf(proba), col = 'blue')

#====
# lab6 - zad1
my_path = 'C:/0_studia/planowanie i analiza eksperymentu/lab/#05 08.04'
egz1_file = file.path(my_path, paste0('Egz_1.csv'))
egzamin_ = read.csv(file = egz1_file, header = T)

multiSample_ = replicate(n = 30, sample(egzamin_$punkty, 10, T))
multiSample_ = t(multiSample_)  # transpozycja macierzy

transposed_ = data.frame(multiSample_,
                         Mean = rowMeans(multiSample_))

generateRowSds = function(m) {
  v = c()
  for (i in 1:dim(m)[1]) {
    v[i] = sd(m[i, ])
  }
  return(v)
}

transposed_$Sd = generateRowSds(multiSample_)

# mediana
generateRowMedians = function(m) {
  v = c()
  for (i in 1:dim(m)[1]) {
    v[i] = median(m[i, ])
  }
  return(v)
}

# min
generateRowMins = function(m) {
  v = c()
  for (i in 1:dim(m)[1]) {
    v[i] = min(m[i, ])
  }
  return(v)
}

# max
generateRowMaxs = function(m) {
  v = c()
  for (i in 1:dim(m)[1]) {
    v[i] = max(m[i, ])
  }
  return(v)
}

transposed_$Median = generateRowMedians(multiSample_)
transposed_$Minimum = generateRowMins(multiSample_)
transposed_$Maximum = generateRowMaxs(multiSample_)



layout(matrix(c(1,2,
                3,3,
                4,5), 2, 3, byrow = F))

plot(1:30, transposed_$Mean)
plot(1:30, transposed_$Sd)
plot(1:30, transposed_$Median)
plot(1:30, transposed_$Minimum)
plot(1:30, transposed_$Maximum)

#====
# lab6 - zad2
groupAScores = subset(egzamin_, grupa=='A', select = punkty)
groupBScores = subset(egzamin_, grupa=='B', select = punkty)
groupCScores = subset(egzamin_, grupa=='C', select = punkty)
groupDScores = subset(egzamin_, grupa=='D', select = punkty)
groupEScores = subset(egzamin_, grupa=='E', select = punkty)
maleScores = subset(egzamin_, plec=='M', select = punkty)
femaleScores = subset(egzamin_, plec=='K', select = punkty)

#reset layoutu
barplot(c(mean(maleScores$punkty),
          mean(femaleScores$punkty),
          mean(groupAScores$punkty),
          mean(groupBScores$punkty),
          mean(groupCScores$punkty),
          mean(groupDScores$punkty),
          mean(groupEScores$punkty)),
        names.arg = c('M', 'K', 'A', 'B', 'C', 'D', 'E'))


