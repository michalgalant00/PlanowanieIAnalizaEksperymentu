my_path = 'C:/0_studia/planowanie i analiza eksperymentu/lab/#04 25.03/'

# ====
importuj = function() {
  lista = list() # lista na pobrane dane
  #pliki = c('plik1.txt','plik2.txt','plik3.txt') # wektor z plikami
  pliki = file.path(my_path, paste0('plik', 1:3, '.txt'))
  for(i in 1:length(pliki)) {
    lista[[i]] = read.table(pliki[i], header = T, sep = ',')
  }
  
  return(lista)
}

obliczSrednia = function(dataFrames) {
  res = c()
  for(i in 1:length(dataFrames)) {
    res = c(res, mean(dataFrames[[i]]$wiek))
  }
  return(res)
}

zaimportowana = importuj()

# wektor ze średnimi wieku per lista
wiek = obliczSrednia(zaimportowana)

barplot(wiek, space=10, names.arg = c('grupa 1','grupa 2','grupa 3'), main = 'srednia wieku')

# ====
imported = read.csv(paste0(my_path,'2017_ludnosc.csv'), header = T, sep = ';')


getNCipher = function(n, sourceNumber) {
  return(as.numeric(substr(sourceNumber,n,n)))
}

# dodanie kolumny z pierwszą cyfrą liczby ludności
imported$C1 = getNCipher(1, imported$Ludnosc)

# dodanie kolumny z drugą cyfrą liczby ludności
imported$C2 = getNCipher(2, imported$Ludnosc)

# dodanie kolumny z trzecią cyfrą liczby ludności
imported$C3 = getNCipher(3, imported$Ludnosc)

countCiphers = function(v) {
  times = c()
  for (i in 0:9) {
    currentCounter = 0
    for (elem in v) {
      if (elem == i)
        currentCounter = currentCounter + 1
    }
    times[i+1] = currentCounter
  }
  
  return(times)
}

cipherAmounts = data.frame(
  Cipher = 0:9,
  TimesOnFirstPosition = countCiphers(imported$C1),
  TimesOnSecondPosition = countCiphers(imported$C2),
  TimesOnThirdPosition = countCiphers(imported$C3)
)

# wykresy pojedynczo
# firstCipherPlot = barplot(cipherAmounts$TimesOnFirstPosition, cipherAmounts$Cipher,
#                           col = rainbow(10), ylim = c(0,300), space = 5, width = 1,
#                           names.arg = c(0:9),
#                           main = 'występowanie cyfry na 1. pozycji liczby')
# secondCipherPlot = barplot(cipherAmounts$TimesOnSecondPosition, cipherAmounts$Cipher,
#                           col = rainbow(10), ylim = c(0,300), width = 1, space = 5,
#                           names.arg = c(0:9),
#                           main = 'występowanie cyfry na 2. pozycji liczby')
# thirdCipherPlot = barplot(cipherAmounts$TimesOnThirdPosition, cipherAmounts$Cipher,
#                           col = rainbow(10), ylim = c(0,300), width = 1, space = 5,
#                           names.arg = c(0:9),
#                           main = 'występowanie cyfry na 3. pozycji liczby')


# wyświetlenie kilku wykresów
layout(matrix(c(1,2,3), 1, 3, byrow = T))
layout(matrix(c(1,2,
                3,3), 2, 2, byrow = T)) # rozpięcie na drugim wierszu
layout(matrix(c(1,2,
                1,3), 2, 2, byrow = T)) # rozpięcie na pierwszej kolumnie
layout(matrix(c(1,0,0,
                0,2,0,
                0,0,3), 3, 3, byrow = T)) # ułożenie po przekątnej
firstCipherPlot = barplot(cipherAmounts$TimesOnFirstPosition, cipherAmounts$Cipher,
                          col = rainbow(10), ylim = c(0,300), space = 5, width = 1,
                          names.arg = c(0:9),
                          main = 'występowanie cyfry na 1. pozycji liczby')
secondCipherPlot = barplot(cipherAmounts$TimesOnSecondPosition, cipherAmounts$Cipher,
                           col = rainbow(10), ylim = c(0,300), width = 1, space = 5,
                           names.arg = c(0:9),
                           main = 'występowanie cyfry na 2. pozycji liczby')
thirdCipherPlot = barplot(cipherAmounts$TimesOnThirdPosition, cipherAmounts$Cipher,
                          col = rainbow(10), ylim = c(0,300), width = 1, space = 5,
                          names.arg = c(0:9),
                          main = 'występowanie cyfry na 3. pozycji liczby')



