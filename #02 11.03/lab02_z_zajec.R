# faktory
marki = sample(c('audi', 'citroen', 'mazda', 'toyota', 'volvo'),
           15, replace=TRUE)
markiFactor = factor(marki)
markiUnikatowe = levels(markiFactor)

naped = sample(c('benzyna', 'olej', 'hybryda', 'elektryk'),
               15, T)
napedFactor = factor(naped)
napedUnikatowe = levels(napedFactor)


table(napedFactor) # table(naped) tez zadziała

barplot(table(markiFactor),
        col=rainbow(5), # liczebnosc kolorow
        las=1)

barplot(table(napedFactor),
        col=rainbow(5), # liczebnosc kolorow
        las=1,
        names.arg=c('Pb', 'D', 'H', 'E'))

table(markiFactor, napedFactor)

barplot(table(markiFactor, napedFactor),
        col=rainbow(length(levels(markiFactor))),
        legend.text=T,
        ylim=c(0,7), xlim=c(0,6)) # dla odsłonięcia wykresu



# macierze
v = 1:30
dim(v) = c(3,10)
view(v)

m1 = matrix(1:30, nrow=6, ncol=5)
m2 = array(1:30, dim=c(5,6))
m3 = array(1:12, dim=c(2,2,3))

m1[3,4] # wartość z 3,4
m1[3, ] # wszystko z 3 wiersza
m1[ ,4] # wszystko z 4 kolumny
m1[ ,-c(1,5)] # wszystko bez pierwszej i ostatniej (5 kolumn, wiec bez 5.)
m1[ ,-c(1, dim(m1)[2])] # wszystko bez pierwszej i ostatniej (uniwersalne)
m1[2,3] = 0

v = sample(1:100, 40) # probka na podstawie ktorej zostanie utworzona macierz
m = matrix(v, nrow=8, ncol=5)

# zerowanie konkretnych miejsc
# I
# m[seq(1,7, by=2), seq(1,5, by=2)] = 0
# II
adresy = array(c(rep(c(1,3,5,7), each=3), rep(c(1,3,5), times=4)), dim=c(12,2))
m[adresy] = 0


m1 = matrix(sample(1:100, 12), nrow=4, ncol=3)
m2 = array(sample(1:100, 15), dim=c(3,5))
# mnozenie macierzy - operator %*%
m3 = m1 %*% m2
# transponowanie macierzy
t(m3)

# odwracanie macierzy kwadratowej
m = matrix(sample(1:100, 16), nrow=4, ncol=4)
mInverse = solve(m)
m%*%mInverse # sprawdzenie czy po przemnożeniu zwrócona zostanie macierz jednostkowa (1 na przekątnej i 0 poza tym)

# zad 12
# 3x - 2y +  z = 1
#  x      -  z = 4
#      2y + 2z = 7
# rbind - zwiąże wiersze, cbind - zwiąże kolumny
A = rbind(c(3,-2, 1),
          c(1, 0,-1),
          c(0, 2, 2))
B = cbind(c(1,4,7))
res = solve(A, B)



# listy
#zad 13
a = c(1, pi, 4)
b = c(T, F, F, F)
C = factor(c(7, pi, 'hello', 'world', pi, 6, 'hello'))
d = matrix(sample(1:20, 12), nrow = 3, ncol = 4)

l = list(a,b,c,d)
l[[2]]
l[[4]][2]

l = list(wektor_liczbowy=a, wektor_logiczny=b, faktor=c, macierz=d)
l$wektor_liczbowy








