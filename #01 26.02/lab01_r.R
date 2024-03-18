# fun = function(x){
#   return (sin(x)/x)
# }
# 
# curve(fun(x), from=-100, to=100, n = 1000,
#       col='blue', lwd=2, main='wykres funkcji', ylim=c(-1, 1), las=1)
# abline(v=0, h=0)

# zad 1
a = log(1024, 2)
a = log(2.718) #ln
a = log(625, 5)
a = log10(1000)
b = sin(30 * pi/180) # sin(30*)
b = cos(pi/4)
b = exp(1) # e
b = exp(1)^7
b = sqrt(exp(1))
b = atan(1) # arctan
c = factorial(4)
c = choose(8, 2) # symbol Newtona
d = signif(pi, 5)
d = floor(exp(1)) # podloga
d = ceiling(exp(1)) # sufit
d = abs(exp(1)^2*pi - 27)
e = 1286 %% 7 # mod
e = 1286 %/% 7 # div
f_zesp = complex(modulus = 4, argument = 3/2*pi)
f_real = Re(f_zesp)
f_imag = Im(f_zesp)
f_zesp = -2 + 4i
f_module = Mod(f_zesp)
f_argument = Arg(f_zesp)


# zad 2
a = c(1,2,3,4,5,6,7,8,9,10)
a = (1:10)
a = seq(1, 10, by=1) # krok
b = seq(12, 104, length=20) # długość
c = rep(c(2,4,6,8), 4)
d = rep((1:5), each=5)
ex = 4 * 3^(seq(0, 6, by=2)) # ciąg geometryczny
e = rep((1:8), (1:8))


# zad 3
# a
a_seq = seq(1, 37, by=4)
a = a_seq[2]
a = a_seq[-3] # oprócz 3 wyrazu
a = a_seq[c(-1, -10)]
a = a_seq[a_seq>18]
a = length(a_seq[a_seq>18])
a = a_seq[a_seq^2 %% 2 == 0]
a_seq > 18 # FALSE FALSE FALSE FALSE FALSE TRUE TRUE TRUE TRUE TRUE - wartość logiczna warunku dla każdego wyrazu
# b
# i)
a_seq = 2/5 * 4^((0:25))
a = mean(a_seq) # średnia
# ii)
b = a_seq
b[seq(2, length(b), by=2)] = -b[seq(2, length(b), by=2)]
# iii)
d = b
d[c(1, 4, length(d))] = c(0, pi, exp(1))
# iv)
f = 1/d


# zad 4
fibb = function(n) {
  if (n == 1) {
    return(0)
  } else if (n == 2) {
    return(1)
  } else {
    a = 0
    b = 1
    for (i in 3:n) {
      c = a + b
      a = b
      b = c
    }
    return(b)
  }
}

fibb(8)


# zad 5
losuj = function(n) {
  # pętla drukuje niepoprawne próby
  while(min(diff(sort(x <- sample(1:100, n)))) > 2) {
    print(x)
    message('najmniejsza roznica: ', min(diff(sort(x))))
  }
  # zwrócenie pierwszego poprawnie wygenerowanego wyniku
  return(x)
}

z = losuj(5)


























