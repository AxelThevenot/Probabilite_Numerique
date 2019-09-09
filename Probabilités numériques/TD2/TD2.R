# pour effacer la mémoire
rm(list = ls()) 


# Exercice 3 # Exercice 3 # Exercice 3 # Exercice 3

f1 <- function(x) cos(x^3)*exp(-x)
f2 <- function(x) sin(x^4)*exp(-2*x)*exp(-x^2/2)
f3 <- function(x) log(1 + x^2)*exp(-x^2)
aire1 = 0
aire2 = 0
aire3 = 0
n_max = 10000
for(i in 1:n_max) 
{
  aire1 = ((i-1)*aire1 + f1(runif(1,0,1)))/i
  aire2 = ((i-1)*aire2 + f2(runif(1,0,1)))/i
  aire3 = ((i-1)*aire3 + f3(runif(1,0,1)))/i
}
print(aire1)
print(aire2)
print(aire3)




f1 <- function(x) cos(x^3)*exp(-x)
f2 <- function(x) sin(x^4)*exp(-2*x)*exp(-x^2/2)
f3 <- function(x) log(1 + x^2)*exp(-x^2)

born_sup = 1
born_inf = 0
n_max = 10000

aire1 = 0
aire2 = 0
aire3 = 0

for(i in 1:n_max) 
{
  val <- rexp(1, 1)
  if((val >= born_inf) && (val < born_sup))
  {
    aire1 = ((i-1)*aire1 + f1(val))/i
    aire2 = ((i-1)*aire2 + f2(val))/i
    aire3 = ((i-1)*aire3 + f3(val))/i
  }

}
print(aire1)
print(aire2)
print(aire3)

# Exercice 4 # Exercice 4 # Exercice 4 # Exercice 4 


library(rgl)
f <- function(x, y, z){
  in_s = 0
  if(x^2 + y^2 + z^2 <= 1){in_s = 1}
  return(in_s)
}

aire = 0
n_max = 10000
X = c()
Y = c()
Z = c()

for(i in 1:n_max) 
{
  val <- runif(3, -1, 1)
  indi = f(val[1], val[2], val[3])
  aire = ((i-1)*aire + indi)/i
  if(indi == 1 ){
    X = c(X, val[1])
    Y = c(Y, val[2])
    Z = c(Z, val[3])
  }

  
}
print(aire*8)
plot3d(X, Y, Z, xlab = "X", ylab = "Y", zlab = "Z", type = "p", col = "green", size = 4)





# Exercice 6 # Exercice 6 # Exercice 6 # Exercice 6 

f <- function(x) sin(sqrt(x))
born_sup = 1
born_inf = 0
n_max = 10000

aire = 0
for(i in 1:n_max) 
{
  aire = ((i-1)*aire + f(runif(1,0,1)))/i
  
}
print(aire)







