# Exercice 1 # Exercice 1 # Exercice 1 # Exercice 1 # Exercice 1 

f <- function(x) exp(x)
values = c()
variance = c()
esperance = 0
esperance_2 = 0
n_max = 10000
for(i in 1:n_max) 
{
  val =  runif(1,0,1)
  esperance = ((i-1)*esperance +f(val))/i
  esperance_2  = ((i-1)*esperance_2 +f(val)^2)/i
  values = c(values, esperance)
  variance = c(variance, esperance_2 - esperance^2)
}
print(esperance)
plot(values, type="l", col="black")
lines(rep(exp(1) - 1, n_max), col='red')

plot(variance, type="l", col="black")
lines(rep(0.24, n_max), col='red')

#########################################

f <- function(x) exp(x)
values = c()
variance = c()
I = 0
I_2 = 0
n_max = 10000
for(i in 1:n_max) 
{
  val =  runif(1,0,1)
  I = ((i-1)*I +(f(val) + f(1 - val))/2)/i
  I_2  = ((i-1)*I_2 +f(val)^2)/i
  values = c(values, I)
  variance = c(variance, I_2 - I^2)
}
print(I)
plot(values, type="l", col="black")
lines(rep(exp(1) - 1, n_max), col='red')

plot(variance, type="l", col="black")
lines(rep(0.24, n_max), col='red')




# Exercice 2 # Exercice 2 # Exercice 2 # Exercice 2 



beta <- 5
g <- function(x) exp(beta*x)*(x > 0)

h <- function(x) (x >0)*(exp(beta*x))


v <- function(g, n)
{
  u <- rnorm(n)
  sigma2hat <- cumsum(g(u)*g(u))/(1:n) - (cumsum(g(u))/(1:n))^2 
  return(sigma2hat)
}

gt <- function(x) g(x) - h(x)

plot(v(gt,10000), type='l')



