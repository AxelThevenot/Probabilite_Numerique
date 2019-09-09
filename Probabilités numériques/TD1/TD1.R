# pour effacer la mémoire
rm(list = ls()) 

# pour savoir dans quel fichier on est
getwd()

# se palcer dans le working directory
setwd("C:/Users/Axel/Documents/R")


# Exercice 1 # Exercice 1 # Exercice 1 # Exercice 1
4.2 * (-3)
6^4
log(18 + sin(pi))
exp(-0.4)

# Exercice 2 # Exercice 2 # Exercice 2 # Exercice 2
# Question 1
rm(list = ls())

# Question 2
IMC = function(m, h){
  
  return(m / h^2)
}
IMC(1, 2) 

# Question 3
data(women)
View(women) # Pour visualiser
str(women) # structure
summary(women) # afficher quelques éléments statistiques console

# Question 4
women$height
women$height[3]

# Question 5
dim(women)
dim(women)[1]

# Question 6
women$height[c(3, 6, seq(9, 12))]



# Exercice 3 # Exercice 3 # Exercice 3 # Exercice 3 

# Question 1
seq(1, 5)
seq(12, 20, 2)
seq(rep(1,3), rep(2,3))
rep(seq(1,3), 2)

# Question 2
x3 <- seq(1, 5, 0.5 )

# Question 3
sample(x3, 5)

# Question 4
p = c(rep(0.1, 8), 0.2)
sample(x3, 5, p = p)



# Exercice 6 # Exercice 6 # Exercice 6 # Exercice 6 

# Question 1
moyenne = function(n) {
  return(mean(rnorm(n, 0, 1)))
}

moyenne(10)
moyenne(100)
moyenne(1000)

# Question 2

moyenne = function(n, m, e){
  
  return(mean(rnorm(n, m, e^(0.5)))) 
}
moyenne(1000, 5, 2)

# Question 3

moments = function(x){
  
  return(list(moyenne=mean(x), variance=var(x)))
}

moments(rnorm(10000, 5, 2))



# Exercice 7 # Exercice 7 # Exercice 7 # Exercice 7 
x <- rnorm(1000, 0, 1)
y <- rnorm(1000, 0, 1)

# Question 1
plot(x + y ~ x)
curve(x * 1, add = TRUE, col = 'red')

# Question 2
xsum = cumsum(x)
xbarre = c()
for(i in 1:length(xsum)) {xbarre[i] = xsum[i]/i}
plot(xbarre ~ seq(1,length(xsum)), type = 'l')
curve(x * 0, add = TRUE, col = 'red')

# Question 3

hist(x, freq = FALSE)
curve(dnorm(x), add = TRUE, col = 'red')





# Exercice 8 # Exercice 8 # Exercice 8 # Exercice 8 


# Question 1
xx <- seq(-4, 4, by = 0.1)
plot(xx, dnorm(xx), xlab="x", ylab='y', type='l', col='red')

# Question 2
xx <- seq(-4, 4, by = 0.1)
plot(xx, dnorm(xx), xlab="", ylab='', type='l', col='red')
lines(x, dt(x, 5))
# Question 3

hist(x, freq = FALSE)
curve(dnorm(x), add = TRUE, col = 'red')














