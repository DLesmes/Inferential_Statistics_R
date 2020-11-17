
# Veamos el funcionamiento del sesgo y la varianza a nivel funcion --------


# Kernel ------------------------------------------------------------------


# Distribución normal -----------------------------------------------------

tamano_muestral <- 100
media <- 5
desv <- 3
iteraciones <- 75

x <- seq(-5, 15, length.out = 100)

Y <- rnorm(tamano_muestral, media, desv)

estimador_kernel <- density(Y)

plot(estimador_kernel)
lines(x = x, y = dnorm(x, media, desv), col = 2, lwd = 2)



plot(estimador_kernel)
for(i in seq_len(iteraciones)){
  Y <- rnorm(tamano_muestral, media, desv)
  
  estimador_kernel <- density(Y)
  
  lines(estimador_kernel)
  
}
lines(x = x, y = dnorm(x, media, desv), col = 2, lwd = 2)




# Distribución uniforme ---------------------------------------------------


tamano_muestral <- 100
a <- 3
b <- 8
iteraciones <- 75

x <- seq(2, 9, length.out = 100)

Y <- runif(tamano_muestral, a, b)

estimador_kernel <- density(Y)

plot(estimador_kernel)
lines(x = x, y = dunif(x, a, b), col = 2, lwd = 2)



plot(estimador_kernel)
for(i in seq_len(iteraciones)){
  Y <- runif(tamano_muestral, a, b)
  
  estimador_kernel <- density(Y)
  
  lines(estimador_kernel)
  
}
lines(x = x, y = dunif(x, a, b), col = 2, lwd = 2)



# ECDF --------------------------------------------------------------------

# distribución normal -----------------------------------------------------


tamano_muestral <- 100
media <- 5
desv <- 3
iteraciones <- 75

x <- seq(-5, 15, length.out = 100)

Y <- rnorm(tamano_muestral, media, desv)

estimador_ecdf <- ecdf(Y)

plot(estimador_ecdf, pch = "", verticals = TRUE)
lines(x = x, y = pnorm(x, media, desv), col = 2, lwd = 2)



plot(estimador_ecdf, pch = "", verticals = TRUE)
for(i in seq_len(iteraciones)){
  Y <- rnorm(tamano_muestral, media, desv)
  
  estimador_ecdf <- ecdf(Y)
  
  lines(estimador_ecdf, pch = "", verticals = TRUE)
  
}
lines(x = x, y = pnorm(x, media, desv), col = 2, lwd = 2)



# Distribución uniforme ---------------------------------------------------

tamano_muestral <- 80
a <- 2
b <- 8
iteraciones <- 75

x <- seq(-5, 15, length.out = 100)

Y <- runif(tamano_muestral, a, b)

estimador_ecdf <- ecdf(Y)

plot(estimador_ecdf, pch = "", verticals = TRUE)
lines(x = x, y = punif(x, a, b), col = 2, lwd = 2)



plot(estimador_ecdf, pch = "", verticals = TRUE)
for(i in seq_len(iteraciones)){
  Y <- runif(tamano_muestral, a, b)
  
  estimador_ecdf <- ecdf(Y)
  
  lines(estimador_ecdf, pch = "", verticals = TRUE)
  
}
lines(x = x, y = punif(x, a, b), col = 2, lwd = 2)



# Regresión lineal --------------------------------------------------------


iteraciones <- 100
tamano_muestral <- 30
beta_0 <- 1
beta_1 <- -0.3


x <- seq(-3, 3, length.out = tamano_muestral)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

y <- genera_y(x, beta_0, beta_1)

modelo_lineal <- lm(y~x)

plot(x, y)
lines(x, modelo_lineal$fitted.values, type = "l")


plot(x, modelo_lineal$fitted.values, type = "l")

for(i in seq_len(iteraciones)){
  y <- genera_y(x, beta_0, beta_1)
  
  modelo_lineal <- lm(y~x)
  
  lines(x, modelo_lineal$fitted.values)
  
}
abline(beta_0, beta_1, col = 2, lwd = 2)



# Red neuronal ------------------------------------------------------------

library("nnet")

iteraciones <- 100
tamano_muestral <- 30


genera_y <- function(x, beta_0, beta_1){
  cos(x) + rnorm(length(x), 0, 0.5)
  # beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}


X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)

plot(Y~X)
lines(cos(X) ~ X, col = 2, lwd = 2)


red_neuronal <- nnet(X, Y, size = 8, linout = TRUE, trace = FALSE)

YY <- predict(red_neuronal)
lines(YY ~ X, col = 4)

for(i in seq_len(iteraciones)){
  
  Y <- genera_y(X)
  red_neuronal <- nnet(X, Y, size = 8, linout = TRUE, trace = FALSE)
  YY <- predict(red_neuronal)
  lines(YY ~ X, col = 4)
  
}

lines(cos(X) ~ X, col = 2, lwd = 2)

