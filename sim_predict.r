

# Red neuronal vs regresión lineal. ¿Cuál es el mejor estimador? ----------


# Paquetes ----------------------------------------------------------------

library("nnet")

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
abline(beta_0, beta_1, col = 2, lwd = 2)
lines(x, modelo_lineal$fitted.values, col = 4, lwd = 2)


plot(x, modelo_lineal$fitted.values, type = "l")

for(i in seq_len(iteraciones)){
  y <- genera_y(x, beta_0, beta_1)
  
  modelo_lineal <- lm(y~x)
  
  lines(x, modelo_lineal$fitted.values)
  
}
abline(beta_0, beta_1, col = 2, lwd = 2)

# Red neuronal ------------------------------------------------------------

iteraciones <- 50
tamano_muestral <- 23

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
lines(YY ~ X, col = 4, lwd = 2)


plot(Y~X, col = "white")

for(i in seq_len(iteraciones)){
  
  Y <- genera_y(X)
  red_neuronal <- nnet(X, Y, size = 8, linout = TRUE, trace = FALSE)
  YY <- predict(red_neuronal)
  lines(YY ~ X, col = 4)
  
}

lines(cos(X) ~ X, col = 2, lwd = 2)



# Tidy approach -----------------------------------------------------------


# Paquetes ----------------------------------------------------------------

library("dplyr")
library("magrittr")
library("ggplot2")
library("LaCroixColoR")

color_setup <- lacroix_palette("PassionFruit", n = 5, type = "discrete")[c(1, 4, 5)]

# Regresión lineal --------------------------------------------------------

iteraciones <- 100
tamano_muestral <- 30
beta_0 <- 1
beta_1 <- -0.3

x <- seq(-3, 3, length.out = tamano_muestral)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos_x = rep(x, iteraciones),
  datos_y = genera_y(datos_x, beta_0, beta_1)
) -> simulaciones

ggplot(simulaciones) +
  # geom_point(aes(x = datos_x, y = datos_y), colour = color_setup[3], size = 0.2) +
  geom_smooth(aes(x = datos_x, y = datos_y, group = iter), method = "lm", colour = color_setup[3], size = 0.2, se = FALSE) +
  geom_abline(intercept = beta_0, slope = beta_1, colour = color_setup[1], size = 1) +
  theme_minimal()


# Red neuronal ------------------------------------------------------------

iteraciones <- 50
tamano_muestral <- 23

X <- seq(0, 3*pi, length.out = tamano_muestral)

genera_y <- function(x, beta_0, beta_1){
  cos(x) + rnorm(length(x), 0, 0.5)
  # beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}


nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos_x = rep(X, iteraciones),
  datos_y = genera_y(datos_x, beta_0, beta_1)
) -> simulaciones

my_nnet <- function(form, data, weights) nnet(form, data = data, size = 8, linout = TRUE, trace = FALSE)

ggplot(simulaciones) +
  # geom_point(aes(x = datos_x, y = datos_y), colour = color_setup[3], size = 0.2) +
  geom_smooth(aes(x = datos_x, y = datos_y, group = iter), method = "my_nnet", colour = color_setup[3], size = 0.2, se = FALSE) +
  geom_line(aes(x = datos_x, y = cos(datos_x)), colour = color_setup[1], size = 1) +
  theme_minimal()


