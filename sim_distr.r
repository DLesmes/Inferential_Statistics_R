

# ECDF para todos! --------------------------------------------------------



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

tamano_muestral <- 1000
a <- 2
b <- 8
iteraciones <- 75

x <- seq(-5, 15, length.out = 100)

Y <- runif(tamano_muestral, a, b)

estimador_ecdf <- ecdf(Y)

plot(estimador_ecdf, pch = "", verticals = TRUE)
lines(x = x, y = punif(x, a, b), col = 2, lwd = 2)



plot(estimador_ecdf, pch = "", verticals = TRUE)
for(i in 1:iteraciones){
  Y <- runif(tamano_muestral, a, b)
  
  estimador_ecdf <- ecdf(Y)
  
  lines(estimador_ecdf, pch = "", verticals = TRUE)
  
}
lines(x = x, y = punif(x, a, b), col = 2, lwd = 2)



# Tidy approach -----------------------------------------------------------


# Paquetes ----------------------------------------------------------------


library("magrittr")
library("ggplot2")
library("LaCroixColoR")
library("dplyr")

# kernel ------------------------------------------------------------------



# Distribución lineal -----------------------------------------------------


color_setup <- lacroix_palette("PassionFruit", n = 5, type = "discrete")[c(1, 4, 5)]

colores_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")

tamano_muestral <- 10
media <- 5
desv <- 3
iteraciones <- 75

tibble(
  x = seq(-5, 15, length.out = 100),
  y = dnorm(x, media, desv)
) -> poblacion

nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos = rnorm(iteraciones*tamano_muestral, media, desv)
) -> simulaciones


ggplot(simulaciones) +
  geom_density(mapping = aes(x = datos, group = iter), colour = color_setup[3], size = 0.2) +
  geom_path(data = poblacion, aes(x = x, y = y), colour = color_setup[1], size = 1) +
  theme_minimal()


# Distribución uniforme ---------------------------------------------------


tamano_muestral <- 1000
a <- 3
b <- 8
iteraciones <- 75

tibble(
  x = seq(1, 10, length.out = 100),
  y = dunif(x, a, b)
) -> poblacion

nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos = runif(iteraciones*tamano_muestral, a, b)
) -> simulaciones


ggplot(simulaciones) +
  geom_density(mapping = aes(x = datos, group = iter), colour = color_setup[3], size = 0.2) +
  geom_path(data = poblacion, aes(x = x, y = y), colour = color_setup[1], size = 1) +
  theme_minimal()



# ECDF --------------------------------------------------------------------



# Distribución normal -----------------------------------------------------


tamano_muestral <- 10
media <- 5
desv <- 3
iteraciones <- 75

tibble(
  x = seq(-5, 15, length.out = 100),
  y = pnorm(x, media, desv)
) -> poblacion

nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos = rnorm(iteraciones*tamano_muestral, media, desv)
) -> simulaciones


ggplot(simulaciones) +
  geom_step(mapping = aes(x = datos, group = iter), colour = color_setup[3], size = 0.2, stat = "ecdf") +
  geom_path(data = poblacion, aes(x = x, y = y), colour = color_setup[1], size = 1) +
  theme_minimal()


# Distribución uniforme ---------------------------------------------------


tamano_muestral <- 10
a <- 3
b <- 8
iteraciones <- 75

tibble(
  x = seq(1, 10, length.out = 100),
  y = punif(x, a, b)
) -> poblacion

nombre_iter <- paste("I", seq_len(iteraciones))

tibble(
  iter = rep(nombre_iter, each=tamano_muestral),
  datos = runif(iteraciones*tamano_muestral, a, b)
) -> simulaciones


ggplot(simulaciones) +
  geom_step(mapping = aes(x = datos, group = iter), colour = color_setup[3], size = 0.2, stat = "ecdf") +
  geom_path(data = poblacion, aes(x = x, y = y), colour = color_setup[1], size = 1) +
  theme_minimal()



