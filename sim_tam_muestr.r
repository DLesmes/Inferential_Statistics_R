
# Podemos ver que el tamaño muestral presenta rendimientos decreci --------


# Distribución normal -----------------------------------------------------

tamano_muestral_max <- 500
iteraciones <- 100
media_poblacional <- 5
desv_est_poblacional <- 3
tamano_muestral <- floor(seq(10, tamano_muestral_max, length.out = iteraciones))

desv_est_estimada <- media_estimada <- dif_cuad_media <- dif_cuad_desv_est <- vector()

for (i in seq_len(iteraciones)) {
  muestra <- rnorm(tamano_muestral[i], media_poblacional, desv_est_poblacional)
  media_estimada[i] <- mean(muestra)
  desv_est_estimada[i] <- sd(muestra)
  dif_cuad_media[i] <- (media_estimada[i] - media_poblacional)^2
  dif_cuad_desv_est[i] <- (desv_est_estimada[i] - desv_est_poblacional)^2
}


plot(media_estimada ~ tamano_muestral)
abline(h = media_poblacional, col = 2, lwd = 2)

plot(dif_cuad_media ~ tamano_muestral, type = "l")


plot(desv_est_estimada ~ tamano_muestral)
abline(h = desv_est_poblacional, col = 2, lwd = 2)

plot(dif_cuad_desv_est ~ tamano_muestral, type = "l")




# Distribución uniforme ---------------------------------------------------


tamano_muestral_max <- 500
iteraciones <- 100
maximo_poblacional <- 8
minimo_poblacional <- 3
tamano_muestral <- floor(seq(10, tamano_muestral_max, length.out = iteraciones))

maximo_estimado <- minimo_estimado <- dif_cuad_maximo <- dif_cuad_minimo <- vector()

for (i in seq_len(iteraciones)) {
  muestra <- runif(tamano_muestral[i], minimo_poblacional, maximo_poblacional)
  maximo_estimado[i] <- max(muestra)
  minimo_estimado[i] <- min(muestra)
  dif_cuad_maximo[i] <- (maximo_estimado[i] - maximo_poblacional)^2
  dif_cuad_minimo[i] <- (minimo_estimado[i] - minimo_poblacional)^2
}


plot(maximo_estimado ~ tamano_muestral)
abline(h = maximo_poblacional, col = 2, lwd = 2)

plot(dif_cuad_media ~ tamano_muestral, type = "l")


plot(minimo_estimado ~ tamano_muestral)
abline(h = minimo_poblacional, col = 2, lwd = 2)

plot(dif_cuad_desv_est ~ tamano_muestral, type = "l")


# Regresión lineal --------------------------------------------------------

tamano_muestral_max <- 500
iteraciones <- 100
beta_0 <- 1
beta_1 <- -0.3
minimo_poblacional <- 3
tamano_muestral <- floor(seq(10, tamano_muestral_max, length.out = iteraciones))

genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

beta_0_estimado <- beta_1_estimado <- dif_cuad_beta_0 <- dif_cuad_beta_1 <- vector()

for (i in seq_len(iteraciones)) {
  X <- genera_x(tamano_muestral[i])
  Y <- genera_y(X, beta_0, beta_1)
  betas <- coef(lm(Y ~ X))
  beta_0_estimado[i] <- betas[1]
  beta_1_estimado[i] <- betas[2]
  dif_cuad_beta_0[i] = (beta_0_estimado[i] - beta_0)^2
  dif_cuad_beta_1[i] = (beta_1_estimado[i] - beta_1)^2
}


plot(beta_0_estimado ~ tamano_muestral)
abline(h = beta_0, col = 2, lwd = 2)

plot(dif_cuad_beta_0 ~ tamano_muestral, type = "l")


plot(beta_1_estimado ~ tamano_muestral)
abline(h = beta_1, col = 2, lwd = 2)

plot(dif_cuad_beta_1 ~ tamano_muestral, type = "l")



# tidy approach -----------------------------------------------------------


# Paquetes ----------------------------------------------------------------


library("dplyr")
library("purrr")
library("ggplot2")
library("magrittr")

colores_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")


# Distribución normal -----------------------------------------------------



tamano_muestral_max <- 500
iteraciones <- 100
media_poblacional <- 5
desv_est_poblacional <- 3


tibble(
  media = media_poblacional,
  desv_est = desv_est_poblacional,
  tamano_muestral = floor(seq(10, tamano_muestral_max, length.out = iteraciones)),
  muestras = map(tamano_muestral, rnorm, media, desv_est),
  media_estimada = map_dbl(muestras, mean),
  desv_est_estimada = map_dbl(muestras, sd),
  dif_cuad_medias = (media - media_estimada)^2,
  dif_cuad_sd = (desv_est_estimada - desv_est)^2
) -> simulaciones

qplot(tamano_muestral, media_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$media, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_medias, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()


qplot(tamano_muestral, desv_est_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$desv_est, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_sd, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()


# Distribución uniforme ---------------------------------------------------


tamano_muestral_max <- 500
iteraciones <- 100

tibble(
  tamano_muestral = floor(seq(10, tamano_muestral_max, length.out = iteraciones)),
  maximo_poblacional = 8,
  minimo_poblacional = 3,
  muestras = map(tamano_muestral, runif, minimo_poblacional, maximo_poblacional),
  maximo_estimado = map_dbl(muestras, max),
  minimo_estimado = map_dbl(muestras, min),
  dif_cuad_maximo = (maximo_poblacional - maximo_estimado)^2,
  dif_cuad_minimo = (minimo_poblacional - minimo_estimado)^2
) -> simulaciones

qplot(tamano_muestral, maximo_estimado, data = simulaciones) +
  geom_hline(yintercept = simulaciones$maximo_poblacional, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_maximo, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, minimo_estimado, data = simulaciones) +
  geom_hline(yintercept = simulaciones$minimo_poblacional, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_minimo, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()



# Regresión lineal simple -------------------------------------------------


tamano_muestral_max <- 500
iteraciones <- 100
beta_0 <- 1
beta_1 <- -0.3
minimo_poblacional <- 3

genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

estima_betas <- function(x, y){
  coef(lm(y ~ x))
}

tibble(
  tamano_muestral = floor(seq(10, tamano_muestral_max, length.out = iteraciones)),
  datos_x = map(tamano_muestral, genera_x),
  datos_y = map(datos_x, genera_y, beta_0, beta_1),
  betas = map2(datos_x, datos_y, estima_betas),
  beta_0_estimado = map_dbl(betas, extract, 1),
  beta_1_estimado = map_dbl(betas, extract, 2),
  dif_cuad_beta_0 = (beta_0_estimado - beta_0)^2,
  dif_cuad_beta_1 = (beta_1_estimado - beta_1)^2
  # plot = map2(datos_x, datos_y, plot)
) -> simulaciones

qplot(tamano_muestral, beta_0_estimado, data = simulaciones) +
  geom_hline(yintercept = beta_0, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_beta_0, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()


qplot(tamano_muestral, beta_1_estimado, data = simulaciones) +
  geom_hline(yintercept = beta_1, colour = colores_platzi[4]) +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_beta_1, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE, colour = colores_platzi[4]) +
  theme_minimal()


