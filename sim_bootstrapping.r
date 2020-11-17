

# Hacemos un bootstrapping para nuestra regresión lineal. -----------------


# Bootstrapping -----------------------------------------------------------


tamano_muestral <- 23
iteraciones <- 1000
beta_0 <- 1
beta_1 <- -0.3
desv_est_error <- 0.5

genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, desv_est_error)
}

datos_x <- genera_x(tamano_muestral)
datos_y <- genera_y(datos_x, beta_0, beta_1)


lm(datos_y ~ datos_x) -> modelo
coefficients(modelo) -> coeficientes_muestrales
summary(modelo)
confint(modelo)

beta_0_estimado <- beta_1_estimado <- vector()

for(i in seq_len(iteraciones)){
  muestra <- sample(seq_along(datos_x), length(datos_x), replace = TRUE)
  muestra_x <- datos_x[muestra]
  muestra_y <- datos_y[muestra]
  lm(muestra_y ~ muestra_x) -> modelo
  coeficientes <- coefficients(modelo)
  beta_0_estimado[i] <- coeficientes[1]
  beta_1_estimado[i] <- coeficientes[2]
}


data.frame(
  limite = c("LI", "LS"),
  beta_0 = quantile(beta_0_estimado, c(0.025, 0.975)),
  beta_1 = quantile(beta_1_estimado, c(0.025, 0.975))
) -> intervalo_bootstrapping

intervalo_bootstrapping



plot(beta_0, beta_1)

points(beta_0_estimado, beta_1_estimado)
points(coeficientes_muestrales[1], coeficientes_muestrales[2], pch = 20, col = 4, cex = 3)
points(beta_0, beta_1, pch = 20, col = 2, cex = 3)
rect(
  intervalo_bootstrapping$beta_0[1], 
  intervalo_bootstrapping$beta_1[1],
  intervalo_bootstrapping$beta_0[2],
  intervalo_bootstrapping$beta_1[2],
  border = 4, lwd = 2)


# Tidy approach -----------------------------------------------------------

# Paquetes ----------------------------------------------------------------


library("dplyr")
library("purrr")
library("tidyr", exclude = c("extract"))
library("ggplot2")
library("magrittr")

colores_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")

tamano_muestral <- 23
iteraciones <- 1000
beta_0 <- 1
beta_1 <- -0.3
desv_est_error <- 0.5

genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, desv_est_error)
}

datos_simulados <- tibble(
  x = genera_x(tamano_muestral),
  y = genera_y(datos_x, beta_0, beta_1)
)


lm(y ~ x, data = datos_simulados) -> modelo
coefficients(modelo) -> coeficientes_muestrales
summary(modelo)
confint(modelo)

tibble(
  muestras = replicate(iteraciones, sample_n(datos_simulados, nrow(datos_simulados), replace = TRUE), simplify = FALSE),
  modelos = map(muestras, function(muestra){lm(y~x, data = muestra)}),
  beta_0_estimado = map(modelos, coef) %>% map_dbl(extract, 1),
  beta_1_estimado = map(modelos, coef) %>% map_dbl(extract, 2)
) -> coeficientes

coeficientes %>% 
  select(beta_0_estimado, beta_1_estimado) %>% 
    gather(key = coeficiente, value = valor) %>%
    group_by(coeficiente) %>%
    summarise(
      LI = quantile(valor, 0.025),
      LS = quantile(valor, 0.975)
    ) -> intervalo_bootstrap

coeficientes %>% 
  select(beta_0_estimado, beta_1_estimado) %>% 
  gather(key = coeficiente, value = valor) %>% 
  ggplot +
  aes(x = valor) + 
  geom_density(fill = "#78D92A", alpha = 0.5, colour = "#78D92A") +
  facet_wrap(~coeficiente, nrow = 4,  scales = "free") + 
  theme_minimal()

ggplot() +
  geom_point(
    data = coeficientes, 
    mapping = aes(x = beta_0_estimado, y = beta_1_estimado)
    ) +
  geom_rect(
    data = intervalo_bootstrap, 
    mapping = aes(xmin = LI[[1]], ymin = LI[[2]], xmax = LS[[1]], ymax = LS[[2]]),
    fill = colores_platzi[1],
    alpha = 0.2
    ) +
  annotate("point",x = beta_0, y = beta_1, colour = colores_platzi[4], size = 5) +
  annotate("point",x = coeficientes_muestrales[1], y = coeficientes_muestrales[2], colour = colores_platzi[3], size = 5) +
  theme_minimal()




