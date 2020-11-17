
# En el espacio de parámetros podemos ver los parámetros y los int --------

# intervalos de confianza de la media -------------------------------------

tamano_muestral <- 35
iteraciones <- 100
media_poblacional_A <- 5
media_poblacional_B <- 3
desv_est_poblacional <- 3
min_gr_A <- media_poblacional_A - 10*desv_est_poblacional/sqrt(tamano_muestral)
max_gr_A <- media_poblacional_A + 10*desv_est_poblacional/sqrt(tamano_muestral)
min_gr_B <- media_poblacional_B - 10*desv_est_poblacional/sqrt(tamano_muestral)
max_gr_B <- media_poblacional_B + 10*desv_est_poblacional/sqrt(tamano_muestral)

plot(media_poblacional_A, media_poblacional_B, xlim = c(min_gr_A, max_gr_A), ylim = c(min_gr_B, max_gr_B), col = 4, pch = 20)

for(i in seq_len(iteraciones)){
  muestra_A <- rnorm(tamano_muestral, media_poblacional_A, desv_est_poblacional)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)

  muestra_B <- rnorm(tamano_muestral, media_poblacional_B, desv_est_poblacional)
  t_test_B <- t.test(muestra_B)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)

  rect(LI_A, LI_B, LS_A, LS_B)
  
}

abline(1,1, col = 2)
points(media_poblacional_A, media_poblacional_B, col = 4, pch = 20, cex = 3)


# Tidy approach -----------------------------------------------------------


# Paquetes ----------------------------------------------------------------


library("dplyr")
library("LaCroixColoR")
library("purrr")
library("magrittr")
library("ggplot2")

colores <- lacroix_palette("Pamplemousse")
colores_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")

tibble(
  muestra_A = replicate(iteraciones, rnorm(tamano_muestral, media_poblacional_A, desv_est_poblacional), simplify = FALSE),
  t_test_A = map(muestra_A, t.test),
  intervalo_A = map(t_test_A, extract2, "conf.int"),
  LI_A = map_dbl(intervalo_A, min),
  LS_A = map_dbl(intervalo_A, max),
  muestra_B = replicate(iteraciones, rnorm(tamano_muestral, media_poblacional_B, desv_est_poblacional), simplify = FALSE),
  t_test_B = map(muestra_B, t.test),
  intervalo_B = map(t_test_B, extract2, "conf.int"),
  LI_B = map_dbl(intervalo_B, min),
  LS_B = map_dbl(intervalo_B, max)
) -> simulaciones


simulaciones %>% 
  ggplot +
  geom_rect(aes(xmin = LI_A, xmax = LS_A, ymin = LI_B, ymax = LS_B), alpha = 0.2, fill = colores[6]) +
  annotate("point", media_poblacional_A, media_poblacional_B, colour = colores[1], size = 5) +
  geom_abline(intercept = 0, slope = 1, colour = colores[4], size = 1) +
  xlim(-2, 8) +
  ylim(-2, 8) +
  theme_minimal()
