

# Revisemos la potencia de una prueba. ------------------------------------


# wmw con dos gamas -------------------------------------------------------

# La media de una gamma es shape/rate, vamos a mover el shape -------------

tamano_muestral <- 50
iteraciones <- 80
dif_media_ini <- 0
dif_media_fin <- 2
media_x <- 1
n_pasos <- 50
umbral_significancia <- 0.05
dif_medias <- seq(dif_media_ini, dif_media_fin, length.out = n_pasos)



# prueba wmw --------------------------------------------------------------


potencia_wmw <- vector()

for(k in seq_along(dif_medias)){ 
  
  sim_shape <- dif_medias[k] + media_x
  
  p_valores <- vector()
  
  for(i in seq_len(iteraciones)){
    x <- rgamma(tamano_muestral, media_x, 1)
    y <- rgamma(tamano_muestral, sim_shape, 1)
    p_valores[i] <- wilcox.test(x, y)$p.value
    
  }
  
  potencia_wmw[k] <- mean(p_valores < umbral_significancia)
  
}


# prueba t ----------------------------------------------------------------



potencia_t <- vector()

for(k in seq_along(dif_medias)){ 
  
  sim_shape <- dif_medias[k] + media_x
  
  p_valores <- vector()
  
  for(i in seq_len(iteraciones)){
    x <- rgamma(tamano_muestral, media_x, 1)
    y <- rgamma(tamano_muestral, sim_shape, 1)
    p_valores[i] <- t.test(x, y)$p.value
    
  }
  
  potencia_t[k] <- mean(p_valores < umbral_significancia)
  
}




plot(dif_medias, potencia_wmw, ylim = c(0, 1), col = 4, type = "l")
lines(dif_medias, potencia_t, col = 2)


# Tidy approach -----------------------------------------------------------

# Paquetes ----------------------------------------------------------------


library("ggplot2")
library("dplyr")
library("tidyr")
library("magrittr")
library("purrr")

tamano_muestral <- 50
iteraciones <- 80
dif_media_ini <- 0
dif_media_fin <- 2
media_x <- 1
n_pasos <- 50
umbral_significancia <- 0.05
dif_medias <- seq(dif_media_ini, dif_media_fin, length.out = n_pasos)



fun_potencia_wmw <- function(shape_1, shape_2, iteraciones){
  map_dbl(
    seq_len(iteraciones), 
    function(i){
      x <- rgamma(tamano_muestral, shape_1, 1)
      y <- rgamma(tamano_muestral, shape_2, 1)
      wilcox.test(x, y) %$% p.value
    }
  ) -> p_valores
  mean(p_valores < umbral_significancia)
}


fun_potencia_t <- function(shape_1, shape_2, iteraciones){
  
  map_dbl(
    seq_len(iteraciones), 
    function(i){
      x <- rgamma(tamano_muestral, shape_1, 1)
      y <- rgamma(tamano_muestral, shape_2, 1)
      t.test(x, y) %$% p.value
    }
  ) -> p_valores
  mean(p_valores < umbral_significancia)
}

tibble(
  dif_medias,
  shape_01 = media_x,
  shape_02 = shape_01 + dif_medias,
  potencia_wmw = map2_dbl(shape_01, shape_02, fun_potencia_wmw, iteraciones),
  potencia_t = map2_dbl(shape_01, shape_02, fun_potencia_t, iteraciones),
) -> simulaciones


simulaciones %>% 
  gather("prueba", "potencia", potencia_wmw, potencia_t) %>% 
  ggplot +
  geom_line(aes(x = dif_medias, y = potencia, colour = prueba), size = 0.3) +
  geom_smooth(aes(x = dif_medias, y = potencia, colour = prueba), size = 1, se = FALSE) + 
  theme_minimal() +
  theme(legend.position = "bottom")
  
