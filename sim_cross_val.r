
# Hacemos la validación cruzada de nuestra red neuronal. ------------------



# Paquetes ----------------------------------------------------------------

library("caret")
library("nnet")
library("parallel")

# función de pliegue ------------------------------------------------------

rmse_fold <- function(pliegue, form, datos,  nn_size){
  pliegue_logic <- seq_len(nrow(datos)) %in% pliegue
  entrena <- subset(datos, !pliegue_logic)
  prueba <- subset(datos, pliegue_logic)
  modelo <- nnet(form, data = datos, size = nn_size, linout = TRUE, trace = FALSE)
  response_name <- setdiff(names(datos), modelo$coefnames)
  Y_pronosticado <- predict(modelo, newdata = prueba)
  rmse <- RMSE(Y_pronosticado, prueba[[response_name]])
  rmse
}


# Red neuronal ------------------------------------------------------------


n_pliegues <- 5
tamano_muestral <- 30
neuronas <- 10

genera_y <- function(x){
  cos(x) + rnorm(length(x), 0, 0.5)
}

X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)

data.frame(X, Y) -> muestra


createFolds(muestra$Y, k = n_pliegues) -> pliegues

mclapply(
  pliegues,
  rmse_fold, 
  Y ~ X,
  muestra, 
  nn_size = neuronas, 
  mc.cores = floor(detectCores()*0.8)
) -> rmse_pliegues

rmse_pliegues <- unlist(rmse_pliegues)
mean(rmse_pliegues)

plot(rmse_pliegues, ylim = c(0, 1))
abline(h = mean(rmse_pliegues), col = 2, lwd = 2)


# Tidy approach -----------------------------------------------------------

library("dplyr")
library("magrittr")

n_pliegues <- 5
tamano_muestral <- 30
neuronas <- 10

tibble(
  pliegues = createFolds(muestra$Y, k = n_pliegues),
  rmse_pliegues = mclapply(
    pliegues,
    rmse_fold, 
    Y ~ X,
    muestra, 
    nn_size = neuronas, 
    mc.cores = floor(detectCores()*0.8)
  ) %>% unlist,
  nombres = names(pliegues)
) -> validacion

validacion %$% mean(rmse_pliegues)

ggplot(validacion) +
  geom_vline(aes(xintercept = 0), size = 1.5) +
  geom_segment(aes(x = 0, y = nombres, xend = rmse_pliegues, yend = nombres), colour = "grey75") +
  geom_point(aes(x = rmse_pliegues, y = nombres), size = 4) +
  theme_minimal()
