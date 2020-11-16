# ctrl + shift +R crea secciones


# Vamos a jugar con datos simulados. Escojan sus distribuciones. ----------


# Distribución normal estándar --------------------------------------------

Y <- rnorm(100)

plot(density(Y))


# Distribución normal de media cinco y desviación estándar nueve ----------

Y <- rnorm(100, 5, 3)

plot(density(Y))


# Distribución uniforme 0, 1 ----------------------------------------------

Y <- runif(100)

plot(density(Y))


# Distribución uniforme a = 3, b = 8 --------------------------------------

Y <- runif(100, 3, 8)

plot(density(Y))


# Ejemplo de la edad y el lugar -------------------------------------------

data.frame(
  Edad = rnorm(50, 10, 1.2),
  Lugar = "Escuela"
) -> escuela


data.frame(
  Edad = rnorm(45, 15, 1.9),
  Lugar = "Preparatoria"
) -> prepa

data.frame(
  Edad = rnorm(80, 21, 2.5),
  Lugar = "Universidad"
) -> universidad


rbind(escuela, prepa, universidad) -> edad_lugar

boxplot(Edad ~ Lugar, data = edad_lugar)




# Modelo lineal --------------------------------------------------------

X <- seq(0, 3*pi, length.out = 100)
Y <- -0.3*X + 1 + rnorm(100, 0, 0.5)
Z <- -0.3*X + 1

data.frame(X, Y, Z) -> datos_lineal

plot(Y~X, data = datos_lineal)
lines(Z~X, data = datos_lineal, col = 2, lwd = 2)

# Modelo no lineal --------------------------------------------------------


X <- seq(0, 3*pi, length.out = 100)
Y <- cos(X) + rnorm(100, 0, 0.5)
Z <- cos(X)

  
data.frame(X, Y, Z) -> datos_no_lineal

plot(Y~X, data = datos_no_lineal)
lines(Z~X, data = datos_no_lineal, col = 2, lwd = 2)


# Tidy approach -----------------------------------------------------------


# Librerías ---------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("LaCroixColoR")

colour_setup <- lacroix_palette("PassionFruit", n = 6)[c(1, 4, 5)]
colores_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")


tibble(
  Edad = rnorm(50, 10, 1.2),
  Lugar = "Escuela"
) -> escuela


tibble(
  Edad = rnorm(45, 15, 1.9),
  Lugar = "Preparatoria"
) -> prepa

tibble(
  Edad = rnorm(80, 21, 2.5),
  Lugar = "Universidad"
) -> universidad


bind_rows(escuela, prepa, universidad) -> edad_lugar

edad_lugar %>% 
  group_by(Lugar) %>% 
  mutate(
    edad_promedio_por_lugar = mean(Edad)
    ) %>% 
  ungroup() %>%
  mutate(
    edad_promedio_global = mean(Edad)
  ) %>% 
  ggplot() +
  geom_vline(aes(xintercept = edad_promedio_global), colour = "grey75", size = 1.5) +
  geom_jitter(aes(x = Edad, y = Lugar, colour = Lugar),  size = 2.5, alpha = 0.3) +
  geom_segment(aes(x = edad_promedio_por_lugar, y = Lugar, xend = edad_promedio_global, yend = Lugar), colour = "grey75") +
  geom_point(aes(x = edad_promedio_por_lugar, y = Lugar, colour = Lugar), size = 4) +
  scale_colour_manual(values = colour_setup) +
  labs(x = "Edad", y = NULL, caption = NULL) +
  xlim(0, 30) +
  theme_minimal() + 
  theme(
    legend.position = "bottom"
    )
  

# Modelo lineal -----------------------------------------------------------

tibble(
  X = seq(0, 3*pi, length.out = 100),
  Y = -0.3*X + 1 + rnorm(100, 0, 0.5),
  Z = -0.3*X + 1
) -> datos_lineal


datos_lineal %>%
  ggplot +
  geom_point(aes(X, Y)) +
  geom_path(aes(X, Z), colour = colour_setup[1], size = 1) +
  # geom_smooth(aes(X, Y), colour = colour_setup[2], method = "lm", size = 1, se = FALSE) +
  theme_minimal()



# Modelo no lineal --------------------------------------------------------


tibble(
  X = seq(0, 3*pi, length.out = 100),
  Y = cos(X) + rnorm(100, 0, 0.5),
  Z = cos(X)
) -> datos_no_lineal

  
datos_no_lineal %>%
  ggplot +
  geom_point(aes(X, Y)) +
  geom_path(aes(X, Z), colour = colour_setup[1], size = 1) +
  # geom_smooth(aes(X, Y), colour = colour_setup[2], method = "loess", size = 1, se = FALSE) +
  theme_minimal()

