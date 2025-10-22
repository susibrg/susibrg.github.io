library(ggplot2)
library(dplyr)
library(tidyr)

################################################################################
# 1 - Definición del problema: 
# El objetivo del análisis exploratorio de datos radica en investigar patrones 
# y tendencias sobre el tipo de viajes que hacen las personas en función del número 
# de días del viaje y el gasto medio diario por persona. De esta manera, las
# empresas hoteleras y hosteleras pueden dirigir su negocio hacia un sector u otro.
# El motivo del viaje se divide en "ocio" o "trabajo"
# 
################################################################################
#2 - Descripción de la base de datos
################################################################################

wd <- getwd()
ruta_datos <- file.path(wd, "23995-cleaned.csv")
ine_turismo <- read.csv(ruta_datos)


# Exploración inicial de los datos 

# periodo: (int) Año desde 2016 a 2023
# motivo_del_viaje: (chr) Tipo de turismo en función de "Turismo cultural" o "Turismo de sol y playa"
# duracion_media_de_los_viajes: (num) Duración media del viaje
# gasto_medio_diario_por_persona: (num) Gasto promedio por persona al día
# gasto_medio_por_persona: (num) Gasto promedio por persona
# gasto_total: (num) Gasto total

# Verificar datos faltantes o valores atípicos antes del análisis.
head(ine_turismo)
str(ine_turismo)
summary(ine_turismo)
# periodo     motivo_del_viaje   duracion_media_de_los_viajes gasto_medio_diario_por_persona gasto_medio_por_persona  gasto_total     
# Min.   :2016   Length:120         Min.   : 0.000               Min.   :  0.0                  Min.   :   0.0          Min.   :    0.0  
# 1st Qu.:2018   Class :character   1st Qu.: 5.772               1st Qu.:105.0                  1st Qu.: 974.8          1st Qu.:  313.3  
# Median :2020   Mode  :character   Median : 6.840               Median :149.0                  Median :1096.5          Median : 1489.9  
# Mean   :2020                      Mean   : 9.015               Mean   :148.7                  Mean   :1211.7          Mean   : 9558.7  
# 3rd Qu.:2021                      3rd Qu.: 8.535               3rd Qu.:182.2                  3rd Qu.:1267.0          3rd Qu.: 6144.6  
# Max.   :2023                      Max.   :49.190               Max.   :339.0                  Max.   :4724.0          Max.   :93412.4 

sum(is.na(ine_turismo))  # ¿Hay valores NA? No.

print(table(ine_turismo$motivo_del_viaje)) # Conteo de tipos de turismo

################################################################################
# 4 - En este momento nos hacemos las preguntas de investigación.
################################################################################
# 4.1 - Estudio sobre la relación entre las variables de motivo_del_viaje y gasto_medio_por_persona
# ¿Existe una relación entre el gasto medio por persona y la probabilidad de que sea viaje de trabajo o de ocio?
# ¿Se puede identificar un patrón en los datos que indique una asociación entre estas dos variables?


# Gráfico de barras apiladas: mediana de cada periodo (más fiable y menor perceptible a outliers)
tabla_contingencia <- ine_turismo %>%
  group_by(periodo, motivo_del_viaje) %>%
  summarise(gasto_mediana = median(gasto_medio_diario_por_persona, na.rm = TRUE)) %>%
  pivot_wider(names_from = motivo_del_viaje, values_from = gasto_mediana)
print(tabla_contingencia)

tabla_long <- tabla_contingencia %>%
  pivot_longer(cols = -periodo, names_to = "motivo_del_viaje", values_to = "gasto_mediana")
ggplot(tabla_long, aes(x=factor(periodo), y=gasto_mediana, fill=motivo_del_viaje)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Periodo", y="Gasto medio diario por persona", fill="Motivo del Viaje") +
  theme_minimal()


# Gasto medio por persona => se deduce:
# mayor gasto medio en viajes de trabajo con tendencia incremental (caída en 2020) => entre [1200,1500]
# menor gasto medio en viajes de ocio con tendencia incremental (caída en 2020) => entre [1000,1500]


# Plot the boxplot
ggplot(ine_turismo, aes(x=factor(periodo), y=gasto_medio_diario_por_persona, fill=motivo_del_viaje)) +
  geom_boxplot() +
  labs(x="Periodo", y="Gasto Medio por Persona", fill="Motivo del Viaje") +
  theme_minimal()

# Gasto medio en viajes de ocio tiende a ser menor que la mediana
# Gasto medio en viajes de trabajo tiende a ser mayor que la mediana
# Caída en 2020 (covid)

# Calcular el Odds Ratio (OR) para el gasto medio por persona >= 1096 como umbral mostrado en los gráficos

# Se coje la mediana como umbral
umbral_reviews <- 149 # median(ine_turismo$gasto_medio_diario_por_persona) = 149

# Calcular el gasto_medio_por_persona >= umbral y < umbral en todos los grupos
ine_turismo$gasto_alto <- ifelse(ine_turismo$gasto_medio_diario_por_persona >= 149, "alto", "bajo")
tabla_odds <- table(ine_turismo$motivo_del_viaje, ine_turismo$gasto_alto)
print(tabla_odds)

ocio_gasto_medio_alto <- tabla_odds["ocio", "alto"]
ocio_gasto_medio_bajo <- tabla_odds["ocio", "bajo"]
trabajo_gasto_medio_alto <- tabla_odds["trabajo", "alto"]
trabajo_gasto_medio_bajo <- tabla_odds["trabajo", "bajo"]


# Calcular el Odds Ratio
# El Odds Ratio (OR) es una medida de la fuerza de asociación entre dos variables categóricas. 
# En este caso, estamos evaluando la relación entre el gasto medio por persona (con un umbral de 1096€)
# y motivo de viaje (trabajo, ocio).
# El OR se calcula comparando las probabilidades (odds) de que ocurra el evento 
# (trabajo) en dos grupos: aquellos con alto gasto medio por persona >= 1096 y 
# bajo gasto medio por persona < 1096.
odds_gasto_medio_alto <- ocio_gasto_medio_alto/trabajo_gasto_medio_alto
odds_gasto_medio_bajo <- ocio_gasto_medio_bajo/trabajo_gasto_medio_bajo

odds_ratio_gasto_medio_alto <- odds_gasto_medio_alto / odds_gasto_medio_bajo
odds_ratio_gasto_medio_bajo <- odds_gasto_medio_bajo / odds_gasto_medio_alto
cat("Odds para gasto medio alto (trabajo vs ocio):", odds_gasto_medio_alto, "\n")
cat("Odds para gasto medio bajo (trabajo vs ocio):", odds_gasto_medio_bajo, "\n")
cat("Odds Ratio Alto (trabajo vs ocio):", odds_ratio_gasto_medio_alto, "\n")
cat("Odds Ratio Bajo (trabajo vs ocio):", odds_ratio_gasto_medio_bajo, "\n")

# El cálculo anterior divide la "odds" de viaje de trabajo en el grupo de alto 
# gasto medio por persona entre las "odds" de viaje de ocio en el grupo de bajo 
# gasto medio.

# Las odds son simplemente la probabilidad de que ocurra un evento 
# (en este caso, viaje de trabajo) dividida entre la probabilidad de que 
# no ocurra (viaje de ocio).

# En el grupo con alto gasto medio (>= 149) 
# las "odds" de ser viaje de ocio serían 31/29 = 1.068966 ~= 1.07

# Esto significa que por cada opción con alto gasto medio que son viajes de trabajo
# hay 1.07 (7% más) opciones que son viajes de ocio

# Si en el grupo con bajo gasto medio (< 149) 
# las "odds" de ser viaje de ocio serían 41/19 = 2.157895 =~ 2.16

# Esto significa que por cada opción con bajo gasto medio que son viajes de trabajo
# hay 2.16 (116% más) opciones que son viajes de ocio


# Calcular el incremento porcentual
# El incremento porcentual en el riesgo es una forma de interpretar el Odds Ratio 
# de manera más intuitiva. Se calcula como: (OR - 1) * 100.

# Incremento porcentual
incremento_porcentual_alto <- (odds_ratio_gasto_medio_alto - 1) * 100
incremento_porcentual_bajo <- (odds_ratio_gasto_medio_bajo - 1) * 100
cat("Incremento porcentual (trabajo vs ocio):", incremento_porcentual, "\n")

#Un odds ratio (subgrupo alto) de 0.4953743 =~ 0.5 nos da un incremento porcentual del -50.46257% aproximadamente. 
# Esto nos indica que los viajes de ocio tienen un 50.46% más de probabilidades de obtener
# menor gasto medio diario por persona en relación a los viajes de trabajo.

#Un odds ratio (subgrupo bajo) de 2.018676 =~ 2.02 nos da un incremento porcentual del 101.8676% aproximadamente. 
# Esto nos indica que los viajes de ocio tienen un 101.87% más de probabilidades de obtener
# menor gasto medio diario por persona en relación a los viajes de trabajo.


# Mostrar los resultados
# La función 'cat()' se usa para imprimir texto y valores calculados de manera más limpia y comprensible.
cat("Tabla de Contingencia para gastos medios:\n")
print(tabla_odds)

# Imprimir el Odds Ratio calculado y el incremento porcentual
cat("\nOdds Ratio (OR) para gasto medio por persona:", odds_ratio_gasto_medio, "\n")
cat("\nIncremento porcentual de que el viaje sea de trabajo por cada gasto mayor medio por persona (?):", incremento_porcentual, "%\n")

# 4.2 - Estudio sobre la relación entre las variables de motivo_del_viaje y duracion_media_de_los_viajes
# ¿Existe una relación entre la duración media de los viajes y la probabilidad de que sea viaje de trabajo o de ocio?
# ¿Se puede identificar un patrón en los datos que indique una asociación entre estas dos variables?


# Gráfico de barras apiladas (considerando mediana de duracion media en cada periodo)
# mediana más fiable ya que no le afectan los outliers
tabla_contingencia <- ine_turismo %>%
  group_by(periodo, motivo_del_viaje) %>%
  summarise(duracion_mediana = median(duracion_media_de_los_viajes, na.rm = TRUE)) %>%
  pivot_wider(names_from = motivo_del_viaje, values_from = duracion_mediana)
print(tabla_contingencia)

tabla_long <- tabla_contingencia %>%
  pivot_longer(cols = -periodo, names_to = "motivo_del_viaje", values_to = "duracion_mediana")
ggplot(tabla_long, aes(x=factor(periodo), y=duracion_mediana, fill=motivo_del_viaje)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Periodo", y="Duración Media del Viaje", fill="Motivo del Viaje") +
  theme_minimal()


# Duracion media del viaje => se deduce:
# mayor duración media de los viajes de ocio => entre [6,8]
# menor duración media de los viajes de trabajo => entre [5,7]


# Plot the boxplot
ggplot(ine_turismo, aes(x=factor(periodo), y=duracion_media_de_los_viajes, fill=motivo_del_viaje)) +
  geom_boxplot() +
  labs(x="Periodo", y="Duración media de los viajes", fill="Motivo del Viaje") +
  theme_minimal()

# Duración media de los viajes de ocio tiende a estar equilibrado con respecto a la mediana
# Duración media de los viajes de trabajo tiende a ser mayor que la mediana
# Gran caída en 2020-2021 (covid) => mayor teletrabajo y viajes más cortos

# Calcular el Odds Ratio (OR) para duracion media del viaje >= 6 como umbral mostrado en los gráficos

# Se coje la mediana como umbral
umbral_reviews <- 6 # median(ine_turismo$duracion_media_de_los_viajes) = 6.84

# Calcular el duracion_media_de_los_viajes >= umbral y < umbral en todos los grupos
ine_turismo$duracion_alta <- ifelse(ine_turismo$duracion_media_de_los_viajes >= 6, "alto", "bajo")
tabla_odds <- table(ine_turismo$motivo_del_viaje, ine_turismo$duracion_alta)
print(tabla_odds)

ocio_duracion_alto <- tabla_odds["ocio", "alto"]
ocio_duracion_bajo <- tabla_odds["ocio", "bajo"]
trabajo_duracion_alto <- tabla_odds["trabajo", "alto"]
trabajo_duracion_bajo <- tabla_odds["trabajo", "bajo"]


# Calcular el Odds Ratio
# El Odds Ratio (OR) es una medida de la fuerza de asociación entre dos variables categóricas. 
# En este caso, estamos evaluando la relación entre el gasto medio por persona (con un umbral de 1096€)
# y motivo de viaje (trabajo, ocio).
# El OR se calcula comparando las probabilidades (odds) de que ocurra el evento 
# (trabajo) en dos grupos: aquellos con alto gasto medio por persona >= 1096 y 
# bajo gasto medio por persona < 1096.
odds_duracion_alto <- ocio_duracion_alto/trabajo_duracion_alto
odds_duracion_bajo <- ocio_duracion_bajo/trabajo_duracion_bajo

odds_ratio_duracion <- odds_duracion_alto / odds_duracion_bajo
cat("Odds para duracion media alto (trabajo vs ocio):", odds_duracion_alto, "\n")
cat("Odds para duracion media bajo (trabajo vs ocio):", odds_duracion_bajo, "\n")
cat("Odds Ratio (trabajo vs ocio):", odds_ratio_duracion, "\n")

# El cálculo anterior divide la "odds" de viaje de trabajo en el grupo de alto 
# gasto medio por persona entre las "odds" de viaje de ocio en el grupo de bajo 
# gasto medio.

# Las odds son simplemente la probabilidad de que ocurra un evento 
# (en este caso, viaje de trabajo) dividida entre la probabilidad de que 
# no ocurra (viaje de ocio).

# En el grupo con mayor duracion media (>= 6) 
# las "odds" de ser viaje de ocio serían 57/28 = 2.035714 ~= 2.04

# Esto significa que por cada opción con mayor duracion media que son viajes de trabajo
# hay 2.04 opciones que son viajes de ocio

# Si en el grupo con menor duracion media (< 6) 
# las "odds" de ser viaje de ocio serían 15/20 = 0.75

# Esto significa que por cada opción con menor duracion media que son viajes de ocio
# hay 1.3 opciones que son viajes de trabajo


# Calcular el incremento porcentual
# El incremento porcentual en el riesgo es una forma de interpretar el Odds Ratio 
# de manera más intuitiva. Se calcula como: (OR - 1) * 100.

# Incremento porcentual
incremento_porcentual <- (odds_ratio_duracion - 1) * 100
cat("Incremento porcentual (trabajo vs ocio):", incremento_porcentual, "\n")

#Un odds ratio de 2.714286 =~ 2.71 nos da un incremento porcentual del 171.4286% aproximadamente. 
# Esto nos indica que los viajes de ocio tienen un 171.43% más de probabilidades de obtener
# mayor duración media en relación a los viajes de trabajo


# Mostrar los resultados
# La función 'cat()' se usa para imprimir texto y valores calculados de manera más limpia y comprensible.
cat("Tabla de Contingencia para gastos medios:\n")
print(tabla_odds)

# Imprimir el Odds Ratio calculado y el incremento porcentual
cat("\nOdds Ratio (OR) para gasto medio por persona:", odds_ratio_gasto_medio, "\n")
cat("\nIncremento porcentual de que el viaje sea de trabajo por cada día más de viaje (?):", incremento_porcentual, "%\n")



################################################################################
# 5 - Conclusiones:
# 5.1: ocio gastan menos
# 5.2: ocio duran más

# 5.1 - Los datos parecen indicar que los viajes de ocio suelen tener 
# menor gasto medio por persona que los los viajes de trabajo
# Los viajes de trabajo tienen más probabilidades de tener un mayor gasto medio 
# por persona en comparación con los viajes de ocio. Esto puede reflejar una
# mayor ganancia para los servicios de turismo (hoteles, restaurantes, ...) cuando
# una persona viaja por trabajo en vez de por ocio. Posiblemente debido a:
# - Viajes por ocio con presupuesto más controlado, los viajes de trabajo son costeados por empresa
# - Viajes por trabajo son más ...
#
#
#

# 5.2 - Los datos parecen indicar que los viajes de trabajo suelen durar menor que
# los viajes de ocio.
# Los viajes de ocio tienen más probabilidades de tener una mayor duración media
# en comparación con viajes de trabajo. Esto puede reflejar ...
#
#


################################################################################
# 6 - Referencias:
# Díaz Delfa, M. (2021). Factores determinantes en la elección del alojamiento turístico: 
# una perspectiva económica y social. Universidad de La Rioja. Recuperado de 
# https://dialnet.unirioja.es/servlet/tesis?codigo=302566

# Bassols Gardella, M., Hernández, J., & López, S. (2021). La influencia de las valoraciones 
# online en la elección del alojamiento turístico. Investigaciones Turísticas, (23), 45-58. 
# Recuperado de https://investigacionesturisticas.ua.es/article/view/17335
