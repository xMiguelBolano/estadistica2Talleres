#### Importación de Datos
library(readr)
# Datos generales
general_noviembre <- read_delim("Noviembre_ 2024/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
general_diciembre <- read_delim("Diciembre_2024/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Datos ocupacion
ocupados_noviembre <- read_delim("Noviembre_ 2024/CSV/Ocupados.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_diciembre <- read_delim("Diciembre_2024/CSV/Ocupados.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Datos migracion
migracion_noviembre <- read_delim("Noviembre_ 2024/CSV/Migración.CSV", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
migracion_diciembre <- read_delim("Diciembre_2024/CSV/Migración.CSV", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

#### Procesamiento

library(dplyr)

general <- bind_rows(
  general_noviembre,
  general_diciembre
)

ocupados <- bind_rows(
  ocupados_noviembre,
  ocupados_diciembre
)

ocupados_diciembre$OFICIO_C8 <- as.character(ocupados_diciembre$OFICIO_C8)

migracion <- bind_rows(
  migracion_noviembre,
  migracion_diciembre
)

base <- plyr::join_all(
list(
general %>%
  group_by(DIRECTORIO) %>%
  summarise(
    integrantes = n(),
    edad = mean(P6040),
    .groups = 'drop'
  )
,
ocupados %>%
  group_by(DIRECTORIO) %>%
  summarise(
    ocupados = n(),
    .groups = 'drop'
  ),
general %>%
  filter(P6040 > 14) %>% # La ley permite que los mayores de 14 trabajen
  group_by(DIRECTORIO) %>%
  summarise(
    en_edad = n()
  )
,
migracion %>%
  filter(P3374 == 3) %>%
  group_by(DIRECTORIO) %>%
  summarise(
    migrantes = n()
  )
)
,
by = 'DIRECTORIO', type = "left", match = "all"
)

base %>% summary()

base <- base %>%
  mutate(
    tasa_ocupacion = if_else(is.na(ocupados/en_edad), 0, ocupados/en_edad) ,
    migrante = factor(if_else(is.na(migrantes), "No", "Si"))
  )

