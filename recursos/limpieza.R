# Este archivo sirve para unir internamente para:
# - unir el archivo auxiliar que contiene la asignación de departamentos a delegaciones
# - filtrar las provincias innecesarias y variables heredadas del censo de indec
# - calcular los datos agrupados por delegación para cada cultivo y campaña

# Todos estos procesos se realizan aquí para evitar la sobrecarga en la shiny


#### Cargar librerías ####
library(dplyr)
library(sf)
library(readr)
library(stringi)
library(readxl)



#### Importar archivos ####
ruta_archivo <- "recursos/pxdptodatosok.shp"
datos_espaciales <- st_read(ruta_archivo)
auxiliar <- read_excel("recursos/auxiliar.xlsx")
estimaciones <- read_excel("recursos/estimaciones.xlsx")

#unimos la información de asignación de departamentos a delegaciones
datos_delegaciones_aux <- datos_espaciales %>%
  left_join(auxiliar, by = c("departamen", "provincia"))

#Filtramos las provincias que no son necesarias
#también se quitan las variables del censo

provincias_a_excluir <- c("Tierra del Fuego", "Santa Cruz", "Río Negro", "Neuquén", "Chubut")
datos_delegaciones <- datos_delegaciones_aux %>%
  filter(!provincia %in% provincias_a_excluir) %>%
  select(-mujeres, -varones, -personas, -hogares, -viv_part, -viv_part_h, -link, -codpcia)

#antes de unir con los datos históricos de SAGyP es necesario transformar provincia y departamento
# pasar a mayúscula y quitar tildes en dos nuevas variables

datos_delegaciones2 <- datos_delegaciones %>%
  mutate(
    PROVINCIA = toupper(stri_trans_general(trimws(provincia), "Latin-ASCII")),
    DEPARTAMENTO = toupper(stri_trans_general(trimws(departamen), "Latin-ASCII"))
  )

# ahora si unimos datos_delegaciones con los datos históricos
# dos departamentos fueron renombrados porque no coincidian los nombres exactamente

datos_unidos <- estimaciones %>%
  mutate(
    Departamento = toupper(stri_trans_general(trimws(Departamento), "Latin-ASCII")),
    Departamento = case_when(
      Departamento == "CORONEL DE MARINA L ROSALES" ~ "CORONEL DE MARINA LEONARDO ROSALES",
      Departamento == "JUAN MARTIN DE PUEYRREDON" ~ "LA CAPITAL",
      TRUE ~ Departamento
    )
  ) %>%
  left_join(datos_delegaciones2, by = c("Departamento" = "DEPARTAMENTO"))


# Filtrar delegaciones no válidas al cargar los datos
datos_unidos <- datos_unidos %>% filter(!is.na(DELEGACION))

#Datos agregados
datos_agregados <- datos_unidos %>%
  group_by(DELEGACION,PROVINCIA,Cultivo,Campaña) %>%
  summarise(
    Produccion = sum(`Producción`, na.rm = TRUE),
    Sup_Sembrada = sum(`Sup. Sembrada`, na.rm = TRUE),
    Sup_Cosechada = sum(`Sup. Cosechada`, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(datos_delegaciones, "recursos/datos_delegaciones.rds")
saveRDS(datos_agregados, "recursos/datos_agregados.rds")

