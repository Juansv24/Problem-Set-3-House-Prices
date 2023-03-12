#-------------------------------------------------------------------------------
# Titulo: "Predecir el precio de venta de inmuebles en la localidad de Chapinero en Bogotá D.C"
# Subtitulo: Problem Set 3
# Curso: "Big Data y Machine Learning"
#-------------------------------------------------------------------------------
# Autores:
# Andres Carrillo
# Diana Higuera
# Sebastian Vallejo
# Lizbeth Hernandez

# Creado: 2023/03/06
# Última modificación: 2023/03/12
# # Descripción:
#-------------------------------------------------------------------------------

### Parte 1: Preparación de las bases de datos y estadísticas descriptivas

#--- Limpieza del entorno
rm(list = ls())

#--- Cargar los paquetes necesarios
library(pacman)
p_load(tidyverse, ggplot2, openxlsx, scales, skimr, stringi, SnowballC, stringr,
       rio, plotly, leaflet, rgeos, tmaptools, sf, stargazer, osmdata)

### Cargar funciones

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))


### Cargar las bases

#setwd("/Users/Dhiguera/Desktop/MECA UNIANDES/2301/PROBLEM SET/PS3/db")

setwd("C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/Data/raw/")

raw_test <- read.csv2("test.csv", 
                               sep = ",", 
                               dec = ".", 
                               fileEncoding = "UTF-8")

raw_train <- read.csv2("train.csv", 
                               sep = ",", 
                               dec = ".", 
                               fileEncoding = "UTF-8")



#---------------------------------------- Ajuste de las bases e inclusión de dos predictores imputados de la descripción de cada inmueble

### Inspección de las bases
glimpse(train)
glimpse(test)

### "Missing values" en la variable price 
filtro <- is.na(train$price)
sum(filtro) ## la variable price no tiene "missing values"

### Distribución del precio del inmueble 
summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

p1 <- ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Precio (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p1)
 

### "Missing values" en la variable surface_total en train y test
colSums(is.na(train)) # surface_total tiene 30790 NA
colSums(is.na(test))  # surface_total tiene 8422 NA

#### Se imputan valores de surface_covered a surface_total en train y tests
train <- train %>% mutate(
  surface_total = 
    ifelse(is.na(surface_total),surface_covered, surface_total))
table(is.na(train$surface_total)) # ahora 26551 NA en train

test <- test %>% mutate(
  surface_total = 
    ifelse(is.na(surface_total),surface_covered, surface_total))
table(is.na(test$surface_total)) # ahora 6848 NA en test

#### Para los apartamentos que en surface_total no tienen información y que en la descripción o el título tienen el área, ésta se imputará usando RegEX

####### Para train

#### Todo el texto en minúsculas
train <- train %>% 
  mutate(description = str_to_lower(string = train$description))
train <- train %>% 
  mutate(title = str_to_lower(string = train$title))
### Se eliminan los espacios extra
train$description <- gsub("\\s+", " ", str_trim(train$description))

### Se define el o los patrones para imputar el área del apartamento

p1 ="\\d{2,3}\\s?[m]"  # dado que los números de la descripción y el título son enteros, no se adiciona un patrón que busque decimales

train <- train %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = train$description,
                                            pattern = p1),
                                surface_total)
  )

train <- train %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = train$title,
                                            pattern = p1),
                                surface_total)
  )

table(is.na(train$surface_total)) #14527 NA


####### Para test

test <- test %>% 
  mutate(title = str_to_lower(string = test$title))

test <- test %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = test$description,
                                            pattern = p1),
                                surface_total)
  )

test <- test %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = test$title,
                                            pattern = p1),
                                surface_total)
  )

table(is.na(test$surface_total)) # ahora surface_total tiene 3627 NA en test


### Imputación de la variable baños en train y test

####### Para train
table(is.na(train$bathrooms))  # 10071 NA

p2 = "\\d\\s?+(banos|bano)" 
p3 = "\\s?+(con bano|un bano|dos banos|tres banos)"

train <- train %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = train$description,
                                        pattern = p2),
                            bathrooms)
  )

train <- train %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = train$description,
                                        pattern = p3),
                            bathrooms)
  )

table(is.na(train$bathrooms)) # 4282 NA

####### Para tests
table(is.na(test$bathrooms))  # 2491 NA

test <- test %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = test$description,
                                        pattern = p2),
                            bathrooms)
  )

test <- test %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = test$description,
                                        pattern = p3),
                            bathrooms)
  )

table(is.na(test$bathrooms)) # 866 NA

### Creación de la variable categórica tiene balcón/terraza, imputándola de la descripción, en train y test

####### Para train
p4 = "\\s?+(balcon|balcones|terraza|terrazas|patio)+\\s?"

train$balcony <- NA

train <- train %>% 
  mutate(balcony = ifelse(is.na(balcony)== T,
                          str_extract(string = train$description,
                                      pattern = p4),
                          balcony)
  )

table(is.na(train$balcony)) #19225 NA

table(is.na(train$balcony))
train$balcony[is.na(train$balcony)] <- 0 # 0 no tiene balcón/terraza/patio

train <- train %>% 
  mutate(balcony = ifelse(balcony!=0,
                          1,             # 1 tiene balcón/terraza/patio
                          balcony)
  )

####### Para test
test$balcony <- NA

test <- test %>% 
  mutate(balcony = ifelse(is.na(balcony)== T,
                          str_extract(string = test$description,
                                      pattern = p4),
                          balcony)
  )

table(is.na(test$balcony)) #5427 NA

table(is.na(test$balcony))
test$balcony[is.na(test$balcony)] <- 0 # 0 no tiene balcón/terraza/patio

test <- test %>% 
  mutate(balcony = ifelse(balcony!=0,
                          1,             # 1 tiene balcón/terraza/patio
                          balcony)
  )


### Creación de la variable categórica con garaje, imputándola de la descripción, en train y test

####### Para train
p5 = "\\s?+(garaje|garajes|parqueadero|parqueaderos)+\\s?"

train$garage <- NA

train <- train %>% 
  mutate(garage = ifelse(is.na(garage)== T,
                         str_extract(string = train$description,
                                     pattern = p5),
                         garage)
  )

table(is.na(train$garage)) #11906 NA

table(is.na(train$garage))
train$garage[is.na(train$garage)] <- 0 # 0 no tiene garaje

train <- train %>% 
  mutate(garage = ifelse(garage!=0,
                         1,             # 1 tiene garaje
                         garage)
  )

####### Para test
test$garage <- NA

test <- test %>% 
  mutate(garage = ifelse(is.na(garage)== T,
                         str_extract(string = test$description,
                                     pattern = p5),
                         garage)
  )

table(is.na(test$garage)) #3169 NA

table(is.na(test$garage))
test$garage[is.na(test$garage)] <- 0 # 0 no tiene garaje

test <- test %>% 
  mutate(garage = ifelse(garage!=0,
                         1,             # 1 tiene garaje
                         garage)
  )


### Se ajusta el formato de las variables imputadas

### Variable surface_total en train
train <- train %>% 
  mutate(surface_total = gsub('m', '', train$surface_total))

train <- train %>% 
  mutate(surface_total = ifelse(surface_total == 'character(0)',
                                NA,
                                surface_total)
  )

train$surface_total <- as.numeric(train$surface_total)

### Variable surface_total en test
test <- test %>% 
  mutate(surface_total = gsub('m', '', test$surface_total))

test <- test %>% 
  mutate(surface_total = ifelse(surface_total == 'character(0)',
                                NA,
                                surface_total)
  )

test$surface_total <- as.numeric(test$surface_total)


### Variable bathrooms en train
train <- train %>% 
  mutate(bathrooms = gsub('banos|bano', '', train$bathrooms))

train <- train %>% 
  mutate(bathrooms = gsub('con|un', '1', train$bathrooms))

train <- train %>% 
  mutate(bathrooms = gsub('dos', '2', train$bathrooms))

train <- train %>% 
  mutate(bathrooms = gsub('tres', '3', train$bathrooms))

train$bathrooms <- as.numeric(train$bathrooms)

### Variable bathrooms en test
test <- test %>% 
  mutate(bathrooms = gsub('banos|bano', '', test$bathrooms))

test <- test %>% 
  mutate(bathrooms = gsub('con|un', '1', test$bathrooms))

test <- test %>% 
  mutate(bathrooms = gsub('dos', '2', test$bathrooms))

test <- test %>% 
  mutate(bathrooms = gsub('tres', '3', test$bathrooms))

test$bathrooms <- as.numeric(test$bathrooms)

### Dicotomizar la variable tipo de propiedad en train y test
train <- train %>% 
  mutate(property_type = ifelse(property_type == 'Casa',
                          1,
                          0)
  )

test <- test %>% 
  mutate(property_type = ifelse(property_type == 'Casa',
                                1,
                                0)
  )






#---------------------------------------- Estudio de patrones espaciales e inclusión de cuatro predictores de fuentes externas


####### Estudio del patrón espacial del precio del inmueble 

#### Se eliminan las observaciones que no tienen información de latitud o longitud
filtro1 <- is.na(train$lat) | is.na(train$lon)
train <- train[!filtro1, ]

#### Revisión que las observaciones estén bien georeferenciadas y que estén dentro de Bogotá
limites <- getbb("Bogota Colombia")

#### Primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

#### Se restringe la muestra a observaciones dentro de Bogotá
filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])
filtro3 <- filtro1 & filtro2
train <- train[filtro3,] 


