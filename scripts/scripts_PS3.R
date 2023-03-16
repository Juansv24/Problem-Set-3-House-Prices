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

#--- Limpieza del entorno de trabajo
rm(list = ls())

#--- Cargar los paquetes necesarios
library(pacman)

p_load(tidyverse,ggplot2,sf,tmaptools,leaflet,tmaptools,osmdata, leaflet, 
       openxlsx,scales,skimr,stringi,SnowballC,stringr,rio, plotly, rgeos, 
       stargazer,ggExtra, gridExtra, tidyr, psych, readr)



### Cargar funciones

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))


#setwd("/Users/Dhiguera/Desktop/MECA UNIANDES/2301/PROBLEM SET/PS3/db")

#--- Establecer el directorio de la carpeta de datos

  setwd("C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/raw")

#--- Importar base de datos : train y test
  
  raw_train <- read_csv("train.csv")
  raw_test <- read_csv("test.csv")

#--- Visualizar las dimensiones de cada matriz de datos [train-test]
  
  dim(raw_train)  # Filas=38644 x Columnas=16
  dim(raw_test)   # Filas=10286 x Columnas=16

################################################################################
# Imputación e inclusión de  predictores imputados externos a los inmuebles
################################################################################
  
  
  summary(raw_train)
  
  # Tabulación
  table(raw_train$rooms)
  table(raw_train$bedrooms)
  table(raw_train$rooms)
  table(raw_train$property_type)  # Apartamento o Casa
  table(raw_train$operation_type) # Solo venta 
  
#--- Inspección de la base de datos [train]
  

### Expolorar missing value

  skim(raw_train)
  glimpse(raw_train)

# Tabla de frecuencia de variables con missing value

  tab_train_missings <- apply(raw_train, 2, function(x) sum(is.na(x)))
  tab_train_missings <- table(tab_train_missings)
  tab_train_missings

#---Descripción base de datos: [train]
      # Hay 10 variables no tienen missing value
      #--- property_id - city - price - moth - year - bedrooms property_type -operation_type
      #--- lat -lon
      
      # Una variable  tiene 12 missing value
      #--- description
      
      # Una variable  tiene 25 missing value
      #--- title
      
      # Una variable  tiene 10071 missing value
      #--- bathrooms
      
      # Una variable  tiene 18260 missing value
      #--- rooms
      
      # Dos variables  tienen 30790 missing value
      #--- surface_total -surface_covered


#--- Inspección de la base de datos [test]

### Expolorar missing value

  skim(raw_test)
  glimpse(raw_test)

# Tabla de frecuencia de variables con missing value

  tab_test_missings <- apply(raw_test, 2, function(x) sum(is.na(x)))
  tab_test_missings <- table(tab_test_missings)
  tab_test_missings

#---Descripción base de datos: test
# Hay 9 variables no tienen missing value
#--- property_id - city - moth - year - bedrooms property_type -operation_type
#--- lat -lon

# Una variable  tiene 3 missing value
#--- description

# Una variable  tiene 26 missing value
#--- title

# Una variable  tiene 2491 missing value
#--- bathrooms

# Una variable  tiene 4582 missing value
#--- rooms

# Una variable  tiene 7459 missing value
#--- surface_covered

# Una variable  tiene 8422 missing value
#--- surface_total 

# Una variable  tiene 10286 missing value
#--- price

#-----------------------------------------------------------------------------


################################################################################
  
  # 1.3 Completar informacion con expresiones regulares
  
  colnames(raw_train)[colnames(raw_train) == "title"] <- "titulo"
  colnames(raw_test)[colnames(raw_test) == "title"] <- "titulo"  

  objetos <- c("raw_train", "raw_test")
  for (obj in objetos) {
    
    
################################################################################

#----"Missing values" en la variable price [train]
  filtro_train <- is.na(raw_train$price)
  sum(filtro_train) ## la variable price no tiene "missing values" en la data train

  filtro_test <- is.na(raw_test$price)
  sum(filtro_test) ## la variable price  tiene 10286 "missing values"
  
#---Distribución del precio del inmueble 
  summary(raw_train$price) %>%
    as.matrix() %>%
    as.data.frame() %>%
    mutate(V1 = scales::dollar(V1))

  p1 <- ggplot(raw_train, aes(x = price)) +
    geom_histogram(fill = "darkblue", alpha = 0.4) +
    labs(x = "Precio (log-scale)", y = "Cantidad") +
    scale_x_log10(labels = scales::dollar) +
    theme_bw()
  ggplotly(p1)
 

### "Missing values" en la variable surface_total en train y test
colSums(is.na(raw_train)) # surface_total tiene 30790 NA
colSums(is.na(raw_test))  # surface_total tiene 8422 NA

#### Se imputan valores de surface_covered a surface_total en train y tests
raw_train <- raw_train %>% mutate(
  surface_total = 
    ifelse(is.na(surface_total),surface_covered, surface_total))
table(is.na(raw_train$surface_total)) # ahora 26551 NA en train

raw_test <- raw_test %>% mutate(
  surface_total = 
    ifelse(is.na(surface_total),surface_covered, surface_total))
table(is.na(raw_test$surface_total)) # ahora 6848 NA en test

# Para los apartamentos que en surface_total no tienen información y que en
#la descripción o el título tienen el área, ésta se imputará usando RegEX

####### Para train

#### Todo el texto en minúsculas
raw_train <- raw_train %>% 
  mutate(description = str_to_lower(string = raw_train$description))
raw_train <- raw_train %>% 
  mutate(title = str_to_lower(string = raw_train$title))

### Se eliminan los espacios extra
raw_train$description <- gsub("\\s+", " ", str_trim(raw_train$description))

### Se define el o los patrones para imputar el área del apartamento

p1 ="\\d{2,3}\\s?[m]"  # dado que los números de la descripción y el título 
#son enteros, no se adiciona un patrón que busque decimales

raw_train <- raw_train %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = raw_train$description,
                                            pattern = p1),
                                surface_total)
  )

raw_train <- raw_train %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = raw_train$title,
                                            pattern = p1),
                                surface_total)
  )

table(is.na(raw_train$surface_total)) #14527 NA


####### Para test

raw_test <- raw_test %>% 
  mutate(title = str_to_lower(string = raw_test$title))

raw_test <- raw_test %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = raw_test$description,
                                            pattern = p1),
                                surface_total)
  )

raw_test <- raw_test %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = raw_test$title,
                                            pattern = p1),
                                surface_total)
  )

table(is.na(raw_test$surface_total)) # ahora surface_total tiene 3627 NA en test


### Imputación de la variable baños en train y test

####### Para train
table(is.na(raw_train$bathrooms))  # 10071 NA

p2 = "\\d\\s?+(banos|bano)" 
p3 = "\\s?+(con bano|un bano|dos banos|tres banos)"

raw_train <- raw_train %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = raw_train$description,
                                        pattern = p2),
                            bathrooms)
  )

raw_train <- raw_train %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = raw_train$description,
                                        pattern = p3),
                            bathrooms)
  )

table(is.na(raw_train$bathrooms)) # 4282 NA

####### Para tests
table(is.na(raw_test$bathrooms))  # 2491 NA

raw_test <- raw_test %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = raw_test$description,
                                        pattern = p2),
                            bathrooms)
  )

raw_test <- raw_test %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = raw_test$description,
                                        pattern = p3),
                            bathrooms)
  )

table(is.na(raw_test$bathrooms)) # 866 NA

### Creación de la variable categórica tiene balcón/terraza, imputándola de la descripción, en train y test

####### Para train
p4 = "\\s?+(balcon|balcones|terraza|terrazas|patio)+\\s?"

raw_train$balcony <- NA

raw_train <- raw_train %>% 
  mutate(balcony = ifelse(is.na(balcony)== T,
                          str_extract(string = raw_train$description,
                                      pattern = p4),
                          balcony)
  )

table(is.na(raw_train$balcony)) #19225 NA

table(is.na(raw_train$balcony))
raw_train$balcony[is.na(raw_train$balcony)] <- 0 # 0 no tiene balcón/terraza/patio

raw_train <- raw_train %>% 
  mutate(balcony = ifelse(balcony!=0,
                          1,             # 1 tiene balcón/terraza/patio
                          balcony)
  )

####### Para test
raw_test$balcony <- NA

raw_test <- raw_test %>% 
  mutate(balcony = ifelse(is.na(balcony)== T,
                          str_extract(string = raw_test$description,
                                      pattern = p4),
                          balcony)
  )

table(is.na(raw_test$balcony)) #5427 NA

table(is.na(raw_test$balcony))
raw_test$balcony[is.na(raw_test$balcony)] <- 0 # 0 no tiene balcón/terraza/patio

raw_test <- raw_test %>% 
  mutate(balcony = ifelse(balcony!=0,
                          1,             # 1 tiene balcón/terraza/patio
                          balcony)
  )


### Creación de la variable categórica con garaje, imputándola de la descripción, en train y test

####### Para train
p5 = "\\s?+(garaje|garajes|parqueadero|parqueaderos)+\\s?"

raw_train$garage <- NA

raw_train <- raw_train %>% 
  mutate(garage = ifelse(is.na(garage)== T,
                         str_extract(string = raw_train$description,
                                     pattern = p5),
                         garage)
  )

table(is.na(raw_train$garage)) #11906 NA

table(is.na(raw_train$garage))
raw_train$garage[is.na(raw_train$garage)] <- 0 # 0 no tiene garaje

raw_train <- raw_train %>% 
  mutate(garage = ifelse(garage!=0,
                         1,             # 1 tiene garaje
                         garage)
  )

####### Para test
raw_test$garage <- NA

raw_test <- raw_test %>% 
  mutate(garage = ifelse(is.na(garage)== T,
                         str_extract(string = raw_test$description,
                                     pattern = p5),
                         garage)
  )

table(is.na(raw_test$garage)) #3169 NA

table(is.na(raw_test$garage))
raw_test$garage[is.na(raw_test$garage)] <- 0 # 0 no tiene garaje

raw_test <- raw_test %>% 
  mutate(garage = ifelse(garage!=0,
                         1,             # 1 tiene garaje
                         garage)
  )


### Se ajusta el formato de las variables imputadas

### Variable surface_total en train
raw_train <- raw_train %>% 
  mutate(surface_total = gsub('m', '', raw_train$surface_total))

raw_train <- raw_train %>% 
  mutate(surface_total = ifelse(surface_total == 'character(0)',
                                NA,
                                surface_total)
  )

raw_train$surface_total <- as.numeric(raw_train$surface_total)

### Variable surface_total en test
raw_test <- raw_test %>% 
  mutate(surface_total = gsub('m', '', raw_test$surface_total))

raw_test <- raw_test %>% 
  mutate(surface_total = ifelse(surface_total == 'character(0)',
                                NA,
                                surface_total)
  )

raw_test$surface_total <- as.numeric(raw_test$surface_total)


### Variable bathrooms en train
raw_train <- raw_train %>% 
  mutate(bathrooms = gsub('banos|bano', '', raw_train$bathrooms))

raw_train <- raw_train %>% 
  mutate(bathrooms = gsub('con|un', '1', raw_train$bathrooms))

raw_train <- raw_train %>% 
  mutate(bathrooms = gsub('dos', '2', raw_train$bathrooms))

raw_train <- raw_train %>% 
  mutate(bathrooms = gsub('tres', '3', raw_train$bathrooms))

raw_train$bathrooms <- as.numeric(raw_train$bathrooms)

### Variable bathrooms en test
raw_test <- raw_test %>% 
  mutate(bathrooms = gsub('banos|bano', '', raw_test$bathrooms))

raw_test <- raw_test %>% 
  mutate(bathrooms = gsub('con|un', '1', raw_test$bathrooms))

raw_test <- raw_test %>% 
  mutate(bathrooms = gsub('dos', '2', raw_test$bathrooms))

raw_test <- raw_test %>% 
  mutate(bathrooms = gsub('tres', '3', raw_test$bathrooms))

raw_test$bathrooms <- as.numeric(raw_test$bathrooms)

### Dicotomizar la variable tipo de propiedad en train y test
raw_train <- raw_train %>% 
  mutate(property_type = ifelse(property_type == 'Casa',
                          1,
                          0)
  )

raw_test <- raw_test %>% 
  mutate(property_type = ifelse(property_type == 'Casa',
                                1,
                                0)
  )


#---- Estudio de patrones espaciales e inclusión de cuatro predictores de fuentes externas


####### Estudio del patrón espacial del precio del inmueble 

#### Se eliminan las observaciones que no tienen información de latitud o longitud
filtro1 <- is.na(raw_train$lat) | is.na(raw_train$lon)
raw_train <- raw_train[!filtro1, ]

#### Revisión que las observaciones estén bien georeferenciadas y que estén dentro de Bogotá
limites <- getbb("Bogota Colombia")

#### Primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = raw_train$lon, 
             lat = raw_train$lat)

#### Se restringe la muestra a observaciones dentro de Bogotá
filtro1 <- between(raw_train$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(raw_train$lat, limites[2, "min"], limites[2, "max"])
filtro3 <- filtro1 & filtro2
raw_train <- raw_train[filtro3,] 



available_tags("leisure")

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 

parque_infantil <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "playground")

available_tags("amenity") 

hospital <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital")

clinica<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "clinic")

estacion_bus<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station")

policia<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police")

colegio<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school")

mercado<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "marketplace")


#---Gráficar mapas variables externas

#---Parques---#
parques_sf <- osmdata_sf(parques)

#--- Extraer geometria
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

parques_geometria

#---Plotear parques 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_geometria, col="green",
              opacity = 0.8, popup = parques_geometria$name)

#---Hospital---#
hospital_sf <- osmdata_sf(hospital)

#--- Extraer geometria
hospital_geometria <- hospital_sf$osm_polygons %>% 
  select(osm_id, name)

hospital_geometria

#---Plotear Hospital 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = hospital_geometria, col="#ADD8E6",
              opacity = 0.8, popup = hospital_geometria$name)


#---estacion_bus---#
estacion_bus_sf <- osmdata_sf(estacion_bus)

#--- Extraer geometria
estacion_bus_geometria <- estacion_bus_sf$osm_polygons %>% 
  select(osm_id, name)

estacion_bus_geometria

#---Plotear estacion_bus 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = estacion_bus_geometria, col="#555555",
              opacity = 0.8, popup = estacion_bus_geometria$name)

#---policia---#
policia_sf <- osmdata_sf(policia)

#--- Extraer geometria
policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name)

policia_geometria

#---Plotear policia 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = policia_geometria, col="#556B2F",
              opacity = 0.8, popup = policia_geometria$name)

#---colegio---#
colegio_sf <- osmdata_sf(colegio)

#--- Extraer geometria
colegio_geometria <- colegio_sf$osm_polygons %>% 
  select(osm_id, name)

colegio_geometria

#---Plotear colegio 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = colegio_geometria, col="#FFBF00",
              opacity = 0.8, popup = colegio_geometria$name)

#---mercado---#
mercado_sf <- osmdata_sf(mercado)

#--- Extraer geometria
mercado_geometria <- mercado_sf$osm_polygons %>% 
  select(osm_id, name)

mercado_geometria

#---Plotear mercado 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mercado_geometria, col="#FFA500",
              opacity = 0.8, popup = mercado_geometria$name)


#---Centroides

centroides_parques <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)
centroides_hospital <- gCentroid(as(hospital_geometria$geometry, "Spatial"), byid = T)
centroides_estacion_bus <- gCentroid(as(estacion_bus_geometria$geometry, "Spatial"), byid = T)
centroides_policia <- gCentroid(as(policia_geometria$geometry, "Spatial"), byid = T)
centroides_colegio <- gCentroid(as(colegio_geometria$geometry, "Spatial"), byid = T)
centroides_mercado <- gCentroid(as(mercado_geometria$geometry, "Spatial"), byid = T)


#---Plotear parques 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_geometria, col="green",
              opacity = 0.8, popup = parques_geometria$name)%>% 
  addCircles(lng = centroides_parques$x, 
             lat = centroides_parques$y, 
             col = "#800080", opacity = 0.5, radius = 1) 
htmlwidgets::saveWidget(parques, "views//parques.html")  

              
 #---Plotear Hospital 
              
              leaflet() %>%
                addTiles() %>%
                addPolygons(data = hospital_geometria, col="#ADD8E6",
                            opacity = 0.8, popup = hospital_geometria$name) %>% 
                              addCircles(lng = centroides_hospital$x, 
                                         lat = centroides_hospital$y, 
                                         col = "#800080", opacity = 0.5, radius = 1) 
                            htmlwidgets::saveWidget(parques, "views//hospital.html") 
                            
  #---Plotear estacion_bus 
                                          
leaflet() %>%
 addTiles() %>%
 addPolygons(data = estacion_bus_geometria, col="#555555",
 opacity = 0.8, popup = estacion_bus_geometria$name) %>% 
 addCircles(lng = centroides_estacion_bus$x, 
 lat = centroides_estacion_bus$y, 
col = "#800080", opacity = 0.5, radius = 1) 
htmlwidgets::saveWidget(parques, "views//estacion_bus.html")
                                                        
#---Plotear policia 
                                                        
leaflet() %>%
 addTiles() %>%
addPolygons(data = policia_geometria, col="#556B2F",
opacity = 0.8, popup = policia_geometria$name) %>% 
addCircles(lng = centroides_policia$x, 
lat = centroides_policia$y, 
col = "#800080", opacity = 0.5, radius = 1) 
htmlwidgets::saveWidget(parques, "views//policia.html") 
                                                                      
                                                                      
#---Plotear colegio 
                                                                      
leaflet() %>%
addTiles() %>%
addPolygons(data = colegio_geometria, col="#FFBF00",
opacity = 0.8, popup = colegio_geometria$name) %>% 
addCircles(lng = centroides_colegio$x, 
 lat = centroides_colegio$y, 
col = "#800080", opacity = 0.5, radius = 1) 
htmlwidgets::saveWidget(parques, "views//colegio.html")       
                                                                                    
                                                                                    
#---Plotear mercado 
                                                                                    
leaflet() %>%
addTiles() %>%
addPolygons(data = mercado_geometria, col="#FFA500",
opacity = 0.8, popup = mercado_geometria$name) %>% 
addCircles(lng = centroides_mercado$x, 
lat = centroides_mercado$y, 
col = "#800080", opacity = 0.5, radius = 1) 
htmlwidgets::saveWidget(parques, "views//.html") 

 # Convertir data espacial 
                                                                                                  
train_sf <- st_as_sf(raw_train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
test_sf <- st_as_sf(raw_test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326

options(scipen = 999)
write.csv(train_sf, file = "C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/work/train_imputada.csv")
write.csv(test_sf, file = "C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/work/test_imputada.csv")


dim(train_sf) # 38644    17
dim(test_sf) # 10286    17

centroides_parques_sf <- st_as_sf(centroides_parques, coords = c("x", "y"))
centroides_hospital_sf <- st_as_sf(centroides_hospital, coords = c("x", "y"))
centroides_estacion_bus_sf <- st_as_sf(centroides_estacion_bus, coords = c("x", "y"))
centroides_policia_sf <- st_as_sf(centroides_policia, coords = c("x", "y"))
centroides_colegio_sf <- st_as_sf(centroides_colegio, coords = c("x", "y"))
centroides_mercado_sf <- st_as_sf(centroides_mercado, coords = c("x", "y"))

#Aqui
#----Distancias parques 
dist_matrix_parques_train <- st_distance(x=train_sf, y=centroides_parques_sf)
dist_min_parques_train <- apply(dist_matrix_parques_train, 1 ,min)
train_sf$distancia_parque <- dist_min_parques_train

posicion <- apply(dist_matrix_parques_train, 1, function(x) which(min(x)==x))
area <- st_area(parques_geometria)
train_sf$area__parque <- area[posicion]
train_sf$area__parque <-as.numeric(train_sf$area__parque)


#----Distancias Hospital
dist_matrix_hospital_train <- st_distance(x=train_sf, y=centroides_hospital_sf)
dist_min_hospital_train <- apply(dist_matrix_hospital_train, 1 ,min)
train_sf$distancia_hospital <- dist_min_hospital_train

#----Distancias estacion_bus
dist_matrix_estacion_bus_train <- st_distance(x=train_sf, y=centroides_estacion_bus_sf)
dist_min_estacion_bus_train <- apply(dist_matrix_estacion_bus_train, 1 ,min)
train_sf$distancia_estacion_bus <- dist_min_estacion_bus_train

#----Distancias policia
dist_matrix_policia_train <- st_distance(x=train_sf, y=centroides_policia_sf)
dist_min_policia_train <- apply(dist_matrix_policia_train, 1 ,min)
train_sf$distancia_policia <- dist_min_policia_train

#----Distancias colegio
dist_matrix_colegio_train <- st_distance(x=train_sf, y=centroides_colegio_sf)
dist_min_colegio_train <- apply(dist_matrix_colegio_train, 1 ,min)
train_sf$distancia_colegio <- dist_min_colegio_train

#----Distancias mercado
dist_matrix_mercado_train <- st_distance(x=train_sf, y=centroides_mercado_sf)
dist_min_mercado_train <- apply(dist_matrix_mercado_train, 1 ,min)
train_sf$distancia_mercado <- dist_min_mercado_train


tab_train_missings_vf <- apply(train_sf, 2, function(x) sum(is.na(x)))
tab_train_missings_vf <- table(tab_train_missings_vf)
tab_train_missings_vf

dim(train_sf) # 38644    24


#----Distancias parques 
dist_matrix_parques_test <- st_distance(x=test_sf, y=centroides_parques_sf)
dist_min_parques_test <- apply(dist_matrix_parques_test, 1 ,min)
test_sf$distancia_parque <- dist_min_parques_test

posicion <- apply(dist_matrix_parques_test, 1, function(x) which(min(x)==x))
area <- st_area(parques_geometria)
test_sf$area__parque <- area[posicion]
test_sf$area__parque <-as.numeric(test_sf$area__parque)

#----Distancias Hospital
dist_matrix_hospital_test <- st_distance(x=test_sf, y=centroides_hospital_sf)
dist_min_hospital_test <- apply(dist_matrix_hospital_test, 1 ,min)
test_sf$distancia_hospital <- dist_min_hospital_test

#----Distancias estacion_bus
dist_matrix_estacion_bus_test <- st_distance(x=test_sf, y=centroides_estacion_bus_sf)
dist_min_estacion_bus_test <- apply(dist_matrix_estacion_bus_test, 1 ,min)
test_sf$distancia_estacion_bus <- dist_min_estacion_bus_test

#----Distancias policia
dist_matrix_policia_test <- st_distance(x=test_sf, y=centroides_policia_sf)
dist_min_policia_test <- apply(dist_matrix_policia_test, 1 ,min)
test_sf$distancia_policia <- dist_min_policia_test

#----Distancias colegio
dist_matrix_colegio_test <- st_distance(x=test_sf, y=centroides_colegio_sf)
dist_min_colegio_test <- apply(dist_matrix_colegio_test, 1 ,min)
test_sf$distancia_colegio <- dist_min_colegio_test

#----Distancias mercado
dist_matrix_mercado_test <- st_distance(x=test_sf, y=centroides_mercado_sf)
dist_min_mercado_test <- apply(dist_matrix_mercado_test, 1 ,min)
test_sf$distancia_mercado <- dist_min_mercado_test

tab_test_missings_vf <- apply(test_sf, 2, function(x) sum(is.na(x)))
tab_test_missings_vf <- table(tab_test_missings_vf)
tab_test_missings_vf

dim(test_sf) # 10286    24

options(scipen = 999)
write.csv(train_sf, file = "C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/work/train_final.csv")
write.csv(test_sf, file = "C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/work/test_final.csv")


setwd("C:/Users/andre/OneDrive/Github/Repositorios/Problem-Set-3-House-Prices/data/work")

## Guardar data
write.table(train_sf, "data_train_final.txt", sep = "\t", quote = F, row.names = F)
write.table(test_sf, "data_test_final.txt", sep = "\t", quote = F, row.names = F)

