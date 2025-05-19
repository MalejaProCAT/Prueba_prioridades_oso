# Prueba_prioridades_oso
Rutina documentada en R para identificar áreas prioritarias para el oso andino.
---
title: "Prueba Técnica"
author: "Maleja Parrado-Vargas"
date: "2025-05-18"
output: html_document
---
### Cargar paquetes y librería
```{r cargar pqt y carpeta, include=FALSE}
library(terra)
library(sf)
setwd("~/Alejandra/Convocatoria Humboldt/Prueba/Prueba/Features")
```
# MANTENER LAS ÁREAS DE IMPORTANCIA PARA LA CONSERVACIÓN DEL OSO ANDINO EN LOS PÁRAMOS DE COLOMBIA

## Qué conservar

El oso de anteojos (*Tremarctos ornatus*) es una especie presente en los Andes tropicales (Vela-Vargas et al., 2021).- Específicamente en Colombia, se ha registrado en áreas de alta montaña Andina en bosques altoandinos y páramos (Gonzáles-Maya et al., 2021), donde la intervención antrópica ha aumentado en los últimos años (Etter et al., 2000; Armenteras et al., 2011), por esta razón su hábitat disponible se ha reducido a más del 15% en Colombia (Cruz-Rodriguez et al., 2020).

Teniendo en cuenta esto y con el fin de saber en dónde y cómo conservar, se propone un modelo de flujo de trabajo para definir soluciones basadas en área, que sean eficientes y poco costosas para la conservación de la especie (Margules & Pressey, 2000), que se concentren principalmente en zonas donde se distribuye la especie y que garantice el área mínima de 3800 Km2 para que una población sea viable (Marquez et al., 2018), en este caso, y considerando las amenazas que afectan a la especie, principalmente mediadas por la pérdida y fragmetnación de hábitat (Kattan et al., 2008) y las interacciones negativas oso-humano se definieron áreas con opciones de manejo diferentes a áreas protegidas, principalmente para favorecer la coexistencia humano-fauna silverstre (Frank et al., 2019).

```{r Inputs, echo=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
#Unidad de planificación 
UP<- st_read("~/Alejandra/Convocatoria Humboldt/Prueba/Prueba/Unidad de planificación/Complejos de Paramos_Escala100k.shp")
UP<-vect(UP)
# Features
Oso<-rast("~/Alejandra/Convocatoria Humboldt/Prueba/Prueba/Features/Tremarctos_ornatus.tif")
# Costs & constraints 
HH_2018<-rast("~/Alejandra/Convocatoria Humboldt/Prueba/Prueba/Costos/HH_2018.tif")
B_neto<-rast("~/Alejandra/Convocatoria Humboldt/Prueba/Prueba/Costos/B_neto.tif")
```

### Procesamiento capas inicial

Con el fin de manejar de manera eficiente la rutina de análisis, se organizaron los rasters a través del procesamiento de capas, alineandolas en extensión, resolución y dimensión. 

```{r capas}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
##Cortar capas a las unidades de planificación
HH_2018 <- mask(crop(HH_2018, UP), UP)
Oso <- mask(crop(Oso, UP), UP)
B_neto <- mask(crop(B_neto, UP), UP)
plot(HH_2018)
# Alinear las capas a la misma extensión, resolución y dimensión 
Oso <- terra::resample(Oso, HH_2018, method = "bilinear")
B_neto <- terra::resample(B_neto, HH_2018, method = "bilinear")
### Reemplazar NA's por 0
Oso[is.na(Oso)] <- 0
B_neto[is.na(B_neto)] <- 0

```

# Identificar prioridades a través de sobreposición de prioridades vs costos
## Cómo conservar

En un paisaje de transformación alto como lo evidenciado en los últimos años en los Andes colombianos (Armenteras et al.,2011), se propone en esta aproximación no solo identificar áreas para la expansión de áreas protegidas, sino que  teniendo en cuenta las amenazas directas a la especie principalmente la pérdida de hábitat y las interacciones negativas humano-Oso andino (Kattan et al., 2008; Parra-Romero et al., 2019) y la alta capacidad de movilidad (Vera-Vargas et al.,2021), se plantea involucrar otro tipo de manejos de áreas que involucren territorios con intervención humana media-alta con el fin de disminuir los conflictos por el uso del territorio y optimizar de mejor manera los recursos técnicos y financieros (Hanson et al., 2024).
 
### Preparación y organización de los costos 
#### Huella Humana
 
```{r features}

# Reclasificar usando los valores teniendo en cuenta lo propuesto por (Correa-Ayram et al.,2020)para identificar el cómo conservar 
## Crear matriz para ponderar importancia de Íntervención antrópica
m <- matrix(c(
  -Inf, 40,   2,  # Natural-Bajo (AP)
   40, 60,  1,   # Medio (Manejo)
   60, Inf,  0    # Alto (Exclusión)
), ncol = 3, byrow = TRUE)

m
## Reclasificar 
HH_2018 <- classify(HH_2018, m)
plot(HH_2018)
## Definir prioridad de distribución T. ornatus con exclusión de áreas con alta Huella Humana 2018

Oso_presence<-Oso > 0
### Generar área vacía para asignar ponderación a áreas de presencia de la especie con una meta del
P1<-HH_2018 *NA
P1[Oso_presence== 1 & HH_2018 == 1] <- 2  # Manejo de áreas
P1[Oso_presence == 1 & HH_2018  == 2] <- 1  # Definición de AP
plot(P1, main = "Ponderación presencia de oso y manejo")
##Exportar raster prioridad Oso 
#writeRaster(P1, "prioridad_manejo.tif", overwrite=TRUE)

```
## Definir la solución con base en las metas del 15% al área mínima que quiero conservar basado en la meta Aichi (CBD,2010) 

```{r umbral}
 # Extraer celdas candidatas (AP) 
candidatas <- which(values(P1) %in% c(1))## Selecciono sólo las áreas donde es más eficiente hacer áreas protegidas porque tiene IHH natural o baja

n_celdas <- length(candidatas) # Número total de celdas candidatas

n_select <- round(0.15 * n_celdas)# Número a seleccionar para 15%
set.seed(123) # reproducibilidad
# Seleccionar aleatoriamente
seleccionadas <- sample(candidatas, n_select)

solucion_1<- rast(P1)
values(solucion_1) <- 0
values(solucion_1)[seleccionadas] <- 1 # Crear raster de solución de conservación (0/1)
plot(solucion_1, main = "Solución conservación 15%")
#writeRaster(solucion_1, "prioridad_manejo.tif", overwrite=TRUE)+

# Definir la solución con base en las metas del 30% al área mínima que quiero conservar basado en la meta Aichi (CBD,2010)
## Comparación si se define la meta Kunming-Montreal Global Biodiversity Framework  (30%) (CBD, 2022)

candidatas <- which(values(P1) %in% c(1)) ## Selecciono sólo las áreas donde es más eficiente hacer áreas protegidas porque tiene IHH # Extraer celdas candidatas (AP) natural o baja

n_celdas <- length(candidatas)# Número total de celdas candidatas

n_select <- round(0.30* n_celdas)# Número a seleccionar para 15%
set.seed(123) # reproducibilidad

seleccionadas <- sample(candidatas, n_select)# Seleccionar aleatoriamente

solucion_30<- rast(P1)
values(solucion_30) <- 0
values(solucion_30)[seleccionadas] <- 1# Crear raster de solución de conservación (0/1)
plot(solucion_30, main = "Solución conservación 30%")

```
## Exclusión de áreas con alto beneficio neto para reducir costos de la inversión de conservación del Oso andino, por estar dentro de áreas con alta rentabilidad económica (Marín et al., 2023)

Se excluyeron todas las áreas con alta renta agropecuaria (mediana de los valores positivos >$39.651.530)

```{r B neto}
## Ponderación Beneficio neto
summary(B_neto)
plot(B_neto)

# Extraer valores del raster
valores <- values(B_neto, na.rm = TRUE)
# Filtrar sólo los valores positivos
valores_pos <- valores[valores > 0]
# Calcular el percentil 50 (la mediana) entre los positivos
umbral_pos <- quantile(valores_pos, 0.5)

# Reclasificar solo los valores positivos en dos clases
m <- matrix(c(
  0, umbral_pos, 1,               # Renta agropecuaria baja (Mediana <$39.651.530)
  umbral_pos, max(valores), 2     # Renta agropecuaria > $39.651.530
), ncol = 3, byrow = TRUE)
m
CO <- classify(B_neto, m)


## Definir prioridad de distribución T. ornatus con exclusión de áreas con alto beneficio neto (Marin et al., 2023)
Oso_presence<-Oso > 0
### Generar área vacía para asignar ponderación a áreas de presencia de la especie
P2<-CO *NA
P2[Oso_presence== 1 & CO == 1] <- 1  # Prioridad de AP de bajo costo 
P2[Oso_presence== 1 & CO == 0] <- 1  # Prioridad de AP de bajo costo 
plot(P2, col="green")

##Exportar raster prioridad Oso 
#writeRaster(P2, "prioridad_CO.tif", overwrite=TRUE)

```

## Definir la solución con base en las metas del 15% al área mínima que quiero conservar basado en la meta Aichi (CBD,2010) 

```{r umbral_BN}
# Extraer celdas candidatas (AP)
candidatas <- which(values(P2) %in% c(1)) ## Selecciono sólo las áreas donde es más eficiente hacer áreas protegidas porque tiene IHH natural o baja
# Número total de celdas candidatas
n_celdas <- length(candidatas)
# Número a seleccionar para 15%
n_select <- round(0.15 * n_celdas)
set.seed(123) # reproducibilidad
# Seleccionar aleatoriamente
seleccionadas <- sample(candidatas, n_select)
# Crear raster de solución de conservación (0/1)
solucion_15_BN<- rast(P2)
values(solucion_15_BN) <- 0
values(solucion_15_BN)[seleccionadas] <- 1
plot(solucion_15_BN, main = "Solución conservación 15% costo=BNT")
#writeRaster(solucion_1, "prioridad_manejo.tif", overwrite=TRUE)+

### Comparación si se define la meta Kunming-Montreal Global Biodiversity Framework  (30%) (CBD, 2022)
 
## Definir la solución con base en las metas del 30% al área mínima que quiero conservar basado en la meta Aichi (CBD,2010)
# Extraer celdas candidatas (AP)
candidatas <- which(values(P2) %in% c(1)) ## Selecciono sólo las áreas donde es más eficiente hacer áreas protegidas porque tiene IHH natural o baja
# Número total de celdas candidatas
n_celdas <- length(candidatas)
# Número a seleccionar para 15%
n_select <- round(0.30* n_celdas)
set.seed(123) # reproducibilidad
# Seleccionar aleatoriamente
seleccionadas <- sample(candidatas, n_select)
# Crear raster de solución de conservación (0/1)
solucion_30_BN<- rast(P2)
values(solucion_30_BN) <- 0
values(solucion_30_BN)[seleccionadas] <- 1
plot(solucion_30_BN, main = "Solución conservación 30% costo=BNT")



