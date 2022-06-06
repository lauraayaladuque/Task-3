#------------------------------Task 3-------------------------------------------
##Laura Ayala, 
##Harold Beltrán, 
##Maria Camila Pinillos

rm(list=ls()) 

#------------------------------Punto 1------------------------------------------
require(pacman)
library(sf)
library(tmap)
library(leaflet)
p_load(tidyverse,rio,sf,rgdal,leaflet,viridis,
       osmdata, 
       ggmap,   
       ggsn,   
       gtools, 
       tmap)  
install.packages('sf')
install.packages('tmap')
install.packages('rgdal')
install.packages('terra')
browseURL("https://www.openstreetmap.org/relation/8811628")

# 1. Datos espaciales

# 1.1. Estaciones de bus
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf()

amenities = osm$osm_points %>% dplyr::select(osm_id,amenity)

#Vias principales 
street = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

street = street$osm_lines %>% dplyr::select(osm_id,name)

#Vias del MIO
street = street %>%
  subset(str_detect(name,"MIO")==T)

#Limites de los barrios 

#  Limites de los barrios
barrio = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = 'admin_level', value = '9') %>% 
  osmdata_sf()

barrio = barrio$osm_multipolygons %>% dplyr::select(osm_id,name,geometry)

# 1.2 Visualizar informacion 
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=amenities, weight=1 , col="green")

#Visualizar vias principales del MIO
leaflet() %>%
  addTiles() %>%
  addPolylines(data=street,col="red")

#Visualizar limites barrios o manzanas
tmap::qtm(barrio)


# 1.3 Estimar distancias 

#Distancia promedio de barrio/manza hasta estacion
distancia_bs = st_distance(x=barrio, y=amenities)
View(distancia_bs)


mean_bs = apply(distancia_bs,1,mean)


barrio$means_bs = mean_bs

# Distancia promedio de barrio/manza hasta vías principales MIO
distancia_vias = st_distance(x=barrio, y=street)
View(distancia_vias)

#  Promedio por barrio de la distancia a cada via principal de MIO
mean_via = apply(distancia_vias,1,mean)


barrio$mean_via = mean_via

Tablas con las estadísticas descriptivas de las variables de distancias.
# Tabla con las estadísticas descriptivas para Promedio de la distancia de las estaciones de bus
sum_bs = summary(barrio$means_bs)
sum_bs
# Tabla con las estadísticas descriptivas para principales vias del mIO 

sum_via=summary(barrio$mean_via)
sum_via

# 1.4 Plot Mapping 

#Mapa cuartiles estacion de buses
# Los cuartiles de la variable de promedio de distancia hasta estacion de bus
quantiles_bs=quantile(barrio$means_bs, probs = seq(0, 1, 1/4))
quantiles_bs

# Categorizar los barrios por los cuartiles
barrio <- barrio %>%
  mutate(mean_bs_q = cut(mean_bs, breaks = quantiles_bs,
                         include.lowest = T, dig.lab=5))
# Graficas usando ggplot la informacion por cuartiles
plot_estaciones = ggplot() +
  geom_sf(data = barrio, aes(fill = mean_bs_q), color = "black", size = 0.25) + 
  scale_fill_brewer(name="Distancia promedio [m]", palette = "YlGn",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))

# Exportar el grafico a la carpeta output
ggsave(plot=plot_estaciones , filename="Task 3/Output/estación_Quartiles_De_Buses.pdf" , width=6.5 , height=8)

Mapa cuartiles vias principales de Transmilenio
# Los cuartiles de la variable de promedio de distancia hasta via principal de Transmilenio
quantiles_vias=quantile(barrio$mean_via, probs = seq(0, 1, 1/4))
quantiles_vias

# Categorizar los barrios por los cuartiles
barrio <- barrio %>%
  mutate(mean_vias_q = cut(mean_via, breaks = quantiles_vias,
                           include.lowest = T, dig.lab=5))

plot_vias = ggplot() +
  geom_sf(data = barrio, aes(fill = mean_vias_q), color = "black", size = 0.25) + 
  scale_fill_brewer(name="Distancia promedio [m]", palette = "YlGnBu",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))

# Exportar el grafico a la carpeta output
ggsave(plot=plot_vias , filename="estación_buses.pdf" , width=6.5 , height=8)

# 1.5 Exportar datos
export(barrio,"Task 3/Output/datos.rds") 

#------------------------------Punto 2------------------------------------------
rm(list=ls())
library(pacman)
library(xts)
library(readxl)
library(pacman)
p_load(tidyverse, # llamar y/o instalar las librerias de la clase
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer,# export tables to latex 
       rvest, # web-scraìng
       XML,   # web-scraìng
       xml2)  # web-scraìng

list.files()

setwd("output/")



# 2.1 Cargar base, importar datos y hacer modelo OLS

#Función para cargar base
fyle_sys.cargar_data_de_accientes=function(
    
  file="f_mapmuse.rds",
  deptos=c("NORTE DE SANTANDER")
  
){
  
  library(dplyr)
  #readxl::read_excel(file) %>%
  #  dplyr::filter(departamento %in%  deptos )
  readRDS(file)
  
}

#Ver base de datos
fyle_sys.cargar_data_de_accientes() %>% View()

data_de_accientes=fyle_sys.cargar_data_de_accientes()

#Función para sacar la regrsión OLS de las variables independientes 
data_de_accientes.modelo_para_fallecimiento=
  function(
    data_de_accientes=fyle_sys.cargar_data_de_accientes(),
    excluidas=""
  ){
    
    matriz_para_regresion=
      data_de_accientes %>%
      dplyr::select(- any_of( excluidas))
    
    lm(formula= fallecido~ ., data= matriz_para_regresion)
    
  }


#Crear Objeto con los coeficientes
ols=data_de_accientes.modelo_para_fallecimiento() 
class(ols)  
View(ols)


# 2.2 Exportar los graficos de los coeficientes

#Función para exportar gráfico
Ols.exportar_graficos_de_coeficientes=
  function(
    Ols=ols
  ){
    class(Ols)
    #
    # 1. crear graficos
    #
    coef_plot=coefplot::coefplot(Ols, intercept = FALSE)
    # 
    # 2. guardarlos en la carpeta de salidas
    #
    ggplot2::ggsave(
      "plot of coefficients.png", 
      plot=coef_plot,
      limitsize = TRUE)
    
  }

#Realizar función
Ols.exportar_graficos_de_coeficientes()


# 2.3  Modelos logit/probit

#FUnción para sacar estimaciones logit o probit
data_de_accientes.modelo_para_fallecimiento_por_logit_o_probit=
  function(
    data_de_accientes=fyle_sys.cargar_data_de_accientes(),
    excluidas="",
    usar_logit=TRUE
  ){
    
    matriz_para_regresion=
      data_de_accientes %>%
      dplyr::select(-any_of(excluidas)) %>%
      dplyr::mutate(fallecido=as.factor(fallecido))
    
    #
    # . devolver logit o probit segun preferencia del usuario de la funcion
    #
    if(usar_logit){
      
      glm(formula= fallecido~ ., data= matriz_para_regresion, family = binomial(link = "logit") ) %>%
        return()
      
    } else {
      
      glm(formula= fallecido~ ., data= matriz_para_regresion, family = binomial(link = "probit") ) %>%
        return()
      
    }  
  }


#Sacar regreción logit
logit=data_de_accientes.modelo_para_fallecimiento_por_logit_o_probit() 
class(logit)  
View(logit)
#Sacar regreción probit
probit=data_de_accientes.modelo_para_fallecimiento_por_logit_o_probit(
  usar_logit = FALSE
) 
class(probit)
View(probit)

# 2.4  Resultados de los de modelos en tabla

#Función con stargazer
tabla_de_coeficientes_de_todos_los_modelos=function(
    Ols=ols,
    Logit=logit,
    Probit=probit
){
  
  stargazer(
    Ols,
    Logit,
    Probit,
    type= 'text',
    title="Modelos para el fallecimiento",
    df = FALSE,
    digits = 3, 
    out = paste0("resultados.txt"))
}

#Exportar la tabla con la función
tabla_de_coeficientes_de_todos_los_modelos()


# 2.5  efectos marginales
logit=data_de_accientes.modelo_para_fallecimiento_por_logit_o_probit() 
class(Logit$model)
View(logit)

probit=data_de_accientes.modelo_para_fallecimiento_por_logit_o_probit(
  usar_logit = FALSE) 


modelos_no_lineales.efectos_marginal.plot=function(
    Logit=logit,
    Probit=probit,
    Plotted_Variables="dist_hospi"
){
  
  #
  logit_me=margins(Logit, variables="dist_hospi")
  probit_me<-margins(Probit$model,  variables=Plotted_Variables)
  #
  mods = list('Logit' =  logit_me,'Probit' = probit_me ) 
  #
  library(ggplot2)
  modelsummary::modelplot(mods)+ 
    coord_flip()+  
    labs(title = "Efecto marginal en probabilidad de muerte" ,
         subtitle = "Por Modelos no lineales")
}
#
modelos_no_lineales.efectos_marginal.plot()


#------------------------------Punto 3-------------------------------------------
#
# 0. set-up

rm(list=ls())
library(rvest)
library(XML)
library(xml2)

# 3.1 Leer html

myhtml = read_html("https://es.wikipedia.org/wiki/Departamentos_de_Colombia")

#3.2 Extraer titulo html 

myhtml %>% html_nodes(xpath = '//h1')
myhtml %>% html_nodes(xpath = '//h1[@id="firstHeading"]')
#En este caso sirve cualquiera

#3.3 Tablas de departamentos

tablas = myhtml %>% html_nodes('table') 
tablas[[4]] %>% html_table(header = T,fill=T) %>% View()



