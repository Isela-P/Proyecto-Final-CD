
#------------ 1 Configuracion y adquisicion de datos----------
#Cargar librerías
library(readr)
library(here)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(factoextra)


#Ingesta de datos
#lectura directa desde URL
calidad_aire<- read.csv("https://www.datos.gob.mx/dataset/bd428f6b-bc9a-4617-8c4b-9dd320101f30/resource/70dfeb69-065b-4ed4-8922-505602666250/download/d3_aire01_49_1.csv")

#Documentación
# Origen: Plataforma Nacional de datos Abiertos (Mexico) 
#URL: https://datos.gob.mx/dataset/calidad_aire_emisiones_contaminantes/resource/70dfeb69-065b-4ed4-8922-505602666250
# Fecha descarga: 23/07/2025
# Método lectura: read_csv, delimitador ",", codificación UTF-8
#Relevancia: En un contexto donde la calidad del aire empeora en 
#todos los Estados del País, conocer las principales fuentes de emisión
#así como los contaminantes principales nos podrían dar ideas para
#la mitigación de ellos.
#NOTA_ Es el inventario del año 2018


#Lectura local
incendios<- read_csv(here("data", "raw", "incendios.csv"))
#Documentación
# Origen: Plataforma Nacional de datos Abiertos (Mexico)
# URL: https://datos.gob.mx/dataset/incendios_forestales
#Fecha descarga: 23/07/2025
# Método lectura: read_csv, delimitador ",", codificación UTF-8
#Relevancia: Acorde al set de datos de la URL que nos muestra las
#fuentes de contaminación por entidad así como los tipos de contaminantes
#el conocer si existe una relación significativa entre la contaminación atmosférica
#y los incendios forestales, siendo estos últimos un problema que ultimamente
#afecta a un volcán muy importante en mi Estado. 

#----------- 2 Limpieza y estandarización ------------

#observamos rápidamente los datos
str(calidad_aire)
print(str(calidad_aire))
#Ya que existen 11 columnas. Debemos seleccionar unicamente las relevantes para el estudio
calidad_aire<- select(calidad_aire, Entidad, Municipio, Tipo_de_Fuente,PM_2_5,CO,SO_2,NOx)

str(calidad_aire)
summary(calidad_aire)

#Selección de columnas de interés
incendios<- select(incendios, Entidad, Municipio, Causa,Total_hectareas, Fecha_Termino)
str(incendios)
summary(incendios)

#Estudio únicamente del Estado de Puebla
calidad_aire<- filter(calidad_aire, Entidad== "Puebla")

incendios<- filter(incendios, Entidad=="Puebla")

#Detección y eliminación de Duplicados
sum(duplicated(calidad_aire))

calidad_aire<- calidad_aire[!duplicated(calidad_aire), ]

sum(duplicated(calidad_aire)) #rectificar eliminación de duplicados.


sum(duplicated(incendios)) 

incendios<- incendios[!duplicated(incendios), ]

sum(duplicated(incendios)) #rectificar la eliminación de duplicados.

#Oservar cuántos datos Nulos tenemos pro columna
summarise_all(calidad_aire, funs(sum(is.na(.))))
#Eliminar los datos nulos. 
calidad_aire<- na.omit(calidad_aire)
summarise_all(calidad_aire, funs(sum(is.na(.))))

summarise_all(incendios, funs(sum(is.na(.)))) #No existen datos Nulos.

#Estandarizar nombres de columnas
incendios<-janitor::clean_names(incendios)

calidad_aire<- janitor::clean_names(calidad_aire) #Debido a que 2 columnas
#mantienen nombres dificies se hará un rename

calidad_aire<-calidad_aire%>%
  rename(pm_25 = pm_2_5)

calidad_aire<-calidad_aire%>%
  rename(nox =n_ox)

calidad_aire<-calidad_aire%>%
  rename(so2 =so_2)

#rectificar formato de fechas


incendios<- incendios%>%
  mutate(fecha_termino=as.Date(fecha_termino, format("%Y-%m-%d"))
  )

#Guardar datos limpios
write.csv(incendios,"data/processed/clean_incendios.csv",row.names = FALSE)
write.csv(calidad_aire,"data/processed/clean_calidad_aire.csv",row.names = FALSE)

#-------3------Transformación y Unificación-----


#Selección de municipios de ESTUDIO

incendios <- incendios %>%
  filter(municipio %in% c("Puebla", "Atlixco", "San Pedro Cholula", "Libres", "Acajete", "Zacatlán", "Amozoc", "Huejotzingo" ))

calidad_aire<- calidad_aire%>%
  filter(municipio %in% c("Puebla", "Atlixco", "San Pedro Cholula", "Libres", "Acajete","Zacatlán", "Amozoc", "Huejotzingo"))

#selección del año de estudio (2018)
incendios <- incendios %>%
  filter(format(fecha_termino, "%Y") == "2018")

#La "repetición" de municipios es debido a que 
#en cada municipio existe un tipo de fuente, vamos a considerar
#las lecturas de los contaminantes en forma general (sin clasificar por tipo de fuente

calidad_aire <- calidad_aire %>%
  select(-tipo_de_fuente) %>%  # Eliminar columna que no se usará
  group_by(municipio) %>%
  summarise(
    pm_25 = mean(pm_25),
    co    = mean(co),
    so2   = mean(so2),
    nox   = mean(nox),
    .groups = "drop"
  )

#de BD "incendios" se eliminan columnas descartables. "Fechas de inicio y termino"

#graficar causas
conteo <- incendios %>%
  group_by(municipio, causa) %>%
  summarise(n = n(), .groups = "drop")

ggplot(conteo, aes(x = municipio, y = n, fill = causa)) +
  geom_bar(stat = "identity") +
  labs(title = "Causas de incendios por municipio",
       x = "Municipio",
       y = "Número de incendios",
       fill = "Causa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# grafico 2: hectareas perdidas por municipios

# Paso 1: Sumar las hectáreas perdidas por municipio
hectareas_municipio <- incendios %>%
  group_by(municipio) %>%
  summarise(total_hectareas = sum(total_hectareas, na.rm = TRUE)) %>%
  arrange(desc(total_hectareas))  # Para ordenar de mayor a menor

# Paso 2: Graficar
ggplot(hectareas_municipio, aes(x = reorder(municipio, -total_hectareas), y = total_hectareas)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Hectáreas perdidas por incendios en cada municipio",
       x = "Municipio",
       y = "Total de hectáreas perdidas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#el municipio de Zacatlán ha perdido muchas hectareas a comparación de Puebla. Sin embargo este ultimo queda en el top3
#a pesar de ser un municipio con pocos bosques.

incendios<- incendios %>%
  select( -fecha_termino) %>%  
  group_by(municipio) %>%
  summarise(
    total_hectareas= mean(total_hectareas, na.rm = TRUE),
    .groups = "drop"
  )

#Unir datos

Data<- full_join(incendios, calidad_aire, by="municipio")
#write.csv(Data,"/data/processed/final_dataset.csv",row.names = FALSE)
Data

#PCA
# Preparamos los datos excluyendo el nombre del municipio
datos_pca <- Data %>%
  select(-municipio) %>%
  na.omit() 

datos_pca
#Aplicamos PCA
pca_result <- prcomp(datos_pca, scale. = TRUE)

#Ver resumen
summary(pca_result)

# Graficar componentes
fviz_pca_biplot(pca_result,
                label = "var",
                habillage = Data$municipio[complete.cases(Data %>% select(-municipio))],
                palette = "jco",
                repel = TRUE,
                title = "Biplot PCA por municipio")

#fin