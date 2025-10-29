library(rgbif) #descargar datos de ocurrencias
library(tidyverse) #procesamiento de datos
library(sf) #manipulación  de datos vectoriales
library(rworldxtra) #datos vectoriales de los paises del mundo
library(geodata) #datos geoespaciales complemenatarios
library(ggspatial)#auxiliar para visualizar datos espaciales
library(terra) #datos raster
library(tidyterra) #maniipulaci?n de raster
library(paletteer) #colores
library(ggcorrplot) #diagrama de correlaciones
library(ggridges) #gráfico de ridges
library(plotly) #gráficos avanzados
library(patchwork) #organizar gr?ficos
library(magick) #para manejo de imagenes
library(grid)

# 1. EXTRAER DATOS DE FOCAS DE CADA PAÍS DE NORTE AMÉRICA
#Vitulina MX
foca1mx <- occ_search(scientificName = "Phoca vitulina Linnaeus", 
                    hasCoordinate = TRUE, 
                    hasGeospatialIssue = FALSE,
                    country = "MX"
                    )$data

foca1mx <- foca1mx %>% 
  filter(scientificName == "Phoca vitulina Linnaeus, 1758") #eliminar subespecie

#richardii MX
foca2mx <- occ_search(scientificName = "Phoca vitulina richardii", 
                      hasCoordinate = TRUE, 
                      hasGeospatialIssue = FALSE,
                      country = "MX"
)$data
##verificar que los datos sean unicamente de mx
unique(foca1mx$countryCode)
unique(foca2mx$countryCode)

#Vitulina CA
foca1ca <- occ_search(scientificName = "Phoca vitulina Linnaeus ", 
                      hasCoordinate = TRUE, 
                      hasGeospatialIssue = FALSE,
                      country = "CA"
)$data
foca1ca <- foca1ca %>% 
  filter(scientificName == "Phoca vitulina Linnaeus, 1758") #eliminar subespecie

#richardii CA
foca2ca <- occ_search(scientificName = "Phoca vitulina richardii", 
                      hasCoordinate = TRUE, 
                      hasGeospatialIssue = FALSE,
                      country = "CA"
)$data

###verificar que los datos sean unicamente de ca
unique(foca1ca$countryCode)
unique(foca2ca$countryCode)

#Vitulina US
foca1us <- occ_search(scientificName = "Phoca vitulina Linnaeus ", 
                      hasCoordinate = TRUE, 
                      hasGeospatialIssue = FALSE,
                      country = "US"
)$data
foca1us <- foca1us %>% 
  filter(scientificName == "Phoca vitulina Linnaeus, 1758")
#richardii US
foca2us <- occ_search(scientificName = "Phoca vitulina richardii", 
                      hasCoordinate = TRUE, 
                      hasGeospatialIssue = FALSE,
                      country = "US"
)$data

###verificar que los datos sean unicamente de us
unique(foca1us$countryCode)
unique(foca2us$countryCode)

# Añadir una columna para indicar el país 
foca1mx <- foca1mx %>% mutate(country = "Mexico", species = "Phoca vitulina Linnaeus")
foca1ca <- foca1ca %>% mutate(country = "Canada", species = "Phoca vitulina Linnaeus")
foca1us <- foca1us %>% mutate(country = "USA", species = "Phoca vitulina Linnaeus")

foca2mx <- foca2mx %>% mutate(country = "Mexico", species = "Phoca vitulina")
foca2ca <- foca2ca %>% mutate(country = "Canada", species = "Phoca vitulina")
foca2us <- foca2us %>% mutate(country = "USA", species = "Phoca vitulina")

#Unir bases de cada país por foca
foca1_NorteA <- bind_rows(foca1mx, foca1ca, foca1us)
foca2_NorteA <- bind_rows(foca2mx, foca2ca, foca2us)

#añadir columna con spp y subespecie
foca1_NorteA <- foca1_NorteA %>% mutate(species = "Phoca vitulina ")
foca2_NorteA <- foca2_NorteA %>% mutate(species = "Phoca vitulina richardii")

#filtrar por tipo de registro
##para phoca vitulina
unique(foca1_NorteA$basisOfRecord)
foca1_NorteA %>% 
  ggplot(aes(x= basisOfRecord, fill= basisOfRecord))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none") 
foca1_NorteA <- foca1_NorteA %>% 
  filter(basisOfRecord %in% c(c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))) #dejar unicamente observaciones humanas y especimenes preservados

##para phoca vitulina richardii
unique(foca2_NorteA$basisOfRecord)
foca2_NorteA %>% 
  ggplot(aes(x= basisOfRecord, fill= basisOfRecord))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none") 
foca2_NorteA <- foca2_NorteA %>% 
  filter(basisOfRecord %in% c(c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))) #dejar unicamente observaciones humanas y especimenes preservados

#filtrar por institución de registro
##para phoca vitulina richardii
unique(foca1_NorteA$institutionCode)

foca1_NorteA %>% 
  ggplot(aes(x= institutionCode, fill= institutionCode))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none") 
#remover na
foca1_NorteA <- foca1_NorteA %>% 
  filter(!is.na(institutionCode))

##para phoca vitulina richardii
unique(foca2_NorteA$institutionCode)
foca2_NorteA %>% 
  ggplot(aes(x= institutionCode, fill= institutionCode))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none") 

#como no hay ningún na no se eliminaron

#ARCHIVOS SF
foca1_NorteA_sf <- foca1_NorteA %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

foca2_NorteA_sf <- foca2_NorteA %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

#CAPA RASTER Y ARCHIVOS VECTORIALES CLIMA
alt <- worldclim_global(var="elev", res=5, path=tempdir())
data(countriesHigh) 
Mundo <- st_as_sf(countriesHigh) 

#MAPA NORTE AMÉRICA
map_occ <-ggplot()+
  geom_spatraster(data= alt, alpha= 0.5)+
  geom_sf(data= foca1_NorteA_sf, aes(col = species))+ 
  geom_sf(data= foca2_NorteA_sf, aes(col = species))+
  #color de los puntos determinados por foca
  geom_sf(data= Mundo, fill= NA, linewidth=0.3)+ 
  #añade capa mundo transparente (shapefile con datos geográficos) y se especifica grosor de linea
  coord_sf(xlim = c(-135, -55), ylim = c(27, 60)) + 
  #se hace zoom según coordenadas
  scale_color_manual(values = c("blue", "red")) + 
  #añade color para puntos solidos
  scale_fill_paletteer_c("grDevices::terrain.colors", 
                         #rellena el resto del mapa
                         limits = c(0, 5000),
                         na.value = "transparent")+
  annotation_north_arrow(location = "bl",
                         which_north="true",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(0.9, "in"),
                         style=north_arrow_nautical(fill = c("white", "grey60")))+ #añade rosa de los vientos 
  annotation_scale(location = "bl",
                   bar_cols = c("grey60", "white"), 
                   text_family = "ArcherPro Book")+ #añade escala
  labs(fill= "Altitud (msnm)", col= "Focas") #cambia etiquetas
#
library(ggplot2)
library(ggspatial)
library(paletteer)

map_occ_bonito <- ggplot() +
  # Capa base: altitud
  geom_spatraster(data = alt, alpha = 0.8) +
  
  # Borde de países
  geom_sf(data = Mundo, fill = NA, color = "gray40", linewidth = 0.2) +
  
  # Capas vectoriales de focas
  geom_sf(data = foca1_NorteA_sf, aes(color = species), size = 2, alpha = 0.5) +
  geom_sf(data = foca2_NorteA_sf, aes(color = species), size = 2, alpha = 0.5) +
  
  # Zoom a Norteamérica
  coord_sf(xlim = c(-135, -55), ylim = c(27, 60)) +
  
  # Escala de altitud (más estética)
  scale_fill_paletteer_c(
    "viridis::viridis",
    direction = -1,
    limits = c(0, 5000),
    na.value = "transparent",
    name = "Altitud (m s.n.m.)"
  ) +
  
  # Colores de especies
  scale_color_manual(
    values = c("#002F70","#742324"),
    
    name = "Especies"
  ) +
  # Título y estilo visual
  labs(
    title = "Distribución de Phoca vitulina y P. v. richardii en América del Norte",
    subtitle = "Registros de ocurrencia (GBIF, 2007–2024)",
    caption = "Elaborado por Equipo 4  |  Datos: GBIF & WorldClim v2.1"
  ) +
  
  #Rosa de los vientos
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.9, "in"),
    style = north_arrow_nautical(fill = c("white", "gray60"))
  ) +
  #Escala
  annotation_scale(
    location = "bl", bar_cols = c("gray60", "white"), text_family = "lato"
  ) +
  # Theme 
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0.5),
    legend.position = c(0.86, 0.25),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

map_occ


# MAPAS POR PAÍS:
map_mx_occ <-ggplot()+
  geom_spatraster(data= alt, alpha= 0.5)+
  geom_sf(data= foca1_NorteA_sf, aes(col = species))+ 
  geom_sf(data= foca2_NorteA_sf, aes(col = species))+
  geom_sf(data= Mundo, fill= NA, linewidth=0.3)+ 
  coord_sf(xlim = c(-118, -110), ylim = c(25, 33)) + 
  scale_color_manual(values = c("blue", "red")) + 
  scale_fill_paletteer_c("grDevices::terrain.colors", 
                         limits = c(0, 5000),
                         na.value = "transparent")+
  annotation_north_arrow(location = "bl",
                         which_north="true",
                         pad_x = unit(0.2, "in"),
                         pad_y = unit(0.7, "in"),
                         style=north_arrow_nautical(fill = c("white", "grey60")))+
  annotation_scale(location = "bl",
                   bar_cols = c("grey60", "white"), 
                   text_family = "ArcherPro Book")+
  labs(fill= "Altitud (msnm)", col= "Focas")

map_us_occ <-ggplot()+
  geom_spatraster(data= alt, alpha= 0.5)+
  geom_sf(data= foca1_NorteA_sf, aes(col = species))+ 
  geom_sf(data= foca2_NorteA_sf, aes(col = species))+
  geom_sf(data= Mundo, fill= NA, linewidth=0.3)+ 
  coord_sf(xlim = c(-125, -68), ylim = c(30, 49)) + 
  scale_color_manual(values = c("blue", "red")) + 
  scale_fill_paletteer_c("grDevices::terrain.colors", 
                         limits = c(0, 5000),
                         na.value = "transparent")+
  annotation_north_arrow(location = "bl",
                         which_north="true",
                         pad_x = unit(0.2, "in"),
                         pad_y = unit(0.7, "in"),
                         style=north_arrow_nautical(fill = c("white", "grey60")))+
  annotation_scale(location = "bl",
                   bar_cols = c("grey60", "white"), 
                   text_family = "ArcherPro Book")+
  labs(fill= "Altitud (msnm)", col= "Focas")

map_ca_occ <-ggplot()+
  geom_spatraster(data= alt, alpha= 0.5)+
  geom_sf(data= foca1_NorteA_sf, aes(col = species))+ #color de los puntos determinados por foca
  geom_sf(data= foca2_NorteA_sf, aes(col = species))+
  geom_sf(data= Mundo, fill= NA, linewidth=0.3)+ #añade capa mundo transparente (shapefile con datos geográficos) se especifica grosor de linea
  coord_sf(xlim = c(-135, -55), ylim = c(45, 63)) + #se hace zoom según coordenadas
  scale_color_manual(values = c("blue", "red")) + #añade color para contornos o puntos solidos
  scale_fill_paletteer_c("grDevices::terrain.colors", #rellena el resto del mapa
                         limits = c(0, 5000),
                         na.value = "transparent")+
  annotation_north_arrow(location = "bl",
                         which_north="true",
                         pad_x = unit(0.2, "in"),
                         pad_y = unit(0.7, "in"),
                         style=north_arrow_nautical(fill = c("white", "grey60")))+
  annotation_scale(location = "bl",
                   bar_cols = c("grey60", "white"), 
                   text_family = "ArcherPro Book")+
  labs(fill= "Altitud (msnm)", col= "Focas")

# Extracción de datos ambientales, simplificación de nombre de variables
env <- worldclim_global(var = "bio", res = 10, path = "datos_wc")
names(env)

v_names <- vector()
for(i in 1:19){
  
  v_names[i] <- paste0("bio_", sprintf("%02d", i)) 
}

v_names

names(env) <- v_names

env

#extraer datos en los puntos de ocurrencia de cada especie
foca1_NorteA_env <- extract(env, foca1_NorteA_sf)

foca1_NorteA_env$species <- c("Phoca vitulina")

foca2_NorteA_env <- extract(env, foca2_NorteA_sf)

foca2_NorteA_env$species <- c("Phoca vitulina richardii")

#CORRELACIÓN
#explorar correlación
df_env <- bind_rows(foca1_NorteA_env, foca2_NorteA_env)
df_env %>% names
str(df_env)

#Analisis de correlación de variables 


# Selecciona solo variables numéricas
df_num <- df_env %>% select(where(is.numeric))

# Calcula la matriz de correlación, eliminando filas con NA
mat_cor <- cor(df_num, use = "complete.obs")
mat_cor <- mat_cor
# Verifica que tiene valores
View(mat_cor)

#correlograma
correlograma <- ggcorrplot(mat_cor,
           hc.order = TRUE,     # ordena por agrupamiento jerárquico
           type = "lower",      # solo mitad inferior
           lab = TRUE,          # muestra coeficientes
           lab_size = 2,
           colors = c("blue", "white", "red"),
           outline.color = "gray60",
           title = "Matriz de correlación de variables bioclimáticas") 

#  Gráficos de diferencias ambientales -------------------------------------
#grafico temperatura media anual
p_tempm_spp <- ggplot(df_env, aes(x = bio_01, y = species, fill = species)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2, color = "white") +
  scale_fill_manual(values = c("#8d62fc", "#fc8d62"))+
  labs(
    x = "Temperatura media anual (BIO1)",
    y = "",
    fill = "Especie",
    title = "Distribución de la temperatura media anual por especie"
  )+
  scale_x_continuous(limits = c(5, 30), breaks = seq(0, 30, 5))+
  theme_ridges()+
  theme(legend.position = "none")

#gráfico estacionalidad de temperatura
p_tempe_spp <- ggplot(df_env, aes(x = bio_04, y = species, fill = species)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2, color = "white", rel_min_height = 0.01) +
  scale_fill_manual(values = c("#8d62fc", "#fc8d62")) +
  labs(
    x = "Estacionalidad de la temperatura (BIO4)",
    y = "",
    fill = "Especie",
    title = "Distribución de estacionalidad de temperatura por especie"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(legend.position = "top")

#gráfico precipitación anual

p_precipa_spp <- ggplot(df_env, aes(x = bio_12, y = species, fill = species)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2, color = "white") +
  scale_fill_manual(values = c("#8d62fc", "#fc8d62"))+
  labs(
    x = "Precipitación anual (BIO12)",
    y = "",
    fill = "Especie",
    title = "Distribución de la precipitación anual por especie"
  )+
  theme_ridges()+
  theme(legend.position = "none")

#gráfico precipitación anual
p_precipe_spp <- ggplot(df_env, aes(x = bio_15, y = species, fill = species)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2, color = "white") +
  scale_fill_manual(values = c("#8d62fc", "#fc8d62"))+
  labs(
    x = "Estacionalidad de precipitación (BIO15)",
    y = "",
    fill = "Especie",
    title = "Distribución de la precipitación anual por especie"
  )+
  theme_ridges()+
  theme(legend.position = "none")

#gráfico de contraste temperatura-precipitación anual
p_contraste_tpanual <- (p_tempm_spp + p_precipa_spp)+
  plot_layout(
    design = "
A
B
")

#gráfico de contraste estacionalidadtemperatura-precipitación 
p_contraste_tpestacional <- (p_tempe_spp + p_precipe_spp)+
  plot_layout(
    design = "
A
B
")

gclimatico_anual<- ggplot(df_env, aes(x = bio_12, y = bio_01, color = species)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_color_manual(values = c("#8d62fc", "#fc8d62"))+
  theme_light(base_size = 13) +
  labs(
    x = "Precipitación anual (mm)",
    y = "Temperatura media anual (°C)",
    color = "Especie",
    title = "Gradiente climático de las especies"
  )

gclimatico_estacional <- ggplot(df_env, aes(x = bio_15, y = bio_04, color = species)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_color_manual(values = c("#8d62fc", "#fc8d62"))+
  theme_light(base_size = 13) +
  labs(
    x = "Estacionalidad de Precipitación (mm)",
    y = "Estacionalidad de Temperatura (°C)",
    color = "Especie",
    title = "Gradiente climático de las especies"
  )

p_dif_anual_env <- ggplot(df_env, aes(x = bio_12, y = bio_01, color = species, fill = species)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(geom = "polygon", alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#8d62fc", "#fc8d62"))+
  scale_fill_manual(values = c("#8d62fc", "#fc8d62"))+
  theme_minimal(base_size = 13) +
  labs(
    x = "Precipitación anual (mm)",
    y = "Temperatura media anual (°C)",
    color = "Especie",
    fill = "Especie",
    title = "Diferencias en el espacio climático entre especies"
  )

p_dif_estacional_env <- ggplot(df_env, aes(x = bio_15, y = bio_04, color = species, fill = species)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(geom = "polygon", alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#8d62fc", "#fc8d62"))+
  scale_fill_manual(values = c("#8d62fc", "#fc8d62"))+
  theme_minimal(base_size = 13) +
  labs(
    x = "Estacionalidad de Precipitación (mm)",
    y = "Estacionalidad de Temperatura (°C)",
    color = "Especie",
    fill = "Especie",
    title = "Diferencias en el espacio climático entre especies"
  )
