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
library(cowplot)

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

# 1.1 ARCHIVOS SF
foca1_NorteA_sf <- foca1_NorteA %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

foca2_NorteA_sf <- foca2_NorteA %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

# 1.2 CAPA RASTER Y ARCHIVOS VECTORIALES CLIMA
alt <- worldclim_global(var="elev", res=5, path=tempdir())
data(countriesHigh) 
Mundo <- st_as_sf(countriesHigh) 

# 2. MAPA NORTE AMÉRICA

mapa_NA_occ <- ggplot() +
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
    legend.position.inside = c(0.86, 0.25),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

mapa_NA_occ

ggsave("mapa1_NorteAmerica.png", 
       mapa_NA_occ, width = 10, height = 8, units = "in",
       bg="white",
       dpi = 300)


# 2.1 MAPAS A ESCALA DE LOS PAISES
#----------------------------------
crear_mapa_pais <- function(xmin, xmax, ymin, ymax, titulo, bandera_path) {
  
  mapa <- ggplot() +
    # Capa de relieve
    geom_spatraster(data = alt, alpha = 0.7) +
    
    # Fronteras del mundo
    geom_sf(data = Mundo, fill = NA, color = "gray40", linewidth = 0.3) +
    
    # Ocurrencias
    geom_sf(data = foca1_NorteA_sf, aes(color = species), size = 2, alpha = 0.9) +
    geom_sf(data = foca2_NorteA_sf, aes(color = species), size = 2, alpha = 0.9) +
    
    # Coordenadas del país
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    
    # Escalas y colores
    scale_fill_paletteer_c("viridis::viridis", direction = -1, limits = c(0, 5000),
                           na.value = "transparent", name = "Altitud (m s.n.m.)") +
    scale_color_manual(values = c("#002F70","#742324"),
      name = "Especies") +
    
    # Escala y rosa de los vientos
    annotation_scale(location = "bl", bar_cols = c("gray60", "white"), text_family = "lato") +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.4, "in"), pad_y = unit(0.9, "in"),
                           style = north_arrow_nautical(fill = c("white", "gray60"))) +
    
    # Etiquetas
    labs(
      title = titulo,
      subtitle = "Registros de ocurrencia (GBIF, 2007–2024)",
      caption = "Elaborado por Equipo 4  |  Datos: GBIF & WorldClim v2.1"
    ) +
    
    theme_minimal() 
    
  
  # Agregar bandera
  bandera <- image_read(bandera_path)
  mapa_final <- ggdraw() +
    draw_plot(mapa) +
    draw_image(bandera, x = 0.78, y = 0.03, width = 0.16, height = 0.16)
  
  return(mapa_final)
}
# MX
map_mx <- crear_mapa_pais(
  xmin = -120, xmax = -105, ymin = 22, ymax = 33,
  titulo = "Distribución de P. vitulina y P.v. richardii en México",
  bandera_path = "Bandera_mx.png"
)
ggsave("mapa_mx.png", 
       map_mx, width = 10, height = 8, units = "in",
       bg="white",
       dpi = 300)

# US
map_us <- crear_mapa_pais(
  xmin = -125, xmax = -65, ymin = 25, ymax = 50,
  titulo = "Distribución de P. vitulina y P.v. richardii en Estados Unidos",
  bandera_path = "Bandera_usa.png"
)
ggsave("mapa_us.png", 
       map_us, width = 10, height = 8, units = "in",
       bg="white",
       dpi = 300)
# CA
map_ca <- crear_mapa_pais(
  xmin = -140, xmax = -50, ymin = 45, ymax = 70,
  titulo = "Distribución de P. vitulina y P.v. richardii en Canadá",
  bandera_path = "Bandera_ca.png"
)
ggsave("mapa_ca.png", 
       map_ca, width = 10, height = 8, units = "in",
       bg="white",
       dpi = 300)