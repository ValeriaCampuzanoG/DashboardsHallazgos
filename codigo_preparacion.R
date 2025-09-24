options(scipen = 999)

# Librerias 
library(pacman)
p_load(tidyverse, sf, readxl, ggrepel, DT, plotly, scales, leaflet, reactable, paletteer, fishualize,
       htmltools, bslib, ggrepel)
p_load(sysfonts, showtext)



# Cargar la fuente de Google
font_add_google("Montserrat", "Montserrat")
showtext_auto()


theme_1<-theme_minimal()+
  theme(text=element_text(family = gt::google_font("Montserrat")),
        plot.title = element_text(family = gt::google_font("Montserrat"),
                                  # face = "bold",
                                  size = 25,
                                  hjust = 0),
        plot.subtitle = element_text(family = gt::google_font("Montserrat"),
                                     size = 20,
                                     hjust = 0,
                                     colour = "grey40"),
        plot.caption = element_text(family = gt::google_font("Montserrat"),
                                    size = 18,
                                    colour = "grey40",
                                    hjust=c(1)),
        axis.text.x = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black"),
        axis.text.y = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = 10,
                                   colour = "black"),
        legend.title = element_text(family = gt::google_font("Montserrat"),
                                    face = "bold",
                                    size = 12,
                                    colour = "black",
                                    hjust = .0),
        legend.title.align = 0,
        legend.text = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black",
                                   hjust = 0),
        legend.text.align = 0,
        legend.position="bottom",
        legend.key.size = unit(19, "pt"))

# Paleta de colores

color_scale <- rev(as.character(
  paletteer::paletteer_c("grDevices::Burg", 30)
))


# Importar datos

bd_impunidad <- read_excel("www/bd/bd_indice_impunidad.xlsx")
catalogo_estatal <- read_excel("www/bd/catalogo_estatal.xlsx")
shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# Añadir cve 

bd_impunidad <- bd_impunidad %>%
  left_join(catalogo_estatal) %>%
  mutate(ano = as.character(ano))


lista_estados <- unique(bd_impunidad$cve_ent)
names(lista_estados) <- unique(bd_impunidad$entidad)

lista_indicadores_impunidad <- unique(bd_impunidad$indicador)
names(lista_indicadores_impunidad) <- unique(bd_impunidad$nom_indicador)


lista_anos_imp <- unique(bd_impunidad$ano)


# Mapa índice de Impunidad ----

#Leaflet de solo un año 


datos_sel_impunidad <- bd_impunidad %>%
  filter(nom_indicador == "Índice de impunidad",
         ano == "2021")



mapx <- left_join(shp, datos_sel_impunidad, by = c("CVE_EDO" = "cve_ent"))

mapx$ranking <- rank(mapx$valor, ties.method = "first")

label <- paste0(
  "<b style='font-size:25px;'>", mapx$ranking, "/32</b><br>",
  "<b style='font-size:20px;'><span style='color:#9e3963;'>", mapx$entidad, "," ,mapx$ano,"</span> </b><br>",
  "<span style='font-size:32px;'>",round(mapx$valor, 2), "%</span>")

paleta <- colorNumeric(palette = color_scale,
                       domain = mapx$valor, reverse = F)

#paleta <- colorNumeric(palette = color_scale,
#                       domain = c(0,100),
#                       reverse = F)



#pasos para un leaflet
mapa_base_indice <-  #0. base de datos de la cual trabajará
  leaflet(mapx) %>%  #1. llamar a leaflet
  addProviderTiles("CartoDB.DarkMatter") %>%  #2. elegir el mapa base, hay un catálogo y es lo que está entre comillas
  addPolygons(color = "white", #3. añadir polígono
              fillColor = paleta(mapx$valor), #paleta de colores y adentro la columna de la que saca el valor
              weight = .5, #tamaño linea
              label = lapply(label, HTML),
              fillOpacity = 0.8) %>%
  addLegend(
    opacity = .9,
    position = "topright",
    pal = paleta,
    values = mapx$valor,
    title = paste("Índice de impunidad<br>Añol:"),
    labels = FALSE,
    labFormat = function(type, cuts, p) {
      return(c("Menor", "", "", "Mayor"))
    })

mapa_base_indice


#shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

gen_mapa_impunidad <- function(ano_sel_imp){
  
  datos_sel_impunidad <- bd_impunidad %>%
    filter(nom_indicador == "Índice de impunidad", 
           ano == ano_sel_imp) 
  
  
  mapx <- left_join(shp, datos_sel_impunidad, by = c("CVE_EDO" = "cve_ent"))  
  
  mapx$ranking <- rank(mapx$valor, ties.method = "first")
  
  label <- paste0(
    "<b style='font-size:25px;'>", mapx$ranking, "/32</b><br>",
    "<b style='font-size:20px;'><span style='color:#9e3963;'>", mapx$entidad, "," ,mapx$ano,"</span> </b><br>",
    "<span style='font-size:32px;'>",round(mapx$valor, 2), "%</span>")
  
  paleta <- colorNumeric(palette = color_scale, 
                         domain = mapx$valor, reverse = F)
  
  #paleta <- colorNumeric(palette = color_scale, 
  #                       domain = c(0,100),
  #                       reverse = F)
  
  
  
  #pasos para un leaflet  
  mapa_base_indice <-  #0. base de datos de la cual trabajará
    leaflet(mapx) %>%  #1. llamar a leaflet
    addProviderTiles("CartoDB.DarkMatter") %>%  #2. elegir el mapa base, hay un catálogo y es lo que está entre comillas
    addPolygons(color = "white", #3. añadir polígono
                fillColor = paleta(mapx$valor), #paleta de colores y adentro la columna de la que saca el valor 
                weight = .5, #tamaño linea
                label = lapply(label, HTML),
                fillOpacity = 0.8) %>% 
    addLegend(
      opacity = .9, 
      position = "topright",
      pal = paleta,
      values = mapx$valor,
      title = paste("Índice de impunidad<br>Año:", ano_sel_imp),  
      labels = FALSE,
      labFormat = function(type, cuts, p) { 
        return(c("Menor", "", "", "Mayor"))
      })
  
  
  mapa_base_indice 
  
}


gen_mapa_impunidad("2020")

# Gráfica de barras índice de Impunidad -----

opciones_impunidad <- c("Índice de impunidad", "Desestimaciones por no ser un delito", 
                        "Casos totales por resolver", "Soluciones efectivas")

#ind_sel== "índice de impunidad"

gen_barras_imp <- function(ind_sel, ano_sel_imp){
  
  datos_sel <- bd_impunidad %>%
    filter(nom_indicador %in% opciones_impunidad) %>%
    filter(nom_indicador == ind_sel, 
           ano== ano_sel_imp) %>% 
    left_join(catalogo_estatal) 
  
  
  g <- datos_sel %>% 
    ggplot(aes(x = reorder(entidad, -valor), 
               y = valor, 
               fill = valor, 
               text = paste0( "<span style='font-size:24px;'><b>", entidad, ", " ,ano,  "</b></span><br><br>",
                              "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ","))
    )) + 
    geom_col() + 
    coord_flip() +
    scale_fill_gradientn(colors = color_scale) + 
    theme_bw() + 
    labs(#title = datos_sel$nom_indicador, 
      x = NULL, y = NULL) + 
    scale_y_continuous(expand = expansion(c(0, 0.1)), 
                       labels = comma_format()) + 
    theme(
      panel.grid = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(), 
      legend.position = "none"
    ) +
    geom_text(aes(label = valor), hjust = -0.5, colour = "#535353") +
    guides(fill = "none")
  
  ggplotly(g, tooltip = "text") 
  
}


gen_barras_imp("Desestimaciones por no ser un delito", "2021")


# Gráfica de línea de Impunidad -------------


gen_lineas_imp <- function(ind_sel){
  
  datos_sel <- bd_impunidad %>%
    filter(nom_indicador %in% opciones_impunidad) %>%
    filter(nom_indicador == ind_sel) %>% 
    left_join(catalogo_estatal) 
  
  
  g <- datos_sel %>% 
    ggplot(aes(x = ano, 
               y = valor, 
               group = entidad, 
               color = entidad,
               text = paste0(
                 "<span style='font-size:24px;'><b>", entidad, ", " ,ano,  "</b></span><br><br>",
                 "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ","))
    )) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    theme_bw() +
    labs(
      x = "Año",
      y = "Porcentaje de impunidad",
      color = "Entidad"
    ) +
    scale_y_continuous(labels = comma_format()) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom")
  
  ggplotly(g, tooltip = "text")
  
  
}


gen_lineas_imp("Índice de impunidad")



# Tabla índice de Impunidad --------

#rev(as.character(colorRampPalette(paletteer::paletteer_c("grDevices::Burg", 30))))

#tabla para 2023

tab_imp2023 <- bd_impunidad %>%
  filter(ano == "2023") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos totales por resolver",  "Desestimaciones por no ser un delito", 
         "Soluciones efectivas", "Índice de impunidad") %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              "Índice de impunidad" = colDef(format = colFormat(suffix = "%", 
                                                                digits = 1), 
                                             style = function(valor) {
                                               scaled <- (valor - 85) / (150 - 85)
                                               scaled <- max(min(scaled, 1), 0)         
                                               color <- color_scale[floor(scaled * 99) + 1]
                                               list(
                                                 background = color,
                                                 color = ifelse(scaled > 0.13, "white", "black")
                                               )
                                             })),
            pagination = F, 
            compact = T,
            bordered = T,
            outlined = T
  )

tab_imp2019



# Función generadores de tablas para cada año 

fun_gen_tablas_impunidad <- function(bd_impunidad, color_scale) {
  years <- c("2019", "2020", "2021", "2022", "2023")
  
  for (year in years) {
    # Create the table for this year
    table_name <- paste0("tab_imp", year)
    
    # Generate the table
    table_data <- bd_impunidad %>%
      filter(ano == year) %>%
      mutate(across(c(valor), as.numeric)) %>%
      select(entidad, nom_indicador, valor) %>%
      pivot_wider(names_from = "nom_indicador", 
                  values_from = "valor") %>%
      select(Ranking, entidad, "Casos totales por resolver",  "Desestimaciones por no ser un delito", 
             "Soluciones efectivas", "Índice de impunidad") %>%
      reactable(striped = T, 
                defaultColDef = colDef( 
                  align = "center",
                  format = colFormat(separators = TRUE)),
                columns = list( 
                  Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
                  entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
                  "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
                  "Índice de impunidad" = colDef(format = colFormat(suffix = "%", 
                                                                    digits = 1), 
                                                 style = function(valor) {
                                                   scaled <- (valor - 85) / (150 - 85)
                                                   scaled <- max(min(scaled, 1), 0)         
                                                   color <- color_scale[floor(scaled * 99) + 1]
                                                   list(
                                                     background = color,
                                                     color = ifelse(scaled > 0.13, "white", "black")
                                                   )
                                                 })),
                pagination = F, 
                compact = T,
                bordered = T,
                outlined = T
      )
    
    # Assign to global environment with the desired name
    # assign(table_name, table_data, envir = .GlobalEnv)
    # 
    # cat(paste("Created table:", table_name, "\n"))
  }
}

# Usage:
fun_gen_tablas_impunidad(bd_impunidad, color_scale)

#bs4DashGallery()

