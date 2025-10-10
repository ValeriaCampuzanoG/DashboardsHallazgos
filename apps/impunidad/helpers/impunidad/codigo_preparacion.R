options(scipen = 999)
options(repos = c(CRAN = "https://cran.r-project.org/"))


# IMPUNIDAD #

# Librerias 
library(tidyverse)
library(sf)
library(readxl)
library(ggrepel)
library(DT)
library(plotly)
library(scales)
library(leaflet)
library(reactable)
library(paletteer)
library(fishualize)
library(htmltools)
library(bslib)
library(fresh)
library(sysfonts)
library(showtext)
library(ggiraph)


# Cambiar colores 
theme_hgz <- create_theme(
  bs4dash_color(
    fuchsia = "#8B3058FF",
    purple = "#541F3FFF",
    maroon = "#D39C83FF",
    red = "#AD466CFF",
    green = "#C1766FFF"
  ),
  bs4dash_status(
    primary = "#8B3058FF",
    secondary = "#A65461FF",
    success = "#FBE6C5FF",
    info = "#7BBCB0FF",
    warning = "#D39C83FF"
  )
)


# Cargar la fuente de Google
# font_add_google("Montserrat", "Montserrat")
# showtext_auto()
# 
# 
# theme_1<-theme_minimal()+
#   theme(text=element_text(family = gt::google_font("Montserrat")),
#         plot.title = element_text(family = gt::google_font("Montserrat"),
#                                   # face = "bold",
#                                   size = 25,
#                                   hjust = 0),
#         plot.subtitle = element_text(family = gt::google_font("Montserrat"),
#                                      size = 20,
#                                      hjust = 0,
#                                      colour = "grey40"),
#         plot.caption = element_text(family = gt::google_font("Montserrat"),
#                                     size = 18,
#                                     colour = "grey40",
#                                     hjust=c(1)),
#         axis.text.x = element_text(family = gt::google_font("Montserrat"),
#                                    # face = "bold",
#                                    size = 12,
#                                    colour = "black"),
#         axis.text.y = element_text(family = gt::google_font("Montserrat"),
#                                    # face = "bold",
#                                    size = 10,
#                                    colour = "black"),
#         legend.title = element_text(family = gt::google_font("Montserrat"),
#                                     face = "bold",
#                                     size = 12,
#                                     colour = "black",
#                                     hjust = .0),
#         legend.title.align = 0,
#         legend.text = element_text(family = gt::google_font("Montserrat"),
#                                    # face = "bold",
#                                    size = 12,
#                                    colour = "black",
#                                    hjust = 0),
#         legend.text.align = 0,
#         legend.position="bottom",
#         legend.key.size = unit(19, "pt"))

# Paleta de colores

color_scale <- rev(as.character(
  paletteer::paletteer_c("grDevices::BrwnYl", 30)
))



# Transformacion de datos 
bd_impunidad_fuentes_pub <- read_excel("apps/impunidad/www/bd/impunidad_fuentes_pub.xlsx", 
                                       sheet = "idei_historico") 

bd_impunidad <- bd_impunidad_fuentes_pub %>%
  pivot_longer(
    cols = 3:9, 
    names_to = "variable", 
    values_to = "valor"
  )


# Importar datos

#bd_impunidad <- read_excel("www/bd/bd_indice_impunidad.xlsx")
catalogo_estatal <- read_excel("www/bd/catalogo_estatal.xlsx")
#shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
shp <- read_sf("www/DivisionEstatal.geojson")

# Añadir cve 

bd_impunidad <- bd_impunidad %>%
  left_join(catalogo_estatal) %>%
  mutate(ano = as.character(ano)) %>%
  select(cve_ent, entidad, ano, variable, valor) %>%
  mutate(
    nom_indicador = case_when(
      variable == "sentencias" ~ "Sentencias",
      variable == "pct_impunidad" ~ "Porcentaje de impunidad",
      variable == "casos_resolver" ~ "Casos por resolver",
      variable == "desestimaciones" ~ "Desestimaciones",
      variable == "pct_efectividad" ~ "Porcentaje de efectividad",
      variable == "ranking" ~ "Ranking", 
      variable == "salidas_alternas" ~ "Salidas y soluciones alternas",
      TRUE ~ NA_character_ 
    )
  )



lista_estados <- unique(bd_impunidad$cve_ent)
names(lista_estados) <- unique(bd_impunidad$entidad)

lista_indicadores_impunidad <- unique(bd_impunidad$nom_indicador)
names(lista_indicadores_impunidad) <- unique(bd_impunidad$nom_indicador)


lista_anos_imp <- unique(bd_impunidad$ano)

# Entidades con más y menos impunidad -------

# fun_gen_caja_mayor <- function(ano_sel_imp){
#   
#   top_impunidad <- bd_impunidad %>%
#     filter(nom_indicador == "Índice de impunidad", 
#            ano == ano_sel_imp)  %>%
#     mutate(valor = as.numeric(valor)) %>%
#     slice_max(order_by = valor, n = 1, with_ties = FALSE) 
#   
#   
#   vb_value <- top_impunidad$entidad
#   vb_subtitle <- paste0(top_impunidad$valor, "%")
# }


  

# bottom_impunidad <- bd_impunidad %>%
#   filter(nom_indicador == "Índice de impunidad", 
#          ano == ano_sel_imp)  %>%
#   mutate(valor = as.numeric(valor)) %>%
#   slice_max(order_by = valor, n = 1, with_ties = FALSE)  # use slice_min for “menor impunidad”


gen_mapa_impunidad <- function(ano_sel_imp){
  
  datos_sel_impunidad <- bd_impunidad %>%
    filter(nom_indicador == "Porcentaje de impunidad", 
           ano == ano_sel_imp) %>%
    mutate( valor = round(valor*100, 2 ))
  
  
  mapx <- left_join(shp, datos_sel_impunidad, by = c("CVE_EDO" = "cve_ent"))  
  
  #mapx$ranking <- rank(mapx$valor, ties.method = "first")
  
  mapx$ranking <- "Sin datos suficientes para calcular su puntuación"
  mapx$ranking[!is.na(mapx$valor)] <- rank(mapx$valor[!is.na(mapx$valor)], ties.method = "first")
  
  label <- paste0(
    "<b style='font-size:25px;'>", mapx$ranking, "/32</b><br>",
    "<b style='font-size:20px;'><span style='color:#9e3963;'>", mapx$entidad, ", " ,mapx$ano,"</span> </b><br>",
    "<span style='font-size:32px;'>",round(mapx$valor, 2), "%</span>")
  
  paleta <- colorNumeric(palette = color_scale, 
                         domain = mapx$valor, reverse = F)
  
  #paleta <- colorNumeric(palette = color_scale, 
  #                       domain = c(0,100),
  #                       reverse = F)
  
  
  
  #pasos para un leaflet  
  mapa_base_indice <-  #0. base de datos de la cual trabajará
    leaflet(mapx, 
            options = leafletOptions(
              minZoom = 5.4,
              #maxZoom = 5.4,
              zoomControl = FALSE
            )) %>%  #1. llamar a leaflet
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

gen_mapa_impunidad("2023")




# Gráfica de barras índice de Impunidad -----

opciones_impunidad <- c("Porcentaje de impunidad", 
                        "Casos por resolver", 
                        "Sentencias",
                        "Salidas y soluciones alternas", 
                        "Desestimaciones")

#ind_sel== "índice de impunidad"

gen_barras_imp <- function(ind_sel, ano_sel_imp){
  
  datos_sel <- bd_impunidad %>%
    filter(nom_indicador %in% opciones_impunidad) %>%
    filter(nom_indicador == ind_sel, 
           ano == ano_sel_imp) %>% 
    filter(!is.na(valor)) %>%
    left_join(catalogo_estatal) 
  
  
  g <- ggplot(datos_sel, 
              aes(x = reorder(entidad, -valor), 
                  y = valor, 
                  fill = valor,
                  tooltip = paste0("<span style='font-size:14px;'><b>", entidad, ", ", ano, "</b></span><br>",
                                   "<span style='font-size:12px;'>", prettyNum(round(valor, 2), big.mark = ","), "</span>"),
                  data_id = entidad)) +
    ggiraph::geom_col_interactive() +
    ggiraph::geom_text_interactive(aes(label = round(valor,2)), hjust = -0.5, colour = "#535353") +
    coord_flip() +
    scale_fill_gradientn(colors = color_scale) +
    #scale_fill_gradient(low = "#D39C83", high = "#813753") +
    scale_y_continuous(expand = expansion(c(0, 0.1)), labels = comma_format()) +
    labs(x = "Entidad", 
         y = "",
         title = paste0(datos_sel$nom_indicador), 
         subtitle = paste0(datos_sel$ano)
         ) +
    theme_bw() +
    theme(
      axis.title = element_text(family = "Montserrat", size = 9),
      plot.subtitle = element_text(family = "Montserrat", 
                                   size = 25, 
                                   colour = "#636363",
                                   margin = margin(b = 30, unit = "pt")),
      plot.title = element_text(family = "Montserrat",
                                face = "bold",
                                size = 35,
                                hjust = 0,
                                margin = margin(b = 15, unit = "pt")),
      #plot.title.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.position = "none" #,
      #axis.ticks.length = unit(0.2, "cm")
      #plot.margin = margin(60, 20, 20, 20, "pt")
    ) 
  
 
    ggiraph::girafe(ggobj = g, 
                                        width_svg = 12, 
                                        height_svg =16,
                                        pointsize = 12,
                                        options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                                       opts_hover(css = ''),
                                                       opts_hover_inv(css = "opacity:0.1;"),
                                                       opts_selection(type = "none"),
                                                       opts_toolbar(saveaspng = FALSE)))
  
}

gen_barras_imp("Porcentaje de impunidad", "2023")


# Gráfica de líneas con selector de entidades

gen_lineas_imp <- function(ind_sel, entidades_resaltadas){
  
  datos_sel <- bd_impunidad %>%
    #filter(nom_indicador %in% opciones_impunidad) %>%
    filter(nom_indicador == ind_sel) %>% 
    left_join(catalogo_estatal) 
  
  
  datos_sel <- datos_sel %>%
    mutate(
      is_selected = entidad %in% entidades_resaltadas,
      line_color  = ifelse(is_selected, "#C1766F", "grey80"),
      point_color = ifelse(is_selected, "#541F3F", "grey75"),
      line_alpha  = ifelse(is_selected, 1, 0.1)
    )
  
  
  datos_labels <- datos_sel %>%
    filter(is_selected) %>%
    group_by(entidad) %>%
    slice_max(ano, n = 1, with_ties = FALSE) %>%
    ungroup()

  g <- datos_sel %>% 
    ggplot(aes(x = ano, 
               y = valor, 
               group = entidad)) +
    geom_line_interactive(aes(color = I(line_color), 
                              alpha = line_alpha,
                              tooltip = if(ind_sel == "Índice de impunidad"){
                                paste0(
                                  "<span style='font-size:24px;'><b>", entidad, ", " ,ano,  "</b></span><br><br>",
                                  "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ","), " %") 
                              } else {
                                paste0(
                                  "<span style='font-size:24px;'><b>", entidad, ", " ,ano,  "</b></span><br><br>",
                                  "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ",")) 
                              },
                              data_id = entidad),
                          size = 0.5, show.legend = FALSE) +
    geom_point_interactive(aes(color = I(point_color),
                               alpha = line_alpha,
                               tooltip = if(ind_sel == "Índice de impunidad"){
                                 paste0(
                                   "<span style='font-size:24px;'><b>", entidad, " " ,ano,  "</b></span><br><br>",
                                   "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ","), " %") 
                               } else {
                                 paste0(
                                   "<span style='font-size:24px;'><b>", entidad, " " ,ano,  "</b></span><br><br>",
                                   "<span style='font-size:18px;'>", prettyNum(round(valor, 2), big.mark = ",")) 
                               },
                               data_id = entidad),
                           size = 2, show.legend = FALSE) +
    scale_alpha_identity() +
    geom_text_interactive(data = datos_labels, 
                          aes(x = ano, y = valor, label = entidad,
                              tooltip = entidad, 
                              data_id = entidad),
                          hjust = -0.1, vjust = 0.5, size = 3, color = "#541F3F") +
    theme_bw() +
    labs(
      x = "Año",
      y = ""
    ) +
    scale_y_continuous(
      labels = if(ind_sel == "Índice de impunidad") {
        function(x) paste0(x, "%")
      } else {
        comma_format()
      }
    ) + 
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none")  # Remove legend completely
  
  
  ggiraph::girafe(ggobj = g, 
                  width_svg = 12, 
                  height_svg =16,
                  pointsize = 12,
                  options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                 opts_hover(css = ''),
                                 opts_hover_inv(css = "opacity:0.1;"),
                                 opts_selection(type = "none"),
                                 opts_toolbar(saveaspng = FALSE)))
  
  
  
}

gen_lineas_imp("Desestimaciones", "Oaxaca")



# Tabla índice de Impunidad --------

#rev(as.character(colorRampPalette(paletteer::paletteer_c("grDevices::Burg", 30))))

#tabla para 2023

tab_imp2023 <- bd_impunidad %>%
  filter(ano == "2023") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos por resolver",  "Sentencias", 
         "Salidas y soluciones alternas", 
         "Desestimaciones", "Porcentaje de impunidad") %>%
  mutate( `Porcentaje de impunidad` = as.numeric(`Porcentaje de impunidad`)) %>%
  filter( !is.na(`Porcentaje de impunidad`)) %>%
  mutate( `Porcentaje de impunidad` = round(`Porcentaje de impunidad`*100, 2)) %>%
  arrange(Ranking) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              `Porcentaje de impunidad` = colDef(format = colFormat(suffix = "%", 
                                                                digits = 2), 
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

tab_imp2023


#tabla para 2022

tab_imp2022 <- bd_impunidad %>%
  filter(ano == "2022") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos por resolver",  "Sentencias", 
         "Salidas y soluciones alternas", 
         "Desestimaciones", "Porcentaje de impunidad") %>%
  mutate( `Porcentaje de impunidad` = as.numeric(`Porcentaje de impunidad`)) %>%
  filter( !is.na(`Porcentaje de impunidad`)) %>%
  mutate( `Porcentaje de impunidad` = round(`Porcentaje de impunidad`*100, 2)) %>%
  arrange(Ranking) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              `Porcentaje de impunidad` = colDef(format = colFormat(suffix = "%", 
                                                                    digits = 2), 
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

tab_imp2022

# Tabla del año 2021

tab_imp2021 <- bd_impunidad %>%
  filter(ano == "2021") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos por resolver",  "Sentencias", 
         "Salidas y soluciones alternas", 
         "Desestimaciones", "Porcentaje de impunidad") %>%
  mutate( `Porcentaje de impunidad` = as.numeric(`Porcentaje de impunidad`)) %>%
  filter( !is.na(`Porcentaje de impunidad`)) %>%
  mutate( `Porcentaje de impunidad` = round(`Porcentaje de impunidad`*100, 2)) %>%
  arrange(Ranking) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              `Porcentaje de impunidad` = colDef(format = colFormat(suffix = "%", 
                                                                    digits = 2), 
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

tab_imp2021



# Tabla del año 2020


tab_imp2020 <- bd_impunidad %>%
  filter(ano == "2020") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos por resolver",  "Sentencias", 
         "Salidas y soluciones alternas", 
         "Desestimaciones", "Porcentaje de impunidad") %>%
  mutate( `Porcentaje de impunidad` = as.numeric(`Porcentaje de impunidad`)) %>%
  filter( !is.na(`Porcentaje de impunidad`)) %>%
  mutate( `Porcentaje de impunidad` = round(`Porcentaje de impunidad`*100, 2)) %>%
  arrange(Ranking) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              `Porcentaje de impunidad` = colDef(format = colFormat(suffix = "%", 
                                                                    digits = 2), 
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

tab_imp2020



# Tabla año 2019

tab_imp2019 <- bd_impunidad %>%
  filter(ano == "2019") %>%
  mutate(across(c(valor), as.numeric)) %>%
  select(entidad, nom_indicador, valor) %>%
  pivot_wider(names_from = "nom_indicador", 
              values_from = "valor") %>%
  select(Ranking, entidad, "Casos por resolver",  "Sentencias", 
         "Salidas y soluciones alternas", 
         "Desestimaciones", "Porcentaje de impunidad") %>%
  mutate( `Porcentaje de impunidad` = as.numeric(`Porcentaje de impunidad`)) %>%
  filter( !is.na(`Porcentaje de impunidad`)) %>%
  mutate( `Porcentaje de impunidad` = round(`Porcentaje de impunidad`*100, 2)) %>%
  arrange(Ranking) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
              `Porcentaje de impunidad` = colDef(format = colFormat(suffix = "%", 
                                                                    digits = 2), 
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

# fun_gen_tablas_impunidad <- function(bd_impunidad, color_scale) {
#   
#   years <- c("2019", "2020", "2021", "2022", "2023")
#   
#   for (year in years) {
#     # Create the table for this year
#     table_name <- paste0("tab_imp", year)
#     
#     # Generate the table
#     table_data <- bd_impunidad %>%
#       filter(ano == year) %>%
#       mutate(across(c(valor), as.numeric)) %>%
#       select(entidad, nom_indicador, valor) %>%
#       pivot_wider(names_from = "nom_indicador", 
#                   values_from = "valor") %>%
#       arrange(Ranking) %>%
#       select(Ranking, entidad, "Casos totales por resolver",  "Desestimaciones por no ser un delito", 
#              "Soluciones efectivas", "Índice de impunidad") %>%
#       reactable(striped = T, 
#                 defaultColDef = colDef( 
#                   align = "center",
#                   format = colFormat(separators = TRUE)),
#                 columns = list( 
#                   Ranking = colDef(filterable = TRUE, format = colFormat(suffix = "°")),
#                   entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
#                   "Desestimaciones por no ser un delito" = colDef(format = colFormat(separators = TRUE , digits = 0)),
#                   "Índice de impunidad" = colDef(format = colFormat(suffix = "%", 
#                                                                     digits = 1), 
#                                                  style = function(valor) {
#                                                    scaled <- (valor - 85) / (150 - 85)
#                                                    scaled <- max(min(scaled, 1), 0)         
#                                                    color <- color_scale[floor(scaled * 99) + 1]
#                                                    list(
#                                                      background = color,
#                                                      color = ifelse(scaled > 0.13, "white", "black")
#                                                    )
#                                                  })),
#                 pagination = F, 
#                 compact = T,
#                 bordered = T,
#                 outlined = T
#       )
#     
#     # Assign to global environment with the desired name
#     # assign(table_name, table_data, envir = .GlobalEnv)
#     # 
#     # cat(paste("Created table:", table_name, "\n"))
#   }
# }
# 
# # Usage:
# fun_gen_tablas_impunidad(bd_impunidad, color_scale)
# 
# 



# Header 
create_mcv_header <- function() {
  tagList(
    # Barra superior con redes sociales
    div(
      class = "mcv-top-bar",
      style = "
        background-color: #6551D0;
        color: white;
        padding: 10px 0;
        font-size: 12px;
        position: relative;
        font-family: 'Ubuntu', sans-serif;
      ",
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          
          # Redes sociales izquierda
          # div(
          #   style = "display: flex; align-items: center; gap: 15px;",
          #   tags$a(
          #     href = "https://twitter.com/mexicocomovamos", 
          #     style = "color: white; font-size: 18px; text-decoration: none;",
          #     HTML('<i class="fab fa-twitter"></i>')
          #   ),
          #   tags$a(
          #     href = "https://facebook.com/mexicocomovamos", 
          #     style = "color: white; font-size: 18px; text-decoration: none;",
          #     HTML('<i class="fab fa-facebook"></i>')
          #   ),
          #   tags$a(
          #     href = "https://youtube.com/mexicocomovamos", 
          #     style = "color: white; font-size: 18px; text-decoration: none;",
          #     HTML('<i class="fab fa-youtube"></i>')
          #   ),
          #   tags$a(
          #     href = "https://instagram.com/mexicocomovamos", 
          #     style = "color: white; font-size: 18px; text-decoration: none;",
          #     HTML('<i class="fab fa-instagram"></i>')
          #   )
          # ),
          
          # Mensaje central
          div(
            style = "color: white; font-weight: 500; font-family: 'Ubuntu', sans-serif;",
            "México, ¿cómo vamos? 🇲🇽"
          ),
          
          # Espacio derecha
          div(
            style = "width: 150px;"
          )
        )
      )
    ),
    
    # Barra principal con navegación
    div(
      class = "mcv-main-header",
      style = "
        background-color: white;
        color: #333333;
        padding: 20px 0;
        font-family: 'Ubuntu', sans-serif;
        border-bottom: 1px solid #e9ecef;
      ",
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          
          # Navegación izquierda
          div(
            style = "display: flex; align-items: center; gap: 30px; flex: 1;",
            
            tags$a(
              href = "https://mexicocomovamos.mx/equipo/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "MÉXICO,",
              br(),
              "¿CÓMO VAMOS?"
            ),
            
            tags$a(
              href = "https://mexicocomovamos.mx/indice-de-progreso-social/",
              style = "
                color: #6551D0;
                text-decoration: none;
                font-size: 12px;
                font-weight: 700;
                padding: 8px 0;
                border-bottom: 2px solid #6551D0;
                font-family: 'Ubuntu', sans-serif;
              ",
              "ÍNDICE DE",
              br(),
              "PROGRESO SOCIAL"
            )
          ),
          
          # Logo central
          div(
            style = "flex: 0 0 auto; margin: 0 20px;",
            tags$a(
              href = "https://mexicocomovamos.mx/",
              tags$img(
                src = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg",
                alt = "México Como Vamos",
                style = "height: 60px; width: auto;"
              )
            )
          ),
          
          # Navegación derecha
          div(
            style = "display: flex; align-items: center; gap: 30px; flex: 1; justify-content: flex-end;",
            
            tags$a(
              href = "https://mexicocomovamos.mx/fichas-por-estado/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "FICHAS",
              br(),
              "POR ESTADO"
            ),
            
            tags$a(
              href = "https://mexicocomovamos.mx/categoria/publicaciones/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "PUBLICACIONES",
              br(),
              "Y MEDIOS"
            )
          )
        )
      )
    )
  )
}
