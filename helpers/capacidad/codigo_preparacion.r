options(scipen = 999)
#options(repos = c(CRAN = "https://cran.r-project.org/"))


# CAPACIDAD #

#install.packages("janitor")

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
library(ggplot2)
library(ggiraph)
library(paletteer)
paletteer::palettes_c_names


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


theme_1<-theme_bw()+
  theme(text=element_text(family = "Montserrat"),
        plot.title = element_text( #family = gt::google_font("Montserrat"),
                                  # face = "bold",
                                  size = 25,
                                  hjust = 0),
        plot.subtitle = element_text( #family = gt::google_font("Montserrat"),
                                     size = 20,
                                     hjust = 0,
                                     colour = "grey40"),
        plot.caption = element_text( #family = gt::google_font("Montserrat"),
                                    size = 18,
                                    colour = "grey40",
                                    hjust=c(1)),
        # axis.text.x = element_text( #family = gt::google_font("Montserrat"),
        #                            # face = "bold",
        #                            size = 12,
        #                            colour = "black"),
        # axis.text.y = element_text( #family = gt::google_font("Montserrat"),
        #                            # face = "bold",
        #                            size = 10,
        #                            colour = "black"),
        legend.title = element_text( #family = gt::google_font("Montserrat"),
                                    face = "bold",
                                    size = 12,
                                    colour = "black",
                                    hjust = .0),
        legend.title.align = 0,
        legend.text = element_text( #family = gt::google_font("Montserrat"),
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

# Transformación de datos 

bd_capacidad_wide <- read_excel("www/bd/bd_capacidad_wide.xlsx", 
                                sheet = "Base de datos agregada ") #base de datos ancha, descargada del excel 

bd_capacidad <- bd_capacidad_wide %>% janitor::row_to_names(row_number = 3) %>%
  select(-cve) %>%
  pivot_longer( 
    cols = ranking:fi_tasa_agencias, 
    names_to = "variable", 
    values_to = "total"
    )


df <- bd_capacidad_wide %>%
  slice(1:3) %>%
  select(-2) 

df <- t(df)
metadatos_cap <- as.data.frame(df)

metadatos_cap <- metadatos_cap %>%
  slice(-1) %>%
  rename(
     "nom_indicador" = V1, 
     "categoria" = V2, 
     "variable" = V3
  )


bd_capacidad <- bd_capacidad %>%
  left_join(metadatos_cap, by = "variable") %>%
  select(entidad, nom_indicador, categoria, total) %>%
  filter(!(entidad %in% c("valor máximo")))

# Importar datos

#bd_capacidad_old <- read_excel("www/bd/bd_indice_capacidad.xlsx")
catalogo_estatal <- read_excel("www/bd/catalogo_estatal.xlsx")
shp <- read_sf("www/DivisionEstatal.geojson")
valores_max_capacidad <- read_excel("www/bd/metadatos_valores_icapacidad.xlsx")


bd_capacidad <- bd_capacidad %>%
  left_join(catalogo_estatal, by="entidad") %>%
  mutate(cve_ent = ifelse(is.na(cve_ent), 15, cve_ent), 
         total = as.numeric(total))
  


# 
opciones_capacidad <- c("Fiscalía", "Poder Judicial", "Defensoría Pública", "Órgano de coordinación")


## Mapa leaflet ----

datos_sel_capacidad <- bd_capacidad %>%
  filter(nom_indicador == "Índice de Capacidad") 

mapcap <- left_join(shp, datos_sel_capacidad, by = c("CVE_EDO" = "cve_ent"))  

mapcap$ranking <- rank( (-mapcap$total), ties.method = "first")

label_cap <- paste0(
  "<b style='font-size:20px;'>", mapcap$ranking, "/32</b><br>",
  "<b style='font-size:20px;'><span style='color:#9e3963;'>", mapcap$entidad, "</span> </b><br>",
  "<span style='font-size:32px;'>",round(mapcap$total*100, 2), "%</span>")

paleta_cap <- colorNumeric(palette = color_scale, 
                           domain = mapcap$total, reverse = F)


#pasos para un leaflet  
mapa_base_capacidad <-  
  leaflet(mapcap,
          options = leafletOptions(
    minZoom = 5.4,
    #maxZoom = 5.4,
    zoomControl = FALSE
  )) %>%  
  addProviderTiles("CartoDB.DarkMatter") %>%  
  addPolygons(color = "white", 
              fillColor = paleta_cap(mapcap$total), 
              weight = .5, 
              label = lapply(label_cap, HTML),
              fillOpacity = 0.8) %>% 
  addLegend(
    opacity = .9, 
    position = "topright",
    pal = paleta_cap,
    values = mapcap$total,
    title = "Índice de Capacidad 2023",  
    labels = FALSE,
    labFormat = function(type, cuts, p) { 
      return(c("Menor", "", "", "Mayor"))
    })

mapa_base_capacidad


## Gráfica de barras de totales -----


g <- 
  bd_capacidad %>%
     filter(nom_indicador=="Índice de Capacidad") %>%
     mutate(total = as.numeric(total),
            cve_ent=as.character(cve_ent)) %>%
  ggplot(aes(x = reorder(entidad, total),
             y = total*100, 
             fill = total*100, 
             tooltip = paste0("<b>", entidad, "</b><br>",
                              "Índice: ", round(total*100, 2), "%"),
             data_id = entidad)) +
  ggiraph::geom_col_interactive() +
  geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
  scale_fill_gradient(low = "#D39C83", high = "#813753") +
  labs(
    title = "Índice de Capacidad, 2023",
    x = "Entidad",
    y = "", 
    caption = "Elaboración propia con base en el Censo de Procuración de Justicia Estatal 2024, Censo de Impartición de Justicia Estatal 2024, Censos de Gobiernos Estatles 2024, Proyecciones de población de CONAPO."
  ) +
  theme_bw() +
  #theme_1 + 
  theme(
    axis.title = element_text(family = "Montserrat", size = 9),
    plot.subtitle = element_text(family = "Montserrat", size = 7, colour = "#636363"),
    #plot.caption  = element_text(family = "Montserrat", size = 7, colour = "#636363", 
                                # margin = margin(b = 30, unit = "pt")),
    plot.title = element_text(family = "Montserrat",
                              face = "bold",
                              size = 35,
                              hjust = 0,
                              #vjust = 5, 
                              margin = margin(b = 30, unit = "pt")),
    #plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm") #,
    #plot.margin = margin(60, 20, 20, 20, "pt")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  ylim(0, 100) 


g_capacidad <- ggiraph::girafe(ggobj = g, 
                               width_svg = 14,    
                               height_svg = 10,    
                               pointsize = 12,
                               options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                              opts_hover(css = ''),
                                              opts_hover_inv(css = "opacity:0.1;"),
                                              opts_selection(type = "none"),
                                              opts_toolbar(saveaspng = FALSE))
                               )

g_capacidad

## Gráfica de barras por institucion -------


gen_barras_cap_inst <- function(ind_sel){
  
  
  datos_sel <- bd_capacidad %>%
    filter(
      categoria == "Total", 
      nom_indicador== ind_sel ) %>%
    mutate(total = as.numeric(total),
           cve_ent=as.character(cve_ent))

g <- datos_sel %>%
  ggplot(aes(x = reorder(entidad, total),
             y = total*100, 
             fill = total*100, 
             tooltip = paste0("<b>", entidad, "</b><br>",
                              "Puntaje: ", round(total*100, 2), "%"),
             data_id = entidad)) +
  ggiraph::geom_col_interactive() +
  scale_fill_gradient(low = "#D39C83", high = "#813753") +
  geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
  labs(
    title = paste0(
      "Puntuación ",
      ifelse(ind_sel %in% c("Fiscalía", "Defensoría Pública"), "de la ", "del "),
      ind_sel), 
    subtitle = "Índice de capacidad, 2023",
    x = "Entidad",
    y = ""
  ) +
  geom_hline( 
    yintercept = case_when(
    ind_sel == "Fiscalía" ~ 35,
    ind_sel == "Poder Judicial" ~ 30,
    ind_sel == "Defensoría Pública" ~ 20,
    ind_sel == "Órgano de coordinación" ~ 15,
    TRUE ~ NA_real_
  ),
  color = "black", 
             linetype="dotted", 
             linewidth =1) +
  annotate(
    "label", 
    x = 12, y = case_when(
      ind_sel == "Fiscalía" ~ 35,
      ind_sel == "Poder Judicial" ~ 30,
      ind_sel == "Defensoría Pública" ~ 20,
      ind_sel == "Órgano de coordinación" ~ 15,
      TRUE ~ NA_real_
    ),         # colocar al final de la línea
    label = "Puntaje máximo",
    hjust = .5, vjust = -10, # ajuste fino posición
    size = 5,                  # tamaño del texto
    fill = "#672044FF",           # fondo de la etiqueta (opcional)
    color = "white",           # color del texto
    fontface = "bold"
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
                              margin = margin(b = 10, unit = "pt")),
    #plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none" #,
    #axis.ticks.length = unit(0.2, "cm")
    #plot.margin = margin(60, 20, 20, 20, "pt")
  ) +
  #scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  ylim(0, 40) 


g_capacidad_institucion <- ggiraph::girafe(ggobj = g, 
                               width_svg = 16,    
                               height_svg = 12,    
                               pointsize = 12,
                               options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                              opts_hover(css = ''),
                                              opts_hover_inv(css = "opacity:0.1;"),
                                              opts_selection(type = "none"),
                                              opts_toolbar(saveaspng = FALSE)))


g_capacidad_institucion
}



gen_barras_cap_inst(ind_sel = "Defensoría Pública")



## Gráfica de barras por estado -------

# Procesamiento de la base para limpiar columnas 

bd_sel_capacidad <- bd_capacidad %>%
  mutate(
    nom_indicador_2 = case_when(
      categoria == "Total" ~ categoria, 
      TRUE ~ nom_indicador),
    institucion = case_when(
      categoria == "Total" ~ nom_indicador, 
      TRUE ~ categoria)
  ) %>%
  select(entidad, cve_ent, nom_indicador_2, institucion, total) %>%
  left_join(valores_max_capacidad,  by = c("institucion"= "nom_indicador")) %>%
  #filter(!(institucion %in% c("Ranking", "Índice de Capacidad"))) %>%
  mutate(titulo = paste0(institucion, " (", valor_maximo , "%)"))



# Función para el texto del box 
gen_texto_capacidad_entidad <- function(ent_sel) {
  
  datos_sel <- bd_sel_capacidad %>%
    filter(entidad == ent_sel)
  
  
  # Valores para boxes
  
  ranking <- datos_sel %>%
    filter( institucion == "Ranking") %>%
    select(entidad, institucion, total) 
  
  indice <- datos_sel %>%
    filter( institucion == "Índice de Capacidad") %>%
    select(entidad, institucion, total) %>%
    mutate(total = round(total*100, 2))
  
  bx_valores <- paste0(indice$total, "%  |  ", ranking$total, "/32" )
  #bx_entidad <- paste0(indice$entidad)
  return(bx_valores)
  #return(bx_entidad)
  
  
}

gen_texto_capacidad_entidad("Ciudad de México")





#Función para hacer las 4 gráficas

gen_grafica_capacidad_entidad <- function(ent_sel) {


  datos_sel <- bd_sel_capacidad %>%
    filter(entidad == ent_sel)
  
#Poder Judicial 
  
 g_4 <- datos_sel %>%
   filter(institucion == "Poder Judicial") %>%
   ggplot(aes(
     x = reorder(stringr::str_wrap(nom_indicador_2, 20), total*100), 
     y = total*100,
     fill = ifelse(nom_indicador_2 == "Total", "#813753FF", "#D39C83FF"),
     tooltip = paste0("<b>", nom_indicador_2, "</b><br>",
                      "Puntuación: ", round(total*100, 2), "%"),
     data_id = nom_indicador_2)) +
   ggiraph::geom_col_interactive(aes(alpha = ifelse(nom_indicador_2 == "Total", 1, 0.8))) +
   geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
   coord_flip() +
   theme_bw() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   scale_x_discrete(labels = c("Total" = expression(bold(Total)))) +
   scale_fill_identity() +
   scale_alpha_identity() +
   labs(
     subtitle = "Poder Judicial (30%)", 
     x = "Componente", 
     y = "Puntuación en porcentaje"
   ) +
   geom_hline( 
     yintercept = 30,
     color = "black", 
     linetype="dotted", 
     linewidth =1) +
   theme(
     axis.text.x = element_text(family = "Montserrat", angle = 0, hjust = 1, size = 10),
     axis.title = element_text(family = "Montserrat", size = 9),
     plot.subtitle = element_text(family = "Montserrat", size = 18, face = "bold", colour = "#636363", margin = margin(b = 30, unit = "pt")),
     panel.grid.major.x = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank(),
     axis.line = element_line(),
     plot.title = element_text(family = "Montserrat",
                               face = "bold",
                               size = 25,
                               hjust = 0),
     legend.position = "none"
   )+
   ylim(0,35)
 
g_pj <- ggiraph::girafe(
   ggobj = g_4,
    width_svg = 10,
    height_svg = 9,
   options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                  opts_hover(css = ''),
                  opts_hover_inv(css = "opacity:0.1;"),
                  opts_selection(type = "none"),
                  opts_toolbar(saveaspng = FALSE))
 )
   
  

# Gráfica de Fiscalía 
 
 
 g_1 <- datos_sel %>%
   filter(institucion == "Fiscalía") %>%
   ggplot(aes(
     x = reorder(stringr::str_wrap(nom_indicador_2, 20), total*100), 
     y = total*100,
     fill = ifelse(nom_indicador_2 == "Total", "#813753FF", "#D39C83FF"),
     tooltip = paste0("<b>", nom_indicador_2, "</b><br>",
                      "Puntuación: ", round(total*100, 2), "%"),
     data_id = nom_indicador_2)) +
   ggiraph::geom_col_interactive(aes(alpha = ifelse(nom_indicador_2 == "Total", 1, 0.8))) +
   geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
   coord_flip() +
   theme_bw() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   scale_x_discrete(labels = c("Total" = expression(bold(Total)))) +
   scale_fill_identity() +
   scale_alpha_identity() +
   labs(
     subtitle = "Fiscalía (35%)", 
     x = "Componente", 
     y = "Puntuación en porcentaje"
   ) +
   geom_hline( 
     yintercept = 35,
     color = "black", 
     linetype="dotted", 
     linewidth =1) +
   theme(
     axis.text.x = element_text(family = "Montserrat", angle = 0, hjust = 1, size = 10),
     axis.title = element_text(family = "Montserrat", size = 9),
     plot.subtitle = element_text(family = "Montserrat", size = 18, face = "bold", colour = "#636363", margin = margin(b = 30, unit = "pt")),
     panel.grid.major.x = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank(),
     axis.line = element_line(),
     plot.title = element_text(family = "Montserrat",
                               face = "bold",
                               size = 25,
                               hjust = 0),
     legend.position = "none"
   ) +
   ylim(0,40)
 
 g_fi <- ggiraph::girafe(
   ggobj = g_1,
   width_svg = 10,
   height_svg = 9,
   options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                  opts_hover(css = ''),
                  opts_hover_inv(css = "opacity:0.1;"),
                  opts_selection(type = "none"),
                  opts_toolbar(saveaspng = FALSE)))
 
 
 # Gráfica de Defensoría Pública 
 
 g_2 <- datos_sel %>%
   filter(institucion == "Defensoría Pública") %>%
   ggplot(aes(
     x = reorder(stringr::str_wrap(nom_indicador_2, 20), total*100), 
     y = total*100,
     fill = ifelse(nom_indicador_2 == "Total", "#813753FF", "#D39C83FF"),
     tooltip = paste0("<b>", nom_indicador_2, "</b><br>",
                      "Puntuación: ", round(total*100, 2), "%"),
     data_id = nom_indicador_2)) +
   ggiraph::geom_col_interactive(aes(alpha = ifelse(nom_indicador_2 == "Total", 1, 0.8))) +
   geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
   coord_flip() +
   theme_bw() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   scale_x_discrete(labels = c("Total" = expression(bold(Total)))) +
   scale_fill_identity() +
   scale_alpha_identity() +
   labs(
     subtitle = "Defensoría pública (20%)", 
     x = "Componente", 
     y = "Puntuación en porcentaje"
   ) +
   geom_hline( 
     yintercept = 20,
     color = "black", 
     linetype="dotted", 
     linewidth =1) +
   theme(
     axis.text.x = element_text(family = "Montserrat", angle = 0, hjust = 1, size = 10),
     axis.title = element_text(family = "Montserrat", size = 9),
     plot.subtitle = element_text(family = "Montserrat", size = 18, face = "bold", colour = "#636363", margin = margin(b = 30, unit = "pt")),
     panel.grid.major.x = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank(),
     axis.line = element_line(),
     plot.title = element_text(family = "Montserrat",
                               face = "bold",
                               size = 25,
                               hjust = 0),
     legend.position = "none"
   ) +
   ylim(0,25)
 
 g_dp <- ggiraph::girafe(
   ggobj = g_2,
   width_svg = 10,
   height_svg = 9,
   options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                  opts_hover(css = ''),
                  opts_hover_inv(css = "opacity:0.1;"),
                  opts_selection(type = "none"),
                  opts_toolbar(saveaspng = FALSE)))
 
 
 
# Gráfica de Órgano de coordinación 
 
 
 g_3 <- datos_sel %>%
   filter(institucion == "Órgano de coordinación") %>%
   ggplot(aes(
     x = reorder(stringr::str_wrap(nom_indicador_2, 20), total*100), 
     y = total*100,
     fill = ifelse(nom_indicador_2 == "Total", "#813753FF", "#D39C83FF"),
     tooltip = paste0("<b>", nom_indicador_2, "</b><br>",
                      "Puntuación: ", round(total*100, 2), "%"),
     data_id = nom_indicador_2)) +
   ggiraph::geom_col_interactive(aes(alpha = ifelse(nom_indicador_2 == "Total", 1, 0.8))) +
   geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
   coord_flip() +
   theme_bw() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   scale_x_discrete(labels = c("Total" = expression(bold(Total)))) +
   scale_fill_identity() +
   scale_alpha_identity() +
   labs(
     subtitle = "Órgano de coordinación (15%)", 
     x = "Componente", 
     y = "Puntuación en porcentaje"
   ) +
   geom_hline( 
     yintercept = 15,
     color = "black", 
     linetype="dotted", 
     linewidth =1) +
   theme(
     axis.text.x = element_text(family = "Montserrat", angle = 0, hjust = 1, size = 10),
     axis.title = element_text(family = "Montserrat", size = 9),
     plot.subtitle = element_text(family = "Montserrat", size = 18, face = "bold", colour = "#636363", margin = margin(b = 30, unit = "pt")),
     panel.grid.major.x = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank(),
     axis.line = element_line(),
     plot.title = element_text(family = "Montserrat",
                               face = "bold",
                               size = 25,
                               hjust = 0),
     legend.position = "none"
   ) +
   ylim(0,20)
 
 g_oc <- ggiraph::girafe(
   ggobj = g_3,
   width_svg = 10,
   height_svg = 9,
   options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                  opts_hover(css = ''),
                  opts_hover_inv(css = "opacity:0.1;"),
                  opts_selection(type = "none"),
                  opts_toolbar(saveaspng = FALSE)))
 
 list(
   grafica_pj = g_pj,
   grafica_fi = g_fi,
   grafica_dp = g_dp,
   grafica_oc = g_oc
 )

}



gen_grafica_capacidad_entidad("Baja California Sur")

## Tabla del índice ----

#componentes_capacidad <- c("Poder Judicial", "Fiscalía", "Defensoría Pública",  "Órgano de coordinación"

# TOTAL 

tab_total <- bd_capacidad %>%
  filter(categoria == "Total") %>%
  select(entidad, nom_indicador, total) %>%
  mutate(total = round(total*100, 2)) %>%
  pivot_wider(
    names_from = nom_indicador,           # Column names will be the state names
    values_from = total,          # Values will be the totals
    id_cols = entidad   # Row identifier (indicator names)
  )  %>%
  select(entidad, `Poder Judicial`, `Fiscalía`, `Defensoría Pública`, `Órgano de coordinación`, `Índice de Capacidad`)  %>%
  bind_rows(
    tibble( 
      entidad = "Valores máximos",
      `Poder Judicial` = 30,
      `Fiscalía` = 35,
      `Defensoría Pública` = 20,
      `Órgano de coordinación` = 15,
      `Índice de Capacidad` = 100)) %>%
  arrange(desc(`Índice de Capacidad`)) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            rowStyle = function(index) {
              if (index == 1) {
                list(
                  background = "#D39C83FF",
                  fontWeight = "bold",
                  color = "black"
                )
              }
            }, 
            columns = list( 
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Índice de Capacidad" = colDef(format = colFormat(suffix = "%"), 
                                             style = function(valor) {
                                               scaled <- (valor - 5) / (150)
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
            outlined = T)

tab_total

# FISCALÍA Y COMPONENTES 

tab_fis_cap <- bd_capacidad %>%
  filter(categoria == "Fiscalía" | nom_indicador == "Fiscalía") %>%
  select(entidad, nom_indicador, total) %>%
  mutate(total = round(total*100, 2)) %>%
  pivot_wider(
    names_from = nom_indicador,           # Column names will be the state names
    values_from = total,          # Values will be the totals
    id_cols = entidad   # Row identifier (indicator names)
  )  %>%
  #select(entidad, `Poder Judicial`, `Fiscalía`, `Defensoría Pública`, `Órgano de coordinación`, `Índice de Capacidad`)  %>%
  bind_rows(
    tibble( 
      entidad = "Valores máximos",
      `Fiscalía` = 35,
      `Personal suficiente` = 10,
      `Presupuesto` = 10,
      `Autonomía constitucional` = 5,
      `MASC` = 1,
      `UMECA` = 1,
      `Justicia para mujeres` = 1,
      `Justicia para adolescentes` = 1,
      `Tasa de fiscalías por cada 100,000 hab` = 5 
      )) %>%
  arrange(desc(`Fiscalía`)) %>%
  select( entidad, `Fiscalía`, `Personal suficiente`, `Presupuesto`, `Autonomía constitucional`,  `Tasa de fiscalías por cada 100,000 hab`, everything()) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Fiscalía" = colDef(format = colFormat(suffix = "%"), 
                                             name = "Total - Fiscalía",
                                             style = function(valor) {
                                               scaled <- (valor - 5) / (70)
                                               scaled <- max(min(scaled, 1), 0)         
                                               color <- color_scale[floor(scaled * 99) + 1]
                                               list(
                                                 background = color,
                                                 color = ifelse(scaled > 0.13, "white", "black")
                                               )
                                             })),
            rowStyle = function(index) {
              if (index == 1) {
                list(
                  background = "#D39C83FF",
                  fontWeight = "bold",
                  color = "black"
                )
              }
            },
            pagination = F, 
            compact = T,
            bordered = T,
            outlined = T)

tab_fis_cap



# PODER JUDICIAL Y COMPONENTES 

tab_pj_cap <- bd_capacidad %>%
  filter(categoria == "Poder Judicial" | nom_indicador == "Poder Judicial") %>%
  select(entidad, nom_indicador, total) %>%
  mutate(total = round(total*100, 2)) %>%
  pivot_wider(
    names_from = nom_indicador,           
    values_from = total,          
    id_cols = entidad   
  )  %>%
  bind_rows(
    tibble( 
      entidad = "Valores máximos",
      `Poder Judicial` = 30,
      `Personal suficiente` = 10,
      `Presupuesto` = 10,
      `Carrera judicial` = 5,
      `Tasa de salas de audiencia por 100,000 hab` = 5
    )) %>%
  arrange(desc(`Poder Judicial`)) %>%
  select( entidad, `Poder Judicial`, `Personal suficiente`, `Presupuesto`, `Carrera judicial`,  `Tasa de salas de audiencia por 100,000 hab`) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Poder Judicial" = colDef(format = colFormat(suffix = "%"), 
                                  name = "Total - PJ",
                                  style = function(valor) {
                                    scaled <- (valor - 5) / (70)
                                    scaled <- max(min(scaled, 1), 0)         
                                    color <- color_scale[floor(scaled * 99) + 1]
                                    list(
                                      background = color,
                                      color = ifelse(scaled > 0.13, "white", "black")
                                    )
                                  })),
            rowStyle = function(index) {
              if (index == 1) {
                list(
                  background = "#D39C83FF",
                  fontWeight = "bold",
                  color = "black"
                )
              }
            },
            pagination = F, 
            compact = T,
            bordered = T,
            outlined = T)

tab_pj_cap


# DEFENSORÍA Y COMPONENTES 

tab_def_cap <- bd_capacidad %>%
  filter(categoria == "Defensoría Pública" | nom_indicador == "Defensoría Pública") %>%
  select(entidad, nom_indicador, total) %>%
  mutate(total = round(total*100, 2)) %>%
  pivot_wider(
    names_from = nom_indicador,           
    values_from = total,          
    id_cols = entidad   
  )  %>%
  bind_rows(
    tibble( 
      entidad = "Valores máximos",
      `Defensoría Pública` = 20,
      `Personal suficiente` = 10,
      `Presupuesto` = 10
    )) %>%
  arrange(desc(`Defensoría Pública`), entidad) %>%
  #select( entidad, `Poder Judicial`, `Personal suficiente`, `Presupuesto`, `Carrera judicial`,  `Tasa de salas de audiencia por 100,000 hab`) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Defensoría Pública" = colDef(format = colFormat(suffix = "%"), 
                                        name = "Total - DP",
                                        style = function(valor) {
                                          scaled <- (valor - 4) / (20)
                                          scaled <- max(min(scaled, 1), 0)         
                                          color <- color_scale[floor(scaled * 99) + 1]
                                          list(
                                            background = color,
                                            color = ifelse(scaled > 0.13, "white", "black")
                                          )
                                        })),
            rowStyle = function(index) {
              if (index == 1) {
                list(
                  background = "#D39C83FF",
                  fontWeight = "bold",
                  color = "black"
                )
              }
            },
            pagination = F, 
            compact = T,
            bordered = T,
            outlined = T)

tab_def_cap


# ÓRGANO Y COMPONENTES 

tab_org_cap <- bd_capacidad %>%
  filter(categoria == "Órgano de coordinación" | nom_indicador == "Órgano de coordinación") %>%
  select(entidad, nom_indicador, total) %>%
  mutate(total = round(total*100, 2)) %>%
  pivot_wider(
    names_from = nom_indicador,           
    values_from = total,          
    id_cols = entidad   
  )  %>%
  bind_rows(
    tibble( 
      entidad = "Valores máximos",
      `Órgano de coordinación` = 15,
      `Institución consolidada` = 10
    )) %>%
  arrange(desc(`Órgano de coordinación`), entidad) %>%
  #select( entidad, `Poder Judicial`, `Personal suficiente`, `Presupuesto`, `Carrera judicial`,  `Tasa de salas de audiencia por 100,000 hab`) %>%
  reactable(striped = T, 
            defaultColDef = colDef( 
              align = "center",
              format = colFormat(separators = TRUE)),
            columns = list( 
              entidad = colDef( name="Entidad", filterable = TRUE, align = "left"),
              "Defensoría Pública" = colDef(format = colFormat(suffix = "%"), 
                                            name = "Total - DP",
                                            style = function(valor) {
                                              scaled <- (valor - 4) / (20)
                                              scaled <- max(min(scaled, 1), 0)         
                                              color <- color_scale[floor(scaled * 99) + 1]
                                              list(
                                                background = color,
                                                color = ifelse(scaled > 0.13, "white", "black")
                                              )
                                            })),
            rowStyle = function(index) {
              if (index == 1) {
                list(
                  background = "#D39C83FF",
                  fontWeight = "bold",
                  color = "black"
                )
              }
            },
            pagination = F, 
            compact = T,
            bordered = T,
            outlined = T)

tab_org_cap