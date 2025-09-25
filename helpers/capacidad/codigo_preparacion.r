options(scipen = 999)
options(repos = c(CRAN = "https://cran.r-project.org/"))


# CAPACIDAD #

install.packages("ggiraph")

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


# Importar datos

bd_capacidad <- read_excel("www/bd/bd_indice_capacidad.xlsx")
catalogo_estatal <- read_excel("www/bd/catalogo_estatal.xlsx")
shp <- read_sf("www/DivisionEstatal.geojson")
valores_max_capacidad <- read_excel("www/bd/metadatos_valores_icapacidad.xlsx")


bd_capacidad <- bd_capacidad %>%
  left_join(catalogo_estatal, by="entidad") %>%
  mutate(cve_ent = ifelse(is.na(cve_ent), 15, cve_ent) )



## Mapa leaflet ----

datos_sel_capacidad <- bd_capacidad %>%
  filter(nom_indicador == "Índice de Capacidad") 

mapcap <- left_join(shp, datos_sel_capacidad, by = c("CVE_EDO" = "cve_ent"))  

mapcap$ranking <- rank(-mapcap$total, ties.method = "first")

label_cap <- paste0(
  "<b style='font-size:25px;'>", mapcap$ranking, "/32</b><br>",
  "<b style='font-size:20px;'><span style='color:#9e3963;'>", mapcap$entidad, "</span> </b><br>",
  "<span style='font-size:32px;'>",round(mapcap$total*100, 2), "%</span>")

paleta_cap <- colorNumeric(palette = color_scale, 
                           domain = mapcap$total, reverse = F)


#pasos para un leaflet  
mapa_base_capacidad <-  
  leaflet(mapcap) %>%  
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
                          tooltip = paste0("<b>", entidad, "</b><br>",
                                           "Índice: ", round(total*100, 2), "%"),
                          data_id = entidad)) +
  ggiraph::geom_segment_interactive(aes(x = reorder(entidad, total), xend = reorder(entidad, total),
                               y = 0, yend = total*100),
                           color = "#D39C83FF", linewidth = 1.0) +
  ggiraph::geom_point_interactive(color = "#541F3FFF", size = 4, alpha = 0.8) +
  geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
  labs(
    title = "Índice de Capacidad, 2023",
    x = "Entidad",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(family = "Montserrat", size = 9),
    plot.subtitle = element_text(family = "Montserrat", size = 7, colour = "#636363"),
    plot.title = element_text(family = "Montserrat",
                              face = "bold",
                              size = 35,
                              hjust = 0,
                              margin = margin(b = 30, unit = "pt")),
    #plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "bottom",
    axis.ticks.length = unit(0.2, "cm"),
    plot.margin = margin(60, 20, 20, 20, "pt")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  ylim(0, 100) 


g_capacidad <- ggiraph::girafe(ggobj = g, 
                               width_svg = 12,    
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


opciones_capacidad<- c("Poder Judicial", "Fiscalía", "Defensoría Pública", "Órgano de coordinación")



gen_barras_cap_inst <- function(ind_sel4){
  
  datos_sel <- bd_capacidad %>%
    filter(nom_indicador %in% opciones_capacidad) %>%
    filter(nom_indicador == ind_sel4) %>%
    left_join(valores_max_capacidad, by= "nom_indicador")
  
  
  g <- datos_sel %>% 
    ggplot(aes(x = reorder(entidad, total), 
               y = total*100, 
               fill = total*100, 
               text = paste0( "<span style='font-size:24px;'><b>", entidad, "</b></span><br><br>",
                              "<span style='font-size:18px;'>", prettyNum(round(total*100, 2), big.mark = ","), "/", valor_maximo
               )
    )) + 
    geom_col() + 
    coord_flip() +
    scale_fill_gradientn(colors = color_scale) + 
    theme_bw() + 
    labs(title = paste0("Puntuación de " , unique(datos_sel$nom_indicador), " por entidad") , 
         x = NULL, 
         y = NULL,) + 
    scale_y_continuous(expand = expansion(c(0, 0.1)), 
                       labels = comma_format()) + 
    theme(
      panel.grid = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(), 
      legend.position = "none"
    ) +
    geom_text(aes(label = round(total*100, 2)), hjust = -0.5, colour = "#535353") +
    guides(fill = "none")
  
  ggplotly(g, tooltip = "text") 
  
}


gen_barras_cap_inst(ind_sel4 = "Fiscalía")



## Gráfica de barras por estado -------

# Estática: índice de capacidad general

bd_capacidad %>%
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
  filter(!(institucion %in% c("Ranking", "Índice de Capacidad"))) %>%
  mutate(titulo = paste0(institucion, " (", valor_maximo , "%)")) %>%
  filter(entidad == "Colima") %>%
  # arrange(desc(nom_indicador_2 == "Total"), total) %>%  
  # mutate(nom_indicador_2 = factor(nom_indicador_2, levels = unique(nom_indicador_2))) %>%
  ggplot(aes(x = reorder(stringr::str_wrap(nom_indicador_2, 20), total*100), y = total*100, fill = institucion)) +
  geom_col(aes(alpha = ifelse(nom_indicador_2 == "Total", 1, 0.8))) +
  geom_text(aes(label = paste0(round(total*100, 2), "%")), hjust = -0.5, colour = "#535353") +
  coord_flip() +
  theme_bw() +
  facet_wrap(~titulo, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("Total" = expression(bold(Total))))  +
  scale_alpha_identity() +
  scale_fill_manual(values = c(
    "Poder Judicial" = "#70284AFF",    # Replace with your actual institutions
    "Fiscalía" = "#DC7176FF",    
    "Defensoría Pública" = "#D39C83FF",    
    "Órgano de coordinación" = "#541F3FFF"  
  )) +
  labs(
    title = paste0("Índice de impunidad, Colima"),
    subtitle = "Componentes",
    x = NULL,   
    y = NULL, 
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(family = gt::google_font("Montserrat"), angle = 0, hjust = 1, size = 10),
    #axis.text.y = element_text(family = gt::google_font("Montserrat")),
    axis.title = element_text(family = gt::google_font("Montserrat"), size = 9),
    plot.subtitle = element_text(family = gt::google_font("Montserrat"), size = 18,   face = "bold",colour = "#636363"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(family = gt::google_font("Montserrat"),
                              face = "bold",
                              size = 25,
                              hjust = 0),
    legend.position = "none"
  ) +
  guides(alpha = "none")


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

tab_def_cap