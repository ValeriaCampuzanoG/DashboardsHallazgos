# DASHBOARD CAPACIDAD


#Librerías 

#install.packages("renv")

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(leaflet)
library(fresh)

#source(file = "paquetes-setup.R")
#source(file = "importacion_textos.R")
#source(file = "codigo_preparacion.R")


# ui
ui <-  dashboardPage( 
  
  freshTheme = theme_hgz, 
  
  help = NULL, 
  fullscreen = TRUE, 
  scrollToTop = TRUE,
  
  
  title = "Índice de capacidad",
  header = dashboardHeader(),

  
  sidebar = bs4DashSidebar(
    skin = "light",
    title = "Índice de capacidad",
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Índice de capacidad",
        tabName = "capacidad",
        icon = icon("chart-line")
        
      )
    )
  ),
  
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "capacidad",
        jumbotron(
          title = "Índice de capacidad",
          status = "info",
          lead = "prueba"
        ),
        # Comienza card del mapa, según yo ya está lista
        fluidRow(
          bs4Card(
            width = 12,
            title = "Mapa",
            solidHeader = F,
            collapsible = F,
            maximizable = TRUE,
            fluidRow(
              column(
                width = 6, 
                bs4Dash::bs4ValueBoxOutput("box_capacidad_mas", width = 12)),
              column( 
                width = 6, 
                      bs4Dash::bs4ValueBoxOutput("box_capacidad_menos", width = 12)
              )), 
            div(leafletOutput("mapa_capacidad", height = "95vh"), style = "margin-top: 15px;")
          )
        ),
        hr(),
        # Comienza card con las tres gráficas (total, por componente y por entidad)
        # fluidRow(
        #   bs4Card(
        #     width = 12,
        #     title = "Componentes del índice de capacidad",
        #     solidHeader = TRUE,
        #     collapsible = TRUE,
        #     maximizable = TRUE, 
        #     headerBorder = TRUE,
        #     div(
        #       style = "margin-bottom: 15px; display: flex; gap: 15px; align-items: end;",
        #       div(
        #         style = "width: 500px;",
        #         selectInput(
        #           inputId = "opcionBarrasImpunidad",
        #           label = "Seleccione un indicador:",
        #           choices = opciones_impunidad,
        #           width = "100%"
        #         )
        #       ),
        #       div(
        #         style = "width: 500px;",
        #         selectInput(
        #           inputId = "anioBarrasImpunidad",
        #           label = "Seleccione un año:",
        #           choices = c("2020", "2021", "2022", "2023", "2024"),
        #           selected = "2023",
        #           width = "100%"
        #         )
        #       )
        #     ),
        #     fluidRow(
        #       column(
        #         width = 6,
        #         h5( style = "text-align: center; margin-bottom: 10px;"),
        #         plotlyOutput("grafica_barras_impunidad", height = "80vh") %>% withSpinner()
        #       ),
        #       column(
        #         width = 6,
        #         h5("", style = "text-align: center; margin-bottom: 10px;"),
        #         plotlyOutput("grafica_lineas_impunidad", height = "80vh") %>% withSpinner(), 
        #         shinyWidgets::pickerInput(
        #           inputId = "seleccion_entidades_lineas_impunidad",
        #           label = "Selecciona la(s) entidad(es):",
        #           choices = unique(bd_impunidad$entidad),
        #           multiple = TRUE,
        #           selected = sort(x = unique(bd_impunidad$entidad)),
        #           options = list(`actions-box` = TRUE,
        #                          `deselect-all-text` = "Deseleccionar todas",
        #                          `select-all-text` = "Seleccionar todas",
        #                          `none-selected-text` = "Ninguna unidad seleccionada")
        #         )
        #       )
        #     )
        #   )
        #   
        # ),
        # Termina card de las tres pestañas 
        br(),
        hr(),
        # Comienza tab de la tabla con la info de cada componente
        tabBox(
          title = "Tabla", 
          width = 12, 
          type = "tabs", 
          status= "pink", 
          solidHeader = TRUE, 
          
          tabPanel(
            "Total", 
            reactableOutput("tablaCapacidadTotal")
          ), 
          tabPanel(
            "Poder Judicial", 
            reactableOutput("tablaCapacidadPJ")
          ), 
          tabPanel(
            "Fiscalía", 
            reactableOutput("tablaCapacidadFis")
          ), 
          tabPanel(
            "Defensoría Pública", 
            reactableOutput("tablaCapacidadDef")
          ), 
          tabPanel(
            "Órgano de Consolidación", 
            reactableOutput("tablaCapacidadOrg")
          )
          
        )
      )
    )
  ),
  controlbar = NULL,
  footer = NULL
)

server <- function(input, output, session) {
  
  #Caja de entidad con mayor capacidad
  output$box_capacidad_mas <- bs4Dash::renderValueBox({  
    top <- bd_capacidad %>%
      filter(
             nom_indicador == "Índice de Capacidad") %>%
      mutate(total = round(total*100, 2)) %>%
      slice_max(order_by = total, n = 1, with_ties = FALSE) 
    
    vb_value <- paste0(top$entidad, ": ", top$total, " %" )
    
    bs4Dash::bs4ValueBox(  
      value = tags$span(vb_value, style = "font-size: 2.2rem; font-weight: 700;"),
      subtitle = "Entidad con mayor capacidad",
      icon = icon("arrow-up"),  # arrow-down if you show “menor impunidad”
      color = "fuchsia",
      width = 12
    )
  })
  
  #Caja de entidad con menor capacidad
  output$box_capacidad_menos <- bs4Dash::renderValueBox({
    
    bottom <- bd_capacidad %>%
      mutate(
       total = round(total*100, 2)
      ) %>%
      filter(
        nom_indicador == "Índice de Capacidad"
      ) %>%
      dplyr::slice_min(order_by = total, n = 1, with_ties = FALSE)
    
    req(nrow(bottom) == 1)
    
    vb_value <- paste0(bottom$entidad, ": ", sprintf("%.1f%%", bottom$total))
    
    bs4Dash::bs4ValueBox(
      value = tags$span(vb_value, style = "font-size: 2.2rem; font-weight: 700;"),
      subtitle = "Entidad con menor capacidad",
      icon = icon("arrow-down"),
      color = "maroon",
      width = 12
    )
  })
  
  #Mapa de capacidad
  output$mapa_capacidad <- renderLeaflet({
    mapa_base_capacidad
  })
  
  
  #Barras totales de capacidad
  output$grafica_barras_capacidad_tot <- ggiraph::renderGirafe({g_capacidad })
  
  #Grafica de barras de capacidad por institución
  output$grafica_barras_capacidad_inst <- renderPlotly({
    gen_barras_cap_inst(ind_sel4 = input$selBarrasCapInst)
  })
  
  #Grafica de barras de capacidad por estado
  output$grafica_barras_capacidad_edo <- renderPlotly({
    gen_barras_cap_edo(ind_sel5  = input$selBarrasImpuEdo)
  })
  
  #Tabla del índice de capacidad total
  output$tablaCapacidadTotal <- renderReactable({
    tab_total
  })
  
  #Tabla del Poder Judicial
  output$tablaCapacidadPJ <- renderReactable({
    tab_pj_cap
  })
  
  #Tabla de la Fiscalía
  output$tablaCapacidadFis <- renderReactable({
    tab_fis_cap
  })
  
  #Tabla de la Defensoría
  output$tablaCapacidadDef <- renderReactable({
    tab_def_cap
  })
  
  #Tabla del Órgano de Consolidación
  output$tablaCapacidadOrg <- renderReactable({
    tab_org_cap
  })
  
  
  
  
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)