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
        #Comienza card con las tres gráficas (total, por componente y por entidad)
        fluidRow(
          bs4Card(
            width = 12,
            title = "Componentes del índice de capacidad",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            headerBorder = TRUE,
            tabBox(
              id = "card_tabs_capacidad_alt",
              width = 12,
              type = "tabs",
              side = "left",          # or "right"
              tabPanel("Total",    
                       ggiraph::girafeOutput("grafica_barras_capacidad_tot", height = "90vh") %>% withSpinner()
                       ),
              tabPanel("Por componente", 
                       div(
                         style = "width: 500px;",
                         selectInput(
                           inputId = "selBarrasCapxInst",
                           label = "Seleccione un componente del índice de capacidad:",
                           choices = opciones_capacidad,
                           width = "100%"
                         )
                       ),
                       ggiraph::girafeOutput("grafica_barras_capacidad_inst", height = "90vh") %>% withSpinner()
                       ),
              tabPanel("Por entidad", 
                       fluidRow(
                         column(
                           width = 3, 
                           style = "width: 500px; margin-bottom: 20px;",
                           selectInput(
                             inputId = "selBarrasCapxEntidad",
                             label = "Seleccione una entidad:",
                             choices = unique(bd_capacidad$entidad),
                             width = "100%"
                           )),
                         column( 
                           width = 6, 
                           bs4Dash::bs4ValueBoxOutput("box_info_capacidad_entidad", width = 12)
                         )),
                       fluidRow(
                         column(
                           width = 6,
                           #h5("Poder Judicial | 30%", style = "text-align: center; margin-bottom: 10px;"),
                           ggiraph::girafeOutput("grafica_barras_pj", height = "50vh") %>% withSpinner()
                         ),
                         column(
                           width = 6,
                           #h5("Fiscalía | 35%", style = "text-align: center; margin-bottom: 10px;"),
                           ggiraph::girafeOutput("grafica_barras_fisc", height = "50vh") %>% withSpinner()
                         )
                       ),
                       br(),
                       fluidRow(
                         column(
                           width = 6,
                           #h5("Defensoría Pública | 20%" , style = "text-align: center; margin-bottom: 10px;"),
                           ggiraph::girafeOutput("grafica_barras_dp", height = "50vh") %>% withSpinner()
                         ),
                         column(
                           width = 6,
                           #h5("Órgano de coordinación | 15%", style = "text-align: center; margin-bottom: 10px;"),
                           ggiraph::girafeOutput("grafica_barras_org", height = "50vh") %>% withSpinner()
                         )
                       )
              )
            )
          )
        ),
        #Termina card de las tres pestañas
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
      icon = icon("arrow-up"), 
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
  output$grafica_barras_capacidad_tot <- ggiraph::renderGirafe({g_capacidad})
  
  #Grafica de barras de capacidad por institución
  output$grafica_barras_capacidad_inst <- ggiraph::renderGirafe({
    gen_barras_cap_inst(ind_sel = input$selBarrasCapxInst)
  })
  
  
  #Caja con el valor total de la entidad y el ranking
  output$box_info_capacidad_entidad <- bs4Dash::renderValueBox({  
    
    texto <- gen_texto_capacidad_entidad(input$selBarrasCapxEntidad)
    
    bs4Dash::bs4ValueBox(  
      value = tags$span(texto, style = "font-size: 2.2rem; font-weight: 700;"),
      subtitle = paste0("Índice de capacidad y ranking de ", input$selBarrasCapxEntidad) ,
      icon = icon("globe"),  
      color = "fuchsia",
      width = 12
    )
  })
  
  
  
  # Graficas con desagregacion de los componentes del índice por entidad
  graficas_entidad <- reactive({
    req(input$selBarrasCapxEntidad)
    gen_grafica_capacidad_entidad(input$selBarrasCapxEntidad)
  })
  
  
  # Render each plot
  output$grafica_barras_pj <- ggiraph::renderGirafe({
    
    #graficas_entidad()$grafica_pj
    plots <- graficas_entidad(); req(is.list(plots), !is.null(plots$grafica_pj))
    plots$grafica_pj
  })
  
  output$grafica_barras_fisc <- ggiraph::renderGirafe({
    #graficas_entidad()$grafica_fi
    plots <- graficas_entidad(); req(is.list(plots), !is.null(plots$grafica_fi))
    plots$grafica_fi
  })
  
  output$grafica_barras_dp <- ggiraph::renderGirafe({
    graficas_entidad()$grafica_dp
  })
  
  output$grafica_barras_org <- ggiraph::renderGirafe({
    graficas_entidad()$grafica_oc
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