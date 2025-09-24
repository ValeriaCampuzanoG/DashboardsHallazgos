#Librerías 

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(leaflet)
library(fresh)




# ui
ui <-  dashboardPage( 
  
  freshTheme = theme_hgz, 
  
  help = NULL, 
  fullscreen = TRUE, 
  scrollToTop = TRUE,
  
  
  title = "Índice de impunidad",
  header = dashboardHeader(),
  # navbar = bs4DashNavbar(
  #   skin = "light",
  #   border = TRUE
  # ),
  sidebar = bs4DashSidebar(
    skin = "light",
    title = "Índice de impunidad",
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Índice de impunidad",
        tabName = "impunidad",
        icon = icon("chart-line")
        
      )
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "impunidad",
        jumbotron(
          title = "Índice de impunidad",
          status = "info",
          lead = "prueba"
        ), 
        fluidRow(
          bs4Card(
            width = 12,
            title = "Mapa",
            solidHeader = F,
            collapsible = F,
            maximizable = TRUE,
            selectInput(
              inputId = "anioMapaImpunidad",
              label = "Seleccione un año:",
              choices = c("2019", "2020", "2021", "2022", "2023"),
              selected = "2023"
            ),
            fluidRow(
              column(
                width = 6, 
                bs4Dash::bs4ValueBoxOutput("box_impunidad_menos", width = 12)),
              column( width = 6, 
                      bs4Dash::bs4ValueBoxOutput("box_impunidad_mas", width = 12)
                      )), 
            div(leafletOutput("mapa_impunidad", height = "95vh"), style = "margin-top: 15px;")
          )
        ),
        hr(),
        fluidRow(
          bs4Card(
            width = 12,
            title = "Componentes del índice de impunidad",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE, 
            headerBorder = TRUE,
            div(
              style = "margin-bottom: 15px; display: flex; gap: 15px; align-items: end;",
              div(
                style = "width: 500px;",
                selectInput(
                  inputId = "opcionBarrasImpunidad",
                  label = "Seleccione un indicador:",
                  choices = opciones_impunidad,
                  width = "100%"
                )
              ),
              div(
                style = "width: 500px;",
                selectInput(
                  inputId = "anioBarrasImpunidad",
                  label = "Seleccione un año:",
                  choices = c("2020", "2021", "2022", "2023", "2024"),
                  selected = "2023",
                  width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                h5( style = "text-align: center; margin-bottom: 10px;"),
                plotlyOutput("grafica_barras_impunidad", height = "80vh") %>% withSpinner()
              ),
              column(
                width = 6,
                h5("", style = "text-align: center; margin-bottom: 10px;"),
                plotlyOutput("grafica_lineas_impunidad", height = "80vh") %>% withSpinner(), 
                shinyWidgets::pickerInput(
                  inputId = "seleccion_entidades_lineas_impunidad",
                  label = "Selecciona la(s) entidad(es):",
                  choices = unique(bd_impunidad$entidad),
                  multiple = TRUE,
                  selected = sort(x = unique(bd_impunidad$entidad)),
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todas",
                                 `select-all-text` = "Seleccionar todas",
                                 `none-selected-text` = "Ninguna unidad seleccionada")
                )
              )
            )
          )

        ),
        br(),
        hr(),
        tabBox(
          title = "Tabla", 
          width = 12, 
          type = "tabs", 
          status= "olive", 
          solidHeader = TRUE, 
          
          tabPanel(
            "2023", 
            reactableOutput("tablaImpunidad23")
          ), 
          tabPanel(
            "2022", 
            reactableOutput("tablaImpunidad22")
          ), 
          tabPanel(
            "2021", 
            reactableOutput("tablaImpunidad21")
          ), 
          tabPanel(
            "2020", 
            reactableOutput("tablaImpunidad20")
          ), 
          tabPanel(
            "2019", 
            reactableOutput("tablaImpunidad19")
          )
          
        )
      )
    )
  ),
  controlbar = NULL,
  footer = NULL
)

server <- function(input, output, session) {
  
  #Caja de entidad con mayor impunidad
  output$box_impunidad_mas <- bs4Dash::renderValueBox({  
    top <- bd_impunidad %>%
      filter(ano == input$anioBarrasImpunidad,
             nom_indicador == "Índice de impunidad") %>%
      mutate(valor = as.numeric(valor)) %>%
      slice_max(order_by = valor, n = 1, with_ties = FALSE) 
  
    vb_value <- paste0(top$entidad, " ", top$valor, " %" )
    
    bs4Dash::bs4ValueBox(  
      value = tags$span(vb_value, style = "font-size: 2.2rem; font-weight: 700;"),
      subtitle = "Entidad con mayor impunidad",
      icon = icon("arrow-up"),  # arrow-down if you show “menor impunidad”
      color = "fuchsia",
      width = 12
    )
  })
  
  #Caja de entidad con menor impunidad
  output$box_impunidad_menos <- bs4Dash::renderValueBox({
    req(input$anioBarrasImpunidad)
    
    bottom <- bd_impunidad %>%
      dplyr::mutate(
        ano = as.character(ano),
        valor = as.numeric(valor)
      ) %>%
      dplyr::filter(
        ano == as.character(input$anioBarrasImpunidad),
        nom_indicador == "Índice de impunidad"
      ) %>%
      dplyr::slice_min(order_by = valor, n = 1, with_ties = FALSE)
    
    req(nrow(bottom) == 1)
    
    vb_value <- paste0(bottom$entidad, " ", sprintf("%.1f%%", bottom$valor))
    
    bs4Dash::bs4ValueBox(
      value = tags$span(vb_value, style = "font-size: 2.2rem; font-weight: 700;"),
      subtitle = "Entidad con menor impunidad",
      icon = icon("arrow-down"),
      color = "maroon",
      width = 12
    )
  })
  
  #Tabla de impunidad 2023
  output$tablaImpunidad23 <- renderReactable({
    tab_imp2023
  }) 
  
  #Tabla de impunidad 2022
  output$tablaImpunidad22 <- renderReactable({
    tab_imp2022
  })  
  
  #Tabla de impunidad 2021
  output$tablaImpunidad21 <- renderReactable({
    tab_imp2021
  })  
  
  #Tabla de impunidad 2020
  output$tablaImpunidad20 <- renderReactable({
    tab_imp2020
  })  
  
  #Tabla de impunidad 2019
  output$tablaImpunidad19 <- renderReactable({
    tab_imp2019
  })  
  #Grafica de barras de impunidad
  output$grafica_barras_impunidad <- renderPlotly({
    gen_barras_imp(ind_sel = input$opcionBarrasImpunidad, 
                   ano_sel_imp = input$anioBarrasImpunidad)
  })
  
  #Gráfica líneas de impunidad 
  output$grafica_lineas_impunidad <- renderPlotly({
    gen_lineas_imp(ind_sel = input$opcionBarrasImpunidad, 
                   entidades_resaltadas = input$seleccion_entidades_lineas_impunidad)
  })
  
  #Mapa de impunidad
  output$mapa_impunidad <- renderLeaflet({
    gen_mapa_impunidad(ano_sel_imp = input$anioMapaImpunidad )
  })
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
