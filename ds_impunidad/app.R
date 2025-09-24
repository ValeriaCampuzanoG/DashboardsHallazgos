#Librerías 

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(leaflet)

# ui
ui <-  dashboardPage( 
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
            selectInput(
              inputId = "anioMapaImpunidad",
              label = "Seleccione un año:",
              choices = c("2019", "2020", "2021", "2022", "2023"),
              selected = "2023"
            ),
            fluidRow(
              column(
                width = 6, 
                infoBox(
                  width = 12, 
                  title = "Entidad con menor impunidad", 
                  value = "Querétaro",
                  icon = icon("arrow-down"),
                  color = "fuchsia")
              ),
              column( width = 6, 
                      infoBox(
                        width = 12, 
                        title = "Entidad con mayor impunidad", 
                        value = "Oaxaca",
                        icon = icon("arrow-up"),
                        color = "fuchsia"))
            ),
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
                plotlyOutput("grafica_barras_impunidad", height = "60vh") %>% withSpinner()
              ),
              column(
                width = 6,
                h5("Serie 2019-2023", style = "text-align: center; margin-bottom: 10px;"),
                plotlyOutput("grafica_lineas_impunidad", height = "60vh") %>% withSpinner()
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
        # fluidRow(
        #   bs4Card(
        #     width = 12,
        #     title = "Tabla",
        #     solidHeader = TRUE,
        #     collapsible = TRUE,
        #     div(
        #       reactableOutput("tablaImpunidad"),
        #       style = "margin-top:30px;"
        #     )
        #   )
        # ) #,
        # br(),
        # div(
        #   class = "footer",
        #   footer_superior,
        #   footer_inferior
        # )
      )
    )
  ),
  controlbar = NULL,
  footer = NULL
)

server <- function(input, output, session) {
  
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
    gen_lineas_imp(ind_sel = input$opcionBarrasImpunidad)
    
  })
  
  #Mapa de impunidad
  output$mapa_impunidad <- renderLeaflet({
    gen_mapa_impunidad(ano_sel_imp = input$anioMapaImpunidad )
  })
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
