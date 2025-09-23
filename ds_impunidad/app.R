#Librerías 

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(leaflet)

# ui
ui <-  dashboardPage(
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
        fluidRow(
          bs4Card(
            width = 12,
            title = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            div(
              h3("Índice de impunidad"),
              br(),
              txt_impunidad_intro,
              style = "margin-top:40px;"
            )
          )
        ),
        fluidRow(
          # bs4Card(
          #   width = 6,
          #   title = "Año del mapa",
          #   solidHeader = TRUE,
          #   collapsible = TRUE,
          #   selectInput(
          #     inputId = "anioMapaImpunidad",
          #     label = "Seleccione un año:",
          #     choices = c("2020", "2021", "2022", "2023", "2024"),
          #     selected = "2023"
          #   ),
          #   # Si decides mostrar el mapa, descomenta lo siguiente y agrega tu render en el server:
          #    #div(leafletOutput("mapa_impunidad", height = "95vh"), style = "margin-top: 15px;")
          # )
        ),
        hr(),
        fluidRow(
          bs4Card(
            width = 12,
            title = "¿Cómo se interpreta?",
            solidHeader = TRUE,
            collapsible = TRUE,
            div(
              h4("¿Cómo se interpreta?"),
              br(),
              p("
                A mayor porcentaje mayor impunidad, significa que el sistema no logró ofrecer una respuesta satisfactoria a los casos."),
              style = "margin-top:40px;"
            )
          )
        ),
        hr(),
        fluidRow(
          bs4Card(
            width = 4,
            title = "Controles",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 6,
                div(
                  selectInput(
                    inputId = "opcionBarrasImpunidad",
                    label = "Seleccione un indicador:",
                    choices = opciones_impunidad
                  ),
                  style = "padding-left:5px;"
                )
              ),
              column(
                width = 6,
                div(
                  selectInput(
                    inputId = "anioBarrasImpunidad",
                    label = "Seleccione un año:",
                    choices = c("2020", "2021", "2022", "2023", "2024"),
                    selected = "2023"
                  ),
                  style = "padding-left:5px;"
                )
              )
            )
          ) #,
          # bs4Card(
          #   width = 8,
          #   title = "Gráfica de barras",
          #   solidHeader = TRUE,
          #   collapsible = TRUE #,
          #   #plotlyOutput("grafica_barras_impunidad", height = "80vh") %>% withSpinner()
          # )
        ),
        br(),
        hr(),
        # fluidRow(
        #   bs4Card(
        #     width = 12,
        #     title = "Variables",
        #     solidHeader = TRUE,
        #     collapsible = TRUE,
        #     div(
        #       h4("Variables"),
        #       br(),
        #       txt_impunidad_variables,
        #       style = "margin-top:40px;"
        #     )
        #   )
        # ),
        hr(),
        fluidRow(
          # bs4Card(
          #   width = 12,
          #   title = "Tabla",
          #   solidHeader = TRUE,
          #   collapsible = TRUE,
          #   div(
          #     #reactableOutput("tablaImpunidad"),
          #     style = "margin-top:30px;"
          #   )
          # )
        ) #,
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
  # Conecta aquí tus renders existentes:
  # output$grafica_barras_impunidad <- renderPlotly({ ... })
  # output$tablaImpunidad <- renderReactable({ ... })
  # output$mapa_impunidad <- renderLeaflet({ ... })  # si decides mostrarlo
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
