# Paquetes requeridos
library(shiny)
library(shinydashboard)

# Cargar todos los módulos en R/
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "DisExp"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Regresión Lineal", tabName = "regresion", icon = icon("chart-line")),
      menuItem("Otros Métodos", tabName = "otros_metodos", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Pestaña de inicio
      tabItem(
        tabName = "home",
        h2("Bienvenido al Analizador de Métodos Estadísticos"),
        p("Seleccione un método del menú lateral para comenzar."),
        h4("Métodos disponibles:"),
        tags$ul(
          tags$li("Regresión Lineal: Análisis de relación entre dos variables cuantitativas"),
          tags$li("Próximamente: Más métodos estadísticos")
        )
      ),
      
      # Pestaña de Regresión Lineal
      tabItem(
        tabName = "regresion",
        mod_regresion_ui("regresion_1")
      ),
      
      # Pestaña para futuros métodos
      tabItem(
        tabName = "otros_metodos",
        h3("Próximamente"),
        p("Se agregarán más métodos estadísticos en futuras actualizaciones.")
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  # Inicializar módulo de regresión
  mod_regresion_server("regresion_1")
}

# Ejecutar la aplicación 
shinyApp(ui = ui, server = server)