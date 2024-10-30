library(shiny)
library(jsonlite)
library(ggplot2)
library(scales)
# Definimos la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Dashboard de Calidad de Datos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargar archivo JSON", accept = c(".json")),
      hr(),
      h4("Información general"),
      verbatimTextOutput("summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Valores Nulos", 
          plotOutput("nullPlot"),
          hr(),
          h4("Porcentajes de Nulos por Columna"),
          fluidRow(
            column(4, actionButton("btn_age", "Porcentaje Nulos en Age")),
            column(4, actionButton("btn_country", "Porcentaje Nulos en Country")),
            column(4, actionButton("btn_email", "Porcentaje Nulos en Email"))
          ),
          fluidRow(
            column(4, actionButton("btn_id", "Porcentaje Nulos en ID")),
            column(4, actionButton("btn_last_login", "Porcentaje Nulos en Last Login")),
            column(4, actionButton("btn_name", "Porcentaje Nulos en Name"))
          ),
          hr(),
          verbatimTextOutput("null_info")
        ),
        tabPanel("Distribución de Estados", plotOutput("statusPlot")),
        tabPanel(
          "Puntajes de Calidad", 
          plotOutput("scoresPlot"),
          hr(),
          h4("Descripción de Conceptos de Calidad de Datos"),
          p("🔍 **Accuracy** (Precisión): La precisión refleja qué tan correcta es la información en comparación con el valor real o esperado."),
          p("📋 **Completeness** (Integridad): Indica qué tan completa está la información. Registros o celdas faltantes pueden afectar su uso."),
          p("🔄 **Consistency** (Consistencia): Evalúa si los datos se mantienen coherentes en distintos campos o bases de datos."),
          p("🔑 **Uniqueness** (Unicidad): Asegura que no haya duplicados en campos que deberían ser únicos, como identificadores o correos.")
        )
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  # Leer y procesar el archivo JSON cargado
  data <- reactive({
    req(input$file)
    fromJSON(input$file$datapath)
  })
  
  # Variable reactiva para la columna seleccionada
  columna_seleccionada <- reactiveVal("age")  # Valor por defecto
  
  # Observadores para cambiar la columna seleccionada al hacer clic en los botones
  observeEvent(input$btn_age, { columna_seleccionada("age") })
  observeEvent(input$btn_country, { columna_seleccionada("country") })
  observeEvent(input$btn_email, { columna_seleccionada("email") })
  observeEvent(input$btn_id, { columna_seleccionada("id") })
  observeEvent(input$btn_last_login, { columna_seleccionada("last_login") })
  observeEvent(input$btn_name, { columna_seleccionada("name") })
  
  # Mostrar porcentaje de nulos y actualizar el gráfico de dona dinámicamente
  output$nullPlot <- renderPlot({
    json_data <- data()
    col_name <- columna_seleccionada()
    null_counts <- json_data$quality_checks$completeness$metrics$null_counts_by_column
    total_rows <- json_data$metadata$total_rows
    
    # Calcular la cantidad de nulos y no nulos para la columna seleccionada
    nulos <- null_counts[[col_name]]
    no_nulos <- total_rows - nulos
    
    # Crear el data frame para el gráfico de dona
    df_donut <- data.frame(
      Categoria = c("Nulos", "No Nulos"),
      Cantidad = c(nulos, no_nulos)
    )
    
    # Calcular los porcentajes
    df_donut$Porcentaje <- round((df_donut$Cantidad / sum(df_donut$Cantidad)) * 100, 2)
    
    # Crear el gráfico de dona
    ggplot(df_donut, aes(x = 2, y = Cantidad, fill = Categoria)) +
      geom_bar(stat = "identity", width = 0.5) +
      coord_polar("y", start = 0) +
      xlim(1.5, 2.5) +  # Crear efecto de anillo
      geom_text(aes(label = paste0(Porcentaje, "%")), 
                position = position_stack(vjust = 0.5), size = 6, color = "white") +
      scale_fill_manual(values = c("Nulos" = "red", "No Nulos" = "steelblue")) +
      theme_void() +
      labs(title = paste("Anillo: Valores Nulos en", col_name))
  })
  
  # Mostrar información del porcentaje de nulos en texto
  output$null_info <- renderPrint({
    json_data <- data()
    col_name <- columna_seleccionada()
    null_counts <- json_data$quality_checks$completeness$metrics$null_counts_by_column
    total_rows <- json_data$metadata$total_rows
    
    porcentaje <- (null_counts[[col_name]] / total_rows) * 100
    paste0("Porcentaje de datos nulos en '", col_name, "': ", round(porcentaje, 2), "%")
  })
  
  # Gráfico de distribución de estados (status)
  output$statusPlot <- renderPlot({
    json_data <- data()
    status_dist <- json_data$quality_checks$consistency$metrics$status$value_distribution
    df_status <- data.frame(Estado = names(status_dist), Frecuencia = unlist(status_dist))
    df_status$Porcentaje <- round((df_status$Frecuencia / sum(df_status$Frecuencia)) * 100, 2)
    
    ggplot(df_status, aes(x = Estado, y = Frecuencia, fill = Estado)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Distribución de Estados", x = "Estado", y = "Frecuencia")
  })
  
  # Gráfico de puntajes por categoría de calidad
  output$scoresPlot <- renderPlot({
    json_data <- data()
    scores <- json_data$overall_quality$category_scores
    df_scores <- data.frame(Categoria = names(scores), Puntaje = unlist(scores))
    
    ggplot(df_scores, aes(x = Categoria, y = Puntaje)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = scales::percent(Puntaje)), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Puntajes por Categoría de Calidad", x = "Categoría", y = "Puntaje")
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
