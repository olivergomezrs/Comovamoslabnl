library(shiny)
library(jsonlite)
library(ggplot2)

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
        tabPanel("Valores Nulos", plotOutput("nullPlot")),
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
  
  # Mostrar resumen de la calidad general
  output$summary <- renderPrint({
    json_data <- data()
    list(
      "Archivo" = json_data$metadata$filename,
      "Filas totales" = json_data$metadata$total_rows,
      "Columnas totales" = json_data$metadata$total_columns,
      "Puntaje General" = json_data$overall_quality$score,
      "Interpretación" = json_data$overall_quality$interpretation
    )
  })
  
  # Gráfico de valores nulos por columna
  output$nullPlot <- renderPlot({
    json_data <- data()
    null_counts <- json_data$quality_checks$completeness$metrics$null_counts_by_column
    df_nulls <- data.frame(Columna = names(null_counts), Nulos = unlist(null_counts))
    
    ggplot(df_nulls, aes(x = Columna, y = Nulos)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = Nulos), vjust = -0.5) +  # Etiquetas con los valores
      theme_minimal() +
      labs(title = "Valores Nulos por Columna", x = "Columna", y = "Cantidad de Nulos")
  })
  
  # Gráfico de distribución de estados (status)
  output$statusPlot <- renderPlot({
    json_data <- data()
    status_dist <- json_data$quality_checks$consistency$metrics$status$value_distribution
    df_status <- data.frame(Estado = names(status_dist), Frecuencia = unlist(status_dist))
    
    ggplot(df_status, aes(x = Estado, y = Frecuencia, fill = Estado)) +
      geom_bar(stat = "identity") +
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
      geom_text(aes(label = round(Puntaje, 3)), vjust = -0.5) +  # Etiquetas con puntajes
      theme_minimal() +
      labs(title = "Puntajes por Categoría de Calidad", x = "Categoría", y = "Puntaje")
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
