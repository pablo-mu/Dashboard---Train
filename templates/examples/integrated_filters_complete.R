# =============================================================================
# COMPLETE EXAMPLE: GRAPHS WITH INTEGRATED FILTERS
# =============================================================================
# Este archivo muestra las 3 técnicas principales para integrar filtros en gráficos

# LIBRARIES (agregar a app.R si no están)
# library(shiny)
# library(shinydashboard) 
# library(DT)

# =============================================================================
# TÉCNICA 1: FILTROS FLOTANTES CON absolutePanel()
# =============================================================================

ui_example_floating <- fluidPage(
  titlePanel("Gráficos con Filtros Integrados - Ejemplos"),
  
  fluidRow(
    # Ejemplo 1: Filtros flotantes esquina inferior izquierda
    column(width = 6,
           box(title = "Método 1: Filtros Flotantes", status = "primary", solidHeader = TRUE, width = NULL,
               div(style = "position: relative; height: 400px;",
                   plotOutput("plot_floating", height = "100%"),
                   
                   # Panel flotante
                   absolutePanel(
                     bottom = 20, left = 20,
                     style = "
                       background: rgba(248, 249, 250, 0.95); 
                       border: 1px solid #dee2e6; 
                       border-radius: 8px; 
                       padding: 12px; 
                       box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                       font-size: 12px;
                       min-width: 200px;
                       z-index: 1000;
                     ",
                     
                     # Título del panel
                     div(style = "font-weight: bold; margin-bottom: 10px; color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;",
                         "🎛️ Controles Rápidos"),
                     
                     # Controles
                     selectInput("data_type", "Tipo de Datos:",
                                choices = c("Ventas" = "sales", "Usuarios" = "users", "Ingresos" = "revenue"),
                                selected = "sales", width = "100%"),
                     
                     sliderInput("time_range", "Período (meses):",
                                min = 1, max = 12, value = 6, width = "100%"),
                     
                     div(style = "display: flex; justify-content: space-between; margin-top: 10px;",
                         checkboxInput("show_trend", "Tendencia", value = FALSE),
                         actionButton("reset_plot", "Reset", size = "sm", 
                                     style = "padding: 4px 12px; font-size: 11px;")
                     )
                   )
               )
           )
    ),
    
    # Ejemplo 2: Filtros en múltiples posiciones
    column(width = 6,
           box(title = "Método 1b: Múltiples Paneles", status = "info", solidHeader = TRUE, width = NULL,
               div(style = "position: relative; height: 400px;",
                   plotOutput("plot_multi_panels", height = "100%"),
                   
                   # Panel superior derecho
                   absolutePanel(
                     top = 20, right = 20,
                     style = "background: rgba(255,255,255,0.9); padding: 8px; border-radius: 5px; border: 1px solid #ccc;",
                     selectInput("chart_type", NULL,
                                choices = c("Línea" = "line", "Barras" = "bar", "Puntos" = "points"),
                                selected = "line", width = "100px")
                   ),
                   
                   # Panel inferior derecho  
                   absolutePanel(
                     bottom = 20, right = 20,
                     style = "background: rgba(255,255,255,0.9); padding: 8px; border-radius: 5px; border: 1px solid #ccc;",
                     numericInput("multiplier", "Factor:", value = 1, min = 0.1, max = 5, step = 0.1, width = "80px")
                   )
               )
           )
    )
  ),
  
  # =============================================================================
  # TÉCNICA 2: FILTROS EN HEADER DEL BOX
  # =============================================================================
  
  fluidRow(
    column(width = 12,
           box(
             title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                         span("Método 2: Filtros en Header", style = "font-weight: bold;"),
                         div(style = "display: flex; gap: 15px; align-items: center;",
                             selectInput("header_category", "Categoría:",
                                        choices = c("A", "B", "C"), selected = "A", width = "100px"),
                             dateRangeInput("header_dates", "Período:",
                                           start = Sys.Date() - 30, end = Sys.Date(), width = "200px"),
                             actionButton("header_update", "Actualizar", 
                                         style = "background: #28a745; color: white; border: none; padding: 6px 12px;")
                         )
             ),
             status = "success", solidHeader = TRUE, width = NULL,
             plotOutput("plot_header_controls", height = "300px")
           )
    )
  ),
  
  # =============================================================================
  # TÉCNICA 3: PANEL LATERAL CON CONTROLES
  # =============================================================================
  
  fluidRow(
    column(width = 12,
           box(title = "Método 3: Panel de Control Lateral", status = "warning", solidHeader = TRUE, width = NULL,
               fluidRow(
                 # Panel de controles lateral
                 column(width = 3,
                        div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; height: 400px; overflow-y: auto;",
                            h4("🔧 Panel de Control", style = "margin-top: 0; color: #495057;"),
                            
                            # Sección Datos
                            h5("📊 Datos", style = "color: #6c757d; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                            selectInput("lateral_dataset", "Dataset:",
                                       choices = c("Dataset 1" = "ds1", "Dataset 2" = "ds2", "Dataset 3" = "ds3")),
                            
                            # Sección Visualización  
                            h5("🎨 Visualización", style = "color: #6c757d; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                            radioButtons("lateral_plot_type", "Tipo de Gráfico:",
                                        choices = c("Líneas" = "line", "Área" = "area", "Barras" = "bar"),
                                        selected = "line"),
                            
                            colourInput("lateral_color", "Color:", value = "#337ab7"),
                            
                            # Sección Filtros
                            h5("🔍 Filtros", style = "color: #6c757d; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                            sliderInput("lateral_threshold", "Umbral:",
                                       min = 0, max = 100, value = 50),
                            
                            checkboxGroupInput("lateral_categories", "Categorías:",
                                              choices = c("Cat A" = "a", "Cat B" = "b", "Cat C" = "c"),
                                              selected = c("a", "b")),
                            
                            # Botones de acción
                            div(style = "margin-top: 20px;",
                                actionButton("lateral_apply", "Aplicar Filtros", 
                                           style = "width: 100%; background: #007bff; color: white; border: none; margin-bottom: 10px;"),
                                actionButton("lateral_reset", "Resetear Todo",
                                           style = "width: 100%; background: #6c757d; color: white; border: none;")
                            )
                        )
                 ),
                 
                 # Área del gráfico
                 column(width = 9,
                        div(style = "padding: 10px;",
                            plotOutput("plot_lateral_controls", height = "380px")
                        )
                 )
               )
           )
    )
  )
)

# =============================================================================
# SERVER PARA LOS EJEMPLOS
# =============================================================================

server_example <- function(input, output, session) {
  
  # Gráfico con filtros flotantes
  output$plot_floating <- renderPlot({
    data_type <- input$data_type %||% "sales"
    months <- input$time_range %||% 6
    show_trend <- input$show_trend %||% FALSE
    
    # Generar datos según el tipo
    x <- 1:months
    y <- switch(data_type,
      "sales" = cumsum(rnorm(months, 100, 20)),
      "users" = cumsum(rnorm(months, 50, 10)), 
      "revenue" = cumsum(rnorm(months, 1000, 200))
    )
    
    plot(x, y, type = "b", pch = 16, lwd = 2, col = "steelblue",
         main = paste("Datos de", switch(data_type, "sales" = "Ventas", "users" = "Usuarios", "revenue" = "Ingresos")),
         xlab = "Meses", ylab = "Valor")
    
    if(show_trend) {
      abline(lm(y ~ x), col = "red", lty = 2, lwd = 2)
    }
    
    grid(col = "lightgray", lty = 3)
  })
  
  # Gráfico con múltiples paneles
  output$plot_multi_panels <- renderPlot({
    chart_type <- input$chart_type %||% "line"
    multiplier <- input$multiplier %||% 1
    
    x <- 1:10
    y <- sin(x) * multiplier * 10
    
    if(chart_type == "line") {
      plot(x, y, type = "l", lwd = 3, col = "darkblue", main = "Gráfico de Línea")
    } else if(chart_type == "bar") {
      barplot(y, names.arg = x, col = "lightblue", main = "Gráfico de Barras")
    } else {
      plot(x, y, pch = 16, cex = 2, col = "red", main = "Gráfico de Puntos")
    }
  })
  
  # Gráfico con controles en header
  output$plot_header_controls <- renderPlot({
    category <- input$header_category %||% "A"
    
    data <- switch(category,
      "A" = c(10, 15, 12, 18, 20, 17, 22),
      "B" = c(8, 12, 10, 14, 16, 13, 18), 
      "C" = c(15, 20, 18, 25, 28, 23, 30)
    )
    
    barplot(data, names.arg = 1:7, col = rainbow(7),
            main = paste("Datos para Categoría", category),
            ylab = "Valores")
  })
  
  # Gráfico con panel lateral
  output$plot_lateral_controls <- renderPlot({
    dataset <- input$lateral_dataset %||% "ds1"
    plot_type <- input$lateral_plot_type %||% "line"
    color <- input$lateral_color %||% "#337ab7"
    threshold <- input$lateral_threshold %||% 50
    
    # Generar datos según dataset
    x <- 1:20
    y <- switch(dataset,
      "ds1" = x^2 + rnorm(20, 0, 10),
      "ds2" = 100 * sin(x/3) + rnorm(20, 0, 5),
      "ds3" = cumsum(rnorm(20, 5, 3))
    )
    
    # Crear gráfico según tipo
    if(plot_type == "line") {
      plot(x, y, type = "l", lwd = 3, col = color, main = paste("Dataset:", dataset))
    } else if(plot_type == "area") {
      plot(x, y, type = "n", main = paste("Dataset:", dataset))
      polygon(c(x, rev(x)), c(y, rep(0, length(y))), col = paste0(color, "80"), border = color)
    } else {
      barplot(y, names.arg = x, col = color, main = paste("Dataset:", dataset))
    }
    
    # Agregar línea de umbral
    abline(h = threshold, col = "red", lty = 2, lwd = 2)
    text(max(x) * 0.8, threshold + max(y) * 0.05, paste("Umbral:", threshold), col = "red")
  })
  
  # Observadores para botones
  observeEvent(input$reset_plot, {
    updateSelectInput(session, "data_type", selected = "sales")
    updateSliderInput(session, "time_range", value = 6)
    updateCheckboxInput(session, "show_trend", value = FALSE)
  })
  
  observeEvent(input$lateral_reset, {
    updateSelectInput(session, "lateral_dataset", selected = "ds1")
    updateRadioButtons(session, "lateral_plot_type", selected = "line")
    updateSliderInput(session, "lateral_threshold", value = 50)
  })
}

# Para ejecutar el ejemplo completo:
# shinyApp(ui_example_floating, server_example)