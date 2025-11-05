# =============================================================================
# ADVANCED PLOTS WITH INTEGRATED FILTERS
# =============================================================================
# Ejemplo de cómo agregar filtros integrados en los gráficos

create_advanced_antimicrobials_content <- function() {
  tagList(
    fluidRow(
      introBox(
        # Método 1: Filtros flotantes usando absolutePanel
        box(title = "Antimicrobianos - Con Filtros Flotantes", status = "primary", solidHeader = TRUE, width = 6,
            div(style = "position: relative; height: 350px;",
                plotOutput("antimicrobials_plot_advanced", height = "100%"),
                
                # Panel flotante en esquina inferior izquierda
                absolutePanel(
                  bottom = 15, left = 15,
                  style = "background: rgba(248, 248, 248, 0.95); 
                           border: 1px solid #ddd; 
                           border-radius: 8px; 
                           padding: 10px; 
                           box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                           font-size: 11px;
                           z-index: 1000;",
                  
                  div(style = "margin-bottom: 8px; font-weight: bold; color: #337ab7;", "Filtros Rápidos"),
                  
                  selectInput("ab_quick_type", NULL,
                             choices = c("Todos" = "all", "Penicilinas" = "pen", "Cefalosporinas" = "cef"),
                             selected = "all",
                             width = "140px"),
                  
                  div(style = "display: flex; align-items: center; margin-top: 5px;",
                      checkboxInput("ab_show_trend_line", "Tendencia", value = FALSE, width = "80px"),
                      actionButton("ab_reset", "⟲", style = "padding: 2px 8px; font-size: 12px;", title = "Reset")
                  )
                )
            )
        ),
        
        # Método 2: Filtros en el header del box
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                        span("Duración del Tratamiento"),
                        div(style = "display: flex; gap: 10px; align-items: center;",
                            selectInput("duration_filter_type", NULL,
                                       choices = c("Días", "Semanas", "Meses"),
                                       selected = "Días", width = "80px"),
                            numericInput("duration_threshold", NULL, value = 7, min = 1, max = 30, width = "60px")
                        )
                    ),
            status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("duration_plot_advanced", height = "300px"),
            br(),
            "Duración con filtros integrados en el header"
        ),
        data.step = 3, data.intro = "Gráficos con filtros integrados avanzados."
      )
    ),
    
    fluidRow(
      introBox(
        # Método 3: Filtros en sidebar lateral del gráfico
        box(title = "Resistencia - Con Panel Lateral", status = "warning", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 3,
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; height: 300px;",
                         h5("Controles", style = "margin-top: 0; color: #495057;"),
                         
                         selectInput("resistance_ab", "Antimicrobiano:",
                                    choices = c("Todos", "Ampicilina", "Ciprofloxacino", "Vancomicina"),
                                    selected = "Todos"),
                         
                         sliderInput("resistance_threshold", "Umbral de Resistencia (%):",
                                    min = 0, max = 100, value = 20, step = 5),
                         
                         checkboxGroupInput("resistance_types", "Mostrar:",
                                           choices = c("Sensible" = "sens", "Intermedio" = "int", "Resistente" = "res"),
                                           selected = c("sens", "res")),
                         
                         div(style = "margin-top: 15px;",
                             actionButton("update_resistance", "Actualizar", 
                                         style = "width: 100%; background: #17a2b8; color: white; border: none;"))
                     )
              ),
              column(width = 9,
                     plotOutput("resistance_plot_advanced", height = "300px")
              )
            )
        ),
        data.step = 4, data.intro = "Gráfico con panel de control lateral."
      )
    )
  )
}

# Función auxiliar para crear filtros compactos flotantes
create_floating_filter_panel <- function(inputId_prefix, position = "bottom-left") {
  
  position_style <- switch(position,
    "bottom-left" = "bottom: 10px; left: 10px;",
    "bottom-right" = "bottom: 10px; right: 10px;", 
    "top-left" = "top: 10px; left: 10px;",
    "top-right" = "top: 10px; right: 10px;"
  )
  
  absolutePanel(
    style = paste0(position_style, 
                   "background: rgba(255,255,255,0.95); 
                    border: 1px solid #ccc; 
                    border-radius: 6px; 
                    padding: 8px; 
                    box-shadow: 0 2px 6px rgba(0,0,0,0.15);
                    font-size: 11px;
                    min-width: 120px;
                    z-index: 1000;"),
    
    # Encabezado del panel
    div(style = "font-weight: bold; margin-bottom: 8px; color: #333; border-bottom: 1px solid #eee; padding-bottom: 4px;",
        "⚙ Filtros"),
    
    # Controles dinámicos basados en el prefijo
    if(inputId_prefix == "antimicrobials") {
      tagList(
        selectInput(paste0(inputId_prefix, "_quick_filter"), "Tipo:",
                   choices = c("Todos", "Gram+", "Gram-"),
                   selected = "Todos", width = "100px"),
        div(style = "margin-top: 5px;",
            actionButton(paste0(inputId_prefix, "_export"), "📊", 
                        style = "padding: 2px 6px; font-size: 10px;", 
                        title = "Exportar"))
      )
    } else if(inputId_prefix == "patients") {
      tagList(
        sliderInput(paste0(inputId_prefix, "_age_range"), "Edad:",
                   min = 0, max = 100, value = c(0, 100),
                   width = "120px"),
        checkboxInput(paste0(inputId_prefix, "_show_gender"), "Por género", width = "100px")
      )
    }
  )
}