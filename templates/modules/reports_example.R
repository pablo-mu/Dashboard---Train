# =============================================================================
# EJEMPLO: NUEVO PANEL - REPORTES
# =============================================================================
# Este archivo muestra cómo agregar un nuevo panel al dashboard

create_reports_content <- function() {
  tagList(
    fluidRow(
      introBox(
        box(title = "Reportes Mensuales", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("monthly_reports_plot"),
            br(),
            "Gráfico de reportes generados mensualmente"
        ),
        box(title = "Exportaciones", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("exports_plot"),
            br(),
            "Análisis de datos exportados por formato"
        ),
        data.step = 3, data.intro = "Panel de reportes y exportaciones."
      )
    ),
    fluidRow(
      introBox(
        box(title = "Usuarios Activos", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("active_users_table"),
            br(),
            "Lista de usuarios activos en el sistema"
        ),
        data.step = 4, data.intro = "Datos de usuarios del sistema."
      )
    )
  )
}

create_reports_filters <- function() {
  list(
    h4("Filtros para Reportes"),
    selectInput("report_type", "Tipo de Reporte:", 
               choices = c("Todos", "Mensual", "Trimestral", "Anual")),
    dateRangeInput("report_period", "Período de Reporte:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    checkboxGroupInput("export_formats", "Formatos de Exportación:", 
                      choices = c("PDF", "Excel", "CSV"), 
                      selected = c("PDF", "Excel", "CSV"))
  )
}

# Ejemplo de gráficos para el nuevo panel
create_reports_plots <- function(output) {
  
  output$monthly_reports_plot <- renderPlot({
    months <- month.abb[1:12]
    reports <- c(45, 52, 38, 61, 55, 48, 67, 59, 44, 53, 49, 58)
    barplot(reports, names.arg = months,
            main = "Reportes Generados por Mes",
            ylab = "Número de Reportes", 
            col = "steelblue",
            las = 2)
  })
  
  output$exports_plot <- renderPlot({
    formats <- c("PDF", "Excel", "CSV", "JSON")
    counts <- c(120, 85, 65, 30)
    pie(counts, labels = paste(formats, " (", counts, ")", sep=""),
        main = "Exportaciones por Formato",
        col = c("lightcoral", "lightgreen", "lightblue", "lightyellow"))
  })
  
  output$active_users_table <- DT::renderDataTable({
    data.frame(
      Usuario = c("admin", "doctor1", "nurse2", "analyst3", "manager4"),
      Último_Acceso = c("2025-11-05 14:30", "2025-11-05 13:45", "2025-11-05 15:20", 
                       "2025-11-05 12:10", "2025-11-05 16:05"),
      Sesiones_Mes = c(45, 32, 28, 22, 18),
      Rol = c("Administrador", "Médico", "Enfermero", "Analista", "Gerente")
    )
  }, options = list(pageLength = 10))
}

# INSTRUCCIONES PARA IMPLEMENTAR:
# 
# 1. Agregar a config.R:
# reports = list(
#   name = "REPORTS",
#   label = "REPORTS", 
#   icon = "file-alt",
#   button_style = "info",
#   button_label = "FILTROS REPORTES"
# )
#
# 2. Agregar observeEvent en dynamic_server.R:
# observeEvent(input$reports, { active_panel("reports") })
#
# 3. Agregar casos switch en dynamic_server.R:
# "reports" = create_reports_content(),
# "reports" = create_reports_filters(),
#
# 4. Agregar botón en main_buttons.R:
# bsButton("reports", label = panel_definitions$reports$name, 
#          icon = icon(panel_definitions$reports$icon), style = "success"),
#
# 5. Cargar módulo en app.R:
# source("templates/modules/reports.R")
#
# 6. Agregar gráficos en plots_data.R o crear función separada