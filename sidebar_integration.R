# =============================================================================
# INTEGRACIÓN SIDEBAR CON MÓDULOS
# =============================================================================
# Este archivo muestra cómo integrar el sidebar con los módulos existentes

# Para usar en tu app.R principal, agrega esto al server:

sidebar_integration_server <- function(input, output, session, datos_raw) {
  
  # Cargar las funciones de filtros
  source("sankey/utils_filters.R", local = TRUE)
  
  # Crear los filtros dinámicos del sidebar
  output$centro_gestor_filter <- crear_filtro_centro_gestor(datos_raw, "centro_gestor")
  output$origen_filter <- renderUI({
    crear_filtro_origen(datos_raw, input$nivel_agregacion, input$centro_gestor, "origen")()
  })
  output$mes_filter <- crear_filtro_mes(datos_raw, "mes")
  output$anyo_filter <- crear_filtro_anyo(datos_raw, "anyo")
  
  # Botón dinámico de filtros (personalizable según necesidades)
  output$dynamic_filter_btn <- renderUI({
    actionButton("toggle_filters", 
                 "Filtros Avanzados", 
                 icon = icon("filter"),
                 class = "btn-info",
                 style = "width: 100%;")
  })
  
  # Lógica de reseteo de filtros
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "centro_gestor", selected = "")
    updateSelectInput(session, "origen", selected = "")
    updateSelectInput(session, "mes", selected = "")
    updateSelectInput(session, "anyo", selected = "")
    updateRadioButtons(session, "nivel_agregacion", selected = "cac")
    updateCheckboxGroupInput(session, "fases_filter", selected = c("1", "2", "3"))
    updateCheckboxInput(session, "mostrar_parametro", value = FALSE)
    updateCheckboxInput(session, "mostrar_estaticos", value = TRUE)
    
    showNotification("Filtros reseteados", type = "message", duration = 3)
  })
  
  # Validaciones antes de generar
  observeEvent(input$generar_sankey, {
    if (is.null(input$centro_gestor) || input$centro_gestor == "") {
      showNotification("Por favor, selecciona un Centro Gestor", type = "error", duration = 5)
      return()
    }
    
    if (is.null(input$fases_filter) || length(input$fases_filter) == 0) {
      showNotification("Por favor, selecciona al menos una fase", type = "error", duration = 5)
      return()
    }
    
    showNotification("Generando diagrama con los filtros seleccionados...", type = "message", duration = 3)
  })
  
  # Retornar los valores de los filtros para usar en otros módulos
  return(list(
    centro_gestor = reactive(input$centro_gestor),
    nivel_agregacion = reactive(input$nivel_agregacion),
    origen = reactive(input$origen),
    mes = reactive(input$mes),
    anyo = reactive(input$anyo),
    fases_filter = reactive(input$fases_filter),
    mostrar_parametro = reactive(input$mostrar_parametro),
    mostrar_estaticos = reactive(input$mostrar_estaticos)
  ))
}

# En tu app.R principal:
# 
# server <- function(input, output, session) {
#   # Cargar datos
#   datos_raw <- reactive({ ... })
#   
#   # Inicializar sidebar
#   sidebar_filters <- sidebar_integration_server(input, output, session, datos_raw)
#   
#   # Pasar los filtros al módulo Sankey
#   sankeyServer("sankey", datos_raw, reactive(input$sidebar_tab))
# }