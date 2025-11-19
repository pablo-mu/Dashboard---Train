create_dynamic_server <- function(input, output,
  session, panel_definitions, default_panel
){
  # Cargar dependencias necesarias
  library(data.table)
  library(dplyr)
  library(shiny)
  library(shinyBS) # Para bsButton
  library(shinydashboard) # Para box, fluidRow, etc.
  library(shinyjs) # Para funciones adicionales de UI
  library(rintrojs) # Para introBox

  source("modules/lib_reparto.R")
  #source("server/data_server.R")
  source("server/filters_server.R")
  
  # Cargar módulos de Sankey
  source("modules/sankey/sankey_ui.R")
  source("modules/sankey/sankey_server.R")
  source("modules/matriz/matriz_server.R")
  source("modules/matriz/matriz_ui.R")


  
  # Crear sistema centralizado de datos
  # UI de estado de carga para el sidebar



  # Aseguramos un panel por defecto válido
  if (is.null(default_panel) || !default_panel %in% names(panel_definitions)) {
    default_panel <- names(panel_definitions)[1]
  }
  active_panel <- shiny::reactiveVal(default_panel)
  
  # Observadores para cambiar de panel cuando se hace click en los botones del navbar
  shiny::observeEvent(input$Sankey, {
    active_panel("sankey")
  })
  
  shiny::observeEvent(input$`Matriz Costes`, {
    active_panel("matriz_costes")
  })
  
  shiny::observeEvent(input$Diagnostics, {
    active_panel("diagnostics")
  })
  
  # Estado de carga de datos
  datos_loaded <- shiny::reactiveVal(FALSE)
  loading_data <- shiny::reactiveVal(FALSE)

  # Render del botón dinámico (carga datos o filtros según estado)
  output$dynamic_action_btn <- shiny::renderUI({
    if (!datos_loaded()) {
      # Mostrar botón de cargar datos
      if (loading_data()) {
        # Estado de carga
        div(
          bsButton("load_data", "Cargando...", 
                  icon = icon("spinner", class = "fa-spin"),
                  style = "info", disabled = TRUE),
          style = "text-align: center;"
        )
      } else {
        # Estado inicial - botón para cargar datos
        bsButton("load_data", "CARGAR DATOS",
                icon = icon("database"),
                style = "danger")
      }
    } else {
      # Datos cargados - mostrar botón de filtros
      panel <- active_panel()
      panel_config <- panel_definitions[[panel]]

      if (!is.null(panel_config)) {
        bsButton("show_filters",
                panel_config$button_label,
                icon = icon(panel_config$icon),
                style = "danger")
      } else {
        bsButton("show_filters", "FILTROS", icon = icon("filter"), style = "danger")
      }
    }
  })

  # <<< NUEVO: no suspender estas salidas si están ocultas >>>
  outputOptions(output, "dynamic_action_btn", suspendWhenHidden = FALSE)

  # Variable reactiva para almacenar los datos
  datos_raw <- shiny::reactiveVal(NULL)

  # Observer para manejar la carga de datos cuando se presiona el botón
  shiny::observeEvent(input$load_data, {
    cat("Botón de cargar datos presionado\n")  # Debug
    
    # Actualizar estado de carga
    loading_data(TRUE)
    
    # Simular un pequeño delay para mostrar el spinner
    shiny::invalidateLater(500, session)
    
    tryCatch({
      cat("Iniciando carga de datos...\n")  # Debug en consola
      
      datos <- preparar_datos_reparto()
      
      if (!is.null(datos)) {
        cat("Datos cargados exitosamente:", nrow(datos), "filas\n")  # Debug
        
        datos_dt <- datos %>% as.data.table()
        
        # Actualizar los datos y estado
        datos_raw(datos)
        datos_loaded(TRUE)
        loading_data(FALSE)
        
        showNotification(
          paste("✓ Datos cargados:", formatC(nrow(datos_dt), format = "d", big.mark = " "), "registros únicos"),
          type = "message",
          duration = 5
        )
      } else {
        cat("Error: preparar_datos_reparto() devolvió NULL\n")  # Debug
        loading_data(FALSE)
        showNotification(
          paste("No se encontraron archivos CSV. Directorio:", getwd()),
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      cat("Error en carga de datos:", e$message, "\n")  # Debug
      loading_data(FALSE)
      showNotification(
        paste("Error al cargar datos:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Crear sistema de filtros dinámicos solo cuando los datos están disponibles
  filters <- NULL
  
  # Observer para inicializar filtros cuando los datos estén cargados
  observe({
    if (datos_loaded() && !is.null(datos_raw()) && is.null(filters)) {
      # Crear reactive wrapper para compatibilidad con filters_server
      datos_reactive <- reactive({ datos_raw() })
      filters <<- create_filters_server(input, output, session, datos_reactive)
    }
  })
  
  # Inicializar servidor de Sankey
  sankey_data <- create_sankey_server(input, output, session, datos_raw, active_panel)

  # Inicializar servidor de Matriz de Costes
  matriz_data <- create_matriz_server(input, output, session, datos_raw, active_panel)
  

  
  # Renderizar contenido de cada panel
  output$sankey_content <- shiny::renderUI({
    create_sankey_content()
  })
  
  output$matriz_costes_content <- shiny::renderUI({
    create_matriz_content()
  })
  
  output$diagnostics_content <- shiny::renderUI({
    create_diagnostics_content()
  })
  
  # Controlar visibilidad de paneles según active_panel
  observe({
    panel <- active_panel()
    
    # Ocultar todos los paneles
    shinyjs::hide("sankey_panel")
    shinyjs::hide("matriz_costes_panel")
    shinyjs::hide("diagnostics_panel")
    
    # Mostrar solo el panel activo
    if (panel == "sankey") {
      shinyjs::show("sankey_panel")
    } else if (panel == "matriz_costes") {
      shinyjs::show("matriz_costes_panel")
    } else if (panel == "diagnostics") {
      shinyjs::show("diagnostics_panel")
    }
  })

  # Modal para generar diagrama y aplicar filtros propios de cada panel
  shiny::observeEvent(input$show_filters, {
    # Solo mostrar modal si los datos están cargados
    if (!datos_loaded()) {
      showNotification(
        "Primero debes cargar los datos antes de configurar los filtros.",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    panel <- active_panel()

    modal_content <- switch(panel,
      "sankey" = create_sankey_filters(),
      "matriz_costes" = create_matriz_filters(),
      "diagnostics" = create_diagnostics_filters(),
      list(
        h4("Filtros no disponibles"),
        p("No hay filtros definidos para este panel.")
      )
    )

    showModal(modalDialog(
      title = NULL,
      div(
        style = "padding: 20px;",
        h3(paste("Configuración -", 
                 switch(panel,
                        "sankey" = "Diagrama Sankey",
                        "matriz_costes" = "Matriz de Costes",
                        "diagnostics" = "Diagnósticos",
                        toupper(panel))),
           style = "margin-top: 0; margin-bottom: 20px; color: #2c3e50; font-weight: 600;"),
        modal_content
      ),
      easyClose = TRUE,
      size = "l",
      footer = div(
        style = "display: flex; justify-content: flex-end; gap: 10px; padding: 15px 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
        actionButton("modal_cancel", "Cancelar", 
                    icon = icon("times"),
                    class = "btn btn-primary",
                    style = "padding: 10px 25px; font-size: 14px; color: white;"),
        actionButton("apply_filters", "Generar Diagrama", 
                    icon = icon("chart-line"),
                    class = "btn btn-primary",
                    style = "background-color: #c8102e; border: none; padding: 10px 30px; font-size: 14px; font-weight: 600;",
                    onclick = "Shiny.setInputValue('apply_filters', Math.random());")
      )
    ))
  })
  
  # Cerrar modal al presionar cancelar
  observeEvent(input$modal_cancel, {
    removeModal()
  })

  observeEvent(input$apply_filters, {
    removeModal()
  })
  
  # Retornar datos para su uso en otros módulos
  return(list(
    active_panel = active_panel,
    datos_raw = datos_raw,
    filters = filters,
    datos_loaded = datos_loaded
  ))
}

# ============================================================================
# FUNCIONES AUXILIARES PARA CONTENIDOS DE PANELES
# ============================================================================
# Note: create_sankey_content is now in modules/sankey/sankey_ui.R

#' Crear contenido del panel Matriz de Costes  
create_matriz_costes_content <- function() {
  tagList(
    fluidRow(
      box(title = "Matriz de Costes", status = "primary", solidHeader = TRUE, width = 12,
          h4("Matriz detallada de costes"),
          p("Configure los filtros y presione 'Generar' para ver la matriz de costes.")
      )
    )
  )
}

#' Crear contenido del panel Diagnostics
create_diagnostics_content <- function() {
  tagList(
    fluidRow(
      box(title = "Diagnósticos", status = "primary", solidHeader = TRUE, width = 12,
          h4("Análisis y diagnósticos"),
          p("Configure los filtros y presione 'Generar' para ver los diagnósticos.")
      )
    )
  )
}

# ============================================================================
# FUNCIONES AUXILIARES PARA FILTROS ESPECÍFICOS DE PANELES
# ============================================================================
# Note: create_sankey_filters is now in modules/sankey/sankey_filters.R

#' Crear filtros específicos del panel Matriz de Costes
create_matriz_costes_filters <- function() {
  tagList(
    h4("Configuración Matriz"),
    p("Filtros específicos para la matriz de costes."),
    checkboxInput("matriz_heatmap", "Vista Heatmap", value = FALSE),
    selectInput("matriz_format", "Formato números:", 
               choices = c("Miles" = "thousands", "Millones" = "millions"),
               selected = "thousands")
  )
}

#' Crear filtros específicos del panel Diagnostics
create_diagnostics_filters <- function() {
  tagList(
    h4("Configuración Diagnósticos"),
    p("Filtros específicos para análisis y diagnósticos."),
    checkboxInput("diag_detailed", "Análisis detallado", value = TRUE),
    selectInput("diag_type", "Tipo de análisis:", 
               choices = c("Básico" = "basic", "Avanzado" = "advanced"),
               selected = "basic")
  )
}