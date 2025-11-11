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


  
  # Crear sistema centralizado de datos
  # UI de estado de carga para el sidebar



  # Aseguramos un panel por defecto válido
  if (is.null(default_panel) || !default_panel %in% names(panel_definitions)) {
    default_panel <- names(panel_definitions)[1]
  }
  active_panel <- shiny::reactiveVal(default_panel)
  
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
  
  
  

  # Renderizamos el contenido principal según el panel activo
  output$dynamic_content <- shiny::renderUI({
    panel <- active_panel()

    tagList(
      switch(panel,
        "sankey" = create_sankey_content(),
        "matriz_costes" = create_matriz_costes_content(),
        "diagnostics" = create_diagnostics_content(),
        # Contenido por defecto
        tagList(
          fluidRow(
            introBox(
              box(title = "Panel General", status = "primary", solidHeader = TRUE, width = 12,
                  h3("Bienvenido al Dashboard Reparto Costes"),
                  p("Selecciona una sección usando los botones superiores para ver el contenido específico:"),
                  tags$ul(
                    tags$li(strong("SANKEY:"), " Diagrama de flujo de costes entre orígenes y destinos a lo largo de las fases."),
                    tags$li(strong("MATRIZ DE COSTES:"), "Matriz detallada de costes entre orígenes y destinos."),
                    tags$li(strong("DIAGNOSTICS:"), "Detalle y análisis de los repartos de costes.")
                  )
              ),
              data.step = 5, data.intro = "Panel principal del dashboard."
            )
          )
        )
      ),
      uiOutput("apply_filters_btn") # Agregamos el botón aquí
    )
  })

  # Renderizamos el botón de aplicar filtros directamente en la interfaz
  output$apply_filters_btn <- shiny::renderUI({
    actionButton("apply_filters", "Generar", class = "btn-primary")
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
      "matriz_costes" = create_matriz_costes_filters(),
      "diagnostics" = create_diagnostics_filters(),
      list(
        h4("Filtros no disponibles"),
        p("No hay filtros definidos para este panel.")
      )
    )

    showModal(modalDialog(
      title = paste("Filtros -", toupper(panel)),
      modal_content,
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton("Cerrar"),
        actionButton("apply_filters", "Generar", class = "btn-primary")
      )
    ))
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

#' Crear contenido del panel Sankey
create_sankey_content <- function() {
  tagList(
    fluidRow(
      box(title = "Diagrama Sankey", status = "primary", solidHeader = TRUE, width = 12,
          h4("Diagrama de flujo de reparto de costes"),
          p("Configure los filtros y presione 'Generar' para ver el diagrama Sankey.")
      )
    )
  )
}

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

#' Crear filtros específicos del panel Sankey
create_sankey_filters <- function() {
  tagList(
    h4("Configuración Sankey"),
    p("Filtros específicos para el diagrama Sankey."),
    checkboxInput("sankey_animated", "Animación", value = TRUE),
    sliderInput("sankey_width", "Ancho del diagrama:", 
               min = 600, max = 1200, value = 900)
  )
}

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