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
  source("modules/diagnostico/diagnostico_server.R")
  source("modules/diagnostico/diagnostico_ui.R")
  source("modules/traza/traza_ui.R")
  source("modules/traza/traza_server.R")


  
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
  
  shiny::observeEvent(input$Traza, {
    active_panel("traza")
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

  # Variable reactiva para controlar el estado del modal de carga
  loading_modal_state <- shiny::reactiveVal("select")  # "select" o "loading"
  
  # Observer para mostrar modal de selección de fuente de datos
  shiny::observeEvent(input$load_data, {
    cat("Botón de cargar datos presionado\n")  # Debug
    loading_modal_state("select")
    
    showModal(modalDialog(
      title = NULL,
      div(
        id = "load_data_modal_content",
        style = "padding: 20px;",
        h3("Seleccionar Fuente de Datos",
           style = "margin-top: 0; margin-bottom: 20px; color: #2c3e50; font-weight: 600;"),
        
        div(
          style = "margin-bottom: 30px;",
          p("Elige la fuente de datos que deseas cargar:",
            style = "color: #555; margin-bottom: 20px;")
        ),
        
        # Opciones de fuente
        div(
          style = "display: flex; gap: 20px; justify-content: center;",
          
          # Opción CASA
          div(
            style = "flex: 1; max-width: 250px;",
            actionButton(
              "select_casa",
              div(
                icon("home", style = "font-size: 48px; margin-bottom: 15px;"),
                h4("CASA", style = "margin: 10px 0;"),
                p("Formato estándar", style = "font-size: 12px; color: #666; margin: 0;")
              ),
              style = "width: 100%; height: 180px; background-color: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px; display: flex; flex-direction: column; align-items: center; justify-content: center; transition: all 0.3s;",
              onclick = "this.style.backgroundColor='#e9ecef'; this.style.borderColor='#c8102e';"
            )
          ),
          
          # Opción SIE
          div(
            style = "flex: 1; max-width: 250px;",
            actionButton(
              "select_sie",
              div(
                icon("database", style = "font-size: 48px; margin-bottom: 15px;"),
                h4("SIE", style = "margin: 10px 0;"),
                p("Formato SIE", style = "font-size: 12px; color: #666; margin: 0;")
              ),
              style = "width: 100%; height: 180px; background-color: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px; display: flex; flex-direction: column; align-items: center; justify-content: center; transition: all 0.3s;",
              onclick = "this.style.backgroundColor='#e9ecef'; this.style.borderColor='#c8102e';"
            )
          )
        )
      ),
      easyClose = TRUE,
      size = "m",
      footer = div(
        style = "display: flex; justify-content: flex-end; gap: 10px; padding: 15px 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
        actionButton("modal_cancel_load", "Cancelar",
                    icon = icon("times"),
                    class = "btn btn-primary",
                    style = "padding: 10px 25px; font-size: 14px; color: white;")
      )
    ))
  })
  
  # Función para actualizar el contenido del modal a estado de carga
  actualizar_modal_cargando <- function(fuente) {
    loading_modal_state("loading")
    
    showModal(modalDialog(
      title = NULL,
      div(
        style = "padding: 40px 20px; text-align: center;",
        icon("spinner", class = "fa-spin", style = "font-size: 64px; color: #c8102e; margin-bottom: 20px;"),
        h3(paste("Cargando datos", fuente), 
           style = "margin: 20px 0 10px 0; color: #2c3e50; font-weight: 600;"),
        p("Por favor espera mientras se procesan los archivos...",
          style = "color: #666; margin: 0;")
      ),
      easyClose = FALSE,
      size = "m",
      footer = NULL
    ))
  }
  
  # Cerrar modal de carga al presionar cancelar
  observeEvent(input$modal_cancel_load, {
    removeModal()
  })
  
  # Observer para cargar datos CASA
  shiny::observeEvent(input$select_casa, {
    cat("Seleccionada fuente CASA\n")  # Debug
    
    # Cambiar modal a estado de carga
    actualizar_modal_cargando("CASA")
    
    # Actualizar estado de carga
    loading_data(TRUE)
    
    tryCatch({
      cat("Iniciando carga de datos CASA...\n")  # Debug en consola
      
      datos <- preparar_datos_reparto(
        rutas = c(".data/CASA", "./data/CASA", "data/CASA", "./.data/CASA")
      )
      
      if (!is.null(datos)) {
        cat("Datos CASA cargados exitosamente:", nrow(datos), "filas\n")  # Debug
        
        datos_dt <- datos %>% as.data.table()
        
        # Actualizar los datos y estado
        datos_raw(datos)
        datos_loaded(TRUE)
        loading_data(FALSE)
        
        # Cerrar modal después de carga exitosa
        removeModal()
        
        showNotification(
          paste("✓ Datos CASA cargados:", formatC(nrow(datos_dt), format = "d", big.mark = " "), "registros únicos"),
          type = "message",
          duration = 5
        )
      } else {
        cat("Error: preparar_datos_reparto() devolvió NULL\n")  # Debug
        loading_data(FALSE)
        
        # Cerrar modal incluso si hay error
        removeModal()
        
        showNotification(
          "No se encontraron archivos CSV en .data/CASA",
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      cat("Error en carga de datos CASA:", e$message, "\n")  # Debug
      loading_data(FALSE)
      
      # Cerrar modal en caso de error
      removeModal()
      
      showNotification(
        paste("Error al cargar datos CASA:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Observer para cargar datos SIE
  shiny::observeEvent(input$select_sie, {
    cat("Seleccionada fuente SIE\n")  # Debug
    
    # Cambiar modal a estado de carga
    actualizar_modal_cargando("SIE")
    
    # Actualizar estado de carga
    loading_data(TRUE)
    
    tryCatch({
      cat("Iniciando carga de datos SIE...\n")  # Debug en consola
      
      datos <- preparar_datos_reparto(
        rutas = c(".data/SIE", "./data/SIE", "data/SIE", "./.data/SIE")
      )
      
      if (!is.null(datos)) {
        cat("Datos SIE cargados exitosamente:", nrow(datos), "filas\n")  # Debug
        
        datos_dt <- datos %>% as.data.table()
        
        # Actualizar los datos y estado
        datos_raw(datos)
        datos_loaded(TRUE)
        loading_data(FALSE)
        
        # Cerrar modal después de carga exitosa
        removeModal()
        
        showNotification(
          paste("✓ Datos SIE cargados:", formatC(nrow(datos_dt), format = "d", big.mark = " "), "registros únicos"),
          type = "message",
          duration = 5
        )
      } else {
        cat("Error: preparar_datos_reparto() devolvió NULL\n")  # Debug
        loading_data(FALSE)
        
        # Cerrar modal incluso si hay error
        removeModal()
        
        showNotification(
          "No se encontraron archivos CSV en .data/SIE",
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      cat("Error en carga de datos SIE:", e$message, "\n")  # Debug
      loading_data(FALSE)
      
      # Cerrar modal en caso de error
      removeModal()
      
      showNotification(
        paste("Error al cargar datos SIE:", e$message),
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
  
  # Inicializar servidor de Diagnóstico
  diagnostico_options <- create_diagnostico_server(input, output, session, datos_raw, active_panel)
  
  # Inicializar servidor de Traza
  traza_data <- create_traza_server(input, output, session, datos_raw, active_panel)
  
  # Renderizar contenido de cada panel
  output$sankey_content <- shiny::renderUI({
    create_sankey_content()
  })
  
  output$matriz_costes_content <- shiny::renderUI({
    create_matriz_content()
  })
  
  output$diagnostics_content <- shiny::renderUI({
    create_diagnostico_content()
  })
  
  output$traza_content <- shiny::renderUI({
    create_traza_content()
  })
  
  # Controlar visibilidad de paneles según active_panel
  observe({
    panel <- active_panel()
    
    # Ocultar todos los paneles
    shinyjs::hide("sankey_panel")
    shinyjs::hide("matriz_costes_panel")
    shinyjs::hide("diagnostics_panel")
    shinyjs::hide("traza_panel")
    
    # Mostrar solo el panel activo
    if (panel == "sankey") {
      shinyjs::show("sankey_panel")
    } else if (panel == "matriz_costes") {
      shinyjs::show("matriz_costes_panel")
    } else if (panel == "diagnostics") {
      shinyjs::show("diagnostics_panel")
    } else if (panel == "traza") {
      shinyjs::show("traza_panel")
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
      "diagnostics" = create_diagnostico_filters(),
      "traza" = create_traza_filters(),
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
                        "traza" = "Matriz de Movimientos",
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
