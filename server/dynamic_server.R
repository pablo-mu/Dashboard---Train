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
    
    # Listar archivos iniciales (CASA por defecto)
    archivos_casa <- listar_archivos_reparto("CASA")
    opciones_archivos <- if(length(archivos_casa) > 0) setNames(archivos_casa, basename(archivos_casa)) else c("No hay archivos" = "")
    
    showModal(modalDialog(
      title = NULL,
      div(
        id = "load_data_modal_content",
        style = "padding: 15px;",
        
        # Header con estilo mejorado
        div(
          style = "text-align: center; margin-bottom: 25px; border-bottom: 1px solid #eee; padding-bottom: 15px;",
          h3("Carga de Datos", 
             style = "margin: 0; color: #2c3e50; font-weight: 700; letter-spacing: -0.5px;"),
          p("Selecciona la fuente y el archivo a procesar", 
            style = "color: #7f8c8d; margin-top: 5px; font-size: 14px;")
        ),
        
        # Selección de Fuente con botones modernos
        div(
          style = "margin-bottom: 25px;",
          tags$label("Fuente de Datos:", style = "margin-bottom: 10px; display: block; color: #34495e;"),
          shinyWidgets::radioGroupButtons(
            inputId = "modal_fuente",
            label = NULL,
            choices = c("CASA", "SIE"),
            selected = "CASA",
            justified = TRUE,
            status = "primary",
            individual = TRUE,
            checkIcon = list(
              yes = icon("check"),
              no = icon("times")
            )
          )
        ),
        
        # Selección de Archivo con búsqueda
        div(
          style = "margin-bottom: 30px;",
          shinyWidgets::pickerInput(
            inputId = "modal_archivo",
            label = "Seleccionar Archivo:",
            choices = opciones_archivos,
            options = list(
              `live-search` = TRUE,
              `none-selected-text` = "Sin archivos disponibles",
              `style` = "btn-outline-secondary",
              `size` = 10
            ),
            width = "100%"
          )
        ),
        
        # Botón de Acción
        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton("modal_cargar", "CARGAR DATOS",
                       icon = icon("cloud-upload-alt"),
                       class = "btn-danger btn-lg",
                       style = "width: 100%; font-weight: bold; text-transform: uppercase; letter-spacing: 1px; box-shadow: 0 4px 6px rgba(50,50,93,.11), 0 1px 3px rgba(0,0,0,.08); transition: all 0.15s ease;")
        )
      ),
      easyClose = TRUE,
      size = "m",
      footer = NULL
    ))
  })
  
  # Actualizar lista de archivos cuando cambia la fuente
  shiny::observeEvent(input$modal_fuente, {
    req(input$modal_fuente)
    archivos <- listar_archivos_reparto(input$modal_fuente)
    opciones <- if(length(archivos) > 0) setNames(archivos, basename(archivos)) else c("No hay archivos" = "")
    
    shinyWidgets::updatePickerInput(session, "modal_archivo", choices = opciones)
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
  
  # Cargar datos al presionar el botón
  shiny::observeEvent(input$modal_cargar, {
    req(input$modal_archivo)
    archivo_seleccionado <- input$modal_archivo
    fuente <- input$modal_fuente
    
    if (archivo_seleccionado == "") {
      showNotification("Por favor seleccione un archivo válido", type = "warning")
      return()
    }
    
    cat(sprintf("Seleccionada fuente %s, archivo: %s\n", fuente, basename(archivo_seleccionado)))
    
    # Cambiar modal a estado de carga
    actualizar_modal_cargando(fuente)
    
    # Actualizar estado de carga
    loading_data(TRUE)
    
    tryCatch({
      cat(sprintf("Iniciando carga de datos %s...\n", fuente))
      
      # Usar archivo específico
      datos <- preparar_datos_reparto(
        archivo_especifico = archivo_seleccionado
      )
      
      if (!is.null(datos)) {
        cat(sprintf("Datos %s cargados exitosamente: %d filas\n", fuente, nrow(datos)))
        
        datos_dt <- datos %>% as.data.table()
        
        # Actualizar los datos y estado
        datos_raw(datos)
        datos_loaded(TRUE)
        loading_data(FALSE)
        
        # Cerrar modal después de carga exitosa
        removeModal()
        
        showNotification(
          paste(sprintf("✓ Datos %s cargados:", fuente), formatC(nrow(datos_dt), format = "d", big.mark = " "), "registros únicos"),
          type = "message",
          duration = 5
        )
      } else {
        cat("Error: preparar_datos_reparto() devolvió NULL\n")
        loading_data(FALSE)
        removeModal()
        showNotification(
          sprintf("No se pudo cargar el archivo %s", basename(archivo_seleccionado)),
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      cat(sprintf("Error en carga de datos %s: %s\n", fuente, e$message))
      loading_data(FALSE)
      removeModal()
      showNotification(
        paste(sprintf("Error al cargar datos %s:", fuente), e$message),
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
