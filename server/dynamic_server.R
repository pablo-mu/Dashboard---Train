create_dynamic_server <- function(input, output, session, panel_definitions, default_panel) {

  # ============================================================
  # DEPENDENCIAS — cargadas UNA SOLA VEZ al iniciar el servidor
  # (nunca dentro de observeEvent / tryCatch)
  # ============================================================
  library(data.table)
  library(dplyr)
  library(shiny)
  library(shinyBS)
  library(shinydashboard)
  library(shinyjs)
  library(rintrojs)

  source("modules/lib_reparto.R")
  source("server/filters_server.R")

  # Módulos UI
  source("modules/sankey/sankey_ui.R")
  source("modules/matriz/matriz_ui.R")
  source("modules/diagnostico/diagnostico_ui.R")
  source("modules/traza/traza_ui.R")

  # Módulos servidor (lógica de negocio cargada aquí, NO en observers)
  source("modules/sankey/sankey.R")
  source("modules/sankey/sankey_server.R")
  source("modules/matriz/matriz_costes.R")
  source("modules/matriz/matriz_server.R")
  source("modules/diagnostico/diagnostico_server.R")
  source("modules/traza/traza_server.R")

  # ============================================================
  # PANEL ACTIVO
  # ============================================================

  if (is.null(default_panel) || !default_panel %in% names(panel_definitions))
    default_panel <- names(panel_definitions)[1]

  active_panel <- reactiveVal(default_panel)

  observeEvent(input$Sankey,           { active_panel("sankey") })
  observeEvent(input$`Matriz Costes`,  { active_panel("matriz_costes") })
  observeEvent(input$Diagnostics,      { active_panel("diagnostics") })
  observeEvent(input$Traza,            { active_panel("traza") })

  # ============================================================
  # ESTADO DE DATOS
  # ============================================================

  datos_loaded  <- reactiveVal(FALSE)
  loading_data  <- reactiveVal(FALSE)
  datos_raw     <- reactiveVal(NULL)

  # ---- Botón dinámico (carga / filtros) ----
  output$dynamic_action_btn <- renderUI({
    if (!datos_loaded()) {
      if (loading_data()) {
        div(bsButton("load_data", "Cargando...",
                     icon = icon("spinner", class = "fa-spin"),
                     style = "info", disabled = TRUE),
            style = "text-align:center;")
      } else {
        bsButton("load_data", "CARGAR DATOS",
                 icon = icon("database"), style = "danger")
      }
    } else {
      panel <- active_panel()
      cfg   <- panel_definitions[[panel]]
      if (!is.null(cfg)) {
        bsButton("show_filters", cfg$button_label,
                 icon = icon(cfg$icon), style = "danger")
      } else {
        bsButton("show_filters", "FILTROS", icon = icon("filter"), style = "danger")
      }
    }
  })
  outputOptions(output, "dynamic_action_btn", suspendWhenHidden = FALSE)

  # ============================================================
  # MODAL DE CARGA DE DATOS
  # ============================================================

  loading_modal_state <- reactiveVal("select")

  observeEvent(input$load_data, {
    loading_modal_state("select")
    archivos_casa <- listar_archivos_reparto("CASA")
    opciones      <- if (length(archivos_casa) > 0)
                       setNames(archivos_casa, basename(archivos_casa))
                     else c("No hay archivos" = "")

    showModal(modalDialog(
      title = NULL,
      div(style = "padding:15px;",
        div(style = "text-align:center;margin-bottom:25px;border-bottom:1px solid #eee;padding-bottom:15px;",
          h3("Carga de Datos", style = "margin:0;color:#2c3e50;font-weight:700;"),
          p("Selecciona la fuente y el archivo a procesar",
            style = "color:#7f8c8d;margin-top:5px;font-size:14px;")
        ),
        div(style = "margin-bottom:25px;",
          tags$label("Fuente de Datos:", style = "margin-bottom:10px;display:block;color:#34495e;"),
          shinyWidgets::radioGroupButtons(
            "modal_fuente", label = NULL, choices = c("CASA","SIE"),
            selected = "CASA", justified = TRUE, status = "primary", individual = TRUE,
            checkIcon = list(yes = icon("check"), no = icon("times"))
          )
        ),
        div(style = "margin-bottom:30px;",
          shinyWidgets::pickerInput(
            "modal_archivo", "Seleccionar Archivo:", choices = opciones,
            options = list(`live-search` = TRUE, `none-selected-text` = "Sin archivos",
                           `style` = "btn-outline-secondary", `size` = 10),
            width = "100%"
          )
        ),
        div(style = "text-align:center;",
          actionButton("modal_cargar", "CARGAR DATOS",
                       icon = icon("cloud-upload-alt"), class = "btn-danger btn-lg",
                       style = "width:100%;font-weight:bold;text-transform:uppercase;letter-spacing:1px;")
        )
      ),
      easyClose = TRUE, size = "m", footer = NULL
    ))
  })

  # Actualizar lista al cambiar fuente
  observeEvent(input$modal_fuente, {
    req(input$modal_fuente)
    archivos <- listar_archivos_reparto(input$modal_fuente)
    opciones <- if (length(archivos) > 0) setNames(archivos, basename(archivos)) else c("No hay archivos" = "")
    shinyWidgets::updatePickerInput(session, "modal_archivo", choices = opciones)
  })

  # Mostrar estado de carga
  .modal_cargando <- function(fuente) {
    showModal(modalDialog(
      title = NULL,
      div(style = "padding:40px 20px;text-align:center;",
        icon("spinner", class = "fa-spin",
             style = "font-size:64px;color:#c8102e;margin-bottom:20px;"),
        h3(paste("Cargando datos", fuente),
           style = "margin:20px 0 10px 0;color:#2c3e50;font-weight:600;"),
        p("Por favor espera...", style = "color:#666;margin:0;")
      ),
      easyClose = FALSE, size = "m", footer = NULL
    ))
  }

  observeEvent(input$modal_cancel_load, { removeModal() })

  observeEvent(input$modal_cargar, {
    req(input$modal_archivo)
    archivo  <- input$modal_archivo
    fuente   <- input$modal_fuente

    if (archivo == "") {
      showNotification("Selecciona un archivo válido.", type = "warning"); return()
    }

    .modal_cargando(fuente)
    loading_data(TRUE)

    tryCatch({
      datos <- preparar_datos_reparto(archivo_especifico = archivo)
      if (!is.null(datos) && nrow(datos) > 0) {
        datos_raw(datos)
        datos_loaded(TRUE)
        removeModal()
        showNotification(
          paste("✓ Datos cargados:", formatC(nrow(datos), format = "d", big.mark = " "), "registros"),
          type = "message", duration = 5
        )
      } else {
        removeModal()
        showNotification(
          paste("No se pudo cargar:", basename(archivo)),
          type = "error", duration = 10
        )
      }
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    }, finally = {
      loading_data(FALSE)
    })
  })

  # ============================================================
  # FILTROS DINÁMICOS (solo se inicializan una vez)
  # ============================================================

  filters <- NULL
  observe({
    if (datos_loaded() && !is.null(datos_raw()) && is.null(filters)) {
      datos_reactive <- reactive({ datos_raw() })
      filters <<- create_filters_server(input, output, session, datos_reactive)
    }
  })

  # ============================================================
  # MÓDULOS DE ANÁLISIS
  # ============================================================

  sankey_data      <- create_sankey_server    (input, output, session, datos_raw, active_panel)
  matriz_data      <- create_matriz_server    (input, output, session, datos_raw, active_panel)
  diagnostico_opts <- create_diagnostico_server(input, output, session, datos_raw, active_panel)
  traza_data       <- create_traza_server     (input, output, session, datos_raw, active_panel)

  # ============================================================
  # CONTENIDO DE PANELES
  # ============================================================

  output$sankey_content        <- renderUI({ create_sankey_content() })
  output$matriz_costes_content <- renderUI({ create_matriz_content() })
  output$diagnostics_content   <- renderUI({ create_diagnostico_content() })
  output$traza_content         <- renderUI({ create_traza_content() })

  # ============================================================
  # VISIBILIDAD DE PANELES
  # ============================================================

  observe({
    panel <- active_panel()
    all_panels <- c("sankey_panel","matriz_costes_panel","diagnostics_panel","traza_panel")
    for (p in all_panels) shinyjs::hide(p)
    target <- switch(panel,
                     sankey        = "sankey_panel",
                     matriz_costes = "matriz_costes_panel",
                     diagnostics   = "diagnostics_panel",
                     traza         = "traza_panel")
    if (!is.null(target)) shinyjs::show(target)
  })

  # ============================================================
  # MODAL DE FILTROS POR PANEL
  # ============================================================

  observeEvent(input$show_filters, {
    if (!datos_loaded()) {
      showNotification("Carga los datos primero.", type = "warning", duration = 3)
      return()
    }

    panel   <- active_panel()
    content <- switch(panel,
                      sankey        = create_sankey_filters(),
                      matriz_costes = create_matriz_filters(),
                      diagnostics   = create_diagnostico_filters(),
                      traza         = create_traza_filters(),
                      list(h4("Sin filtros"), p("No hay filtros para este panel.")))

    titulo <- switch(panel,
                     sankey        = "Diagrama Sankey",
                     matriz_costes = "Matriz de Costes",
                     diagnostics   = "Diagnósticos",
                     traza         = "Matriz de Movimientos",
                     toupper(panel))

    showModal(modalDialog(
      title = NULL,
      div(style = "padding:20px;",
        h3(paste("Configuración —", titulo),
           style = "margin-top:0;margin-bottom:20px;color:#2c3e50;font-weight:600;"),
        content
      ),
      easyClose = TRUE, size = "l",
      footer = div(
        style = "display:flex;justify-content:flex-end;gap:10px;padding:15px 20px;background:#f8f9fa;border-top:1px solid #dee2e6;",
        actionButton("modal_cancel", "Cancelar", icon = icon("times"),
                     class = "btn btn-primary",
                     style = "padding:10px 25px;font-size:14px;color:white;"),
        actionButton("apply_filters", "Generar Diagrama", icon = icon("chart-line"),
                     class = "btn btn-primary",
                     style = "background-color:#c8102e;border:none;padding:10px 30px;font-size:14px;font-weight:600;",
                     onclick = "Shiny.setInputValue('apply_filters', Math.random());")
      )
    ))
  })

  observeEvent(input$modal_cancel, { removeModal() })
  observeEvent(input$apply_filters, { removeModal() })

  # ============================================================
  # RETORNO
  # ============================================================

  list(
    active_panel = active_panel,
    datos_raw    = datos_raw,
    filters      = filters,
    datos_loaded = datos_loaded
  )
}