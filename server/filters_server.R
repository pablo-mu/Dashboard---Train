# ============================================================================
# FILTERS SERVER - RENDERIZACIÓN DINÁMICA DE FILTROS
# ============================================================================

#' Crear servidor de filtros dinámicos
#' @param input Input de la sesión
#' @param output Output de la sesión
#' @param session Sesión de Shiny
#' @param datos_raw Reactive con los datos
#' @return Lista con valores de filtros reactivos
create_filters_server <- function(input, output, session, datos_raw) {
  
  # ========================================
  # FILTRO CENTRO GESTOR
  # ========================================
  output$centro_gestor_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    centros <- sort(unique(datos$ZCENT_GEST))
    centros <- centros[!is.na(centros) & centros != ""]
    
    selectInput(
      inputId = "centro_gestor",
      label = "Centro Gestor*:",
      choices = c("Todos" = "", centros),
      selected = ""
    )
  })
  
  # ========================================
  # FILTRO ORIGEN (DEPENDE DE NIVEL Y CENTRO)
  # ========================================
  output$origen_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    # Filtrar por centro gestor si está seleccionado
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    # Usar el nivel de agregación seleccionado en el radiobutton
    nivel_agregacion <- if (!is.null(input$nivel_agregacion)) {
      input$nivel_agregacion
    } else {
      "cac"  # valor por defecto
    }
    
    if (nivel_agregacion == "subcac") {
      origenes <- sort(unique(datos$ZCT_SUBCAC_E))
      label <- "Subcentro de Actividad (Origen):"
      choices <- origenes[!is.na(origenes) & origenes != ""]
      final_choices <- c("Todos" = "", choices)
    } else if (nivel_agregacion == "cac2") {
      origenes <- unique(substr(datos$ZACT_E, 1, 2))
      origenes <- sort(origenes)
      label <- "CAC (2 dígitos - Origen):"
      choices <- origenes[!is.na(origenes) & origenes != ""]
      final_choices <- c("Todos" = "", choices)
    } else if (nivel_agregacion == "cac1") {
      origenes <- unique(substr(datos$ZACT_E, 1, 1))
      origenes <- sort(origenes)
      label <- "CAC (1 dígito - Origen):"
      choices <- origenes[!is.na(origenes) & origenes != ""]
      final_choices <- c("Todos" = "", choices)
    } else {
      origenes <- sort(unique(datos$ZACT_E))
      origenes <- origenes[!is.na(origenes) & origenes != ""]
      label <- "Centro de Actividad (Origen):"
      
      # Generar grupos por primer dígito
      first_digits <- unique(substr(origenes, 1, 1))
      groups <- paste0(sort(first_digits), "..")
      
      final_choices <- list(
        "Global" = c("Todos" = ""),
        "Grupos" = groups,
        "Individuales" = origenes
      )
    }
    
    selectInput(
      inputId = "origen",
      label = label,
      choices = final_choices,
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO FASE 1
  # ========================================
  output$fase1_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    nivel_agregacion <- if (!is.null(input$nivel_agregacion)) input$nivel_agregacion else "cac"
    
    cols_fase1 <- c("ZACT_RE12", "ZACT_RE34", "ZACT_F1SD")
    
    vals <- unique(unlist(lapply(cols_fase1, function(col) {
      if (col %in% names(datos)) datos[[col]][!is.na(datos[[col]]) & datos[[col]] != ""]
    })))
    
    if (nivel_agregacion == "cac1") {
      vals <- unique(substr(vals, 1, 1))
      label <- "Fase 1 (CAC1):"
      final_choices <- c("Todos" = "", sort(vals))
    } else if (nivel_agregacion == "cac2") {
      vals <- unique(substr(vals, 1, 2))
      label <- "Fase 1 (CAC2):"
      final_choices <- c("Todos" = "", sort(vals))
    } else {
      label <- "Fase 1 (CAC):"
      vals <- sort(vals)
      
      # Generar grupos
      first_digits <- unique(substr(vals, 1, 1))
      groups <- paste0(sort(first_digits), "..")
      
      final_choices <- list(
        "Global" = c("Todos" = ""),
        "Grupos" = groups,
        "Individuales" = vals
      )
    }
    
    selectInput("fase1", label, choices = final_choices, selected = "", multiple = TRUE)
  })

  # ========================================
  # FILTRO FASE 2
  # ========================================
  output$fase2_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    nivel_agregacion <- if (!is.null(input$nivel_agregacion)) input$nivel_agregacion else "cac"
    
    cols_fase2 <- c("ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD")
    
    vals <- unique(unlist(lapply(cols_fase2, function(col) {
      if (col %in% names(datos)) datos[[col]][!is.na(datos[[col]]) & datos[[col]] != ""]
    })))
    
    if (nivel_agregacion == "cac1") {
      vals <- unique(substr(vals, 1, 1))
      label <- "Fase 2 (CAC1):"
      final_choices <- c("Todos" = "", sort(vals))
    } else if (nivel_agregacion == "cac2") {
      vals <- unique(substr(vals, 1, 2))
      label <- "Fase 2 (CAC2):"
      final_choices <- c("Todos" = "", sort(vals))
    } else {
      label <- "Fase 2 (CAC):"
      vals <- sort(vals)
      
      # Generar grupos
      first_digits <- unique(substr(vals, 1, 1))
      groups <- paste0(sort(first_digits), "..")
      
      final_choices <- list(
        "Global" = c("Todos" = ""),
        "Grupos" = groups,
        "Individuales" = vals
      )
    }
    
    selectInput("fase2", label, choices = final_choices, selected = "", multiple = TRUE)
  })

  # ========================================
  # FILTRO DESTINO (FASE 3)
  # ========================================
  output$destino_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    # Filtrar por centro gestor si está seleccionado
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    # Usar el nivel de agregación seleccionado en el radiobutton
    nivel_agregacion <- if (!is.null(input$nivel_agregacion)) {
      input$nivel_agregacion
    } else {
      "cac"  # valor por defecto
    }
    
    # Columnas de destino por fase (SOLO FASE 3)
    cols_destino <- c("ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD")
    
    if (nivel_agregacion == "subcac") {
      # Subcac no suele aplicar a destino final agregado, pero mantenemos lógica
      destinos <- unique(unlist(lapply(cols_destino, function(col) {
        if (col %in% names(datos)) {
          datos[[col]][datos[[col]] != ""]
        }
      })))
      label <- "Subcentro (Destino F3):"
      choices <- sort(destinos[!is.na(destinos) & destinos != ""])
      final_choices <- c("Todos" = "", choices)
    } else if (nivel_agregacion == "cac2") {
      destinos_raw <- unique(unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        })))
      destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 2))
      label <- "CAC 2 dígitos (Destino F3):"
      choices <- sort(destinos)
      final_choices <- c("Todos" = "", choices)
    } else if (nivel_agregacion == "cac1") {
      destinos_raw <- unique(unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        })))
      destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 1))
      label <- "CAC 1 dígito (Destino F3):"
      choices <- sort(destinos)
      final_choices <- c("Todos" = "", choices)
    } else {
      destinos <- unique(unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        })))
      label <- "CAC (Destino F3):"
      choices <- sort(destinos[!is.na(destinos) & destinos != ""])
      
      # Generar grupos
      first_digits <- unique(substr(choices, 1, 1))
      groups <- paste0(sort(first_digits), "..")
      
      final_choices <- list(
        "Global" = c("Todos" = ""),
        "Grupos" = groups,
        "Individuales" = choices
      )
    }
    
    selectInput(
      inputId = "destino",
      label = label,
      choices = final_choices,
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO MES
  # ========================================
  output$mes_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    meses <- sort(unique(datos$ZMES))
    meses <- meses[!is.na(meses)]
    
    selectInput(
      inputId = "mes",
      label = "Mes:",
      choices = c("Todos" = "", as.character(meses)),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO AÑO
  # ========================================
  output$anyo_filter <- shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    anyos <- sort(unique(datos$ZANYO))
    anyos <- anyos[!is.na(anyos)]
    
    selectInput(
      inputId = "anyo",
      label = "Año:",
      choices = c("Todos" = "", as.character(anyos)),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # BOTÓN RESET FILTROS
  # ========================================
  shiny::observeEvent(input$reset_filters, {
    updateSelectInput(session, "centro_gestor", selected = "")
    updateSelectInput(session, "origen", selected = "")
    updateSelectInput(session, "destino", selected = "")
    updateSelectInput(session, "mes", selected = "")
    updateSelectInput(session, "anyo", selected = "")
  })
  
  # ========================================
  # RETURN VALORES DE FILTROS REACTIVOS
  # ========================================
  return(list(
    centro_gestor = reactive({ input$centro_gestor }),
    origen = reactive({ input$origen }),
    destino = reactive({ input$destino }),
    mes = reactive({ input$mes }),
    anyo = reactive({ input$anyo })
  ))
}