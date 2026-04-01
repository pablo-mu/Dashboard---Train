# ============================================================================
# TRAZA SERVER LOGIC
# ============================================================================

#' Create Traza server logic
#' This function sets up all the reactive logic for the Traza panel
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param datos_raw Reactive value containing the raw data
#' @param active_panel Reactive value indicating the active panel
create_traza_server <- function(input, output, session, datos_raw, active_panel) {
  
  # Variable reactiva para almacenar resultados de la Traza
  traza_data <- reactiveVal(NULL)
  
  # Cargar datos de mapeo CAC -> Linea/Modalidad
  mapping_cac <- reactive({
      tryCatch({
          df <- readxl::read_excel(".data/linea_actividad_unificada.xlsx")
          # Asegurar que CAC es string para el join
          df$CAC <- as.character(df$CAC)
          df
      }, error = function(e) {
          NULL
      })
  })

  # Renderizar selectores dinámicamente
  output$ui_modal_traza_linea <- renderUI({
      if (is.null(input$modal_traza_nivel_agregacion) || input$modal_traza_nivel_agregacion != "cac") {
          return(NULL)
      }
      
      map_data <- mapping_cac()
      choices <- c("Todos")
      
      if (!is.null(map_data)) {
          lineas <- sort(unique(map_data$`Línea de Actividad`))
          choices <- c("Todos", lineas)
      }
      
      selectInput("modal_traza_linea", "Línea de Actividad:", 
                  choices = choices, selected = "Todos")
  })
  
  output$ui_modal_traza_modalidad <- renderUI({
      if (is.null(input$modal_traza_nivel_agregacion) || input$modal_traza_nivel_agregacion != "cac") {
          return(NULL)
      }
      
      map_data <- mapping_cac()
      choices <- c("Todos")
      
      if (!is.null(map_data)) {
          modalidades <- sort(unique(map_data$Modalidad))
          choices <- c("Todos", modalidades)
      }
      
      selectInput("modal_traza_modalidad", "Modalidad:", 
                  choices = choices, selected = "Todos")
  })
  
  # Limpiar memoria al cambiar de pestaña
  observe({
    if (!is.null(active_panel()) && active_panel() != "traza") {
      traza_data(NULL)
      gc()
    }
  })
  
  # ========================================================================
  # GENERACIÓN DE LA TRAZA cuando se presiona "Generar"
  # ========================================================================
  
  observeEvent(input$apply_filters, {
    # Solo procesar si estamos en el panel de Traza
    if (is.null(active_panel()) || active_panel() != "traza") {
      return()
    }
    
    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados", type = "error", duration = 5)
      traza_data(NULL)
      return()
    }
    
    # ==========================================================================
    # CAPTURAR FILTROS DEL SIDEBAR
    # ==========================================================================
    
    # Centro Gestor
    centro_gestor_val <- if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      input$centro_gestor
    } else {
      NULL
    }
    
    # Origen (CAC)
    cac_origen_val <- if (!is.null(input$origen) && length(input$origen) > 0 && input$origen[1] != "") {
      input$origen
    } else {
      NULL
    }
    
    # Destino (CAC final - fase 3)
    cac_destino_val <- if (!is.null(input$destino) && length(input$destino) > 0 && input$destino[1] != "") {
      input$destino
    } else {
      NULL
    }
    
    # Fase 1
    fase1_val <- if (!is.null(input$fase1) && length(input$fase1) > 0 && input$fase1[1] != "") {
      input$fase1
    } else {
      NULL
    }

    # Fase 2
    fase2_val <- if (!is.null(input$fase2) && length(input$fase2) > 0 && input$fase2[1] != "") {
      input$fase2
    } else {
      NULL
    }
    
    # Mes
    mes_val <- if (!is.null(input$mes) && length(input$mes) > 0 && input$mes[1] != "") {
      as.numeric(input$mes)
    } else {
      NULL
    }
    
    # Año
    anyo_val <- if (!is.null(input$anyo) && length(input$anyo) > 0 && input$anyo[1] != "") {
      as.numeric(input$anyo)
    } else {
      NULL
    }
    
    # Fases a incluir (siempre 1, 2, 3 para esta tabla)
    fases_incluir_val <- c(1, 2, 3)
    
    # ==========================================================================
    # CAPTURAR NIVEL DE AGREGACIÓN DEL MODAL (aplica a todas las fases)
    # ==========================================================================
    
    # Nivel de agregación único para toda la tabla
    nivel_agregacion <- if (!is.null(input$modal_traza_nivel_agregacion)) {
      input$modal_traza_nivel_agregacion
    } else {
      "cac"  # Por defecto CAC (3 dígitos)
    }
    
    # Mostrar porcentajes
    mostrar_pct <- if (!is.null(input$modal_traza_mostrar_porcentajes)) {
      input$modal_traza_mostrar_porcentajes
    } else {
      FALSE
    }
    
    tryCatch({
      source("modules/lib_reparto.R")
      
      # Filtrar datos según criterios del sidebar
      # Siempre generar a nivel CAC (3 dígitos)
      datos_filtrados <- filtrar_datos_reparto(
        datos,
        centro_gestor = centro_gestor_val,
        cacs_origen = cac_origen_val,
        subcacs_origen = NULL,
        mes = mes_val,
        anyo = anyo_val,
        nivel_agregacion = "cac"
      )
      
      if (is.null(datos_filtrados) || nrow(datos_filtrados) == 0) {
        showNotification("No hay datos que cumplan los criterios de filtrado", type = "warning", duration = 5)
        traza_data(NULL)
        return()
      }
      
      # Generar matriz de movimientos siempre a nivel CAC
      matriz <- extraer_movimientos(
        datos_filtrados,
        nivel_agregacion = "cac",
        fases_incluir = fases_incluir_val,
        incluir_tipo_param = FALSE,  # No necesitamos tipo ni parámetro para esta vista
        verbose = TRUE,
        debug = FALSE
      )
      
      if (is.null(matriz) || nrow(matriz) == 0) {
        showNotification("No se pudo generar la matriz de movimientos", type = "error", duration = 5)
        traza_data(NULL)
        return()
      }
      
      # Convertir a data.frame
      matriz_df <- as.data.frame(matriz)
      
      # Aplicar agregación según el nivel seleccionado
      aplicar_nivel <- function(columna, nivel) {
        resultado <- columna
        validos <- !is.na(columna) & columna != ""
        
        if (nivel == "cac1") {
          resultado[validos] <- substr(columna[validos], 1, 1)
        } else if (nivel == "cac2") {
          resultado[validos] <- substr(columna[validos], 1, 2)
        }
        # Para "cac" no hacemos nada (ya está a 3 dígitos)
        
        return(resultado)
      }
      
      # Aplicar nivel de agregación a todas las fases
      matriz_df$fase_0 <- aplicar_nivel(matriz_df$fase_0, nivel_agregacion)
      matriz_df$fase_1 <- aplicar_nivel(matriz_df$fase_1, nivel_agregacion)
      matriz_df$fase_2 <- aplicar_nivel(matriz_df$fase_2, nivel_agregacion)
      matriz_df$fase_3 <- aplicar_nivel(matriz_df$fase_3, nivel_agregacion)
      
      # ==========================================================================
      # APLICAR FILTROS DE FASE (1, 2, 3)
      # ==========================================================================
      
      # Expandir selecciones de fase usando la función de lib_reparto.R
      if (!is.null(fase1_val)) {
        fase1_val <- expandir_seleccion_cac(fase1_val, unique(matriz_df$fase_1))
      }
      if (!is.null(fase2_val)) {
        fase2_val <- expandir_seleccion_cac(fase2_val, unique(matriz_df$fase_2))
      }
      if (!is.null(cac_destino_val)) {
        cac_destino_val <- expandir_seleccion_cac(cac_destino_val, unique(matriz_df$fase_3))
      }
      
      # Helper para filtrar robustamente
      filtrar_fase <- function(datos, columna, valores) {
        if (is.null(valores)) return(datos)
        datos[datos[[columna]] %in% valores, ]
      }
      
      matriz_df <- filtrar_fase(matriz_df, "fase_1", fase1_val)
      matriz_df <- filtrar_fase(matriz_df, "fase_2", fase2_val)
      matriz_df <- filtrar_fase(matriz_df, "fase_3", cac_destino_val)
      
      # ==========================================================================
      # CREAR TABLA PIVOTADA: CAC_Final x [CD, F1, F2, F3]
      # ==========================================================================
      
      # Identificar en qué fase cada registro llega al CAC final (fase_3)
      # CD: Coste Directo - llega directamente desde fase_0
      # F1: Llega en fase 1 (fase_1 == fase_3)
      # F2: Llega en fase 2 (fase_2 == fase_3)
      # F3: Llega en fase 3 (último movimiento)
      
      matriz_df$tipo_llegada <- "F3"  # Por defecto, llega en fase 3
      
      # Si fase_0 == fase_3 → Coste Directo (CD)
      matriz_df$tipo_llegada[!is.na(matriz_df$fase_0) & !is.na(matriz_df$fase_3) & 
                             matriz_df$fase_0 == matriz_df$fase_3] <- "CD"
      
      # Si fase_1 == fase_3 (y no es CD) → Llega en F1
      matriz_df$tipo_llegada[!is.na(matriz_df$fase_1) & !is.na(matriz_df$fase_3) & 
                             matriz_df$fase_1 == matriz_df$fase_3 & 
                             matriz_df$tipo_llegada != "CD"] <- "F1"
      
      # Si fase_2 == fase_3 (y no es CD ni F1) → Llega en F2
      matriz_df$tipo_llegada[!is.na(matriz_df$fase_2) & !is.na(matriz_df$fase_3) & 
                             matriz_df$fase_2 == matriz_df$fase_3 & 
                             !matriz_df$tipo_llegada %in% c("CD", "F1")] <- "F2"
      
      # Agrupar por CAC final (fase_3) y tipo de llegada, sumando importes
      tabla_pivot <- matriz_df %>%
        group_by(fase_3, tipo_llegada) %>%
        summarise(importe = sum(importe, na.rm = TRUE), .groups = "drop")
      
      # Pivotar la tabla para tener columnas CD, F1, F2, F3
      library(tidyr)
      tabla_final <- tabla_pivot %>%
        pivot_wider(
          names_from = tipo_llegada,
          values_from = importe,
          values_fill = 0
        ) %>%
        as.data.frame()
      
      # Renombrar fase_3 a CAC_Final
      names(tabla_final)[names(tabla_final) == "fase_3"] <- "CAC_Final"
      
      # Asegurar que existen todas las columnas CD, F1, F2, F3
      for (col in c("CD", "F1", "F2", "F3")) {
        if (!col %in% names(tabla_final)) {
          tabla_final[[col]] <- 0
        }
      }

      # Join with mapping
      # Only if level is CAC or linea or modalidad
      if (nivel_agregacion %in% c("cac", "linea", "modalidad")) {
          map_data <- mapping_cac()
          if (!is.null(map_data)) {
              # tabla_final has CAC_Final. map_data has CAC.
              # Ensure types match
              tabla_final$CAC_Final <- as.character(tabla_final$CAC_Final)
              
              # Left join to keep all CACs from calculation, adding info where available
              tabla_final <- tabla_final %>%
                  left_join(map_data, by = c("CAC_Final" = "CAC"))
                  
              # Filter by Linea (only if level is CAC)
              if (nivel_agregacion == "cac" && !is.null(input$modal_traza_linea) && input$modal_traza_linea != "Todos") {
                  tabla_final <- tabla_final %>% 
                      filter(`Línea de Actividad` == input$modal_traza_linea)
              }
              
              # Filter by Modalidad (only if level is CAC)
              if (nivel_agregacion == "cac" && !is.null(input$modal_traza_modalidad) && input$modal_traza_modalidad != "Todos") {
                  tabla_final <- tabla_final %>% 
                      filter(Modalidad == input$modal_traza_modalidad)
              }
          }
      }
      
      # Agregación especial para Línea o Modalidad
      if (nivel_agregacion == "linea") {
          # Rellenar NAs antes de agrupar
          tabla_final$`Línea de Actividad`[is.na(tabla_final$`Línea de Actividad`)] <- "Sin Asignar"
          
          # Agrupar por Línea de Actividad
          tabla_final <- tabla_final %>%
              group_by(`Línea de Actividad`) %>%
              summarise(
                  CD = sum(CD, na.rm = TRUE),
                  F1 = sum(F1, na.rm = TRUE),
                  F2 = sum(F2, na.rm = TRUE),
                  F3 = sum(F3, na.rm = TRUE),
                  .groups = "drop"
              ) %>%
              rename(CAC_Final = `Línea de Actividad`)
              
      } else if (nivel_agregacion == "modalidad") {
          # Rellenar NAs antes de agrupar
          tabla_final$Modalidad[is.na(tabla_final$Modalidad)] <- "Sin Asignar"
          
          # Agrupar por Modalidad
          tabla_final <- tabla_final %>%
              group_by(Modalidad) %>%
              summarise(
                  CD = sum(CD, na.rm = TRUE),
                  F1 = sum(F1, na.rm = TRUE),
                  F2 = sum(F2, na.rm = TRUE),
                  F3 = sum(F3, na.rm = TRUE),
                  .groups = "drop"
              ) %>%
              rename(CAC_Final = Modalidad)
      }
      
      # Capture checkbox values
      mostrar_linea <- if (!is.null(input$modal_traza_mostrar_linea) && nivel_agregacion == "cac") input$modal_traza_mostrar_linea else FALSE
      mostrar_modalidad <- if (!is.null(input$modal_traza_mostrar_modalidad) && nivel_agregacion == "cac") input$modal_traza_mostrar_modalidad else FALSE

      # Handle column visibility
      # Remove columns if not shown
      if (!mostrar_linea && "Línea de Actividad" %in% names(tabla_final)) {
          tabla_final$`Línea de Actividad` <- NULL
      }
      if (!mostrar_modalidad && "Modalidad" %in% names(tabla_final)) {
          tabla_final$Modalidad <- NULL
      }

      # Ordenar columnas: CAC_Final, [Linea, Modalidad], CD, F1, F2, F3
      cols_metadata <- c("CAC_Final")
      if (mostrar_linea && "Línea de Actividad" %in% names(tabla_final)) {
          cols_metadata <- c(cols_metadata, "Línea de Actividad")
      }
      if (mostrar_modalidad && "Modalidad" %in% names(tabla_final)) {
          cols_metadata <- c(cols_metadata, "Modalidad")
      }
      cols_values <- c("CD", "F1", "F2", "F3")
      
      # Keep any other columns that might exist
      known_cols <- c(cols_metadata, cols_values)
      other_cols <- setdiff(names(tabla_final), known_cols)
      
      tabla_final <- tabla_final[, c(cols_metadata, cols_values, other_cols), drop = FALSE]
      
      # Calcular Total por CAC
      tabla_final$Total <- tabla_final$CD + tabla_final$F1 + tabla_final$F2 + tabla_final$F3
      
      # Calcular porcentajes si se solicita
      if (mostrar_pct) {
        # Evitar división por cero
        total_safe <- ifelse(tabla_final$Total == 0, 1, tabla_final$Total)
        
        tabla_final$Pct_CD <- (tabla_final$CD / total_safe) * 100
        tabla_final$Pct_F1 <- (tabla_final$F1 / total_safe) * 100
        tabla_final$Pct_F2 <- (tabla_final$F2 / total_safe) * 100
        tabla_final$Pct_F3 <- (tabla_final$F3 / total_safe) * 100
        
        # Reordenar columnas
        cols_pct <- c("CD", "Pct_CD", "F1", "Pct_F1", "F2", "Pct_F2", "F3", "Pct_F3", "Total")
        tabla_final <- tabla_final[, c(cols_metadata, cols_pct), drop = FALSE]
      }
      
      # Ordenar por Total descendente
      tabla_final <- tabla_final %>%
        arrange(desc(Total))
      
      if (nrow(tabla_final) == 0) {
        showNotification("No hay datos después de aplicar los filtros", type = "warning", duration = 5)
        traza_data(NULL)
        return()
      }
      
      # Calcular estadísticas
      n_cacs <- nrow(tabla_final)
      importe_total <- sum(tabla_final$Total, na.rm = TRUE)
      importe_cd <- sum(tabla_final$CD, na.rm = TRUE)
      importe_f1 <- sum(tabla_final$F1, na.rm = TRUE)
      importe_f2 <- sum(tabla_final$F2, na.rm = TRUE)
      importe_f3 <- sum(tabla_final$F3, na.rm = TRUE)
      
      showNotification(
        paste0("✓ Traza generada: ", n_cacs, " CACs, Total: ", 
               formatC(importe_total, format = "f", big.mark = " ", digits = 2), "€"),
        type = "message",
        duration = 5
      )
      
      traza_data(list(
        tabla = tabla_final,
        info = list(
          n_cacs = n_cacs,
          importe_total = importe_total,
          importe_cd = importe_cd,
          importe_f1 = importe_f1,
          importe_f2 = importe_f2,
          importe_f3 = importe_f3,
          nivel_agregacion = nivel_agregacion,
          mostrar_pct = mostrar_pct,
          mostrar_linea = mostrar_linea,
          mostrar_modalidad = mostrar_modalidad
        )
      ))
      
    }, error = function(e) {
      showNotification(
        paste("Error al generar la traza:", e$message),
        type = "error",
        duration = 10
      )
      traza_data(NULL)
    })
  })
  
  # ========================================================================
  # OUTPUTS - INFO BOXES
  # ========================================================================
  
  output$traza_info_registros <- renderInfoBox({
    traza <- traza_data()
    n <- if (!is.null(traza)) traza$info$n_cacs else 0
    infoBox(
      "CACs Destino",
      formatC(as.integer(n), format = "d", big.mark = " "),
      icon = icon("building"),
      color = if (n > 0) "blue" else "red"
    )
  })
  
  output$traza_info_fases <- renderInfoBox({
    traza <- traza_data()
    if (!is.null(traza)) {
      # Calcular porcentaje de coste directo
      pct_cd <- if (traza$info$importe_total > 0) {
        round(100 * traza$info$importe_cd / traza$info$importe_total, 1)
      } else {
        0
      }
      texto <- paste0(pct_cd, "% CD")
    } else {
      texto <- "0%"
    }
    
    infoBox(
      "Coste Directo",
      texto,
      icon = icon("bolt"),
      color = if (!is.null(traza) && traza$info$importe_cd > 0) "green" else "red"
    )
  })
  
  output$traza_info_importe <- renderInfoBox({
    traza <- traza_data()
    imp <- if (!is.null(traza)) traza$info$importe_total else 0
    infoBox(
      "Importe Total",
      paste0(formatC(imp, format = "f", big.mark = " ", digits = 2), "€"),
      icon = icon("euro-sign"),
      color = if (imp > 0) "yellow" else "red"
    )
  })
  
  # ========================================================================
  # OUTPUT - TABLA DE TRAZA (PIVOTADA)
  # ========================================================================
  
  output$traza_table <- DT::renderDataTable({
    traza <- traza_data()
    
    if (is.null(traza)) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos para mostrar. Configura los filtros y presiona 'Generar Traza'."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    tabla <- as.data.frame(traza$tabla)
    mostrar_pct <- if (!is.null(traza$info$mostrar_pct)) traza$info$mostrar_pct else FALSE
    mostrar_linea <- if (!is.null(traza$info$mostrar_linea)) traza$info$mostrar_linea else FALSE
    mostrar_modalidad <- if (!is.null(traza$info$mostrar_modalidad)) traza$info$mostrar_modalidad else FALSE
    
    # Formatear importes como texto para display
    tabla_display <- tabla
    tabla_display$CD <- paste0(formatC(tabla$CD, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F1 <- paste0(formatC(tabla$F1, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F2 <- paste0(formatC(tabla$F2, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F3 <- paste0(formatC(tabla$F3, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$Total <- paste0(formatC(tabla$Total, format = "f", big.mark = " ", digits = 2), "€")
    
    if (mostrar_pct) {
      tabla_display$Pct_CD <- paste0(formatC(tabla$Pct_CD, format = "f", digits = 1), "%")
      tabla_display$Pct_F1 <- paste0(formatC(tabla$Pct_F1, format = "f", digits = 1), "%")
      tabla_display$Pct_F2 <- paste0(formatC(tabla$Pct_F2, format = "f", digits = 1), "%")
      tabla_display$Pct_F3 <- paste0(formatC(tabla$Pct_F3, format = "f", digits = 1), "%")
    }

    # Construct column names dynamically
    base_names <- c('CAC Final' = 'CAC_Final')
    
    # Si estamos agrupando por línea o modalidad, cambiamos el nombre de la primera columna
    if (!is.null(traza$info$nivel_agregacion)) {
        if (traza$info$nivel_agregacion == "linea") {
            base_names <- c('Línea de Actividad' = 'CAC_Final')
        } else if (traza$info$nivel_agregacion == "modalidad") {
            base_names <- c('Modalidad' = 'CAC_Final')
        }
    }
    
    extra_names <- c()
    if (mostrar_linea && "Línea de Actividad" %in% names(tabla_display)) {
        extra_names <- c(extra_names, 'Línea de Actividad' = 'Línea de Actividad')
    }
    if (mostrar_modalidad && "Modalidad" %in% names(tabla_display)) {
        extra_names <- c(extra_names, 'Modalidad' = 'Modalidad')
    }

    if (mostrar_pct) {
        value_names <- c(
            'CD' = 'CD', '% CD' = 'Pct_CD',
            'F1' = 'F1', '% F1' = 'Pct_F1',
            'F2' = 'F2', '% F2' = 'Pct_F2',
            'F3' = 'F3', '% F3' = 'Pct_F3',
            'Total' = 'Total'
        )
    } else {
        value_names <- c(
            'Coste Directo (CD)' = 'CD',
            'Fase 1 (F1)' = 'F1',
            'Fase 2 (F2)' = 'F2',
            'Fase 3 (F3)' = 'F3',
            'Total' = 'Total'
        )
    }
    
    col_names <- c(base_names, extra_names, value_names)
    
    # Calculate indices for centering
    n_extra <- length(extra_names)
    n_values <- length(value_names)
    
    targets_left <- c(0)
    if (n_extra > 0) {
        targets_left <- c(targets_left, 1:n_extra)
    }
    
    targets_center <- (n_extra + 1):(n_extra + n_values)
    
    col_defs <- list(
        list(className = 'dt-center', targets = targets_center),
        list(className = 'dt-left', targets = targets_left)
    )
    
    DT::datatable(
      tabla_display,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ CACs",
          info = "Mostrando _START_ a _END_ de _TOTAL_ CACs",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        ),
        columnDefs = col_defs
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover',
      colnames = col_names
    )
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR TABLA
  # ========================================================================
  
  observeEvent(input$expand_traza_table, {
    traza <- traza_data()
    if (is.null(traza)) return()
    
    tabla <- as.data.frame(traza$tabla)
    mostrar_pct <- if (!is.null(traza$info$mostrar_pct)) traza$info$mostrar_pct else FALSE
    mostrar_linea <- if (!is.null(traza$info$mostrar_linea)) traza$info$mostrar_linea else FALSE
    mostrar_modalidad <- if (!is.null(traza$info$mostrar_modalidad)) traza$info$mostrar_modalidad else FALSE
    
    # Formatear importes
    tabla_display <- tabla
    tabla_display$CD <- paste0(formatC(tabla$CD, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F1 <- paste0(formatC(tabla$F1, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F2 <- paste0(formatC(tabla$F2, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$F3 <- paste0(formatC(tabla$F3, format = "f", big.mark = " ", digits = 2), "€")
    tabla_display$Total <- paste0(formatC(tabla$Total, format = "f", big.mark = " ", digits = 2), "€")
    
    if (mostrar_pct) {
      tabla_display$Pct_CD <- paste0(formatC(tabla$Pct_CD, format = "f", digits = 1), "%")
      tabla_display$Pct_F1 <- paste0(formatC(tabla$Pct_F1, format = "f", digits = 1), "%")
      tabla_display$Pct_F2 <- paste0(formatC(tabla$Pct_F2, format = "f", digits = 1), "%")
      tabla_display$Pct_F3 <- paste0(formatC(tabla$Pct_F3, format = "f", digits = 1), "%")
    }

    # Construct column names dynamically
    base_names <- c('CAC Final' = 'CAC_Final')
    
    # Si estamos agrupando por línea o modalidad, cambiamos el nombre de la primera columna
    if (!is.null(traza$info$nivel_agregacion)) {
        if (traza$info$nivel_agregacion == "linea") {
            base_names <- c('Línea de Actividad' = 'CAC_Final')
        } else if (traza$info$nivel_agregacion == "modalidad") {
            base_names <- c('Modalidad' = 'CAC_Final')
        }
    }
    
    extra_names <- c()
    if (mostrar_linea && "Línea de Actividad" %in% names(tabla_display)) {
        extra_names <- c(extra_names, 'Línea de Actividad' = 'Línea de Actividad')
    }
    if (mostrar_modalidad && "Modalidad" %in% names(tabla_display)) {
        extra_names <- c(extra_names, 'Modalidad' = 'Modalidad')
    }

    if (mostrar_pct) {
        value_names <- c(
            'CD' = 'CD', '% CD' = 'Pct_CD',
            'F1' = 'F1', '% F1' = 'Pct_F1',
            'F2' = 'F2', '% F2' = 'Pct_F2',
            'F3' = 'F3', '% F3' = 'Pct_F3',
            'Total' = 'Total'
        )
    } else {
        value_names <- c(
            'Coste Directo (CD)' = 'CD',
            'Fase 1 (F1)' = 'F1',
            'Fase 2 (F2)' = 'F2',
            'Fase 3 (F3)' = 'F3',
            'Total' = 'Total'
        )
    }
    
    col_names <- c(base_names, extra_names, value_names)
    
    # Calculate indices for centering
    n_extra <- length(extra_names)
    n_values <- length(value_names)
    
    targets_left <- c(0)
    if (n_extra > 0) {
        targets_left <- c(targets_left, 1:n_extra)
    }
    
    targets_center <- (n_extra + 1):(n_extra + n_values)
    
    col_defs <- list(
        list(className = 'dt-center', targets = targets_center),
        list(className = 'dt-left', targets = targets_left)
    )
    
    showModal(modalDialog(
      title = "Análisis de Costes por CAC - Vista Expandida",
      DT::dataTableOutput("traza_table_expanded"),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
    
    output$traza_table_expanded <- DT::renderDataTable({
      DT::datatable(
        tabla_display,
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100, 200),
          scrollX = TRUE,
          scrollY = "600px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ CACs",
            info = "Mostrando _START_ a _END_ de _TOTAL_ CACs",
            paginate = list(previous = "Anterior", `next` = "Siguiente")
          ),
          columnDefs = col_defs
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover',
        colnames = col_names
      )
    })
  })
  
  # ========================================================================
  # DOWNLOAD HANDLER - TABLA CSV
  # ========================================================================
  
  output$download_traza_csv <- downloadHandler(
    filename = function() {
      paste0("analisis_costes_cac_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      traza <- traza_data()
      if (is.null(traza)) return()
      
      tabla <- as.data.frame(traza$tabla)
      mostrar_pct <- if (!is.null(traza$info$mostrar_pct)) traza$info$mostrar_pct else FALSE
      
      # Renombrar columnas para mejor comprensión
      # Si hay columnas extra, se mantienen con su nombre
      
      if (mostrar_pct) {
        # Si hay porcentajes, los nombres ya son descriptivos (Pct_CD, etc)
        # Solo ajustamos los principales si es necesario
        names(tabla)[names(tabla) == "CAC_Final"] <- "CAC_Final"
        names(tabla)[names(tabla) == "CD"] <- "Coste_Directo_CD"
        names(tabla)[names(tabla) == "F1"] <- "Fase_1_F1"
        names(tabla)[names(tabla) == "F2"] <- "Fase_2_F2"
        names(tabla)[names(tabla) == "F3"] <- "Fase_3_F3"
      } else {
        # Si no hay porcentajes, renombramos las columnas de valores
        # Pero cuidado con las columnas extra
        names(tabla)[names(tabla) == "CAC_Final"] <- "CAC_Final"
        names(tabla)[names(tabla) == "CD"] <- "Coste_Directo_CD"
        names(tabla)[names(tabla) == "F1"] <- "Fase_1_F1"
        names(tabla)[names(tabla) == "F2"] <- "Fase_2_F2"
        names(tabla)[names(tabla) == "F3"] <- "Fase_3_F3"
        # Total ya se llama Total
      }
      
      write.csv2(tabla, file, row.names = FALSE, fileEncoding = "latin1")
    }
  )
  
  return(traza_data)
}
