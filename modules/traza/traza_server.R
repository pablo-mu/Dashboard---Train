# ============================================================================
# TRAZA SERVER LOGIC
# ============================================================================

create_traza_server <- function(input, output, session, datos_raw, active_panel) {

  traza_data <- reactiveVal(NULL)

  # Cargar mapping CAC → Linea/Modalidad (reactivo con cache simple)
  mapping_cac <- reactive({
    tryCatch({
      df <- readxl::read_excel(".data/linea_actividad_unificada.xlsx")
      df$CAC <- as.character(df$CAC)
      df
    }, error = function(e) NULL)
  })

  observe({
    if (!is.null(active_panel()) && active_panel() != "traza") {
      traza_data(NULL)
      gc()
    }
  })

  # ---- UI dinámicos para filtros de modal ----
  output$ui_modal_traza_linea <- renderUI({
    if (is.null(input$modal_traza_nivel_agregacion) ||
        input$modal_traza_nivel_agregacion != "cac") return(NULL)
    map_data <- mapping_cac()
    choices  <- c("Todos", if (!is.null(map_data))
                    sort(unique(map_data$`Línea de Actividad`)))
    selectInput("modal_traza_linea", "Línea de Actividad:", choices, "Todos")
  })

  output$ui_modal_traza_modalidad <- renderUI({
    if (is.null(input$modal_traza_nivel_agregacion) ||
        input$modal_traza_nivel_agregacion != "cac") return(NULL)
    map_data <- mapping_cac()
    choices  <- c("Todos", if (!is.null(map_data)) sort(unique(map_data$Modalidad)))
    selectInput("modal_traza_modalidad", "Modalidad:", choices, "Todos")
  })

  # ========================================================================
  # GENERAR TRAZA
  # ========================================================================

  observeEvent(input$apply_filters, {
    if (is.null(active_panel()) || active_panel() != "traza") return()

    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados.", type = "error", duration = 5)
      traza_data(NULL)
      return()
    }

    # ---- Filtros sidebar ----
    centro_gestor_val <- if (!is.null(input$centro_gestor) && input$centro_gestor != "")
                           input$centro_gestor else NULL
    cac_origen_val    <- if (!is.null(input$origen) && length(input$origen) > 0 &&
                             input$origen[1] != "") input$origen else NULL
    cac_destino_val   <- if (!is.null(input$destino) && length(input$destino) > 0 &&
                             input$destino[1] != "") input$destino else NULL
    fase1_val         <- if (!is.null(input$fase1) && length(input$fase1) > 0 &&
                             input$fase1[1] != "") input$fase1 else NULL
    fase2_val         <- if (!is.null(input$fase2) && length(input$fase2) > 0 &&
                             input$fase2[1] != "") input$fase2 else NULL
    mes_val           <- if (!is.null(input$mes) && length(input$mes) > 0 &&
                             input$mes[1] != "") as.numeric(input$mes) else NULL
    anyo_val          <- if (!is.null(input$anyo) && length(input$anyo) > 0 &&
                             input$anyo[1] != "") as.numeric(input$anyo) else NULL

    # ---- Opciones del modal ----
    nivel_agregacion <- if (!is.null(input$modal_traza_nivel_agregacion))
                          input$modal_traza_nivel_agregacion else "cac"
    mostrar_pct      <- isTRUE(input$modal_traza_mostrar_porcentajes)

    tryCatch({
      # Filtrar siempre a nivel CAC (3 dígitos) para el cálculo
      datos_filtrados <- filtrar_datos_reparto(
        datos, centro_gestor = centro_gestor_val,
        cacs_origen = cac_origen_val, subcacs_origen = NULL,
        mes = mes_val, anyo = anyo_val, nivel_agregacion = "cac"
      )

      if (is.null(datos_filtrados) || nrow(datos_filtrados) == 0) {
        showNotification("Sin datos tras los filtros.", type = "warning", duration = 5)
        traza_data(NULL)
        return()
      }

      # extraer_movimientos ya cargada
      matriz <- extraer_movimientos(
        datos_filtrados, nivel_agregacion = "cac",
        fases_incluir = c(1, 2, 3), incluir_tipo_param = FALSE,
        verbose = TRUE, debug = FALSE
      )

      if (is.null(matriz) || nrow(matriz) == 0) {
        showNotification("No se pudo generar la matriz de movimientos.", type = "error", duration = 5)
        traza_data(NULL)
        return()
      }

      # ---- Aplicar nivel de agregación ----
      aplicar_nivel <- function(columna, nivel) {
        if (nivel == "cac1") return(ifelse(!is.na(columna) & columna != "",
                                           substr(columna, 1, 1), columna))
        if (nivel == "cac2") return(ifelse(!is.na(columna) & columna != "",
                                           substr(columna, 1, 2), columna))
        columna
      }

      # Usar data.table para la transformación
      matriz_df <- as.data.frame(matriz)
      for (col in c("fase_0","fase_1","fase_2","fase_3")) {
        if (col %in% names(matriz_df))
          matriz_df[[col]] <- aplicar_nivel(matriz_df[[col]], nivel_agregacion)
      }

      # ---- Expandir y aplicar filtros de fase ----
      if (!is.null(fase1_val))
        fase1_val <- expandir_seleccion_cac(fase1_val, unique(matriz_df$fase_1))
      if (!is.null(fase2_val))
        fase2_val <- expandir_seleccion_cac(fase2_val, unique(matriz_df$fase_2))
      if (!is.null(cac_destino_val))
        cac_destino_val <- expandir_seleccion_cac(cac_destino_val, unique(matriz_df$fase_3))

      filtrar_fase <- function(df, col, vals) {
        if (is.null(vals) || !col %in% names(df)) return(df)
        df[df[[col]] %in% vals, ]
      }
      matriz_df <- filtrar_fase(matriz_df, "fase_1", fase1_val)
      matriz_df <- filtrar_fase(matriz_df, "fase_2", fase2_val)
      matriz_df <- filtrar_fase(matriz_df, "fase_3", cac_destino_val)

      if (nrow(matriz_df) == 0) {
        showNotification("Sin datos tras filtros de fase.", type = "warning", duration = 5)
        traza_data(NULL)
        return()
      }

      # ---- Tipo de llegada — vectorizado con data.table ----
      mdt <- as.data.table(matriz_df)
      mdt[, tipo_llegada := "F3"]
      mdt[!is.na(fase_0) & !is.na(fase_3) & fase_0 == fase_3,        tipo_llegada := "CD"]
      mdt[!is.na(fase_1) & !is.na(fase_3) & fase_1 == fase_3 &
          tipo_llegada != "CD",                                         tipo_llegada := "F1"]
      mdt[!is.na(fase_2) & !is.na(fase_3) & fase_2 == fase_3 &
          !tipo_llegada %in% c("CD","F1"),                              tipo_llegada := "F2"]

      # Pivot con data.table (más rápido que tidyr para datos grandes)
      tabla_pivot <- mdt[, .(importe = sum(importe, na.rm = TRUE)), by = .(fase_3, tipo_llegada)]
      tabla_wide  <- dcast(tabla_pivot, fase_3 ~ tipo_llegada, value.var = "importe", fill = 0)

      # Asegurar columnas CD, F1, F2, F3
      for (col in c("CD","F1","F2","F3")) {
        if (!col %in% names(tabla_wide)) tabla_wide[[col]] <- 0
      }
      setnames(tabla_wide, "fase_3", "CAC_Final")

      # ---- Join con mapping (si nivel es cac / linea / modalidad) ----
      if (nivel_agregacion %in% c("cac","linea","modalidad")) {
        map_data <- mapping_cac()
        if (!is.null(map_data)) {
          tabla_wide$CAC_Final <- as.character(tabla_wide$CAC_Final)
          tabla_wide <- merge(tabla_wide, as.data.table(map_data),
                              by.x = "CAC_Final", by.y = "CAC", all.x = TRUE)

          # Filtros de línea / modalidad (solo nivel cac)
          if (nivel_agregacion == "cac") {
            if (!is.null(input$modal_traza_linea) && input$modal_traza_linea != "Todos")
              tabla_wide <- tabla_wide[`Línea de Actividad` == input$modal_traza_linea]
            if (!is.null(input$modal_traza_modalidad) && input$modal_traza_modalidad != "Todos")
              tabla_wide <- tabla_wide[Modalidad == input$modal_traza_modalidad]
          }
        }
      }

      # Agregación especial Línea / Modalidad
      if (nivel_agregacion == "linea") {
        if ("Línea de Actividad" %in% names(tabla_wide)) {
          tabla_wide[is.na(`Línea de Actividad`), `Línea de Actividad` := "Sin Asignar"]
          tabla_wide <- tabla_wide[, .(CD = sum(CD), F1 = sum(F1), F2 = sum(F2), F3 = sum(F3)),
                                   by = .(`Línea de Actividad`)]
          setnames(tabla_wide, "Línea de Actividad", "CAC_Final")
        }
      } else if (nivel_agregacion == "modalidad") {
        if ("Modalidad" %in% names(tabla_wide)) {
          tabla_wide[is.na(Modalidad), Modalidad := "Sin Asignar"]
          tabla_wide <- tabla_wide[, .(CD = sum(CD), F1 = sum(F1), F2 = sum(F2), F3 = sum(F3)),
                                   by = .(Modalidad)]
          setnames(tabla_wide, "Modalidad", "CAC_Final")
        }
      }

      tabla_final <- as.data.frame(tabla_wide)

      # Visibilidad de columnas extra
      mostrar_linea     <- isTRUE(input$modal_traza_mostrar_linea)    && nivel_agregacion == "cac"
      mostrar_modalidad <- isTRUE(input$modal_traza_mostrar_modalidad) && nivel_agregacion == "cac"

      if (!mostrar_linea     && "Línea de Actividad" %in% names(tabla_final))
        tabla_final$`Línea de Actividad` <- NULL
      if (!mostrar_modalidad && "Modalidad" %in% names(tabla_final))
        tabla_final$Modalidad <- NULL

      # Ordenar columnas
      cols_meta   <- c("CAC_Final",
                       if (mostrar_linea     && "Línea de Actividad" %in% names(tabla_final)) "Línea de Actividad",
                       if (mostrar_modalidad && "Modalidad"          %in% names(tabla_final)) "Modalidad")
      cols_meta   <- cols_meta[!sapply(cols_meta, is.null)]
      cols_values <- c("CD","F1","F2","F3")
      cols_rest   <- setdiff(names(tabla_final), c(cols_meta, cols_values))
      tabla_final <- tabla_final[, c(cols_meta, cols_values, cols_rest), drop = FALSE]

      # Total
      tabla_final$Total <- tabla_final$CD + tabla_final$F1 + tabla_final$F2 + tabla_final$F3

      # Porcentajes
      if (mostrar_pct) {
        total_safe <- ifelse(tabla_final$Total == 0, 1, tabla_final$Total)
        tabla_final$Pct_CD <- tabla_final$CD / total_safe * 100
        tabla_final$Pct_F1 <- tabla_final$F1 / total_safe * 100
        tabla_final$Pct_F2 <- tabla_final$F2 / total_safe * 100
        tabla_final$Pct_F3 <- tabla_final$F3 / total_safe * 100
        tabla_final <- tabla_final[, c(cols_meta, "CD","Pct_CD","F1","Pct_F1",
                                        "F2","Pct_F2","F3","Pct_F3","Total"), drop = FALSE]
      }

      # Ordenar por Total desc
      tabla_final <- tabla_final[order(-tabla_final$Total), ]

      if (nrow(tabla_final) == 0) {
        showNotification("Sin datos tras todos los filtros.", type = "warning", duration = 5)
        traza_data(NULL)
        return()
      }

      showNotification(
        paste0("✓ Traza: ", nrow(tabla_final), " destinos, Total: ",
               formatC(sum(tabla_final$Total), format = "f", big.mark = " ", digits = 2), "€"),
        type = "message", duration = 5
      )

      traza_data(list(
        tabla = tabla_final,
        info  = list(
          n_cacs            = nrow(tabla_final),
          importe_total     = sum(tabla_final$Total, na.rm = TRUE),
          importe_cd        = sum(tabla_final$CD,    na.rm = TRUE),
          importe_f1        = sum(tabla_final$F1,    na.rm = TRUE),
          importe_f2        = sum(tabla_final$F2,    na.rm = TRUE),
          importe_f3        = sum(tabla_final$F3,    na.rm = TRUE),
          nivel_agregacion  = nivel_agregacion,
          mostrar_pct       = mostrar_pct,
          mostrar_linea     = mostrar_linea,
          mostrar_modalidad = mostrar_modalidad
        )
      ))

    }, error = function(e) {
      showNotification(paste("Error al generar la traza:", e$message),
                       type = "error", duration = 10)
      traza_data(NULL)
    })
  })

  # ========================================================================
  # INFO BOXES
  # ========================================================================

  output$traza_info_registros <- renderInfoBox({
    n <- if (!is.null(traza_data())) traza_data()$info$n_cacs else 0
    infoBox("CACs Destino", formatC(as.integer(n), format = "d", big.mark = " "),
            icon = icon("building"), color = if (n > 0) "blue" else "red")
  })

  output$traza_info_fases <- renderInfoBox({
    traza <- traza_data()
    texto <- if (!is.null(traza) && traza$info$importe_total > 0) {
      paste0(round(100 * traza$info$importe_cd / traza$info$importe_total, 1), "% CD")
    } else "0%"
    infoBox("Coste Directo", texto, icon = icon("bolt"),
            color = if (!is.null(traza) && traza$info$importe_cd > 0) "green" else "red")
  })

  output$traza_info_importe <- renderInfoBox({
    imp <- if (!is.null(traza_data())) traza_data()$info$importe_total else 0
    infoBox("Importe Total", paste0(formatC(imp, format = "f", big.mark = " ", digits = 2), "€"),
            icon = icon("euro-sign"), color = if (imp > 0) "yellow" else "red")
  })

  # ========================================================================
  # TABLA (helper reutilizable)
  # ========================================================================

  .build_datatable <- function(traza, height = "500px", page_len = 25) {
    if (is.null(traza)) {
      return(DT::datatable(
        data.frame(Mensaje = "Configura los filtros y presiona 'Generar Traza'."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    tabla        <- as.data.frame(traza$tabla)
    mostrar_pct  <- isTRUE(traza$info$mostrar_pct)
    nivel        <- traza$info$nivel_agregacion

    tabla_d <- tabla
    for (col in c("CD","F1","F2","F3","Total"))
      tabla_d[[col]] <- paste0(formatC(tabla[[col]], format = "f", big.mark = " ", digits = 2), "€")
    if (mostrar_pct)
      for (col in c("Pct_CD","Pct_F1","Pct_F2","Pct_F3"))
        tabla_d[[col]] <- paste0(formatC(tabla[[col]], format = "f", digits = 1), "%")

    # Nombres de columnas dinámicos
    base_name <- switch(nivel, linea = "Línea de Actividad", modalidad = "Modalidad", "CAC Final")
    base_names <- setNames("CAC_Final", base_name)

    extra_names <- c()
    if (isTRUE(traza$info$mostrar_linea) && "Línea de Actividad" %in% names(tabla_d))
      extra_names <- c(extra_names, `Línea de Actividad` = "Línea de Actividad")
    if (isTRUE(traza$info$mostrar_modalidad) && "Modalidad" %in% names(tabla_d))
      extra_names <- c(extra_names, `Modalidad` = "Modalidad")

    value_names <- if (mostrar_pct) {
      c("CD"="CD","% CD"="Pct_CD","F1"="F1","% F1"="Pct_F1",
        "F2"="F2","% F2"="Pct_F2","F3"="F3","% F3"="Pct_F3","Total"="Total")
    } else {
      c("Coste Directo (CD)"="CD","Fase 1 (F1)"="F1","Fase 2 (F2)"="F2","Fase 3 (F3)"="F3","Total"="Total")
    }
    col_names <- c(base_names, extra_names, value_names)

    n_extra   <- length(extra_names)
    n_values  <- length(value_names)
    t_left    <- c(0, if (n_extra > 0) seq_len(n_extra))
    t_center  <- (n_extra + 1):(n_extra + n_values)

    DT::datatable(
      tabla_d,
      options = list(
        pageLength = page_len, lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE, scrollY = height,
        dom = "Bfrtip", buttons = c("copy","csv","excel"),
        language = list(search = "Buscar:", info = "Mostrando _START_ a _END_ de _TOTAL_",
                        paginate = list(previous = "Anterior", `next` = "Siguiente")),
        columnDefs = list(
          list(className = "dt-center", targets = t_center),
          list(className = "dt-left",   targets = t_left)
        )
      ),
      rownames = FALSE, class = "cell-border stripe hover",
      colnames = col_names
    )
  }

  output$traza_table <- DT::renderDataTable({ .build_datatable(traza_data(), "500px", 25) })

  # ---- Modal expandido ----
  observeEvent(input$expand_traza_table, {
    traza <- traza_data()
    if (is.null(traza)) return()
    showModal(modalDialog(
      title = "Análisis de Costes — Vista Expandida",
      DT::dataTableOutput("traza_table_expanded"),
      size = "l", easyClose = TRUE, footer = NULL
    ))
    output$traza_table_expanded <- DT::renderDataTable({
      .build_datatable(traza, "600px", 50)
    })
  })

  # ========================================================================
  # DESCARGA CSV
  # ========================================================================

  output$download_traza_csv <- downloadHandler(
    filename = function() paste0("traza_costes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      traza <- traza_data()
      if (is.null(traza)) return()
      tabla <- as.data.frame(traza$tabla)
      names(tabla)[names(tabla) == "CAC_Final"] <- "CAC_Final"
      for (old in c("CD","F1","F2","F3"))
        names(tabla)[names(tabla) == old] <- c(CD="Coste_Directo", F1="Fase_1",
                                                F2="Fase_2", F3="Fase_3")[old]
      write.csv2(tabla, file, row.names = FALSE, fileEncoding = "latin1")
    }
  )

  traza_data
}