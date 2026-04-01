# ============================================================================
# SANKEY SERVER LOGIC
# ============================================================================

#' Asignar color a parámetro en enlaces Sankey
asignar_color_parametro <- function(parametro, tipo_reparto) {
  if (is.na(parametro) || parametro == "" || parametro == "NO PARAM") {
    return("rgba(150, 150, 150, 0.25)")
  }
  parametro_hash <- sum(utf8ToInt(as.character(parametro))) %% 12
  colores <- c(
    "rgba(59, 130, 246, 0.5)", "rgba(16, 185, 129, 0.5)", "rgba(245, 158, 11, 0.5)",
    "rgba(239, 68, 68, 0.5)",  "rgba(168, 85, 247, 0.5)", "rgba(236, 72, 153, 0.5)",
    "rgba(20, 184, 166, 0.5)", "rgba(251, 146, 60, 0.5)", "rgba(139, 92, 246, 0.5)",
    "rgba(34, 197, 94, 0.5)",  "rgba(248, 113, 113, 0.5)","rgba(96, 165, 250, 0.5)"
  )
  colores[parametro_hash + 1]
}

#' Crear plot de Sankey reutilizable
crear_plot_sankey <- function(sankey, font_size = 11) {
  if (is.null(sankey)) {
    return(plotly_empty() %>%
      layout(title = list(text = "Configure los filtros y presione 'Generar'",
                          font = list(size = 18))))
  }

  if (sankey$mostrar_parametro && "parametro" %in% names(sankey$links)) {
    link_colors <- vapply(seq_len(nrow(sankey$links)), function(i) {
      asignar_color_parametro(sankey$links$parametro[i], sankey$links$tipo_reparto[i])
    }, character(1))
    hover_template <- paste0(
      "Importe: %{value:,.2f}€<br>Parámetro: ",
      ifelse(is.na(sankey$links$parametro) | sankey$links$parametro == "",
             "Sin parámetro", sankey$links$parametro),
      "<extra></extra>"
    )
  } else {
    link_colors    <- "rgba(100, 116, 139, 0.3)"
    hover_template <- "Importe: %{value:,.2f}€<extra></extra>"
  }

  plot_ly(
    type = "sankey", orientation = "h", arrangement = "snap",
    node = list(
      label = sankey$nodes$name,
      color = "rgba(31, 119, 180, 0.8)",
      pad = 18, thickness = 25,
      line = list(color = "rgba(255,255,255,0.8)", width = 1.5),
      hovertemplate = "%{label}<extra></extra>"
    ),
    link = list(
      source = sankey$links$source,
      target = sankey$links$target,
      value  = sankey$links$value,
      color  = link_colors,
      hovertemplate = hover_template
    )
  ) %>%
    layout(
      font = list(family = "Inter, sans-serif", size = font_size, color = "#1e293b"),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 30, b = 10)
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

#' Create Sankey server logic
create_sankey_server <- function(input, output, session, datos_raw, active_panel) {

  sankey_data <- reactiveVal(NULL)

  observe({
    if (!is.null(active_panel()) && active_panel() != "sankey") {
      sankey_data(NULL)
      gc()
    }
  })

  # ---- Capturar filtros del sidebar (función auxiliar) ----
  .get_filtros <- function() {
    list(
      centro_gestor      = if (!is.null(input$centro_gestor) && input$centro_gestor != "")
                             input$centro_gestor else NULL,
      nivel_agregacion   = if (!is.null(input$nivel_agregacion)) input$nivel_agregacion else "cac",
      cac_subcac_origen  = if (!is.null(input$origen) && length(input$origen) > 0 &&
                               input$origen[1] != "") input$origen else NULL,
      cac_subcac_destino = if (!is.null(input$destino) && length(input$destino) > 0 &&
                               input$destino[1] != "") input$destino else NULL,
      fase1              = if (!is.null(input$fase1) && length(input$fase1) > 0 &&
                               input$fase1[1] != "") input$fase1 else NULL,
      fase2              = if (!is.null(input$fase2) && length(input$fase2) > 0 &&
                               input$fase2[1] != "") input$fase2 else NULL,
      mes                = if (!is.null(input$mes) && length(input$mes) > 0 &&
                               input$mes[1] != "") as.numeric(input$mes) else NULL,
      anyo               = if (!is.null(input$anyo) && length(input$anyo) > 0 &&
                               input$anyo[1] != "") as.numeric(input$anyo) else NULL,
      fases_incluir      = if (!is.null(input$fases_filter) && length(input$fases_filter) > 0)
                             as.numeric(input$fases_filter) else c(1, 2, 3),
      excluir_estaticos  = {
        base <- if (!is.null(input$mostrar_estaticos)) !input$mostrar_estaticos else TRUE
        if (!is.null(input$modal_excluir_estaticos)) !input$modal_excluir_estaticos else base
      },
      mostrar_parametro  = {
        base <- if (!is.null(input$sankey_mostrar_parametro)) input$sankey_mostrar_parametro else FALSE
        if (!is.null(input$modal_mostrar_parametro)) input$modal_mostrar_parametro else base
      }
    )
  }

  # ========================================================================
  # GENERAR SANKEY
  # ========================================================================

  observeEvent(input$apply_filters, {
    if (is.null(active_panel()) || active_panel() != "sankey") return()

    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados.", type = "error", duration = 5)
      sankey_data(NULL)
      return()
    }

    f <- .get_filtros()

    # construir_enlaces_sankey ya está disponible (cargado en dynamic_server.R)
    resultado <- tryCatch(
      construir_enlaces_sankey(
        datos              = datos,
        centro_gestor      = f$centro_gestor,
        nivel_agregacion   = f$nivel_agregacion,
        cac_subcac_origen  = f$cac_subcac_origen,
        cac_subcac_destino = f$cac_subcac_destino,
        fase1_filter       = f$fase1,
        fase2_filter       = f$fase2,
        mes                = f$mes,
        anyo               = f$anyo,
        fases_incluir      = f$fases_incluir,
        excluir_estaticos  = f$excluir_estaticos,
        verbose            = TRUE
      ),
      error = function(e) {
        showNotification(paste("Error al construir Sankey:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )

    if (is.null(resultado)) { sankey_data(NULL); return() }

    enlaces_df <- resultado$enlaces
    if (is.null(enlaces_df) || nrow(enlaces_df) == 0) {
      showNotification("Sin enlaces con los filtros seleccionados.", type = "warning", duration = 5)
      sankey_data(NULL)
      return()
    }

    # Agregar enlaces
    if (f$mostrar_parametro) {
      enlaces_agregados <- enlaces_df %>%
        group_by(source, target, parametro, tipo_reparto) %>%
        summarise(value = sum(value, na.rm = TRUE), n_registros = n(), .groups = "drop") %>%
        filter(value > 0)
    } else {
      enlaces_agregados <- enlaces_df %>%
        group_by(source, target) %>%
        summarise(
          value        = sum(value, na.rm = TRUE),
          n_registros  = n(),
          parametro    = NA_character_,
          tipo_reparto = if (dplyr::n_distinct(tipo_reparto) > 1) "AGREGADO" else dplyr::first(tipo_reparto),
          .groups = "drop"
        ) %>%
        filter(value > 0)
    }

    nodos <- unique(c(enlaces_agregados$source, enlaces_agregados$target))
    nodes_df <- data.frame(name = nodos, stringsAsFactors = FALSE)
    links_df <- enlaces_agregados %>%
      mutate(source = match(source, nodes_df$name) - 1L,
             target = match(target, nodes_df$name) - 1L) %>%
      select(source, target, value, parametro, tipo_reparto)

    fase_maxima <- max(enlaces_df$fase, na.rm = TRUE)
    importe_total <- sum(enlaces_df$value[enlaces_df$fase == fase_maxima], na.rm = TRUE)

    showNotification(paste("✓ Sankey:", nrow(links_df), "enlaces únicos"),
                     type = "message", duration = 3)

    sankey_data(list(
      nodes             = nodes_df,
      links             = links_df,
      enlaces_detallados = enlaces_df,
      enlaces_agregados  = enlaces_agregados,
      matriz_movimientos = resultado$matriz,
      mostrar_parametro  = f$mostrar_parametro,
      info = list(
        n_nodos              = nrow(nodes_df),
        n_enlaces            = nrow(links_df),
        n_enlaces_detallados = nrow(enlaces_df),
        importe_total        = importe_total,
        fase_maxima          = fase_maxima,
        fases_procesadas     = unique(enlaces_df$fase),
        tipos_reparto        = table(enlaces_df$tipo_reparto)
      )
    ))
  })

  # ========================================================================
  # INFO BOXES
  # ========================================================================

  output$sankey_info_nodos <- renderInfoBox({
    n <- if (!is.null(sankey_data())) sankey_data()$info$n_nodos else 0L
    infoBox("Nodos", formatC(as.integer(n), format = "d", big.mark = " "),
            icon = icon("dot-circle"), color = if (n > 0) "blue" else "red")
  })

  output$sankey_info_enlaces <- renderInfoBox({
    n <- if (!is.null(sankey_data())) sankey_data()$info$n_enlaces else 0L
    infoBox("Enlaces Únicos", formatC(as.integer(n), format = "d", big.mark = " "),
            icon = icon("arrows-left-right"), color = if (n > 0) "purple" else "red")
  })

  output$sankey_info_importe <- renderInfoBox({
    sankey <- sankey_data()
    imp <- if (!is.null(sankey)) sankey$info$importe_total else 0
    fase_lbl <- if (!is.null(sankey)) paste0(" (Fase ", sankey$info$fase_maxima, ")") else ""
    infoBox(
      paste0("Importe Total", fase_lbl),
      paste0(formatC(imp, format = "f", big.mark = " ", digits = 2), "€"),
      icon = icon("euro-sign"),
      color = if (imp > 0) "yellow" else "red"
    )
  })

  # ========================================================================
  # DIAGRAMA
  # ========================================================================

  output$sankey_diagram          <- renderPlotly({ crear_plot_sankey(sankey_data(), 11) })
  output$sankey_diagram_expanded <- renderPlotly({ crear_plot_sankey(sankey_data(), 11) })

  # ========================================================================
  # TABLAS
  # ========================================================================

  .enlaces_display <- function(sankey) {
    if (is.null(sankey)) return(data.frame(Mensaje = "Genera el diagrama primero"))
    enlaces <- as.data.frame(sankey$enlaces_detallados)
    enlaces %>%
      mutate(value_fmt = paste0(formatC(value, format = "f", big.mark = " ", digits = 2), "€")) %>%
      select(Fase = fase, Origen = source, Destino = target,
             Tipo = tipo_reparto, Parámetro = parametro, Importe = value_fmt)
  }

  output$sankey_enlaces_table <- DT::renderDataTable({
    df <- .enlaces_display(sankey_data())
    DT::datatable(df,
      options = list(scrollX = TRUE, scrollY = "350px", paging = TRUE, pageLength = 15,
                     dom = "frtip", language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
      rownames = FALSE, class = "stripe hover compact"
    )
  })

  output$sankey_resumen_table <- DT::renderDataTable({
    sankey <- sankey_data()
    if (is.null(sankey)) {
      return(DT::datatable(data.frame(Mensaje = "Genera el diagrama primero"),
                           options = list(dom = "t"), rownames = FALSE))
    }
    enlaces <- as.data.frame(sankey$enlaces_detallados)
    resumen <- enlaces %>%
      group_by(fase, origen) %>%
      summarise(Importe = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(fase, origen) %>%
      mutate(Importe_fmt = paste0(formatC(Importe, format = "f", big.mark = " ", digits = 2), "€")) %>%
      select(Fase = fase, `Origen (Fase 0)` = origen, `Importe Acumulado` = Importe_fmt)

    DT::datatable(resumen,
      options = list(scrollX = TRUE, scrollY = "350px", paging = TRUE, pageLength = 15,
                     dom = "frtip", language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
      rownames = FALSE, class = "stripe hover compact"
    )
  })

  # ========================================================================
  # MODALES DE EXPANSIÓN
  # ========================================================================

  observeEvent(input$expand_sankey, {
    if (is.null(sankey_data())) return()
    showModal(modalDialog(
      title = NULL,
      plotlyOutput("sankey_diagram_expanded", width = "100%"),
      easyClose = TRUE, size = "xl", footer = NULL
    ))
  })

  observeEvent(input$expand_enlaces_table, {
    sankey <- sankey_data()
    if (is.null(sankey)) return()
    df <- .enlaces_display(sankey)
    showModal(modalDialog(
      title = "Enlaces Detallados — Vista Expandida",
      div(style = "height:600px;overflow:auto;",
          DT::dataTableOutput("sankey_enlaces_table_expanded", width = "100%")),
      easyClose = TRUE, size = "xl", footer = NULL
    ))
    output$sankey_enlaces_table_expanded <- DT::renderDataTable({
      DT::datatable(df,
        options = list(scrollX = TRUE, autoWidth = FALSE, pageLength = 25,
                       language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE, class = "stripe hover compact"
      )
    })
  })

  # ========================================================================
  # DESCARGA
  # ========================================================================

  output$download_sankey <- downloadHandler(
    filename = function() paste0("sankey_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content  = function(file) {
      sankey <- sankey_data()
      if (is.null(sankey)) {
        showNotification("No hay diagrama para descargar.", type = "warning"); return()
      }
      htmlwidgets::saveWidget(crear_plot_sankey(sankey, 14), file, selfcontained = TRUE)
    }
  )

  sankey_data
}