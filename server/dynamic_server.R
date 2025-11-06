create_dynamic_server <- function(input, output,
  session, panel_definitions, default_panel
){
  # active panel
  active_panel <- shiny::reactiveVal(default_panel)

  shiny::observeEvent(input$sankey, { active_panel("sankey")})
  shiny::observeEvent(input$matriz_costes, { active_panel("matriz_costes")})
  shiny::observeEvent(input$diagnostics, { active_panel("diagnostics")})

  # Renderizamos el botón de generar diagrama según el panel activo
  output$dynamic_filter_btn <- shiny::renderUI({
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
  })

  # Renderizamos el contenido principal según el panel activo
  output$dynamic_content <- shiny::renderUI({
    panel <- active_panel()

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
    )
  })

  # Modal para generar diagrama y aplicar filtros propios de cada panel
  shiny::observeEvent(input$show_filters, {
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
  return(active_panel)
}