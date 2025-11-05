# =============================================================================
# DYNAMIC CONTENT SERVER MODULE
# =============================================================================

create_dynamic_server <- function(input, output, session, panel_definitions, default_panel) {
  
  # Variable reactiva para el panel activo
  active_panel <- reactiveVal(default_panel)
  
  # Observadores para los bsButton
  observeEvent(input$patients, { active_panel("patients") })
  observeEvent(input$antimicrobials, { 
    active_panel("antimicrobials")
    # Crear las cajas específicas de antimicrobianos cuando se active el panel
    create_antimicrobials_boxes(output)
  })
  observeEvent(input$diagnostics, { active_panel("diagnostics") })
  observeEvent(input$outcome, { active_panel("outcome") })

  # Renderiza el botón de filtro según el panel activo
  output$dynamic_filter_btn <- renderUI({
    panel <- active_panel()
    panel_config <- panel_definitions[[panel]]
    
    if (!is.null(panel_config)) {
      bsButton("show_filters", 
               panel_config$button_label, 
               icon = icon(panel_config$icon), 
               style = panel_config$button_style)
    } else {
      bsButton("show_filters", "FILTROS", icon = icon("filter"), style = "default")
    }
  })
  
  # Renderiza el contenido principal según el panel activo
  output$dynamic_content <- renderUI({
    panel <- active_panel()
    
    switch(panel,
      "antimicrobials" = create_antimicrobials_content(),
      "patients" = create_patients_content(),
      "diagnostics" = create_diagnostics_content(),
      "outcome" = create_outcome_content(),
      # Contenido por defecto
      tagList(
        fluidRow(
          introBox(
            box(title = "Panel General", status = "primary", solidHeader = TRUE, width = 12,
                h3("Bienvenido al Dashboard RadaR"),
                p("Selecciona una sección usando los botones superiores para ver el contenido específico:"),
                tags$ul(
                  tags$li(strong("PATIENTS:"), " Análisis demográfico y de procedencia"),
                  tags$li(strong("ANTIMICROBIALS:"), " Uso y resistencia de antimicrobianos"),
                  tags$li(strong("DIAGNOSTICS:"), " Resultados microbiológicos y cultivos"),
                  tags$li(strong("OUTCOME:"), " Resultados clínicos y estancia hospitalaria")
                )
            ),
            data.step = 3, data.intro = "Panel principal del dashboard."
          )
        )
      )
    )
  })

  # Modal de filtros dinámico
  observeEvent(input$show_filters, {
    panel <- active_panel()
    
    modal_content <- switch(panel,
      "antimicrobials" = create_antimicrobials_filters(),
      "patients" = create_patients_filters(),
      "diagnostics" = create_diagnostics_filters(),
      "outcome" = create_outcome_filters(),
      list(
        h4("Filtros Generales"),
        selectInput("period", "Periodo:", 
                   choices = c("Última semana", "Último mes", "Último trimestre", "Último año")),
        selectInput("region", "Región:", 
                   choices = c("Todas", "Valencia", "Alicante", "Castellón"))
      )
    )
    
    panel_name <- switch(panel,
      "antimicrobials" = "Antimicrobianos",
      "patients" = "Pacientes", 
      "diagnostics" = "Diagnósticos",
      "outcome" = "Outcome",
      "General"
    )
    
    showModal(
      modalDialog(
        title = paste("Filtros -", panel_name),
        modal_content,
        footer = tagList(
          modalButton("Cerrar"),
          actionButton("apply_filters", "Aplicar")
        )
      )
    )
  })
  
  return(active_panel)
}