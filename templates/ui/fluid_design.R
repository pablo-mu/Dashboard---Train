# =============================================================================
# FLUID DESIGN MODULE - GRÁFICOS CON FILTROS INTEGRADOS
# =============================================================================

# Función principal que crea el layout dinámico 2x2
fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        if (!is.null(y)) uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        if (!is.null(z)) uiOutput(z)
      )
    )
  )
}

# Función auxiliar para crear un box con filtros integrados
create_box_with_filters <- function(title, status, plot_output_id, filters_list = NULL, description = NULL) {
  
  # Contenido principal del box
  box_content <- list(
    plotOutput(plot_output_id, height = "300px")
  )
  
  # Agregar descripción si existe
  if (!is.null(description)) {
    box_content <- append(box_content, list(br(), p(description, style = "font-size: 12px; color: #666;")), after = 1)
  }
  
  # Agregar filtros en la esquina inferior izquierda si existen
  if (!is.null(filters_list)) {
    filter_div <- div(
      class = "plot-filters",
      style = "position: absolute; bottom: 10px; left: 10px; background: rgba(255,255,255,0.9); 
               padding: 8px; border-radius: 5px; border: 1px solid #ddd; z-index: 1000;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      filters_list
    )
    box_content <- append(box_content, list(filter_div))
  }
  
  # Crear el box con posición relativa para los filtros absolutos
  box(
    title = title, 
    status = status, 
    solidHeader = TRUE, 
    width = 12,
    style = "position: relative; min-height: 400px;",
    box_content
  )
}

# Función para crear filtros compactos
create_compact_filter <- function(input_id, label, choices, selected = NULL, type = "select") {
  
  filter_style <- "margin: 2px 0; font-size: 11px;"
  label_style <- "margin-bottom: 2px; font-weight: bold; font-size: 10px;"
  
  switch(type,
    "select" = div(
      style = filter_style,
      tags$label(label, style = label_style),
      selectInput(input_id, NULL, choices = choices, selected = selected, 
                 width = "120px", size = "sm")
    ),
    "checkbox" = div(
      style = filter_style,
      tags$label(label, style = label_style),
      checkboxGroupInput(input_id, NULL, choices = choices, selected = selected,
                        width = "120px")
    ),
    "slider" = div(
      style = filter_style,
      tags$label(label, style = label_style),
      sliderInput(input_id, NULL, min = choices[1], max = choices[2], 
                 value = if(is.null(selected)) choices else selected,
                 width = "120px", step = 1)
    ),
    "daterange" = div(
      style = filter_style,
      tags$label(label, style = label_style),
      dateRangeInput(input_id, NULL, start = choices[1], end = choices[2],
                    width = "140px")
    )
  )
}