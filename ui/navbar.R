create_bar <- function(panel_definitions) {
  shiny::fluidRow(
    column(
      width = 12,
      introBox(
        bsButton(
          "Sankey",
          label = panel_definitions$sankey$name,
          icon = icon(panel_definitions$sankey$icon),
          style = panel_definitions$sankey$button_style
        ),
        bsButton(
          "Matriz Costes",
          label = panel_definitions$matriz_costes$name,
          icon = icon(panel_definitions$matriz_costes$icon),
          style = panel_definitions$matriz_costes$button_style
        ),
        bsButton(
          "Diagnostics",
          label = panel_definitions$diagnostics$name,
          icon = icon(panel_definitions$diagnostics$icon),
          style = panel_definitions$diagnostics$button_style
        ),
        data.step = 2,
        data.intro = "Use these buttons to quickly access main dashboard panels."
      )
    )
  )
}