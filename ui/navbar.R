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
        bsButton(
          "Traza",
          label = panel_definitions$traza$name,
          icon = icon(panel_definitions$traza$icon),
          style = panel_definitions$traza$button_style
        ),
        data.step = 2,
        data.intro = "<strong>Barra de Navegación</strong><br/>Usa estos botones para cambiar rápidamente entre los paneles principales del dashboard:<br/>• <strong>Sankey</strong>: Diagrama de flujo de costes<br/>• <strong>Matriz Costes</strong>: Vista de matriz de reparto<br/>• <strong>Diagnósticos</strong>: Análisis y estadísticas detalladas<br/>• <strong>Traza</strong>: Matriz de movimientos con agregación personalizada"
      )
    )
  )
}