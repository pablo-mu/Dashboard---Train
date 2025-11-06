# =============================================================================
# MAIN BUTTONS MODULE  
# =============================================================================

create_main_buttons <- function(panel_definitions) {
  shiny::fluidRow(
    column(
      width = 12,
      introBox(
        bsButton("patients", label = panel_definitions$patients$name, icon = icon(panel_definitions$patients$icon), style = "success"),
        bsButton("antimicrobials", label = panel_definitions$antimicrobials$name, icon = icon(panel_definitions$antimicrobials$icon), style = "success"),
        bsButton("diagnostics", label = panel_definitions$diagnostics$name, icon = icon(panel_definitions$diagnostics$icon), style = "success"),
        bsButton("outcome", label = panel_definitions$outcome$name, icon = icon(panel_definitions$outcome$icon), style = "success"),
        data.step = 2, data.intro = "Use these buttons to quickly access main dashboard panels."
      )
    )
  )
}