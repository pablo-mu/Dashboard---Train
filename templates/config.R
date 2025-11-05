# =============================================================================
# DASHBOARD CONFIGURATION
# =============================================================================
# Este archivo define la configuración general del dashboard modular

# Panel definitions
PANEL_DEFINITIONS <- list(
  antimicrobials = list(
    name = "ANTIMICROBIALS",
    label = "ANTIMICROBIALS", 
    icon = "spinner",
    button_style = "danger",
    button_label = "FILTROS ANTIMICROBIALS"
  ),
  patients = list(
    name = "PATIENTS",
    label = "PATIENTS",
    icon = "user", 
    button_style = "primary",
    button_label = "FILTROS PACIENTES"
  ),
  diagnostics = list(
    name = "DIAGNOSTICS", 
    label = "DIAGNOSTICS",
    icon = "flask",
    button_style = "warning", 
    button_label = "FILTROS DIAGNÓSTICOS"
  ),
  outcome = list(
    name = "OUTCOME",
    label = "OUTCOME", 
    icon = "thumbs-o-up",
    button_style = "info",
    button_label = "FILTROS OUTCOME"
  )
)

# Default panel
DEFAULT_PANEL <- "antimicrobials"

# Dashboard settings
DASHBOARD_CONFIG <- list(
  skin = "black",
  title = "Radar Dashboard",
  header_title = "Radar Dashboard",
  sidebar_width = 300,
  github_repo = "https://github.com/yourrepo/radar"
)