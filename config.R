DASHBOARD_CONFIG <- list(
    skin = "black",
    title = "Dashboard Development",
    header_title = "Dashboard Development",
    sidebar_width = 300,
    github_url = "https://github.com/yourrepo/radar",
    color_theme = "blue"  # Cambiar a "health" para usar la paleta de salud
)

# Paleta de colores azul (corporativa)
blue_colors <- list(
    primary = "#004B87",
    secondary = "#0066B3", 
    accent = "#00A8E8",
    light_bg = "#F5F7FA",
    text = "#333333",
    danger = "#c8102e",
    sidebar = "#66819E"
)

# Paleta de colores salud (rojo-azul salud)
health_colors <- list(
    primary = "#c8102e",
    secondary = "#66819E",
    accent = "#8BA3B8",
    light_bg = "#F8F9FA",
    text = "#2C3E50", 
    danger = "#E74C3C",
    sidebar = "#34495E"
)

# Función para obtener la paleta activa
get_active_colors <- function() {
    if (DASHBOARD_CONFIG$color_theme == "health") {
        return(health_colors)
    } else {
        return(blue_colors)
    }
}

# Función para cambiar la configuración del tema
set_color_theme <- function(theme) {
    DASHBOARD_CONFIG$color_theme <<- theme
}


PANEL_DEFINITIONS <- list(
    sankey = list(
        name = "Sankey",
        label = "sankey",
        icon = "sitemap",
        button_style = "success",
        button_label = "Generar Sankey"
    ),

    matriz_costes = list(
        name = "Matriz Costes",
        label = "matriz_costes",
        icon = "table",
        button_style = "success",
        button_label = "Generar Matriz de Costes"
    ),

    diagnostics = list(
        name = "Diagnostics",
        label = "diagnostics",
        icon = "flask",
        button_style = "success",
        button_label = "Generar Diagnósticos"
    )
)

DEFAULT_PANEL <- "sankey"