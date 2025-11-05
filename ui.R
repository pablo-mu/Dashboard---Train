# brand Color Palette
brand_colors <- list(
	primary = "#004B87",
	secondary = "#0066B3",
	accent = "#00A8E8",
	light_bg = "#F5F7FA",
	text = "#333333"
)

# ...existing code...

# ...existing code...

# ...existing code...

ui <- page_sidebar(
  title = div(
    style = sprintf(
      "background: %s; color: white; padding: 18px 32px; font-size: 1.7rem; font-weight: 600; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
      brand_colors$primary
    ),
    "Dashboard Principal",
    actionButton("show_filters", "Filtros", icon = icon("filter"), style = "float: right; background: white; color: #004B87; border: none; margin-left: 20px;")
  ),
  sidebar = sidebar(
    # Sidebar vacío o con navegación
  ),
  navset_tab(
    id = "dashboard_tabs",
    nav_panel("Vista General", icon = icon("chart-pie")),
    nav_panel("Análisis Regional", icon = icon("map-marked-alt")),
    nav_panel("Tendencias", icon = icon("chart-line")),
    nav_panel("Datos", icon = icon("table"))
  ),
  main = div(
    # ...main content...
  ),
  # Add custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)

# ...existing code...
# ...existing code...
# ...existing code...