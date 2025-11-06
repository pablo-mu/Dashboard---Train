# =============================================================================
# DASHBOARD HEADER MODULE
# =============================================================================

create_dashboard_header <- function(config) {
  shinydashboard::dashboardHeader(
    title = span(img(src = "radar.svg", height = 35), config$header_title),
    titleWidth = config$sidebar_width,
    dropdownMenu(
      type = "notifications",
      headerText = strong("HELP"),
      icon = icon("question"),
      badgeStatus = NULL,
      notificationItem(text = "Step 1: Introduction", icon = icon("spinner")),
      notificationItem(text = "Step 2: Data Entry", icon = icon("address-card")),
      notificationItem(text = "Step 3: Calendar", icon = icon("calendar")),
      notificationItem(text = "Step 4: User", icon = icon("user-md")),
      notificationItem(text = "Step 5: Ambulance", icon = icon("ambulance")),
      notificationItem(text = "Step 6: Lab", icon = icon("flask")),
      notificationItem(text = strong("Step 7: Warning"), icon = icon("exclamation"))
    ),
    tags$li(
      a(
        strong("ABOUT Radar"),
        height = 40,
        href = config$github_repo,
        target = "_blank"
      ),
      class = "dropdown"
    )
  )
}