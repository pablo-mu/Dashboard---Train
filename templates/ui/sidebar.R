# =============================================================================
# DASHBOARD SIDEBAR MODULE
# =============================================================================

create_dashboard_sidebar <- function(config) {
  dashboardSidebar(
    width = config$sidebar_width,
    introBox(
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      sidebarMenu(
        id = "sidebar_tab",
        introBox(
          div(id = "sidebar_button",
              uiOutput("dynamic_filter_btn")
          ),
          data.step = 1, data.intro = "Pulsa para abrir los filtros avanzados."
        ),
        div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
        menuItem("ANTIMICROBIALS", tabName = "antimicrobials", icon = icon("spinner"),
          menuItem("START OF ANTIMICROBIALS",
            sliderInput("ab_timingInput", "", min = 0, max = 10, value = c(0, 10), step = 1),
            switchInput("ab_anyInput", label = "SELECTION", value = FALSE, onLabel = "SLIDER", offLabel = "ANY")
          ),
          menuItem("MINIMUM TREATMENT DURATION",
            sliderInput("ab_allInput", "", min = 1, max = 10, value = 2, step = 1)
          )
        ),
        menuItem("PATIENTS", tabName = "patients", icon = icon("address-card"),
          checkboxGroupInput("genderInput", "", choices = c("Male", "Female"), selected = c("Male", "Female")),
          sliderInput("ageInput", "Age", min = 0, max = 100, value = c(0, 100), step = 1)
        ),
        menuItem("YEAR", tabName = "year", icon = icon("calendar"),
          sliderInput("yearInput", "Year", min = 2015, max = 2025, value = c(2015, 2025), step = 1)
        ),
        menuItem("SPECIALTY", tabName = "specialty", icon = icon("user-md"),
          checkboxGroupInput("specInput", "SPECIALTY", choices = c("A", "B", "C"), selected = c("A", "B", "C"))
        ),
        menuItem("ORIGIN", tabName = "admission", icon = icon("ambulance"),
          checkboxGroupInput("admissionInput", "", choices = c("ER", "Ward"), selected = c("ER", "Ward"))
        ),
        menuItem("DIAGNOSTICS", tabName = "diagnostics", icon = icon("flask"),
          selectInput("diagnosticsInput", "", choices = c("Blood", "Urine"))
        ),
        menuItem("DOWNLOAD", tabName = "download", icon = icon("download"),
          textInput("filename", "Name download file", ""),
          downloadButton("downloadData", "Save Data", icon = icon("download"))
        )
      )
    )
  )
}