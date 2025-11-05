# server place-holder

server <- function(input, output, session) {
  observeEvent(input$show_filters, {
    showModal(
      modalDialog(
        title = "Filtros",
        selectInput("period", "Periodo:", choices = c("Última semana", "Último mes", "Último trimestre", "Último año")),
        selectInput("region", "Región:", choices = c("Todas", "Valencia", "Alicante", "Castellón")),
        footer = tagList(
          modalButton("Cerrar"),
          actionButton("apply_filters", "Aplicar")
        )
      )
    )
  })
  # ...resto de tu lógica server...
}