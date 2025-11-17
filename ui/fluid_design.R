# =============================================================================
# FLUID DESIGN MODULE - PANELES DINÁMICOS
# =============================================================================

# Función principal que crea el layout dinámico para paneles
fluid_design <- function(id, w, x = NULL, y = NULL, z = NULL) {
  fluidRow(
    div(
      id = id,
      column(
        width = if (!is.null(x)) 6 else 12,
        uiOutput(w),
        if (!is.null(y)) uiOutput(y)
      ),
      if (!is.null(x)) {
        column(
          width = 6,
          uiOutput(x),
          if (!is.null(z)) uiOutput(z)
        )
      }
    )
  )
}
