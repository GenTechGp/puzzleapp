#' Variants Tab Server
#' @param id Module ID
#' @export
#' @import shiny
variants_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {

    output$variants_table <- DT::renderDataTable({
      df <- shared_data()  # <- call reactive!
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }
      DT::datatable(df)
    })

  })
}
