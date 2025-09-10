add_dt_styles <- function(dt) {
  # Available columns in the DT object
  dt_cols <- colnames(dt$x$data)

  style_rules <- list(
    # list(
    #   col = "ID",
    #   fn  = function(dt, col) DT::formatStyle(
    #     dt, col,
    #     backgroundColor = DT::styleEqual("chr10_100984619_28914", "#FFFF0099"),
    #     target = "row"
    #   )
    # ),
    list(
      col = "spliceai_override",
      fn  = function(dt, col) DT::formatStyle(
        dt, col,
        backgroundColor = DT::styleEqual(TRUE, "#FFFF0099"),
        target = "row"
      )
    ),
    list(
      col = "clinvar_override",
      fn  = function(dt, col) DT::formatStyle(
        dt, col,
        backgroundColor = DT::styleEqual(TRUE, "#FFA50099"),
        target = "row"
      )
    ),
    list(
      col = "PRIORITYFlag",
      fn  = function(dt, col) DT::formatStyle(
        dt, col,
        backgroundColor = DT::styleEqual(
          c(TRUE, FALSE),
          c("#90EE90", "#FFCCCC")
        ),
        target = "row"
      )
    )
  )

  for (rule in style_rules) {
    if (rule$col %in% dt_cols) {
      dt <- rule$fn(dt, rule$col)
    }
  }

  dt
}
