#' Widget output function for use in Shiny
#'
#' @param outputId outputId
#' @param width width
#' @param height height
#' @export
streamgraphOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'streamgraph', width, height, package = 'streamgraph')
}


#' Widget render function for use in Shiny
#'
#' @param expr expr
#' @param env env
#' @param quoted quoted
#' @export
renderStreamgraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, streamgraphOutput, env, quoted = TRUE)
}


event_data <- function(event = c("d3_hover", "d3_click", "d3_selected", 
                                 "d3_relayout"), source = "A") {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    stop("No reactive domain detected. This function can only be called \n",
         "from within a reactive shiny context.")
  }
  val <- session$input[[sprintf(".clientValue-%s-%s", event[1], source)]]
  if (is.null(val)) val else jsonlite::fromJSON(val)
}
