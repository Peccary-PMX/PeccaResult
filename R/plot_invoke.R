#' @export
plot_invoke <- function(x){

  suppressMessages(invoke(plot_grid, x$plot))
}
