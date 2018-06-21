#' odczytuje rok z liczby miesięcy wyrażonej liczbą
#' @param okres liczba miesięcy wyrażona liczbą
#' @return numeric rok
#' @seealso data2okres
#' @export
okres2rok = function(okres){
  return(as.integer((okres - 1L) / 12))
}