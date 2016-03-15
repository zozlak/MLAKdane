#' odczytuje rok z liczby miesięcy wyrażonej liczbą
#' @param okres liczba miesięcy wyrażona liczbą
#' @return numeric rok
#' @seealso data2okres
#' @export
okres2rok = function(okres){
  tmp = okres %% 12
  return(floor(okres / 12) - ifelse(tmp == 0, 1, 0))
}