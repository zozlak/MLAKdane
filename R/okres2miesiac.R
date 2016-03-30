#' odczytuje miesiąc z liczby miesięcy wyrażonej liczbą
#' @param okres liczba miesięcy wyrażona liczbą
#' @return numeric miesiąc (1-12)
#' @seealso data2okres
#' @export
okres2miesiac = function(okres){
  miesiac = okres %% 12
  return(ifelse(miesiac == 0, 12, miesiac))
}