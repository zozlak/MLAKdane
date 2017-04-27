#' konwertuje okres składkowy (rok i miesiąc) wyrażony liczbą na datę
#' @param okres liczba miesięcy wyrażona liczbą
#' @return character data wyrażona jako łańcuch znaków (np. '2004-11')
#' @export
okres2data = function(okres){
  filtr = !is.na(okres)
  tmp = okres[filtr] %% 12L
  okres[filtr] = sprintf('%04d-%02d', floor(okres[filtr] / 12L) - ifelse(tmp == 0L, 1L, 0L), ifelse(tmp == 0L, 12L, tmp))
  return(okres)
}