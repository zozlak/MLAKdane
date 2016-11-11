#' konwertuje okres składkowy (rok i miesiąc) wyrażony liczbą na datę
#' @param okres liczba miesięcy wyrażona liczbą
#' @return character data wyrażona jako łańcuch znaków (np. '2004-11')
#' @export
okres2data = function(okres){
  tmp = okres %% 12L
  return(sprintf('%04d-%02d', floor(okres / 12L) - ifelse(tmp == 0L, 1L, 0L), ifelse(tmp == 0L, 12L, tmp)))
}