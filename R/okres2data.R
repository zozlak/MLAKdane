#' konwertuje okres składkowy (rok i miesiąc) wyrażony liczbą na datę
#' @param okres liczba miesięcy wyrażona liczbą
#' @return character data wyrażona jako łańcuch znaków (np. '2004-11')
#' @export
okres2data = function(okres){
  return(sprintf('%04d-%02d', floor(okres / 12), okres %% 12))
}