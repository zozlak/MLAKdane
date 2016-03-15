#' konwertuje okres składkowy (rok i miesiąc) wyrażony liczbą na datę
#' @param okres liczba miesięcy wyrażona liczbą
#' @return character data wyrażona jako łańcuch znaków (np. '2004-11')
#' @export
okres2data = function(okres){
  tmp = okres %% 12
  return(sprintf('%04d-%02d', floor(okres / 12) - ifelse(tmp == 0, 1, 0), ifelse(tmp == 0, 12, tmp)))
}