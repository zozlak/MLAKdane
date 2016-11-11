#' konwertuje datę w postaci tekstu/daty na liczbę miesięcy
#' @details
#' Akceptowane są daty w formacie "{CYFRY}-{CYFRY}{COKOLWIEK}", gdzie pierwszy
#' ciąg cyfr interpretowany jest jako rok, a drugi jako liczba miesięcy. Stąd
#' funkcja działa poprawnie także dla niepełnych data (np. okres składkowy ZUS
#' "2014-05")
#' @param data data
#' @return numeric liczba miesięcy jako ROK*12 + MIESIĄC
#' @export
data2okres = function(data){
  return(as.integer(sub('-.*$', '', data)) * 12L + as.integer(sub('^[0-9]+-([0-9]+).*$', '\\1', data)))
}