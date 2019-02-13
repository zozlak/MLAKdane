#' Przygotowuje słownik uczelni
#' @description
#' Zwraca słownik uczelni
#' @param katZr katalog, w którym znajduje się pliki UCZELNIE.xlsx oraz
#'   UCZELNIE_MUNDUROWE.xlsx
#' @return [data.frame] ramka danych opisująca uczelnie
#' @export
#' @import dplyr
przygotuj_uczelnie = function(katZr){
  uczelnie = as.tbl(openxlsx::readWorkbook(paste0(katZr, '/UCZELNIE.xlsx')))
  colnames(uczelnie) = tolower(colnames(uczelnie))
  stopifnot(nrow(uczelnie) == length(unique(uczelnie$uczelnia_id)))
  mundur = as.tbl(openxlsx::readWorkbook(paste0(katZr, '/UCZELNIE_MUNDUROWE.xlsx'))) %>%
    mutate_(uczelnia_mundur = 1L)
  uczelnie = uczelnie %>%
    left_join(mundur) %>%
    mutate_(
      uczelnia_mundur = ~coalesce(uczelnia_mundur, 0L),
      uczelnia_nazwa  = ~gsub('"', '', uczelnia_nazwa)
    )
  class(uczelnie) = c('uczelnie_df', class(uczelnie))
  return(uczelnie)
}