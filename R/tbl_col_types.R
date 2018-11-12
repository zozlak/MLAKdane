#' Zwraca opis typów danych w formacie readr::read_csv()
#' @param tbl ramka danych korzystająca z dowolnego backendu
#' @return character
#' @export
tbl_col_types = function(tbl) {
  tbl = tbl %>% utils::head() %>% collect()
  typy = sapply(tbl, function(x){class(x)[1]})
  names(typy) = NULL
  typy = paste0(sub('n', 'd', substr(typy, 1, 1)), collapse = '')
  return(typy)
}