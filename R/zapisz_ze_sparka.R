#' Eksportuje dane ze Sparka do CSV
#' @param dane ramka danych Sparka
#' @param nazwaPliku nazwa pliku CSV (bez rozszerzenia)
#' @param buforuj czy zbuforować ramkę danych przed zapisem
#' @param katalog katalog, w którym zapisane zostaną dane, jeśli nie podany,
#'   będzie to podkatalog "cache" w katalogu roboczym
#' @param polSparka połączeni ze Sparkiem, niezbędne gdy \code{buforuj = TRUE}
#' @param zlacz czy złączyć wszystkie partycje Sparka w jeden zbiór
#' @return character ścieżka zapisu
#' @export
zapisz_ze_sparka = function(dane, nazwaPliku, buforuj = FALSE, katalog = NULL, polSparka = NULL, zlacz = TRUE) {
  if (is.null(katalog)) {
    sciezka = paste0(getwd(), '/', nazwa_pliku(nazwaPliku, '.csv'))
  } else {
    sciezka = paste0(katalog, '/', nazwaPliku, '.csv')
  }

  dane = sparklyr::sdf_register(dane, nazwaPliku)

  if (buforuj) {
    sparklyr::tbl_cache(polSparka, nazwaPliku)
  }

  opcje = list(positiveInf = '', negativeInf = '', nanValue = '')
  if (zlacz) {
    sparklyr::spark_write_csv(dane, paste0('file://', sciezka, '__'), na = '', mode = 'overwrite', options = opcje)
    writeLines(paste0(colnames(dane), collapse = ','), sciezka)
    system(sprintf("tail -q -n +2 %s__/*.csv >> '%s' && rm -fr '%s__'", sciezka, sciezka, sciezka))
  } else {
    sparklyr::spark_write_csv(dane, paste0('file://', sciezka), na = '', mode = 'overwrite', options = opcje)
  }

  return(sciezka)
}