#' Wczytuje i rejestruje podany plik CSV w Sparku
#' @param polSparka połączenie ze sparkiem uzyskane za pomocą
#'   \code{\link[sparklyr]{spark_connect}}
#' @param nazwaPliku nazwa pliku do wczytania (bez rozszerzenia)
#' @param katalog katalog katalog, w którym znajduje się plik (gdy nie podany,
#'   funkcja zakłada, że plik znajduje się w podkatalogu "cache" katalogu
#'   roboczego)
#' @return ramka danych Sparka
#' @export
wczytaj_do_sparka = function(polSparka, nazwaPliku, katalog = NULL) {
  if (is.null(katalog)) {
    sciezka = paste0(getwd(), '/', nazwa_pliku(nazwaPliku, rozszerzenie = '.csv'))
  } else {
    sciezka = paste0(katalog, '/', nazwaPliku, '.csv')
  }
  dane = sparklyr::spark_read_csv(polSparka, nazwaPliku, paste0('file://', sciezka))
  dane = sparklyr::sdf_register(dane, nazwaPliku)
  return(dane)
}