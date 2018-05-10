#' Zapisuje ramkę danych w formacie CSV kompatybilnym ze Sparkiem
#' @param dane ramka danych do zapisania
#' @param nazwaPliku nazwa pliku, w którym zpaisane zostaną dane (bez
#'   rozszerzenia)
#' @param katalog katalog, w którym zapisane zostaną dane, jeśli nie podany,
#'   będzie to podkatalog "cache" w katalogu roboczym
#' @return character ścieżka zapisu pliku
#' @export
zapisz_dla_sparka = function(dane, nazwaPliku, katalog = NULL) {
  if (is.null(katalog)) {
    sciezka = nazwa_pliku(nazwaPliku, '.csv')
  } else {
    sciezka = paste0(katalog, '/', nazwaPliku, '.csv')
  }
  readr::write_csv(dane, sciezka, na = '')
  return(invisible(sciezka))
}