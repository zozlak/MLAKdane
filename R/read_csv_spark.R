#' wczytuje dane zapisane za pomocą sparklyr::spark_write_csv()
#' @description
#' sparklyr::spark_write_csv() zapisuje większe zbiory danych w częściach.
#' Ta funkcja umożliwia ich łatwe wczytywanie
#' @param sciezka ścieżka do pliku/katalogu przechowującego dane
#' @param typyKolumn opcjonalnie typy kolumn w formacie parametru
#'   \code{col_types} funkcji \code{\link[readr]{read_csv}} (d - double, i -
#'   integer, c - character)
#' @return data.frame
#' @export
read_csv_spark = function(sciezka, typyKolumn = NULL) {
  if (dir.exists(sciezka)) {
    dane = list()
    for (plik in list.files(sciezka, 'csv$', full.names = TRUE)) {
      dane[[length(dane) + 1]] = readr::read_csv(plik, col_types = typyKolumn)
    }
    dane = dplyr::bind_rows(dane)
  } else {
    dane = readr::read_csv(sciezka, col_types = typyKolumn)
  }
  return(dane)
}