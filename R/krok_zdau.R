#' wycina podzbiór obserwacji ze zbioru ZDAU
#' @description Typowy scenariusz użycia to wyliczanie zmiennych dla okresów w
#' środowisku o ograniczonym rozmiarze pamięci.
#'
#' Funkcja jest stabilna, tzn. dla tych samych danych wejściowych zawsze zwraca
#' takie same dane.
#' @param zdau ramka danych ZDAU zwrócona przez funkcję
#'   \code{\link{przygotuj_zdau}}
#' @param n jaką liczbę rekordów ze zbioru zachować zaczynając od jakiego
#'   rekordu, np. \code{n = c(0, 0)} zachowuje wszystkie rekordy, a \code{n =
#'   c(1000, 15000)} zachowuje rekordy od 15001 do 16000
#' @param tylkoAbs czy pozostawić w danych ZDAU jedynie absolwentów
#' @return data.frame z jedną zmienną - "id_zdau"
#' @export
krok_zdau = function(zdau, n, tylkoAbs = TRUE) {
  stopifnot(
    methods::is(zdau, 'zdau_df'),
    is.vector(n), is.numeric(n), length(n) == 2, all(!is.na(n)), all(n >= 0),
    is.vector(tylkoAbs), is.logical(tylkoAbs), length(tylkoAbs) == 1, all(!is.na(tylkoAbs))
  )

  if (tylkoAbs) {
    zdau = zdau %>%
      filter_(~typ %in% 'A')
  }

  filtr = zdau %>%
    select_('id') %>%
    distinct() %>%
    arrange_('id') %>%
    filter_(~row_number() > n[2] & row_number() <= n[1] + n[2])
  wynik = zdau %>%
    select_('id_zdau', 'id') %>%
    inner_join(filtr) %>%
    select_('id_zdau')

  return(wynik)
}