#' uzupełnia rekordy ramki danych o unikalne obserwacje z innej ramki danych
#' @param wynik ramka danych do uzupełnienia
#' @param wzor ramka danych zawierająca wszystkie obserwacje
#' @param id wektor nazw zmiennych wyznaczających unikalne obserwacje w ramce
#'   danych \code{wzor}
#' @return data.frame dane z parametru \code{wynik} uzupełnione o wszystkie
#'   unikalne identyfikatory z parametru \code{wzor}
#' @export
#' @import dplyr
uzupelnij_obserwacje = function(wynik, wzor, id = 'id_zdau'){
  stopifnot(
    is(wynik, 'data.frame'),
    is(wzor, 'data.frame')
  )
  wzor %>%
    select_(.dots = id) %>%
    distinct() %>%
    left_join(wynik) %>%
    return()
}