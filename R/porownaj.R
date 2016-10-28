#' zestawia ze sobą wartości wskazanej zmiennej w dwóch zbiorach
#' @param a pierwszy zbiór
#' @param b drugi zbiór
#' @param zmiennaA nazwa zmiennej do porównania w zbiorze a
#' @param zmiennaB nazwa zmiennej do porównania w zbiorze b
#' @param id wektor nazw zmiennych stanowiących klucz podstawowy w zbiorach
#' @return data.frame wartości zmiennej połączone z obydwy zbiorów
#' @export
#' @import dplyr
porownaj = function(a, b, zmiennaA, zmiennaB = zmiennaA, id = c('ID_ZDAU', 'ID')){
  wynik = a %>%
    ungroup() %>%
    select_(.dots = c(zmiennaA, id)) %>%
    rename_(a = zmiennaA) %>%
    full_join(
      b %>%
        ungroup() %>%
        select_(.dots = c(zmiennaB, id)) %>%
        rename_(b = zmiennaB)
    ) %>%
    mutate_(
      d = ~a - b,
      d = ~ifelse(is.na(d) & is.na(a) & is.na(b), 0, d)
    )
  return(wynik)
}