#' ustawia zmienne okienka czasowego (okres_min, okres_max, len)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}},
#'   lub \code{\link{agreguj_do_miesiecy}}
#' @param okienko obiekt opisujący okienko stworzony za pomocą funkcji
#'   \code{\link{okienko}}
#' @param filtrZdau ramka danych zawierająca zmienną \code{id_zdau} ograniczająca
#'   obserwacje, które mają się znaleźć w okienku
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_okienko = function(dane, okienko, filtrZdau = NULL){
  stopifnot(
    is.null(filtrZdau) | is.data.frame(filtrZdau) & 'id_zdau' %in% colnames(filtrZdau)
  )

  if (!is.null(filtrZdau)) {
    dane = dane %>%
      inner_join(
        filtrZdau %>%
          select_('id_zdau') %>%
          distinct()
      )
  }

  dataMin = data2okres(okienko[['dataMin']])
  dataMax = data2okres(okienko[['dataMax']])

  dane = dane %>%
    mutate_(
      okres_min = paste('as.integer(', okienko[['zmiennaMin']], '+', okienko[['offsetMin']], ')'),
      okres_max = paste('as.integer(', okienko[['zmiennaMax']], '+', okienko[['offsetMax']], ')')
    ) %>%
    mutate_(
      okres_max = ~if_else(okres_max >= koniec & !is.na(koniec), koniec - 1L, okres_max)
    ) %>%
    mutate_(
      okres_min  = ~if_else(okres_min < dataMin, dataMin, okres_min),
      okres_max  = ~if_else(okres_max > dataMax, dataMax, okres_max)
    ) %>%
    mutate_(
      len = ~okres_max - okres_min + 1L
    ) %>%
    filter_(~len > 0L)

  return(dane)
}