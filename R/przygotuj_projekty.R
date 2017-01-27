#' Przygotowuje zbiór z informacjami o projektach naukowych
#' @description Przygotowuje zbiór z informacjami o projektach naukowych
#' @param katZr katalog, w którym znajduje się plik projekty.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @param dataMax koniec badanego okresu (jako łańcuch znaków, np. '2015-09-30')
#' @return ramka danych z informacjami o projektach naukowych
#' @export
#' @import dplyr
przygotuj_projekty = function(katZr, kariera, dataMax){
  stopifnot(
    file.exists(paste0(katZr, '/projekty.csv')),
    methods::is(kariera, 'kariera_df')
  )
  projekty = readr::read_csv2(paste0(katZr, '/projekty.csv'))
  names(projekty) = tolower(names(projekty))

  projekty = projekty %>%
    mutate_(
      data_od = ~as.integer(rok_od) * 12L + as.integer(mies_od),
      data_do = ~as.integer(rok_do) * 12L + as.integer(mies_do)
    ) %>%
    filter_(~kierownik %in% 1)

  projekty = oblicz_etap(projekty, kariera, dataMax)

  projekty = ungroup(projekty)
  names(projekty) = paste0('proj_', names(projekty))
  class(projekty) = c('projekty_df', class(projekty))
  return(projekty)
}