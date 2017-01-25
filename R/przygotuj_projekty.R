#' Przygotowuje zbiór z informacjami o projektach naukowych
#' @description Przygotowuje zbiór z informacjami o projektach naukowych
#' @param katZr katalog, w którym znajduje się plik projekty.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @return ramka danych z informacjami o projektach naukowych
#' @export
#' @import dplyr
przygotuj_projekty = function(katZr, kariera){
  stopifnot(
    file.exists(paste0(katZr, '/projekty.csv')),
    methods::is(kariera, 'kariera_df')
  )
  projekty = readr::read_csv2(paste0(katZr, '/projekty.csv'))
  projekty = projekty %>%
    mutate_(
      proj_data_od = ~as.integer(rok_od) * 12L + as.integer(mies_od),
      proj_data_do = ~as.integer(rok_do) * 12L + as.integer(mies_do)
    )
  projekty = projekty %>%
    left_join(kariera)

  class(projekty) = c('projekty_df', class(projekty))
  return(projekty)
}