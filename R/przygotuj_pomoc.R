#' Przygotowuje zbiór z informacjami o pomocy materialnej
#' @description Przygotowuje zbiór z informacjami o pomocy materialnej
#' @param katZr katalog, w którym znajduje się plik pomoc.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @return ramka danych z informacjami o pomocy materialnej
#' @export
#' @import dplyr
przygotuj_projekty = function(katZr, kariera){
  stopifnot(
    file.exists(paste0(katZr, '/pomoc.csv')),
    methods::is(kariera, 'kariera_df')
  )
  pomoc = readr::read_csv2(paste0(katZr, '/pomoc.csv'))
  pomoc = pomoc %>%
    mutate_(
      pom_data_od = ~as.integer(rok_od) * 12L + as.integer(mies_od),
      pom_data_do = ~as.integer(rok_do) * 12L + as.integer(mies_do)
    )
  pomoc = pomoc %>%
    left_join(kariera)

  class(pomoc) = c('pomoc_df', class(pomoc))
  return(pomoc)
}