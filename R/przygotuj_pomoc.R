#' Przygotowuje zbiór z informacjami o pomocy materialnej
#' @description Przygotowuje zbiór z informacjami o pomocy materialnej
#' @param katZr katalog, w którym znajduje się plik pomoc.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @param dataMax koniec badanego okresu (jako łańcuch znaków, np. '2015-09-30')
#' @return ramka danych z informacjami o pomocy materialnej
#' @export
#' @import dplyr
przygotuj_pomoc = function(katZr, kariera, dataMax){
  stopifnot(
    file.exists(paste0(katZr, '/pomoc.csv')),
    methods::is(kariera, 'kariera_df')
  )
  pomoc = readr::read_csv2(paste0(katZr, '/pomoc.csv'))
  names(pomoc) = tolower(names(pomoc))

  pomoc = pomoc %>%
    mutate_(
      data_od = ~as.integer(rok_od) * 12L + as.integer(mies_od),
      data_do = ~as.integer(rok_do) * 12L + as.integer(mies_do)
    )

  pomoc = oblicz_etap(pomoc, kariera, dataMax)

  pomoc = ungroup(pomoc)
  names(pomoc) = paste0('pom_', names(pomoc))
  class(pomoc) = c('pomoc_df', class(pomoc))
  return(pomoc)
}