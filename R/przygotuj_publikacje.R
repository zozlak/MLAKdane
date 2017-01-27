#' Przygotowuje zbiór z informacjami o publikacjach naukowych
#' @description Przygotowuje zbiór z informacjami o publikacjach naukowych
#' @param katZr katalog, w którym znajduje się plik publikacje.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @param dataMax koniec badanego okresu (jako łańcuch znaków, np. '2015-09-30')
#' @return ramka danych z informacjami o publikacjach naukowych
#' @export
#' @import dplyr
przygotuj_publikacje = function(katZr, kariera, dataMax){
  stopifnot(
    file.exists(paste0(katZr, '/publikacje.csv')),
    methods::is(kariera, 'kariera_df')
  )
  publikacje = readr::read_csv2(paste0(katZr, '/publikacje.csv'))
  names(publikacje) = tolower(names(publikacje))

  publikacje = publikacje %>%
    mutate_(
      data_od = ~as.integer(rok) * 12L + as.integer(mies)
    ) %>%
    filter_(~recenzja %in% 1)

  publikacje = oblicz_etap(publikacje, kariera, dataMax)

  publikacje = ungroup(publikacje)
  names(publikacje) = paste0('pub_', names(publikacje))
  class(publikacje) = c('publikacje_df', class(publikacje))
  return(publikacje)

}
