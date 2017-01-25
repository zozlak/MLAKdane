#' Przygotowuje zbiór z informacjami o publikacjach naukowych
#' @description Przygotowuje zbiór z informacjami o publikacjach naukowych
#' @param katZr katalog, w którym znajduje się plik publikacje.csv
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @return ramka danych z informacjami o publikacjach naukowych
#' @export
#' @import dplyr
przygotuj_publikacje = function(katZr, kariera){
  stopifnot(
    file.exists(paste0(katZr, '/publikacje.csv')),
    methods::is(kariera, 'kariera_df')
  )
  publikacje = readr::read_csv2(paste0(katZr, '/publikacje.csv'))
  names(publikacje) = tolower(names(publikacje))
  publikacje = publikacje %>%
    mutate_(
      pub_data = ~as.integer(rok) * 12L + as.integer(mies)
    ) %>%
    select_('-rok', '-mies')
  publikacje = publikacje %>%
    left_join(kariera) %>%
    mutate_(
      pub_etap = ~case_when(
        .$pub_data < .$data_od_slic & !is.na(.$data_od_slic) ~ NA_character_,
        .$pub_data < .$data_od_smgr & !is.na(.$data_od_smgr) ~ 'SLIC',
        .$pub_data < .$data_od_sdr  & !is.na(.$data_od_sdr)  ~ 'SMGR',
        .$pub_data < .$data_od_dr   & !is.na(.$data_od_dr)   ~ 'SDR',
        .$pub_data < .$data_od_hab  & !is.na(.$data_od_hab)  ~ 'DR',
        .$pub_data < .$data_od_prof & !is.na(.$data_od_prof) ~ 'HAB',
        TRUE ~ 'PROF'
      )
    ) %>%
    select_(.dots = c(names(publikacje), 'pub_etap'))

  class(publikacje) = c('publikacje_df', class(publikacje))
  return(publikacje)

}
