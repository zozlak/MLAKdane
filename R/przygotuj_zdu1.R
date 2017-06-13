#' Wczytuje zbiór ZDU1
#' @description
#' Wczytuje zbiór opisujący osoby ubezpieczone (ZDU1)
#' @param katZr katalog, w którym znajduje się plik ZDU1.csv
#' @export
#' @import dplyr
#' @import readr
przygotuj_zdu1 = function(katZr){
  zdu1 = utils::read.csv2(paste0(katZr, '/ZDU1.csv'), header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)[, c(1, 5:8)]
  colnames(zdu1) = c('id', 'rok_ur', 'plec', 'koniec_r', 'koniec_m')
  zdu1 = zdu1 %>%
    mutate_(
      id = ~as.integer(id),
      rok_ur = ~as.integer(rok_ur),
      koniec = ~as.integer(koniec_r) * 12L + as.integer(koniec_m)
    ) %>%
    select_('-koniec_r', '-koniec_m')
  return(zdu1)
}