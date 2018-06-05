#' Odfiltrowuje rekordy z różnicami wartości wskazanej zmiennej
#' @description Używać na wyniku funkcji \code{\link{porownaj_zbiory}}
#' @param dane zbiór danych
#' @param zmienna prefiks zmiennej, między którymi występują różnice
#' @param zmienneId list zmiennych, która powinna zostać dołączona do wyniku
#' @param tolerancja tolerancja przy porównywaniu liczb zmiennoprzecinkowych
#' @return data.frame odfiltrowane dane
#' @import dplyr
#' @export
pokaz_roznice = function(dane, zmienna, zmienneId = c('id_zdau', 'id', 'ID_ZDAU', 'ID', 'okres', 'OKRES', 'id_zdu3'), tolerancja = 0.00001) {
  zmienneId = intersect(zmienneId, colnames(dane))
  z1 = paste0(zmienna, '_1')
  z2 = paste0(zmienna, '_2')
  if (class(dane[, z1])[1] == 'numeric' & class(dane[, z2])[1] == 'numeric') {
    filtr = sprintf('abs(%s - %s) > %f | is.na(%s) + is.na(%s) == 1', z1, z2, tolerancja, z1, z2)
  } else {
    filtr = sprintf('%s != %s | is.na(%s) + is.na(%s) == 1', z1, z2, z1, z2)
  }
  wynik = dane %>%
    filter_(.dots = filtr) %>%
    select_(.dots = c(zmienneId, z1, z2))
  return(as.tbl(wynik))
}