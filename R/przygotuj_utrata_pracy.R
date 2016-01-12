#' oblicza zmienne utraty pracy, używane do wyliczania zmiennych NMLE oraz NMLEP
#' @param zus dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zus}}
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
przygotuj_utrata_pracy = function(zus, dataMax, multidplyr = TRUE){
  stopifnot(
    is(zus, 'zus_df')
  )

  wynik = zus %>%
    select_('id', 'id_platnika', 'okres', 'etat', 'netat', 'samoz', 'platnik_kon')
  if(multidplyr){
    wynik = multidplyr::partition(wynik, id, id_platnika, okres)
  }else{
    wynik = group_by_(wynik, 'id', 'id_platnika', 'okres')
  }
  wynik = wynik %>%
    summarize_(
      etat        = ~ sum(etat, na.rm = TRUE),
      praca       = ~ sum(etat + netat + samoz, na.rm = TRUE),
      platnik_kon = ~ first(platnik_kon)
    ) %>%
    collect()
  wynik = wynik %>%
    group_by_('id', 'id_platnika') %>%
    arrange_('okres') %>%
    mutate_(
      utretatu    = ~ etat > 0 & (lead(etat) %in% 0 | !is.na(lead(okres)) & lead(okres) - okres > 1 | is.na(lead(etat)) & okres != data2okres(dataMax)),
      utretatu_v2 = ~ utretatu %in% 1 & (is.na(platnik_kon) | platnik_kon - okres > 6),
      utrpracy    = ~ praca > 0 & (lead(praca) %in% 0 | is.na(lead(praca)) & lead(okres) - okres > 1 | is.na(lead(praca)) & okres != data2okres(dataMax))
    ) %>%
    select_('id', 'id_platnika', 'okres', 'utretatu', 'utretatu_v2', 'utrpracy')

  class(wynik) = c('utrata_etatu_df', class(wynik))
  return(wynik)
}