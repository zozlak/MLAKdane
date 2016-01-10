#' oblicza zmienne NMLE oraz NMLEP
#' @param dane zbiór zdau (plus zmienne okresMin i okresMax) zus
przygotuj_utrata_pracy = function(zus, dataMax){
  stopifnot(
    is(zus, 'zus_df')
  )

  wynik = zus %>%
    select_('id', 'id_platnika', 'okres', 'etat', 'netat', 'samoz', 'platnik_kon') %>%
    group_by_('id', 'id_platnika', 'okres') %>%
    summarize_(
      etat        = ~ sum(etat, na.rm = TRUE),
      praca       = ~ sum(etat + netat + samoz, na.rm = TRUE),
      platnik_kon = ~ first(platnik_kon)
    ) %>%
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