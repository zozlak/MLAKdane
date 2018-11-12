#' oblicza zmienne utraty pracy, używane do wyliczania zmiennych NMLE, NMLEP
#' oraz zmiennych czasowych
#' @param zus dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zus}}
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
przygotuj_utrata_pracy = function(zus, dataMax){
  stopifnot(
    methods::is(zus, 'zus_df')
  )

  wynik = zus %>%
    select_('id', 'id_platnika', 'okres', 'etat', 'netat', 'samoz', 'prawnik', 'mundur', 'platnik_kon') %>%
    group_by_('id', 'id_platnika', 'okres') %>%
    summarize_(
      etat        = ~sum(etat, na.rm = TRUE),
      samoz       = ~sum(samoz, na.rm = TRUE),
      zatr        = ~sum(etat + netat, na.rm = TRUE),
      praca       = ~sum(etat + netat + samoz, na.rm = TRUE),
      prawnik     = ~sum(prawnik, na.rm = TRUE),
      mundur      = ~sum(mundur, na.rm = TRUE),
      platnik_kon = ~first(platnik_kon)
    ) %>%
    arrange_('okres') %>%
    mutate_(dataMax = ~ data2okres(dataMax)) %>%
    group_by_('id', 'id_platnika') %>%
    mutate_(
      utretatu    = ~as.integer(etat    > 0 & (lead(etat)    %in% 0 | !is.na(lead(okres))  & lead(okres) - okres > 1 | is.na(lead(etat))    & okres != dataMax))
    ) %>%
    mutate_(
      utretatu_v2 = ~as.integer(utretatu %in% 1 & (is.na(platnik_kon) | platnik_kon - okres > 6)),
      utrsamoz    = ~as.integer(samoz   > 0 & (lead(samoz)   %in% 0 | is.na(lead(samoz))   & lead(okres) - okres > 1 | is.na(lead(samoz))   & okres != dataMax)),
      utrzatr     = ~as.integer(zatr    > 0 & (lead(zatr)    %in% 0 | is.na(lead(zatr))    & lead(okres) - okres > 1 | is.na(lead(zatr))    & okres != dataMax)),
      utrpracy    = ~as.integer(praca   > 0 & (lead(praca)   %in% 0 | is.na(lead(praca))   & lead(okres) - okres > 1 | is.na(lead(praca))   & okres != dataMax)),
      utrprawnik  = ~as.integer(prawnik > 0 & (lead(prawnik) %in% 0 | is.na(lead(prawnik)) & lead(okres) - okres > 1 | is.na(lead(prawnik)) & okres != dataMax)),
      utrmundur   = ~as.integer(mundur  > 0 & (lead(mundur)  %in% 0 | is.na(lead(mundur))  & lead(okres) - okres > 1 | is.na(lead(mundur))  & okres != dataMax))
    ) %>%
    select_('id', 'id_platnika', 'okres', 'utretatu', 'utretatu_v2', 'utrsamoz', 'utrzatr', 'utrpracy', 'utrprawnik', 'utrmundur') %>%
    filter_(~utretatu + utrsamoz + utrzatr + utrpracy + utrprawnik + utrmundur > 0) %>%
    ungroup()
  return(wynik)
}
