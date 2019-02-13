#' oblicza zmienne utraty pracy, używane do wyliczania zmiennych NMLE, NMLEP
#' oraz zmiennych czasowych
#' @param zus dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zus}}
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
przygotuj_utrata_pracy = function(zus, dataMax){
  dataMaxOkres = data2okres(dataMax)

  wynik = zus %>%
    select_('id', 'id_platnika', 'okres', 'etat', 'netat', 'samoz', 'prawnik', 'mundur', 'platnik_kon') %>%
    group_by_('id', 'id_platnika', 'okres') %>%
    summarize_(
      etat        = ~as.integer(sum(etat, na.rm = TRUE)),
      samoz       = ~as.integer(sum(samoz, na.rm = TRUE)),
      zatr        = ~as.integer(sum(etat + netat, na.rm = TRUE)),
      praca       = ~as.integer(sum(etat + netat + samoz, na.rm = TRUE)),
      prawnik     = ~as.integer(sum(prawnik, na.rm = TRUE)),
      mundur      = ~as.integer(sum(mundur, na.rm = TRUE)),
      platnik_kon = ~first(platnik_kon)
    ) %>%
    arrange_('okres') %>%
    mutate_(dataMax = dataMaxOkres) %>%
    group_by_('id', 'id_platnika') %>%
    mutate_(
      utretatu    = ~as.integer(etat    > 0L & (lead(etat)    %in% 0L | !is.na(lead(okres))  & lead(okres) - okres > 1L | is.na(lead(etat))    & okres != dataMax))
    ) %>%
    mutate_(
      utretatu_v2 = ~as.integer(utretatu %in% 1L & (is.na(platnik_kon) | platnik_kon - okres > 6)),
      utrsamoz    = ~as.integer(samoz   > 0L & (lead(samoz)   %in% 0L | is.na(lead(samoz))   & lead(okres) - okres > 1L | is.na(lead(samoz))   & okres != dataMax)),
      utrzatr     = ~as.integer(zatr    > 0L & (lead(zatr)    %in% 0L | is.na(lead(zatr))    & lead(okres) - okres > 1L | is.na(lead(zatr))    & okres != dataMax)),
      utrpracy    = ~as.integer(praca   > 0L & (lead(praca)   %in% 0L | is.na(lead(praca))   & lead(okres) - okres > 1L | is.na(lead(praca))   & okres != dataMax)),
      utrprawnik  = ~as.integer(prawnik > 0L & (lead(prawnik) %in% 0L | is.na(lead(prawnik)) & lead(okres) - okres > 1L | is.na(lead(prawnik)) & okres != dataMax)),
      utrmundur   = ~as.integer(mundur  > 0L & (lead(mundur)  %in% 0L | is.na(lead(mundur))  & lead(okres) - okres > 1L | is.na(lead(mundur))  & okres != dataMax))
    ) %>%
    select_('id', 'id_platnika', 'okres', 'utretatu', 'utretatu_v2', 'utrsamoz', 'utrzatr', 'utrpracy', 'utrprawnik', 'utrmundur') %>%
    filter_(~utretatu + utrsamoz + utrzatr + utrpracy + utrprawnik + utrmundur > 0) %>%
    ungroup()
  return(wynik)
}
