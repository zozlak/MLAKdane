#' przygotowuje powiatowe dane o bezrobociu i zarobkach
#' @export
#' @import dplyr
przygotuj_powiaty = function(){
  miesiace = stats::setNames(1:12, c('styczeń', 'luty', 'marzec', 'kwiecień', 'maj', 'czerwiec', 'lipiec', 'sierpień', 'wrzesień', 'październik', 'listopad', 'grudzień'))

  # BDL: rynek pracy->bezrobocie rejestrowane->stopa bezrobocia rejestrowanego i stopa napływu bezrobotnych zarejestrowanych (dane miesięczne)
  # pobrane w podziale wg TERYT!
  # pobrane jako "tabela relacyjna" (czyli w postaci długiej)
  bezrobocie = utils::read.csv2('dane/GUS_bezrobocie.csv', stringsAsFactors = FALSE)
  bezrobocie = bezrobocie %>%
    rename_(rok = 'Rok') %>%
    mutate_(
      terytPow = ~floor(Kod / 1000),
      terytWoj = ~floor(terytPow / 100),
      okres    = ~data2okres(sprintf('%d-%02d', rok, miesiace[Miesiące])),
      powpbezd = ~as.numeric(sub(',', '.', Wartosc))
    ) %>%
    select_('terytPow', 'terytWoj', 'rok', 'okres', 'powpbezd') %>%
    filter_(~ !is.na(powpbezd))
  stopifnot(
    bezrobocie %>% group_by_('terytPow', 'okres') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0
  )

  # BDL: wynagrodzenia i świadczenia społeczne->wynagrodzenia->przeciętne miesięczne wynagrodzenia brutto
  # pobrane w podziale wg TERYT!
  # pobrane jako "tabela relacyjna" (czyli w postaci długiej)
  zarobki = utils::read.csv2('dane/GUS_zarobki.csv', stringsAsFactors = FALSE)
  zarobki = zarobki %>%
    rename_(rok = 'Rok') %>%
    mutate_(
      terytWoj = ~floor(Kod / 100000),
      powezar  = ~as.numeric(sub(',', '.', Wartosc))
    ) %>%
    select_('terytWoj', 'rok', 'powezar') %>%
    filter_(~ !is.na(powezar)) %>%
    arrange_('terytWoj', 'rok') %>%
    group_by_('terytWoj') %>%
    mutate_(
      powezar_p1 = ~(lead(powezar) - powezar) / (lead(rok) - rok) / 12,
      powezar_m1 = ~(powezar - lag(powezar)) / (rok - lag(rok)) / 12
    ) %>%
    mutate_(
      powezar_p1 = ~coalesce(powezar_p1, powezar_m1),
      powezar_m1 = ~coalesce(powezar_m1, powezar_p1)
    ) %>%
    ungroup()
  stopifnot(
    zarobki %>% group_by_('terytWoj', 'rok') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0
  )

  powiaty = suppressMessages(full_join(zarobki, bezrobocie)) %>%
    mutate_(
      teryt = ~ terytPow * 100
    ) %>%
    mutate_(
      miesiac = ~okres2miesiac(okres),
      powezar = ~powezar - pmax(0, 6.5 - miesiac) * powezar_m1 + pmax(0, miesiac - 6.5) * powezar_p1
    ) %>%
    select_('teryt', 'rok', 'okres', 'powpbezd', 'powezar')

  stopifnot(
    nrow(powiaty) == nrow(bezrobocie),
    powiaty %>% group_by_('teryt') %>% summarize_(n = ~n()) %>% group_by_('n') %>% summarize_(nn = ~n()) %>% nrow() == 1
  )

  class(powiaty) = c('powiaty_df', class(powiaty))
  return(powiaty)
}