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
    mutate_(
      terytPow = ~floor(Kod / 1000),
      terytWoj = ~floor(terytPow / 100),
      okres    = ~data2okres(sprintf('%d-%02d', Rok, miesiace[Miesiące])),
      powpbezd = ~as.numeric(sub(',', '.', Wartosc))
    ) %>%
    select_('terytPow', 'terytWoj', 'Rok', 'okres', 'powpbezd') %>%
    filter_(~ !is.na(powpbezd))
  stopifnot(
    bezrobocie %>% group_by_('terytPow', 'okres') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0
  )

  # BDL: wynagrodzenia i świadczenia społeczne->wynagrodzenia->przeciętne miesięczne wynagrodzenia brutto
  # pobrane w podziale wg TERYT!
  # pobrane jako "tabela relacyjna" (czyli w postaci długiej)
  zarobki = utils::read.csv2('dane/GUS_zarobki.csv', stringsAsFactors = FALSE)
  zarobki = zarobki %>%
    mutate_(
      terytWoj = ~floor(Kod / 100000),
      powezar  = ~as.numeric(sub(',', '.', Wartosc))
    ) %>%
    select_('terytWoj', 'Rok', 'powezar') %>%
    filter_(~ !is.na(powezar))
  stopifnot(
    zarobki %>% group_by_('terytWoj', 'Rok') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0
  )

  powiaty = suppressMessages(full_join(bezrobocie, zarobki)) %>%
    mutate_(
      teryt = ~ terytPow * 100
    ) %>%
    rename_(rok = 'Rok') %>%
    select_('-terytPow', '-terytWoj')

  stopifnot(
    nrow(powiaty) == nrow(bezrobocie),
    powiaty %>% group_by_('teryt') %>% summarize_(n = ~n()) %>% group_by_('n') %>% summarize_(nn = ~n()) %>% nrow() == 1
  )

  class(powiaty) = c('powiaty_df', class(powiaty))
  return(powiaty)
}