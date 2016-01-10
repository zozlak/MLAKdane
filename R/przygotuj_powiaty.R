#' Przygotowuje powiatowe dane o bezrobociu i zarobkach
przygotuj_powiaty = function(){
  miesiace = setNames(1:12, c('styczeń', 'luty', 'marzec', 'kwiecień', 'maj', 'czerwiec', 'lipiec', 'sierpień', 'wrzesień', 'październik', 'listopad', 'grudzień'))

  # BDL: rynek pracy->bezrobocie rejestrowane->stopa bezrobocia rejestrowanego i stopa napływu bezrobotnych zarejestrowanych (dane miesięczne)
  # pobrane jako "tabela relacyjna" (czyli w postaci długiej), z ręcznie usuniętymi nagłówkami
  bezrobocie = read.csv2('dane/GUS_bezrobocie.csv', stringsAsFactors = FALSE)
  bezrobocie = bezrobocie %>%
    mutate_(
      terytPow = ~ bdl2teryt(ifelse(Kod == 0, '0000000000', Kod + 1)),
      terytWoj = ~ floor(terytPow / 100),
      okres    = ~ data2okres(sprintf('%d-%02d', Lata, miesiace[Miesiące])),
      powpbezd = ~ as.numeric(sub(',', '.', Wartość))
    ) %>%
    select_('terytPow', 'terytWoj', 'okres', 'powpbezd') %>%
    filter_(~ !is.na(powpbezd))

  # BDL: wynagrodzenia i świadczenia społeczne->wynagrodzenia->przeciętne miesięczne wynagrodzenia brutto w sektorze przedsiębiorstw wg PKD 2007 (dane krótkookresowe)
  # pobrane jako "tabela relacyjna" (czyli w postaci długiej), z ręcznie usuniętymi nagłówkami
  zarobki = read.csv2('dane/GUS_zarobki.csv', stringsAsFactors = FALSE)
  zarobki = zarobki %>%
    mutate_(
      terytWoj = ~ floor(bdl2teryt(ifelse(Kod == 0, '0000000000', Kod + 1)) / 100),
      okres    = ~ data2okres(sprintf('%d-%02d', Lata, miesiace[Okresy])),
      powezar  = ~ as.numeric(sub(',', '.', Wartość))
    ) %>%
    select_('terytWoj', 'okres', 'powezar') %>%
    filter_(~ !is.na(powezar))

  powiaty = suppressMessages(full_join(bezrobocie, zarobki)) %>%
    mutate_(
      teryt = ~ terytPow * 100,
      rok   = ~ floor(okres / 12)
    ) %>%
    select_('-terytPow', '-terytWoj')

  class(powiaty) = c('powiaty_df', class(powiaty))
  return(powiaty)
}