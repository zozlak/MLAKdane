#' przyłącza dane powiatów do danych PNA i agreguje je tak, by (rok, pna) było
#' unikalne
#' @param pna dane PNA wygenerowane za pomocą funkcji \code{\link{przygotuj_pna}}
#' @param powiaty dane powiatów wygenerowane za pomocą funkcji \code{\link{przygotuj_powiaty}}
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2015-09-30')
#' @return data.frame wyliczone dane
#' @export
#' @import dplyr
polacz_pna_powiaty = function(pna, powiaty, dataMin, dataMax){
  stopifnot(
    methods::is(pna, 'pna_df'),
    methods::is(powiaty, 'powiaty_df'),
    pna %>% group_by_('pna5', 'rok') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0,
    powiaty %>% group_by_('okres', 'teryt') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow() == 0
  )

  pna = pna %>%
    filter_(~ rok >= floor(data2okres(dataMin) / 12) & rok <= floor(data2okres(dataMax) / 12))

  pnaAgr = pna %>%
    group_by_('pna', 'rok') %>%
    summarize_(
      pow_grodzki2 = ~ min(pow_grodzki),
      pow_grodzki  = ~ ifelse(n_distinct(pow_grodzki) > 1, NA, first(pow_grodzki)),
      miejzam2     = ~ min(miejzam),
      miejzam      = ~ ifelse(n_distinct(miejzam) > 1, NA, first(miejzam)),
      klaszam2     = ~ min(klaszam),
      klaszam      = ~ ifelse(n_distinct(klaszam) > 1, NA, first(klaszam))
    )

  pnaWskSr = pna %>%
    select_('rok', 'pna', 'teryt') %>%
    distinct() %>%
    left_join(powiaty)
  stopifnot(
    all(!is.na(pnaWskSr$okres))
  )
  pnaWskSr = pnaWskSr %>%
    group_by_('rok', 'pna', 'okres') %>%
    summarize_(
      powpbezd_sr = ~ mean(powpbezd, na.rm = TRUE),
      powezar_sr  = ~ mean(powezar, na.rm = TRUE)
    )

  pnaWskJST = pna %>%
    select_('rok', 'pna', 'teryt') %>%
    distinct() %>%
    group_by_('rok', 'pna') %>%
    summarize_(
      teryt = ~ uzgodnij_teryt(teryt)
    ) %>%
    left_join(powiaty) %>%
    rename_(
      powpbezd_teryt = 'powpbezd',
      powezar_teryt  = 'powezar'
    )
  stopifnot(
    all(!is.na(pnaWskJST$okres))
  )

  wynik = pnaAgr %>%
    full_join(pnaWskSr) %>%
    full_join(pnaWskJST) %>%
    select_('-rok')
  stopifnot(
    nrow(pnaWskJST) == nrow(wynik),
    nrow(pnaWskSr)  == nrow(wynik)
  )

  class(wynik) = c('pna_powiaty_df', class(wynik))
  return(wynik)
}
