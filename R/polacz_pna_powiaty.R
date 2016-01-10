polacz_pna_powiaty = function(pna, powiaty, dataMin, dataMax){
  stopifnot(
    is(pna, 'pna_df'),
    is(powiaty, 'powiaty_df')
  )

  pna = pna %>%
    filter_(~ rok >= floor(data2okres(dataMin) / 12) & rok <= floor(data2okres(dataMax) / 12))

  pnaAgr = pna %>%
    group_by_('pna', 'rok') %>%
    summarize_(
      pow_grodzki  = ~ ifelse(n_distinct(pow_grodzki) > 1, NA, first(pow_grodzki)),
      pow_grodzki2 = ~ min(pow_grodzki),
      miejzam      = ~ ifelse(n_distinct(miejzam) > 1, NA, first(miejzam)),
      miejzam2     = ~ min(miejzam),
      klasazam     = ~ ifelse(n_distinct(klasazam) > 1, NA, first(klasazam)),
      klasazam2    = ~ min(klasazam)
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
    left_join(powiaty)
  stopifnot(
    all(!is.na(pnaWskJST$okres))
  )

  wynik = pnaAgr %>%
    full_join(pnaWskSr) %>%
    full_join(pnaWskJST)
  stopifnot(
    nrow(pnaWskJST) == nrow(wynik),
    nrow(pnaWskSr)  == nrow(wynik)
  )

  class(wynik) = c('pna_powiaty_df', class(wynik))
  return(wynik)
}