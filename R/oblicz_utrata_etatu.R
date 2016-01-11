oblicz_utrata_etatu = function(okienko, utrataEtatu){
  stopifnot(
    is(okienko, 'okienko_df'),
    is(utrataEtatu, 'utrata_etatu_df')
  )

  nmle = okienko %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    select_('id_zdau', 'id', 'okres') %>%
    left_join(utrataEtatu) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nmle  = ~ sum(utretatu, na.rm = TRUE),
      nmlep = ~ sum(utretatu_v2, na.rm = TRUE)
    )

  class(nmle) = c('absolwent_df', class(nmle))
  return(nmle)
}