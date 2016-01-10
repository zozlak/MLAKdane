#' oblicza zmienne NNDN oraz NNNN
#' @param połączone zbiory zdau (plus zmienna okresMin) oraz zus
oblicz_nowi_pracodawcy = function(dane){
  stopifnot(
    is(dane, 'okienko_df')
  )

  nnn = dane %>%
    filter_(~ okres <= okres_max) %>%
    mutate_(
      przed = ~ okres < okres_min
    ) %>%
    select_('id_zdau', 'id_platnika', 'przed', 'etat', 'netat') %>%
    distinct()
  poprzedni = nnn %>%
    filter_(~ przed == TRUE) %>%
    select_('id_zdau', 'id_platnika')

  nndn = nnn %>%
    filter_(~ etat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nndn = ~ sum(!is.na(id_platnika))
    )

  nnnn = nnn %>%
    filter_(~ etat %in% 0 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nnnn = ~ sum(!is.na(id_platnika))
    )

  nnn = full_join(nndn, nnnn) %>%
    uzupelnij_obserwacje(dane, 'id_zdau') %>%
    mutate_(
      nndn = ~ ifelse(is.na(nndn), 0, nndn),
      nnnn = ~ ifelse(is.na(nnnn), 0, nnnn)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(nnn)
}