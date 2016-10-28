#' oblicza zmienne NNDN oraz NNNN
#' @param dane zbiór danych wygenerowany funkcją \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_nowi_pracodawcy = function(dane){
  stopifnot(
    is(dane, 'okienko_zus_zdau_df')
  )

  np = dane %>%
    filter_(~ okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    mutate_(
      npm_e = ~length(unique(id_platnika[etat])),
      np_e = ~length(unique(id_platnika[etat])),
      np_n = ~length(unique(id_platnika[netat])),
      epm_e = ~dplyr::coalesce(npm_e / nm_e, NA_real_),
      epm_e2 = ~dplyr::coalesce(npm_e / len, NA_real_),
      pp_e = ~100 * np_e / nm_e, # (0 dla 0/0),
      pp_n = ~100 * np_n / nm_n  # (NA dla 0/0),
    )
  #up_el = ~Liczba przypadków utraty etatu z uwagi na likwidację płatnika w okresie do 6 miesięcy od utraty etatu. NMLE – NMlenP,

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
    filter_(~ netat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nnnn = ~ sum(!is.na(id_platnika))
    )

  nnn = full_join(nndn, nnnn) %>%
    full_join(np) %>%
    uzupelnij_obserwacje(dane, 'id_zdau') %>%
    mutate_(
      nndn = ~ ifelse(is.na(nndn), 0, nndn),
      nnnn = ~ ifelse(is.na(nnnn), 0, nnnn)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(nnn)
}