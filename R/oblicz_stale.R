#' oblicza proste zmienne niezwiazane z okienkami czasu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_w_baze}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_stale = function(dane, zdau, multidplyr = TRUE){
  stopifnot(
    methods::is(dane, 'baza_df'),
    methods::is(zdau, 'zdau_df')
  )

  dane = zdau %>%
    select_('id_zdau', 'data_od') %>%
    inner_join(dane)

  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  w1 = dane %>%
    select_('id_zdau', 'id_platnika') %>%
    group_by_('id_zdau') %>%
    summarize_(
      if_zus = ~as.numeric(any(!is.na(id_platnika)))
    ) %>%
    collect() %>%
    ungroup()

  # w trakcie studiów
  w2 = dane %>%
    select_('id_zdau', 'okres', 'data_od', 'data_do', 'etat', 'netat', 'samoz', 'podst') %>%
    filter_(~okres >= data_od & okres <= data_do) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nm_k   = ~first(data_do) - first(data_od) + 1,
      nm_e_k = ~length(unique(okres[etat > 0])),
      nm_z_k = ~length(unique(okres[etat + netat > 0])),
      nm_s_k = ~length(unique(okres[samoz > 0])),
      sz_k   = ~sum(podst, na.rm = TRUE),
      sz_e_k = ~sum(podst[etat > 0], na.rm = TRUE),
      sz_z_k = ~sum(podst[etat + netat > 0], na.rm = TRUE),
      sz_s_k = ~sum(podst[samoz > 0], na.rm = TRUE)
    ) %>%
    mutate_(
      if_es_k = ~as.integer(nm_e_k + nm_s_k > 0),
      ez_k    = ~dplyr::coalesce(sz_k / nm_k, NA_real_),
      ez_e_k  = ~dplyr::coalesce(sz_e_k / nm_e_k, NA_real_),
      ez_z_k  = ~dplyr::coalesce(sz_z_k / nm_z_k, NA_real_),
      ez_s_k  = ~dplyr::coalesce(sz_s_k / nm_s_k, NA_real_)
    ) %>%
    collect() %>%
    ungroup()

  # przed studiami
  w3 = dane %>%
    select_('id_zdau', 'okres', 'data_od', 'data_do', 'etat', 'netat', 'samoz', 'podst') %>%
    filter_(~ okres < data_od) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nm_r   = ~length(unique(okres)),
      nm_e_r = ~length(unique(okres[etat > 0])),
      nm_z_r = ~length(unique(okres[etat + netat > 0])),
      nm_s_r = ~length(unique(okres[samoz > 0])),
      sz_r   = ~sum(podst, na.rm = TRUE),
      sz_e_r = ~sum(podst[etat > 0], na.rm = TRUE),
      sz_z_r = ~sum(podst[etat + netat > 0], na.rm = TRUE),
      sz_s_r = ~sum(podst[samoz > 0], na.rm = TRUE)
    ) %>%
    mutate_(
      if_es_r = ~as.integer(nm_e_r + nm_s_r > 0),
      ez_r    = ~dplyr::coalesce(sz_r / nm_r, NA_real_),
      ez_e_r  = ~dplyr::coalesce(sz_e_r / nm_e_r, NA_real_),
      ez_z_r  = ~dplyr::coalesce(sz_z_r / nm_z_r, NA_real_),
      ez_s_r  = ~dplyr::coalesce(sz_s_r / nm_s_r, NA_real_)
    ) %>%
    collect() %>%
    ungroup()

  dane = w1 %>%
    full_join(w2) %>%
    full_join(w3) %>%
    mutate_(
      if_es_k = ~coalesce(if_es_k, 0L),
      if_es_r = ~coalesce(if_es_r, 0L),
      dosw_es = ~ifelse(if_es_r > 0, 1, ifelse(if_es_k > 0, 2, 3))
    )
  return(dane)
}