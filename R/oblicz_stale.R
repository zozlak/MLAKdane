#' oblicza proste zmienne niezwiazane z okienkami czasu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_stale = function(dane, zdau){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )

  dane = dane %>%
    inner_join(
      zdau %>%
        select_('id_zdau', 'data_od'),
      copy = TRUE
    )

  w1 = dane %>%
    select_('id_zdau', 'id_platnika') %>%
    group_by_('id_zdau') %>%
    summarize_(
      if_zus = ~as.integer(sum(as.integer(!is.na(id_platnika)), na.rm = TRUE) > 0L)
    ) %>%
    ungroup()

  # w trakcie studiów
  w2 = dane %>%
    select_('id_zdau', 'okres', 'data_od', 'data_do', 'etat', 'netat', 'samoz', 'podst') %>%
    filter_(~okres >= data_od & okres <= data_do) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nm_k   = ~min(data_do, na.rm = TRUE) - min(data_od, na.rm = TRUE) + 1,
      nm_e_k = ~as.integer(n_distinct(if_else(etat > 0L, okres, NA_integer_))),
      nm_z_k = ~as.integer(n_distinct(if_else(etat + netat > 0L, okres, NA_integer_))),
      nm_s_k = ~as.integer(n_distinct(if_else(samoz > 0L, okres, NA_integer_))),
      sz_k   = ~sum(podst, na.rm = TRUE),
      sz_e_k = ~sum(if_else(etat > 0L, podst, 0L), na.rm = TRUE),
      sz_z_k = ~sum(if_else(etat + netat > 0L, podst, 0L), na.rm = TRUE),
      sz_s_k = ~sum(if_else(samoz > 0L, podst, 0L), na.rm = TRUE)
    ) %>%
    mutate_(
      if_es_k = ~as.integer(nm_e_k + nm_s_k > 0L),
      ez_k   = ~sz_k / nm_k,
      ez_e_k = ~sz_e_k / nm_e_k,
      ez_z_k = ~sz_z_k / nm_z_k,
      ez_s_k = ~sz_s_k / nm_s_k
    ) %>%
    ungroup()

  # przed studiami
  w3 = dane %>%
    select_('id_zdau', 'okres', 'data_od', 'data_do', 'etat', 'netat', 'samoz', 'podst') %>%
    filter_(~ okres < data_od) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nm_r   = ~as.integer(n_distinct(okres)),
      nm_e_r = ~as.integer(n_distinct(if_else(etat > 0L, okres, NA_integer_))),
      nm_z_r = ~as.integer(n_distinct(if_else(etat + netat > 0L, okres, NA_integer_))),
      nm_s_r = ~as.integer(n_distinct(if_else(samoz > 0L, okres, NA_integer_))),
      sz_r   = ~sum(podst, na.rm = TRUE),
      sz_e_r = ~sum(if_else(etat > 0L, podst, 0), na.rm = TRUE),
      sz_z_r = ~sum(if_else(etat + netat > 0L, podst, 0), na.rm = TRUE),
      sz_s_r = ~sum(if_else(samoz > 0L, podst, 0), na.rm = TRUE)
    ) %>%
    mutate_(
      if_es_r = ~as.integer(nm_e_r + nm_s_r > 0),
      ez_r   = ~sz_r / nm_r,
      ez_e_r = ~sz_e_r / nm_e_r,
      ez_z_r = ~sz_z_r / nm_z_r,
      ez_s_r = ~sz_s_r / nm_s_r
    ) %>%
    ungroup()

  dane = w1 %>%
    full_join(w2) %>%
    full_join(w3) %>%
    mutate_(
      if_es_k = ~coalesce(if_es_k, 0L),
      if_es_r = ~coalesce(if_es_r, 0L)
    ) %>%
    mutate_(
      dosw_es = ~if_else(if_es_r > 0L, 1L, if_else(if_es_k > 0L, 2L, 3L))
    )
  return(dane %>% collect())
}