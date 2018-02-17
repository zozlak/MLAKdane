#' agreguje dane na poziomie skladek do poziomu {id, id_zdau, okres, pkd}
#' @description Agreguje dane do poziomu {id, id_zdau, okres, pkd}
#' @param dane dane wygenerowane za pomocą funkcji
#'   \code{\link{polacz_zus_zdau}}
#' @param okres okres agregacji (okienko agregacji liczone jest jako
#'   \code{(okres - data_do) / okres})
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_pkd = function(dane, okres, multidplyr = TRUE) {
  stopifnot(
    methods::is(dane, 'baza_df')
  )

  dane = dane %>%
    filter_(~etat == 1 & okres >= data_do) %>%
    select_('id_zdau', 'id', 'data_do', 'okres', 'podst', 'pkd', 'pkd2', 'pkd_klasa', 'pkd_klasa_edu', 'klaszam', 'klaszam2', 'powpbezd_sr', 'powezar_sr', 'if_x_s')

  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  wynik = dane %>%
    group_by_('id_zdau', 'okres') %>%
    mutate_(
      podst_1 = ~sum(podst, na.rm = TRUE),
      podst_2 = ~podst,
      kwartal = ~floor((okres - data_do) / okres)
    ) %>%
    group_by_('id_zdau', 'id', 'kwartal', 'pkd', 'pkd2', 'pkd_klasa', 'pkd_klasa_edu') %>%
    summarize_(
      nm_e = length(unique(okres)),
      nm_e_s = ~length(unique(okres[if_x_s == 1L])),
      if_x_s = ~as.integer(any(if_x_s == 1L)),
      klaszam = ~ifelse(dplyr::n_distinct(klaszam) == 1L, klaszam, na),
      klasz   = ~dplyr::na_if(min(klaszam2, na.rm = TRUE), Inf),
      sz_e_n_1 = ~sum(podst_1[if_x_s == 0L], na.rm = TRUE),
      sz_e_s_1 = ~sum(podst_1[if_x_s == 1L], na.rm = TRUE),
      sz_e_n_2 = ~sum(podst_2[if_x_s == 0L], na.rm = TRUE),
      sz_e_s_2 = ~sum(podst_2[if_x_s == 1L], na.rm = TRUE),
      ezpow_e = ~mean(powezar_sr, na.rm = TRUE),
      ppow_b = ~mean(powpbezd_sr, na.rm = TRUE)
    ) %>%
    mutate_(
      nm_e_n = ~nm_e - nm_e_s,
      if_x_s = ~as.integer(nm_e_s > 0),
      sz_e_1 = ~dplyr::coalesce(sz_e_n_1, 0) + dplyr::coalesce(sz_e_s_1, 0),
      sz_e_2 = ~dplyr::coalesce(sz_e_n_2, 0) + dplyr::coalesce(sz_e_s_2, 0),
      ez_e_1 = ~sz_e_1 / dplyr::na_if(nm_e, 0),
      ez_e_2 = ~sz_e_2 / dplyr::na_if(nm_e, 0),
      ez_e_n_1 = ~sz_e_n_1 / dplyr::na_if(nm_e_n, 0),
      ez_e_n_2 = ~sz_e_n_2 / dplyr::na_if(nm_e_n, 0),
      ez_e_s_1 = ~sz_e_s_1 / dplyr::na_if(nm_e_s, 0),
      ez_e_s_2 = ~sz_e_s_2 / dplyr::na_if(nm_e_s, 0),
      wzg_ez_e_1 = ~ez_e_1 / ezpow_e,
      wzg_ez_e_2 = ~ez_e_2 / ezpow_e,
      wzg_ez_e_n_1 = ~ez_e_n_1 / ezpow_e,
      wzg_ez_e_n_2 = ~ez_e_n_2 / ezpow_e,
      wzg_ez_e_s_1 = ~ez_e_s_1 / ezpow_e,
      wzg_ez_e_s_2 = ~ez_e_s_2 / ezpow_e
    ) %>%
    collect() %>%
    ungroup()
  return(wynik)
}