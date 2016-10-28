#' @title agreguje dane do poziomu {id, id_zdau}
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_do_okresu = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df') & is(dane, 'miesieczne_df')
  )

  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      len = ~dplyr::first(len),
      ezpow_e_n = ~dplyr::coalesce(mean(ezpow_e_n, na.rm = TRUE), NA_real_),
      ezpow_e_s = ~dplyr::coalesce(mean(ezpow_e_s, na.rm = TRUE), NA_real_),
      ezpow_n = ~dplyr::coalesce(mean(ezpow_n, na.rm = TRUE), NA_real_),
      ezpow_s = ~dplyr::coalesce(mean(ezpow_s, na.rm = TRUE), NA_real_),
      ezpow_z_n = ~dplyr::coalesce(mean(ezpow_z_n, na.rm = TRUE), NA_real_),
      ezpow_z_s = ~dplyr::coalesce(mean(ezpow_z_s, na.rm = TRUE), NA_real_),
      nm_b    = ~sum(nm_b, na.rm = TRUE),
      nm_b2   = ~sum(nm_b2, na.rm = TRUE),
      nm_bd_n = ~sum(nm_bd_n, na.rm = TRUE),
      nm_bd_s = ~sum(nm_bd_s, na.rm = TRUE),
      nm_d_n  = ~sum(nm_d_n, na.rm = TRUE),
      nm_d_s  = ~sum(nm_d_s, na.rm = TRUE),
      nm_e_n  = ~sum(nm_e_n, na.rm = TRUE),
      nm_e_s  = ~sum(nm_e_s, na.rm = TRUE),
      nm_i_n  = ~sum(nm_i_n, na.rm = TRUE),
      nm_i_s  = ~sum(nm_i_s, na.rm = TRUE),
      nm_j_n  = ~sum(nm_j_n, na.rm = TRUE),
      nm_j_s  = ~sum(nm_j_s, na.rm = TRUE),
      nm_m_n  = ~sum(nm_m_n, na.rm = TRUE),
      nm_m_s  = ~sum(nm_m_s, na.rm = TRUE),
      nm_n_n  = ~sum(nm_n_n, na.rm = TRUE),
      nm_n_s  = ~sum(nm_n_s, na.rm = TRUE),
      nm_p_n  = ~sum(nm_p_n, na.rm = TRUE),
      nm_p_s  = ~sum(nm_p_s, na.rm = TRUE),
      nm_s_n  = ~sum(nm_s_n, na.rm = TRUE),
      nm_s_s  = ~sum(nm_s_s, na.rm = TRUE),
      nm_st   = ~sum(nm_st, na.rm = TRUE),
      nm_z_n  = ~sum(nm_z_n, na.rm = TRUE),
      nm_z_s  = ~sum(nm_z_s, na.rm = TRUE),
      npm_e = ~sum(npm_e, na.rm = TRUE),
      ppow_b = ~dplyr::coalesce(mean(ppow_b, na.rm = TRUE), NA_real_),
      sz_e_n = ~sum(sz_e_n, na.rm = TRUE),
      sz_e_s = ~sum(sz_e_s, na.rm = TRUE),
      sz_n_n = ~sum(sz_n_n, na.rm = TRUE),
      sz_n_s = ~sum(sz_n_s, na.rm = TRUE),
      sz_p_n = ~sum(sz_p_n, na.rm = TRUE),
      sz_p_s = ~sum(sz_p_s, na.rm = TRUE),
      sz_s_n = ~sum(sz_s_n, na.rm = TRUE),
      sz_s_s = ~sum(sz_s_s, na.rm = TRUE),
      sz_x_n = ~sum(sz_x_n, na.rm = TRUE),
      sz_x_s = ~sum(sz_x_s, na.rm = TRUE),
      sz_z_n = ~sum(sz_z_n, na.rm = TRUE),
      sz_z_s = ~sum(sz_z_s, na.rm = TRUE)
    ) %>%
    collect() %>%
    ungroup()

  dane = oblicz_zmienne_pochodne(dane, multidplyr)

  return(dane)
}