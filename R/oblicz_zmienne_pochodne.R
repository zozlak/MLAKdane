#' oblicza zmienne wywodzone z już wcześniej policzonych
#' @param dane ramki danych wygenerowane wewnątrz pomocą funkcji
#'   \code{\link{agreguj_do_miesiecy}} lub \code{\link{agreguj_do_okresu}}
#' @return data.frame wyliczone zmienne
#' @import dplyr
oblicz_zmienne_pochodne = function(dane, multidplyr = TRUE){
  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  dane = dane %>%
    mutate_(
      nm_bd = ~nm_bd_n + nm_bd_s,
      nm_d  = ~nm_d_n + nm_d_s,
      nm_e  = ~nm_e_n + nm_e_s,
      nm_i  = ~nm_i_n + nm_i_s,
      nm_j  = ~nm_j_n + nm_j_s,
      nm_m  = ~nm_m_n + nm_m_s,
      nm_n  = ~nm_n_n + nm_n_s,
      nm_p  = ~nm_p_n + nm_p_s,
      nm_s  = ~nm_s_n + nm_s_s,
      nm_z  = ~nm_z_n + nm_z_s,
      if_b   = ~nm_b > 0L,
      if_b2  = ~nm_b2 > 0L,
      if_e   = ~nm_e > 0L,
      if_e_n = ~nm_e_n > 0L,
      if_e_s = ~nm_e_s > 0L,
      if_m   = ~nm_m > 0L,
      if_p   = ~nm_p > 0L,
      if_s   = ~nm_s > 0L,
      if_st  = ~nm_st > 0L,
      epm_e  = ~dplyr::coalesce(npm_e / nm_e, NA_real_),
      pm_b    = ~dplyr::coalesce(100 * nm_b / len, NA_real_),
      pm_b2   = ~dplyr::coalesce(100 * nm_b2 / len, NA_real_),
      pm_bd   = ~dplyr::coalesce(100 * nm_bd / len, NA_real_),
      pm_bd_n = ~dplyr::coalesce(100 * nm_bd_n / len, NA_real_),
      pm_bd_s = ~dplyr::coalesce(100 * nm_bd_s / len, NA_real_),
      pm_be   = ~dplyr::coalesce((len - nm_e) / len, NA_real_),
      pm_bp   = ~dplyr::coalesce((len - nm_p) / len, NA_real_),
      pm_d    = ~dplyr::coalesce(100 * nm_d / len, NA_real_),
      pm_d_n  = ~dplyr::coalesce(100 * nm_d_n / len, NA_real_),
      pm_d_s  = ~dplyr::coalesce(100 * nm_d_s / len, NA_real_),
      pm_e    = ~dplyr::coalesce(100 * nm_e / len, NA_real_),
      pm_e_n  = ~dplyr::coalesce(100 * nm_e_n / len, NA_real_),
      pm_e_s  = ~dplyr::coalesce(100 * nm_e_s / len, NA_real_),
      pm_i    = ~dplyr::coalesce(100 * nm_i / len, NA_real_),
      pm_i_n  = ~dplyr::coalesce(100 * nm_i_n / len, NA_real_),
      pm_i_s  = ~dplyr::coalesce(100 * nm_i_s / len, NA_real_),
      pm_n    = ~dplyr::coalesce(100 * nm_n / len, NA_real_),
      pm_n_n  = ~dplyr::coalesce(100 * nm_n_n / len, NA_real_),
      pm_n_s  = ~dplyr::coalesce(100 * nm_n_s / len, NA_real_),
      pm_p    = ~dplyr::coalesce(100 * nm_p / len, NA_real_),
      pm_p_n  = ~dplyr::coalesce(100 * nm_p_n / len, NA_real_),
      pm_p_s  = ~dplyr::coalesce(100 * nm_p_s / len, NA_real_),
      pm_s    = ~dplyr::coalesce(100 * nm_s / len, NA_real_),
      pm_s_n  = ~dplyr::coalesce(100 * nm_s_n / len, NA_real_),
      pm_s_s  = ~dplyr::coalesce(100 * nm_s_s / len, NA_real_),
      pm_st   = ~dplyr::coalesce(100 * nm_st / len, NA_real_),
      sz_e = ~sz_e_n + sz_e_s,
      sz_n = ~sz_n_n + sz_n_s,
      sz_p = ~sz_p_n + sz_p_s,
      sz_s = ~sz_s_n + sz_s_s,
      sz_x = ~sz_x_n + sz_x_s,
      sz_z = ~sz_z_n + sz_z_s,
      ez_e   = ~dplyr::coalesce(sz_e / nm_e, NA_real_),
      ez_e_n = ~dplyr::coalesce(sz_e_n / nm_e_n, NA_real_),
      ez_e_s = ~dplyr::coalesce(sz_e_s / nm_e_s, NA_real_),
      ez_x   = ~dplyr::coalesce(sz_x / len, NA_real_),
      ez_z   = ~dplyr::coalesce(sz_z / nm_z, NA_real_),
      ez_z_n = ~dplyr::coalesce(sz_z_n / nm_z_n, NA_real_),
      ez_z_s = ~dplyr::coalesce(sz_z_s / nm_z_s, NA_real_),
      wzg_ez_e    = ~dplyr::coalesce(ez_e / ezpow_e, NA_real_),
      wzg_ez_z    = ~dplyr::coalesce(ez_z / ezpow_z, NA_real_),
      wzg_ez_z_n   = ~dplyr::coalesce(ez_z_n / ezpow_n, NA_real_),
      wzg_ez_z_s   = ~dplyr::coalesce(ez_z_s / ezpow_s, NA_real_),
      wzg_ryzbez   = ~dplyr::coalesce(pm_b / ppow_b, 0)
    ) %>%
    collect() %>%
    ungroup()
  return(dane)
}