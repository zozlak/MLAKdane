#' agreguje dane do poziomu {id, id_zdau}
#' @description Agreguje dane do poziomu {id, id_zdau}
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_do_okresu = function(dane){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      len       = ~min(len, na.rm = TRUE),
      ezpow     = ~mean(ezpow, na.rm = TRUE),
      ezpow_e   = ~mean(ezpow_e, na.rm = TRUE),
      ezpow_e_n = ~mean(ezpow_e_n, na.rm = TRUE),
      ezpow_e_s = ~mean(ezpow_e_s, na.rm = TRUE),
      ezpow_n   = ~mean(ezpow_n, na.rm = TRUE),
      ezpow_s   = ~mean(ezpow_s, na.rm = TRUE),
      ezpow_z   = ~mean(ezpow_z, na.rm = TRUE),
      ezpow_z_n = ~mean(ezpow_z_n, na.rm = TRUE),
      ezpow_z_s = ~mean(ezpow_z_s, na.rm = TRUE),
      nm_b     = ~coalesce(as.integer(sum(nm_b, na.rm = TRUE)), 0L),
      nm_b2    = ~coalesce(as.integer(sum(nm_b2, na.rm = TRUE)), 0L),
      nm_bd_n  = ~coalesce(as.integer(sum(nm_bd_n, na.rm = TRUE)), 0L),
      nm_bd_s  = ~coalesce(as.integer(sum(nm_bd_s, na.rm = TRUE)), 0L),
      nm_d_n   = ~coalesce(as.integer(sum(nm_d_n, na.rm = TRUE)), 0L),
      nm_d_s   = ~coalesce(as.integer(sum(nm_d_s, na.rm = TRUE)), 0L),
      nm_e_n   = ~coalesce(as.integer(sum(nm_e_n, na.rm = TRUE)), 0L),
      nm_e_s   = ~coalesce(as.integer(sum(nm_e_s, na.rm = TRUE)), 0L),
      nm_end   = ~coalesce(as.integer(sum(nm_end, na.rm = TRUE)), 0L),
      nm_i_n   = ~coalesce(as.integer(sum(nm_i_n, na.rm = TRUE)), 0L),
      nm_i_s   = ~coalesce(as.integer(sum(nm_i_s, na.rm = TRUE)), 0L),
      nm_ii_n  = ~coalesce(as.integer(sum(nm_ii_n, na.rm = TRUE)), 0L),
      nm_ii_s  = ~coalesce(as.integer(sum(nm_ii_s, na.rm = TRUE)), 0L),
      nm_j_n   = ~coalesce(as.integer(sum(nm_j_n, na.rm = TRUE)), 0L),
      nm_j_s   = ~coalesce(as.integer(sum(nm_j_s, na.rm = TRUE)), 0L),
      nm_m_n   = ~coalesce(as.integer(sum(nm_m_n, na.rm = TRUE)), 0L),
      nm_m_s   = ~coalesce(as.integer(sum(nm_m_s, na.rm = TRUE)), 0L),
      nm_n_n   = ~coalesce(as.integer(sum(nm_n_n, na.rm = TRUE)), 0L),
      nm_n_s   = ~coalesce(as.integer(sum(nm_n_s, na.rm = TRUE)), 0L),
      nm_nnd   = ~coalesce(as.integer(sum(nm_nnd, na.rm = TRUE)), 0L),
      nm_p_n   = ~coalesce(as.integer(sum(nm_p_n, na.rm = TRUE)), 0L),
      nm_p_s   = ~coalesce(as.integer(sum(nm_p_s, na.rm = TRUE)), 0L),
      nm_pnd_n = ~coalesce(as.integer(sum(nm_pnd_n, na.rm = TRUE)), 0L),
      nm_pnd_s = ~coalesce(as.integer(sum(nm_pnd_s, na.rm = TRUE)), 0L),
      nm_s_n   = ~coalesce(as.integer(sum(nm_s_n, na.rm = TRUE)), 0L),
      nm_s_s   = ~coalesce(as.integer(sum(nm_s_s, na.rm = TRUE)), 0L),
      nm_snd   = ~coalesce(as.integer(sum(nm_snd, na.rm = TRUE)), 0L),
      nm_x_n   = ~coalesce(as.integer(sum(nm_x_n, na.rm = TRUE)), 0L),
      nm_x_s   = ~coalesce(as.integer(sum(nm_x_s, na.rm = TRUE)), 0L),
      nm_z_n   = ~coalesce(as.integer(sum(nm_z_n, na.rm = TRUE)), 0L),
      nm_z_s   = ~coalesce(as.integer(sum(nm_z_s, na.rm = TRUE)), 0L),
      npm_e    = ~coalesce(as.integer(sum(npm_e, na.rm = TRUE)), 0L),
      npm_e_n  = ~coalesce(as.integer(sum(npm_e_n, na.rm = TRUE)), 0L),
      npm_e_s  = ~coalesce(as.integer(sum(npm_e_s, na.rm = TRUE)), 0L),
      ppow_b   = ~mean(ppow_b, na.rm = TRUE),
      ppow_b_n = ~mean(ppow_b_n, na.rm = TRUE),
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
    mutate_(
      na_n = ~if_else(nm_x_n > 0L & !is.na(nm_x_n), 1L, NA_integer_),
      na_s = ~if_else(nm_x_s > 0L & !is.na(nm_x_s), 1L, NA_integer_)
    ) %>%
    mutate_(
      ezpow_e_n = ~ezpow_e_n * na_n,
      ezpow_e_s = ~ezpow_e_s * na_s,
      ezpow_z_n = ~ezpow_z_n * na_n,
      ezpow_z_s = ~ezpow_z_s * na_s,
      nm_bd_n  = ~nm_bd_n * na_n,
      nm_bd_s  = ~nm_bd_s * na_s,
      nm_d_n   = ~nm_d_n * na_n,
      nm_d_s   = ~nm_d_s * na_s,
      nm_e_n   = ~nm_e_n * na_n,
      nm_e_s   = ~nm_e_s * na_s,
      nm_end   = ~nm_end * coalesce(na_n, na_s),
      nm_i_n   = ~nm_i_n * na_n,
      nm_i_s   = ~nm_i_s * na_s,
      nm_ii_n  = ~nm_ii_n * na_n,
      nm_ii_s  = ~nm_ii_s * na_s,
      nm_j_n   = ~nm_j_n * na_n,
      nm_j_s   = ~nm_j_s * na_s,
      nm_m_n   = ~nm_m_n * na_n,
      nm_m_s   = ~nm_m_s * na_s,
      nm_n_n   = ~nm_n_n * na_n,
      nm_n_s   = ~nm_n_s * na_s,
      nm_nnd   = ~nm_nnd * coalesce(na_n, na_s),
      nm_p_n   = ~nm_p_n * na_n,
      nm_p_s   = ~nm_p_s * na_s,
      nm_pnd_n = ~nm_pnd_n * na_n,
      nm_pnd_s = ~nm_pnd_s * na_s,
      nm_s_n   = ~nm_s_n * na_n,
      nm_s_s   = ~nm_s_s * na_s,
      nm_snd   = ~nm_snd * coalesce(na_n, na_s),
      nm_z_n   = ~nm_z_n * na_n,
      nm_z_s   = ~nm_z_s * na_s,
      npm_e_n  = ~nm_e_n * na_n,
      npm_e_s  = ~nm_e_s * na_s,
      sz_e_n = ~sz_e_n * na_n,
      sz_e_s = ~sz_e_s * na_s,
      sz_n_n = ~sz_n_n * na_n,
      sz_n_s = ~sz_n_s * na_s,
      sz_p_n = ~sz_p_n * na_n,
      sz_p_s = ~sz_p_s * na_s,
      sz_s_n = ~sz_s_n * na_n,
      sz_s_s = ~sz_s_s * na_s,
      sz_x_n = ~sz_x_n * na_n,
      sz_x_s = ~sz_x_s * na_s,
      sz_z_n = ~sz_z_n * na_n,
      sz_z_s = ~sz_z_s * na_s
    ) %>%
    select_('-na_n', '-na_s') %>%
    ungroup()

  dane = oblicz_zmienne_pochodne(dane)

  return(dane)
}
