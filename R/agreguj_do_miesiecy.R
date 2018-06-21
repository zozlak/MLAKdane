#' agreguje dane do poziomu {id, id_zdau, okres}
#' @description Agreguje dane do poziomu {id, id_zdau, okres}
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param grupy lista zmiennych wyznaczających grupy (domyślnie 'id', 'id_zdau'
#'   i 'okres')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_do_miesiecy = function(dane, zdau, grupy = c('id', 'id_zdau', 'okres')){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )
  dane = dane %>%
    group_by_(.dots = grupy) %>%
    summarize_(
      len = ~as.integer(sum(as.integer(okres <= koniec | is.na(koniec)), na.rm = TRUE) > 0L),
      if_x_stprg = ~min(if_x_stprg, na.rm = TRUE), # wystarczyłoby wziąć pierwszą wartość, ale Spark nie zna first()
      koniec = ~min(koniec, na.rm = TRUE), # wystarczyłoby wziąć pierwszą wartość, ale Spark nie zna first(); rzutowanie na integer zamienia NaN-a w NA
      ezpow     = ~mean(powezar_sr, na.rm = TRUE),
      ezpow_e   = ~mean(if_else(etat > 0L,                        powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_e_n = ~mean(if_else(etat > 0L & if_x_s == 0L,         powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_e_s = ~mean(if_else(etat > 0L & if_x_s == 1L,         powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_n   = ~mean(if_else(if_x_s == 0L,                     powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_s   = ~mean(if_else(if_x_s == 1L,                     powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_z   = ~mean(if_else(etat + netat > 0L,                powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_z_n = ~mean(if_else(etat + netat > 0L & if_x_s == 0L, powezar_sr, NA_real_), na.rm = TRUE),
      ezpow_z_s = ~mean(if_else(etat + netat > 0L & if_x_s == 1L, powezar_sr, NA_real_), na.rm = TRUE),
      nm_b    = ~as.integer(sum(as.integer(bezrob > 0L), na.rm = TRUE) > 0L),
      nm_b2   = ~as.integer(sum(as.integer(bezrob > 0L & if_x_s + etat + netat + samoz + rentemer + prawnik + mundur + dziecko == 0), na.rm = TRUE) > 0L),
      nm_bd_n = ~as.integer(sum(as.integer(is.na(id_platnika) & if_x_s == 0L), na.rm = TRUE) == n()),
      nm_bd_s = ~as.integer(sum(as.integer(is.na(id_platnika) & if_x_s == 1L), na.rm = TRUE) == n()),
      nm_d_n  = ~as.integer(sum(as.integer(dziecko > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_d_s  = ~as.integer(sum(as.integer(dziecko > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_e_n  = ~as.integer(sum(as.integer(etat > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_e_s  = ~as.integer(sum(as.integer(etat > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_end  = ~as.integer(sum(as.integer(etat > 0L & dziecko == 0L), na.rm = TRUE) > 0L),
      nm_i_n  = ~as.integer(sum(as.integer(inne > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_i_s  = ~as.integer(sum(as.integer(inne > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_ii_n = ~as.integer(sum(as.integer(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0L), na.rm = TRUE) == n() & sum(as.integer(inne > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_ii_s = ~as.integer(sum(as.integer(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0L), na.rm = TRUE) == n() & sum(as.integer(inne > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_j_n  = ~as.integer(sum(as.integer(prawnik > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_j_s  = ~as.integer(sum(as.integer(prawnik > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_m_n  = ~as.integer(sum(as.integer(mundur > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_m_s  = ~as.integer(sum(as.integer(mundur > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_n_n  = ~as.integer(sum(as.integer(netat > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_n_s  = ~as.integer(sum(as.integer(netat > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_nnd  = ~as.integer(sum(as.integer(netat > 0L & dziecko == 0L), na.rm = TRUE) > 0L),
      nm_p_n  = ~as.integer(sum(as.integer((etat + netat + samoz > 0L) & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_p_s  = ~as.integer(sum(as.integer((etat + netat + samoz > 0L) & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_pnd_n = ~as.integer(sum(as.integer((etat + netat + samoz > 0L & dziecko == 0L) & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_pnd_s = ~as.integer(sum(as.integer((etat + netat + samoz > 0L & dziecko == 0L) & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_s_n  = ~as.integer(sum(as.integer(samoz > 0L & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_s_s  = ~as.integer(sum(as.integer(samoz > 0L & if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_snd  = ~as.integer(sum(as.integer(samoz > 0L & dziecko == 0L), na.rm = TRUE) > 0L),
      nm_x_s  = ~as.integer(sum(as.integer(if_x_s == 1L), na.rm = TRUE) > 0L),
      nm_z_n  = ~as.integer(sum(as.integer((etat + netat > 0L) & if_x_s == 0L), na.rm = TRUE) > 0L),
      nm_z_s  = ~as.integer(sum(as.integer((etat + netat > 0L) & if_x_s == 1L), na.rm = TRUE) > 0L),
      npm_e    = ~n_distinct(if_else(etat > 0L,                id_platnika, NA_integer_)),
      npm_e_n  = ~n_distinct(if_else(etat > 0L & if_x_s == 0L, id_platnika, NA_integer_)),
      npm_e_s  = ~n_distinct(if_else(etat > 0L & if_x_s == 1L, id_platnika, NA_integer_)),
      ppow_b   = ~mean(powpbezd_sr, na.rm = TRUE),
      ppow_b_n = ~mean(if_else(if_x_s == 0L, powpbezd_sr, NA_real_), na.rm = TRUE),
      sz_e_n = ~sum(if_else(etat > 0L & if_x_s == 0L,                                    podst, 0), na.rm = TRUE),
      sz_e_s = ~sum(if_else(etat > 0L & if_x_s == 1L,                                    podst, 0), na.rm = TRUE),
      sz_n_n = ~sum(if_else(netat > 0L & if_x_s == 0L,                                   podst, 0), na.rm = TRUE),
      sz_n_s = ~sum(if_else(netat > 0L & if_x_s == 1L,                                   podst, 0), na.rm = TRUE),
      sz_p_n = ~sum(if_else((etat + netat + samoz > 0L) & if_x_s == 0L,                  podst, 0), na.rm = TRUE),
      sz_p_s = ~sum(if_else((etat + netat + samoz > 0L) & if_x_s == 1L,                  podst, 0), na.rm = TRUE),
      sz_s_n = ~sum(if_else(samoz > 0L & if_x_s == 0L,                                   podst, 0), na.rm = TRUE),
      sz_s_s = ~sum(if_else(samoz > 0L & if_x_s == 1L,                                   podst, 0), na.rm = TRUE),
      sz_x_n = ~sum(if_else(etat + netat + samoz + prawnik + mundur > 0L & if_x_s == 0L, podst, 0), na.rm = TRUE),
      sz_x_s = ~sum(if_else(etat + netat + samoz + prawnik + mundur > 0L & if_x_s == 1L, podst, 0), na.rm = TRUE),
      sz_z_n = ~sum(if_else(etat + netat > 0L & if_x_s == 0L,                            podst, 0), na.rm = TRUE),
      sz_z_s = ~sum(if_else(etat + netat > 0L & if_x_s == 1L,                            podst, 0), na.rm = TRUE)
    ) %>%
    mutate_(
      nm_x_n = ~1 - nm_x_s,
      na_n = 1L,
      na_s = 1L
    ) %>%
    mutate_(
      na_n = ~if_else(is.na(nm_x_n) | nm_x_n == 0L, NA_integer_, 1L),
      na_s = ~if_else(is.na(nm_x_s) | nm_x_s == 0L, NA_integer_, 1L)
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
    ungroup() %>%
    inner_join(zdau)

  dane = oblicz_zmienne_pochodne(dane)

  return(dane)
}
