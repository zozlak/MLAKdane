#' agreguje dane do poziomu {id, id_zdau, okres}
#' @description Agreguje dane do poziomu {id, id_zdau, okres}
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_do_miesiecy = function(dane, zdau, multidplyr = TRUE){
  stopifnot(
    methods::is(dane, 'baza_df'),
    methods::is(zdau, 'zdau_df')
  )

  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  dane = dane %>%
    group_by_('id', 'id_zdau', 'okres') %>%
    summarize_(
      len = ~as.integer(dplyr::first(okres <= koniec | is.na(koniec))),
      if_x_stprg = ~first(if_x_stprg),
      plec = ~first(plec),
      koniec = ~dplyr::first(koniec),
      ezpow     = ~dplyr::coalesce(mean(powezar_sr, na.rm = TRUE), NA_real_),
      ezpow_e   = ~dplyr::coalesce(mean(powezar_sr[etat > 0L], na.rm = TRUE), NA_real_),
      ezpow_e_n = ~dplyr::coalesce(mean(powezar_sr[etat > 0L & if_x_s == 0L], na.rm = TRUE), NA_real_),
      ezpow_e_s = ~dplyr::coalesce(mean(powezar_sr[etat > 0L & if_x_s == 1L], na.rm = TRUE), NA_real_),
      ezpow_n   = ~dplyr::coalesce(mean(powezar_sr[if_x_s == 0], na.rm = TRUE), NA_real_),
      ezpow_s   = ~dplyr::coalesce(mean(powezar_sr[if_x_s == 1], na.rm = TRUE), NA_real_),
      ezpow_z   = ~dplyr::coalesce(mean(powezar_sr[etat + netat > 0L], na.rm = TRUE), NA_real_),
      ezpow_z_n = ~dplyr::coalesce(mean(powezar_sr[etat + netat > 0L & if_x_s == 0L], na.rm = TRUE), NA_real_),
      ezpow_z_s = ~dplyr::coalesce(mean(powezar_sr[etat + netat > 0L & if_x_s == 1L], na.rm = TRUE), NA_real_),
      nm_b    = ~as.integer(any(bezrob > 0L)),
      nm_b2   = ~as.integer(any(bezrob > 0L & if_x_s + etat + netat + samoz + rentemer + prawnik + mundur + dziecko == 0)),
      nm_bd_n = ~as.integer(all(is.na(id_platnika) & if_x_s == 0L)),
      nm_bd_s = ~as.integer(all(is.na(id_platnika) & if_x_s == 1L)),
      nm_d_n  = ~as.integer(any(dziecko > 0L & if_x_s == 0L)),
      nm_d_s  = ~as.integer(any(dziecko > 0L & if_x_s == 1L)),
      nm_e_n  = ~as.integer(any(etat > 0L & if_x_s == 0L)),
      nm_e_s  = ~as.integer(any(etat > 0L & if_x_s == 1L)),
      nm_end  = ~as.integer(any(etat > 0L & dziecko == 0L)),
      nm_i_n  = ~as.integer(any(inne > 0L & if_x_s == 0L)),
      nm_i_s  = ~as.integer(any(inne > 0L & if_x_s == 1L)),
      nm_ii_n = ~as.integer(all(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0L) & any(inne > 0L & if_x_s == 0L)),
      nm_ii_s = ~as.integer(all(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0L) & any(inne > 0L & if_x_s == 1L)),
      nm_j_n  = ~as.integer(any(prawnik > 0L & if_x_s == 0L)),
      nm_j_s  = ~as.integer(any(prawnik > 0L & if_x_s == 1L)),
      nm_m_n  = ~as.integer(any(mundur > 0L & if_x_s == 0L)),
      nm_m_s  = ~as.integer(any(mundur > 0L & if_x_s == 1L)),
      nm_n_n  = ~as.integer(any(netat > 0L & if_x_s == 0L)),
      nm_n_s  = ~as.integer(any(netat > 0L & if_x_s == 1L)),
      nm_nnd  = ~as.integer(any(netat > 0L & dziecko == 0L)),
      nm_p_n  = ~as.integer(any((etat + netat + samoz > 0L) & if_x_s == 0L)),
      nm_p_s  = ~as.integer(any((etat + netat + samoz > 0L) & if_x_s == 1L)),
      nm_pnd_n = ~as.integer(any((etat + netat + samoz > 0L & dziecko == 0L) & if_x_s == 0L)),
      nm_pnd_s = ~as.integer(any((etat + netat + samoz > 0L & dziecko == 0L) & if_x_s == 1L)),
      nm_s_n  = ~as.integer(any(samoz > 0L & if_x_s == 0L)),
      nm_s_s  = ~as.integer(any(samoz > 0L & if_x_s == 1L)),
      nm_snd  = ~as.integer(any(samoz > 0L & dziecko == 0L)),
      nm_x_s  = ~as.integer(any(if_x_s == 1L)),
      nm_x_n  = ~1 - nm_x_s,
      nm_z_n  = ~as.integer(any((etat + netat > 0L) & if_x_s == 0L)),
      nm_z_s  = ~as.integer(any((etat + netat > 0L) & if_x_s == 1L)),
      npm_e    = ~length(unique(id_platnika[etat > 0L])),
      npm_e_n  = ~length(unique(id_platnika[etat > 0L & if_x_s == 0L])),
      npm_e_s  = ~length(unique(id_platnika[etat > 0L & if_x_s == 1L])),
      ppow_b   = ~dplyr::coalesce(mean(powpbezd_sr, na.rm = TRUE), NA_real_),
      ppow_b_n = ~dplyr::coalesce(mean(powpbezd_sr[if_x_s == 0L], na.rm = TRUE), NA_real_),
      sz_e_n = ~sum(podst[etat > 0L & if_x_s == 0L], na.rm = TRUE),
      sz_e_s = ~sum(podst[etat > 0L & if_x_s == 1L], na.rm = TRUE),
      sz_n_n = ~sum(podst[netat > 0L & if_x_s == 0L], na.rm = TRUE),
      sz_n_s = ~sum(podst[netat > 0L & if_x_s == 1L], na.rm = TRUE),
      sz_p_n = ~sum(podst[(etat + netat + samoz > 0L) & if_x_s == 0L], na.rm = TRUE),
      sz_p_s = ~sum(podst[(etat + netat + samoz > 0L) & if_x_s == 1L], na.rm = TRUE),
      sz_s_n = ~sum(podst[samoz > 0L & if_x_s == 0L], na.rm = TRUE),
      sz_s_s = ~sum(podst[samoz > 0L & if_x_s == 1L], na.rm = TRUE),
      sz_x_n = ~sum(podst[etat + netat + samoz + prawnik + mundur > 0L & if_x_s == 0L], na.rm = TRUE),
      sz_x_s = ~sum(podst[etat + netat + samoz + prawnik + mundur > 0L & if_x_s == 1L], na.rm = TRUE),
      sz_z_n = ~sum(podst[etat + netat > 0L & if_x_s == 0L], na.rm = TRUE),
      sz_z_s = ~sum(podst[etat + netat > 0L & if_x_s == 1L], na.rm = TRUE)
    ) %>%
    mutate_(
      na_n = ~ifelse(nm_x_n > 0L & !is.na(nm_x_n), 1L, NA_integer_),
      na_s = ~ifelse(nm_x_s > 0L & !is.na(nm_x_s), 1L, NA_integer_),
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
      nm_end   = ~nm_end * dplyr::coalesce(na_n, na_s),
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
      nm_nnd   = ~nm_nnd * dplyr::coalesce(na_n, na_s),
      nm_p_n   = ~nm_p_n * na_n,
      nm_p_s   = ~nm_p_s * na_s,
      nm_pnd_n = ~nm_pnd_n * na_n,
      nm_pnd_s = ~nm_pnd_s * na_s,
      nm_s_n   = ~nm_s_n * na_n,
      nm_s_s   = ~nm_s_s * na_s,
      nm_snd   = ~nm_snd * dplyr::coalesce(na_n, na_s),
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
    collect() %>%
    ungroup() %>%
    inner_join(zdau)

  dane = oblicz_zmienne_pochodne(dane, multidplyr)

  class(dane) = c('miesieczne_df', class(dane))
  return(dane)
}
