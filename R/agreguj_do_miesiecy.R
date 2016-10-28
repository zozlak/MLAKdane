#' @title agreguje dane do poziomu {id, id_zdau, okres}
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
agreguj_do_miesiecy = function(dane, zdau, multidplyr = TRUE){
  stopifnot(
    is(dane, 'baza_df'),
    is(zdau, 'zdau_df')
  )

  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  }

  dane = dane %>%
    group_by_('id', 'id_zdau', 'okres') %>%
    summarize_(
      len = ~as.integer(dplyr::first(okres <= koniec | is.na(koniec))),
      if_stprg = ~first(if_stprg),
      koniec = ~dplyr::first(koniec),
      ezpow_e_n = ~dplyr::coalesce(mean(powezar_sr[etat > 0 & if_st == 0], na.rm = TRUE), NA_real_),
      ezpow_e_s = ~dplyr::coalesce(mean(powezar_sr[etat > 0 & if_st == 1], na.rm = TRUE), NA_real_),
      ezpow_n   = ~dplyr::coalesce(mean(powezar_sr[if_st == 0], na.rm = TRUE), NA_real_),
      ezpow_s   = ~dplyr::coalesce(mean(powezar_sr[if_st == 1], na.rm = TRUE), NA_real_),
      ezpow_z_n = ~dplyr::coalesce(mean(powezar_sr[etat + netat > 0 & if_st == 0], na.rm = TRUE), NA_real_),
      ezpow_z_s = ~dplyr::coalesce(mean(powezar_sr[etat + netat > 0 & if_st == 1], na.rm = TRUE), NA_real_),
      nm_b    = ~as.integer(any(bezrob > 0)),
      nm_b2   = ~as.integer(any(bezrob > 0 & if_st + etat + netat + samoz + rentemer + prawnik + mundur + dziecko == 0)),
      nm_bd_n = ~as.integer(all(is.na(id_platnika) & if_st == 0)),
      nm_bd_s = ~as.integer(all(is.na(id_platnika) & if_st == 1)),
      nm_d_n  = ~as.integer(any(dziecko > 0 & if_st == 0)),
      nm_d_s  = ~as.integer(any(dziecko > 0 & if_st == 1)),
      nm_e_n  = ~as.integer(any(etat > 0 & if_st == 0)),
      nm_e_s  = ~as.integer(any(etat > 0 & if_st == 1)),
      nm_i_n  = ~as.integer(any(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0 & !is.na(id_platnika) & if_st == 0)),
      nm_i_s  = ~as.integer(any(etat + netat + samoz + bezrob + rentemer + prawnik + mundur + dziecko == 0 & !is.na(id_platnika) & if_st == 1)),
      nm_j_n  = ~as.integer(any(prawnik > 0 & if_st == 0)),
      nm_j_s  = ~as.integer(any(prawnik > 0 & if_st == 1)),
      nm_m_n  = ~as.integer(any(mundur > 0 & if_st == 0)),
      nm_m_s  = ~as.integer(any(mundur > 0 & if_st == 1)),
      nm_n_n  = ~as.integer(any(netat > 0 & if_st == 0)),
      nm_n_s  = ~as.integer(any(netat > 0 & if_st == 1)),
      nm_p_n  = ~as.integer(any((etat + netat + samoz > 0) & if_st == 0)),
      nm_p_s  = ~as.integer(any((etat + netat + samoz > 0) & if_st == 1)),
      nm_s_n  = ~as.integer(any(samoz > 0 & if_st == 0)),
      nm_s_s  = ~as.integer(any(samoz > 0 & if_st == 1)),
      nm_st   = ~as.integer(any(if_st == 1)),
      nm_z_n  = ~as.integer(any((etat + netat > 0) & if_st == 0)),
      nm_z_s  = ~as.integer(any((etat + netat > 0) & if_st == 1)),
      npm_e  = ~length(unique(id_platnika[etat > 0])),
      ppow_b = ~dplyr::coalesce(mean(powpbezd_sr, na.rm = TRUE), NA_real_),
      sz_e_n = ~sum(podst[etat > 0 & if_st == 0], na.rm = TRUE),
      sz_e_s = ~sum(podst[etat > 0 & if_st == 1], na.rm = TRUE),
      sz_n_n = ~sum(podst[netat > 0 & if_st == 0], na.rm = TRUE),
      sz_n_s = ~sum(podst[netat > 0 & if_st == 1], na.rm = TRUE),
      sz_p_n = ~sum(podst[(etat + netat + samoz > 0) & if_st == 0], na.rm = TRUE),
      sz_p_s = ~sum(podst[(etat + netat + samoz > 0) & if_st == 1], na.rm = TRUE),
      sz_s_n = ~sum(podst[samoz > 0 & if_st == 0], na.rm = TRUE),
      sz_s_s = ~sum(podst[samoz > 0 & if_st == 1], na.rm = TRUE),
      sz_x_n = ~sum(podst[etat + netat + samoz + prawnik + mundur > 0 & if_st == 0], na.rm = TRUE),
      sz_x_s = ~sum(podst[etat + netat + samoz + prawnik + mundur > 0 & if_st == 1], na.rm = TRUE),
      sz_z_n = ~sum(podst[etat + netat > 0 & if_st == 0], na.rm = TRUE),
      sz_z_s = ~sum(podst[etat + netat > 0 & if_st == 1], na.rm = TRUE)
    ) %>%
    collect() %>%
    ungroup() %>%
    inner_join(zdau)

  dane = oblicz_zmienne_pochodne(dane, multidplyr)

  class(dane) = c('miesieczne_df', class(dane))
  return(dane)
}