#' oblicza zmienne UP_E, UP_EL oraz UP_ENL
#' @param okienko dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_okienko}} na danych miesięcznych (tzn. danych
#'   wygenerowanych wcześniej funkcją \code{\link{agreguj_do_miesiecy}})
#' @param utrataPracy dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_utrata_pracy}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_utrata_etatu = function(okienko, utrataPracy, multidplyr = TRUE){
  stopifnot(
    methods::is(okienko, 'okienko_df') & methods::is(okienko, 'miesieczne_df'),
    methods::is(utrataPracy, 'utrata_pracy_df')
  )

  up = okienko %>%
    filter_(~okres >= okres_min & okres <= okres_max) %>%
    select_('id_zdau', 'id', 'okres', 'nm_e', 'nm_e_n', 'nm_e_s', 'if_x_s', 'len') %>%
    distinct() %>%
    left_join(utrataPracy)
  if (multidplyr) {
    up = multidplyr::partition(up, id_zdau)
  } else {
    up = group_by_(up, 'id_zdau')
  }
  up = up %>%
    summarize_(
      len  = ~first(len),
      nm_e = ~sum(nm_e, na.rm = TRUE),
      up_e_n   = ~ifelse(sum(nm_e_n, na.rm = TRUE) > 0L, sum(utretatu[if_x_s == 0L], na.rm = TRUE), NA_integer_),
      up_e_s   = ~ifelse(sum(nm_e_s, na.rm = TRUE) > 0L, sum(utretatu[if_x_s == 1L], na.rm = TRUE), NA_integer_),
      up_enl_n = ~ifelse(sum(nm_e_n, na.rm = TRUE) > 0L, sum(utretatu_v2[if_x_s == 0L], na.rm = TRUE), NA_integer_),
      up_enl_s = ~ifelse(sum(nm_e_s, na.rm = TRUE) > 0L, sum(utretatu_v2[if_x_s == 1L], na.rm = TRUE), NA_integer_)
    ) %>%
    mutate_(
      up_el_n = ~up_e_n - up_enl_n,
      up_el_s = ~up_e_s - up_enl_s,
      up_e   = ~ifelse(nm_e > 0L, dplyr::coalesce(up_e_n, 0L) + dplyr::coalesce(up_e_s, 0L), NA_integer_),
      up_el  = ~ifelse(nm_e > 0L, dplyr::coalesce(up_el_n, 0L) + dplyr::coalesce(up_el_s, 0L), NA_integer_),
      up_enl = ~ifelse(nm_e > 0L, dplyr::coalesce(up_enl_n, 0L) + dplyr::coalesce(up_enl_s, 0L), NA_integer_),
      enup_e     = ~dplyr::coalesce(12L * up_e / len, NA_real_),
      enup_e_n   = ~dplyr::coalesce(12L * up_e_n / len, NA_real_),
      enup_e_s   = ~dplyr::coalesce(12L * up_e_s / len, NA_real_),
      enup_el    = ~dplyr::coalesce(12L * up_el / len, NA_real_),
      enup_el_n  = ~dplyr::coalesce(12L * up_el_n / len, NA_real_),
      enup_el_s  = ~dplyr::coalesce(12L * up_el_s / len, NA_real_),
      enup_enl   = ~dplyr::coalesce(12L * up_enl / len, NA_real_),
      enup_enl_n = ~dplyr::coalesce(12L * up_enl_n / len, NA_real_),
      enup_enl_s = ~dplyr::coalesce(12L * up_enl_s / len, NA_real_)
    ) %>%
    select_('-len', '-nm_e') %>%
    collect() %>%
    ungroup()

  return(up)
}