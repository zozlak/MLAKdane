#' oblicza zmienne UP_E, UP_EL oraz UP_ENL
#' @param okienko dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_okienko}} na danych miesięcznych (tzn. danych
#'   wygenerowanych wcześniej funkcją \code{\link{agreguj_do_miesiecy}})
#' @param utrataPracy dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_utrata_pracy}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_utrata_etatu = function(okienko, utrataPracy){
  stopifnot(
    methods::is(okienko, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )

  up = okienko %>%
    filter_(~okres >= okres_min & okres <= okres_max) %>%
    select_('id_zdau', 'id', 'okres', 'nm_e', 'nm_e_n', 'nm_e_s', 'if_x_s', 'len') %>%
    distinct() %>%
    left_join(utrataPracy)
  up = up %>%
    group_by_('id_zdau') %>%
    summarize_(
      len    = ~min(len, na.rm = TRUE),
      nm_e   = ~as.integer(sum(nm_e, na.rm = TRUE)),
      nm_e_n = ~as.integer(sum(nm_e_n, na.rm = TRUE)),
      nm_e_s = ~as.integer(sum(nm_e_s, na.rm = TRUE)),
      up_e_n   = ~as.integer(sum(if_else(if_x_s == 0L & utretatu == 1L & !is.na(utretatu), 1L, 0L), na.rm = TRUE)),
      up_e_s   = ~as.integer(sum(if_else(if_x_s == 1L & utretatu == 1L & !is.na(utretatu), 1L, 0L), na.rm = TRUE)),
      up_enl_n = ~as.integer(sum(if_else(if_x_s == 0L & utretatu_v2 == 1L & !is.na(utretatu), 1L, 0L), na.rm = TRUE)),
      up_enl_s = ~as.integer(sum(if_else(if_x_s == 1L & utretatu_v2 == 1L & !is.na(utretatu), 1L, 0L), na.rm = TRUE))
    ) %>%
    mutate_(
      up_e_n   = ~if_else(nm_e_n > 0L, up_e_n, NA_integer_),
      up_e_s   = ~if_else(nm_e_s > 0L, up_e_s, NA_integer_),
      up_enl_n = ~if_else(nm_e_n > 0L, up_enl_n, NA_integer_),
      up_enl_s = ~if_else(nm_e_s > 0L, up_enl_s, NA_integer_)
    ) %>%
    mutate_(
      up_el_n = ~up_e_n - up_enl_n,
      up_el_s = ~up_e_s - up_enl_s
    ) %>%
    mutate_(
      up_e   = ~if_else(nm_e > 0L, coalesce(up_e_n, 0L) + coalesce(up_e_s, 0L), NA_integer_),
      up_el  = ~if_else(nm_e > 0L, coalesce(up_el_n, 0L) + coalesce(up_el_s, 0L), NA_integer_),
      up_enl = ~if_else(nm_e > 0L, coalesce(up_enl_n, 0L) + coalesce(up_enl_s, 0L), NA_integer_)
    ) %>%
    mutate_(
      enup_e     = ~coalesce(12L * up_e / len, NA_real_),
      enup_e_n   = ~coalesce(12L * up_e_n / nm_e_n, NA_real_),
      enup_e_s   = ~coalesce(12L * up_e_s / nm_e_s, NA_real_),
      enup_el    = ~coalesce(12L * up_el / len, NA_real_),
      enup_el_n  = ~coalesce(12L * up_el_n / nm_e_n, NA_real_),
      enup_el_s  = ~coalesce(12L * up_el_s / nm_e_s, NA_real_),
      enup_enl   = ~coalesce(12L * up_enl / len, NA_real_),
      enup_enl_n = ~coalesce(12L * up_enl_n / nm_e_n, NA_real_),
      enup_enl_s = ~coalesce(12L * up_enl_s / nm_e_s, NA_real_)
    ) %>%
    select_('-len', '-nm_e', '-nm_e_n', '-nm_e_s') %>%
    ungroup()

  return(up)
}