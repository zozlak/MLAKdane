#' oblicza zmienne UP_E, UP_EL oraz UP_ENL
#' @param okienko dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_okienko}} na danych miesięcznych (tzn. danych
#'   wygenerowanych wcześniej funkcją \code{\link{agreguj_do_miesiecy}})
#' @param utrataEtatu dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_utrata_etatu}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_utrata_etatu = function(okienko, utrataEtatu, multidplyr = TRUE){
  stopifnot(
    is(okienko, 'okienko_df') & is(okienko, 'miesieczne_df'),
    is(utrataEtatu, 'utrata_etatu_df')
  )

  up = okienko %>%
    filter_(~okres >= okres_min & okres <= okres_max) %>%
    select_('id_zdau', 'id', 'okres', 'nm_e', 'len') %>%
    distinct() %>%
    left_join(utrataEtatu)
  if (multidplyr) {
    up = multidplyr::partition(up, id_zdau)
  } else {
    up = group_by_(up, 'id_zdau')
  }
  up = up %>%
    summarize_(
      len = ~first(len),
      up_e   = ~ifelse(sum(nm_e) > 0, sum(utretatu, na.rm = TRUE), NA_integer_),
      up_enl = ~ifelse(sum(nm_e) > 0, sum(utretatu_v2, na.rm = TRUE), NA_integer_)
    ) %>%
    mutate_(
      up_el = ~up_e - up_enl,
      enup_e   = ~dplyr::coalesce(12 * up_e / len, NA_real_),
      enup_el  = ~dplyr::coalesce(12 * up_el / len, NA_real_),
      enup_enl = ~dplyr::coalesce(12 * up_enl / len, NA_real_)
    ) %>%
    select_('-len') %>%
    collect() %>%
    ungroup()

  return(up)
}