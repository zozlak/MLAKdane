#' @title oblicza zmienne związane z liczbą pracodawców
#' @details
#' Zmienne opisujące pracodawców wymagają danych źródłowych na poziomie nie
#' wyższym niż {osoba x płatnik x okres}, przez co nie mogą być liczone z danych
#' zagregowanych do poziomu miesięcy w funkcji \code{\link{agreguj_do_okresu}} i
#' wymagają osobnej funkcji.
#' @param dane zbiór danych wygenerowany funkcją \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_pracodawcy = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df') & is(dane, 'baza_df')
  )

  if (multidplyr) {
    wynik = multidplyr::partition(dane, id_zdau)
  } else {
    wynik = dane
  }

  wynik = wynik %>%
    filter_(~ okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      len = ~first(len),
      nm_e = ~length(unique(okres[etat > 0])),
      nm_n = ~length(unique(okres[netat > 0])),
      np_e = ~length(unique(id_platnika[etat])),
      np_n = ~length(unique(id_platnika[netat])),
      pp_e = ~dplyr::coalesce(100 * np_e / nm_e, 0),
      pp_n = ~dplyr::coalesce(100 * np_n / nm_n, NA_real_)
    ) %>%
    select_('-nm_e', '-nm_n') %>%
    collect() %>%
    ungroup()

  npn = dane %>%
    filter_(~okres <= okres_max) %>%
    mutate_(
      przed = ~okres < okres_min
    ) %>%
    select_('id_zdau', 'id_platnika', 'przed', 'etat', 'netat') %>%
    distinct()
  poprzedni = npn %>%
    filter_(~przed == TRUE) %>%
    select_('id_zdau', 'id_platnika') %>%
    collect()

  npn_e = npn %>%
    filter_(~etat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      npn_e = ~ sum(!is.na(id_platnika))
    )

  npn_n = npn %>%
    filter_(~netat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      npn_n = ~sum(!is.na(id_platnika))
    )

  dane = wynik %>%
    left_join(npn_e) %>%
    left_join(npn_n) %>%
    mutate_(
      npn_e = ~dplyr::coalesce(npn_e, 0L),
      npn_n = ~dplyr::coalesce(npn_n, 0L),
      enpn_e   = ~dplyr::coalesce(12 * npn_e / len, NA_real_),
      enpn_n   = ~dplyr::coalesce(12 * npn_n / len, NA_real_)
    ) %>%
    select_('-len')

  return(dane)
}