#' @title oblicza zmienne związane z liczbą pracodawców
#' @description
#' Zmienne opisujące pracodawców wymagają danych źródłowych na poziomie nie
#' wyższym niż {osoba x płatnik x okres}, przez co nie mogą być liczone z danych
#' zagregowanych do poziomu miesięcy w funkcji \code{\link{agreguj_do_okresu}} i
#' wymagają osobnej funkcji.
#' @param dane zbiór danych wygenerowany funkcją \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_pracodawcy = function(dane){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )
  wynik = dane %>%
    filter_(~okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      len = ~mean(len, na.rm = TRUE),
      nm_e = ~as.integer(n_distinct(if_else(etat > 0L, okres, NA_integer_))),
      nm_n = ~as.integer(n_distinct(if_else(netat > 0L, okres, NA_integer_))),
      np_e = ~as.integer(n_distinct(if_else(etat > 0L, id_platnika, NA_integer_))),
      np_n = ~as.integer(n_distinct(if_else(netat > 0L, id_platnika, NA_integer_)))
    ) %>%
    mutate_(
      pp_e = ~if_else(nm_e > 0L, 100 * np_e / nm_e, 0),
      pp_n = ~if_else(nm_n > 0L, 100 * np_n / nm_n, NA_real_)
    ) %>%
    select_('-nm_e', '-nm_n') %>%
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
    select_('id_zdau', 'id_platnika')

  npn_e = npn %>%
    filter_(~etat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      npn_e = ~as.integer(sum(as.integer(!is.na(id_platnika)), na.rm = TRUE))
    )

  npn_n = npn %>%
    filter_(~netat %in% 1 & przed == FALSE) %>%
    anti_join(poprzedni) %>%
    group_by_('id_zdau') %>%
    summarize_(
      npn_n = ~as.integer(sum(as.integer(!is.na(id_platnika)), na.rm = TRUE))
    )

  dane = wynik %>%
    left_join(npn_e) %>%
    left_join(npn_n) %>%
    mutate_(
      npn_e = ~coalesce(npn_e, 0L),
      npn_n = ~coalesce(npn_n, 0L)
    ) %>%
    mutate_(
      enpn_e   = ~coalesce(12 * npn_e / len, NA_real_),
      enpn_n   = ~coalesce(12 * npn_n / len, NA_real_)
    ) %>%
    select_('-len')

  return(dane)
}