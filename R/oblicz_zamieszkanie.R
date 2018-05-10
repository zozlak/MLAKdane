#' oblicza zmienne związane z miejscem zamieszkania
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#'   z danych wygenerowanych wcześniej funkcją \code{\link{polacz_zus_zdau}})
#' @param jednostki dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_jednostki}}
#' @param wMomDyplomu czy wyliczyć w momencie dyplomu czy dla końca okienka
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zamieszkanie = function(dane, jednostki, wMomDyplomu){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )

  jednostki = jednostki %>%
    select_('rok', 'jednostka_id', 'teryt') %>%
    mutate_(
      powiat_jedn = ~as.integer(teryt / 100L),
      woj_jedn    = ~as.integer(teryt / 10000L)
    ) %>%
    select_('-teryt')

  dane = dane %>%
    mutate_(wMomDyplomu = ~wMomDyplomu) %>%
    filter_(~okres == if_else(wMomDyplomu, data_do, okres_max)) %>%
    mutate_(rok = ~as.integer((okres - 1) / 12)) %>%
    left_join(jednostki, copy = TRUE) %>%
    mutate_(
      jpdzam_ = ~ifelse(
        powiat_jedn == as.integer(teryt / 100L), 1L, if_else(
          woj_jedn == as.integer(teryt / 10000L), 2L, if_else(
            teryt > 0L, 3L, NA_integer_)
          )
        )
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      powiat    = ~if_else(
        n_distinct(teryt) == 1L,
        min(teryt, na.rm = TRUE),
        if_else(
          n_distinct(as.integer(teryt / 100L)) == 1L,
          min(as.integer(teryt / 100L), na.rm = TRUE) * 100L,
          if_else(
            n_distinct(as.integer(teryt / 10000L)) == 1L,
            min(as.integer(teryt / 10000L), na.rm = TRUE) * 10000L,
            0L
          )
        )
      ),
      klaszam   = ~if_else(n_distinct(klaszam) == 1L, min(klaszam, na.rm = TRUE), NA_integer_),
      klasz     = ~min(klaszam2, na.rm = TRUE),
      miejzam   = ~if_else(n_distinct(miejzam) == 1L, min(miejzam, na.rm = TRUE), NA_integer_),
      miejz     = ~min(miejzam2, na.rm = TRUE),
      jpdzam    = ~min(jpdzam_, na.rm = TRUE),
      jpdzam_v2 = ~if_else(n_distinct(jpdzam_) == 1L, min(jpdzam_, na.rm = TRUE), NA_integer_)
    ) %>%
    mutate_(
      nrwoj  = ~if_else(powiat > 0L, as.integer(powiat / 10000L), 0L),
      powiat = ~if_else(powiat %% 10000L > 0L, powiat, 0L)
    )

  return(dane)
}