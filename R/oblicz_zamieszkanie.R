#' oblicza zmienne związane z miejscem zamieszkania
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#'   z danych wygenerowanych wcześniej funkcją \code{\link{polacz_zus_zdau}})
#' @param jednostki dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_jednostki}}
#' @param wMomDyplomu czy wyliczyć w momencie dyplomu czy dla końca okienka
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zamieszkanie = function(dane, jednostki, wMomDyplomu, multidplyr = FALSE){
  stopifnot(
    methods::is(dane, 'okienko_df') & methods::is(dane, 'baza_df')
  )

  jednostki = jednostki %>%
    select_('jednostka_id', 'teryt') %>%
    mutate_(
      powiat_jedn = ~floor(teryt / 100),
      woj_jedn    = ~floor(teryt / 10000)
    ) %>%
    select_('-teryt')

  dane = dane %>%
    filter_(~okres == ifelse(rep(wMomDyplomu, nrow(dane)), data_do, okres_max)) %>%
    left_join(jednostki) %>%
    mutate_(
      jpdzam = ~ifelse(
        powiat_jedn == floor(teryt / 100), 1, ifelse(
          woj_jedn == floor(teryt / 10000), 2, ifelse(
            teryt > 0, 3, NA)
          )
        )
    )
  if (multidplyr) {
    dane = multidplyr::partition(dane, id_zdau)
  } else {
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      powiat    = ~uzgodnij_teryt(teryt),
      klaszam   = ~ifelse(n_distinct(klaszam) == 1, klaszam, NA),
      klasz     = ~min(klaszam2),
      miejzam   = ~ifelse(n_distinct(miejzam) == 1, miejzam, NA),
      miejz     = ~min(miejzam2),
      jpdzam    = ~min(jpdzam),
      jpdzam_v2 = ~ifelse(n_distinct(jpdzam) == 1, jpdzam, NA)
    ) %>%
    mutate_(
      nrwoj  = ~ifelse(powiat > 0, floor(powiat / 10000), 0),
      powiat = ~ifelse(powiat %% 10000 > 0, powiat, 0)
    ) %>%
    collect()

  return(dane)
}