#' oblicza zmienne generowane z poziomu (zus x zdau) przez poziom (zdau x okres) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_miesieczne = function(dane, zdau, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df'),
    is(zdau, 'zdau_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }
  dane = dane %>%
    group_by_('id_zdau', 'okres') %>%
    summarize_(
      gbezd = ~ mean(powpbezd_sr, na.rm = TRUE),
      nmb   = ~ max(bezrob == 1),
      nmz   = ~ max(etat == 1 | netat == 1),
      sze   = ~ sum(podst[etat == 1], na.rm = TRUE),
      szn   = ~ sum(podst[netat == 1], na.rm = TRUE),
      zpow  = ~ mean(powezar_sr[etat + netat > 0], na.rm = TRUE)
    ) %>%
    mutate_(
      zpow = ~ ifelse(is.finite(zpow), zpow, NA)
    ) %>%
    mutate_(
      bilod = ~ 100 * nmb / gbezd,
      zilo = ~ (sze + szn) / (nmz * zpow)
    ) %>%
    select_('bilod', 'zilo')
    collect()
  dane = dane %>%
    inner_join(
      zdau %>%
        select_('id_zdau', 'uczelnia_id', 'jednostka_id', 'kierunek_id', 'data_rozp', 'data_zak')
    )
  class(dane) = c('miesieczne_df', class(dane))
  return(dane)
}