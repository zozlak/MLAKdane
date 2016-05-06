#' oblicza zmienne generowane z poziomu (zus x zdau) przez poziom (zdau x okres) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_absolwent_okres = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }
  dane = dane %>%
    group_by_('id_zdau', 'okres') %>%
    summarize_(
      bezrob  = ~ sum(bezrob),
      gbezd   = ~ mean(powpbezd_sr, na.rm = TRUE),
      gezd    = ~ mean(powezar_sr, na.rm = TRUE),
      nbezrob = ~ sum(etat + netat + samoz + rentemer + student),
      nem     = ~ length(unique(id_platnika[!is.na(id_platnika) & etat == 1])),
      zpow    = ~ mean(powezar_sr[etat + netat > 0], na.rm = TRUE)
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      gbezd   = ~ mean(gbezd[is.finite(gbezd)], na.rm = TRUE),
      gezd    = ~ mean(gezd[is.finite(gezd)], na.rm = TRUE),
      nem     = ~ sum(nem, na.rm = TRUE),
      nmb_v2  = ~ sum(bezrob > 0 & nbezrob == 0),
      zpow    = ~ mean(zpow[is.finite(zpow)], na.rm = TRUE)
    ) %>%
    mutate_(
      gbezd   = ~ ifelse(is.finite(gbezd), gbezd, NA),
      gezd    = ~ ifelse(is.finite(gezd), gezd, NA),
      zpow    = ~ ifelse(is.finite(zpow), zpow, NA)
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}