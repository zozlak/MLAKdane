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
      zpow    = ~ mean(powezar_sr[etat + netat > 0], na.rm = TRUE),
      nmes     = ~ any(etat == 1) & all(student2 == 1),
      nmns     = ~ any(netat == 1) & all(etat == 0 & student2 == 1),
      nmdzs    = ~ any(dziecko == 1) & all(student2 == 1),
      nminnes  = ~ any(!is.na(id_platnika)) & all(etat == 0 & netat == 0 & dziecko == 0 & bezrob == 0 & student2 == 1),
      nmbss    = ~ all(is.na(id_platnika) & student2 == 1),
      nmens    = ~ any(etat == 1) & all(student2 == 0),
      nmnns    = ~ any(netat == 1) & all(etat == 0 & student2 == 0),
      nmdzns   = ~ any(dziecko == 1) & all(student2 == 0),
      nminnens = ~ any(!is.na(id_platnika)) & all(etat == 0 & netat == 0 & dziecko == 0 & bezrob == 0 & student2 == 0),
      nmbns    = ~ any(bezrob == 1) & all(student2 == 0),
      nmbsns   = ~ all(is.na(id_platnika) & student2 == 0),
      nn       = ~ nmes + nmns + nmdzs + nminnes + nmbss + nmens + nmnns + nmdzns + nminnens + nmbns + nmbsns
    ) %>%
    group_by_('id_zdau') %>%
    mutate_(
      nn       = ~ as.numeric(ifelse(nn == 0, 1, nn)),
      nmes     = ~ nmes / nn,
      nmns     = ~ nmns / nn,
      nmdzs    = ~ nmdzs / nn,
      nminnes  = ~ nminnes / nn,
      nmbss    = ~ nmbss / nn,
      nmens    = ~ nmens / nn,
      nmnns    = ~ nmnns / nn,
      nmdzns   = ~ nmdzns / nn,
      nminnens = ~ nminnens / nn,
      nmbns    = ~ nmbns / nn,
      nmbsns   = ~ nmbsns / nn
    ) %>%
    summarize_(
      gbezd   = ~ mean(gbezd[is.finite(gbezd)], na.rm = TRUE),
      gezd    = ~ mean(gezd[is.finite(gezd)], na.rm = TRUE),
      nem     = ~ sum(nem, na.rm = TRUE),
      nmb_v2  = ~ sum(bezrob > 0 & nbezrob == 0),
      zpow    = ~ mean(zpow[is.finite(zpow)], na.rm = TRUE),
      nmes     = ~ sum(nmes),
      nmns     = ~ sum(nmns),
      nmdzs    = ~ sum(nmdzs),
      nminnes  = ~ sum(nminnes),
      nmbss    = ~ sum(nmbss),
      nmens    = ~ sum(nmens),
      nmnns    = ~ sum(nmnns),
      nmdzns   = ~ sum(nmdzns),
      nminnens = ~ sum(nminnens),
      nmbns    = ~ sum(nmbns),
      nmbsns   = ~ sum(nmbsns)
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