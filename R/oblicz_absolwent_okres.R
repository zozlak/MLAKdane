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
      nmstudetat     = ~ any(etat == 1) & all(student2 == 1),
      nmstudnetat    = ~ any(netat == 1) & all(etat == 0 & student2 == 1),
      nmstuddziecko  = ~ any(dziecko == 1) & all(student2 == 1),
      nmstudinne     = ~ any(!is.na(id_platnika)) & all(etat == 0 & netat == 0 & dziecko == 0 & bezrob == 0 & student2 == 1),
      nmstudbd       = ~ all(is.na(id_platnika) & student2 == 1),
      nmnstudetat    = ~ any(etat == 1) & all(student2 == 0),
      nmnstudnetat   = ~ any(netat == 1) & all(etat == 0 & student2 == 0),
      nmnstuddziecko = ~ any(dziecko == 1) & all(student2 == 0),
      nmnstudbezrob  = ~ any(bezrob == 1) & all(student2 == 0),
      nmnstudinne    = ~ any(!is.na(id_platnika)) & all(etat == 0 & netat == 0 & dziecko == 0 & bezrob == 0 & student2 == 0),
      nmnstudbd      = ~ all(is.na(id_platnika) & student2 == 0),
      nn = ~ nmstudetat + nmstudnetat + nmstuddziecko + nmstudinne + nmstudbd + nmnstudetat + nmnstudnetat + nmnstuddziecko + nmnstudbezrob + nmnstudinne + nmnstudbd
    ) %>%
    group_by_('id_zdau') %>%
    mutate_(
      nn = ~ as.numeric(ifelse(nn == 0, 1, nn)),
      nmstudetat     = ~ nmstudetat / nn,
      nmstudnetat    = ~ nmstudnetat / nn,
      nmstuddziecko  = ~ nmstuddziecko / nn,
      nmstudinne     = ~ nmstudinne / nn,
      nmstudbd       = ~ nmstudbd / nn,
      nmnstudetat    = ~ nmnstudetat / nn,
      nmnstudnetat   = ~ nmnstudnetat / nn,
      nmnstuddziecko = ~ nmnstuddziecko / nn,
      nmnstudbezrob  = ~ nmnstudbezrob / nn,
      nmnstudinne    = ~ nmnstudinne / nn,
      nmnstudbd      = ~ nmnstudbd / nn
    ) %>%
    summarize_(
      gbezd   = ~ mean(gbezd[is.finite(gbezd)], na.rm = TRUE),
      gezd    = ~ mean(gezd[is.finite(gezd)], na.rm = TRUE),
      nem     = ~ sum(nem, na.rm = TRUE),
      nmb_v2  = ~ sum(bezrob > 0 & nbezrob == 0),
      zpow    = ~ mean(zpow[is.finite(zpow)], na.rm = TRUE),
      nmstudetat     = ~ sum(nmstudetat),
      nmstudnetat    = ~ sum(nmstudnetat),
      nmstuddziecko  = ~ sum(nmstuddziecko),
      nmstudinne     = ~ sum(nmstudinne),
      nmstudbd       = ~ sum(nmstudbd),
      nmnstudetat    = ~ sum(nmnstudetat),
      nmnstudnetat   = ~ sum(nmnstudnetat),
      nmnstuddziecko = ~ sum(nmnstuddziecko),
      nmnstudbezrob  = ~ sum(nmnstudbezrob),
      nmnstudinne    = ~ sum(nmnstudinne),
      nmnstudbd      = ~ sum(nmnstudbd)
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