#' oblicza zmienne generowane w jednym kroku z poziomu (zus x zdau) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_absolwent = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }else{
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      nde = ~ length(unique(id_platnika[etat == 1])),
      ndn = ~ length(unique(id_platnika[netat == 1])),
      nmb = ~ length(unique(okres[bezrob == 1])),
      nme = ~ length(unique(okres[etat == 1])),
      nmj = ~ length(unique(okres[prawnik == 1])),
      nmm = ~ length(unique(okres[mundur == 1])),
      nmn = ~ length(unique(okres[netat == 1])),
      nmp = ~ length(unique(okres[etat == 1 | netat == 1 | samoz == 1])),
      nms = ~ length(unique(okres[samoz == 1])),
      nmz = ~ length(unique(okres[etat == 1 | netat == 1])),
      sz  = ~ sum(podst, na.rm = TRUE),
      sze = ~ sum(podst[etat == 1], na.rm = TRUE),
      szn = ~ sum(podst[netat == 1], na.rm = TRUE)
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}