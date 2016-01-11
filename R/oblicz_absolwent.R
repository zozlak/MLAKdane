#' oblicza zmienne generowane w jednym kroku z poziomu (zus x zdau) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_absolwent = function(dane){
  stopifnot(
    is(dane, 'okienko_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      sz    = ~ sum(podst, na.rm = TRUE),
      sze   = ~ sum(podst[etat == 1], na.rm = TRUE),
      nde   = ~ length(unique(id_platnika[etat %in% 1])),
      ndn   = ~ length(unique(id_platnika[netat %in% 1])),
      nmb   = ~ length(unique(okres[bezrob == 1])),
      nme   = ~ length(unique(okres[etat == 1])),
      nmn   = ~ length(unique(okres[netat == 1])),
      nms   = ~ length(unique(okres[samoz == 1])),
      nmp   = ~ length(unique(okres[etat == 1 | netat == 1 | samoz == 1])),
      nmj   = ~ length(unique(okres[prawnik == 1])),
      nmm   = ~ length(unique(okres[mundur == 1])),
      nzus  = ~ length(unique(okres[!is.na(id_platnika)]))
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}