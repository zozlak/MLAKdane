#' oblicza zmienne generowane z poziomu (zus x zdau) przez poziom (zdau x okres) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_absolwent_okres = function(dane){
  stopifnot(
    is(dane, 'okienko_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau', 'okres') %>%
    summarize_(
      bezrob      = ~ sum(bezrob),
      nbezrob     = ~ sum(etat + netat + samoz + rentemer + student),
      nem         = ~ length(unique(id_platnika[!is.na(id_platnika)])),
      gbezd       = ~ mean(powpbezd_sr, na.rm = TRUE),
      gezbazyd    = ~ mean(powezar_sr[etat + netat + samoz > 0], na.rm = TRUE),
      gezbazyd_v2 = ~ mean(powezar_sr, na.rm = TRUE)
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nmb_v2      = ~ sum(bezrob > 0 & nbezrob == 0),
      nem         = ~ sum(nem, na.rm = TRUE),
      gbezd       = ~ mean(gbezd[is.finite(gbezd)], na.rm = TRUE),
      gezbazyd    = ~ mean(gezbazyd[is.finite(gezbazyd)], na.rm = TRUE),
      gezbazyd_v2 = ~ mean(gezbazyd_v2[is.finite(gezbazyd_v2)], na.rm = TRUE)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}