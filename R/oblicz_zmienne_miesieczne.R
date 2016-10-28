#' oblicza zmienne generowane z poziomu (zus x zdau) przez poziom (zdau x okres) na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_miesieczne = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df'),
    is(zdau, 'zdau_df')
  )
  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)

  mies = dane
  if(multidplyr){
    mies = multidplyr::partition(mies, id_zdau)
  }
  mies = mies %>%
    group_by_('id_zdau', 'okres', 'id') %>%
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
      zilo  = ~ (sze + szn) / (nmz * zpow)
    ) %>%
    select_('id_zdau', 'id', 'okres', 'bilod', 'zilo') %>%
    collect() %>%
    ungroup()

  stud = dane %>%
    select_('id_zdau', 'okres', 'id', 'data_rozp', 'data_zak') %>%
    filter_(~ okres >= data_rozp & (okres <= data_zak | is.na(data_zak))) %>%
    select_('id_zdau', 'id', 'okres')

  mies = mies %>%
    left_join(
      stud %>%
        select_('id_zdau', 'okres') %>%
        mutate_(studprog = 1)
    ) %>%
    left_join(
      stud %>%
        select_('id', 'okres') %>%
        distinct() %>%
        mutate_(stud = 1)
    ) %>%
    mutate_(
      okres    = ~ okres2data(okres),
      stud     = ~ ifelse(is.na(stud), 0, stud),
      studprog = ~ ifelse(is.na(studprog), 0, studprog)
    ) %>%
    select_('-id')

  class(mies) = c('miesieczne_df', class(mies))
  names(mies) = paste0(names(mies), '_m')
  return(mies)
}
