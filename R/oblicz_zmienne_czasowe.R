#' oblicza zmienne związane z czasem, który upłynął od momentu uzyskania dyplomu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param utrataEtatu dane wygenerowane za pomocą funkcji \code{\link{oblicz_utrata_etatu}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_czasowe = function(dane, utrataEtatu){
  stopifnot(
    is(dane, 'okienko_df')
  )

  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    left_join(utrataEtatu) %>%
    mutate_(
      roznica = ~ okres - data_zak,
      utrpracy = ~ ifelse(is.na(utrpracy), FALSE, utrpracy)
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      czasmun     = ~ max(0, min(ifelse(mundur > 0, roznica, NA), na.rm = TRUE)),
      czaspraw    = ~ max(0, min(ifelse(prawnik > 0, roznica, NA), na.rm = TRUE)),
      czasprd     = ~ max(0, min(ifelse(etat + netat + samoz > 0, roznica, NA), na.rm = TRUE)),
      czasprsd    = ~ max(0, min(ifelse(samoz > 0, roznica, NA), na.rm = TRUE)),
      czasprud    = ~ max(0, min(ifelse(etat > 0, roznica, NA), na.rm = TRUE)),
      czaszat     = ~ max(0, min(ifelse(etat + netat > 0, roznica, NA), na.rm = TRUE)),
      czasmun_v2  = ~ max(0, min(ifelse(!utrpracy & mundur > 0, roznica, NA), na.rm = TRUE)),
      czaspraw_v2 = ~ max(0, min(ifelse(!utrpracy & prawnik > 0, roznica, NA), na.rm = TRUE)),
      czasprd_v2  = ~ max(0, min(ifelse(!utrpracy & etat + netat + samoz > 0, roznica, NA), na.rm = TRUE)),
      czasprsd_v2 = ~ max(0, min(ifelse(!utrpracy & samoz > 0, roznica, NA), na.rm = TRUE)),
      czasprud_v2 = ~ max(0, min(ifelse(!utrpracy & etat > 0, roznica, NA), na.rm = TRUE)),
      czaszat_v2  = ~ max(0, min(ifelse(!utrpracy & etat + netat > 0, roznica, NA), na.rm = TRUE))
    ) %>%
    mutate_(
      czasmun     = ~ ifelse(is.infinite(czasmun),  NA, czasmun),
      czaspraw    = ~ ifelse(is.infinite(czaspraw), NA, czaspraw),
      czasprd     = ~ ifelse(is.infinite(czasprd),  NA, czasprd),
      czasprsd    = ~ ifelse(is.infinite(czasprsd), NA, czasprsd),
      czasprud    = ~ ifelse(is.infinite(czasprud), NA, czasprud),
      czaszat     = ~ ifelse(is.infinite(czaszat),  NA, czaszat),
      czasmun_v2  = ~ ifelse(is.infinite(czasmun_v2),  NA, czasmun_v2),
      czaspraw_v2 = ~ ifelse(is.infinite(czaspraw_v2), NA, czaspraw_v2),
      czasprd_v2  = ~ ifelse(is.infinite(czasprd_v2),  NA, czasprd_v2),
      czasprsd_v2 = ~ ifelse(is.infinite(czasprsd_v2), NA, czasprsd_v2),
      czasprud_v2 = ~ ifelse(is.infinite(czasprud_v2), NA, czasprud_v2),
      czaszat_v2  = ~ ifelse(is.infinite(czaszat_v2),  NA, czaszat_v2)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}