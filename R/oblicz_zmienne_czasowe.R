#' oblicza zmienne związane z czasem, który upłynął od momentu uzyskania dyplomu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param utrataEtatu dane wygenerowane za pomocą funkcji \code{\link{oblicz_utrata_etatu}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_czasowe = function(dane, utrataEtatu, multidplyr = TRUE){
  stopifnot(
    is(dane, 'baza_df')
  )

  dane = dane %>%
    filter_(~ okres >= data_zak) %>%
    left_join(utrataEtatu) %>%
    mutate_(
      roznica   = ~ okres - data_zak,
      nutrpracy = ~ ifelse(is.na(utrpracy) | okres != data_zak, TRUE, !utrpracy)
    )
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }else{
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      czasmun_v2  = ~ min(ifelse(nutrpracy & mundur > 0, roznica, NA_integer_), na.rm = TRUE),
      czaspraw_v2 = ~ min(ifelse(nutrpracy & prawnik > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprd_v2  = ~ min(ifelse(nutrpracy & etat + netat + samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprsd_v2 = ~ min(ifelse(nutrpracy & samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprud_v2 = ~ min(ifelse(nutrpracy & etat > 0, roznica, NA_integer_), na.rm = TRUE),
      czaszat_v2  = ~ min(ifelse(nutrpracy & etat + netat > 0, roznica, NA_integer_), na.rm = TRUE)
    ) %>%
    mutate_(
      czasmun_v2  = ~ ifelse(is.infinite(czasmun_v2),  NA_integer_, czasmun_v2),
      czaspraw_v2 = ~ ifelse(is.infinite(czaspraw_v2), NA_integer_, czaspraw_v2),
      czasprd_v2  = ~ ifelse(is.infinite(czasprd_v2),  NA_integer_, czasprd_v2),
      czasprsd_v2 = ~ ifelse(is.infinite(czasprsd_v2), NA_integer_, czasprsd_v2),
      czasprud_v2 = ~ ifelse(is.infinite(czasprud_v2), NA_integer_, czasprud_v2),
      czaszat_v2  = ~ ifelse(is.infinite(czaszat_v2),  NA_integer_, czaszat_v2),
      czasmun     = ~ max(0, czasmun_v2),
      czaspraw    = ~ max(0, czaspraw_v2),
      czasprd     = ~ max(0, czasprd_v2),
      czasprsd    = ~ max(0, czasprsd_v2),
      czasprud    = ~ max(0, czasprud_v2),
      czaszat     = ~ max(0, czaszat_v2)
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}