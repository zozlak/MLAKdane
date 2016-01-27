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

  # liczone do dupy, to trzeba liczyć w podziale na pracodawców, bo tylko tam ma sens utratapracy!

  dane = dane %>%
    filter_(~ okres >= data_zak) %>%
    left_join(utrataEtatu) %>%
    mutate_(
      roznica   = ~ okres - data_zak
    )
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }else{
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      czasmun_v2  = ~ min(ifelse((roznica == 0 & is.na(utrmundur)  | roznica > 0) & mundur > 0, roznica, NA_integer_), na.rm = TRUE),
      czaspraw_v2 = ~ min(ifelse((roznica == 0 & is.na(utrprawnik) | roznica > 0) & prawnik > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprd_v2  = ~ min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & etat + netat + samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprsd_v2 = ~ min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      czasprud_v2 = ~ min(ifelse((roznica == 0 & is.na(utretatu)   | roznica > 0) & etat > 0, roznica, NA_integer_), na.rm = TRUE),
      czaszat_v2  = ~ min(ifelse((roznica == 0 & is.na(utrzatr)    | roznica > 0) & etat + netat > 0, roznica, NA_integer_), na.rm = TRUE)
    ) %>%
    mutate_(
      czasmun_v2  = ~ ifelse(is.infinite(czasmun_v2),  NA_integer_, czasmun_v2),
      czaspraw_v2 = ~ ifelse(is.infinite(czaspraw_v2), NA_integer_, czaspraw_v2),
      czasprd_v2  = ~ ifelse(is.infinite(czasprd_v2),  NA_integer_, czasprd_v2),
      czasprsd_v2 = ~ ifelse(is.infinite(czasprsd_v2), NA_integer_, czasprsd_v2),
      czasprud_v2 = ~ ifelse(is.infinite(czasprud_v2), NA_integer_, czasprud_v2),
      czaszat_v2  = ~ ifelse(is.infinite(czaszat_v2),  NA_integer_, czaszat_v2),
      czasmun     = ~ ifelse(czasmun_v2  > 0, czasmun_v2  - 1, 0), # działa poprawnie także dla czas*_v2 równego NA
      czaspraw    = ~ ifelse(czaspraw_v2 > 0, czaspraw_v2 - 1, 0),
      czasprd     = ~ ifelse(czasprd_v2  > 0, czasprd_v2  - 1, 0),
      czasprsd    = ~ ifelse(czasprsd_v2 > 0, czasprsd_v2 - 1, 0),
      czasprud    = ~ ifelse(czasprud_v2 > 0, czasprud_v2 - 1, 0),
      czaszat     = ~ ifelse(czaszat_v2  > 0, czaszat_v2  - 1, 0)
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}