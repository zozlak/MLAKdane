#' oblicza stałe czasowe i konwertuje istniejące stałe czasowe z powrotem na daty
#' @param dane ramka danych ZDAU zwrócona przez funkcję \code{\link{przygotuj_zdau}}
#' @param data_badania data konca badanego okresu
#' @return data.frame
#' @export
#' @import dplyr
oblicz_stale_czasowe = function(dane, data_badania){
  stopifnot(
    methods::is(dane, 'zdau_df')
  )
  dane = dane %>%
    mutate_(
      nokr     = ~ data2okres(data_badania) - data_do,
      mcstart  = ~ okres2miesiac(data_od),
      rokstart = ~ okres2rok(data_od),
      mcdyp    = ~ okres2miesiac(data_do),
      rokdyp   = ~ okres2rok(data_do),
      data_od  = ~ okres2data(data_od),
      data_do  = ~ okres2data(data_do)
    )
  return(dane)
}