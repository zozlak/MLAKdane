#' oblicza stałe czasowe i konwertuje istniejące stałe czasowe z powrotem na daty
#' @param dane ramka danych ZDAU (lub pochodna)
#' @return data.frame
#' @export
#' @import dplyr
oblicz_stale_czasowe = function(dane){
  dane = dane %>%
    mutate_(
      mcstart   = ~ okres2miesiac(data_rozp),
      rokstart  = ~ okres2rok(data_rozp),
      mcdyp     = ~ okres2miesiac(data_zak),
      rokdyp    = ~ okres2rok(data_zak),
      data_rozp = ~ okres2data(data_rozp),
      data_zak  = ~ okres2data(data_zak)
    )
  return(dane)
}