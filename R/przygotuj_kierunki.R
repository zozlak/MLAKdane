#' Przygotowuje słownik kierunków
#' @export
#' @import dplyr
przygotuj_kierunki = function(){
  dane = openxlsx::readWorkbook('dane/sl_kierunki.xlsx')
  colnames(dane) = tolower(colnames(dane))
  dane = dane %>%
    rename_(
      kierunek_id  = 'studia_kier_id',
      jednostka_id = 'jednostka_prowadzaca_id',
      jednostka    = 'jednostka_prowadzaca'
    ) %>%
    select_('uczelnia_id', 'jednostka_id', 'kierunek_id', 'kierunek')
  return(dane)
}