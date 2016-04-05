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

  obszary = read.csv2('dane/sl_kierunki_obszary.csv', stringsAsFactors = FALSE)
  colnames(obszary) = tolower(colnames(obszary))
  obszary = obszary %>%
    rename_(
      kierunek_id = 'kierunek'
    ) %>%
    group_by_('kierunek_id') %>%
    summarize_(
      obsz_kod      = ~ first(obszar_kod),
      obsz          = ~ first(obszar),
      dziedzin_kod  = ~ first(dziedzina_kod),
      dziedzin      = ~ first(dziedzina),
      dyscyp_kod    = ~ first(dyscyplina_kod),
      dyscyp        = ~ first(dyscyplina),
      n = ~ n()
    ) %>%
    mutate_(
      obsz_kod      = ~ ifelse(n > 1, 99, obsz_kod),
      obsz          = ~ ifelse(n > 1, 'studia międzyobszarowe', obsz),
      dziedzin_kod  = ~ ifelse(n > 1, 99, dziedzin_kod),
      dziedzin      = ~ ifelse(n > 1, 'studia międzydziedzinowe', dziedzin),
      dyscyp_kod    = ~ ifelse(n > 1, 999, dyscyp_kod),
      dyscyp        = ~ ifelse(n > 1, 'studia interdyscyplinarne', dyscyp)
    ) %>%
    select_('-n')

  return(left_join(dane, obszary))
}