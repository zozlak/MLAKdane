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
    select_('kierunek', 'obszar_kod', 'obszar', 'dziedzina_kod', 'dziedzina', 'dyscyplina_kod', 'dyscyplina') %>%
    distinct() %>%
    rename_(
      kierunek_id = 'kierunek'
    ) %>%
    group_by_('kierunek_id') %>%
    summarize_(
      obsz_kod  = ~ first(obszar_kod),
      obsz      = ~ first(obszar),
      dzie_kod  = ~ first(dziedzina_kod),
      dzie      = ~ first(dziedzina),
      dysc_kod  = ~ first(dyscyplina_kod),
      dysc      = ~ first(dyscyplina),
      n = ~ n()
    ) %>%
    mutate_(
      obsz_kod  = ~ ifelse(n > 1, 99, obsz_kod),
      obsz      = ~ ifelse(n > 1, 'studia międzyobszarowe', obsz),
      dzie_kod  = ~ ifelse(n > 1, 99, dzie_kod),
      dzie      = ~ ifelse(n > 1, 'studia międzydziedzinowe', dzie),
      dysc_kod  = ~ ifelse(n > 1, 999, dysc_kod),
      dysc      = ~ ifelse(n > 1, 'studia interdyscyplinarne', dysc)
    ) %>%
    select_('-n')

  return(left_join(dane, obszary))
}