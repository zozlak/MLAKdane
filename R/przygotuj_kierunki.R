#' Przygotowuje słownik kierunków
#' @description
#' Zwraca słownik kierunków studiów
#' @param katZr katalog, w którym znajduje się plik sl_instytucje.xlsx
#' @param zmDodatkowe nazwy dodatkowych zmiennych do pozostawienia w zwracanym zbiorze danych
#' @return [data.frame] ramka danych opisująca kierunki studiów
#' @export
#' @import dplyr
przygotuj_kierunki = function(katZr, zmDodatkowe = character()){
  dane = openxlsx::readWorkbook(paste0(katZr, '/sl_kierunki.xlsx'))
  colnames(dane) = tolower(colnames(dane))

  slFormy = c('Niestacjonarne' = 'niestac.', 'Stacjonarne' = 'stac.', 'Stacjonarne/Niestacjonarne' = 'stac./niestac.')

  dane = dane %>%
    rename_(
      kierunek_id  = 'studia_kier_id',
      jednostka_id = 'jednostka_prowadzaca_id',
      jednostka    = 'jednostka_prowadzaca'
    ) %>%
    group_by_('kierunek_id') %>%
    summarize_(
      kierunek = ~paste0(unique(kierunek), collapse = '/'),
      forma_ksztalcenia = ~ifelse(length(unique(forma_ksztalcenia)) > 1, 'Stacjonarne/Niestacjonarne', first(forma_ksztalcenia)),
      jednostka_id      = ~ifelse(length(unique(jednostka_id)) > 1, NA, first(jednostka_id)),
      jednostka         = ~ifelse(length(unique(jednostka_id)) > 1, 'studia międzywydziałowe', first(jednostka)),
      uczelnia_id       = ~ifelse(length(unique(uczelnia_id)) > 1, NA, first(uczelnia_id)),
      uczelnia          = ~ifelse(length(unique(uczelnia_id)) > 1, 'studia między uczelniane', first(uczelnia))
    ) %>%
    mutate_(
      kieruneknazwa = ~paste0(jednostka, ', ', kierunek, ', ', slFormy[forma_ksztalcenia], ' (POLon: ', kierunek_id, ')')
    ) %>%
    select_(
      .dots = c('uczelnia_id', 'jednostka_id', 'kierunek_id', 'kieruneknazwa', zmDodatkowe)
    )

  obszary = utils::read.csv2(paste0('dane/sl_kierunki_obszary.csv'), stringsAsFactors = FALSE)
  colnames(obszary) = tolower(colnames(obszary))
  obszary = obszary %>%
    select_('kierunek', 'obsz_kod', 'obsz', 'dzie_kod', 'dzie', 'dysc_kod', 'dysc') %>%
    distinct() %>%
    rename_(
      kierunek_id = 'kierunek'
    ) %>%
    group_by_('kierunek_id') %>%
    summarize_(
      obsz_kod  = ~ first(obsz_kod),
      obsz      = ~ first(obsz),
      dzie_kod  = ~ first(dzie_kod),
      dzie      = ~ first(dzie),
      dysc_kod  = ~ first(dysc_kod),
      dysc      = ~ first(dysc),
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

  dane = dane %>%
    left_join(obszary)
  if (any(is.na(dane$obsz_kod))) {
    warning('brak informacji o obszarach/dziedzinach/dyscyplinach dla ', sum(is.na(dane$obsz_kod)), ' kierunków')
  }

  stopifnot(
    all(!duplicated(dane$kierunek_id))
  )

  return(dane)
}