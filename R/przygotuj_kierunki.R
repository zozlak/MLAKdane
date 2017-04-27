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
      uczelnia          = ~ifelse(length(unique(uczelnia_id)) > 1, 'studia między uczelniane', first(uczelnia)),
      obsz_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(wiodaca_obszar_kod)))),
      obsz              = ~first(wiodaca_obszar),
      dzie_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(wiodaca_dziedz_kod)))),
      dzie              = ~first(wiodaca_dziedz),
      dysc_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(wiodaca_dysc_kod)))),
      dysc              = ~first(wiodaca_dysc)
    ) %>%
    mutate_(
      dzie_kod = ~as.integer(sub('^([0-9])0([0-9])$', '\\1\\2', dzie_kod)),
      dysc_kod = ~as.integer(sub('^([0-9])0([0-9]{3})$', '\\1\\2', dysc_kod)),
      kieruneknazwa = ~paste0(jednostka, ', ', kierunek, ', ', slFormy[forma_ksztalcenia], ' (POLon: ', kierunek_id, ')')
    ) %>%
    select_(
      .dots = c('uczelnia_id', 'jednostka_id', 'kierunek_id', 'kieruneknazwa', 'obsz_kod', 'obsz', 'dzie_kod', 'dzie', 'dysc_kod', 'dysc', zmDodatkowe)
    )

  if (any(is.na(dane$obsz_kod))) {
    warning('brak informacji o obszarach/dziedzinach/dyscyplinach dla ', sum(is.na(dane$obsz_kod)), ' kierunków')
  }

  stopifnot(
    all(!duplicated(dane$kierunek_id))
  )

  return(dane)
}