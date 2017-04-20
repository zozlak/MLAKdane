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
      kieruneknazwa = ~paste0(jednostka, ', ', kierunek, ', ', slFormy[forma_ksztalcenia], ' (POLon: ', kierunek_id, ')')
    ) %>%
    select_(
      .dots = c('uczelnia_id', 'jednostka_id', 'kierunek_id', 'kieruneknazwa', 'obsz_kod', 'obsz', 'dzie_kod', 'dzie', 'dysc_kod', 'dysc', zmDodatkowe)
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
      obsz_kod2  = ~ first(obsz_kod),
      obsz2      = ~ first(obsz),
      dzie_kod2  = ~ first(dzie_kod),
      dzie2      = ~ first(dzie),
      dysc_kod2  = ~ first(dysc_kod),
      dysc2      = ~ first(dysc),
      n = ~ n()
    ) %>%
    mutate_(
      obsz_kod2  = ~ ifelse(n > 1, 99, obsz_kod2),
      obsz2      = ~ ifelse(n > 1, 'studia międzyobszarowe', obsz2),
      dzie_kod2  = ~ ifelse(n > 1, 99, dzie_kod2),
      dzie2      = ~ ifelse(n > 1, 'studia międzydziedzinowe', dzie2),
      dysc_kod2  = ~ ifelse(n > 1, 999, dysc_kod2),
      dysc2      = ~ ifelse(n > 1, 'studia interdyscyplinarne', dysc2)
    ) %>%
    select_('-n')
  dane = dane %>%
    left_join(obszary) %>%
    mutate_(
      obsz_kod  = ~ dplyr::coalesce(obsz_kod, obsz_kod2),
      obsz      = ~ dplyr::coalesce(obsz, obsz2),
      dzie_kod  = ~ dplyr::coalesce(dzie_kod, dzie_kod2),
      dzie      = ~ dplyr::coalesce(dzie, dzie2),
      dysc_kod  = ~ dplyr::coalesce(dysc_kod, dysc_kod2),
      dysc      = ~ dplyr::coalesce(dysc, dysc2)
    ) %>%
    select_('-obsz_kod2', '-obsz2', '-dzie_kod2', '-dzie2', '-dysc_kod2', '-dysc2')

  if (any(is.na(dane$obsz_kod))) {
    warning('brak informacji o obszarach/dziedzinach/dyscyplinach dla ', sum(is.na(dane$obsz_kod)), ' kierunków')
  }

  stopifnot(
    all(!duplicated(dane$kierunek_id))
  )

  return(dane)
}