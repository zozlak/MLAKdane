#' Przygotowuje słownik kierunków
#' @description
#' Zwraca słownik kierunków studiów
#'
#' Uwaga, kluczem unikalnym zwracanych danych jest para {kierunek_id,
#' jednostka_id}!
#' @param katZr katalog, w którym znajduje się plik sl_instytucje.xlsx
#' @param rok rok, który zostanie przypisany jednostkom
#' @return [data.frame] ramka danych opisująca kierunki studiów
#' @export
#' @import dplyr
przygotuj_kierunki = function(katZr, rok){
  dane = openxlsx::readWorkbook(paste0(katZr, '/sl_kierunki.xlsx'))
  colnames(dane) = tolower(colnames(dane))

  slFormy = c('Niestacjonarne' = 'niestac.', 'Stacjonarne' = 'stac.', 'Stacjonarne/Niestacjonarne' = 'stac./niestac.')
  dataWzor = paste0(rok, '-01-01')

  dane = dane %>%
    rename_(
      kierunek_id  = 'studia_kier_id',
      jednostka_id = 'jednostka_prowadzaca_id',
      jednostka    = 'jednostka_prowadzaca',
      data_od      = 'data_od_obowiazywania',
      n_semestrow  = 'liczba_semestrow'
    ) %>%
    mutate_(
      data_od = ~ifelse(is.na(data_od), dataWzor, data_od),
      data = ~dataWzor
    ) %>%
    filter_(~data_od <= data) %>%
    group_by_('kierunek_id', 'jednostka_id') %>%
    filter_(~data_od == max(data_od))

  nn = nrow(dane)
  dane = dane %>%
    filter_(~row_number() == 1)
  if (nn != nrow(dane)) {
    warning('ze zbioru usunięto ', nn - nrow(dane), ' rekordów o zduplikowanych wartościach pary {kierunek_id, jednostka_id} dla daty ', dataWzor)
  }

  dane = dane %>%
    mutate_(
      kierunek_nazwa = ~paste0(jednostka, ', ', kierunek, ', ', slFormy[forma_ksztalcenia], ' (POLon: ', kierunek_id, ')'),
      n_semestrow    = ~as.integer(n_semestrow)
    ) %>%
    select_(
      .dots = c('uczelnia_id', 'jednostka_id', 'kierunek_id', 'kierunek_nazwa', 'n_semestrow')
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
    warning('brak informacji o obszarach/dziedzinach/dyscyplinach dla ', sum(is.na(dane$obsz_kod)), ' rekordów')
  }

  stopifnot(
    all(!duplicated(dane %>% select_('kierunek_id', 'jednostka_id')))
  )

  dane = ungroup(dane)
  class(dane) = c('kierunki_df', class(dane))
  return(dane)
}