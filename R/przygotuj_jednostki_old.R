#' Przygotowuje słownik uczelni
#' @description
#' Zwraca słownik uczelni
#' @param katZr katalog, w którym znajduje się plik sl_instytucje.xlsx
#' @param rok rok, który zostanie przypisany jednostkom (gdy NA, obecny rok)
#' @return [data.frame] ramka danych opisująca uczelnie
#' @export
#' @import dplyr
przygotuj_jednostki_old = function(katZr, rok){
  sl_jednostki = openxlsx::readWorkbook(paste0(katZr, '/sl_instytucje.xlsx'))
  colnames(sl_jednostki) = tolower(colnames(sl_jednostki))

  sl_jednostki = sl_jednostki %>%
    rename_(
      teryt     = 'kod_lokalizacji',
      jednostka = 'jednostka_podstawowa',
      typ_jednostki = 'typ'
    ) %>%
    mutate_(teryt = ~ floor(as.numeric(teryt) / 10))

  nazwy2teryt = utils::read.csv2('dane/nazwy2teryt.csv', stringsAsFactors = FALSE) %>%
    mutate_(
      wojewodztwo = ~enc2utf8(tolower(wojewodztwo)),
      powiat = ~enc2utf8(tolower(powiat)),
      gmina = ~enc2utf8(tolower(gmina)),
      teryt = ~100 * floor(teryt / 100)
    ) %>%
    distinct()
  braki = sl_jednostki %>%
    filter_(~ is.na(teryt)) %>%
    select_('jednostka_id', 'wojewodztwo', 'powiat', 'gmina') %>%
    mutate_(
      wojewodztwo = ~ enc2utf8(tolower(wojewodztwo)),
      powiat = ~ enc2utf8(tolower(powiat)),
      gmina = ~ enc2utf8(tolower(gmina)),
      rok = ~ as.numeric(ifelse(is.na(rok), substring(Sys.Date(), 1, 4), rok))
    ) %>%
    left_join(nazwy2teryt) %>%
    select_('jednostka_id', 'teryt') %>%
    rename_(teryt2 = 'teryt')
  if (any(is.na(braki$teryt2))) {
    warning(sum(is.na(braki$teryt2)), ' jednostki bez kodu TERYT')
  }
  sl_jednostki = sl_jednostki %>%
    left_join(braki) %>%
    mutate_(teryt = ~ ifelse(is.na(teryt), teryt2, teryt)) %>%
    select_('-teryt2')

  stopifnot(
    all(!duplicated(sl_jednostki$jednostka_id))
  )

  sl_jednostki = sl_jednostki %>%
    mutate(data_od = NA_character_, zmiana_nazwy_jednostki = NA_character_, nazwa_pelna = paste(uczelnianazwa, jednostka)) %>%
    select(jednostka_id, data_od, zmiana_nazwy_jednostki, teryt, kod_pocztowy, miejscowosc, poczta, ulica, nr_domu, nr_lokalu, kod, nazwa_pelna)

  class(sl_jednostki) = c('jednostki_df', class(sl_jednostki))
  return(sl_jednostki)
}