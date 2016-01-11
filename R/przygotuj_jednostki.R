#' Przygotowuje słownik jednostek
#' @param rok rok, który zostanie przypisany jednostkom (gdy NA, obecny rok)
#' @export
#' @import dplyr
przygotuj_jednostki = function(rok = NA){
  sl_jednostki = openxlsx::readWorkbook('dane/sl_instytucje.xlsx')
  colnames(sl_jednostki) = tolower(colnames(sl_jednostki))

  sl_jednostki = sl_jednostki %>%
    rename_(teryt = 'kod_lokalizacji') %>%
    mutate_(teryt = ~ floor(as.numeric(teryt) / 10))

  nazwy2teryt = read.csv2('dane/nazwy2teryt.csv', stringsAsFactors = FALSE)
  braki = sl_jednostki %>%
    filter_(~ is.na(teryt)) %>%
    select_('jednostka_id', 'wojewodztwo', 'powiat', 'gmina') %>%
    mutate_(
      wojewodztwo = ~ tolower(wojewodztwo),
      powiat = ~ tolower(powiat),
      gmina = ~ tolower(gmina),
      rok = ~ as.numeric(ifelse(is.na(rok), substring(Sys.Date(), 1, 4), rok))
    ) %>%
    left_join(nazwy2teryt) %>%
    select_('jednostka_id', 'teryt') %>%
    rename_(teryt2 = 'teryt')
  sl_jednostki = sl_jednostki %>%
    left_join(braki) %>%
    mutate_(teryt = ~ ifelse(is.na(teryt), teryt2, teryt)) %>%
    select_('-teryt2')

  return(sl_jednostki)
}