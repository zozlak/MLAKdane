#' Przygotowuje plik ZDAU
#' @description
#' ZDAU to plik z uczelni, w którym jednostką jest student na kierunku studiów
przygotuj_zdau = function(){
  zdau = read.csv2('dane/ZDAU.csv', header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)
  colnames(zdau) = tolower(c('ID', 'CZY_PESEL', 'TYP', 'CUDZOZIEMIEC', 'LICZBA_REKORDÓW', 'UCZELNIA_ID', 'JEDNOSTKA_ID', 'KIERUNEK_ID', 'ISM_ID', 'FORMA', 'POZIOM', 'PROFIL', 'ROK_OD', 'MIES_OD', 'ROK_DO', 'MIES_DO'))
  zdau = zdau %>%
    mutate_(
      id_zdau   = ~ row_number(),
      data_rozp = ~ paste(rok_od, mies_od, '01', sep = '-'),
      data_zak  = ~ paste(rok_do, mies_do, '01', sep = '-')
    ) %>%
    mutate_(
      data_rozp = ~ data2okres(ifelse(grepl('NA', data_rozp), NA, data_rozp)),
      data_zak  = ~ data2okres(ifelse(grepl('NA', data_zak), NA, data_zak))
    ) %>%
    select_('-rok_od', '-mies_od', '-rok_do', '-mies_do')

  class(zdau) = c('zdau_df', class(zdau))
  return(zdau)
}