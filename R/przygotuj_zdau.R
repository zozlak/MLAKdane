#' Przygotowuje plik ZDAU
#' @description ZDAU to plik z uczelni, w którym jednostką jest student na
#' kierunku studiów.
#' @param katZr katalog, w którym znajduje się plik ZDAU.csv
#' @param probka jaki odsetek rekordów ze zbioru zachować (w wypadku wartości
#'   mniejszych od 1 zadany odsetek wybierany jest losowo)
#' @export
#' @import dplyr
przygotuj_zdau = function(katZr, probka = 1){
  stopifnot(
    is.vector(probka), is.numeric(probka), length(probka) == 1, all(!is.na(probka)), all(probka > 0), all(probka <= 1)
  )

  zdau = utils::read.csv2(paste0(katZr, '/ZDAU.csv'), header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)
  colnames(zdau) = tolower(c('ID', 'CZY_PESEL', 'TYP', 'CUDZOZIEMIEC', 'LICZBA_REKORDÓW', 'UCZELNIA_ID', 'JEDNOSTKA_ID', 'KIERUNEK_ID', 'ISM_ID', 'FORMA', 'POZIOM', 'PROFIL', 'ROK_OD', 'MIES_OD', 'ROK_DO', 'MIES_DO'))
  zdau = zdau %>%
    mutate_(
      id      = ~as.integer(id),
      id_zdau = ~row_number(),
      data_od = ~paste(rok_od, mies_od, '01', sep = '-'),
      data_do = ~paste(rok_do, mies_do, '01', sep = '-')
    ) %>%
    mutate_(
      data_od  = ~data2okres(ifelse(grepl('NA', data_od), NA_character_, data_od)),
      data_do  = ~data2okres(ifelse(grepl('NA', data_do), NA_character_, data_do)),
      data_skr = ~ifelse(poziom %in% 3 & typ %in% 'S', data_do, NA_integer_)
    ) %>%
    select_('-rok_od', '-mies_od', '-rok_do', '-mies_do', '-liczba_rekordów')

  if (probka < 1) {
    zdau = zdau %>%
      inner_join(
        zdau %>%
          select_('id') %>%
          distinct() %>%
          sample_n(probka)
      )
  }

  class(zdau) = c('zdau_df', class(zdau))
  return(zdau)
}