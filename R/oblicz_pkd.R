#' oblicza zmienne ilustrujące zatrudnienie w poszczególnych działach PKD
#' @param okienko obiekt okienka utworzony za pomocą funkcji
#'   \code{\link{okienko}}
#' @param zus dane zwrócone przez funkcję \code{\link{przygotuj_zus}}
#' @return [data.frame] obliczone dane
#' @import dplyr
oblicz_pkd = function(zus) {
  stopifnot(
    methods::is(zus, 'zus_df')
  )

  dzialy = read.csv2('dane/pkd_dzialy.csv', stringsAsFactors = FALSE)

  wynik = zus %>%
    select_('id', 'okres', 'pkd') %>%
    filter_(~!is.na(pkd)) %>%
    distinct_() %>%
    mutate_(numer = ~as.integer(substr(pkd, 1, 2))) %>%
    left_join(dzialy) %>%
    mutate_(
      pkd = ~paste0('if_pkd_', dzial, substr(pkd, 1, 2))
    ) %>%
    select_('-numer', '-dzial') %>%
    distinct_() %>%
    mutate(
      tmp = TRUE
    ) %>%
    tidyr::spread(pkd, tmp)

  class(wynik) = c('pkd_df', class(wynik))
  return(wynik)
}