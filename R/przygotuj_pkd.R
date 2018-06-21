#' Przygotowuje różne klasyfikacje wywodzone z PKD
#' @export
#' @import dplyr
przygotuj_pkd = function() {
  pkd = openxlsx::readWorkbook('dane/pkd.xlsx') %>%
    select_('pkd', 'pkd_klasa', 'pkd_klasa_edu') %>%
    mutate_(pkd2 = ~substr(pkd, 1, 2))
  return(pkd)
}