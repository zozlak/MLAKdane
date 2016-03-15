#' przygotowuje dane o PNA i powiatach na podstawie zbioru danych od Marka
#' Bożykowskiego
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2015-09-30')
#' @return data.frame wyliczone dane
#' @export
#' @import dplyr
przygotuj_pna_powiaty_mb = function(dataMin, dataMax){
  dane = read.csv2('dane/pna_powiaty_mb.csv', stringsAsFactors = FALSE) %>%
    mutate_(
      teryt = ~ teryt * 100,
      pna5 = ~ sub('#', '-', pna5)
    )

  powGrodzkie = c(261, 262, 264, 265, 461, 462, 463, 464, 661, 662, 663, 664, 861, 862, 1061, 1062, 1063, 1261, 1262, 1263, 1461, 1462, 1463, 1464, 1465, 1661, 1861, 1862, 1863, 1864, 2061, 2062, 2063, 2261, 2262, 2263, 2264, 2461, 2462, 2463, 2464, 2465, 2466, 2467, 2468, 2469, 2470, 2471, 2472, 2473, 2474, 2475, 2476, 2477, 2478, 2479, 2661, 2861, 2862, 3061, 3062, 3063, 3064, 3261, 3262, 3263)
  pna = dane %>%
    mutate_(
      pow_grodzki = ~ as.numeric(teryt %in% (100 * powGrodzkie))
    ) %>%
    select_('pna', 'powiat', 'wojewodztwo', 'teryt', 'pow_grodzki', 'miejzam', 'klasazam', 'pna5') %>%
    full_join(expand.grid(rok = floor(data2okres(dataMin) / 12):floor(data2okres(dataMax) / 12), pna5 = dane$pna5, stringsAsFactors = FALSE)) %>%
    bind_rows(expand.grid(
      pna = -1, powiat = 'Polska', wojewodztwo = 'Polska', teryt = 0, rok = floor(data2okres(dataMin) / 12):floor(data2okres(dataMax) / 12),
      stringsAsFactors = FALSE
    ))
  class(pna) = c('pna_df', class(pna))

  dane = reshape2::melt(dane, id.vars = c('pna5', 'pna', 'teryt', 'id_gus', 'powiat', 'wojewodztwo', 'miejzam', 'klasazam'))
  dane$stat = ifelse(grepl('^bezr', dane$variable), 'powpbezd', 'powezar')
  dane$okres = as.numeric(gsub('[^0-9]', '', dane$variable))
  dane$rok = 2000 + (dane$okres %% 100)
  dane$okres = data2okres(paste0(2000 + (dane$okres %% 100), '-', floor(dane$okres / 100)))
  dane = dane %>%
    filter_(okres <= data2okres('2015-10'))

  # wartości unikalne dla powiatów
  stopifnot(
    dane %>%
      group_by_('stat', 'okres', 'teryt') %>%
      summarize_(n = ~ n_distinct(value)) %>%
      ungroup() %>%
      filter_(~ n > 1) %>%
      nrow() == 0
  )

  powezar = dane %>%
    filter_(~ stat == 'powezar') %>%
    select_('rok', 'value', 'teryt') %>%
    rename_(powezar = 'value') %>%
    distinct()

  powpbezd = dane %>%
    filter_(~ stat == 'powpbezd') %>%
    select_('okres', 'rok', 'value', 'teryt') %>%
    rename_(powpbezd = 'value') %>%
    distinct()

  powiaty = full_join(powezar, powpbezd) %>%
    select_('okres', 'powpbezd', 'powezar', 'teryt', 'rok')
  powiaty = suppressWarnings(przygotuj_powiaty()) %>%
    filter_(~ teryt %% 10000 == 0 & teryt > 0) %>%
    bind_rows(powiaty)
  class(powiaty) = c('powiaty_df', class(powiaty))

  return(polacz_pna_powiaty(pna, powiaty, dataMin, dataMax))
}