#' przygotowuje dane o PNA i powiatach
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2015-09-30')
#' @return data.frame wyliczone dane
#' @export
#' @import dplyr
przygotuj_pna_powiaty = function(dataMin, dataMax){
  typy = readr::cols(pna5 = 'c', pna = 'i', teryt = 'i', id_gus = 'c', powiat = 'c', wojewodztwo = 'c', miejzam = 'i', 'klaszam' = 'i', .default = 'd')
  dane = readr::read_csv2('dane/pna_powiaty.csv', col_types = typy) %>%
    mutate_(
      teryt = ~teryt * 100,
      pna5 = ~sub('#', '-', pna5)
    )
  stopifnot(
    dane %>% filter_(~teryt > 0 & teryt < 20000) %>% nrow() == 0, # pna właściwe
    dane %>% filter_(~teryt %% 10000 == 0 & (teryt / 10000) %% 2 == 0 & teryt > 0) %>% select_('teryt') %>% distinct() %>% nrow() == 16, # województwa
    dane %>% filter_(~teryt == 0) %>% nrow() == 1 # Polska
  )
  wszystkiePna = expand.grid(
    rok = floor(data2okres(dataMin) / 12):floor(data2okres(dataMax) / 12),
    pna5 = unique(stats::na.exclude(dane$pna5)),
    stringsAsFactors = FALSE
  )

  powGrodzkie = c(261, 262, 264, 265, 461, 462, 463, 464, 661, 662, 663, 664, 861, 862, 1061, 1062, 1063, 1261, 1262, 1263, 1461, 1462, 1463, 1464, 1465, 1661, 1861, 1862, 1863, 1864, 2061, 2062, 2063, 2261, 2262, 2263, 2264, 2461, 2462, 2463, 2464, 2465, 2466, 2467, 2468, 2469, 2470, 2471, 2472, 2473, 2474, 2475, 2476, 2477, 2478, 2479, 2661, 2861, 2862, 3061, 3062, 3063, 3064, 3261, 3262, 3263)
  pna = dane %>%
    filter_(~pna >= 0) %>%
    mutate_(
      pow_grodzki = ~as.numeric(teryt %in% (100 * powGrodzkie))
    ) %>%
    select_('pna', 'powiat', 'wojewodztwo', 'teryt', 'pow_grodzki', 'miejzam', 'klaszam', 'pna5') %>%
    full_join(wszystkiePna)
  class(pna) = c('pna_df', class(pna))
  stopifnot(pna %>% filter_(~is.na(pna) | is.na(pna5)) %>% nrow == 0)

  dane = tidyr::gather(dane, 'stat', 'wartosc', dplyr::starts_with('bezr'), dplyr::starts_with('zar')) %>%
    mutate_(
      rok     = ~2000L + as.integer(sub('^.*([0-9][0-9])$', '\\1', stat)),
      miesiac = ~dplyr::coalesce(suppressWarnings(as.integer(sub('^[^0-9]*([0-9]?[0-9])[0-9][0-9]$', '\\1', stat))), 6L),
      stat    = ~dplyr::if_else(grepl('^bezr', stat), 'powpbezd', 'powezar')
    ) %>%
    mutate_(
      okres = ~data2okres(paste0(rok, '-', miesiac))
    ) %>%
    select_('-miesiac') %>%
    filter_(~okres <= data2okres(dataMax))

  # wartości unikalne dla powiatów/województw/Polski
  stopifnot(
    dane %>%
      group_by_('stat', 'okres', 'teryt') %>%
      filter_(~n_distinct(wartosc) > 1) %>%
      nrow() == 0
  )

  powezar = dane %>%
    filter_(~ stat == 'powezar') %>%
    select_('rok', 'teryt', 'wartosc') %>%
    rename_(powezar = 'wartosc') %>%
    distinct() %>%
    arrange_('teryt', 'rok') %>%
    group_by_('teryt') %>%
    mutate_(
      powezar_p1 = ~(lead(powezar) - powezar) / (lead(rok) - rok) / 12,
      powezar_m1 = ~(powezar - lag(powezar)) / (rok - lag(rok)) / 12
    ) %>%
    mutate_(
      powezar_p1 = ~coalesce(powezar_p1, powezar_m1),
      powezar_m1 = ~coalesce(powezar_m1, powezar_p1)
    ) %>%
    ungroup()

  powpbezd = dane %>%
    filter_(~ stat == 'powpbezd') %>%
    select_('okres', 'rok', 'teryt', 'wartosc') %>%
    rename_(powpbezd = 'wartosc') %>%
    distinct()

  powiaty = full_join(powezar, powpbezd) %>%
    filter_(~!is.na(okres)) %>%
    mutate_(
      miesiac = ~okres2miesiac(okres),
      powezar = ~powezar - pmax(0, 6.5 - miesiac) * powezar_m1 + pmax(0, miesiac - 6.5) * powezar_p1
    ) %>%
    select_('okres', 'powpbezd', 'powezar', 'teryt', 'rok')

  class(powiaty) = c('powiaty_df', class(powiaty))

  return(polacz_pna_powiaty(pna, powiaty, dataMin, dataMax))
}