#' przygotowuje dane opisujące PNA
#' @export
#' @import dplyr
przygotuj_pna = function(){
  # zbiór zebrany z bazy ZPD:
  # SELECT rok, id_wojewodztwa * 100 + id_gminy AS teryt, p.nazwa AS powiat, w.nazwa AS wojewodztwo, id_wojewodztwa * 100 + id_gminy IN (261, 262, 264, 265, 461, 462, 463, 464, 661, 662, 663, 664, 861, 862, 1061, 1062, 1063, 1261, 1262, 1263, 1461, 1462, 1463, 1464, 1465, 1661, 1861, 1862, 1863, 1864, 2061, 2062, 2063, 2261, 2262, 2263, 2264, 2461, 2462, 2463, 2464, 2465, 2466, 2467, 2468, 2469, 2470, 2471, 2472, 2473, 2474, 2475, 2476, 2477, 2478, 2479, 2661, 2861, 2862, 3061, 3062, 3063, 3064, 3261, 3262, 3263) AS pow_grodzki
  # FROM teryt_powiaty p JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
  powiaty = utils::read.csv2('dane/powiaty.csv', stringsAsFactors = FALSE)
  powiaty = powiaty %>%
    mutate_(
      powiat = ~tolower(powiat),
      wojewodztwo = ~tolower(wojewodztwo)
    )
  # placówki poczty ze spisu PLACÓWEK poczty
  pna = utils::read.csv2('dane/pna_placówki.csv', stringsAsFactors = FALSE)
  colnames(pna) = tolower(colnames(pna))
  pna = pna %>%
    mutate_(
      powiat = ~tolower(powiat),
      wojewodztwo = ~tolower(wojewodztwo)
    )
  pna = pna %>%
    left_join(powiaty) %>%
    select_('-gmina') %>%
    distinct() %>%
    group_by_('rok', 'pna') %>%
    filter_(~ pow_grodzki == max(pow_grodzki)) %>%
    mutate_(n = ~ n()) %>%
    ungroup()
  niejdnozn = c('68-100' = 810, '73-115' = 3214, '26-020' = 2604, '32-015' = 1219, '34-424' = 1211, '55-220' = 215, '62-045' = 3024, '67-410' = 812, '76-020' = 3209)
  pna = pna %>%
    mutate_(popr_teryt = ~ ifelse(n == 1, teryt, niejdnozn[pna])) %>%
    filter_(~ teryt == popr_teryt) %>%
    select_('-popr_teryt', '-n')
  # powiel na poprzednie lata - jedyna zmiana to Wałbrzych w 2012
  for(r in 2013:2009){
    tmp = pna %>%
      filter_(~ rok == r + 1) %>%
      mutate_(rok = ~ r)
    if(r == 2012){
      tmp = tmp %>%
        mutate_(teryt = ~ ifelse(teryt == 265, 221, teryt))
    }
    pna = bind_rows(pna, tmp)
  }
  powiatyNajw = c(1465, 1261, 1061, 264, 3064) # Warszawa, Kraków, Łódź, Wrocław, Poznań
  powiatyAkadem = append(powiatyNajw, c(2061, 461, 2261, 2262, 2469, 663, 2862, 1863, 3262, 463)) # powiatyNajw + Bialystok, Bydgoszcz, Gdansk, Gdynia, Katowice, Lublin, Olsztyn, Rzeszow, Szczecin, Torun
  pna = pna %>%
    mutate_(
      miejzam = ~ ifelse(teryt %in% powiatyAkadem, 1, ifelse(pow_grodzki == 1, 2, 3)),
      klaszam = ~ ifelse(teryt %in% powiatyNajw, 1, ifelse(pow_grodzki == 1, 2, 3))
    )

  # testy
  stopifnot(
    pna %>% group_by_('rok', 'pna') %>% summarize(n = n()) %>% filter(n > 1) %>% nrow() == 0,
    pna %>% left_join(powiaty %>% select_('rok', 'teryt', 'pow_grodzki')) %>% filter_(~ is.na(pow_grodzki)) %>% nrow() == 0
  )

  pna = pna %>%
    mutate_(
      pna5 = ~ pna,
      pna = ~ as.numeric(substring(sub('-', '', pna), 1, 3)),
      teryt = ~ teryt * 100
    ) %>%
    distinct()

  # dodaj "nieznany kod pocztowy" kodowany jako -1
  lata = min(pna$rok):max(pna$rok)
  pna = bind_rows(
    pna,
    data_frame(
      rok = lata,
      pna = rep(-1, length(lata)),
      powiat = rep('Polska', length(lata)),
      wojewodztwo = rep('Polska', length(lata)),
      teryt = rep(0, length(lata))
    )
  )

  class(pna) = c('pna_df', class(pna))
  return(pna)
}