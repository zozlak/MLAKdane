oblicz_zamieszkanie = function(dane, jednostki, okienkoMin){
  stopifnot(
    is(dane, 'okienko_df')
  )

  jednostki = jednostki %>%
    select_('jednostka_id', 'teryt') %>%
    mutate_(
      powiat_jedn = ~ floor(teryt / 100),
      woj_jedn    = ~ floor(teryt / 10000)
    ) %>%
    select_('-teryt')

  dane = dane %>%
    filter_(~ okres == ifelse(okienkoMin == 0, okres_min, okres_max)) %>%
    left_join(jednostki) %>%
    mutate_(
      jpdzam = ~ ifelse(
        powiat_jedn == floor(teryt / 100), 1, ifelse(
          woj_jedn == floor(teryt / 10000), 2, ifelse(
            teryt > 0, 3, NA)
          )
        )
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      powiat      = ~ uzgodnij_teryt(teryt),
      klasazam    = ~ ifelse(n_distinct(klasazam) == 1, klasazam, NA),
      klasazam_v2 = ~ min(klasazam2),
      miejzam     = ~ ifelse(n_distinct(miejzam) == 1, miejzam, NA),
      miejzam_v2  = ~ min(miejzam2),
      jpdzam      = ~ min(jpdzam),
      jpdzam_v2   = ~ ifelse(n_distinct(jpdzam) == 1, jpdzam, NA)
    ) %>%
    mutate_(
      powiat    = ~ ifelse(powiat %% 100 > 0, powiat, NA),
      nrwoj     = ~ ifelse(powiat > 0, floor(powiat / 100), NA)
    )

  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}