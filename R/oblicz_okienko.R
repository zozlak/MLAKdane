oblicz_okienko = function(dane, okienkoMin, okienkoMax, dataMin, dataMax){
  stopifnot(
    is(dane, 'baza_df'),
    is.vector(okienkoMin), is.numeric(okienkoMin), length(okienkoMin) == 1, all(!is.na(okienkoMin)),
    is.vector(okienkoMax), is.numeric(okienkoMax), length(okienkoMax) == 1, all(!is.na(okienkoMax)),
    is.vector(dataMin), is.character(dataMin), length(dataMin) == 1, all(!is.na(dataMin)),
    is.vector(dataMax), is.character(dataMax), length(dataMax) == 1, all(!is.na(dataMax))
  )

  klasy = class(dane)

  dane = dane %>%
    mutate_(
      okres_min = ~ data_zak + okienkoMin,
      okres_max = ~ data_zak + okienkoMax
    ) %>%
    mutate_(
      okres_min  = ~ ifelse(okres_min < data2okres(dataMin), data2okres(dataMin), okres_min),
      okres_max  = ~ ifelse(okres_max > data2okres(dataMax), data2okres(dataMax), okres_max),
      len        = ~ okres_max - okres_min + 1
    ) %>%
    filter_(~ len >= 0)

  class(dane) = c('okienko_df', klasy)
  return(dane)
}