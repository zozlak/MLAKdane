#' łączy dane zus, zdau oraz statystyki powiatów
#' @description
#' Aby poprawnie przyłączyć statystyki powiatów macierz danych uzupełniana jest
#' tak, aby dla każdego absolwenta zawierała informacje o każdym okresie w
#' zadanym przedziale czasu, ale nie później niż do "końca obserwacji w zus"
#' (zakładając zamieszkanie absolwenta w uzupełnianych okresach "w Polsce",
#' kodowane jako pna = -1, oraz wszelkie cechy tytułu ubezpieczenia ZUS równe 0,
#' w szczególności także bezrob = 0).
#' @param zus dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zus}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param pnaPowiaty dane wygenerowane za pomocą funkcji \code{\link{polacz_pna_powiaty}}
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch znaków, np. '2015-09-30')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
polacz_zus_zdau = function(zus, zdau, pnaPowiaty, dataMin, dataMax){
  stopifnot(
    is(zus, 'zus_df'),
    is(zdau, 'zdau_df')
  )

  okresy = data2okres(dataMin):data2okres(dataMax)
  absolw = zdau %>%
    filter_(~ typ %in% 'A')
  wynik = data.frame(
    id_zdau      = rep(absolw$id_zdau,      each = length(okresy)),
    id           = rep(absolw$id,           each = length(okresy)),
    data_zak     = rep(absolw$data_zak,     each = length(okresy)),
    jednostka_id = rep(absolw$jednostka_id, each = length(okresy)),
    okres        = rep(okresy,              times = nrow(absolw))
  )
  wynik = wynik %>%
    left_join(
      zus %>%
        select_('-nspraw', '-pkd', '-platnik_kon', '-plec', '-rok_ur', '-rolnik', '-zlec')
    ) %>%
    filter_(~ okres < koniec) %>%
    select_('-koniec')

  wynik = wynik %>%
    mutate_(
      etat     = ~ ifelse(is.na(etat), 0, etat),
      netat    = ~ ifelse(is.na(netat), 0, netat),
      samoz    = ~ ifelse(is.na(samoz), 0, samoz),
      bezrob   = ~ ifelse(is.na(bezrob), 0, bezrob),
      rentemer = ~ ifelse(is.na(rentemer), 0, rentemer),
      student  = ~ ifelse(is.na(student), 0, student),
      prawnik  = ~ ifelse(is.na(prawnik), 0, prawnik),
      mundur   = ~ ifelse(is.na(mundur), 0, mundur),
      # zlec     = ~ ifelse(is.na(zlec), 0, zlec),
      # nspraw   = ~ ifelse(is.na(nspraw), 0, nspraw),
      # rolnik   = ~ ifelse(is.na(rolnik), 0, rolnik),
      podst    = ~ ifelse(is.na(podst), 0, podst),
      pna      = ~ ifelse(is.na(pna), -1, pna),
      rok      = ~ floor(okres / 12)
    )

  wynik = left_join(wynik, pnaPowiaty)

  class(wynik) = c('baza_df', class(wynik))
  return(wynik)
}