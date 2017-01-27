#' wylicza zadane okienko danych
#' @param okienko obiekt okienka utworzony za pomocą funkcji
#'   \code{\link{okienko}}
#' @param zdau dane zwrócone przez funkcję \code{\link{przygotuj_zdau}}
#' @param baza dane zwrócone przez funkcję \code{\link{polacz_w_baze}}
#' @param miesieczne dane miesieczne zwrócone przez funkcję
#'   \code{\link{agreguj_do_miesiecy}}
#' @param utrataPracy dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_utrata_pracy}}
#' @param jednostki dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_jednostki}}
#' @param krok liczba obserwacji obliczanych w pojedynczym kroku (determinuje
#'   rozmiar potrzebnej pamięci - im większa, tym więcej pamięci potrzeba, ale
#'   też obliczenia przebiegają nieco szybciej)
#' @param info czy wyświetlać informacje o przetwarzanym okienku
#' @return [data.frame] dane obliczone dla wskazanego okienka czasu
#' @import dplyr
oblicz_okienko = function(okienko, zdau, baza, miesieczne, utrataPracy, jednostki, krok = 1000000, info = TRUE) {
  stopifnot(
    methods::is(okienko, 'okienko'),
    methods::is(zdau, 'zdau_df'),
    methods::is(baza, 'baza_df'),
    methods::is(miesieczne, 'miesieczne_df'),
    methods::is(utrataPracy, 'utrata_pracy_df'),
    methods::is(jednostki, 'jednostki_df'),
    is.vector(krok), is.numeric(krok), length(krok) == 1, all(!is.na(krok)),
    is.vector(info), is.logical(info), length(info) == 1, all(!is.na(info))
  )

  if (info) {
    message(okienko[['offsetMin']], '-', okienko[['offsetMax']])
  }

  abs = up = np = zam = list()
  n = c(krok, 0)
  while (TRUE) {
    krokZdau = krok_zdau(zdau, n, TRUE)
    if (nrow(krokZdau) == 0) {
      break
    }
    if (info) {
      message('  ', (n[2] + 1), '-', (n[2] + n[1]))
    }

    okienkoMies = ustaw_okienko(miesieczne, okienko, krokZdau)
    okienkoBaza = ustaw_okienko(baza, okienko, krokZdau)

    abs[[length(abs) + 1]] = agreguj_do_okresu(okienkoMies)
    up[[length(up) + 1]] = oblicz_utrata_etatu(okienkoMies, utrataPracy)
    np[[length(np) + 1]] = oblicz_pracodawcy(okienkoBaza)

    zam[[length(zam) + 1]]  =  oblicz_zamieszkanie(okienkoBaza, jednostki, TRUE)

    n[2] = n[2] + krok
  }

  abs = bind_rows(abs)
  up = bind_rows(up)
  np = bind_rows(np)
  zam = bind_rows(zam)

  zdauAbs = krok_zdau(zdau, c(nrow(zdau), 0), TRUE)
  razem = zdauAbs %>%
    full_join(abs) %>%
    full_join(np) %>%
    full_join(up) %>%
    full_join(zam) %>%
    mutate_(len = ~dplyr::coalesce(as.integer(len), 0L))
  stopifnot(
    nrow(zdauAbs) == nrow(razem),
    length(unique(razem$id_zdau)) == nrow(razem)
  )

  colnames(razem) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(razem), okienko[['sufiks']]))
  return(razem)
}
