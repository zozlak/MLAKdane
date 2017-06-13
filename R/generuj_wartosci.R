#' @title generuje wartości zadanych zmiennych dla zadanych odbiorców
#' @description Raporty w postaci plików pdf są trudne do automatycznego
#' przetwarzania, stąd potrzeba wygenerowania zawartych w nich informacji w
#' formie "zwyczajnych" zbiorów danych, co umożliwia niniejsza funkcja.
#'
#' Niestety w chwili obecnej zamiast parsować szablon raportu wymaga manualnego
#' podania definicji wszystkich zmiennych.
#' @details
#' Zmienne w zwracanej ramce danych noszą nazwy będące połączeniem parametru
#' \code{prefiks} oraz ich indeksu w parametrze \code{zmienne}. Jeśli dana
#' zmienna została pominięta (patrz parametr \code{pomin}), nie powoduje to
#' przenumerowania zmiennych następujących za nią (np. gdy \code{zmienne =
#' c('formuła1', 'NIE DOTYCZY', 'formuła2')} i \code{prefiks = 'p_'}, wtedy
#' wygenerowane zostaną zmienne \code{p_1} oraz \code{p_3}).
#' @param odbiorcy ramka danych opisująca odbiorców
#' @param zbDanych lista zawierająca zbiory danych na podstawie których
#'   wyliczane będą wartości zmiennych
#' @param zmienne wektor definicji zmiennych (wektor stringów, patrz też
#'   parametr \code{pomin})
#' @param grupa warunek wybierający obserwacje należące do danego odbiorcy (w
#'   postaci stringu)
#' @param prefiks prefiks nazw generowanych zmiennych
#' @param stale wektor indeksów parametru \code{zmienne} wskazujący zmienne,
#'   które nie podlegają anonimizacji (są generowane bez względu na liczebność
#'   grupy - patrz parametr \code{nMin})
#' @param pomin wektor wartości oznaczających, że dana zmienna powinna zostać
#'   pominięta (patrz sekcja "details")
#' @param nMin liczba obserwacji poniżej której wartości nie są wyliczane
#' @return data.frame ramka danych z wyliczonymi wartościami zmiennych
#' @export
oblicz_wartosci = function(odbiorcy, zbDanych, zmienne, grupa, prefiks, stale = 1:9, pomin = 'NIE DOTYCZY', nMin = 10){
  stopifnot(
    is.data.frame(odbiorcy),
    is.list(zbDanych), all(lapply(zbDanych, is.data.frame)),
    is.vector(zmienne), is.character(zmienne),
    is.vector(prefiks), is.character(prefiks), length(prefiks) == 1, all(!is.na(prefiks)),
    is.vector(grupa), is.character(grupa), length(grupa) == 1, all(!is.na(grupa)),
    is.vector(stale), is.numeric(stale), all(!is.na(stale)),
    is.vector(pomin), is.character(pomin), all(!is.na(pomin)),
    is.vector(nMin), is.numeric(nMin), length(nMin) == 1, all(!is.na(nMin))
  )

  for (i in seq_along(zbDanych)) {
    attach(zbDanych[[i]])
  }

  wartosci = matrix(NA_real_, nrow(odbiorcy), length(zmienne))
  colnames(wartosci) = paste0(prefiks, seq_along(zmienne))
  wartosci = as.data.frame(wartosci)

  zmFiltr = !(zmienne %in% pomin)
  zmienne = zmienne[zmFiltr]
  wartosci = wartosci[, which(zmFiltr)]

  for (i in seq_len(nrow(odbiorcy))) {
    attach(odbiorcy[i, ])
    zmTmp = zmienne
    eval(parse(text = grupa))
    if (eval(parse(text = paste0('sum(', sub('[ =].*$', '', grupa), ') < ', nMin)))) {
      zmTmp = zmienne[stale]
    }
    for (j in seq_along(zmTmp)) {
      wartosci[i, j] = eval(parse(text = zmTmp[j]))
    }
    detach(odbiorcy[i, ])
  }
  for (i in seq_along(zbDanych)) {
    detach(zbDanych[[i]])
  }
  return(wartosci)
}
