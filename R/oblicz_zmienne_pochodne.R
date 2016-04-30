#' oblicza zmienne wywodzone z wcześniej policzonych zmiennych
#' @param dane połączone dane na poziomie (zdau) wygenerowane za pomocą innych
#'   funkcji
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_pochodne = function(dane){
  stopifnot(
    'id_zdau' %in% colnames(dane),
    length(unique(unlist(dane[, 'id_zdau']))) == nrow(dane)
  )
  dane = dane %>%
    mutate_(
      bezd        = ~ as.numeric(nmb > 0),
      bbazyd      = ~ as.numeric(nmb_v2 > 0),
      ifzus       = ~ as.numeric(nzus > 0),
      munbaz      = ~ as.numeric(nmm > 0),
      netatd      = ~ as.numeric(nme > 0),
      npracd      = ~ as.numeric(nmp > 0),
      samod       = ~ as.numeric(nms > 0),
      eem         = ~ nem / len,
      eemp        = ~ nem / nme,
      emle        = ~ nmle / len,
      endn        = ~ ndn / len,
      ennn        = ~ nnnn / len,
      prndaw      = ~ ndn / nmn,
      prudaw      = ~ nde / nme,
      ezard       = ~ (sze + szn) / nmp,
      ezarda      = ~ (sze + szn) / len,
      ezud        = ~ sze / nme,
      mnprd       = ~ (len - nmp) / len,
      mnprud      = ~ (len - nme) / len,
      pbezd       = ~ nmb / len,
      pbezd_v2    = ~ nmb_v2 / len,
      zilorazd    = ~ ezard / gezbazyd,
      zilorazd_v2 = ~ ezard / gezbazyd_v2,
      bilod       = ~ pbezd / gbezd
    ) %>%
    mutate_(
      eem         = ~ ifelse(is.finite(eem), eem, NA),
      eemp        = ~ ifelse(is.finite(eemp), eemp, NA),
      emle        = ~ ifelse(is.finite(emle), emle, NA),
      endn        = ~ ifelse(is.finite(endn), endn, NA),
      ennn        = ~ ifelse(is.finite(ennn), ennn, NA),
      prndaw      = ~ ifelse(is.finite(prndaw), prndaw, NA),
      prudaw      = ~ ifelse(is.finite(prudaw), prudaw, 0),
      ezard       = ~ ifelse(is.finite(ezard), ezard, NA),
      ezarda      = ~ ifelse(is.finite(ezarda), ezarda, NA),
      ezud        = ~ ifelse(is.finite(ezubazyd), ezubazyd, NA),
      mnprd       = ~ ifelse(is.finite(mnprd), mnprd, NA),
      mnprud      = ~ ifelse(is.finite(mnprud), mnprud, NA),
      pbezd       = ~ ifelse(is.finite(pbezd), pbezd, NA),
      pbezd_v2    = ~ ifelse(is.finite(pbezd_v2), pbezd_v2, NA),
      zilorazd    = ~ ifelse(is.finite(zilorazd), zilorazd, 0),
      zilorazd_v2 = ~ ifelse(is.finite(zilorazd_v2), zilorazd_v2, 0),
      bilod       = ~ ifelse(is.finite(bilod), bilod, 0)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}