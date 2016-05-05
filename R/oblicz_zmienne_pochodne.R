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
      munbaz      = ~ as.numeric(nmm > 0),
      netatd      = ~ as.numeric(nme > 0),
      npracd      = ~ as.numeric(nmp > 0),
      samod       = ~ as.numeric(nms > 0),
      eem         = ~ nem / len,
      eemp        = ~ nem / nme,
      emle        = ~ 12 * nmle / len,
      emlenp      = ~ 12 * nmlenp / len,
      emlep       = ~ 12 * nmlep / len,
      endn        = ~ 12 * ndn / len,
      ennn        = ~ 12 * nnnn / len,
      prndaw      = ~ ndn / nmn,
      prudaw      = ~ nde / nme,
      ezard       = ~ (sze + szn) / nmp,
      ezarda      = ~ (sze + szn) / len,
      ezud        = ~ sze / nme,
      mnprd       = ~ (len - nmp) / len,
      mnprud      = ~ (len - nme) / len,
      pbezd       = ~ nmb / len,
      pbezd_v2    = ~ nmb_v2 / len,
      zilo        = ~ ezard / zpow,
      ziloa       = ~ ezard / gezd,
      bilod       = ~ pbezd / gbezd
    ) %>%
    mutate_(
      eem      = ~ ifelse(is.finite(eem), eem, NA),
      eemp     = ~ ifelse(is.finite(eemp), eemp, NA),
      emle     = ~ ifelse(is.finite(emle), emle, NA),
      emlenp   = ~ ifelse(is.finite(emlenp), emlenp, NA),
      emlep    = ~ ifelse(is.finite(emlep), emlep, NA),
      endn     = ~ ifelse(is.finite(endn), endn, NA),
      ennn     = ~ ifelse(is.finite(ennn), ennn, NA),
      prndaw   = ~ ifelse(is.finite(prndaw), prndaw, NA),
      prudaw   = ~ ifelse(is.finite(prudaw), prudaw, 0),
      ezard    = ~ ifelse(is.finite(ezard), ezard, NA),
      ezarda   = ~ ifelse(is.finite(ezarda), ezarda, NA),
      ezud     = ~ ifelse(is.finite(ezud), ezud, NA),
      mnprd    = ~ ifelse(is.finite(mnprd), mnprd, NA),
      mnprud   = ~ ifelse(is.finite(mnprud), mnprud, NA),
      pbezd    = ~ ifelse(is.finite(pbezd), pbezd, NA),
      pbezd_v2 = ~ ifelse(is.finite(pbezd_v2), pbezd_v2, NA),
      zilo     = ~ ifelse(is.finite(zilo), zilo, 0),
      ziloa    = ~ ifelse(is.finite(ziloa), ziloa, 0),
      bilod    = ~ ifelse(is.finite(bilod), bilod, 0),
      nmb_v2   = ~ ifelse(is.na(nmb_v2), 0, nmb_v2),
      nem      = ~ ifelse(is.na(nem), 0, nem),
      sz       = ~ ifelse(is.na(sz), 0, sz),
      sze      = ~ ifelse(is.na(sze), 0, sze),
      szn      = ~ ifelse(is.na(szn), 0, szn),
      nde      = ~ ifelse(is.na(nde), 0, nde),
      ndn      = ~ ifelse(is.na(ndn), 0, ndn),
      nmb      = ~ ifelse(is.na(nmb), 0, nmb),
      nme      = ~ ifelse(is.na(nme), 0, nme),
      nmn      = ~ ifelse(is.na(nmn), 0, nmn),
      nms      = ~ ifelse(is.na(nms), 0, nms),
      nmz      = ~ ifelse(is.na(nmz), 0, nmz),
      nmp      = ~ ifelse(is.na(nmp), 0, nmp),
      nmj      = ~ ifelse(is.na(nmj), 0, nmj),
      nmm      = ~ ifelse(is.na(nmm), 0, nmm),
      nzus     = ~ ifelse(is.na(nzus), 0, nzus),
      nmle     = ~ ifelse(nme > 0, nmle, NA),
      nmlep    = ~ ifelse(nme > 0, nmlep, NA),
      nmlenp   = ~ ifelse(nme > 0, nmlenp, NA)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}