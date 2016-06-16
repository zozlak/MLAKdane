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
      gbezd  = ~ ifelse(len > 0, gbezd, NA),
      gezd   = ~ ifelse(len > 0, gezd, NA),
      nde    = ~ ifelse(len > 0, nde, NA),
      ndn    = ~ ifelse(len > 0, ndn, NA),
      nem    = ~ ifelse(len > 0, nem, NA),
      nmb    = ~ ifelse(len > 0, nmb, NA),
      nmb_v2 = ~ ifelse(len > 0, nmb_v2, NA),
      nme    = ~ ifelse(len > 0, nme, NA),
      nmj    = ~ ifelse(len > 0, nmj, NA),
      nmle   = ~ ifelse(len > 0 & nme > 0, nmle, NA),
      nmlenp = ~ ifelse(len > 0 & nme > 0, nmlenp, NA),
      nmlep  = ~ ifelse(len > 0 & nme > 0, nmlep, NA),
      nmm    = ~ ifelse(len > 0, nmm, NA),
      nmn    = ~ ifelse(len > 0, nmn, NA),
      nmp    = ~ ifelse(len > 0, nmp, NA),
      nms    = ~ ifelse(len > 0, nms, NA),
      nmz    = ~ ifelse(len > 0, nmz, NA),
      nndn   = ~ ifelse(len > 0, nndn, NA),
      nnnn   = ~ ifelse(len > 0, nnnn, NA),
      sz     = ~ ifelse(len > 0, sz, NA),
      sze    = ~ ifelse(len > 0, sze, NA),
      szn    = ~ ifelse(len > 0, szn, NA),
      zpow   = ~ ifelse(len > 0 & nmz > 0, zpow, NA)
    ) %>%
    mutate_(
      bbazyd      = ~ as.numeric(nmb_v2 > 0),
      bezd        = ~ as.numeric(nmb > 0),
      eem         = ~ nem / len,
      eemp        = ~ nem / nme,
      emle        = ~ 12 * nmle / len,
      emlenp      = ~ 12 * nmlenp / len,
      emlep       = ~ 12 * nmlep / len,
      endn        = ~ 12 * ndn / len,
      ennn        = ~ 12 * nnnn / len,
      ezard       = ~ (sze + szn) / nmz,
      ezarda      = ~ (sze + szn) / len,
      ezud        = ~ sze / nme,
      mnprd       = ~ (len - nmp) / len,
      mnprud      = ~ (len - nme) / len,
      netatd      = ~ as.numeric(nme > 0),
      npracd      = ~ as.numeric(nmp > 0),
      pbezd       = ~ 100 * nmb / len,
      pbezd_v2    = ~ 100 * nmb_v2 / len,
      prndaw      = ~ ndn / nmn,
      prudaw      = ~ nde / nme,
      samod       = ~ as.numeric(nms > 0),
      zilo        = ~ ezard / zpow,
      ziloa       = ~ ezard / gezd,
      bilod       = ~ pbezd / gbezd,
      pmstudetat     = ~ nmstudetat / len,
      pmstudnetat    = ~ nmstudnetat / len,
      pmstuddziecko  = ~ nmstuddziecko / len,
      pmstudinne     = ~ nmstudinne / len,
      pmstudbd       = ~ nmstudbd / len,
      pmnstudetat    = ~ nmnstudetat / len,
      pmnstudnetat   = ~ nmnstudnetat / len,
      pmnstuddziecko = ~ nmnstuddziecko / len,
      pmnstudbezrob  = ~ nmnstudbezrob / len,
      pmnstudinne    = ~ nmnstudinne / len,
      pmnstudbd      = ~ nmnstudbd / len      
    ) %>%
    mutate_(
      bilod    = ~ ifelse(is.infinite(bilod), 0, bilod),
      eem      = ~ ifelse(is.infinite(eem), NA, eem),
      eemp     = ~ ifelse(is.infinite(eemp), NA, eemp),
      emle     = ~ ifelse(is.infinite(emle), NA, emle),
      emlenp   = ~ ifelse(is.infinite(emlenp), NA, emlenp),
      emlep    = ~ ifelse(is.infinite(emlep), NA, emlep),
      endn     = ~ ifelse(is.infinite(endn), NA, endn),
      ennn     = ~ ifelse(is.infinite(ennn), NA, ennn),
      ezard    = ~ ifelse(is.infinite(ezard), NA, ezard),
      ezarda   = ~ ifelse(is.infinite(ezarda), NA, ezarda),
      ezud     = ~ ifelse(is.infinite(ezud), NA, ezud),
      mnprd    = ~ ifelse(is.infinite(mnprd), NA, mnprd),
      mnprud   = ~ ifelse(is.infinite(mnprud), NA, mnprud),
      pbezd    = ~ ifelse(is.infinite(pbezd), NA, pbezd),
      pbezd_v2 = ~ ifelse(is.infinite(pbezd_v2), NA, pbezd_v2),
      prndaw   = ~ ifelse(is.infinite(prndaw), NA, prndaw),
      prudaw   = ~ ifelse(is.infinite(prudaw), 0, prudaw),
      zilo     = ~ ifelse(is.infinite(zilo), NA, zilo),
      ziloa    = ~ ifelse(is.infinite(ziloa), NA, ziloa),
      pmstudetat     = ~ ifelse(is.infinite(pmstudetat), NA, pmstudetat),
      pmstudnetat    = ~ ifelse(is.infinite(pmstudnetat), NA, pmstudnetat),
      pmstuddziecko  = ~ ifelse(is.infinite(pmstuddziecko), NA, pmstuddziecko),
      pmstudinne     = ~ ifelse(is.infinite(pmstudinne), NA, pmstudinne),
      pmstudbd       = ~ ifelse(is.infinite(pmstudbd), NA, pmstudbd),
      pmnstudetat    = ~ ifelse(is.infinite(pmnstudetat), NA, pmnstudetat),
      pmnstudnetat   = ~ ifelse(is.infinite(pmnstudnetat), NA, pmnstudnetat),
      pmnstuddziecko = ~ ifelse(is.infinite(pmnstuddziecko), NA, pmnstuddziecko),
      pmnstudbezrob  = ~ ifelse(is.infinite(pmnstudbezrob), NA, pmnstudbezrob),
      pmnstudinne    = ~ ifelse(is.infinite(pmnstudinne), NA, pmnstudinne),
      pmnstudbd      = ~ ifelse(is.infinite(pmnstudbd), NA, pmnstudbd)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}