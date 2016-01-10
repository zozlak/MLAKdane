oblicz_zmienne_pochodne = function(dane){
  stopifnot(
    is(dane, 'absolwent_df')
  )
  dane = dane %>%
    mutate_(
      bezd        = ~ as.numeric(nmb > 0),
      bezd_v2     = ~ as.numeric(nmb_v2 > 0),
      ifzus       = ~ as.numeric(nzus > 0),
      munbazy     = ~ as.numeric(nmm > 0),
      netatpod    = ~ as.numeric(nme > 0),
      npracpod    = ~ as.numeric(nmp > 0),
      samopod     = ~ as.numeric(nms > 0),
      eem         = ~ nem / len,
      eemp        = ~ nem / nme,
      emle        = ~ nmle / len,
      endn        = ~ ndn / len,
      ennn        = ~ nnnn / len,
      eprndawd    = ~ ndn / nmn,
      eprudawd    = ~ nde / nme,
      ezard       = ~ sz / nmp,
      ezard_v2    = ~ sz / len,
      ezubazyd    = ~ sze / nme,
      mnprd       = ~ nmb / len,
      mnprud      = ~ (len - nme) / len,
      pbezd       = ~ nmb / len,
      pbezd_v2    = ~ nmb_v2 / len,
      zilorazd    = ~ ezard / gezbazyd,
      zilorazd_v2 = ~ ezard / gezbazyd_v2,
      bilod       = ~ pbezd / gbezd
    ) %>%
    mutate_(
      eem         = ~ ifelse(is.finite(eem), eem, 0),
      eemp        = ~ ifelse(is.finite(eemp), eemp, 0),
      emle        = ~ ifelse(is.finite(emle), emle, 0),
      endn        = ~ ifelse(is.finite(endn), endn, 0),
      ennn        = ~ ifelse(is.finite(ennn), ennn, 0),
      eprndawd    = ~ ifelse(is.finite(eprndawd), eprndawd, 0),
      eprudawd    = ~ ifelse(is.finite(eprudawd), eprudawd, 0),
      ezard       = ~ ifelse(is.finite(ezard), ezard, 0),
      ezard_v2    = ~ ifelse(is.finite(ezard_v2), ezard_v2, 0),
      ezubazyd    = ~ ifelse(is.finite(ezubazyd), ezubazyd, 0),
      mnprd       = ~ ifelse(is.finite(mnprd), mnprd, 0),
      mnprud      = ~ ifelse(is.finite(mnprud), mnprud, 0),
      pbezd       = ~ ifelse(is.finite(pbezd), pbezd, 0),
      pbezd_v2    = ~ ifelse(is.finite(pbezd_v2), pbezd_v2, 0),
      zilorazd    = ~ ifelse(is.finite(zilorazd), zilorazd, 0),
      zilorazd_v2 = ~ ifelse(is.finite(zilorazd_v2), zilorazd_v2, 0),
      bilod       = ~ ifelse(is.finite(bilod), bilod, 0)
    )
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}