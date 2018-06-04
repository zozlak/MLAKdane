generuj_odbiorcow = function(katZr, dataBazy, katalog, format = 'xlsx', typ = c('Polska', 'uczelnie', 'kierunki'), rocznik = NULL, zdau = NULL) {
  if (is.null(zdau)) {
    zdau = przygotuj_zdau(katZr)
  }

  poziomy1 = c('1' = 'Studia pierwszego stopnia', '2' = 'Studia drugiego stopnia', 'JM' = 'Studia jednolite magisterskie')
  poziomy2 = c('1' = '1_stopnia', '2' = '2_stopnia', 'JM' = 'jednolite_mgr')
  poziomy3 = c('1' = '1st', '2' = '2st', 'JM' = 'jmgr')
  poziomy4 = c('1' = 'pierwszego stopnia', '2' = 'drugiego stopnia', 'JM' = 'jednolite magisterskie')
  formy = c('niestac.' = 'studia niestacjonarne', 'stac.' = 'studia stacjonarne', 'stac./niestac.' = 'studia stacjonarne i niestacjonarne')

  zdau = zdau %>%
    collect() %>%
    filter_(~typ == 'A') %>%
    mutate_(
      vrok = ~okres2rok(data_do),
      vpoziom = ~poziomy1[poziom],
      dwb = ~dataBazy,
      vrokdyp = ~vrok
    ) %>%
    rename_(vstopien = 'poziom')
  if (!is.null(rocznik)) {
    zdau = zdau %>%
      filter_(~vrok == rocznik)
  }

  rok = as.integer(sub('.*([0-9]{4}).*', '\\1', dataBazy))
  typy = readr::cols(pna5 = 'c', pna = 'i', teryt = 'i', id_gus = 'c', powiat = 'c', wojewodztwo = 'c', miejzam = 'i', 'klaszam' = 'i', .default = 'd')
  pnaPowiaty = readr::read_csv2('dane/pna_powiaty_mb.csv', col_types = typy) %>%
    mutate_(pna = ~sub('#', '-', pna5)) %>%
    select_('pna', 'teryt') %>%
    distinct() %>%
    filter_(~!is.na(pna))
  nazwyPowiatow = suppressMessages(readr::read_csv2(paste0(katZr, '/../nazwy2teryt.csv'))) %>%
    select_('-gmina') %>%
    mutate_(
      teryt = ~as.integer(teryt / 100)
    ) %>%
    distinct() %>%
    mutate_(
      wojewodztwo_dop = ~sub('e$', 'm', wojewodztwo),
      powiat_dop = ~sub('ki$', 'kiego', gsub('ki ', 'kiego ', powiat))
    )
  pnaPowiaty = pnaPowiaty %>%
    inner_join(nazwyPowiatow) %>%
    group_by_('pna', 'teryt') %>%
    arrange_('desc(rok)') %>%
    filter_(~row_number(pna) == 1) %>%
    ungroup()

  if ('Polska' %in% typ) {
    pl = zdau %>%
      select_('dwb', 'vrok', 'vrokdyp', 'vstopien', 'vpoziom') %>%
      distinct() %>%
      arrange_('vrok', 'vstopien') %>%
      mutate_(
        nr       = ~row_number(vrok),
        odbiorca = ~'PL'
      ) %>%
      mutate_(
        nazwa_pliku = ~sprintf('%d_%d_%s_%s', vrok %% 100, as.integer(sub('^.*([0-9]{4}).*$', '\\1', dwb)) - vrok, odbiorca, poziomy2[vstopien])
      ) %>%
      select_('nazwa_pliku', 'nr', 'odbiorca', 'vrokdyp', 'dwb', 'vrok', 'vstopien', 'vpoziom')
    colnames(pl) = toupper(colnames(pl))
    sciezka = paste0(katalog, '/odbiorcy_PL.', format)
    if (format == 'xlsx') {
      openxlsx::write.xlsx(pl, sciezka)
    } else {
      readr::write_delim(pl, sciezka, delim = ';', na = '')
    }
  }

  if ('uczelnie' %in% typ) {
    uczelnie = zdau %>%
      select_('dwb', 'vrok', 'vrokdyp', 'vstopien', 'vpoziom', 'uczelnia_id') %>%
      distinct() %>%
      arrange_('vrok', 'vstopien', 'uczelnia_id') %>%
      left_join(przygotuj_uczelnie(katZr)) %>%
      rename_(pna = 'kod_pocztowy') %>%
      left_join(pnaPowiaty) %>%
      select_('dwb', 'vrok', 'vrokdyp', 'vstopien', 'vpoziom', 'uczelnia_id', 'uczelnia_kod', 'uczelnia_nazwa', 'powiat_dop', 'wojewodztwo_dop', 'uczelnia_mundur') %>%
      mutate_(
        powiat_dop      = ~coalesce(powiat_dop, '-'),
        wojewodztwo_dop = ~coalesce(wojewodztwo_dop, '-')
      ) %>%
      rename_(
        vuczelnia = 'uczelnia_id',
        vucz      = 'uczelnia_nazwa',
        vuczskrt  = 'uczelnia_kod',
        vmundur   = 'uczelnia_mundur'
      ) %>%
      mutate_(
        odbiorca              = ~vuczskrt,
        nr                    = ~row_number(vuczelnia),
        nazwa_pliku           = ~sprintf('%d_%d_%s_%s_%d', vrok %% 100, as.integer(sub('^.*([0-9]{4}).*$', '\\1', dwb)) - vrok, vuczskrt, poziomy3[vstopien], vuczelnia),
        opis_pliku            = ~sprintf('%d_%d_%s_%s', vrok %% 100, as.integer(sub('^.*([0-9]{4}).*$', '\\1', dwb)) - vrok, vuczskrt, poziomy2[vstopien]),
        nazwawojewodztwo      = ~paste0('w województwie ', wojewodztwo_dop),
        nazwapozawojewodztwem = ~paste0('poza województwem ', wojewodztwo_dop),
        nazwamiasto           = ~paste0('na terenie powiatu ', if_else(tolower(substr(powiat_dop, 1, 1)) == substr(powiat_dop, 1, 1), '', 'm. '), powiat_dop)
      ) %>%
      select_('nazwa_pliku', 'opis_pliku', 'nr', 'odbiorca', 'vrokdyp', 'vuczelnia', 'dwb', 'vrok', 'vstopien', 'vpoziom', 'vmundur', 'vucz', 'vuczskrt', 'nazwamiasto', 'nazwawojewodztwo', 'nazwapozawojewodztwem')
    colnames(uczelnie) = toupper(colnames(uczelnie))
    sciezka = paste0(katalog, '/odbiorcy_uczelnie.', format)
    if (format == 'xlsx') {
      openxlsx::write.xlsx(uczelnie, sciezka)
    } else {
      readr::write_delim(uczelnie, sciezka, delim = ';', na = '')
    }
  }

  if ('kierunki' %in% typ) {
    kierunki = przygotuj_kierunki(katZr, TRUE) %>%
      rename_(vrok = 'rok')
    jednostki = przygotuj_jednostki(katZr) %>%
      rename_(vrok = 'rok') %>%
      mutate_(teryt = ~as.integer(teryt / 100))
    uczelnie = przygotuj_uczelnie(katZr) %>%
      select_('uczelnia_id', 'uczelnia_nazwa', 'uczelnia_kod', 'uczelnia_mundur')
    kierunkiOdb = zdau %>%
      select_('dwb', 'vrok', 'vrokdyp', 'vstopien', 'vpoziom', 'kierunek_id') %>%
      distinct() %>%
      arrange_('vrok', 'vstopien', 'kierunek_id') %>%
      left_join(kierunki) %>%
      left_join(jednostki) %>%
      left_join(uczelnie) %>%
      left_join(nazwyPowiatow %>% rename_(vrok = 'rok')) %>%
      mutate_(
        powiat          = ~coalesce(powiat, '-'),
        wojewodztwo     = ~coalesce(wojewodztwo, '-'),
        powiat_dop      = ~coalesce(powiat_dop, '-'),
        wojewodztwo_dop = ~coalesce(wojewodztwo_dop, '-')
      ) %>%
      select_('dwb', 'vrok', 'vrokdyp', 'vstopien', 'poziom', 'kierunek_id', 'kierunek_nazwa', 'miedzywydz', 'forma_ksztalcenia', 'jednostka_id', 'jednostka_nazwa', 'uczelnia_id', 'uczelnia_kod', 'uczelnia_nazwa', 'uczelnia_mundur', 'miejscowosc', 'powiat', 'wojewodztwo', 'wojewodztwo_dop', 'powiat_dop') %>%
      rename_(
        vkier     = 'kierunek_id',
        vuczelnia = 'uczelnia_id',
        vucz      = 'uczelnia_nazwa',
        vuczskrt  = 'uczelnia_kod',
        vmundur   = 'uczelnia_mundur'
      ) %>%
      mutate_(
        nr                    = ~row_number(vkier),
        nazwa_pliku           = ~sprintf('%d_%d_%s_%d', vrok %% 100, as.integer(sub('^.*([0-9]{4}).*$', '\\1', dwb)) - vrok, vuczskrt, vkier),
        vkierunek             = ~paste0(kierunek_nazwa, ', ', formy[forma_ksztalcenia], ' ', poziomy4[poziom]),
        nazwawojewodztwo      = ~paste0('w województwie ', wojewodztwo_dop),
        nazwapozawojewodztwem = ~paste0('poza województwem ', wojewodztwo_dop),
        nazwamiasto           = ~paste0('na terenie powiatu ', if_else(tolower(substr(powiat_dop, 1, 1)) == substr(powiat_dop, 1, 1), '', 'm. '), powiat_dop)
      ) %>%
      group_by_('vrok', 'vuczelnia') %>%
      mutate_(
        vjpd = ~ifelse(rep(n_distinct(jednostka_id) > 1L, n()), sub('^.*; ?', '', jednostka_nazwa), ' ')
      ) %>%
      ungroup() %>%
      mutate_(
        vjpd = ~if_else(vjpd == vucz, '', vjpd)
      ) %>%
      select_('nazwa_pliku', 'nr', 'vrokdyp', 'vkier', 'dwb', 'vrok', 'vstopien', 'vjpd', 'vkierunek', 'vucz', 'vuczskrt', 'miejscowosc', 'powiat', 'wojewodztwo', 'vmundur', 'nazwamiasto', 'nazwawojewodztwo', 'nazwapozawojewodztwem')
    colnames(kierunkiOdb) = toupper(colnames(kierunkiOdb))
    sciezka = paste0(katalog, '/odbiorcy_kierunki.', format)
    if (format == 'xlsx') {
      openxlsx::write.xlsx(kierunkiOdb, sciezka)
    } else {
      readr::write_delim(kierunkiOdb, sciezka, delim = ';', na = '')
    }
  }
}