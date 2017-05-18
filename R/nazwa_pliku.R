#' generuje nazwę pliku zgodną z konwencją
#' @description
#' Ułatwia generowanie nazw plików w powtarzalny sposób
#' @param zbior nazwa zbioru
#' @param katalog ścieżka do katalogu
#' @param rocznik rocznik absolwentów (np. 2014)
#' @param rozszerzenie rozszerzenie pliku (domyślnie .RData)
#' @export
nazwa_pliku = function(zbior, katalog = '', rocznik = '', rozszerzenie = '.RData') {
  katalog = sub('^/', '', paste0(katalog, '/'))
  rocznik = sub('^_', '', paste0(rocznik, '_'))
  return(paste0(katalog, rocznik, zbior, rozszerzenie))
}