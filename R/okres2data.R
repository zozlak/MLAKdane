#' konwertuje okres składkowy (rok i miesiąc) wyrażony liczbą na datę
okres2data = function(okres){
  return(sprintf('%04d-%02d', floor(okres / 12), okres %% 12))
}