test_garch <- function(data, model = 'sGARCH', distribution.model = 'snorm') {
  
  g_specs <- ugarchspec(
    variance.model = list(model = model, garchOrder = c(1, 1)), 
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
    distribution.model = distribution.model)
  
  g_fit <- tryCatch({
    suppressWarnings(ugarchfit(data = data, spec = g_specs))
    
  }, error = function(cond) {
    message('error!')
    return(NULL)
  })
  
  return(g_fit)
}
