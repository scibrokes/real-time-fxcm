info_aic <- function(fit) {
  tryCatch({
    aic <- suppressWarnings(infocriteria(fit))
    aic <- tibble(info = rownames(aic), value = aic[,1])
    
  }, error = function(cond) {
    aic <- tibble(info = rownames(aic), value = rep(NA, 4))
  })
}


