## Due to limit number of files and single size, here I force to push
##   the files by batch.

dr <- paste0(getwd(), '/data/')
system(paste0('cd ', dr, ''))

cr_code <- dir(paste0(dr))[-6]

lfs <- llply(cr_code, function(x) {
  system(paste0('cd ', getwd(), '/data/', x, ''))
  gt <- paste('!git add', paste0(list.files(paste0(dr, x), pattern = '.rds')))
  gt2 <- paste0('git commit -m \'add ', list.files(paste0(dr, x), pattern = '.rds'), '\'')
  gt3 <- paste(gt, '&&', gt2)
  llply(gt3, function(y) {
    system(y)
    cat(y, '\n')
  })
})



