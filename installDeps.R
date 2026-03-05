
install.packages("pak")


pak::cache_clean()
pak::meta_clean()
pak::pkg_install(c('caret', 'car', 'MLmetrics', 
                  'rpart', 'rpart.plot', 'tidyverse', 
                  'here', 'pander', 'precrec',
                  'ggrepel', 'performance'))

install.packages('rmarkdown')
