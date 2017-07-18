#source("/opt/data-science/projects/databol/teste_modelo_cartola/src/init_cartola_2017.R")

setwd("/opt/data-science/projects/databol/teste_modelo_cartola/")

if( !require(ProjectTemplate) ) {
  if( !require(devtools) ) {
    install.packages("devtools")
    library(devtools)
  }
  install_github("johnmyleswhite/ProjectTemplate")
  library(ProjectTemplate)
}

# Carregar projeto
load.project( translate.dcf("./config/cartola_2017.dcf") )

# Setar parametros da configuracao
logger$logfile <- "./logs/cartola_2017.log"
config$data_loading <- TRUE
config$munging <- TRUE

if(config$load_libraries) {
  list.files("./lib", pattern = "*.R$", full.names=TRUE) %>%
    sort() %>%
    sapply(., 
           source, 
           .GlobalEnv)
}

if(config$data_loading) {
  list.files("./data", pattern = ".R$", full.names=TRUE) %>%
    sapply(., source)
  
  list.files("./data", pattern = ".RData$", full.names=TRUE) %>%
    sapply(., load, .GlobalEnv)
}

if(config$munging) {
  list.files("./munge", pattern = "*.R$", full.names=TRUE) %>%
    sort() %>%
    sapply(., source)
}

# nohup Rscript /opt/data-science/projects/databol/teste_modelo_cartola/src/init_cartola_2017.R &