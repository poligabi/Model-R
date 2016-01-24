install.packages('maps')
install.packages('rgdal')
install.packages('raster')
install.packages('dismo')
install.packages('rjson')
install.packages('devtools')
devtools::install_github("rstudio/leaflet")
install.packages("shiny")
install.packages('shinythemes')
install.packages('html_vignette')
install.packages("digest")
install.packages("rgbif")
install.packages('shinydashboard')
install.packages("randomForest")
install.packages('kernlab')
install.packages('rJava')
devtools::install_github("rstudio/vignette")
install.packages("leaflet")

projeto<-'Bioclim'
projeto
caminho = paste0("./www/projeto/",projeto,"/models/")
evall<- list.files(path = paste0(caminho),pattern=paste0("."),full.names = T)
caminho
evall<- list.files(path = paste0("C:/Users/Rafael/Documents/Modelagem/R/www/projeto/Bioclim/models/"),pattern=paste0("evaluate_.txt"),full.names = T)
evall
