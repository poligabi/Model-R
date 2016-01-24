library(shinydashboard)
library(leaflet)
#library(dplyr)
#library(curl) # make the jsonlite suggested dependency explicit


# server.R
rm(list = ls(all = TRUE))
rm(list = setdiff(ls(), lsf.str()))

ARQUIVO_SAIDA <- 'Script.R'
t <- 7
ext1 <- -90 
ext2 <- -33
ext3 <- -32
ext4 <- 23

ext12 <- -90 
ext22 <- -33
ext32 <- -32
ext42 <- 23

arquivo <- list()
arquivo2 <- list()

ETAPA <- 0

getOcorrencia <-
  function(pTaxon){
    library("rjson")
    
    json_file <- paste("http://aplicacoes.jbrj.gov.br/jabot/v2/ws/server.php?coordenada=S&taxon=",pTaxon,sep="")
    json_data <- fromJSON(file=json_file)
    #str(json_data)
    #y = as.data.frame(do.call(rbind, json_data))
    
    final_data <- do.call(rbind, json_data)
    # Then write it to a flat csv file
    write.csv(final_data, "final_data.csv")
    #head(final_data)
    y2 <- final_data[,c("taxoncompleto", "longitude", "latitude")]
    
    #    y2 <- cbind(y2[,1],
    #                  as.numeric(y2[ ,2]),
    #                  as.numeric(y2[ ,3])
    #    )  
    y2 <- cbind(
      as.numeric(y2[ ,2]),
      as.numeric(y2[ ,3])
    )  
    
    #colnames(y2) <- c("especie","longitude", "latitude")
    colnames(y2) <- c("Longitude", "Latitude")
    #    y2 <- y[,c("taxoncompleto", "longitude", "latitude")] 
    y2 <- data.frame(y2)
    return (y2) 
  }

options(shiny.maxRequestSize = 100*1024^2)


# 1=South, 2=East, 3=West, 4=North
dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

function(input, output, session) {
  rm(especie)
  library(maps)
  library(rgdal)
  library(raster)
  library(dismo)
  library(rgbif)
  library('XML')
  library('leaflet')
  #library('ncdf')


  
  dismo.mod <- function(sp,
                        occs=spp.filt,
                        var=expl,
                        var2=expl2,
                        maxent=F,
                        Bioclim=F,
                        GLM=F,
                        RF=F,
                        SVM=F,
                        Mahal=F,
                        part=3,
                        numpontos = 500,
                        seed=123,
                        write.cont=T, # escreve modelos cont?nuos
                        bin=T, # faz modelos bin?rios
                        write.bin=T, # escreve modelos bin?rios
                        mult=T, # faz modelos multiplicados(bin*con)
                        write.mult=T, # escreve modelos multiplicados
                        future.model=F, # faz modelos futuros
                        future.raster=newdata, # vari?veis futuras
                        write.future=F, # escreve modelos futuros
                        write.projecao=F) 
  
  
{
    
    #if ((maxent=='T') || (Bioclim=='T') || (GLM=='T') || (RF=='T') || (SVM=='T') || (Mahal=='T'))
    #{
    ## Carregando bibliotecas
    library(dismo)
    library(randomForest)
    library(kernlab)
    library(XML)
    library(raster)
    library(rgdal)
    #library(rJava)
    library(maps)
  
    
    
    
    print(date())
    
    cat(paste("Modeling",sp,"...",'\n'))
    #extrai as coordenadas de cada especie 
    #coord <- occs[occs$sp==sp,c('Longitude','Latitude')]
    coord <- especie
    n <- nrow(coord)
    ## Extraindo os valores das vari?veis onde h? pontos de registros
    
    presvals<- extract(var,coord)
    
    ## Seeting seed para sempre criar os mesmos pontos aleat?rios
    set.seed(seed)
    
    ## Gerando as pseudoaus?ncias aleatoriamente
    backgr <- randomPoints(var, numpontos)
    
    ## Determina os nomes da colunas de coordenadas para os pontos de background
    colnames(backgr) = c('Longitude', 'Latitude')
    
    ## Extraindo os valores das vari?veis onde h? pseudoaus?ncias
    absvals <- extract(var, backgr)
    
    ## Cria um vetor contendo algarismo "1" e "0" correspondendo ao n?mero de registros presen?as e aus?ncias respectivamente.
    pre_abs <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
    
    ## N?mero de parti??es
    if (n<10) part<-n else part <- part
    
    ## Setting seed para distribuir as presen?as sempre para as mesmas parti??es
    set.seed(seed)
    
    ## Separando as presen?as e pseudoaus?ncias nos grupos das parti??es
    group_pre <- kfold(coord,part)
    set.seed(seed)
    group_abs <- kfold(backgr,part)
    
    # Cosntruindo o data.frame com todos os dados
    append_1 <- append(group_pre,group_abs)
    cbind_1 <- cbind(coord,presvals)
    cbind_2 <- cbind(backgr,absvals)
    rbind_1 <- rbind(cbind_1,cbind_2)
    sdmdata <- data.frame(cbind(append_1,pre_abs,rbind_1))
    colnames(sdmdata)[1] <- "group"
    
    
    for (i in unique(group_pre)){
      
      progress$set(message = paste("Processando a modelagem",i), value = 0)
      # Separar o sdmdata entre teste e treino
      sdmdata_train <- subset(sdmdata,group!=i)
      sdmdata_teste <- subset(sdmdata,group ==i)
      
      # Preparando sdmdata train para fazer o modelo
      envtrain <- subset(sdmdata_train,select= c(-group,-Longitude,-Latitude))
      
      # Preparando sdmdata test para avaliar modelos que n?o usam s? presen?a
      envtest_pre <- subset(sdmdata_teste,pre_abs==1,select= c(-group,-Latitude,-Latitude,-pre_abs))
      envtest_abs <- subset(sdmdata_teste,pre_abs==0,select= c(-group,-Latitude,-Latitude,-pre_abs))
      
      ### Separando os dados (apenas coordenadas) para fazer BioClim, Maxent e Mahalanobis que usam s? presen?a
      coord_pres_train <- subset(sdmdata_train,pre_abs==1,select= c(Longitude,Latitude))
      coord_pres_teste <- subset(sdmdata_teste,pre_abs==1,select= c(Longitude,Latitude))
      coord_abs_train <- subset(sdmdata_train,pre_abs==0,select= c(Longitude,Latitude))
      coord_abs_teste <- subset(sdmdata_teste,pre_abs==0,select= c(Longitude,Latitude))
      
      
      ### FAZ OS MODELOS
      cat(paste("Modeling...",sp,"Partition",i,'\n'))
      
      if (Bioclim==T){
        cat(paste("Bioclim",'\n'))
        # Constr?i o modelo no espa?o ambiental
        bc <- bioclim (var, coord_pres_train)
        # Valida??o da performance
        ebc <<- evaluate (coord_pres_teste,coord_abs_teste,bc,var)
        # C?lculo do TSS
        bcTSS <- max(ebc@TPR + ebc@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tbc <- threshold (ebc,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo 
        bc_cont <- predict (var,bc,progress='text')
        bc_cont_proj <- predict (var2,bc,progress='text')
        
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        bc_bin <- bc_cont>tbc
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        bc_mult <- bc_bin*bc_cont
        # Normaliza o modelo mult
        bc_mult <- bc_mult/maxValue(bc_mult)
        # Faz os modelos futuros
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          bc_future <- predict(future.raster,bc,progress='text') 
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {bc_future_bin <- bc_future > tbc} 
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            bc_future_mult <- bc_future_bin * bc_future
            # Normaliza o modelo mult
            bc_future_mult <- bc_future_mult/maxValue(bc_future_mult)} 
        } # Fecha o modelo futuro
      } # Fecha o algoritmo Bioclim
      
      if (maxent==T){
        cat(paste("Maxent",'\n'))
        # Constr?i o modelo no espa?o ambiental
        mx <- maxent (var, coord_pres_train)
        # Valida??o da performance
        emx <- evaluate (coord_pres_teste,coord_abs_teste,mx,var)
        # C?lculo do TSS
        mxTSS <- max(emx@TPR + emx@TNR)-1 
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tmx <- threshold (emx,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo 
        mx_cont <- predict (var,mx,progress='text')
        mx_cont_proj <- predict (var2,mx,progress='text')
        
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        mx_bin <- mx_cont>tmx
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        mx_mult <- mx_bin*mx_cont 
        # Normaliza o modelo mult
        mx_mult <- mx_mult/maxValue(mx_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          mx_future <- predict(future.raster,mx,progress='text') 
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {mx_future_bin <- mx_future > tmx} 
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            mx_future_mult <- mx_future_bin * mx_future
            # Normaliza o modelo mult
            mx_future_mult <- mx_future_mult/maxValue(mx_future_mult)} 
        } # Fecha o modelo futuro
      } # Fecha o algoritmo Maxent
      
      if (GLM==T){
        cat(paste("GLM",'\n'))
        # Constr?i o modelo no espa?o ambiental
        mglm <- glm(pre_abs~.,data=envtrain)
        # Valida??o da performance
        eglm <- evaluate(envtest_pre,envtest_abs,mglm)
        # C?lculo do TSS
        glmTSS <- max(eglm@TPR + eglm@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tglm <- threshold (eglm,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo 
        glm_cont <- predict (var,mglm,progress='text')
        glm_cont_proj <- predict (var2,mglm,progress='text')
        
        #plot(glm_cont)
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        glm_bin <- glm_cont>tglm
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        glm_mult <- glm_bin*glm_cont 
        # Normaliza o modelo mult
        glm_mult <- glm_mult/maxValue(glm_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          glm_future <- predict(future.raster,mglm,progress='text') 
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {glm_future_bin <- glm_future > tglm} 
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            glm_future_mult <- glm_future_bin * glm_future
            # Normaliza o modelo mult
            glm_future_mult <- glm_future_mult/maxValue(glm_future_mult)}  
        } # Fecha o modelo futuro
      } # Fecha o algoritmo GLM
      
      if (RF==T){
        cat(paste("RF",'\n'))
        # Constr?i o modelo no espa?o ambiental
        ##rf1 <- randomForest (pre_abs~.,data=envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
        ##envtrain
        ##pre_abs
        rf1 <- randomForest (pre_abs~.,data=envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
        #rf2 <- randomForest (factor(pre_abs) ~ ., data=envtrain) # faz classification e n?o d? mensagem de erro.
        # rf2 tem como output somente modelos bin?rios
        # Valida??o de performance
        erf1 <- evaluate(envtest_pre,envtest_abs, rf1)
        #erf2 <- evaluate(envtest_pre,envtest_abs, rf2)
        # C?lculo do TSS
        rfTSS1 <- max(erf1@TPR + erf1@TNR)-1
        #rfTSS2 <- max(erf2@TPR + erf2@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        trf1 <- threshold (erf1,'spec_sens')
        #trf2 <- threshold (erf2,'spec_sens') # tbm d? mensagem de erro
        # Projeta no espa?o geogr?fico o modelo cont?nuo 
        rf1_cont <- predict (var,rf1,progress='text')
        rf1_cont_proj <- predict (var2,rf1,progress='text')
        #rf_cont2 <- predict (var,rf2,progress='text') # o cont?nuo fica igual ao bin?rio!
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        rf1_bin <- rf1_cont>trf1
        #rf_bin2 <- rf_cont2>trf2
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        rf1_mult <- rf1_bin*rf1_cont 
        #rf_mult2 <- rf_bin2*rf_cont2
        # Normaliza o modelo mult
        rf1_mult <- rf1_mult/maxValue(rf1_mult)
        
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          rf1_future <- predict(future.raster,rf1,progress='text') 
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {rf1_future_bin <- rf1_future > trf1} 
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            rf1_future_mult <- rf1_future_bin * rf1_future
            # Normaliza o modelo mult
            rf1_future_mult <- rf1_future_mult/maxValue(rf1_future_mult)}  
        } # Fecha o modelo futuro
      } # Fecha o algoritmo RandomForest
      
      if (SVM==T){
        cat(paste("SVM",'\n'))
        # Constr?i o modelo no espa?o ambiental
        msvm <- ksvm(pre_abs~.,data=envtrain)
        # Valida??o da performance
        esvm <- evaluate(envtest_pre,envtest_abs,msvm)
        # C?lculo do TSS
        svmTSS <- max(esvm@TPR + esvm@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tsvm <- threshold (esvm,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo 
        svm_cont <- predict (var,msvm,progress='text')
        svm_cont_proj <- predict (var2,msvm,progress='text')
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        svm_bin <- svm_cont>tsvm
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        svm_mult <- svm_bin*svm_cont
        # Normaliza o modelo mult
        svm_mult <- svm_mult/maxValue(svm_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          svm_future <- predict(future.raster,msvm,progress='text') 
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {svm_future_bin <- svm_future > tsvm} 
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            svm_future_mult <- svm_future_bin * svm_future
            # Normaliza o modelo mult
            svm_future_mult <- svm_future_mult/maxValue(svm_future_mult)}   
        } # Fecha o modelo futuro
      } # Fecha o algoritmo SVM
      
      if (Mahal==T){
        cat(paste("Mahal",'\n'))
        # Checa se o n?mero de registros de presen?a ? maior que o n?mero de vari?veis
        condicao_Mahal<- nrow(coord_pres_train)>length(names(var))
        if (condicao_Mahal==TRUE){
          # Constr?i o modelo no espa?o ambiental
          ma <- mahal (var, coord_pres_train)
          # Valida??o da performance
          ema <- evaluate (coord_pres_teste,coord_abs_teste,ma,var)
          # C?lculo do TSS
          maTSS <- max(ema@TPR + ema@TNR)-1
          # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
          tma <- threshold (ema,'spec_sens')
          # Projeta no espa?o geogr?fico o modelo cont?nuo 
          ma_cont <- predict (var,ma,progress='text')
          ma_cont_proj <- predict (var2,ma,progress='text')
          # Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
          ma_cont_invert <- ma_cont+(-1*minValue(ma_cont))
          # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tma
          ma_bin <- ma_cont>tma
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo invertido
          ma_mult <- ma_bin * ma_cont_invert
          # Normaliza o modelo mult
          ma_mult <- ma_mult/maxValue(ma_mult)
          # Faz os modelos futuros
          if (future.model==T){
            # Projeta o modelo nas vari?veis futuras
            ma_future <- predict(future.raster,ma,progress='text') 
            # Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
            ma_future_invert <- ma_future+(-1*minValue(ma_future))
            # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
            if (bin ==T) {ma_future_bin <- ma_future > tma} 
            # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
            if (mult==T) {
              ma_future_mult <- ma_future_bin * ma_future_invert
              # Normaliza o modelo mult
              ma_future_mult <- ma_future_mult/maxValue(ma_future_mult)} 
          } # Fecha o modelo futuro
        } # Fecha o algoritmo Mahalanobis
        else {cat("Impossíel rodar Mahalanobis para",sp,"registros de presença menor que o número de variáveis ambientais",'\n')}
      }
      
      
      
      ### ESCREVE OS MODELOS
      
      ## Modelos contínuos
      if (write.cont==T){
        cat(paste("Salvando modelos cont?nuos...",sp,i,'\n'))
        
        if(Bioclim==T){
          writeRaster(x=bc_cont,filename=paste0("./www/models/pre_",i,"_bc_con",".tif"),overwrite=T)
          
          png(filename=paste0("./www/jpg/pre_",i,"_bc_con",".jpg"))
          plot(bc_cont,main=paste("BioClim - ",i))
          dev.off()
          
          #dev.copy(jpeg,filename="plot.jpg");
          #dev.off ();
          if (write.projecao==T)
          {
            writeRaster(x=bc_cont_proj,filename=paste0("./www/proj/pre_",i,"_bc_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=bc_future,filename=paste0("./www/futuro/fut_",i,"_bc_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_bc_con",".jpg"))
            plot(bc_future,main=paste("BioClim - Fut ",i))
            dev.off()
          }
        }
        
        if(maxent==T){
          writeRaster(x=mx_cont,filename=paste0("./www/models/pre_",i,"_mx_con",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_mx_con",".jpg"))
          plot(mx_cont,main=paste("Maxent - ",i))
          dev.off()
          
          if (write.projecao==T)
          {
            writeRaster(x=mx_cont_proj,filename=paste0("./www/proj/pre_",i,"_mx_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=mx_future,filename=paste0("./www/futuro/fut_",i,"_mx_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_mx_con",".jpg"))
            plot(mx_future,main=paste("Maxent - Fut ",i))
            dev.off()
          }
        }
        
        if(GLM==T){
          writeRaster(x=glm_cont,filename=paste0("./www/models/pre_",i,"_glm_con",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_glm_con",".jpg"))
          plot(glm_cont,main=paste("GLM - ",i))
          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=glm_cont_proj,filename=paste0("./www/proj/pre_",i,"_glm_con_proj",".tif"),overwrite=T)
          }
          #writeRaster(x=glm_cont,filename=paste0("./models/pre_glm_con_",i,".grd"),overwrite=T)
          
          if(write.future==T) {
            writeRaster(x=glm_future,filename=paste0("./www/futuro/fut_",i,"_glm_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_glm_con",".jpg"))
            plot(glm_future,main=paste("GLM - Fut ",i))
            dev.off()
          }
        }
        
        if(RF==T){
          writeRaster(x=rf1_cont,filename=paste0("./www/models/pre_",i,"_rf_con",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_rf_con",".jpg"))
          plot(rf1_cont,main=paste("RF - ",i))
          dev.off()
          
          if (write.projecao==T)
          {
            writeRaster(x=rf1_cont_proj,filename=paste0("./www/proj/pre_",i,"_rf_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=rf1_future,filename=paste0("./www/futuro/fut_",i,"_rf1_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_rf1_con",".jpg"))
            plot(rf1_future,main=paste("RF - Fut ",i))
            dev.off()
          }
        }
        
        if(SVM==T){
          writeRaster(x=svm_cont,filename=paste0("./www/models/pre_",i,"_svm_con",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_svm_con",".jpg"))
          plot(svm_cont,main=paste("SVM - ",i))
          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=svm_cont_proj,filename=paste0("./www/proj/pre_",i,"_svm_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=svm_future,filename=paste0("./www/futuro/fut_",i,"_svm_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_svm_con",".jpg"))
            plot(svm_future,main=paste("SVM - Fut ",i))
            dev.off()
          }
        }
        
        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_cont,filename=paste0("./www/models/pre_",i,"_ma_con",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_ma_con",".jpg"))
          plot(ma_cont,main=paste("Mahalanobis - ",i))
          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=ma_cont_proj,filename=paste0("./www/proj/pre_",i,"_ma_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=ma_future,filename=paste0("./www/futuro/fut_",i,"_ma_con",".tif"),overwrite=T)
            png(filename=paste0("./www/jpg/fut_",i,"_ma_con",".jpg"))
            plot(ma_future,main=paste("Mahalanobis - Fut ",i))
            dev.off()
          }
        }
        
      } # Fecha escrita de modelos cont?nuos
      
      
      ## Modelos bin?rios
      if (write.bin==T){
        cat(paste("Salvando modelos binários...",sp,i,'\n'))
        
        if(Bioclim==T){
          writeRaster(x=bc_bin,filename=paste0("./www/models/pre_",i,"_bc_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_bc_bin",".jpg"))
          plot(bc_bin,main=paste("Bioclim - Binário ",i))
          dev.off()
          
          if(write.future==T) { writeRaster(x=bc_future_bin,filename=paste0("./www/futuro/fut_",i,"_bc_bin",".tif"),overwrite=T)}}
        
        if(maxent==T){
          writeRaster(x=mx_bin,filename=paste0("./www/models/pre_",i,"_mx_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_mx_bin",".jpg"))
          plot(mx_bin,main=paste("Maxent - Binário ",i))
          dev.off()
          
          if(write.future==T) { writeRaster(x=mx_future_bin,filename=paste0("./www/futuro/fut_",i,"_mx_bin",".tif"),overwrite=T)}}
        
        if(GLM==T){
          writeRaster(x=glm_bin,filename=paste0("./www/models/pre_",i,"_glm_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_glm_bin",".jpg"))
          plot(glm_bin,main=paste("GLM - Binário ",i))
          dev.off()
          
          if(write.future==T) { writeRaster(x=glm_future_bin,filename=paste0("./www/futuro/fut_",i,"_glm_bin",".tif"),overwrite=T)}}       
        
        
        if(RF==T){
          writeRaster(x=rf1_bin,filename=paste0("./www/models/pre_",i,"_rf_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_rf_bin",".jpg"))
          plot(rf1_bin,main=paste("RF - Binário ",i))
          dev.off()
          if(write.future==T) { writeRaster(x=rf1_future_bin,filename=paste0("./www/futuro/fut_",i,"_rf_bin",".tif"),overwrite=T)}}
        
        if(SVM==T){
          writeRaster(x=svm_bin,filename=paste0("./www/models/pre_",i,"_svm_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_svm_bin",".jpg"))
          plot(svm_bin,main=paste("SVM - Binário ",i))
          dev.off()
          if(write.future==T) { writeRaster(x=svm_future_bin,filename=paste0("./www/futuro/fut_",i,"_svm_bin",".tif"),overwrite=T)}}
        
        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_bin,filename=paste0("./www/models/pre_",i,"_ma_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/jpg/pre_",i,"_ma_bin",".jpg"))
          plot(ma_bin,main=paste("Mahalanobis - Binário ",i))
          dev.off()
          if(write.future==T) { writeRaster(x=ma_future_bin,filename=paste0("./www/futuro/fut_",i,"_ma_bin",".tif"),overwrite=T)}}
        
        
      } # Fecha escrita de modelos bin?rios   
      
      
      ## Modelos multiplicados
      if (write.mult==T){
        cat(paste("Salvando modelos multiplicados...",sp,i,'\n'))
        
        if(Bioclim==T){
          writeRaster(x=bc_mult,filename=paste0("./www/models/pre_",i,"_bc_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=bc_future_mult,filename=paste0("./www/futuro/fut_",i,"_bc_mult",".tif"),overwrite=T)}}
        
        if(maxent==T){
          writeRaster(x=mx_mult,filename=paste0("./www/models/pre_",i,"_mx_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=mx_future_mult,filename=paste0("./www/futuro/fut_",i,"_mx_mult",".tif"),overwrite=T)}}
        
        if(GLM==T){
          writeRaster(x=glm_mult,filename=paste0("./www/models/pre_",i,"_glm_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=glm_future_mult,filename=paste0("./www/futuro/fut_",i,"_glm_mult",".tif"),overwrite=T)}}
        
        if(RF==T){
          writeRaster(x=rf1_mult,filename=paste0("./www/models/pre_",i,"_rf_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=rf1_future_mult,filename=paste0("./www/futuro/fut_",i,"_rf_mult",".tif"),overwrite=T)}}
        
        if(SVM==T){
          writeRaster(x=svm_mult,filename=paste0("./www/models/pre_",i,"_svm_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=svm_future_mult,filename=paste0("./www/futuro/fut_",i,"_svm_mult",".tif"),overwrite=T)}}
        
        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_mult,filename=paste0("./www/models/pre_",i,"_ma_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=ma_future_mult,filename=paste0("./www/models/fut_",i,"_ma_mult",".tif"),overwrite=T)}}
      } # Fecha escrita de modelos multiplicados
      
      
      
      ### SALVA ARQUIVOS DE VALIDA??O DE PERFORMANCE
      ##
      cat(paste("Salavando o arquivo de valida??o...",sp,i,'\n'))
      sink(file=paste0("./www/models/evaluate_",sp,".txt"),split=T,append=T)
      if(Bioclim==T){
        print(paste(sp,sp,i,"BioClim",round(ebc@auc,3),round(bcTSS,3),round(tbc,3),round(threshold(ebc)$kappa,3),round(threshold(ebc)$equal_sens_spec,3),round(threshold(ebc)$no_omission,3),round(threshold(ebc)$prevalence,3),round(threshold(ebc)$sensitivity,3),sep=","))}
      if(maxent==T){
        print(paste(sp,sp,i,"maxent",round(emx@auc,3),round(mxTSS,3),round(tmx,3),round(threshold(emx)$kappa,3),round(threshold(emx)$equal_sens_spec,3),round(threshold(emx)$no_omission,3),round(threshold(emx)$prevalence,3),round(threshold(emx)$sensitivity,3),sep=","))}
      if(GLM==T){
        print(paste(sp,sp,i,"GLM",round(eglm@auc,3),round(glmTSS,3),round(tglm,3),round(threshold(eglm)$kappa,3),round(threshold(eglm)$equal_sens_spec,3),round(threshold(eglm)$no_omission,3),round(threshold(eglm)$prevalence,3),round(threshold(eglm)$sensitivity,3),sep=","))}
      if(RF==T){
        print(paste(sp,sp,i,"RF",round(erf1@auc,3),round(rfTSS1,3),round(trf1,3),round(threshold(erf1)$kappa,3),round(threshold(erf1)$equal_sens_spec,3),round(threshold(erf1)$no_omission,3),round(threshold(erf1)$prevalence,3),round(threshold(erf1)$sensitivity,3),sep=","))}
      if(SVM==T){
        print(paste(sp,sp,i,"SVM",round(esvm@auc,3),round(svmTSS,3),round(tsvm,3),round(threshold(esvm)$kappa,3),round(threshold(esvm)$equal_sens_spec,3),round(threshold(esvm)$no_omission,3),round(threshold(esvm)$prevalence,3),round(threshold(esvm)$sensitivity,3),sep=","))}
      if(Mahal==T && condicao_Mahal==TRUE){
        print(paste(sp,sp,i,"Mahal",round(ema@auc,3),round(maTSS,3),round(tma,3),round(threshold(ema)$kappa,3),round(threshold(ema)$equal_sens_spec,3),round(threshold(ema)$no_omission,3),round(threshold(ema)$prevalence,3),round(threshold(ema)$sensitivity,3),sep=","))}
      sink()
      
      cat(paste("Salvando arquivos de validação para todas as espécies...",sp,i,'\n'))
      sink(file="./www/models/evaluate_ALL_models.txt",split=T,append=T)
      if(Bioclim==T){
        print(paste(sp,sp,i,"BioClim",round(ebc@auc,3),round(bcTSS,3),round(tbc,3),round(threshold(ebc)$kappa,3),round(threshold(ebc)$equal_sens_spec,3),round(threshold(ebc)$no_omission,3),round(threshold(ebc)$prevalence,3),round(threshold(ebc)$sensitivity,3),sep=","))}
      if(maxent==T){
        print(paste(sp,sp,i,"maxent",round(emx@auc,3),round(mxTSS,3),round(tmx,3),round(threshold(emx)$kappa,3),round(threshold(emx)$equal_sens_spec,3),round(threshold(emx)$no_omission,3),round(threshold(emx)$prevalence,3),round(threshold(emx)$sensitivity,3),sep=","))}
      if(GLM==T){
        print(paste(sp,sp,i,"GLM",round(eglm@auc,3),round(glmTSS,3),round(tglm,3),round(threshold(eglm)$kappa,3),round(threshold(eglm)$equal_sens_spec,3),round(threshold(eglm)$no_omission,3),round(threshold(eglm)$prevalence,3),round(threshold(eglm)$sensitivity,3),sep=","))}
      if(RF==T){
        print(paste(sp,sp,i,"RF",round(erf1@auc,3),round(rfTSS1,3),round(trf1,3),round(threshold(erf1)$kappa,3),round(threshold(erf1)$equal_sens_spec,3),round(threshold(erf1)$no_omission,3),round(threshold(erf1)$prevalence,3),round(threshold(erf1)$sensitivity,3),sep=","))}
      if(SVM==T){
        print(paste(sp,sp,i,"SVM",round(esvm@auc,3),round(svmTSS,3),round(tsvm,3),round(threshold(esvm)$kappa,3),round(threshold(esvm)$equal_sens_spec,3),round(threshold(esvm)$no_omission,3),round(threshold(esvm)$prevalence,3),round(threshold(esvm)$sensitivity,3),sep=","))}
      if(Mahal==T && condicao_Mahal==TRUE){
        print(paste(sp,sp,i,"Mahal",round(ema@auc,3),round(maTSS,3),round(tma,3),round(threshold(ema)$kappa,3),round(threshold(ema)$equal_sens_spec,3),round(threshold(ema)$no_omission,3),round(threshold(ema)$prevalence,3),round(threshold(ema)$sensitivity,3),sep=","))}
      
      sink()
      
      # The sinked files are re-read and tranformed into a proper data frame...
      stats <- read.delim(file="./www/models/evaluate_ALL_models.txt",header=F,sep=",",quote="",col.names=c("id","sp","partition","algorithm","AUC","TSS","TSSth", "Kappa","Equal_sens_spec","No_omission","Prevalence","Sensitivity" ))
      stats$Sensitivity<-as.numeric(sub(pattern="\"","",stats$Sensitivity))
      stats2 <- stats[order(stats$sp,stats$algorithm,stats$partition),-1]
      write.table(stats2,"./www/models/statsALL.txt")
      output$dbgridresultado <- renderDataTable({
        stats2  
      }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10)
      )  
    } # Fecha o for loop
    cat(c(date(),"====FIM====",'\n','\n'))
   # }
} # Fecha a fun??o dismo.mod
  

##############################
# INICIO FUNÇÃO MODELAGEM
##############################

    modelagem <- function() ({
    
    library(raster)
    numpontos = input$edtnumpontos
    numparticoes <- input$edtnumgrupo
    numcolunas <- 2
    if ((input$BIOCLIM==TRUE) && (input$BIOCLIM==TRUE))
    {
      numlinhas<-2*numparticoes
    }
    else
    {
      numlinhas<-1*numparticoes
    }
    par(mfrow=c(numlinhas,numcolunas),mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
    
    
    ########################################
    ## INICIO UTILIZAÇÃO FUNÇÃO DISMO.MOD ##
    ########################################
    
    
    ## LIMPANDO OS RESULTADOS ANTERIORES
    
    lista <- list.files("www/models/",full.names=T,pattern=paste0("."))
    if (length(lista>0))
    {
      file.remove("www/models/",lista)
    }
    
    lista <- list.files("www/final/",full.names=T,pattern=paste0("."))
    if (length(lista>0))
    {
      file.remove("www/final/",lista)
    }

    lista <- list.files("www/proj/",full.names=T,pattern=paste0("."))
    if (length(lista>0))
    {
      file.remove("proj/",lista)
    }
    
    lista <- list.files("www/futuro/",full.names=T,pattern=paste0("."))
    if (length(lista>0))
    {
      file.remove("www/futuro/",lista)
    }

    lista <- list.files("www/jpg/",full.names=T,pattern=paste0(".jpg"))
    if (length(lista>0))
    {
      file.remove("www/jpg/",lista)
    }    
    
    ## RODANDO A FUNÇÃO DISMO.MOD modificada
    futuro = FALSE
    if (input$periodo != 'current')
    {
      futuro = T
    }
    if (input$periodobiooracle != 'current')
    {
      futuro = T
    }
    write.projecao = F
    if (input$PROJETAR==T)
    {
      write.projecao = T
    }
    
    dismo.mod("",especie,pred_nf,pred_nf2,input$MAXENT,input$BIOCLIM,input$GLM,input$RF,input$SVM,input$MAHALANOBIS,numparticoes,numpontos,123,T,input$BINARIO,T,F,F,futuro,pred_nffuturo,futuro,write.projecao)
    
    progress$set(message = paste("Gerando script"), value = 0)
    
    # script gerado pelo sistema
    
    cat("library(maps)",file=ARQUIVO_SAIDA,sep="\n")
    cat("library(dismo)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(rgdal)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(raster)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(rgbif)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(XML)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("# INFORME O CAMINHO DA APLICACAO",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("path <- \"\"",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("file <- paste(path,\"csv/dados.csv\", sep=\"\")",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("especie <- read.csv(file)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("head(especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("especie <- especie[,1:2]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("head(especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("arquivo <- list()",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("# ADICIONE OS RASTERS",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    if (length(arquivo2>0))
    {
      for (i in 1:length(arquivo2)){
        cat(paste0("arquivo <- c(arquivo,paste(path,'ex/current/",input$resolucao,arquivo2[[i]],"',sep=''))"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      }
    }   
    #cat(paste0("arquivo <- c(arquivo,paste(path,'ex/current/",input$resolucao,"/bio19.bil',sep=''))"),file=ARQUIVO_SAIDA,append=TRUE)
    #cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("arquivo",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("predictors <- stack(arquivo)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("pred_nf <- predictors",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    #cat(paste0("ext <- extent(",input$edtextend1,",",input$edtextend2,",",input$edtextend3,",",input$edtextend4,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("ext <- extent(",ext1,",",ext3,",",ext2,",",ext4,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("pred_nf <- crop(pred_nf, ext)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("plot(pred_nf, 1)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("points(especie, bg='red', cex=1,pch=21)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("presvals <- extract(pred_nf, especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)    
    
    
    # SCRIPT SISTEMA
    
    cat(paste0("group <- kfold(especie,",numparticoes,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat(paste0("backg <- randomPoints(pred_nf, n=",input$edtnumpontos,",", "ext=ext,"," extf = ",input$edtextf,")"),file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("colnames(backg) = c( 'lon' ,  'lat' )",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat(paste0("group <- kfold(backg, ",numparticoes,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("for (i in unique(group)){",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("pres_train <- especie[group != i, ]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("pres_test <- especie[group == i, ]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("backg_train <- backg[group != i, ]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("backg_test <- backg[group == i, ]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("bc <- bioclim(pred_nf, pres_train)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("e <- evaluate(pres_test, backg_test, bc, pred_nf)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("pb <- predict(pred_nf, bc, ext=ext, progress='')",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("threshold <- e@t[which.max(e@TPR + e@TNR)]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("plot(pb, main=paste('Bioclim - : ',i,'\n','AUC =', round(e@auc,2),'-','TSS =',round(max(e@TPR + e@TNR)-1,2)))",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("plot(pb > threshold, main=paste('BIOCLIM pres/absence - ',i,'\n','TS =', round(threshold,2),''))",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    cat("}",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    
    #    sink()
    
    output$ui <- renderUI({
      lista_jpg <- list.files("www/jpg",full.names=F,pattern=paste0(".jpg"))
      
      lapply(1:length(order(lista_jpg)), function(i) {
        tags$a(href=paste0('jpg/',lista_jpg[i]), tags$img(src = paste0('jpg/',lista_jpg[i]), height = "200px"), target="_blank")
        
#        htmloutput(paste0(pre_1_bc_con.jpg))
        #verbatimTextOutput(paste0("edtresultado_teste",i))
      })
    })

  output$uiarquivosdados <- renderUI({
    
    lista_csv <- list.files("www/csv",full.names=F,pattern=paste0(".csv"))
  
    lapply(1:length(lista_csv), function(i) {
      tags$div(
      tags$a(href=paste0('csv/',lista_csv[i]), paste0(lista_csv[i]))
      )
    })
  })

output$uiarquivosmodelos <- renderUI({
  
  lista_models <- list.files("www/models",full.names=F,pattern=paste0("pre_"))
  
      lapply(1:length(sort(lista_models)), function(i) {
        tags$div(
        tags$a(href=paste0('models/',lista_models[i]), paste0(lista_models[i]))
        )
      })
})

output$uiarquivosensemble <- renderUI({
  
      lista_final <- list.files("www/final",full.names=F,pattern=paste0(".tif"))
  
      lapply(1:length(sort(lista_final)), function(i) {
        tags$div(
          tags$a(href=paste0('final/',lista_final[i]), paste0(lista_final[i]))
        )
      })    
})


output$uiarquivosprojecao <- renderUI({
  
      lista_proj <- list.files("www/proj",full.names=F,pattern=paste0(".tif"))
  
      lapply(1:length(sort(lista_proj)), function(i) {
        tags$div(
          tags$a(href=paste0('proj/',lista_proj[i]), paste0(lista_proj[i]))
        )
      })    
})

output$uiarquivosprojecaofuturo <- renderUI({
  
      lista_futuro <- list.files("www/futuro",full.names=F,pattern=paste0(".tif"))
  
      lapply(1:length(sort(lista_futuro)), function(i) {
        tags$div(
          tags$a(href=paste0('futuro/',lista_futuro[i]), paste0(lista_futuro[i]))
        )
      })    
})


#    lista_futuro <- list.files("www/futuro",full.names=F,pattern=paste0(".tif"))
    
#    lapply(1:length(lista_futuro), function(i) {
#      tags$div(
#        tags$a(href=paste0('futuro/',lista_futuro[i]), paste0(lista_futuro[i]))
#      )
#    })    
    
    
#  })


#    if (input$MAHALANOBIS==TRUE)
#    {
#      output$uimahalanobis <- renderUI({
#        lapply(1:input$edtnumgrupo, function(i) {
#          verbatimTextOutput(paste0("edtresultado_mahalanobis",i))
#        })
#      })
#    }
    
    #ext <<- extent(input$edtextend1, input$edtextend3, input$edtextend2, input$edtextend4)
     
    progress$set(message = "Salvando dados...", value = 0)
    
    write.csv(especie, file = "www/csv/dados.csv")
      
    # EMSEMBLE
    #
    output$plotesemble <- renderPlot({
      input$btnModelar
      
      if (ETAPA>1)
      {
      isolate({
      
        par(mfrow=c(2,3))
        
        conta_alg = 0;
        algoritmos = ''
        if (input$GLM==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'GLM')
          glm_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("glm_con.tif"))
          glm_raster<-stack(glm_arquivos)
          ensemble.glm<-mean(glm_raster,glm_raster)
          writeRaster(ensemble.glm,filename=paste0("www/final/","glm_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.glm, main=paste("(GLM - Ensemble)"))
          png(filename=paste0("./www/jpg/glm_ensemble",".jpg"))
          plot(ensemble.glm,main=paste("GLM - Ensemble "))
          dev.off()
        }
        if (input$RF==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'RF')
          rf_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("rf_con.tif"))
          rf_raster<-stack(rf_arquivos)
          ensemble.rf<-mean(rf_raster,rf_raster)
          writeRaster(ensemble.rf,filename=paste0("www/final/","rf_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.rf, main=paste("(RF - Ensemble)"))
          png(filename=paste0("./www/jpg/rf_ensemble",".jpg"))
          plot(ensemble.rf,main=paste("RF - Ensemble"))
          dev.off()
        }
        if (input$BIOCLIM==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'Bioclim')
          bioclim_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("bc_con.tif"))
          bc_raster<-stack(bioclim_arquivos)
          ensemble.bc<-mean(bc_raster,bc_raster)
          writeRaster(ensemble.bc,filename=paste0("www/final/","bc_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.bc, main=paste("BIOCLIM - Ensemble"))
          png(filename=paste0("./www/jpg/bc_ensemble",".jpg"))
          plot(ensemble.bc,main=paste("BIOCLIM - Ensemble"))
          dev.off()
        }
        if (input$MAHALANOBIS==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'Mahalanobis')
          maha_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("ma_con.tif"))
          ma_raster<-stack(maha_arquivos)
          ensemble.ma<-mean(ma_raster,ma_raster)
          writeRaster(ensemble.ma,filename=paste0("www/final/","ma_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.ma, main=paste("(MAHALANOBIS - Ensemble)"))
          png(filename=paste0("./www/jpg/ma_ensemble",".jpg"))
          plot(ensemble.ma,main=paste("MAHALANOBIS - Ensemble"))
          dev.off()
        }

        if (input$SVM==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'SVM')
          svm_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("svm_con.tif"))
          svm_raster<-stack(svm_arquivos)
          ensemble.svm<-mean(svm_raster,svm_raster)
          writeRaster(ensemble.svm,filename=paste0("www/final/","svm_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.svm, main=paste("(SVM - Ensemble)"))
          png(filename=paste0("./www/jpg/svm_ensemble",".jpg"))
          plot(ensemble.svm,main=paste("SVM - Ensemble"))
          dev.off()
        }        

        if (input$MAXENT==TRUE)
        {
          conta_alg = conta_alg + 1
          algoritmos <- paste(algoritmos,'Maxent')
          mx_arquivos <- list.files(paste0("./www/models/"),full.names=T,pattern=paste0("mx_con.tif"))
          mx_raster<-stack(mx_arquivos)
          ensemble.mx<-mean(mx_raster,mx_raster)
          writeRaster(ensemble.mx,filename=paste0("www/final/","mx_ensemble.tif"), format='GTiff', overwrite=T)
          plot( ensemble.mx, main=paste("(MAXENT - Ensemble)"))
          png(filename=paste0("./www/jpg/mx_ensemble",".jpg"))
          plot(ensemble.mx,main=paste("MAXENT - Ensemble"))
          dev.off()
        }
        
        ensemble_arquivos <- list.files(paste0("./www/final/"),full.names=T,pattern=paste0("ensemble.tif"))
        
        if (conta_alg>1)
        {
          ensemble_raster<-stack(ensemble_arquivos)
          ensemble.geral<-mean(ensemble_raster,ensemble_raster)
          plot(ensemble.geral, main=paste("Ensemble ",algoritmos))
          points(especie, bg='red', cex=1,pch=21)
          writeRaster(ensemble.geral,filename=paste0("www/final/","ensemble_geral.tif"), format='GTiff', overwrite=T)
        
          png(filename=paste0("./www/jpg/ensemble_geral",".jpg"))
          plot(ensemble.geral,main=paste("Ensemble ",algoritmos))
          dev.off()
        }

        if (futuro == T)
        {
        ## ENSEMBLE FUTURO        

        ensemble_futuro_arquivos <- list.files(paste0("./www/futuro/"),full.names=T,pattern=paste0("con"))
        ensemble_futuro_raster<-stack(ensemble_futuro_arquivos)
        ensemble_futuro.geral<-mean(ensemble_futuro_raster,ensemble_futuro_raster)
        writeRaster(ensemble_futuro.geral,filename=paste0("www/final/","ensemble_futuro_geral.tif"), format='GTiff', overwrite=T)
        plot(ensemble_futuro.geral, main=paste("Ensemble Futuro",''))
        #points(especie, bg='red', cex=1,pch=21)
        
        png(filename=paste0("./www/jpg/ensemble_futuro",".jpg"))
        plot(ensemble_futuro.geral,main=paste("Ensemble Futuro ",''))
        dev.off()
        ## FIM ENSEMBLE FUTURO        
        }
        
        if (input$PROJETAR==TRUE)
        {
          ensemble_arquivos_projecao <- list.files(paste0("./www/proj/"),full.names=T,pattern=paste0("proj.tif"))
          ensemble_raster_projecao<-stack(ensemble_arquivos_projecao)
          ensemble.projecao<-mean(ensemble_raster_projecao,ensemble_raster_projecao)
          writeRaster(ensemble.projecao,filename=paste0("www/final/","proj_ensemble.tif"), format='GTiff', overwrite=T)
          plot(ensemble.projecao, main=paste("Ensemble Projeção"))
          
          png(filename=paste0("./www/jpg/ensemble_projecao",".jpg"))
          plot(ensemble.projecao,main=paste("Ensemble Projeção"))
          dev.off()
          #points(especie, bg='red', cex=1,pch=21)
        }
        
        
        
      })
      }
    })
    
    
    #output$uiarquivos <- renderUI({
    #  lista <- list.files("final/",full.names=T,pattern=paste0(".grd"))
    #  verbatimTextOutput(paste0(lista))
    #})
    
    
  })  

  ##############################
  # FIM FUNÇÃO MODELAGEM
  ##############################


  #output$uiarquivos <- renderUI({
  #  lista <- list.files("final/",full.names=T,pattern=paste0(".grd"))
  #  verbatimTextOutput(paste0(lista))
  #})
  
  
  #output$text <- renderUI({
  #  HTML(paste(dir(path = "final"),sep = '<br/>'))
  #})  
  
  output$pontosmodelagem <- renderPlot({
    input$btnModelar
    isolate({
    if ((input$MAXENT=='TRUE') || (input$BIOCLIM=='TRUE') || (input$GLM=='TRUE') || (input$RF=='TRUE') || (input$SVM=='TRUE') || (input$GLM=='TRUE'))
    {
      if (ETAPA>1)
      {
        if (exists("especie"))
        {
            progress <<- shiny::Progress$new()
            progress$set(message = "Processando...", value = 0)
            on.exit(progress$close())
            modelagem()
        }
      }
    }
    })
  })  
  
  output$mapapontosextend <- renderLeaflet({
    if (exists("especie"))
    {
      ext1 <<- input$edtextend1
      ext3 <<- input$edtextend3
      ext2 <<- input$edtextend2
      ext4 <<- input$edtextend4
      #setView(lng = -31.5, lat = -13.4, zoom = 1) %>%
      map = leaflet() %>% addTiles %>%  addMarkers(especie[,1], especie[,2]) %>%
        addRectangles(
          input$edtextend1, input$edtextend3, input$edtextend2, input$edtextend4, color = 'red', fill = TRUE, dashArray = '5,5', weight = 3
        )
      map
    }
  })

  output$mapapontosextend2 <- renderLeaflet({
    if (exists("especie"))
    {
      ext12 <<- input$edtextend12
      ext32 <<- input$edtextend32
      ext22 <<- input$edtextend22
      ext42 <<- input$edtextend42
      
      #r <- raster::raster("C:/Users/Rafael/Modelagem_Shiny_v8/ex/current/bio_10m_bil/bio1.bil")
      #points(especie,pch=21,bg="red",cex=1)
      #setView(lng = -31.5, lat = -13.4, zoom = 1) %>%
      map = leaflet() %>% addTiles %>%  
        addRectangles(
          input$edtextend12, input$edtextend32, input$edtextend22, input$edtextend42, color = 'green', fill = TRUE, dashArray = '5,5', weight = 3
        )
      
      #%>% addRaster(r, options = tileOptions(opacity = opacity, noWrap = TRUE, detectRetina = FALSE),
      #          colorFunc = colorFunc)
      map
    }
  })
  
  
  output$mapaabiotico <- renderPlot({
    input$btnAtualizaSelecaoVariaveis
    ETAPA <<- 3
    isolate({

      if (input$tipodadoabiotico=='WORLDCLIN')
      {
        
      path = paste(getwd(),'/ex/current/',input$resolucao,sep='')
      pathfuturo = paste(getwd(),'/ex/',input$periodo,'/',input$resolucao,sep='')
      arquivo = list()
      arquivofuturo = list()
      selecionado = FALSE
      if (input$Bio1==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio1.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio2==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio2.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio3==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio3.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio4==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio4.bil',sep=''))
        selecionado = TRUE
      }
      
      if (input$Bio5==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio5.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio6==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio6.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio7==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio7.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio8==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio8.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio9==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio9.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio10==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio10.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio11==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio11.bil',sep=''))
        selecionado = TRUE
      }
      
      if (input$Bio12==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio12.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio13==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio13.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio14==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio14.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio15==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio15.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio16==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio16.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio17==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio17.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio18==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio18.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio19==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio19.bil',sep=''))
        selecionado = TRUE
      }

      #############################################################################
      # SE FOI ESCOLHIDO ALGUM PERÍODO DIFERENTE DO CURRENT ENTAO PROJETO O FUTURO
      #############################################################################
      if (input$periodo != 'current')
      {
      if (input$Bio1==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio1.bil',sep=''))
      }
      if (input$Bio2==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio2.bil',sep=''))
      }
      if (input$Bio3==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio3.bil',sep=''))
      }
      if (input$Bio4==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio4.bil',sep=''))
      }
      
      if (input$Bio5==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio5.bil',sep=''))
      }
      if (input$Bio6==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio6.bil',sep=''))
      }
      if (input$Bio7==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio7.bil',sep=''))
      }
      if (input$Bio8==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio8.bil',sep=''))
      }
      if (input$Bio9==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio9.bil',sep=''))
      }
      if (input$Bio10==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio10.bil',sep=''))
      }
      if (input$Bio11==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio11.bil',sep=''))
      }
      
      if (input$Bio12==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio12.bil',sep=''))
      }
      if (input$Bio13==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio13.bil',sep=''))
      }
      if (input$Bio14==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio14.bil',sep=''))
      }
      if (input$Bio15==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio15.bil',sep=''))
      }
      if (input$Bio16==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio16.bil',sep=''))
      }
      if (input$Bio17==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio17.bil',sep=''))
      }
      if (input$Bio18==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio18.bil',sep=''))
      }
      if (input$Bio19==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio19.bil',sep=''))
      }
      }  # IF CURRENT 
      }
      
      # BIOORACLE
      if (input$tipodadoabiotico=='BIOORACLE')
      {
      path <- paste(getwd(),'/ex/biooracle/',sep='')
      #
      pathfuturo = paste(getwd(),'/ex/biooracle/',input$cenariobiooracle,'/',input$periodobiooracle,sep='')
      cat(paste("presente: ",path,'\n'))
      cat(paste("futuro: ",pathfuturo,'\n'))
      arquivo = list()
      arquivofuturo = list()
      selecionado = FALSE
      
      if (input$calcite==TRUE) {
        arquivo <- c(arquivo,paste(path,'/calcite.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$nitrate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/nitrate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$ph==TRUE) {
        arquivo <- c(arquivo,paste(path,'/ph.asc',sep=''))
        selecionado = TRUE
      }
      if (input$silicate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/silicate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlorange==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlorange.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$parmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/parmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$phosphate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/phosphate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstrange==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstrange.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$dissox==TRUE) {
        arquivo <- c(arquivo,paste(path,'/dissox.asc',sep=''))
        selecionado = TRUE
      }
      if (input$parmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/parmean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$salinity==TRUE) {
        arquivo <- c(arquivo,paste(path,'/salinity.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmean.asc',sep=''))
        selecionado = TRUE
      }
      
      #############################################################################
      # SE FOI ESCOLHIDO ALGUM PERÍODO DIFERENTE DO CURRENT ENTAO PROJETO O FUTURO
      #############################################################################
      if (input$periodobiooracle != 'current')
      {
        if (input$calcite==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/calcite.asc',sep=''))
        }
        if (input$chlomin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomin.asc',sep=''))
        }
        if (input$cloudmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmean.asc',sep=''))
        }
        if (input$damean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damean.asc',sep=''))
        }
        if (input$nitrate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/nitrate.asc',sep=''))
        }
        if (input$ph==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/ph.asc',sep=''))
        }
        if (input$silicate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/silicate.asc',sep=''))
        }
        if (input$sstmin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmin.asc',sep=''))
        }
        if (input$chlomax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomax.asc',sep=''))
        }
        if (input$chlorange==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlorange.asc',sep=''))
        }
        if (input$cloudmin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmin.asc',sep=''))
        }
        if (input$damin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damin.asc',sep=''))
        }
        if (input$parmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/parmax.asc',sep=''))
        }
        if (input$phosphate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/phosphate.asc',sep=''))
        }
        if (input$sstmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmax.asc',sep=''))
        }
        if (input$sstrange==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstrange.asc',sep=''))
        }
        if (input$chlomean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomean.asc',sep=''))
        }
        if (input$cloudmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmax.asc',sep=''))
        }
        if (input$damax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damax.asc',sep=''))
        }
        if (input$dissox==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/dissox.asc',sep=''))
        }
        if (input$parmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/parmean.asc',sep=''))
        }
        if (input$salinity==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/salinity.asc',sep=''))
        }
        if (input$sstmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmean.asc',sep=''))
        }
      }  # IF CURRENT 
      
      
      
      
      
      } # VAR BIOORACLE
      arquivo2 <<- arquivo
      arquivo3 = arquivo      
      if (length(arquivo>0))
      {
        if ((selecionado == TRUE) && (exists("especie")))
        {
          predictors <- stack(arquivo)
          predictors3 = stack(arquivo3)
          
          if (input$periodo != 'current')
          {
            predictorsfuturo = stack(arquivofuturo)
          }
          if (input$periodobiooracle != 'current')
          {
            predictorsfuturo = stack(arquivofuturo)
          }
          
          ext <<- extent(ext1, ext2, ext3, ext4)
          ext2 = extent(ext12, ext22, ext32, ext42)
          pred_nf <<- crop(predictors, ext)  
          pred_nf2 <<-  crop(predictors3, ext2) 
          
          if (input$periodo != 'current')
          {
            pred_nffuturo <<- crop(predictorsfuturo,ext)
          }
          if (input$periodobiooracle != 'current')
          {
            pred_nffuturo <<- crop(predictorsfuturo,ext)
          }
          presvals <<- extract(pred_nf, especie)
          plot(pred_nf)
          #plot(pred_nf2)
          #plot(pred_nffuturo)
        }
        
      }
      
    }) #FIM ISOLATE
    
  })
  

  
  datasetInput <- reactive({
    if (exists("especie"))
    {
      switch('especie',
             "especie" = especie)
    }
    
  })

  output$dgbriddadosdatacleaning = renderDataTable({
  n <- nrow(especie)
  if (n>0)
  {
    input$btnapagar
    input$btneliminarduplicatas
    input$btnbuscarespecieCSV
    input$btnbuscarespeciejabot
    input$btnbuscarespecie
    
    if (exists("especie"))
    {
      if (input$btneliminarduplicatas > 0)
      {
        progress <- shiny::Progress$new()  
        progress$set(message = "Excluindo duplicatas...", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        especie <<- unique(especie)
      }
      isolate({
        input$edtelemento
        if (input$edtelemento!='0')
        {
          if (input$btnapagar == 0)
            return()
          especie <<- especie[-input$edtelemento,]
        }
        rownames(especie) <- NULL
        especie$id = 1:nrow(especie)
        especie
      })
      especie
    }
  } #IF
  
  }
  , options = list(searching = FALSE,lengthMenu = c(5, 30, 50), pageLength = 5)
  )  
  
  output$mapadistribuicaodatacleaning <- renderLeaflet({
    #input$btnAtualizaMapaDataCleaning
    input$btnapagar
    input$btneliminarduplicatas
    input$btnbuscarespecieCSV
    input$btnbuscarespeciejabot
    input$btnbuscarespecie    
    if (exists("especie"))
    {
      rownames(especie) <- NULL
      especie$id = 1:nrow(especie)
      #%>% setView(lng = -31.5, lat = -13.4, zoom = 3)
      map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude)  %>% addMarkers(especie[,1], especie[,2]) %>% addMarkers(especie[,1], especie[,2],popup =~paste('<b><a onclick="document.getElementById(\'edtelemento\').value=',especie[,3],'">ID: ',especie[,3],'</a>') ) 
      map
    }
    
  })  


  pegaDadosCSV <- eventReactive(input$btnbuscarespecieCSV, {
    ETAPA <<- 1
    inFile <<- input$file1
    if (is.null(inFile))
    {
      return(NULL)
    } 
    else
    {
      especie<<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                          quote=input$quote)
      
      arquivo_path <<- inFile$datapath
      arquivo_header <<- input$header
      arquivo_sep <<- input$sep
      arquivo_quote <<- input$quote
      especie <<- especie[,2:3]
    }
  })
  
  
  pegaDadosGBif <- eventReactive(input$btnbuscarespecie, {
    ETAPA <<- 1
    key <- name_backbone(name=input$edtespecie)$speciesKey
    especie <<- occ_search(taxonKey=key, return='data', limit=100)
    especie <<- subset(especie, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    especie <<- especie[, c(4,3)]    
    names(especie)<<-c("Longitude", "Latitude")
    dados <- especie
    especie
  })
  
  pegaDadosJabot <- eventReactive(input$btnbuscarespeciejabot, {
    ETAPA <<- 1
    especie <<- getOcorrencia(input$edtespeciejabot)
    especie
  })
  
  
  output$dgbriddados <- renderDataTable({
    ETAPA <<- 1
    progress <- shiny::Progress$new()
    progress$set(message = "Localizando dados...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    if(input$tipodado=="csv")
    {
      pegaDadosCSV()
    }
    else
    {
      if (input$tipodado=='jabot')
      {
        pegaDadosJabot()
      }
      else
      {
        pegaDadosGBif()
      }
    }
    
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  
  
  )  
  
#########################################
## DOWNLOADS
#########################################

output$downloadData <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste('especie2', 'csv', sep = ".")
#    paste('Script', 'R', sep = ".")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
#    file.copy('Script.R', file, overwrite = TRUE)
    
    # Write to a file specified by the 'file' argument
    write.table(especie, file, sep = ';', row.names = FALSE)
  }
)

output$downloadscript <- downloadHandler(
  filename = function() {
    paste('Script', 'R', sep = ".")
  },
  content = function(file) {
    file.copy('Script.R', file, overwrite = TRUE)
  }
)




  
  output$mapadistribuicao <- renderLeaflet({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Atualizando o mapa...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    input$btnbuscarespecieCSV
    input$btnbuscarespecie
    input$btnbuscarespeciejabot
    
    if (exists("especie"))
    {
      if(input$tipodado=="gbif")
      {
        map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>% setView(lng = -31.5, lat = -13.4, zoom = 3)
        #%>% addMarkers(especie[,1], especie[,2])  
      }
      else
      {
        if(input$tipodado=="csv")
        {
          map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
            setView(lng = -31.5, lat = -13.4, zoom = 1) 
          #%>% addMarkers(especie[,1], especie[,2])  
        }
        else
        {
          if(input$tipodado=="jabot")
          {
            map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
              setView(lng = -31.5, lat = -13.4, zoom = 1) 
            #%>% addMarkers(especie[,1], especie[,2])  
          }
        }
      }
      map
    }
#    map
  })  
  

}
