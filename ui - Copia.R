library(shinydashboard)
library(leaflet)

vars <- c(
  "GBif - The Global Biodiversity Information Facility" = "gbif",
  "Jabot - Banco de dados JBRJ" = "jabot",
  "CSV - Separados por Virgula" = "csv"
)

varabiotico <- c(
  "Worldclim" = "WORLDCLIN",
  "Bio Oracle" = "BIOORACLE",
  "Outros" = "OUTROS",
  "Pessoal" = "MEU"
)

varabioticopassado <- c(
  "Worldclim" = "WORLDCLIN",
  "Bio Oracle" = "BIOORACLE"
)


varabioticofuturo <- c(
  "Bio Oracle A1B 2100" = "BIOORACLEA1B2100",
  "Bio Oracle A1B 2200" = "BIOORACLEA1B2200",
  "Bio Oracle A2 2100" = "BIOORACLEA22100",
  "Bio Oracle B1 2100" = "BIOORACLEB12100",
  "Bio Oracle B1 2200" = "BIOORACLEB12200",
  "Worldclim" = "WORLDCLIM"
)

cenariobiooracle <- c(
  "A1B" = "A1B",
  "A2" = "A2",
  "B1" = "B1"
)

periodobiooracle <- c(
  "Atual" = "current",
  "2100" = "2100",
  "2200" = "2200"
)

periodo <- c(
  "Atual" = "current",
  "2050" = "2050",
  "2070" = "2070"
)


tipomapa <- c(
  "Mundo" = "world",
  "America do Sul" = "South America"
)

resolucao <- c(
  "10 arc-minutes" = "bio_10m_bil",
  "5 arc-minutes" = "bio_5m_bil",
  "30 seg. arc-minutes" = "bio_30s_bil"
)

header <- dashboardHeader(
  title = "Modelagem v0.92"
)
body <- dashboardBody(
  fluidRow(
    column(width = 12,
           tabBox(
             title = "Etapas",width = NULL,height= "1000px",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1", 
             tabPanel("Variávei Bióticas", column(width = 12,
                                     
                                     tabBox(side = "right",selected = "Projeto",
                                       title = "",width = NULL,height= "600px",
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset1", 
									   
                                       tabPanel("Localização", column(width = 12,
                                                                             box(width = NULL, solidHeader = TRUE,
                                                                                 leafletOutput('mapadistribuicao'), height = 500)
                                                                             
                                         )
                                      ),
                                      tabPanel("Dados", column(width = 9,
                                                                     box(width = NULL,
                                                                         dataTableOutput('dgbriddados')
                                                                     )
                                      ),
                                      
                                      
                                      column(width = 3,
                                             box(width = NULL, status = "warning",
                                                 selectInput("tipodado", "Tipo de Entrada", vars),
                                                 conditionalPanel("input.tipodado == 'csv' ",
                                                                  helpText('Formato: [espécie,longitude,latitude]'),
                                                                  
                                                                  fileInput('file1', '',
                                                                            accept=c('text/csv', 
                                                                                     'text/comma-separated-values,text/plain', 
                                                                                     '.csv')),
                                                                  checkboxInput('header', 'Header', TRUE),
                                                                  radioButtons('sep', 'Separator',
                                                                               c(Virgula=',',
                                                                                 "Ponto e Virgual"=';',
                                                                                 Tab='\t'),
                                                                               ',', inline = TRUE),
                                                                  radioButtons('quote', 'Aspas',
                                                                               c('Sem'='',
                                                                                 'Duplas'='"',
                                                                                 'Simples'="'"),
                                                                               '"', inline = TRUE),
                                                                  actionButton("btnbuscarespecieCSV", "Visualizar",icon = icon("search"))
                                                                  
                                                 ),
                                                 conditionalPanel("input.tipodado == 'jabot' ",
                                                                  helpText('Entre com o nome do epíeteto específico.'),
                                                                  textInput("edtespeciejabot", label = "Espécie", value = ""),
                                                                  actionButton("btnbuscarespeciejabot", "Buscar", icon = icon("search"))
                                                                  
                                                 ),
                                                 conditionalPanel("input.tipodado == 'gbif' ",
                                                                  helpText('Entre com o nome do epíteto específico.'),
                                                                  textInput("edtespecie", label = "Espécie", value = ""),
                                                                  actionButton("btnbuscarespecie", "Buscar", icon = icon("search"))
                                                                  
                                                 )
                                             )
                                             
                                      )
                                      
                                      
                                      
                                      
                                      
                                      ),
									  tabPanel("Projeto", column(width = 9,
                                                                     box(width = NULL,
                                                                         textInput("edtprojeto", label = "Projeto", value = ""),
																		 actionButton("btncriarprojeto", "Criar projeto",icon = icon("gear")),
																		 actionButton("btnconsultarprojeto", "Consultar projeto",icon = icon("search"))
                                                                     )
                                      ),
																		 
																		 
																		 
																		 column(width = 3,
																		        box(width = NULL, helpText('Disponíveis'),
																		            
																		            if (length(list.files("./www/projeto/",full.names=F,pattern=paste0("."))>0))
																		            {
																		              lista_outros <- list.files("./www/projeto/",full.names=F,pattern=paste0("."))
																		              #       checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE)
																		              #   tags$div(
																		              #     tags$a(href=paste0('csv/',lista_csv[i]), paste0(lista_csv[i]))
																		              #   )
																		              lapply(1:length(lista_outros), function(i) {
																		                tagList(tags$h4(lista_outros[i]))
																		                #checkboxInput( paste0('chboxoutro',i), lista_outros[i] , value = FALSE)
																		              })
																		            }
																		            
																		            
																		            
																		        )
																		 )
																		 
																		 
																		 
																		 
																		 
																		 
																		 
									  )
                                      )
                                      )
                                     
                                      
                                      
                                      
                                      
                                      
                                      
                                      ),
                                      
                                    
             
             tabPanel("Data Cleaning", 
                      
                      column(width = 6,
                             box(width = NULL, solidHeader = TRUE,
                                 leafletOutput('mapadistribuicaodatacleaning', height = 500)
                             )
                      ),
                      
                      column(width = 6,
                             box(width = NULL, status = "warning",
                                 numericInput("edtelemento", "ID:", min = 0, max = 100, value = 0),
                                 
                                 actionButton("btnapagar", "Excluir",icon = icon("trash")),
                                 actionButton('btneliminarduplicatas', 'Remover duplicatas',icon = icon("cubes")),
                                 downloadButton('downloadData', 'Download dados')
                                 ),
                             
                             box(width = NULL,
                                 dataTableOutput('dgbriddadosdatacleaning')
                             )
                      )
             ) ,

             
             
             
             tabPanel("Dados Abióticos", 
                      column(width = 12,
                             
                             tabBox(side = "right",
                                    title = "",width = NULL,height= "600px",selected = "Extensão Criação",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", 
                                    
                                    
                                    tabPanel("Dados Abióticos", column(width = 8,
                                                                      plotOutput(outputId = "mapaabiotico", height = "400px"),
                                                                      plotOutput(outputId = "grafico_correlacao", width = "500px"),  
                                                                      plotOutput(outputId = "meuraster1", height = "400px"),
                                                                      plotOutput(outputId = "meuraster2", height = "400px")
                                    ),
                                    column(width = 4,
                                           box(width = NULL, status = "warning",
                                               actionButton("btnAtualizaSelecaoVariaveis", "Utilizar Selecionados"),
                                               
                                               selectInput("tipodadoabiotico", "Dados abióticos", varabiotico),
                                               conditionalPanel("input.tipodadoabiotico == 'BIOORACLE' ",
                                                                selectInput("periodobiooracle", "Período", periodobiooracle),
                                                                selectInput("cenariobiooracle", "Cenário", cenariobiooracle),
                                                                checkboxInput('calcite', 'calcite', value = FALSE),
                                                                checkboxInput('chlomin', 'chlomin', value = FALSE),
                                                                checkboxInput('cloudmean', 'cloudmean', value = FALSE),
                                                                checkboxInput('damean', 'damean', value = FALSE),
                                                                checkboxInput('nitrate', 'nitrate', value = FALSE),
                                                                checkboxInput('ph', 'ph', value = FALSE),
                                                                checkboxInput('silicate', 'silicate', value = FALSE),
                                                                checkboxInput('sstmin', 'sstmin', value = FALSE),
                                                                checkboxInput('chlomax', 'chlomax', value = FALSE),
                                                                checkboxInput('chlorange', 'chlorange', value = FALSE),
                                                                checkboxInput('cloudmin', 'cloudmin', value = FALSE),
                                                                checkboxInput('damin', 'damin', value = FALSE),
                                                                checkboxInput('parmax', 'parmax', value = FALSE),
                                                                checkboxInput('phosphate', 'phosphate', value = FALSE),
                                                                checkboxInput('sstmax', 'sstmax', value = FALSE),
                                                                checkboxInput('sstrange', 'sstrange', value = FALSE),
                                                                checkboxInput('chlomean', 'chlomean', value = FALSE),
                                                                checkboxInput('cloudmax', 'cloudmax', value = FALSE),
                                                                checkboxInput('damax', 'damax', value = FALSE),                                                  
                                                                checkboxInput('dissox', 'dissox', value = FALSE),                                                  
                                                                checkboxInput('parmean', 'parmean', value = FALSE),                                                  
                                                                checkboxInput('salinity', 'salinity', value = FALSE),                                                  
                                                                checkboxInput('sstmean', 'sstmean', value = FALSE)                                                  
                                               ),
                                               conditionalPanel("input.tipodadoabiotico == 'WORLDCLIN' ",
                                                                selectInput("periodo", "Período", periodo),
                                                                selectInput("resolucao", "Resolucao", resolucao),
                                                                
                                                                checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE),
                                                                checkboxInput('Bio2', 'BIO2 Mean Diurnal Range (Mean of monthly)', value = FALSE),
                                                                checkboxInput('Bio3', 'BIO3 Isothermality', value = FALSE),
                                                                checkboxInput('Bio4', 'BIO4 Temperature Seasonality', value = FALSE),
                                                                checkboxInput('Bio5', 'BIO5 Max Temperature of Warmest Month', value = FALSE),
                                                                checkboxInput('Bio6', 'BIO6 Min Temperature of Coldest Month', value = FALSE),
                                                                checkboxInput('Bio7', 'BIO7 Temperature Annual Range', value = FALSE),
                                                                checkboxInput('Bio8', 'BIO8 Mean Temperature of Wettest Quarter', value = FALSE),
                                                                checkboxInput('Bio9', 'BIO9 Mean Temperature of Driest Quarter', value = FALSE),
                                                                checkboxInput('Bio10', 'BIO10 Mean Temperature of Warmest Quarter', value = FALSE),
                                                                checkboxInput('Bio11', 'BIO11 Mean Temperature of Coldest Quarter', value = FALSE),
                                                                checkboxInput('Bio12', 'BIO12 Annual Precipitation', value = FALSE),
                                                                checkboxInput('Bio13', 'BIO13 Precipitation of Wettest Month', value = FALSE),
                                                                checkboxInput('Bio14', 'BIO14 Precipitation of Driest Month', value = FALSE),
                                                                checkboxInput('Bio15', 'BIO15 Precipitation Seasonality', value = FALSE),
                                                                checkboxInput('Bio16', 'BIO16 Precipitation of Wettest Quarter', value = FALSE),
                                                                checkboxInput('Bio17', 'BIO17 Precipitation of Driest Quarter', value = FALSE),
                                                                checkboxInput('Bio18', 'BIO18 Precipitation of Warmest Quarter', value = FALSE),
                                                                checkboxInput('Bio19', 'BIO19 Precipitation of Coldest Quarter', value = FALSE)
                                               ),
                                               conditionalPanel("input.tipodadoabiotico == 'OUTROS' ",
                                                              # checkboxInput('chboxoutro1', 'outro1' , value = FALSE)
                                                                if (length(list.files("ex/outros/",full.names=F,pattern=paste0(".bil"))>0))
                                                                {
                                                                  lista_outros <- list.files("ex/outros/",full.names=F,pattern=paste0(".bil"))
                                                               #       checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE)
                                                               #   tags$div(
                                                               #     tags$a(href=paste0('csv/',lista_csv[i]), paste0(lista_csv[i]))
                                                               #   )
                                                                  lapply(1:length(lista_outros), function(i) {
                                                                    checkboxInput( paste0('chboxoutro',i), lista_outros[i] , value = FALSE)
                                                                  })
                                                                }

                                               ),
                                               conditionalPanel("input.tipodadoabiotico == 'MEU' ",
                                                                htmlOutput("uiwordclim"),
                                                                fileInput('fileraster1', 'Raster 1',
                                                                          accept = c(
                                                                            'bil',
                                                                            'grd',
                                                                            'asc'
                                                                          )
                                                                ),
                                                                fileInput('fileraster2', 'Raster 2',
                                                                          accept = c(
                                                                            'bil',
                                                                            'grd',
                                                                            'asc'
                                                                          )
                                                                ) 
                                                                
                                               )
                                               
                                           )
                                    )),
                                    
                                    
                                    
                                    
                                    tabPanel("Extensão Projeção", column(width = 8,
                                                                         box(width = NULL, solidHeader = TRUE,
                                                                             leafletOutput('mapapontosextend2', height = 500)
                                                                         )
                                    ),
                                    column(width = 4,
                                           box(width = NULL, solidHeader = TRUE,
                                               box(width = NULL, status = "warning",
                                                   sliderInput("edtextend12", "Longitude esquerda:",
                                                               min = -180, max = 180, value = -90, step = 1),
                                                   sliderInput("edtextend22", "Longitude direita:",
                                                               min = -180, max = 180, value = -32, step = 1),
                                                   sliderInput("edtextend42", "Latitude superior:",
                                                               min = -90, max = 90, value = 23, step = 1),
                                                   sliderInput("edtextend32", "Latitude inferior:",
                                                               min = -90, max = 90, value = -33, step = 1)
                                                   
                                               )
                                               
                                               
                                           )
                                    )
                                    
                                    ),
                                    
                                    
                                    
                                    tabPanel("Extensão Criação", column(width = 8,
                                                                        box(width = NULL, solidHeader = TRUE,
                                                                            leafletOutput('mapapontosextend', height = 500)
                                                                        )
                                    ),
                                    column(width = 4,
                                           box(width = NULL, solidHeader = TRUE,
                                               box(width = NULL, status = "warning",
                                                   sliderInput("edtextend1", "Longitude esquerda:",
                                                               min = -180, max = 180, value = -90, step = 1),
                                                   sliderInput("edtextend2", "Longitude direita:",
                                                               min = -180, max = 180, value = -32, step = 1),
                                                   sliderInput("edtextend4", "Latitude superior:",
                                                               min = -90, max = 90, value = 23, step = 1),
                                                   sliderInput("edtextend3", "Latitude inferior:",
                                                               min = -90, max = 90, value = -33, step = 1)
                                               )
                                               
                                               
                                           )
                                    )
                                    
                                    )
                                    
                                    
                                    
                                    
                                    
                                    
                             )
                      )
                      
             ) ,             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             tabPanel("Modelagem", 
                      
                      column(width = 6,
                             tabBox(side = "left",
                                    title = "",width = NULL, height = 500,
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset2", 
                                    tabPanel("BC", column(width = 12,
                                                                  leafletOutput('maparesultadobc', height = 500)
                                                                  
                                      )
                                    ),
                                    tabPanel("MH", column(width = 12,
                                                               leafletOutput('maparesultadomh', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("MX", column(width = 12,
                                                                   leafletOutput('maparesultadomax', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("GLM", column(width = 12,
                                                                   leafletOutput('maparesultadoglm', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("RF", column(width = 12,
                                                                   leafletOutput('maparesultadorf', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("SVM", column(width = 12,
                                                                   leafletOutput('maparesultadosvm', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("Domain", column(width = 12,
                                                           leafletOutput('maparesultadodo', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("Ensemble", column(width = 12,
                                                           leafletOutput('maparesultadoessemble', height = 500),
                                                           #plotOutput(outputId = "pontosmodelagem", height = "1px",width = "500px"),
                                                           plotOutput(outputId = "plotesemble")
                                                           
                                    )
                                    )
                             ),
                             plotOutput(outputId = "plotmodelagem",height = "500px",width = "500px")
                      ),
                      column(width = 3,
                             box(width = NULL, status = "warning",
                                 
                                 selectInput("dataset", "Tipo de Particionamento", 
                                             choices = c("KFold", "Bootstrap")),
                                 sliderInput("edtnumgrupo", "No. de Partições:",
                                             min = 1, max = 50, value = 3, step = 1),
                                 sliderInput("edtnumpontos", "No. de Pontos (Pseudo Ausência):",
                                             min = 100, max = 2000, value = 1000, step = 100),
                                 
                                 sliderInput("edtextf", "Buffer:",
                                             min = 1, max = 2, value = 1.25, step = 0.05),
                                 #h4("Modelos"),
                                 checkboxInput('PROJETAR', 'Projetar em outra extensão', value = FALSE)
                                 
                             )

                      )
                      ,
                      column(width = 3,
                             
                             box(width = NULL, status = "warning",
                                 h4("Algoritmos"),
                                 checkboxInput('BIOCLIM', 'Bioclim', value = FALSE),
                                 checkboxInput('MAHALANOBIS', 'Mahalanobis', value = FALSE),
                                 checkboxInput('MAXENT', 'Maxent', value = FALSE),
                                 checkboxInput('GLM', 'GLM', value = FALSE),
                                 checkboxInput('RF', 'RandomForest', value = FALSE),
                                 checkboxInput('SVM', 'SVM', value = FALSE),
                                 checkboxInput('DOMAIN', 'Domain', value = FALSE),
                                 h4("Gerar modelos"),
                                 #checkboxInput('BINARIO', 'Binario', value = TRUE),
                                 checkboxInput('ENSEMBLE', 'Ensemble', value = TRUE),
                                 sliderInput("TSS", "TSS:", 
                                             min = 0, max = 1, value = 0.2, step= 0.1)
                                 
                             ),
                             box(width = NULL, status = "warning",
                                 actionButton("btnModelar", "Executar",icon = icon("cogs"))
                                 
                             )
                             
                      )
                      
                      
                      ),
             tabPanel("Resultado",
                      box(width = NULL,
						  
                          dataTableOutput('dbgridresultado')
                      )
             ),
             tabPanel("Downloads",
                      column(width = 12,
                             box(width = NULL,
                                 tabBox(side = "left",
                                        title = "",width = NULL,
                                        # The id lets us use input$tabset1 on the server to find the current tab
                                        id = "tabset2", 
                                        tabPanel("Modelos Binários e Contínuos", column(width = 12,
                                                              htmlOutput("ui")
                                                              
                                        )
                                        ),
                                        tabPanel("Modelos Finais", column(width = 12,
                                                                                        htmlOutput("uifinal")
                                        )
                                        )
                                 )
                                 #downloadButton('downloadscript', 'Script'),
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Script"),
                                 htmlOutput("uiscript")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Estatistica"),
                                 htmlOutput("uiestatistica")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Dados"),
                                 htmlOutput("uiarquivosdados")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Modelos"),
                                 htmlOutput("uiarquivosmodelos")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Ensemble"),
                                 htmlOutput("uiarquivosensemble")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Projeção"),
                                 htmlOutput("uiarquivosprojecao")
                             )
                      ),
                      column(width = 2,
                             box(width = NULL,status = "warning",
                                 h4("Projeção Futuro"),
                                 htmlOutput("uiarquivosprojecaofuturo")
                             )
                      )
                      
                      ),
             tabPanel("Sobre",
                      column(width = 2,
                             img(src="Logoenbtpequeno.gif")
                      ),
                      column(width = 10,
                             h4('Instituto de Pesquisas Jardim Botânico do Rio de Janeiro'),
                             h4('Escola Nacional de Botânica Tropical'),
                             h5('Programa de Mestrado Profissional: Biodiversidade em Unidades de Conservação'),
                             h5('Projeto de Trabalho de Conclusão de Curso de Mestrado Profissional - 2015'),
                             h5('Título: Desenvolvimento de programas para automatização de processos em análises espaciais e ecológicas no ambiente R.'),
                             h5('Aluno: Rafael Oliveira Lima'),
                             h5('Orientador: Marinez Ferreira de Siqueira'),
                             h5('Coorientador: Luis Alexandre da Silva Estevão')
                      )
               
             ),
									  
									  tabPanel("Ajuda",
									           column(width = 12,
									                  img(src="fluxo.jpg")
									           )

									  )     									  
									  
									  
									  
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
