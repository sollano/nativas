library(shinythemes)
library(shiny)
library(DT)
library(formattable)
#library(plyr)
library(tidyr)
library(dplyr)
library(lazyeval)
library(ggplot2)
library(ggdendro)
library(ggthemes)
library(plotly)
library(xlsx)
library(xlsxjars)
library(markdown)
shinyUI(
  
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
  navbarPage("Inventário de Nativas (BETA)",
             
             theme = "green_yeti2.css",
            # theme = "green.css", # seleciona um tema contido na pasta www
            # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
             
             # Painel Intro ####          
             tabPanel( "Intro" ,
                       fluidRow(
                         column(5,
                                includeMarkdown("about.md")
                         ),
                         column(6,
                                img(contentType = "image/jpg",
                                    src="intro_picture.jpg",
                                    width = 770,
                                    height = 750)
                               
                         )
                       ) # fluid row
             ), # Painel Intro             
             
             
             # Upload de dados ####
             tabPanel("Dados",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3("Dados"),
                          
                          radioButtons("df_select", 
                                       "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                       c("Fazer o upload", "Utilizar o dado de exemplo"), 
                                       selected = "Fazer o upload"),
                          
                           uiOutput("upload") # tipos de arquivos aceitos
                         

                          
                        ), # sidebarPanel
                        
                        mainPanel(
                          DT::dataTableOutput("rawdata")
                        ) # mainPanel
                      ) # sidebarLayout
             ), # tabPanel Upload  de dados
             tabPanel("Filtrar dados",
                      
                      sidebarPanel(
                        
                        h3("Filtrar dados"),
                        
                        uiOutput("filtrar_dados_col1_ui"),
                        uiOutput("filtrar_dados_col1_filtro_ui"),
                        uiOutput("filtrar_dados_rm_cols_ui"),
                        
                        actionButton( # botao que o usuario clica, e gera uma acao no server
                          "Loadfiltrar", # Id
                          "Filtrar")
                        
                        
                      ),# sidebarPanel
                      
                      mainPanel(DT::dataTableOutput("filter_table"))# mainPanel
               
               
               
             ), # tabPanel filtrar dados
             # NavbarMenu Indices ####
             
             navbarMenu("Índices",
                        
                        # Painel I. de diversidade ####
                        
                        tabPanel("Índice de diversidade",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     h3("Índices de diversidade"),
                                     
                                     uiOutput("selec_especiesdiv"),
                                     
                                     uiOutput("selec_rotuloNIdiv"),
                                     
                                     h4("Variável opcional: calcular índices por parcela"),
                                     
                                     uiOutput("selec_parcelasdiv"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loaddiv", # Id
                                       "rodar")
                                     
                                     
                                   ), # sidebar Panel
                                   mainPanel(
                                     DT::dataTableOutput("div")
                                   ) # main Panel
                                   
                                 )# Sidebar layout
                                 
                        ), # tab panel Diversidade
                        
                        
                        
                        # Painel Matriz Similaridade ####
                        
                        tabPanel("Matriz de Similaridade", 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     tags$style(type="text/css",
                                                ".recalculating {opacity: 1.0;}"
                                     ),
                                     
                                     h3("Matriz de Similaridade"),
                                     
                                     uiOutput("selec_especiesmsim"),
                                     
                                     uiOutput("selec_parcelasmsim"),
                                     
                                     uiOutput("selec_rotuloNImsim"),
                                     
                                     uiOutput("rb_slider_graphmsim1"),
                                     
                                     uiOutput("rb_slider_graphmsim2"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadmsim", # Id
                                       "rodar")
                                     
                                   ), # sidebar Panel
                                   mainPanel(
                                     tabsetPanel(id = "mainPanel_Indices",
                                       tabPanel("Jaccard", DT::dataTableOutput("msim1") ),
                                       tabPanel("Dendrograma Jaccard", plotOutput("msim1_graph_"), value = "id_msim1_graph"),
                                       tabPanel("Sorensen", DT::dataTableOutput("msim2") ),
                                       tabPanel("Dendrograma Sorensen", plotOutput("msim2_graph_"), value = "id_msim2_graph" )  )
                                   ) # main Panel
                                   
                                 )# Sidebar layout
                                 
                        ), # tab Panel Matriz Similaridade
                        
                        # Painel Pareado Similaridade ####
                        
                        tabPanel("Pareado Similaridade",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     h3("Pareado Similaridade"),
                                     
                                     uiOutput("selec_especiespsim"),
                                     
                                     uiOutput("selec_parcelaspsim"),
                                     
                                     uiOutput("selec_psimselec_parc1"),
                                     
                                     uiOutput("selec_psimselec_parc2"),
                                     
                                     uiOutput("selec_rotuloNIpsim"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadpsim", # Id
                                       "rodar")
                                     
                                   ), # sidebar Panel
                                   mainPanel(
                                     DT::dataTableOutput("psim", "70%") 
                                   ) # main Panel
                                   
                                 )# Sidebar layout
                                 
                        ), # tab Panel Pareado Similaridade
                        
                        
                        # Painel I. de agregacao ####
                        tabPanel("Índice de agregação",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     h3("Índices de agregação"),
                                     
                                     uiOutput("selec_especiesagreg"),
                                     
                                     uiOutput("selec_parcelasagreg"),
                                     
                                     uiOutput("selec_rotuloNIagreg"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadagreg", # Id
                                       "rodar")
                                     
                                   ), #sidebarPanel
                                   mainPanel(
                                     
                                     DT::dataTableOutput("agreg")
                                     
                                   ) # mainPanel
                                 ) #sidebarLayout
                        ) #tabPanel Agregate
                        
                # navbarMenu end ####

             ), # NavbarMenu Indices
             
             # Painel Estrutura ####
          
          tabPanel("Estrutura",
                   sidebarLayout(
                     sidebarPanel(
                       
                       h3("Estrutura"),
                       
                       uiOutput("selec_especiesestr"),
                       
                       uiOutput("selec_dapestr"),
                       
                       uiOutput("selec_parcelasestr"),
                       
                       uiOutput("selec_area.parcelaestr"),
                       
                       uiOutput("selec_rotuloNIestr"),
                       
                       h4("Variáveis opcionais:"),
                       
                       uiOutput("selec_est.verticalestr"),
                       
                       uiOutput("selec_est.internoestr"),
                       
                       sliderInput("cdestr", 
                                   label = "Selecione o nº de casas decimais:", 
                                   min = 0, 
                                   max = 10, 
                                   value = 2,
                                   step = 1),
                       
                       
                       
                       actionButton( # botao que o usuario clica, e gera uma acao no server
                         "Loadestr", # Id
                         "rodar")
                       
                     ), # sidebar Panel
                     
                     mainPanel(
                       DT::dataTableOutput("estr")
                     ) # main Panel
                     
                   )# Sidebar layout
                   
          ),# Panel Estrutura
          
          
          
             # Painel BDq ####   
          tabPanel("BDq Meyer",
                   sidebarLayout(
                     
                     sidebarPanel(
                       
                       tags$style(type="text/css",
                                  ".recalculating {opacity: 1.0;}"
                       ),
                       
                       h3("BDq Meyer"),
                       
                       uiOutput("selec_parcelasBDq"),
                       
                       uiOutput("selec_dapBDq"),
                       
                       uiOutput("selec_area.parcelaBDq"),
                       
                       sliderInput("min.dapBDq", 
                                   label = "Selecione o DAP mínimo:", 
                                   min = 0, 
                                   max = 50, 
                                   value = 5,
                                   step = 1),
                       
                       sliderInput("intervalo.classeBDq", 
                                   label = "Selecione um intervalo de classe:", 
                                   min = 0, 
                                   max = 10, 
                                   value = 5,
                                   step = 1),

                       sliderInput("i.licourtBDq", 
                                   label = "Selecione um valor de quociente de Licourt:", 
                                   min = 0, 
                                   max = 5, 
                                   value = 1.3,
                                   step = .1),
                       
                       actionButton( # botao que o usuario clica, e gera uma acao no server
                         "LoadBDq", # Id
                         "rodar")
                       
                     ), # sidebar Panel
                     mainPanel(
                       tabsetPanel(
                         tabPanel("BDq", DT::dataTableOutput("BDq1") ),
                         tabPanel("Gráfico", plotly::plotlyOutput( "BDq_graph_" ) ),
                       #  tabPanel("Gráfico", plotOutput( "BDq_graph_" ) ),
                         tabPanel("Coeficientes", DT::dataTableOutput("BDq3", "70%") )
                       )
                       
                     ) # main Panel
                     
                   )# Sidebar layout
                   
          ), # tab Panel
          
          
          
             # NavbarMenu Inventario ####

             navbarMenu("Inventario",
                        
                        # Painel Totalização de Parcelas ####
                        tabPanel("Totalização de Parcelas",
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     uiOutput("tot_parc_ui1"),
                                     
                                     h4("Inserir valores de área:"),
                                     
                                     radioButtons("area_radio_new",
                                                  "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                                  c("Lista de colunas", "Manualmente"),
                                                  "Manualmente"),
                                     
                                     uiOutput("tot_parc_ui2"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadnew", # Id
                                       "Rodar")

                                   ), # sidebar panel
                                   
                                   mainPanel(
                                     
                                     DT::dataTableOutput("newdata")
                                     
                                   ) # mainPanel
                                   
                                   
                                 ) #sidebar layout
                                 
                                 
                        ) , # tab panel nivel parcela       
                        
                        
                        # Painel Casual Simples ####
                        tabPanel("A. Casual Simples",
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(


                                     uiOutput("acs_ui1"),
                                     
                                     h4("Inserir valores de área:"),
                                     
                                     radioButtons("area_radio_acs",
                                                  "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                                  c("Lista de colunas", "Manualmente"),
                                                  "Lista de colunas"),
                                     
                                     uiOutput("acs_ui2"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadacs", # Id
                                       "Rodar Inventario")
                                     
                                   ), # sidebarPanel
                                   mainPanel(
                                     
                                     DT::dataTableOutput("acs")
                                     
                                   ) # mainPanel
                                   
                                 ) # sidebarLayout            
                                 
                                 
                        ), # tabPanel Casual Simples
                        
                        # Painel Casual Estratificada ####
                        tabPanel("A. Casual Estratificada",
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(

                                     uiOutput("ace_ui"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadace", # Id
                                       "Rodar Inventario")
                                     
                                   ), # sidebarPanel
                                   mainPanel(
                                     
                                     tabsetPanel(
                                       id="tabsace",
                                       tabPanel("Resultados", DT::dataTableOutput("ace2")),
                                       tabPanel("Resultados por estrato", DT::dataTableOutput("ace1") )
                                       
                                       
                                     )#tabsetPanel
                                     
                                   ) # mainPanel
                                   
                                 ) # sidebarLayout            
                                 
                                 
                        ), # tabPanel Casual Estratificada
                        
                        
                        
                        # Painel Sistematica ####
                        tabPanel("A. Sistematica",
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     uiOutput("as_ui1"),
                                     
                                     h4("Inserir valores de área:"),
                                     
                                     radioButtons("area_radio_as",
                                                  "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                                  c("Lista de colunas", "Manualmente"),
                                                  "Lista de colunas"),
                                     
                                     uiOutput("as_ui2"),
                                     
                                     actionButton( # botao que o usuario clica, e gera uma acao no server
                                       "Loadas", # Id
                                       "Rodar Inventario")
                                     
                                   ), # sidebarPanel
                                   mainPanel(
                                     
                                     DT::dataTableOutput("as")
                                     
                                   ) # mainPanel
                                   
                                 ) # sidebarLayout            
                                 
                                 
                                 
                        ) # tabPanel Sistematica
                        
                        
                          # navbar Menu end ####
                        ),# navbar Menu inventario
             
             # NavbarMenu Download ####
          
          navbarMenu("Download",
                     # Painel Download Tabelas ####
                     
                     tabPanel("Download de tabelas", 
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  h3("Download de tabelas"),
                                  
                                  selectInput("dataset", "Escolha uma tabela:", 
                                              choices = c(
                                                "Dado utilizado / filtrado",
                                                "Indice diversidade",
                                                "Matriz similaridade - Jaccard",
                                                "Matriz similaridade - Sorensen",
                                                "Pareado similaridade",
                                                "Indice de agregacao",
                                                "Estrutura",
                                                "BDq Meyer",
                                                "BDq Meyer - Coeficientes",
                                                "Amostragem Casual Simples", 
                                                "Amostragem Casual Estratificada 1", 
                                                "Amostragem Casual Estratificada 2",
                                                "Amostragem Sistematica",
                                                "Nivel Parcela")),
                                  
                                  selectInput("datasetformat",
                                              "Escolha o formato da tabela:",
                                              choices = c("Valor separado por Virgulas (.CSV)" = ".csv",
                                                          "Planilha do Excel (.xlsx)" = ".xlsx")
                                  ),
                                  
                                  downloadButton('downloadData', 'Download')
                                  
                                ),
                                mainPanel(
                                  DT::dataTableOutput('table')      
                                )
                              )
                     ), # download tabelas
                     
                     # Painel Download Graficos ####
                     
                     tabPanel("Download de graficos", 
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  tags$style(type="text/css",
                                             ".recalculating {opacity: 1.0;}"
                                  ),
                                  
                                  h3("Download de graficos"),
                                  
                                  selectInput("graph_d", "Escolha uma grafico:", 
                                              choices = c(
                                               "Distribuicao - BDq Meyer",
                                                "Dendrograma - Jaccard",
                                                "Dendrograma - Sorensen" )),
                                  
                                  selectInput("graphformat",
                                              "Escolha o formato do gráfico:",
                                              choices = c("PNG" = ".png",
                                                          "JPG" = ".jpg",
                                                          "PDF" = ".pdf") ),
                                  
                                  downloadButton('downloadGraph', 'Download')
                                  
                                ),
                                mainPanel(
                                  plotOutput("graph_d_out")      
                                )
                              )
                     ) # download graficos
                     
                     # final navbarMenu download ####           
          )
          
  
             # final da UI  ####    
  )# navbarPage
  )#tagList
) # ShinyUI