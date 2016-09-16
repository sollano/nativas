

shinyUI(
  navbarPage("Inventario de Nativas",
             
             theme = "bootstrap8.css", # seleciona um tema contido na pasta www
             
             # Painel Intro ####          
             tabPanel( "Intro" ,
                       fluidRow(
                         column(6,
                                includeMarkdown("about.md")
                         ),
                         column(6,
                                img(class="img-polaroid",
                                    src="http://www.wernermadeiras.com.br/imagens/noticias/capa1.jpg"  ),
                                tags$small(
                                  "Source: Google images"
                                )
                         )
                       ) # fluid row
             ), # Painel Intro             
             
             
             # Upload de dados ####
             tabPanel("Dados",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3("Importar os dados"),
                          
                          fileInput( # input de arquivos
                            inputId = "file1", # Id
                            
                            label = h4("Selecione o arquivo: (.csv, .txt ou .xlsx)"), # nome que sera mostrado na UI
                            
                            accept=c('text/csv/xlsx','.csv', ".txt", ".xlsx")), # tipos de arquivos aceitos
                          
                          checkboxInput(inputId = "excel",
                                        label = "Excel (.xls ou .xslx) ?",
                                        value = F),
                          
                          div("Recomendados o uso do formato .csv", style = "color:blue"),
                          
                          radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId='sep',  #Id
                            label='Separador:', # nome que sera mostrado na UI
                            choices=c(Virgula=',', "Ponto e Virgula"=';', Tab='\t'), # opcoes e seus nomes
                            selected=','), # valor que sera selecionado inicialmente
                          
                          radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId='dec', # Id
                            label='Decimal:', # nome que sera mostrado na UI
                            choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
                            selected="."), # valor que sera selecionado inicialmente
                          
                          
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Load", # Id
                            "Carregue o arquivo")
                          
                        ), # sidebarPanel
                        
                        mainPanel(
                          DT::dataTableOutput("rawdata")
                        ) # mainPanel
                      ) # sidebarLayout
             ), # tabPanel Upload  de dados
             
             # Painel I. de agregacao ####
             tabPanel("I. de agregação",
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Índices de agregação"),
                          
                          uiOutput("selec_especiesagreg"),
                          
                          uiOutput("selec_parcelasagreg"),
                          
                          h4("Rotular espécies não identificadas:"),
                          
                          radioButtons("CBagreg",
                                       "Escolher rótulo da lista de especies, ou inserir manualmente?",
                                        c("lista de especies", "Manualmente"),
                                       "Manualmente"),

                          uiOutput("selec_rotuloNIagreg"),
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Loadagreg", # Id
                            "rodar")
                          
                        ), #sidebarPanel
                        mainPanel(
                          
                          DT::dataTableOutput("agreg")
                          
                        ) # mainPanel
                      ) #sidebarLayout
             ), #tabPanel Agregate
             
             
             # Painel Estrutura ####
             
             tabPanel("Estrutura",
               sidebarLayout(
                 sidebarPanel(
                   
                   h3("Estrutura"),
                   
                   uiOutput("selec_especiesestr"),
                   
                   uiOutput("selec_dapestr"),
                   
                   uiOutput("selec_parcelasestr"),
                   
                   uiOutput("selec_area.parcelaestr"),
                   
                   h4("Rotular espécies não identificadas:"),
                   
                   radioButtons("CBestr",
                                  "Escolher rótulo da lista de especies, ou inserir manualmente?",
                                  c("lista de especies", "Manualmente"),
                                  "Manualmente"),
                   
                   uiOutput("selec_rotuloNIestr"),
                   
                   h3("Variaveis opcionais:"),
                   
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
             
             
             # Painel Diversidade ####
             
             tabPanel("I. de diversidade",
               sidebarLayout(
                             
                             sidebarPanel(
                               
                               h3("Índices de diversidade"),
                               
                               uiOutput("selec_especiesdiv"),
                               
                               h4("Rotular espécies não identificadas:"),
                               
                               radioButtons("CBdiv",
                                            "Escolher rótulo da lista de especies, ou inserir manualmente?",
                                            c("lista de especies", "Manualmente"),
                                            "Manualmente"),
                               
                               uiOutput("selec_rotuloNIdiv"),
                               
                               actionButton( # botao que o usuario clica, e gera uma acao no server
                                 "Loaddiv", # Id
                                 "rodar")
                               
                               
                             ), # sidebar Panel
                             mainPanel(
                               DT::dataTableOutput("div", "70%")
                             ) # main Panel
                             
               )# Sidebar layout
               
             ), # tab panel Diversidade
             
             
             # Painel BDq ####   
             tabPanel("BDq Meyer",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3("BDq Meyer"),

                          uiOutput("selec_parcelasBDq"),
                          
                          uiOutput("selec_dapBDq"),
                          
                          uiOutput("selec_area.parcelaBDq"),
                          
                          sliderInput("intervalo.classeBDq", 
                                      label = "Intervalo de classe:", 
                                      min = 0, 
                                      max = 10, 
                                      value = 5,
                                      step = 1),
                          
                          sliderInput("min.dapBDq", 
                                      label = "DAP mínimo::", 
                                      min = 0, 
                                      max = 10, 
                                      value = 5,
                                      step = 1),
                          
                          sliderInput("i.licourtBDq", 
                                      label = "Quociente de Licourt:", 
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
                            tabPanel("Coeficientes", DT::dataTableOutput("BDq3") )
                          )
                          
                        ) # main Panel
                        
                      )# Sidebar layout
                      
             ), # tab Panel
             
          
             # Painel Matriz Similaridade ####
          
          tabPanel("M. Similaridade",
                   sidebarLayout(
                     
                     sidebarPanel(

                       h3("Matriz de Similaridade"),
                       
                       uiOutput("selec_especiesmsim"),
                       
                       uiOutput("selec_parcelasmsim"),
                       
                       h4("Rotular espécies não identificadas:"),
                       
                       selectizeInput("CBmsim",
                                      "Escolher rótulo da lista de especies, ou inserir manualmente?",
                                      c("lista de especies", "Manualmente"),
                                      "Manualmente"),
                       
                       uiOutput("selec_rotuloNImsim"),
                       
                       actionButton( # botao que o usuario clica, e gera uma acao no server
                         "Loadmsim", # Id
                         "rodar")
                       
                     ), # sidebar Panel
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Jaccard", DT::dataTableOutput("msim1") ),
                         tabPanel("Sorensen", DT::dataTableOutput("msim2") ) )
                       ) # main Panel
                     
                   )# Sidebar layout
                   
          ), # tab Panel Matriz Similaridade
          
             # Painel Pareado Similaridade ####
          
          tabPanel("P. Similaridade",
                   sidebarLayout(
                     
                     sidebarPanel(

                       h3("Pareado Similaridade"),

                       uiOutput("selec_especiespsim"),
                       
                       uiOutput("selec_parcelaspsim"),
                       
                       uiOutput("selec_psimselec_parc1"),
                      
                        uiOutput("selec_psimselec_parc2"),
                       
                       
                       h4("Rotular espécies não identificadas:"),
                       
                       selectizeInput("CBpsim",
                                      "Escolher rótulo da lista de especies, ou inserir manualmente?",
                                      c("lista de especies", "Manualmente"),
                                      "Manualmente"),
                       
                       uiOutput("selec_rotuloNIpsim"),
                       
                       actionButton( # botao que o usuario clica, e gera uma acao no server
                         "Loadpsim", # Id
                         "rodar")
                       
                     ), # sidebar Panel
                     mainPanel(
                       DT::dataTableOutput("psim") 
                     ) # main Panel
                     
                   )# Sidebar layout
                   
          ), # tab Panel Pareado Similaridade
          
             # NavbarMenu Inventario ####

             navbarMenu("Inventario",
                        
                        # Painel Totalização de Parcelas ####
                        tabPanel("Totalização de Parcelas",
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     h3("Totalização de Parcelas"),
                                     
                                     uiOutput("selec_DAPnew"),
                                     
                                     uiOutput("selec_HTnew"), 
                                     
                                     uiOutput("selec_VCCnew"), 
                                     
                                     uiOutput("selec_area_parcelanew"), 
                                     
                                     uiOutput("selec_gruposnew"), 
                                     
                                     h3("Variaveis opcionais:"),
                                     
                                     uiOutput("selec_area_totalnew"), 
                                     
                                     uiOutput("selec_idadenew"), 
                                     
                                     uiOutput("selec_VSCnew"), 
                                     
                                     uiOutput("selec_HDnew"),
                                     
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

                                     h3("Amostragem Casual Simples"),

                                     selectInput("dfacs", 
                                                 h4("Selecione o Dataset:"), 
                                                 choices = c("Nivel Parcela", 
                                                             "Nivel Arv/Parcela"),
                                                 selected = "Nivel Arv/Parcela"),
                                     
                                     h4("Selecione as Variaveis:"),
                                     
                                     uiOutput("selec_area_totalacs"),
                                     
                                     uiOutput("selec_area_parcelaacs"),
                                     
                                     uiOutput("selec_VCCacs"),
                                     
                                     h4("Variaveis opcionais:"),
                                     
                                     uiOutput("selec_idadeacs"),
                                     
                                     uiOutput("selec_gruposacs"),
                                     
                                     sliderInput("cdacs", 
                                                 label = "Selecione o nº de casas decimais:", 
                                                 min = 0, 
                                                 max = 10, 
                                                 value = 4,
                                                 step = 1),
                                     
                                     sliderInput("alphaacs", 
                                                 label = "Selecione o nível de significância:", 
                                                 min = 0.01, 
                                                 max = 0.10, 
                                                 value = 0.05,
                                                 step = 0.01),
                                     
                                     sliderInput("erroacs", 
                                                 label = "Selecione o erro admitido (%):", 
                                                 min = 1, 
                                                 max = 20, 
                                                 value = 10,
                                                 step = 1),
                                     
                                     radioButtons(
                                       inputId='popacs', # Id
                                       label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
                                       choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                                       selected="inf"
                                     ),

                                     radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                                       inputId="tidyacs",  #Id
                                       label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
                                       choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                                       selected=T), # valor que sera selecionado inicialmente
                                     
                                     
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
                                     
                                     h3("Amostragem Casual Estratificada"),
                                     
                                     selectInput("dface", 
                                                 h4("Selecione o Dataset:"), 
                                                 choices = c("Nivel Parcela", 
                                                             "Nivel Arv/Parcela"),
                                                 selected = "Nivel Arv/Parcela"),
                                     
                                     h4("Selecione as Variaveis:"),
                                     
                                     uiOutput("selec_area_totalace"),
                                     
                                     uiOutput("selec_area_parcelaace"),
                                     
                                     uiOutput("selec_VCCace"),
                                     
                                     uiOutput("selec_gruposace"), # limita o numero de variaveis que o usuario pode selecionar
                                     
                                     h4("Variaveis opcionais:"),
                                     
                                     uiOutput("selec_idadeace"),
                                     
                                     sliderInput("cdace", 
                                                 label = "Selecione o nº de casas decimais:", 
                                                 min = 0, 
                                                 max = 10, 
                                                 value = 4,
                                                 step = 1),
                                     
                                     sliderInput("alphaace", 
                                                 label = "Selecione o nível de significância:", 
                                                 min = 0.01, 
                                                 max = 0.10, 
                                                 value = 0.05,
                                                 step = 0.01),
                                     
                                     sliderInput("erroace", 
                                                 label = "Selecione o erro admitido (%):", 
                                                 min = 1, 
                                                 max = 20, 
                                                 value = 10,
                                                 step = 1),
                                     
                                     radioButtons(
                                       inputId='popace', # Id
                                       label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
                                       choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                                       selected="inf"
                                     ),

                                     radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                                       inputId="tidyace",  #Id
                                       label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
                                       choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                                       selected=T), # valor que sera selecionado inicialmente
                                     
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
                                     
                                     h3("Amostragem Sistematica"),
                                     
                                     selectInput("dfas", 
                                                 h4("Selecione o Dataset:"), 
                                                 choices = c("Nivel Parcela", 
                                                             "Nivel Arv/Parcela"),
                                                 selected = "Nivel Arv/Parcela"),
                                     
                                     h4("Selecione as Variaveis:"),
                                     
                                     uiOutput("selec_area_totalas"),
                                     
                                     uiOutput("selec_area_parcelaas"),
                                     
                                     uiOutput("selec_VCCas"),
                                     
                                     h4("Variaveis opcionais:"),
                                     
                                     uiOutput("selec_idadeas"),
                                     
                                     uiOutput("selec_gruposas"),
                                     
                                     sliderInput("cdas", 
                                                 label = "Selecione o nº de casas decimais:", 
                                                 min = 0, 
                                                 max = 10, 
                                                 value = 4,
                                                 step = 1),
                                     
                                     sliderInput("alphaas", 
                                                 label = "Selecione o nível de significância:", 
                                                 min = 0.01, 
                                                 max = 0.10, 
                                                 value = 0.05,
                                                 step = 0.01),
                                     
                                     sliderInput("erroas", 
                                                 label = "Selecione o erro admitido (%):", 
                                                 min = 1, 
                                                 max = 20, 
                                                 value = 10,
                                                 step = 1),
                                     
                                     
                                     radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                                       inputId="tidyas",  #Id
                                       label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
                                       choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                                       selected=T), # valor que sera selecionado inicialmente
                                     
                                     
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
                        ),# navbar Menu
             
             # Painel Download ####
             
             tabPanel("Download", 
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3("Download"),
                          
                          selectInput("dataset", "Escolha um Dataset:", 
                                      choices = c(
                                        "Agregar",
                                        "Estrutura",
                                        "Diversidade",
                                        "BDq Meyer",
                                        "BDq Meyer - Coeficientes",
                                        "Matriz Similaridade - Jaccard",
                                        "Matriz Similaridade - Sorensen",
                                        "Pareado Similaridade",
                                        "Amostragem Casual Simples", 
                                        "Amostragem Casual Estratificada 1", 
                                        "Amostragem Casual Estratificada 2",
                                        "Amostragem Sistematica",
                                        "Nivel Parcela")),
                          
                          selectInput("datasetformat",
                                      "Escolha o Formato do Dataset:",
                                      choices = c("Valor separado por Virgulas (.CSV)" = ".csv",
                                                  "Planilha do Excel (.xlsx)" = ".xlsx")
                          ),
                          
                          downloadButton('downloadData', 'Download')
                          
                        ),
                        mainPanel(
                          DT::dataTableOutput('table')      
                        )
                      )
             )
             
             # final da UI  ####    
  )# navbarPage
) # ShinyUI