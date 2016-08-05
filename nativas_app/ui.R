

shinyUI(
  navbarPage("Inventario Florestal",
             
             theme = "bootstrap8.css", # seleciona um tema contido na pasta www
             
             # Painel Principal - Upload de dados ####
             tabPanel("Importar os dados",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          fileInput( # input de arquivos
                            inputId = "file1", # Id
                            
                            label = "Selecione o arquivo .csv, .txt ou .xlsx", # nome que sera mostrado na UI
                            
                            accept=c('text/csv/xlsx','.csv', ".txt", ".xlsx")), # tipos de arquivos aceitos
                          
                          checkboxInput(inputId = "excel",
                                        label = "Excel (.xls ou .xslx) ?",
                                        value = F),
                          
                          radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId='sep',  #Id
                            label='Separador', # nome que sera mostrado na UI
                            choices=c(Virgula=',', "Ponto e Virgula"=';', Tab='\t'), # opcoes e seus nomes
                            selected=','), # valor que sera selecionado inicialmente
                          
                          radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId='dec', # Id
                            label='Decimal', # nome que sera mostrado na UI
                            choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
                            selected="."), # valor que sera selecionado inicialmente
                          
                          
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Load", # Id
                            "Carregue o arquivo")
                          
                        ), # sidebarPanel
                        
                        mainPanel(
                          DT::dataTableOutput("data")
                        ) # mainPanel
                      ) # sidebarLayout
             ), # tabPanel Visualizar os dados
             
             
             # Painel Agregate ####
             tabPanel("Agregate",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            "col.especies", # Id
                            "Selecione as especies", # nome que sera mostrado na UI
                            choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            # selected = 2  
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            "col.parcelas", # Id
                            "Selecione as parcelas", # nome que sera mostrado na UI
                            choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            # selected = 2  
                          ),
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Loadagreg", # Id
                            "rodar")
                          
                        ), #sidebarPanel
                        mainPanel(
                          
                          DT::dataTableOutput("agreg")
                          
                        ) # mainPanel
                      ) #sidebarLayout
                      ), #tabPanel Agregate
              
             # Painel Casual Simples ####
             tabPanel("A. Casual Simples",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'idadeacs', # Id
                            "Selecione Idade (meses):", # nome que sera mostrado na UI
                            choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            # selected = 2  
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_totalacs', # Id
                            "Selecione a area total (ha):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_parcelaacs', # Id
                            "Selecione a area da parcela (m²):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'VCCacs', # Id
                            "Selecione o Volume (m³):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'gruposacs', # Id
                            "selecione os grupos", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = TRUE  # permite mais de uma opcao ser selecionada
                          ), # limita o numero de variaveis que o usuario pode selecionar
                          
                          selectInput(
                            inputId='popacs', # Id
                            label='Populacao', # nome que sera mostrado na UI
                            choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                            selected="inf"
                          ),
                          
                          sliderInput("cdacs", 
                                      label = "Casas Decimais", 
                                      min = 0, 
                                      max = 10, 
                                      value = 4,
                                      step = 1),
                          
                          sliderInput("alphaacs", 
                                      label = "alpha", 
                                      min = 0.01, 
                                      max = 0.10, 
                                      value = 0.05,
                                      step = 0.01),
                          
                          sliderInput("erroacs", 
                                      label = "Erro admitido (%)", 
                                      min = 1, 
                                      max = 20, 
                                      value = 10,
                                      step = 1),
                          
                          
                          selectInput( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId="tidyacs",  #Id
                            label='Arranjo da tabela', # nome que sera mostrado na UI
                            choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                            selected=T), # valor que sera selecionado inicialmente
                          
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Loadacs", # Id
                            "rodar inventario")
                          
                        ), # sidebarPanel
                        mainPanel(
                          
                          DT::dataTableOutput("acs")
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout            
                      
                      
             ), # tabPanel Casual Simples
             
             # Painel Casual Estratificada ####
             tabPanel("A. Estratificada",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'idadeace', # Id
                            "Selecione Idade (meses):", # nome que sera mostrado na UI
                            choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            # selected = 2  
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_estratoace', # Id
                            "Selecione a area total (ha):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_parcelaace', # Id
                            "Selecione a area da parcela (m²):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'VCCace', # Id
                            "Selecione o Volume (m³):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'gruposace', # Id
                            "selecione os grupos", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = TRUE  # permite mais de uma opcao ser selecionada
                          ), # limita o numero de variaveis que o usuario pode selecionar
                          
                          selectInput(
                            inputId='popace', # Id
                            label='Populacao', # nome que sera mostrado na UI
                            choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                            selected="inf"
                          ),
                          
                          
                          sliderInput("cdace", 
                                      label = "Casas Decimais", 
                                      min = 0, 
                                      max = 10, 
                                      value = 4,
                                      step = 1),
                          
                          sliderInput("alphaace", 
                                      label = "alpha", 
                                      min = 0.01, 
                                      max = 0.10, 
                                      value = 0.05,
                                      step = 0.01),
                          
                          sliderInput("erroace", 
                                      label = "Erro admitido (%)", 
                                      min = 1, 
                                      max = 20, 
                                      value = 10,
                                      step = 1),
                          
                          selectInput( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId="tidyace",  #Id
                            label='Arranjo da tabela', # nome que sera mostrado na UI
                            choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                            selected=T), # valor que sera selecionado inicialmente
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Loadace", # Id
                            "rodar inventario")
                          
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
             tabPanel("A.Sistematica",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'idadeas', # Id
                            "Selecione Idade (meses):", # nome que sera mostrado na UI
                            choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            # selected = 2  
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_totalas', # Id
                            "Selecione a area total (ha):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'area_parcelaas', # Id
                            "Selecione a area da parcela (m²):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'VCCas', # Id
                            "Selecione o Volume (m³):", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = FALSE  # permite mais de uma opcao ser selecionada
                          ),
                          
                          selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
                            'gruposas', # Id
                            "selecione os grupos", # nome que sera mostrado na UI
                            choices = "", # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                            multiple = TRUE  # permite mais de uma opcao ser selecionada
                          ), # limita o numero de variaveis que o usuario pode selecionar
                          
                          sliderInput("cdas", 
                                      label = "Casas Decimais", 
                                      min = 0, 
                                      max = 10, 
                                      value = 4,
                                      step = 1),
                          
                          sliderInput("alphaas", 
                                      label = "alpha", 
                                      min = 0.01, 
                                      max = 0.10, 
                                      value = 0.05,
                                      step = 0.01),
                          
                          sliderInput("erroas", 
                                      label = "Erro admitido (%)", 
                                      min = 1, 
                                      max = 20, 
                                      value = 10,
                                      step = 1),
                          
                          
                          selectInput( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
                            inputId="tidyas",  #Id
                            label='Arranjo da tabela', # nome que sera mostrado na UI
                            choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
                            selected=T), # valor que sera selecionado inicialmente
                          
                          
                          actionButton( # botao que o usuario clica, e gera uma acao no server
                            "Loadas", # Id
                            "rodar inventario")
                          
                        ), # sidebarPanel
                        mainPanel(
                          
                          DT::dataTableOutput("as")
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout            
                      
                      
                      
             ), # tabPanel Sistematica
             
             # navbar Menu (Download + about) ####
             
             navbarMenu("Download dos arquivos",
                        # tabDownload ####
                        tabPanel("Download", 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     
                                     
                                     selectInput("dataset", "Escolha um dataset:", 
                                                 choices = c("Amostragem Casual Simples", 
                                                             "Amostragem Casual Estratificada 1", 
                                                             "Amostragem Casual Estratificada 2",
                                                             "Amostragem Sistematica")),
                                     
                                     selectInput("datasetformat",
                                                 "Escolha o formato do dataset:",
                                                 choices = c("Valor separado por Virgulas (.CSV)" = ".csv",
                                                             "Planilha do Excel (.xlsx)" = ".xlsx")
                                     ),
                                     
                                     downloadButton('downloadData', 'Download')
                                     
                                   ),
                                   mainPanel(
                                     tableOutput('table')      
                                   )
                                 )
                        ),
                        
                        # tab about #### 
                        
                        tabPanel( "Referencias" ,
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
                                  )
                        )
             )
             
             # final do server  ####    
  )# navbarPage
) # ShinyUI