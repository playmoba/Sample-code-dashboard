library(shiny)
library(tidyr)
library(readr)
require(ggplot2)
library(plotly)
library(crosstalk)
library(shinyWidgets)
library(shinydashboard)
library(nycflights13)
library(shinyjs)
library(V8)
library(ggpubr)
library(png)
library(scales)
library(gridExtra)
library(skimr)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(stringi)
library(stringr)
library(reshape2)


# 1. Leitura de dados ------------------------------------------------------------------------------

## 1.1. Opções gerais shiny ........................................................................
# setwd(".../Shiny_Dashboards/02_Dashboards_Final")

options(scipen=2) # para forçar o R a não usar notação científica


## 1.2. Base compras ...............................................................................
base_global_compras1 <- readRDS('base_compras_original_mat_serv3_sample.RDS')

### Criando variável com concatenação entre Material/Serviço & Descrição ...........................
base_global_compras1 <- base_global_compras1 %>%
  mutate(
    texto_breve_ajust=stringr::str_to_title(texto_breve_ajust),
    mat_serv_texto_breve_ajust=ifelse(flag_servico==1,paste0(servico,'_',texto_breve_ajust),
                                      paste0(material,'_',texto_breve_ajust))
  )

### Filtrando apenas a área de Compras e pedidos não migrados e seleção de variáveis ...............
data <- base_global_compras1 %>%
  filter(
    flag_gr_compras==1,
    flag_pedido_migrado==0
  ) %>%
  mutate(
    data_pedido=as.Date(paste0(substr(data_pedido,1,2),'/',substr(data_pedido,4,5),'/',substr(data_pedido,7,10)),"%d/%m/%Y")
  ) %>%
  mutate(
    mat_serv=ifelse(is.na(material) | material=='',servico,material),
    preco_liq_ajust=ifelse(is.na(preco_liq_ajust) | preco_liq_ajust=='',preco_liq,preco_liq_ajust),
    
    ano_mes_semana=paste0(substr(data_pedido,3,4),'/',
                          ifelse(month(data_pedido)<10,paste0(0,month(data_pedido)),month(data_pedido)),
                          '/',ifelse(round(day(data_pedido)/7,0)==0,1,round(day(data_pedido)/7,0))),
    nome_do_fornecedor=stringr::str_to_title(nome_do_fornecedor),
    # nome_do_fornecedor_abrev=stringr::str_to_title(paste0(substr(nome_do_fornecedor,1,10),'...'))
    nome_do_fornecedor_abrev=nome_do_fornecedor
  ) %>%
  rename(
    preco_liq_sap=preco_liq,
    contrato=contr
  ) %>%
  select(
    anomes,
    data_pedido,
    doc_compras,
    nome_do_fornecedor,
    nome_do_fornecedor_abrev,
    mat_serv,
    mat_serv_texto_breve_ajust,
    texto_breve_ajust,
    contrato,
    grupo_compradores,
    flag_pedido_spot,
    
    un,
    por,
    quantidade,
    preco_liq_ajust,
    preco_liq_sap,
    val_liq_ajust
  )
rm(base_global_compras1)

# data <- sample_n(data, 50000) # TESTE: ELIMINAR ESTA LINHA QUANDO FOR TESTAR O CÓDIGO FINAL !!!!!!!!!!!!!!!!!!!!!!!!!!!



# 2. Shiny App -------------------------------------------------------------------------------------

## 2.1 User interface ..............................................................................
ui <- dashboardPage(
  skin="blue",
  
  dashboardHeader(title=h4(strong("Dashboard Compras")),
                  tags$li(
                    class="dropdown")
  ),
  
  dashboardSidebar(sidebarMenu(id="sidebar",
                               menuItem(strong(h4("Preço Histórico")), tabName="tabOne", icon=icon("chart-line"))
                               # menuItem(tags$b(h4("Contrato")), tabName="tabFour", icon=icon("file-contract"))
  )),
  
  dashboardBody(
    # useShinyjs(),
    # extendShinyjs(text="shinyjs.resetClick=function() { Shiny.onInputChange('.clientValue-plotly_click-month_select', 'null'); }"),
    
    tabItems(
      # tabOne - Preço Unitário ....................................................................
      tabItem(tabName="tabOne",
              fluidRow(
                ## seleção de filtros 
                shiny::column(width=3,
                              fluidRow(
                                column(width=12,
                                       shiny::selectInput("mat_serv_texto_breve_ajust", label=h4(strong("Material/Serviço:")), 
                                                          choices=data %>% select(mat_serv_texto_breve_ajust) %>% unique(),
                                                          selected=NULL
                                                          # selected=data[1,c('mat_serv_texto_breve_ajust')]
                                       )
                                )
                              ),
                              br(),
                              fluidRow(
                                column(width=12,
                                       selectInput("nome_do_fornecedor", label=h5(strong("Fornecedor:")),
                                                   choices=NULL, selected=NULL, multiple=TRUE)
                                )
                              ),
                              br(),
                              fluidRow(
                                column(width=12,
                                       shiny::selectInput("grupo_compradores", label=h5(strong("Grupo de Compradores:")), 
                                                          choices=NULL, selected=NULL,multiple=TRUE)
                                )
                              ),
                              br(),
                              fluidRow(
                                column(width=10, offset=1,
                                       sliderInput("range_tempo", h5(strong("Período (Ano-Mês-Dia):")),
                                                   min=min(data$data_pedido),
                                                   max=max(data$data_pedido),
                                                   value=max(data$data_pedido))))
                ),
                ## gráfico
                shiny::column(width=9,
                              fluidRow(
                                shiny::column(tags$b(h3(strong("Preço Líquido Unitário (R$) - Materiais/Serviços"))),
                                              width=12, align="center", 
                                              plotlyOutput("scatter_preco")
                                )
                              )
                )
              ),
              fluidRow(
                shiny::column(tags$b(h4(strong("Histórico de Preço Líquido Unitário (R$) - Materiais/Serviços"))),
                              width=12, align="center", 
                              h6(dataTableOutput("tabela_preco"))
                )
              )
      )
      
      
    )),
  # formatacoes gerais ...........................................................................
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #c40000;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #c6b1b1;
                            }
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #474543;
                            }       
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #d8c0c0;
                            }
                            
                            ')))
  )



# 2.2. Server ......................................................................................
server <- function(session, input, output) { 
  
  # tabOne .........................................................................................
  ## atualizando os nomes dos Fornecedores, dependendo do material/serviço selecionado
  observe(
    {
      fornecedor_subset <- unique(data[data$mat_serv_texto_breve_ajust==input$mat_serv_texto_breve_ajust,]$nome_do_fornecedor)
      
      updateSelectInput(session,"nome_do_fornecedor", "Fornecedor:",choices=fornecedor_subset)
    }
  )
  
  ## atualizando os Grupos de Compradores, dependendo do material/serviço e fornecedor selecionados
  observe(
    {
      grupo_compradores_subset <- unique(data[data$mat_serv_texto_breve_ajust==input$mat_serv_texto_breve_ajust & 
                                                data$nome_do_fornecedor %in% c(input$nome_do_fornecedor),]$grupo_compradores)
      
      updateSelectInput(session,"grupo_compradores", "Grupo de Compradores:",choices=grupo_compradores_subset)
    }
  )
  
  scatter_bar_react <- reactive({
    if(is.null(input$nome_do_fornecedor)){
      data[data$mat_serv_texto_breve_ajust==input$mat_serv_texto_breve_ajust &
             data$data_pedido<=input$range_tempo,]
    } else if(!is.null(input$nome_do_fornecedor) & is.null(input$grupo_compradores)){
      data[data$mat_serv_texto_breve_ajust==input$mat_serv_texto_breve_ajust &
             (data$nome_do_fornecedor %in% input$nome_do_fornecedor) &
             data$data_pedido<=input$range_tempo,]
    } else {
      data[data$mat_serv_texto_breve_ajust==input$mat_serv_texto_breve_ajust &
             (data$nome_do_fornecedor %in% input$nome_do_fornecedor) &
             (data$grupo_compradores %in% input$grupo_compradores) &
             data$data_pedido<=input$range_tempo,]
    } 
  })
  
  # Scatter de preço unitário
  output$scatter_preco <- renderPlotly({
    
    scatter_preco <- scatter_bar_react() %>%
      mutate(
        preco_liq_ajust=round(preco_liq_ajust,1)
      ) %>%
      # ggplot(aes(x=as.factor(format(data_pedido,'%y/%m')), y=preco_liq_ajust,
      ggplot(aes(x=data_pedido, 
                 y=preco_liq_ajust,
                 fill=nome_do_fornecedor_abrev,
                 shape=as.factor(flag_pedido_spot),
                 text=paste0('Código/Descrição: ', mat_serv_texto_breve_ajust, '\n',
                             'Fornecedor: ', nome_do_fornecedor, '\n',
                             'Contrato: ', ifelse(contrato=='' | is.na(contrato),'spot',contrato), '\n',
                             'Preço Líq. SAP: R$ ', prettyNum(preco_liq_sap,big.mark=".", decimal.mark = ',',scientific=FALSE), '\n',
                             'Preço Líq. Ajustado (Valor/Qtde): R$ ', prettyNum(preco_liq_ajust,big.mark=".", decimal.mark = ',',scientific=FALSE), '\n'))) +
      
      geom_jitter(width=0.75, size=3, alpha=0.7, col="darkblue") +
      scale_y_continuous(limits=c(0, NA), labels=function(x) format(x, big.mark = ".", decimal.mark = ',',scientific = FALSE)) +
      scale_x_date(labels=date_format('%b/%y')) +
      xlab("Mês/Ano") +
      ylab("Preço Unitário (R$)") +
      theme( legend.title = element_blank() )
    
    ggplotly(scatter_preco,tooltip="text") %>%
      add_annotations( text="Fornecedor, Spot=1", xref="paper", yref="paper",
                       x=1.02, xanchor="left",
                       y=1, yanchor="bottom",    # Same y as legend below
                       legendtitle=TRUE, showarrow=FALSE ) %>%
      layout(xaxis=list(showgrid=FALSE),
             yaxis=list(showgrid=TRUE), height=375,
             showlegend=TRUE,
             legend=list(y=1, yanchor="top")
      )
    
  })
  
  ## tabela de preços
  output$tabela_preco <- renderDataTable(
    scatter_bar_react() %>%
      select(
        anomes,
        doc_compras,
        nome_do_fornecedor,
        mat_serv,
        texto_breve_ajust,
        contrato,
        grupo_compradores,
        
        un,
        por,
        quantidade,
        preco_liq_ajust,
        preco_liq_sap,
        val_liq_ajust
      ) %>%
      mutate(
        anomes=paste0(substr(anomes,1,4),'/',substr(anomes,5,6)),
        preco_liq_sap=prettyNum(preco_liq_sap,big.mark=".", decimal.mark = ',',scientific=FALSE),
        preco_liq_ajust=prettyNum(preco_liq_ajust,big.mark=".", decimal.mark = ',',scientific=FALSE),
        val_liq_ajust=prettyNum(val_liq_ajust,big.mark=".", decimal.mark = ',',scientific=FALSE)
      ) %>%
      rename(
        `Ano/Mes`=anomes,
        Doc_Compras=doc_compras,
        Fornecedor=nome_do_fornecedor,
        Cod_Mat_Serv=mat_serv,
        Descricao=texto_breve_ajust,
        Contrato=contrato,
        Grupo=grupo_compradores,
        Un=un,
        Por=por,
        Qtde=quantidade,
        Preco_Liq_SAP=preco_liq_sap,
        Preco_Liq_Ajust=preco_liq_ajust,
        Val_Liq=val_liq_ajust
      ) 
  )
  
}



# 2.3. Run the application .........................................................................

app <- shinyApp(ui=ui, server=server)
# runApp(app)

# rsconnect::deployApp() # rodar este código somente no console, após o resto do código ter sido rodado


