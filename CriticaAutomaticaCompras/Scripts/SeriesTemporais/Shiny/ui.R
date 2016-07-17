shinyUI(fluidPage(
  
  ##--------------------------------------------------------##
  
  navbarPage("Critica automática de compras", theme = "bootstrap-flatly.css", 
             tabPanel("Modelo SARIMA",
                      fluidRow(
                        column(2,                               
                               selectInput("prod", label = h3("Selecione um produto"), 
                                           choices = produtos, 
                                           selected = "1T 1095"),
                               uiOutput("loja")
                        ),
                        column(3,                
                               h1(strong("Vendas"), align = "center"),                                      
                               plotOutput("vendas")
                        ),
                        column(3,                
                               h1(strong("Compras"), align = "center"),
                               plotOutput("compras")
                        ),
                        column(3,                
                               h1(strong("Estoque"), align = "center"),
                               plotOutput("estoque")
                        )
                      )
             ) 
  )
)
)