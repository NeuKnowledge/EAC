shinyServer(function(input, output){
  
  output$loja <- renderUI({
    prod_analise <- input$prod
    lojas <- as.list(unique(subset(vendas, PRODUTO == prod_analise)$LOJA))
    loja <- lojas[[1]]
    selectInput("loja", label = h3("Selecione uma loja"), 
                choices = lojas, 
                selected = loja) 
  })
  
  SeriesTemporais <- reactive({  
    #--- Inputs
    prod_analise <- input$prod
    loja_analise <- input$loja
    #--- Subset das bases
    vendas_prod_treino <- vendas_treino[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Treino"]
    vendas_prod_validacao <- vendas_validacao[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Validacao"]
    vendas_prod_aux <- rbind(vendas_prod_treino, vendas_prod_validacao)
    
    compras_prod_treino <- compras_treino[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Treino"]
    compras_prod_validacao <- compras_validacao[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Validacao"]
    compras_prod_aux <- rbind(compras_prod_treino, compras_prod_validacao)
    compras_prod_aux <- compras_prod_aux[, list(ANO_MES_DIA, QTDE_COMPRA)]
    
    estoque_prod_treino <- estoque_treino[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Treino"]
    estoque_prod_validacao <- estoque_validacao[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Validacao"]
    estoque_prod_aux <- rbind(estoque_prod_treino, estoque_prod_validacao)
    estoque_prod_aux <- estoque_prod_aux[, list(ANO_MES_DIA, QTDE_COMPRA)]
    
    #--- Alocando espaço para as predições
    nPredicts <- nrow(vendas_prod_validacao)
    vendas_pred <- data.frame(ANO_MES_DIA = vendas_prod_validacao$ANO_MES_DIA, 
                              PREDICAO = 0, 
                              LIMITE_INFERIOR_1 = 0, 
                              LIMITE_SUPERIOR_1 = 0,
                              LIMITE_INFERIOR_2 = 0, 
                              LIMITE_SUPERIOR_2 = 0)
    vendas_ajuste <- vendas_prod_treino
    
    #--- Modelando
    for(i in 1:nPredicts){
      model_prod <- auto.arima(x = vendas_ajuste$QTDE_VENDA, 
                               stationary = T,
                               d = 1)
      prev_0.95 <- forecast(object = model_prod, level = 0.95, h = 1)
      prev_0.99 <- forecast(object = model_prod, level = 0.99, h = 1)
      vendas_pred[i, -1] <- c(max(0, prev_0.95$mean), 
                              max(0, prev_0.95$lower), max(0, prev_0.95$upper), 
                              max(0, prev_0.99$lower), max(0, prev_0.99$upper))
      
      vendas_ajuste <- rbind(vendas_ajuste, vendas_prod_validacao[i,])
    }
    
    #------------------------------------------
    #--- Tirar quando houver a base de estoque
    #------------------------------------------
    names(estoque_prod_aux) <- c("ANO_MES_DIA", "QTDE_ESTOQUE")
    estoque_prod_aux$QTDE_ESTOQUE <- rpois(n = nrow(estoque_prod_aux), lambda = estoque_prod_aux$QTDE_ESTOQUE)
    #------------------------------------------
    
    dadosCompletos <- merge(vendas_prod_aux, compras_prod_aux, by = "ANO_MES_DIA")
    dadosCompletos <- merge(dadosCompletos, estoque_prod_aux, by = "ANO_MES_DIA")
    nTreino <- nrow(subset(dadosCompletos, BASE == "Treino"))
    nVal <- nrow(subset(dadosCompletos, BASE == "Validacao"))
    dadosCompletos$PREV_VENDA <- c(dadosCompletos$QTDE_VENDA[1:nTreino], vendas_pred$PREDICAO)
    dadosCompletos$PREV_VENDA_INF <- c(dadosCompletos$QTDE_VENDA[1:nTreino], vendas_pred$LIMITE_INFERIOR_2)
    dadosCompletos$PREV_VENDA_SUP <- c(dadosCompletos$QTDE_VENDA[1:nTreino], vendas_pred$LIMITE_SUPERIOR_2)
    dadosCompletos$PREV_COMPRA <- c(dadosCompletos$QTDE_COMPRA[1:nTreino], rep(0, nVal))
    dadosCompletos$PREV_ESTOQUE <- c(dadosCompletos$QTDE_ESTOQUE[1:nTreino], rep(0, nVal))
    dadosCompletos <- data.frame(dadosCompletos)
    
    for(i in nTreino:nrow(dadosCompletos)){
      dadosCompletos[i, "PREV_COMPRA"] <- max(0, -(dadosCompletos[(i-1), "PREV_ESTOQUE"] - dadosCompletos[i, "PREV_VENDA_SUP"]))
      dadosCompletos[i, "PREV_ESTOQUE"] <- dadosCompletos[(i-1), "PREV_ESTOQUE"] - dadosCompletos[i, "QTDE_VENDA"] + dadosCompletos[i, "PREV_COMPRA"]
    }
    
    return(dadosCompletos = dadosCompletos)
  })
  
  #--- Outputs
  
  output$vendas <- renderPlot({
    dadosCompletos <- SeriesTemporais()
    dadosValidacao <- subset(dadosCompletos, BASE == "Validacao")
    
    vendas_graph <- ggplot(data = dadosCompletos, aes(x = ANO_MES_DIA)) +
      geom_line(aes(y = QTDE_VENDA, colour = BASE), size = 1) +
      geom_point(aes(y = QTDE_VENDA, colour = BASE), size = 1.4) +
      geom_line(data = dadosValidacao, aes(y = PREV_VENDA, colour = "Predicao"), size = 1) +
      geom_point(data = dadosValidacao, aes(y = PREV_VENDA, colour = "Predicao"), size = 1.4) +
      geom_ribbon(data = dadosValidacao, aes(ymin = PREV_VENDA_INF, ymax = PREV_VENDA_SUP), alpha = 0.3) +
      scale_colour_manual("", labels = c("Treino" = "Treino", 
                                         "Validacao" = "Validação", 
                                         "Predicao" = "Predição"),
                          values = c("Treino" = "#ED665D",
                                     "Validacao" = "#729ECE",
                                     "Predicao" = "#FF9E4A")) +
      expand_limits(y = 0) + 
      ylab("Vendas") + xlab("") +
      theme_hc() +
      theme(legend.key.size = unit(1.5, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "gray70", fill = 'transparent'))
    
    print(vendas_graph)      
  })
  
  output$compras <- renderPlot({
    dadosCompletos <- SeriesTemporais()
    dadosValidacao <- subset(dadosCompletos, BASE == "Validacao")
    
    compras_graph <- ggplot(data = dadosCompletos, aes(x = ANO_MES_DIA)) +
      geom_line(aes(y = QTDE_COMPRA, colour = BASE), size = 1) +
      geom_point(aes(y = QTDE_COMPRA, colour = BASE), size = 1.4) +
      geom_line(data = dadosValidacao, aes(y = PREV_COMPRA, colour = "Predicao"), size = 1) +
      geom_point(data = dadosValidacao, aes(y = PREV_COMPRA, colour = "Predicao"), size = 1.4) +
      scale_colour_manual("", labels = c("Treino" = "Treino", 
                                         "Validacao" = "Validação", 
                                         "Predicao" = "Predição"),
                          values = c("Treino" = "#ED665D",
                                     "Validacao" = "#729ECE",
                                     "Predicao" = "#FF9E4A")) +
      expand_limits(y = 0) + 
      ylab("Compras") + xlab("") +
      theme_hc() +
      theme(legend.key.size = unit(1.5, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "gray70", fill = 'transparent'))
    
    print(compras_graph)      
  })
  
  output$estoque <- renderPlot({
    dadosCompletos <- SeriesTemporais()
    dadosValidacao <- subset(dadosCompletos, BASE == "Validacao")
    
    estoque_graph <- ggplot(data = dadosCompletos, aes(x = ANO_MES_DIA)) +
      geom_line(aes(y = QTDE_ESTOQUE, colour = BASE), size = 1) +
      geom_point(aes(y = QTDE_ESTOQUE, colour = BASE), size = 1.4) +
      geom_line(data = dadosValidacao, aes(y = PREV_ESTOQUE, colour = "Predicao"), size = 1) +
      geom_point(data = dadosValidacao, aes(y = PREV_ESTOQUE, colour = "Predicao"), size = 1.4) +
      scale_colour_manual("", labels = c("Treino" = "Treino", 
                                         "Validacao" = "Validação", 
                                         "Predicao" = "Predição"),
                          values = c("Treino" = "#ED665D",
                                     "Validacao" = "#729ECE",
                                     "Predicao" = "#FF9E4A")) +
      expand_limits(y = 0) + 
      ylab("Estoque") + xlab("") +
      theme_hc() +
      theme(legend.key.size = unit(1.5, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "gray70", fill = 'transparent'))
    
    print(estoque_graph)      
  })
  
})