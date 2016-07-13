#--- Séries temporais
rm(list=ls())
gc(reset = T)

#--- Setando o diretorio onde estão os arquivos relacionados à EAC
dirEAC <- "/home/douglas/Dropbox/Projetos/Neuk"
setwd(dirEAC)

#--- Pacotes
require(data.table)
require(ggplot2)
require(ggthemes)
require(tseries)
require(forecast)

#--- Carregando os dados
load(file = paste0(dirEAC, "/Dados/vendas.RData"))
vendas[, "DATA" := as.Date(DATA, format = "%d/%m/%Y")]
vendas[, "ANO" := year(DATA)]
vendas[, "MES" := month(DATA)]
vendas[, "DIA" := mday(DATA)]

analise = "MES"

if(analise == "DIA"){
  vendas <- vendas[, list(QTDE_ACUM = sum(QTDE),
                          VALOR_BRUTO_MES = sum(VALOR_BRUTO),
                          VALOR_LIQUIDO_MES = sum(VALOR_LIQUIDO)),
                   by =.(LOJA, ANO, MES, DIA, PRODUTO)][, ANO_MES_DIA := as.Date(paste(ANO, MES, DIA, sep = "/"), format = "%Y/%m/%d")]
  
} else{
  vendas <- vendas[, list(QTDE_ACUM = sum(QTDE),
                          VALOR_BRUTO_MES = sum(VALOR_BRUTO),
                          VALOR_LIQUIDO_MES = sum(VALOR_LIQUIDO)),
                   by =.(LOJA, ANO, MES, PRODUTO)][, ANO_MES_DIA := as.Date(paste(ANO, MES, "01", sep = "/"), format = "%Y/%m/%d")]
}

#--- Separando as bases em treino e validação
vendas_treino <- vendas[ANO != 2015][order(ANO_MES_DIA),]
vendas_validacao <- vendas[ANO == 2015][order(ANO_MES_DIA),]

#--- Setando o produto para análise
prod_analise <- "1T 1095"#"1T 1258" #"1P 55" #"1T 1095"
loja_analise <- 1

vendas_prod_treino <- vendas_treino[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Treino"]
vendas_prod_validacao <- vendas_validacao[PRODUTO == prod_analise & LOJA == loja_analise][order(ANO_MES_DIA),][, BASE := "Validacao"]
vendas_prod <- rbind(vendas_prod_treino, vendas_prod_validacao)

#--- Ajustando uma série temporal online
nPredicts <- nrow(vendas_prod_validacao)
predicao_prod <- data.frame(ANO_MES_DIA = vendas_prod_validacao$ANO_MES_DIA, 
                            PREDICAO = 0, 
                            LIMITE_INFERIOR_1 = 0, 
                            LIMITE_SUPERIOR_1 = 0,
                            LIMITE_INFERIOR_2 = 0, 
                            LIMITE_SUPERIOR_2 = 0)
vendas_ajuste <- vendas_prod_treino

for(i in 1:nPredicts){
  #--- Modelos
  model_prod <- auto.arima(x = vendas_ajuste$QTDE_ACUM, 
                           stationary = T,
                           d = 1)
  prev_0.95 <- forecast(object = model_prod, level = 0.95, h = 1)
  prev_0.99 <- forecast(object = model_prod, level = 0.99, h = 1)
  predicao_prod[i, -1] <- c(prev_0.95$mean, prev_0.95$lower, prev_0.95$upper, prev_0.99$lower, prev_0.99$upper)
  
  if(i != nPredicts){
    vendas_ajuste <- rbind(vendas_ajuste, vendas_prod_validacao[i,])
  }
}

#--- Grafico das vendas anuais com previsões

ggplot(data = vendas_prod, aes(x = ANO_MES_DIA)) +
  geom_line(aes(y = QTDE_ACUM, colour = BASE), size = 1) +
  geom_point(aes(y = QTDE_ACUM, colour = BASE), size = 1.4) +
  geom_line(data = predicao_prod, aes(y = PREDICAO, colour = "Predicao"), size = 1) +
  geom_point(data = predicao_prod, aes(y = PREDICAO, colour = "Predicao"), size = 1.4) +
  geom_ribbon(data = predicao_prod, aes(ymin = LIMITE_INFERIOR_1, ymax = LIMITE_SUPERIOR_1), alpha = 0.3) +
  #geom_ribbon(data = predicao_prod, aes(ymin = LIMITE_INFERIOR_2, ymax = LIMITE_SUPERIOR_2), alpha = 0.2) +
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
