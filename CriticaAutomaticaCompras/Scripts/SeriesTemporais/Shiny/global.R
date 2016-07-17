#--- Definições gerais
dirEAC <- "~/Documents/Github/Neuk/EAC/CriticaAutomaticaCompras/Data/"
setwd(dirEAC)

#--- Pacotes
require(data.table)     # Versão: 1.9.6     
require(ggplot2)        # Versão: 2.1.0
require(ggthemes)       # Versão: 3.0.3
require(tseries)        # Versão: 0.10-35
require(forecast)       # Versão: 7.1

#--- Carregando os dados
load(file = paste0(dirEAC, "vendas.RData"))
load(file = paste0(dirEAC, "compras.RData"))
#load(file = paste0(dirEAC, "estoque.RData")) #Meu note não dá conta

#--- Manipulando a data
vendas[, "DATA" := as.Date(DATA, format = "%d/%m/%Y")]
vendas[, "ANO" := year(DATA)]
vendas[, "MES" := month(DATA)]
vendas[, "DIA" := mday(DATA)]
compras[, "DATA_ENTRADA" := as.Date(DATA_ENTRADA, format = "%d/%m/%Y")]
compras[, "ANO" := year(DATA_ENTRADA)]
compras[, "MES" := month(DATA_ENTRADA)]
compras[, "DIA" := mday(DATA_ENTRADA)]

vendas <- vendas[, list(QTDE_VENDA = sum(QTDE)),
                 by =.(LOJA, ANO, MES, PRODUTO)][, ANO_MES_DIA := as.Date(paste(ANO, MES, "01", sep = "/"), format = "%Y/%m/%d")]

compras <- compras[, list(QTDE_COMPRA = sum(QTDE_RECEBIDA)),
                   by =.(LOJA, ANO, MES, PRODUTO)][, ANO_MES_DIA := as.Date(paste(ANO, MES, "01", sep = "/"), format = "%Y/%m/%d")]

#-------------------------------
#--- Mudar quando houver estoque
estoque <- compras 
#-------------------------------

#--- Separando as bases em treino e validação
vendas_treino <- vendas[ANO != 2015][order(ANO_MES_DIA),]
vendas_validacao <- vendas[ANO == 2015][order(ANO_MES_DIA),]
compras_treino <- compras[ANO != 2015][order(ANO_MES_DIA),]
compras_validacao <- compras[ANO == 2015][order(ANO_MES_DIA),]
estoque_treino <- estoque[ANO != 2015][order(ANO_MES_DIA),]
estoque_validacao <- estoque[ANO == 2015][order(ANO_MES_DIA),]

load("produtosMaisVendidosR.RData")
produtos <- as.list(unique(vendas$PRODUTO)[unique(vendas$PRODUTO) %in% produtosMaisVendidosR])
