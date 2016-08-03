#--- Definições gerais
dirEAC <- "~/MiltonNote/NeuK/EAC/CriticaAutomaticaCompras/Dados/"
setwd(dirEAC)

#--- Pacotes
require(data.table)     # Versão: 1.9.6     
require(ggplot2)        # Versão: 2.1.0
require(ggthemes)       # Versão: 3.0.3
require(tseries)        # Versão: 0.10-35
require(forecast)       # Versão: 7.1
require(dplyr)          # Versão: 0.4.3
require(plotly)         # 3.6.0
require(ggthemes)       # 3.0.3

#--- Carregando os dados
load(file = paste0(dirEAC, "vendas.RData"))
load(file = paste0(dirEAC, "compras.RData"))
load(file = paste0(dirEAC, "estoque.RData")) #Meu note não dá conta
load(file = paste0(dirEAC, "resumoProdutosMaisVendidos.RData"))


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

estoque[, "DATA_ESTOQUE" := as.Date(DATA_ESTOQUE, format = "%d/%m/%Y")]
estoque[, "ANO" := year(DATA_ESTOQUE)]
estoque[, "MES" := month(DATA_ESTOQUE)]
estoque[, "DIA" := mday(DATA_ESTOQUE)]

estoque <- estoque[, list(QTDE_ESTOQUE = sum(ESTOQUE)),
                   by =.(LOJA, ANO, MES, PRODUTO)][, ANO_MES_DIA := as.Date(paste(ANO, MES, "01", sep = "/"), format = "%Y/%m/%d")]
#-------------------------------
#--- Mudar quando houver estoque
#estoque <- compras 
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


#
# Dados utilizados na análise descritiva
#
dtAux <- data.frame(Estoque = rep(0,60), 
                    Vendas = rep(0,60), 
                    Compras = rep(0,60),
                    AnoMes = c(201101:201112,
                               201201:201212,
                               201301:201312,
                               201401:201412,
                               201501:201512))

dtAux$AnoMes <- as.character(dtAux$AnoMes)
Datas <- dtAux$AnoMes
Datas <- Datas[c(1,12,25,36,49,60)]
