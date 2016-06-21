rm=(list=ls())
library(data.table)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)

#
# Produtos 
#
{
    load(file="../Dados/varejao5anos/produtos.RData")
    codProduto <- "1T 1095"
    subset(produtos,COD_PRODUTO == codProduto)
    produtos <- produtos[order(COD_PRODUTO)]
    ptn <- "1O95"
    ndx <- grep(ptn, produtos$COD_PRODUTO)
    p <- produtos[ndx,][order(COD_PRODUTO)]
}
#
# Vendas : Inclusão dos campos ANO_VENDA, MES_ANO_VENDA
#
{
    load(file="../Dados/varejao5anos/vendas.RData")
    vendas[, c("ANO_VENDA") := substr(vendas$DATA,7,10)]
    vendas[, c("MES_ANO_VENDA") := as.Date(paste0("01",substr(vendas$DATA,4,5),substr(vendas$DATA,7,10)),"%d%m%Y")]
    setkey(vendas,PRODUTO,GRADE)
    vendas$PRODUTO_GRADE <- paste0(vendas$PRODUTO,"-",vendas$GRADE, sep = "")
    sort(unique(vendas$MES_ANO_VENDA)) # meses com dados
    length(sort(unique(vendas$MES_ANO_VENDA))) # meses com dados
}    
    
#
# Vendas : Preparação dos dados por ano
#
{    
    vendas2011 <- subset(vendas, ANO_VENDA == 2011)
    vendas2012 <- subset(vendas, ANO_VENDA == 2012)
    vendas2013 <- subset(vendas, ANO_VENDA == 2013)
    vendas2014 <- subset(vendas, ANO_VENDA == 2014)
    vendas2015 <- subset(vendas, ANO_VENDA == 2015)
    save(vendas2011,file="../Dados/varejao5anos/vendas2011.RData")
    save(vendas2012,file="../Dados/varejao5anos/vendas2012.RData")
    save(vendas2013,file="../Dados/varejao5anos/vendas2013.RData")
    save(vendas2014,file="../Dados/varejao5anos/vendas2014.RData")
    save(vendas2015,file="../Dados/varejao5anos/vendas2015.RData")

    load(file="../Dados/varejao5anos/vendas2011.RData")
    load(file="../Dados/varejao5anos/vendas2012.RData")
    load(file="../Dados/varejao5anos/vendas2013.RData")
    load(file="../Dados/varejao5anos/vendas2014.RData")
    load(file="../Dados/varejao5anos/vendas2015.RData")
}

#
# Vendas : Preparação dos dados dos 10 produtos mais vendidos por Ano, Ano/Mes e respectivas vendas por data
#
{
    # 2011 : 10 Produtos mais vendidos : Geral
    load(file="../Dados/varejao5anos/vendas2011.RData")
    load(file="../Dados/varejao5anos/produtos.RData")
    vendas2011 <- vendas2011[,.(QTDE = sum(QTDE)),
                                by = .(ANO_VENDA,PRODUTO,GRADE,PRODUTO_GRADE)][order(QTDE, decreasing=T)]
    top10Vendas2011 <- vendas2011[,head(.SD,10)]
    top10Vendas2011 <- merge(top10Vendas2011,produtos,by.x = "PRODUTO", by.y = "COD_PRODUTO", all.x = TRUE,sort=FALSE)
    save(top10Vendas2011,file="../Dados/varejao5anos/top10Vendas2011.RData")
    rm(vendas2011)
    
    # 2012 : 10 Produtos mais vendidos : Geral
    load(file="../Dados/varejao5anos/vendas2012.RData")
    vendas2012 <- vendas2012[,.(QTDE = sum(QTDE)),
                             by = .(ANO_VENDA,PRODUTO,GRADE,PRODUTO_GRADE)][order(QTDE, decreasing=T)]
    top10Vendas2012 <- vendas2012[,head(.SD,10)]
    top10Vendas2012 <- merge(top10Vendas2012,produtos,by.x = "PRODUTO", by.y = "COD_PRODUTO", all.x = TRUE,sort=FALSE)
    save(top10Vendas2012,file="../Dados/varejao5anos/top10Vendas2012.RData")
    rm(vendas2012)

    # 2013 : 10 Produtos mais vendidos : Geral
    load(file="../Dados/varejao5anos/vendas2013.RData")
    vendas2013 <- vendas2013[,.(QTDE = sum(QTDE)),
                             by = .(ANO_VENDA,PRODUTO,GRADE,PRODUTO_GRADE)][order(QTDE, decreasing=T)]
    top10Vendas2013 <- vendas2013[,head(.SD,10)]
    top10Vendas2013 <- merge(top10Vendas2013,produtos,by.x = "PRODUTO", by.y = "COD_PRODUTO", all.x = TRUE,sort=FALSE)
    save(top10Vendas2013,file="../Dados/varejao5anos/top10Vendas2013.RData")
    rm(vendas2013)

    # 2014 : 10 Produtos mais vendidos : Geral
    load(file="../Dados/varejao5anos/vendas2014.RData")
    vendas2014 <- vendas2014[,.(QTDE = sum(QTDE)),
                             by = .(ANO_VENDA,PRODUTO,GRADE,PRODUTO_GRADE)][order(QTDE, decreasing=T)]
    top10Vendas2014 <- vendas2014[,head(.SD,10)]
    top10Vendas2014 <- merge(top10Vendas2014,produtos,by.x = "PRODUTO", by.y = "COD_PRODUTO", all.x = TRUE,sort=FALSE)
    save(top10Vendas2014,file="../Dados/varejao5anos/top10Vendas2014.RData")
    rm(vendas2014)
    
    # 2015 : 10 Produtos mais vendidos : Geral
    load(file="../Dados/varejao5anos/vendas2015.RData")
    vendas2015 <- vendas2015[,.(QTDE = sum(QTDE)),
                             by = .(ANO_VENDA,PRODUTO,GRADE,PRODUTO_GRADE)][order(QTDE, decreasing=T)]
    top10Vendas2015 <- vendas2015[,head(.SD,10)]
    top10Vendas2015 <- merge(top10Vendas2015,produtos,by.x = "PRODUTO", by.y = "COD_PRODUTO", all.x = TRUE,sort=FALSE)
    save(top10Vendas2015,file="../Dados/varejao5anos/top10Vendas2015.RData")
    rm(vendas2015)
    
    #
    top10Vendas <- rbind(top10Vendas2011,top10Vendas2012,top10Vendas2013,top10Vendas2014,top10Vendas2015)
    top10Vendas <- top10Vendas[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
    produtosMaisVendidos <- as.data.table(unique(top10Vendas$PRODUTO_GRADE))
    names(produtosMaisVendidos) <- c("PRODUTO_GRADE")
    save(produtosMaisVendidos,file="../Dados/varejao5anos/produtosMaisVendidos.RData")
    
    
    # Gera dados de vendas dos 10 mais vendidos
    load(file="../Dados/varejao5anos/vendas2013Top10Vendas.RData")
    vendas2012Top10Vendas <- subset(vendas,ANO_VENDA == 2012 & PRODUTO_GRADE %in% unique(top10Vendas2012$PRODUTO_GRADE))
    vendas2012Top10Vendas <- vendas2012Top10Vendas[, .(QTDE = sum(QTDE)),
                               by = .(DATA,MES_ANO_VENDA,PRODUTO_GRADE)]
    
    save(vendas2012Top10Vendas,file="vendas2012Top10Vendas.RData")
    
    vendas2013Top10Vendas <- subset(vendas,ANO_VENDA == 2013 & PRODUTO_GRADE %in% unique(top10Vendas2013$PRODUTO_GRADE))
    vendas2013Top10Vendas <- vendas2013Top10Vendas[, .(QTDE = sum(QTDE)),
                                                   by = .(DATA,MES_ANO_VENDA,PRODUTO_GRADE)]
    
    save(vendas2013Top10Vendas,file="vendas2013Top10Vendas.RData")

    load(file="../Dados/varejao5anos/vendas2014Top10Vendas.RData")
    vendas2014Top10Vendas <- subset(vendas,ANO_VENDA == 2014 & PRODUTO_GRADE %in% unique(top10Vendas2014$PRODUTO_GRADE))
    vendas2014Top10Vendas <- vendas2014Top10Vendas[, .(QTDE = sum(QTDE)),
                                                   by = .(DATA,MES_ANO_VENDA,PRODUTO_GRADE)]
    
    save(vendas2014Top10Vendas,file="vendas2014Top10Vendas.RData")

    load(file="../Dados/varejao5anos/vendas2015Top10Vendas.RData")
    vendas2015Top10Vendas <- subset(vendas,ANO_VENDA == 2015 & PRODUTO_GRADE %in% unique(top10Vendas2015$PRODUTO_GRADE))
    vendas2015Top10Vendas <- vendas2015Top10Vendas[, .(QTDE = sum(QTDE)),
                                                   by = .(DATA,MES_ANO_VENDA,PRODUTO_GRADE)]
    
    save(vendas2015Top10Vendas,file="vendas2015Top10Vendas.RData")
    
}

#
# Compras : Inclusão dos campos ANO_ENTRADA, MES_ANO_ENTRADA
#
{
    load(file="../Dados/varejao5anos/compras.RData")
    compras[, c("ANO_ENTRADA") := substr(compras$DATA_ENTRADA,7,10)]
    compras[, c("MES_ANO_ENTRADA") := as.Date(paste0("01",substr(compras$DATA_ENTRADA,4,5),substr(compras$DATA_ENTRADA,7,10)),"%d%m%Y")]
    setkey(compras,PRODUTO,GRADE)
    compras$PRODUTO_GRADE <- paste0(compras$PRODUTO,"-",compras$GRADE, sep = "")
    sort(unique(compras$MES_ANO_ENTRADA)) # meses com dados
    length(sort(unique(compras$MES_ANO_ENTRADA))) # meses com dados
    save(compras,file="../Dados/varejao5anos/compras.RData")
}

#
# Compras : Preparação dos dados por ano
#
{    
    compras2012 <- subset(compras, ANO_ENTRADA == 2012)
    compras2013 <- subset(compras, ANO_ENTRADA == 2013)
    compras2014 <- subset(compras, ANO_ENTRADA == 2014)
    compras2015 <- subset(compras, ANO_ENTRADA == 2015)
    save(compras2012,file="../Dados/varejao5anos/compras2012.RData")
    save(compras2013,file="../Dados/varejao5anos/compras2013.RData")
    save(compras2014,file="../Dados/varejao5anos/compras2014.RData")
    save(compras2015,file="../Dados/varejao5anos/compras2015.RData")

    load(file="../Dados/varejao5anos/compras2011.RData")
    load(file="../Dados/varejao5anos/compras2012.RData")
    load(file="../Dados/varejao5anos/compras2013.RData")
    load(file="../Dados/varejao5anos/compras2014.RData")
    load(file="../Dados/varejao5anos/compras2015.RData")
    
}

#
# Compras : Preparação dos dados dos 10 produtos mais vendidos por Ano/Mes/loja
#
{
  # 2012
  load(file="../Dados/varejao5anos/compras2012.RData")
  compras2012 <- compras2012[, .(QTDE_RECEBIDA = sum(QTDE_RECEBIDA)),
                             by = .(DATA_ENTRADA,MES_ANO_ENTRADA,PRODUTO_GRADE)]
  compras2012Top10Vendas <- subset(compras2012,PRODUTO_GRADE %in% unique(top10Vendas2012$PRODUTO_GRADE))
  save(compras2012Top10Vendas,file="compras2012Top10Vendas.RData")
  rm(compras2012)
  
  # 2013
  load(file="../Dados/varejao5anos/compras2013.RData")
  compras2013 <- compras2013[, .(QTDE_RECEBIDA = sum(QTDE_RECEBIDA)),
                             by = .(DATA_ENTRADA,MES_ANO_ENTRADA,PRODUTO_GRADE)]
  compras2013Top10Vendas <- subset(compras2013,PRODUTO_GRADE %in% unique(top10Vendas2013$PRODUTO_GRADE))
  save(compras2013Top10Vendas,file="compras2013Top10Vendas.RData")
  rm(compras2013)
  
  # 2014
  load(file="../Dados/varejao5anos/compras2014.RData")
  compras2014 <- compras2014[, .(QTDE_RECEBIDA = sum(QTDE_RECEBIDA)),
                             by = .(DATA_ENTRADA,MES_ANO_ENTRADA,PRODUTO_GRADE)]
  compras2014Top10Vendas <- subset(compras2014,PRODUTO_GRADE %in% unique(top10Vendas2014$PRODUTO_GRADE))
  save(compras2014Top10Vendas,file="compras2014Top10Vendas.RData")
  rm(compras2014)
  
  # 2015
  load(file="../Dados/varejao5anos/compras2015.RData")
  compras2015 <- compras2015[, .(QTDE_RECEBIDA = sum(QTDE_RECEBIDA)),
                             by = .(DATA_ENTRADA,MES_ANO_ENTRADA,PRODUTO_GRADE)]
  compras2015Top10Vendas <- subset(compras2015,PRODUTO_GRADE %in% unique(top10Vendas2015$PRODUTO_GRADE))
  save(compras2015Top10Vendas,file="compras2015Top10Vendas.RData")
  rm(compras2015)
}
#
# Estoque : Inclusão dos campos ANO_ESTOQUE, MES_ANO_ESTOQUE
#
{
    load(file="../Dados/varejao5anos/estoque.RData")
    estoque[, c("ANO_ESTOQUE") := substr(estoque$DATA_ESTOQUE,7,10)]
    estoque[, c("MES_ANO_ESTOQUE") := as.Date(estoque$DATA_ESTOQUE,"%d/%m/%Y")]
    setkey(estoque,PRODUTO,GRADE)
    estoque$PRODUTO_GRADE <- paste0(estoque$PRODUTO,"-",estoque$GRADE, sep = "")
    sort(unique(estoque$MES_ANO_ESTOQUE)) # meses com dados
    length(sort(unique(estoque$MES_ANO_ESTOQUE))) # meses com dados
    save(estoque,file="../Dados/varejao5anos/estoque.RData")

}

#
# Estoque : Preparação dos dados por ano
#
{
    estoque2011 <- subset(estoque, ANO_ESTOQUE == 2011)
    estoque2012 <- subset(estoque, ANO_ESTOQUE == 2012)
    estoque2013 <- subset(estoque, ANO_ESTOQUE == 2013)
    estoque2014 <- subset(estoque, ANO_ESTOQUE == 2014)
    estoque2015 <- subset(estoque, ANO_ESTOQUE == 2015)
    save(estoque2011,file="../Dados/varejao5anos/estoque2011.RData")
    save(estoque2012,file="../Dados/varejao5anos/estoque2012.RData")
    save(estoque2013,file="../Dados/varejao5anos/estoque2013.RData")
    save(estoque2014,file="../Dados/varejao5anos/estoque2014.RData")
    save(estoque2015,file="../Dados/varejao5anos/estoque2015.RData")

    load(file="../Dados/varejao5anos/estoque2011.RData")
    load(file="../Dados/varejao5anos/estoque2012.RData")
    load(file="../Dados/varejao5anos/estoque2013.RData")
    load(file="../Dados/varejao5anos/estoque2014.RData")
    load(file="../Dados/varejao5anos/estoque2015.RData")
}

#
# Estoque : Preparação dos dados dos 10 produtos mais vendidos por ano
#
{
  load(file="../Dados/varejao5anos/estoque2012.RData")
  load(file="../Dados/varejao5anos/estoque2013.RData")
  load(file="../Dados/varejao5anos/estoque2014.RData")
  load(file="../Dados/varejao5anos/estoque2015.RData")
  estoque2012Top10Vendas <- subset(estoque2012,PRODUTO_GRADE %in% unique(top10Vendas2012$PRODUTO_GRADE))
  estoque2013Top10Vendas <- subset(estoque2013,PRODUTO_GRADE %in% unique(top10Vendas2013$PRODUTO_GRADE))
  estoque2014Top10Vendas <- subset(estoque2014,PRODUTO_GRADE %in% unique(top10Vendas2014$PRODUTO_GRADE))
  estoque2015Top10Vendas <- subset(estoque2015,PRODUTO_GRADE %in% unique(top10Vendas2015$PRODUTO_GRADE))
  save(estoque2012Top10Vendas,file="../Dados/varejao5anos/estoque2012Top10Vendas.RData")
  save(estoque2013Top10Vendas,file="../Dados/varejao5anos/estoque2013Top10Vendas.RData")
  save(estoque2014Top10Vendas,file="../Dados/varejao5anos/estoque2014Top10Vendas.RData")
  save(estoque2015Top10Vendas,file="../Dados/varejao5anos/estoque2015Top10Vendas.RData")
}

# Compras, Vendas, Estoque : Seleção do produto mais vendido
{
    load(file="../Dados/varejao5anos/compras2012Top10Vendas.RData")
    compras2012Top1Vendas <- subset(compras2012Top10Vendas, PRODUTO_GRADE == "1T 1095-M")[, DADOS := 'Compras']
    compras2012Top1Vendas <- compras2012Top1Vendas[, .(DADOS,PRODUTO_GRADE, DATA_ENTRADA,MES_ANO_ENTRADA,QTDE_RECEBIDA)]
    setnames(compras2012Top1Vendas,c("DATA_ENTRADA","MES_ANO_ENTRADA","QTDE_RECEBIDA"),c("DATA","MES_ANO","QTDE"))
    save(compras2012Top1Vendas,file="../Dados/varejao5anos/compras2012Top1Vendas.RData")
    
    load(file="../Dados/varejao5anos/vendas2012Top10Vendas.RData")
    vendas2012Top1Vendas <-  subset(vendas2012Top10Vendas, PRODUTO_GRADE == "1T 1095-M")[, DADOS := 'Vendas']
    vendas2012Top1Vendas <- vendas2012Top1Vendas[, .(DADOS,PRODUTO_GRADE, DATA,MES_ANO_VENDA,QTDE)]
    setnames(vendas2012Top1Vendas,c("MES_ANO_VENDA"),c("MES_ANO"))
    save(vendas2012Top1Vendas,file="../Dados/varejao5anos/vendas2012Top1Vendas.RData")
    
    load(file="../Dados/varejao5anos/estoque2012Top10Vendas.RData")
    estoque2012Top1Vendas <- subset(estoque2012Top10Vendas, PRODUTO_GRADE == "1T 1095-M")[, DADOS := 'Estoque']
    estoque2012Top1Vendas <- estoque2012Top1Vendas[, .(DADOS,PRODUTO_GRADE, DATA_ESTOQUE,MES_ANO_ESTOQUE,ESTOQUE)]
    setnames(estoque2012Top1Vendas,c("DATA_ESTOQUE","MES_ANO_ESTOQUE","ESTOQUE"),c("DATA","MES_ANO","QTDE"))
    estoque2012Top1Vendas <- subset(estoque2012Top1Vendas, QTDE != 0) # ESTOQUE ZERADOS E COM DADOS NA MESMA DATA ####################################
    save(estoque2012Top1Vendas,file="../Dados/varejao5anos/estoque2012Top1Vendas.RData")

    estoque2011Top1Vendas <- subset(estoque2011, PRODUTO_GRADE == "1T 1095-M" & ESTOQUE != 0)[, DADOS := 'Estoque']
    save(estoque2011Top1Vendas,file="../Dados/varejao5anos/estoque2011Top1Vendas.RData")
    
}

#
# Datas inicial e final
#
{

  top10VendasRank <- top10Vendas[,RANK:=rank(-as.numeric(QTDE),ties.method="first"),by=ANO_VENDA]
  top10VendasRank <- top10Vendas[,.(PRODUTO,RANK,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))][order(RANK,ANO_VENDA)]
  
  load(file="../Dados/varejao5anos/vendas2012.RData")
  vendas2012Top10 <- subset(vendas2012, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasVendas2012Top10 <- vendas2012Top10[,.(DATA_INICIAL = format(min(as.Date(vendas2012Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             DATA_FINAL = format(max(as.Date(vendas2012Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             ANO_VENDA = min(ANO_VENDA),
                                             DADOS = max("Vendas")),
                                          by = .(PRODUTO_GRADE)]
  
  datasVendas2012Top10 <- merge(datasVendas2012Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_VENDA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasVendas2012Top10 <- datasVendas2012Top10[order(RANK)]
  datasVendas2012Top10 <- datasVendas2012Top10[,.(PRODUTO_GRADE,ANO_VENDA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(vendas2012)
  
  load(file="../Dados/varejao5anos/vendas2013.RData")
  vendas2013Top10 <- subset(vendas2013, PRODUTO_GRADE %in% top10Vendas2013$PRODUTO_GRADE)
  datasVendas2013Top10 <- vendas2013Top10[,.(DATA_INICIAL = format(min(as.Date(vendas2013Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             DATA_FINAL = format(max(as.Date(vendas2013Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             ANO_VENDA = min(ANO_VENDA),
                                             DADOS = max("Vendas")),
                                          by = .(PRODUTO_GRADE)]
  datasVendas2013Top10 <- merge(datasVendas2013Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_VENDA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasVendas2013Top10 <- datasVendas2013Top10[order(RANK)]
  datasVendas2013Top10 <- datasVendas2013Top10[,.(PRODUTO_GRADE,ANO_VENDA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(vendas2013)
  
  load(file="../Dados/varejao5anos/vendas2014.RData")
  vendas2014Top10 <- subset(vendas2014, PRODUTO_GRADE %in% top10Vendas2014$PRODUTO_GRADE)
  datasVendas2014Top10 <- vendas2014Top10[,.(DATA_INICIAL = format(min(as.Date(vendas2014Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             DATA_FINAL = format(max(as.Date(vendas2014Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             ANO_VENDA = min(ANO_VENDA),
                                             DADOS = max("Vendas")),
                                          by = .(PRODUTO_GRADE)]
  datasVendas2014Top10 <- merge(datasVendas2014Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_VENDA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasVendas2014Top10 <- datasVendas2014Top10[order(RANK)]
  datasVendas2014Top10 <- datasVendas2014Top10[,.(PRODUTO_GRADE,ANO_VENDA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(vendas2014)
  
  load(file="../Dados/varejao5anos/vendas2015.RData")
  vendas2015Top10 <- subset(vendas2015, PRODUTO_GRADE %in% top10Vendas2015$PRODUTO_GRADE)
  datasVendas2015Top10 <- vendas2015Top10[,.(DATA_INICIAL = format(min(as.Date(vendas2015Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             DATA_FINAL = format(max(as.Date(vendas2015Top10$DATA,"%d/%m/%Y")),"%d/%m/%Y"),
                                             ANO_VENDA = min(ANO_VENDA),
                                             DADOS = max("Vendas")),
                                          by = .(PRODUTO_GRADE)]
  datasVendas2015Top10 <- merge(datasVendas2015Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_VENDA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasVendas2015Top10 <- datasVendas2015Top10[order(RANK)]
  datasVendas2015Top10 <- datasVendas2015Top10[,.(PRODUTO_GRADE,ANO_VENDA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(vendas2015)
  
  load(file="../Dados/varejao5anos/compras2012.RData")
  compras2012Top10 <- subset(compras2012, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasCompras2012Top10 <- compras2012Top10[,.(DATA_INICIAL = format(min(as.Date(compras2012Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(compras2012Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ENTRADA = min(ANO_ENTRADA),
                                               DADOS = max("Compras")),
                                            by = .(PRODUTO_GRADE)]
  datasCompras2012Top10 <- merge(datasCompras2012Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ENTRADA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasCompras2012Top10 <- datasCompras2012Top10[order(RANK)]
  datasCompras2012Top10 <- datasCompras2012Top10[,.(PRODUTO_GRADE,ANO_ENTRADA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(compras2012)
  
  load(file="../Dados/varejao5anos/compras2013.RData")
  compras2013Top10 <- subset(compras2013, PRODUTO_GRADE %in% top10Vendas2013$PRODUTO_GRADE)
  datasCompras2013Top10 <- compras2013Top10[,.(DATA_INICIAL = format(min(as.Date(compras2013Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(compras2013Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ENTRADA = min(ANO_ENTRADA),
                                               DADOS = max("Compras")),
                                            by = .(PRODUTO_GRADE)]
  datasCompras2013Top10 <- merge(datasCompras2013Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ENTRADA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasCompras2013Top10 <- datasCompras2013Top10[order(RANK)]
  datasCompras2013Top10 <- datasCompras2013Top10[,.(PRODUTO_GRADE,ANO_ENTRADA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(compras2013)
  
  load(file="../Dados/varejao5anos/compras2014.RData")
  compras2014Top10 <- subset(compras2014, PRODUTO_GRADE %in% top10Vendas2014$PRODUTO_GRADE)
  datasCompras2014Top10 <- compras2014Top10[,.(DATA_INICIAL = format(min(as.Date(compras2014Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(compras2014Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ENTRADA = min(ANO_ENTRADA),
                                               DADOS = max("Compras")),
                                            by = .(PRODUTO_GRADE)]
  datasCompras2014Top10 <- merge(datasCompras2014Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ENTRADA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasCompras2014Top10 <- datasCompras2014Top10[order(RANK)]
  datasCompras2014Top10 <- datasCompras2014Top10[,.(PRODUTO_GRADE,ANO_ENTRADA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(compras2014)
  
  load(file="../Dados/varejao5anos/compras2015.RData")
  compras2015Top10 <- subset(compras2015, PRODUTO_GRADE %in% top10Vendas2015$PRODUTO_GRADE)
  datasCompras2015Top10 <- compras2015Top10[,.(DATA_INICIAL = format(min(as.Date(compras2015Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(compras2015Top10$DATA_ENTRADA,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ENTRADA = min(ANO_ENTRADA),
                                               DADOS = max("Compras")),
                                            by = .(PRODUTO_GRADE)]
  datasCompras2015Top10 <- merge(datasCompras2015Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ENTRADA"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasCompras2015Top10 <- datasCompras2015Top10[order(RANK)]
  datasCompras2015Top10 <- datasCompras2015Top10[,.(PRODUTO_GRADE,ANO_ENTRADA,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(compras2015)
  
  load(file="../Dados/varejao5anos/estoque2011.RData")
  estoque2011Top10 <- subset(estoque2011, PRODUTO_GRADE %in% top10Vendas2011$PRODUTO_GRADE)
  datasEstoque2011Top10 <- estoque2011Top10[,.(DATA_INICIAL = format(min(as.Date(estoque2011Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(estoque2011Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ESTOQUE = min(ANO_ESTOQUE),
                                               DADOS = max("Estoque")),
                                            by = .(PRODUTO_GRADE)]
  datasEstoque2011Top10 <- merge(datasEstoque2011Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ESTOQUE"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasEstoque2011Top10 <- datasEstoque2011Top10[order(RANK)]
  datasEstoque2011Top10 <- datasEstoque2011Top10[,.(PRODUTO_GRADE,ANO_ESTOQUE,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(estoque2011)
  
  load(file="../Dados/varejao5anos/estoque2012.RData")
  estoque2012Top10 <- subset(estoque2012, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasEstoque2012Top10 <- estoque2012Top10[,.(DATA_INICIAL = format(min(as.Date(estoque2012Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(estoque2012Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ESTOQUE = min(ANO_ESTOQUE),
                                               DADOS = max("Estoque")),
                                            by = .(PRODUTO_GRADE)]
  datasEstoque2012Top10 <- merge(datasEstoque2012Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ESTOQUE"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasEstoque2012Top10 <- datasEstoque2012Top10[order(RANK)]
  datasEstoque2012Top10 <- datasEstoque2012Top10[,.(PRODUTO_GRADE,ANO_ESTOQUE,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(estoque2012)
  
  load(file="../Dados/varejao5anos/estoque2013.RData")
  estoque2013Top10 <- subset(estoque2013, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasEstoque2013Top10 <- estoque2013Top10[,.(DATA_INICIAL = format(min(as.Date(estoque2013Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(estoque2013Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ESTOQUE = min(ANO_ESTOQUE),
                                               DADOS = max("Estoque")),
                                            by = .(PRODUTO_GRADE)]
  datasEstoque2013Top10 <- merge(datasEstoque2013Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ESTOQUE"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasEstoque2013Top10 <- datasEstoque2013Top10[order(RANK)]
  datasEstoque2013Top10 <- datasEstoque2013Top10[,.(PRODUTO_GRADE,ANO_ESTOQUE,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(estoque2013)
  
  load(file="../Dados/varejao5anos/estoque2014.RData")
  estoque2014Top10 <- subset(estoque2014, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasEstoque2014Top10 <- estoque2014Top10[,.(DATA_INICIAL = format(min(as.Date(estoque2014Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(estoque2014Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ESTOQUE = min(ANO_ESTOQUE),
                                               DADOS = max("Estoque")),
                                            by = .(PRODUTO_GRADE)]
  datasEstoque2014Top10 <- merge(datasEstoque2014Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ESTOQUE"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasEstoque2014Top10 <- datasEstoque2014Top10[order(RANK)]
  datasEstoque2014Top10 <- datasEstoque2014Top10[,.(PRODUTO_GRADE,ANO_ESTOQUE,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(estoque2014)
  
  load(file="../Dados/varejao5anos/estoque2015.RData")
  estoque2015Top10 <- subset(estoque2015, PRODUTO_GRADE %in% top10Vendas2012$PRODUTO_GRADE)
  datasEstoque2015Top10 <- estoque2015Top10[,.(DATA_INICIAL = format(min(as.Date(estoque2015Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               DATA_FINAL = format(max(as.Date(estoque2015Top10$DATA_ESTOQUE,"%d/%m/%Y")),"%d/%m/%Y"),
                                               ANO_ESTOQUE = min(ANO_ESTOQUE),
                                               DADOS = max("Estoque")),
                                            by = .(PRODUTO_GRADE)]
  datasEstoque2015Top10 <- merge(datasEstoque2015Top10,top10VendasRank,by.x=c("PRODUTO_GRADE","ANO_ESTOQUE"),by.y=c("PRODUTO_GRADE","ANO_VENDA"))
  datasEstoque2015Top10 <- datasEstoque2015Top10[order(RANK)]
  datasEstoque2015Top10 <- datasEstoque2015Top10[,.(PRODUTO_GRADE,ANO_ESTOQUE,DATA_INICIAL,DATA_FINAL,DADOS,RANK)]
  rm(estoque2015)
  
  datas2011Top10 <- merge(datasVendas2011Top10,datasEstoque2011Top10,
                          by.x = c("PRODUTO_GRADE","ANO_VENDA"), by.y = c("PRODUTO_GRADE","ANO_ESTOQUE"),
                          all.x = TRUE,
                          suffixes = c(".V",".E"))[,.(PRODUTO_GRADE,ANO = ANO_VENDA,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.E,DATA_FINAL.E,RANK = RANK.V)][order(PRODUTO_GRADE)]
  save(datas2011Top10,file="../Dados/varejao5anos/datas2011Top10.RData")  
  
  datas2012Top10 <- merge(datasVendas2012Top10,datasCompras2012Top10,
                          by.x = c("PRODUTO_GRADE","ANO_VENDA"), by.y = c("PRODUTO_GRADE","ANO_ENTRADA"),
                          all.x = TRUE,
                          suffixes = c(".V",".C"))[,.(PRODUTO_GRADE,ANO = ANO_VENDA,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,RANK = RANK.V )][order(PRODUTO_GRADE)]
  datas2012Top10 <- merge(datas2012Top10,datasEstoque2012Top10,
                          by.x = c("PRODUTO_GRADE","ANO"), by.y = c("PRODUTO_GRADE","ANO_ESTOQUE"),
                          all.x = TRUE)[,.(PRODUTO_GRADE,ANO,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,DATA_INICIAL.E = DATA_INICIAL,DATA_FINAL.E = DATA_FINAL,RANK = RANK.x)][order(PRODUTO_GRADE)]
  save(datas2012Top10,file="../Dados/varejao5anos/datas2012Top10.RData")  
  
  datas2013Top10 <- merge(datasVendas2013Top10,datasCompras2013Top10,
                          by.x = c("PRODUTO_GRADE","ANO_VENDA"), by.y = c("PRODUTO_GRADE","ANO_ENTRADA"),
                          all.x = TRUE,
                          suffixes = c(".V",".C"))[,.(PRODUTO_GRADE,ANO = ANO_VENDA,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,RANK = RANK.V )][order(PRODUTO_GRADE)]
  datas2013Top10 <- merge(datas2013Top10,datasEstoque2013Top10,
                          by.x = c("PRODUTO_GRADE","ANO"), by.y = c("PRODUTO_GRADE","ANO_ESTOQUE"),
                          all.x = TRUE)[,.(PRODUTO_GRADE,ANO,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,DATA_INICIAL.E = DATA_INICIAL,DATA_FINAL.E = DATA_FINAL,RANK = RANK.x)][order(PRODUTO_GRADE)]
  save(datas2013Top10,file="../Dados/varejao5anos/datas2013Top10.RData")  
  
  datas2014Top10 <- merge(datasVendas2014Top10,datasCompras2014Top10,
                          by.x = c("PRODUTO_GRADE","ANO_VENDA"), by.y = c("PRODUTO_GRADE","ANO_ENTRADA"),
                          all.x = TRUE,
                          suffixes = c(".V",".C"))[,.(PRODUTO_GRADE,ANO = ANO_VENDA,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,RANK = RANK.V )][order(PRODUTO_GRADE)]
  datas2014Top10 <- merge(datas2014Top10,datasEstoque2014Top10,
                          by.x = c("PRODUTO_GRADE","ANO"), by.y = c("PRODUTO_GRADE","ANO_ESTOQUE"),
                          all.x = TRUE)[,.(PRODUTO_GRADE,ANO,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,DATA_INICIAL.E = DATA_INICIAL,DATA_FINAL.E = DATA_FINAL,RANK = RANK.x)][order(PRODUTO_GRADE)]
  save(datas2014Top10,file="../Dados/varejao5anos/datas2014Top10.RData")  
  
  datas2015Top10 <- merge(datasVendas2015Top10,datasCompras2015Top10,
                          by.x = c("PRODUTO_GRADE","ANO_VENDA"), by.y = c("PRODUTO_GRADE","ANO_ENTRADA"),
                          all.x = TRUE,
                          suffixes = c(".V",".C"))[,.(PRODUTO_GRADE,ANO = ANO_VENDA,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,RANK = RANK.V )][order(PRODUTO_GRADE)]
  datas2015Top10 <- merge(datas2015Top10,datasEstoque2015Top10,
                          by.x = c("PRODUTO_GRADE","ANO"), by.y = c("PRODUTO_GRADE","ANO_ESTOQUE"),
                          all.x = TRUE)[,.(PRODUTO_GRADE,ANO,DATA_INICIAL.V,DATA_FINAL.V,DATA_INICIAL.C,DATA_FINAL.C,DATA_INICIAL.E = DATA_INICIAL,DATA_FINAL.E = DATA_FINAL,RANK = RANK.x)][order(PRODUTO_GRADE)]
  save(datas2015Top10,file="../Dados/varejao5anos/datas2015Top10.RData")  
}

#
# Qtde de produtos vendidos, comprados e com estoque : 2011-2015
#
{
  load(file="../Dados/varejao5anos/vendas2012.RData")
  qtdeProdVendidos2012 <- as.data.table(length(unique(vendas2012$PRODUTO_GRADE)))
  names(qtdeProdVendidos2012) <- c("A2012")
  qtdeProdVendidos2012 <- qtdeProdVendidos2012[,.(DADOS = "Vendas", A2012)]
  rm(vendas2012)
  
  load(file="../Dados/varejao5anos/vendas2013.RData")
  qtdeProdVendidos2013 <- as.data.table(length(unique(vendas2013$PRODUTO_GRADE)))
  names(qtdeProdVendidos2013) <- c("A2013")
  rm(vendas2013)
  
  load(file="../Dados/varejao5anos/vendas2014.RData")
  qtdeProdVendidos2014 <- as.data.table(length(unique(vendas2014$PRODUTO_GRADE)))
  names(qtdeProdVendidos2014) <- c("A2014")
  rm(vendas2014)
  
  load(file="../Dados/varejao5anos/vendas2015.RData")
  qtdeProdVendidos2015 <- as.data.table(length(unique(vendas2015$PRODUTO_GRADE)))
  names(qtdeProdVendidos2015) <- c("A2015")
  rm(vendas2015)
  
  qtdeProdVendidos <- cbind(qtdeProdVendidos2012,qtdeProdVendidos2013,qtdeProdVendidos2014,qtdeProdVendidos2015)
  
  load(file="../Dados/varejao5anos/compras2012.RData")
  qtdeProdComprados2012 <- as.data.table(length(unique(compras2012$PRODUTO_GRADE)))
  names(qtdeProdComprados2012) <- c("A2012")
  qtdeProdComprados2012 <- qtdeProdComprados2012[,.(DADOS = "Compras", A2012)]
  rm(compras2012)
  
  load(file="../Dados/varejao5anos/compras2013.RData")
  qtdeProdComprados2013 <- as.data.table(length(unique(compras2013$PRODUTO_GRADE)))
  names(qtdeProdComprados2013) <- c("A2013")
  rm(compras2013)
  
  load(file="../Dados/varejao5anos/compras2014.RData")
  qtdeProdComprados2014 <- as.data.table(length(unique(compras2014$PRODUTO_GRADE)))
  names(qtdeProdComprados2014) <- c("A2014")
  rm(compras2014)
  
  load(file="../Dados/varejao5anos/compras2015.RData")
  qtdeProdComprados2015 <- as.data.table(length(unique(compras2015$PRODUTO_GRADE)))
  names(qtdeProdComprados2015) <- c("A2015")
  rm(compras2015)
  
  qtdeProdComprados <- cbind(qtdeProdComprados2012,qtdeProdComprados2013,qtdeProdComprados2014,qtdeProdComprados2015)
  
  load(file="../Dados/varejao5anos/estoque2012.RData")
  qtdeProdEstoque2012 <- subset(estoque2012, ESTOQUE != 0)
  qtdeProdEstoque2012 <- as.data.table(length(unique(qtdeProdEstoque2012$PRODUTO_GRADE)))
  names(qtdeProdEstoque2012) <- c("A2012")
  qtdeProdEstoque2012 <- qtdeProdEstoque2012[,.(DADOS = "Estoque", A2012)]
  
  qtdeRegsEstoque2012Zerado <- as.numeric(nrow(subset(estoque2012, ESTOQUE == 0)))
  qtdeRegsEstoque2012Total  <- as.numeric(nrow(estoque2012))
  qtdeRegsEstoque2012 <- as.data.table(t(c(qtdeRegsEstoque2012Total,
                                           qtdeRegsEstoque2012Zerado,
                                           (qtdeRegsEstoque2012Total - qtdeRegsEstoque2012Zerado))))
  names(qtdeRegsEstoque2012) <- c("TOT_ESTOQUE","TOT_ZERADO","TOT_VALIDO")
  qtdeRegsEstoque2012 <- qtdeRegsEstoque2012[,.(DADOS = "Estoque-2012",
                                                TOT_ESTOQUE = format(TOT_ESTOQUE,big.mark = ".",decimal.mark = ","),
                                                TOT_ZERADO  = format(TOT_ZERADO,big.mark = ".",decimal.mark = ","),
                                                TOT_VALIDO = format(TOT_VALIDO, big.mark = ".",decimal.mark = ","))]
  rm(estoque2012)
  
  load(file="../Dados/varejao5anos/estoque2013.RData")
  qtdeProdEstoque2013 <- subset(estoque2013, ESTOQUE != 0)
  qtdeProdEstoque2013 <- as.data.table(length(unique(qtdeProdEstoque2013$PRODUTO_GRADE)))
  names(qtdeProdEstoque2013) <- c("A2013")
  
  qtdeRegsEstoque2013Zerado <- as.numeric(nrow(subset(estoque2013, ESTOQUE == 0)))
  qtdeRegsEstoque2013Total  <- as.numeric(nrow(estoque2013))
  qtdeRegsEstoque2013 <- as.data.table(t(c(qtdeRegsEstoque2013Total,
                                           qtdeRegsEstoque2013Zerado,
                                           (qtdeRegsEstoque2013Total - qtdeRegsEstoque2013Zerado))))
  names(qtdeRegsEstoque2013) <- c("TOT_ESTOQUE","TOT_ZERADO","TOT_VALIDO")
  qtdeRegsEstoque2013 <- qtdeRegsEstoque2013[,.(DADOS = "Estoque-2013",
                                                TOT_ESTOQUE = format(TOT_ESTOQUE,big.mark = ".",decimal.mark = ","),
                                                TOT_ZERADO  = format(TOT_ZERADO,big.mark = ".",decimal.mark = ","),
                                                TOT_VALIDO = format(TOT_VALIDO, big.mark = ".",decimal.mark = ","))]
  
  rm(estoque2013)
  
  load(file="../Dados/varejao5anos/estoque2014.RData")
  qtdeProdEstoque2014 <- subset(estoque2014, ESTOQUE != 0)
  qtdeProdEstoque2014 <- as.data.table(length(unique(qtdeProdEstoque2014$PRODUTO_GRADE)))
  names(qtdeProdEstoque2014) <- c("A2014")
  
  qtdeRegsEstoque2014Zerado <- as.numeric(nrow(subset(estoque2014, ESTOQUE == 0)))
  qtdeRegsEstoque2014Total  <- as.numeric(nrow(estoque2014))
  qtdeRegsEstoque2014 <- as.data.table(t(c(qtdeRegsEstoque2014Total,
                                           qtdeRegsEstoque2014Zerado,
                                           (qtdeRegsEstoque2014Total - qtdeRegsEstoque2014Zerado))))
  names(qtdeRegsEstoque2014) <- c("TOT_ESTOQUE","TOT_ZERADO","TOT_VALIDO")
  qtdeRegsEstoque2014 <- qtdeRegsEstoque2014[,.(DADOS = "Estoque-2014",
                                                TOT_ESTOQUE = format(TOT_ESTOQUE,big.mark = ".",decimal.mark = ","),
                                                TOT_ZERADO  = format(TOT_ZERADO,big.mark = ".",decimal.mark = ","),
                                                TOT_VALIDO = format(TOT_VALIDO, big.mark = ".",decimal.mark = ","))]
  
  rm(estoque2014)
  
  load(file="../Dados/varejao5anos/estoque2015.RData")
  qtdeProdEstoque2015 <- subset(estoque2015, ESTOQUE != 0)
  qtdeProdEstoque2015 <- as.data.table(length(unique(qtdeProdEstoque2015$PRODUTO_GRADE)))
  names(qtdeProdEstoque2015) <- c("A2015")
  
  qtdeRegsEstoque2015Zerado <- as.numeric(nrow(subset(estoque2015, ESTOQUE == 0)))
  qtdeRegsEstoque2015Total  <- as.numeric(nrow(estoque2015))
  qtdeRegsEstoque2015 <- as.data.table(t(c(qtdeRegsEstoque2015Total,
                                           qtdeRegsEstoque2015Zerado,
                                           (qtdeRegsEstoque2015Total - qtdeRegsEstoque2015Zerado))))
  names(qtdeRegsEstoque2015) <- c("TOT_ESTOQUE","TOT_ZERADO","TOT_VALIDO")
  qtdeRegsEstoque2015 <- qtdeRegsEstoque2015[,.(DADOS = "Estoque-2015",
                                                TOT_ESTOQUE = format(TOT_ESTOQUE,big.mark = ".",decimal.mark = ","),
                                                TOT_ZERADO  = format(TOT_ZERADO,big.mark = ".",decimal.mark = ","),
                                                TOT_VALIDO = format(TOT_VALIDO, big.mark = ".",decimal.mark = ","))]
  
  rm(estoque2015)
  
  qtdeProdEstoque <- cbind(qtdeProdEstoque2012,qtdeProdEstoque2013,qtdeProdEstoque2014,qtdeProdEstoque2015)
  qtdeProd <- rbind(qtdeProdEstoque,
                    qtdeProdVendidos,
                    qtdeProdComprados)
  qtdeProd  <- qtdeProd[,.(DADOS,
                           A2012 = format(A2012,big.mark = ".",decimal.mark = ","),
                           A2013 = format(A2013,big.mark = ".",decimal.mark = ","),
                           A2014 = format(A2014,big.mark = ".",decimal.mark = ","),
                           A2015 = format(A2015,big.mark = ".",decimal.mark = ","))]
  save(qtdeProd,file="../Dados/varejao5anos/qtdeProd.RData")
  
  qtdeRegsEstoque <- rbind(qtdeRegsEstoque2012,qtdeRegsEstoque2013,qtdeRegsEstoque2014,qtdeRegsEstoque2015)
  save(qtdeRegsEstoque,file="../Dados/varejao5anos/qtdeRegsEstoque.RData")
  
  
}

#############
#   INICIO
#############

# Carga dos Dados de Compras, Vendas. Estoque do produto mais vendido data/loja
{
  load(file="../Dados/varejao5anos/compras2012Top1Vendas.RData")
  load(file="../Dados/varejao5anos/vendas2012Top1Vendas.RData")
  load(file="../Dados/varejao5anos/estoque2012Top1Vendas.RData")
  load(file = "../Dados/varejao5anos/estoque2011Top1Vendas.RData")
  
}

# Calcula Evolução do estoque do produto mais vendido
{
  Jan <- 1; Fev <- 2; Mar <- 3; Abr <- 4; Mai <- 5; Jun <- 6; Jul <- 7; Ago <- 8; Set <- 9; Out <- 10; Nov <- 11; Dez <- 12
  rEstoque <- 1;   rVendas <- 2 ;   rCompras <- 3
  r2012 <- 1 ;   r2013 <- 2;   r2014 <- 3;  r2015 <- 4
  mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  diasMes <- matrix(c(31,29,31,30,31,30,31,31,30,31,30,31,
                      31,28,31,30,31,30,31,31,30,31,30,31,
                      31,28,31,30,31,30,31,31,30,31,30,31,
                      31,28,31,30,31,30,31,31,30,31,30,31),nrow=4,byrow=T,ncol=12)
  rownames(diasMes) <- c("2012","2013","2014","2015")
  colnames(diasMes) <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

  estoque2012Top1Vendas <- estoque2012Top1Vendas[, DIA := substr(estoque2012Top1Vendas$DATA,1,2)]
  estoque2012Top1Vendas <- estoque2012Top1Vendas[, MES := substr(estoque2012Top1Vendas$DATA,4,5)]
  # Cria coluna dia e mês nos dados
  vendas2012Top1Vendas <- vendas2012Top1Vendas[, DIA := substr(vendas2012Top1Vendas$DATA,1,2)]
  vendas2012Top1Vendas <- vendas2012Top1Vendas[, MES := substr(vendas2012Top1Vendas$DATA,4,5)]
  compras2012Top1Vendas <- compras2012Top1Vendas[,DIA := substr(compras2012Top1Vendas$DATA,1,2)]
  compras2012Top1Vendas <- compras2012Top1Vendas[,MES := substr(compras2012Top1Vendas$DATA,4,5)]
  # Criar matriz 3x31 colunas zerada
  matrizEstoque <- data.frame(Jan = I(matrix(0,nrow=3,ncol=diasMes[r2012,Jan],byrow=T)),
                              Fev = I(matrix(0,nrow=3,ncol=diasMes[r2012,Fev],byrow=T)),
                              Mar = I(matrix(0,nrow=3,ncol=diasMes[r2012,Mar],byrow=T)),
                              Abr = I(matrix(0,nrow=3,ncol=diasMes[r2012,Abr],byrow=T)),
                              Mai = I(matrix(0,nrow=3,ncol=diasMes[r2012,Mai],byrow=T)),
                              Jun = I(matrix(0,nrow=3,ncol=diasMes[r2012,Jun],byrow=T)),
                              Jul = I(matrix(0,nrow=3,ncol=diasMes[r2012,Jul],byrow=T)),
                              Ago = I(matrix(0,nrow=3,ncol=diasMes[r2012,Ago],byrow=T)),
                              Set = I(matrix(0,nrow=3,ncol=diasMes[r2012,Set],byrow=T)),
                              Out = I(matrix(0,nrow=3,ncol=diasMes[r2012,Out],byrow=T)),
                              Nov = I(matrix(0,nrow=3,ncol=diasMes[r2012,Nov],byrow=T)),
                              Dez = I(matrix(0,nrow=3,ncol=32,byrow=T)))
#                             Dez = I(matrix(0,nrow=3,ncol=diasMes[r2012,Dez],byrow=T)))
  rownames(matrizEstoque) <- c("Estoque","Vendas","Compras")
  # Preencher coluna de estoque 
  for ( i in 1:12) {
    matrizEstoque[rEstoque,as.numeric(mes[i])][as.numeric(subset(estoque2012Top1Vendas, MES == mes[i])$DIA)] <- subset(estoque2012Top1Vendas, MES == mes[i])$QTDE
  }
  matrizEstoque[rEstoque,as.numeric(mes[Jan])][1] <- subset(estoque2011Top1Vendas, DATA_ESTOQUE == "31/12/2011")$ESTOQUE
  matrizEstoque[rEstoque,as.numeric(mes[Dez])][32] <- subset(estoque2012Top1Vendas, DATA == "31/12/2012")$QTDE
  # Preencher colunas de vendas
  for (j in 1:12) {
    mVendas <- subset(vendas2012Top1Vendas, MES == mes[j])[,.(DADOS,DIA,QTDE)]
    mVendas <- acast(mVendas,DADOS ~ DIA, value.var='QTDE')
    for (i in 1:ncol(mVendas)) {
      c <- as.numeric(colnames(mVendas)[i])
      matrizEstoque[rVendas,as.numeric(mes[j])][c] <- mVendas[1,i] 
    }
    # Preencher colunas de compras
    mCompras <- subset(compras2012Top1Vendas, MES == mes[j])[,.(DADOS,DIA,QTDE)]
    mCompras <- acast(mCompras,DADOS ~ DIA, value.var='QTDE')
    for (i in 1:ncol(mCompras)) {
      c <- as.numeric(colnames(mCompras)[i])
      matrizEstoque[rCompras,as.numeric(mes[j])][c] <- mCompras[1,i] 
    }
  }
  # Loop nas colunas de meses
  for (j in 1:12) {
    # Loop nas colunas de dias
    for (k in 1:(diasMes[r2012,j]-1)) {
      # atualiza coluna de estoque subtraindo vendas e somando compras  
      matrizEstoque[rEstoque,j][k+1] <- matrizEstoque[rEstoque,j][k] - matrizEstoque[rVendas,j][k] + matrizEstoque[rCompras,j][k]
    }    
    if (j == 11){
      matrizEstoque[rEstoque,12][1] <- matrizEstoque[rEstoque,11][30] - matrizEstoque[rVendas,11][30] + matrizEstoque[rCompras,11][30]
    }
  }
  save(matrizEstoque,file="../Dados/varejao5anos/matrizEstoque.RData")
  write.table(matrizEstoque,file="../Dados/varejao5anos/matrizEstoque.csv",sep=";")
}


load(file="../Dados/varejao5anos/matrizEstoque.RData")
rEstoque <- 1;   rVendas <- 2 ;   rCompras <- 3

# Visualização Mensal do produto mais vendido
{

  # plotDadosEstoque <- as.data.table(subset(estoque2012Top1Vendas, MES_ANO == '2012-02-01'))
  # plotDadosVendas <- as.data.frame(subset(vendas2012Top1Vendas, MES_ANO == '2012-02-01'))
  # plotDadosCompras <- as.data.frame(subset(compras2012Top1Vendas, MES_ANO == '2012-02-01'))
  # tblv <- tableGrob(plotDadosVendas, rows=NULL)
  # tblc <- tableGrob(plotDadosCompras, rows=NULL)
  # tble <- tableGrob(plotDadosEstoque, rows=NULL)
  
  numMes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  nomeMes <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  i <- 3
  {
    for ( i in 1:12) {
      evolucaoEstoque <- as.data.frame(as.matrix(matrizEstoque[i]))
  
      plotDadosEstoque <- as.data.table(melt(evolucaoEstoque[rEstoque,]))[,DADOS :="Estoque"]
      plotDadosVendas <- as.data.table(melt(evolucaoEstoque[rVendas,]))[,DADOS :="Vendas"]
      plotDadosCompras <- as.data.table(melt(evolucaoEstoque[rCompras,]))[,DADOS :="Compras"]
      plotDados <- rbind(plotDadosEstoque, plotDadosVendas, plotDadosCompras)
      plotDados <- as.data.frame(plotDados)
      names(plotDados) <- c("DIA","QTDE","DADOS")

      noArq <- paste0("MaisVendido2012_",numMes[i])
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      
      g <- ggplot(plotDados,aes(x=DIA,y=QTDE, group=DADOS,color=DADOS,label=QTDE)) + 
        geom_line() +
        geom_point() +
        geom_text(hjust = 0, vjust = -0.1, size=2) +
        ggtitle(paste0("Evolucão estoque em ",nomeMes[i],"/2012 do produto mais vendido em 2012 : 1T 1095-M")) +
        theme(axis.text.x = element_text(angle=70,hjust=1,size=10),
              plot.title = element_text(lineheight=1.5, face="bold",colour = "red")) +
        theme(plot.margin=unit(c(0.5,0,0.5,0.5),"cm"))
      print (g)
      dev.off()
    }    
  }
}

#
# Visualização Anual, por semana, do produto mais vendido
#
{

  evolucaoEstoque <- as.data.frame(as.matrix(matrizEstoque[1:12]))

  # 2012
  diasSemana <- c(1,  8,    15,     22,     29,      
                  32, 39,   46,     53,     60,      
                  61, 68,   75,     82,     89,
                  92, 99,   106,    113,    120,
                  122,129,  136,    143,    150,
                  154,161,  168,    175,    182,
                  183,190,  197,    204,    211,
                  214,221,  228,    235,    242,
                  245,252,  259,    266,    273,
                  275,282,  289,    296,    303,
                  306,313,  320,    327,    334,
                  336,343,  350,    357,    364,
                  366,
                  367)
  evolucaoEstoque <- evolucaoEstoque[,diasSemana]
  
  diasRect <- c(6,11,16,21,26,31,36,41,46,51,56,61)
  
  plotDadosEstoque <- as.data.table(melt(evolucaoEstoque[rEstoque,]))[,DADOS :="Estoque"]
  plotDadosVendas <- as.data.table(melt(evolucaoEstoque[rVendas,]))[,DADOS :="Vendas"]
  plotDadosCompras <- as.data.table(melt(evolucaoEstoque[rCompras,]))[,DADOS :="Compras"]
  plotDados <- rbind(plotDadosEstoque, plotDadosVendas, plotDadosCompras)
  plotDados <- as.data.frame(plotDados)
  names(plotDados) <- c("DIA","QTDE","DADOS")
  
  Datas <- colnames(evolucaoEstoque)[c(diasRect)]
  
  rects <- function(Datas,alpha) {
     l <- list()
     r <- geom_rect(mapping=aes(xmin = Datas[1], xmax = Datas[2], ymin = -Inf, ymax = Inf), 
              fill = "gray",
              alpha = 0.15,
              colour="gray"
              ) 
     l <- append(l,r) 
     r <- geom_rect(mapping=aes(xmin = Datas[3], xmax = Datas[4], ymin = -Inf, ymax = Inf), 
                fill = "gray",
                alpha = 0.15,
                colour="gray") 
     l <- append(l,r) 
     r <- geom_rect(mapping=aes(xmin = Datas[5], xmax = Datas[6], ymin = -Inf, ymax = Inf), 
                     fill = "gray",
                     alpha = 0.15,
                     colour="gray") 
     l <- append(l,r) 
     r <- geom_rect(mapping=aes(xmin = Datas[7], xmax = Datas[8], ymin = -Inf, ymax = Inf), 
                     fill = "gray",
                     alpha = 0.15,
                     colour="gray") 
     l <- append(l,r) 
     r <- geom_rect(mapping=aes(xmin = Datas[9], xmax = Datas[10], ymin = -Inf, ymax = Inf), 
                     fill = "gray",
                     alpha = 0.15,
                     colour="gray") 
     l <- append(l,r) 
     r <- geom_rect(mapping=aes(xmin = Datas[11], xmax = Datas[12], ymin = -Inf, ymax = Inf), 
                    fill = "gray",
                    alpha = 0.15,
                    colour="gray") 
     l <- append(l,r) 
     return(l)
  }
    
  noFile <- "~/MiltonNote/NeuK/EAC/CriticaAutomaticaCompras/Relatorio/MaisVendido2012.pdf"
  pdf(width=12,height=12,file=noFile)

  ggplot(plotDados,aes(x=DIA,y=QTDE, group=DADOS,color=DADOS,label=QTDE)) + 
    rects(Datas) +
    geom_line() +
    geom_point() +
    geom_text(hjust = 0, vjust = -0.1, size=3) +
    theme(axis.text.x = element_text(angle=70,hjust=1,size=10)) 
  
  dev.off()
}
