library(data.table)
library(xtable)
library(stringr)
setwd("/home/ufmg/milton/MiltonNote/NeuK/varejao5anos")

#
# ESTOQUE
#
{
  estoque <- read.delim(file="/home/ufmg/milton/MiltonNote/NeuK/varejao5anos/estoque_varejao_5_anos.csv",sep="\t",header=FALSE,stringsAsFactors =FALSE)
  names(estoque) = c("ID","LOJA","FORNECEDOR","CENTRO_LUCRO","PRODUTO","GRADE","ESTOQUE","DATA_ESTOQUE","CUSTO_MEDIO")
  estoque <- as.data.table(estoque)
  save(estoque,file="estoque.RData")
  estoqueSubset <- subset(estoque, ID <= 100)
  save(estoqueSubset,file="estoqueSubset.RData")
  numRowsEst <- nrow(estoque)
  totCustoMedioEst <- sum(estoque$CUSTO_MEDIO)
  totEstoque <- sum(estoque$ESTOQUE)
  minDataEst <- min(estoque$DATA_ESTOQUE)
  maxDataEst <- max(estoque$DATA_ESTOQUE)
  load(file="estoqueSubset.RData")
}
#
# COMPRAS
#
{
  compras <- read.delim(file="/home/ufmg/milton/MiltonNote/NeuK/varejao5anos/compras_varejao_5_anos.csv",sep="\t",header=FALSE,stringsAsFactors =FALSE)
  names(compras) = c("ID","LOJA","FORNECEDOR","PRODUTO","GRADE","CENTRO_LUCRO","QTDE_RECEBIDA","DATA_ENTRADA","CUSTO_UNITARIO","ICMS","IPI","FRETE","ICMS_ST")
  compras <- as.data.table(compras)
  save(compras,file="compras.RData")
  comprasSubset <- subset(compras, ID <= 100)
  save(comprasSubset,file="comprasSubset.RData")
  
  numRowsComp <- nrow(compras)
  totCustoUnitarioComp <- sum(compras$CUSTO_UNITARIO)
  totCompras <- sum(compras$QTDE_RECEBIDA)
  minDataComp <- min(compras$DATA_ENTRADA)
  maxDataComp <- max(compras$DATA_ENTRADA)
  load(file="compras.RData")
  
  totCustoUnitarioComp <- format(round(sum(compras$CUSTO_UNITARIO),2),big.mark=".",decimal.mark=",",small.interval=2)
}
#
# VENDAS
#
{
  vendas <- read.delim(file="/home/ufmg/milton/MiltonNote/NeuK/varejao5anos/vendas_varejao_5_anos.csv",sep="\t",header=FALSE,stringsAsFactors =FALSE)
  names(vendas) = c("ID","LOJA","CLIENTE","CENTRO_LUCRO","PRODUTO","GRADE","QTDE","DATA","VALOR_BRUTO","VALOR_LIQUIDO","ACRESCIMO","DESCONTO","CMV")
  vendas <- as.data.table(vendas)
  save(vendas,file="vendas.RData")
  vendasSubset <- subset(vendas, ID <= 100)
  save(vendasSubset,file="vendasSubset.RData")
  
  numRowsVendas <- nrow(vendas)
  totVlrBrutoVendas <- sum(vendas$VALOR_BRUTO)
  totVlrLiquido <- sum(vendas$VALOR_LIQUIDO)
  minDataVendas <- min(vendas$DATA)
  maxDataVendas <- max(vendas$DATA)
  
  produtos <- read.delim(file="/home/milton/MiltonNote/NeuK/EAC/CriticaAutomaticaCompras/Dados/varejao5anos/produtos_varejao_5_anos.csv",sep="\t",header=FALSE,stringsAsFactors =FALSE)
  names(produtos) <- c("COD_PRODUTO","NO_PRODUTO")
  save(produtos,file="/home/milton/MiltonNote/NeuK/EAC/CriticaAutomaticaCompras/Dados/varejao5anos/produtos.RData")
  
  rmarkdown::render("Critica Automatica de Compras.Rmd", output_format="pdf_document",output_file="Critica_Automatica_de_Compras.pdf")
}

#
# Resumo
#
{
  resumoProdutos <- read.csv2(file = "../Dados/resumo.csv",sep = ",", header = FALSE, stringsAsFactors = FALSE)
  colnames(resumoProdutos) <- c("Produto","Loja","Compras","Vendas","Estoque","AnoMes")
  resumoProdutos <- as.data.table(resumoProdutos)
  load(file = "../Dados/produtosMaisVendidos.RData")
  produtosMaisVendidosR <- data.frame(produtosMaisVendidos)
  produtosMaisVendidosR <- produtosMaisVendidosR[,1]
  produtosMaisVendidosR <- sapply(strsplit(produtosMaisVendidosR, "\\_"), '[[', 1)
  save(produtosMaisVendidosR, file = "../Dados/produtosMaisVendidosR.RData")
  resumoProdutosMaisVendidos <- subset(resumoProdutos, Produto %in% produtosMaisVendidosR)
  save(resumoProdutosMaisVendidos, file = "../Dados/resumoProdutosMaisVendidos.RData")
}