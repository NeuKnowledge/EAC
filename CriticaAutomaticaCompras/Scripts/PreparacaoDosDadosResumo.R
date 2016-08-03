rm(list=ls())
library(data.table)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(plotly)
#
# Carga Resumo produtos mais vendidos
#
load(file = "../Dados/resumoProdutosMaisVendidos.RData")
load(file = "../Dados/produtosMaisVendidosR.RData")


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

#############
#   INICIO
#############


# Visualização Mensal do produto mais vendido
{
  geraGraficoLinha <- function (pplotDados, pDatas, pCoProduto, pTitulo, pCoLoja, pFlagPontos = TRUE) {
    g <- ggplot(pplotDados,aes(x=AnoMes,y=Qtde, group=Dados,label=Qtde)) +
      geom_rect(mapping=aes(xmin = pDatas[1], xmax = pDatas[2], ymin = -Inf, ymax = Inf), 
                fill = "gray90",
                alpha = 0.05,
                colour="gray90") + 
      geom_rect(mapping=aes(xmin = pDatas[3], xmax = pDatas[4], ymin = -Inf, ymax = Inf), 
                fill = "gray90",
                alpha = 0.05,
                colour="gray90") + 
      geom_rect(mapping=aes(xmin = pDatas[5], xmax = pDatas[6], ymin = -Inf, ymax = Inf), 
                fill = "gray90",
                alpha = 0.05,
                colour="gray90") + 
      geom_line(aes(linetype = Dados, color = Dados)) +
      ggtitle(paste0(pTitulo," produto : ",pCoProduto," - Loja : ",pCoLoja)) +
      theme(axis.text.x = element_text(angle=70,hjust=1,size=10),
            plot.title = element_text(lineheight=1.5, face="bold",colour = "black")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5),"cm")) +
      scale_size_manual(values=c(1, 1.5)) +
      geom_hline(yintercept = 0)
    if (pFlagPontos) {
      g <- g  +     
        geom_point(aes(color = Dados)) +
        geom_text(hjust = 0, vjust = -0.1, size=2) 
    }
    return(g)
  }

  geraBoxPlot <- function (pplotDados, pCoProduto, pTitulo, pCoLoja) {
    pplotDados <- as.data.table(pplotDados)[,.(Ano = substr(AnoMes,1,4), Dados, Qtde)]
    g <- ggplot(pplotDados,aes(x=Ano,y=Qtde)) +
      geom_boxplot(show.legend = TRUE) +
      stat_summary(fun.y=sd,col='blue',geom='point', aes(shape = "desvio padrão")) +
      stat_summary(fun.y=mean,col='red',geom='point', aes(shape="média")) +
      ggtitle(paste0(pTitulo," produto : ",pCoProduto," - Loja : ",pCoLoja)) +
      theme(axis.text.x = element_text(angle=70,hjust=1,size=10),
            plot.title = element_text(lineheight=1.5, face="bold",colour = "black")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5),"cm")) +
      scale_size_manual(values=c(1, 1.5)) + 
      coord_flip()
    return(g)
  }

  gplotDados <- NULL
  printGraficos <- function(pProdutosMaisVendidos, pNoListaProdutos, pFlagPontos, pTipoGrafico) {
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
    Datas <- Datas[c(1,12,24,36,48,60)]
    coLoja <- 1
    listaVendas <- list()
    listaCompras <- list()
    listaEstoqueCalculado <- list()
    listaDifComprasVendas <- list()
    for ( i in 1:length(pProdutosMaisVendidos)) {
      coProduto <- pProdutosMaisVendidos[i]
      resumoProduto <- subset(resumoProdutosMaisVendidos, Produto == coProduto & Loja == coLoja)[,.(Compras,Vendas,Estoque,AnoMes)]
      resumoProduto <- rbind(resumoProduto,dtAux)
      resumoProduto <- as.data.frame(resumoProduto[,.(Compras = sum(Compras),
                                                      Vendas = sum(Vendas),
                                                      Estoque = sum(Estoque)), by=.(AnoMes)])
      resumoProduto <- as.data.table(arrange(resumoProduto,AnoMes))
      resumoProduto <- resumoProduto[, .(Compras, Vendas, Estoque, AnoMes, "Estoque Calculado" = Compras - Vendas, "Dif Compras X Vendas" = Compras - Vendas)]
      for (j in 2:nrow(resumoProduto)) {
        if (resumoProduto[j,]$"Estoque Calculado" != 0){
          resumoProduto[j,]$"Estoque Calculado" <- resumoProduto[j-1,]$"Estoque Calculado" + resumoProduto[j,]$Compras  - resumoProduto[j,]$Vendas
        } else {
          resumoProduto[j,]$"Estoque Calculado" <- resumoProduto[j-1,]$"Estoque Calculado" 
        }
      }
      plotDadosEstoque <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Estoque")))
      plotDadosVendas <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Vendas")))
      plotDadosCompras <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Compras")))
      plotDadosEstoqueCalculado <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Estoque Calculado")))
      plotDadosDifComprasVendas <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Dif Compras X Vendas")))
      plotDados <- rbind(plotDadosEstoque, plotDadosVendas, plotDadosCompras, plotDadosEstoqueCalculado, plotDadosDifComprasVendas)
      plotDados <- as.data.frame(plotDados)
      names(plotDados) <- c("AnoMes","Dados","Qtde")
      gplotDados <<- plotDados
      # noArq <- paste0("ResumoProduto_",coProduto,"_Loja_",coLoja,"Dif")
      # noFile <- paste0("../Relatorio/",noArq,".pdf")
      # pdf(width=12,height=12,file=noFile)
      
      if (pTipoGrafico == "Linha") {
        dadosMantidos <- c("Estoque","Vendas","Compras","Dif Compras X Vendas")
        g <- geraGraficoLinha(pplotDados = subset(plotDados, Dados %in% dadosMantidos),
                              pDatas = Datas,
                              pTitulo = "Dif Compras X Vendas",
                              pCoProduto = coProduto,
                              pCoLoja = coLoja,
                              pFlagPontos)
        g <- g +
          scale_color_manual(values=c("Estoque" = "darkgreen", 
                                      "Vendas" = "blue", 
                                      "Compras" = "red", 
                                      "Dif Compras X Vendas" = "black")) +
          scale_linetype_manual(values=c("Estoque" =  "solid", 
                                         "Vendas" = "solid", 
                                         "Compras" = "solid",
                                         "Dif Compras X Vendas" = "blank")) 
        listaDifComprasVendas[[i]] <- g
        # print (g)
        # dev.off()
        
        # noArq <- paste0("ResumoProduto_",coProduto,"_Loja_",coLoja,"EstCalc")
        # noFile <- paste0("../Relatorio/",noArq,".pdf")
        # pdf(width=12,height=12,file=noFile)
        
        dadosMantidos <- c("Estoque","Vendas","Compras","Estoque Calculado")
        g <- geraGraficoLinha(pplotDados = subset(plotDados, Dados %in% dadosMantidos),
                         pDatas = Datas,
                         pTitulo = "Estoque Calculado",
                         pCoProduto = coProduto,
                         pCoLoja = coLoja,
                         pFlagPontos)
        g <- g +
          scale_color_manual(values=c("Estoque" = "darkgreen", 
                                      "Vendas" = "blue", 
                                      "Compras" = "red", 
                                      "Estoque Calculado" = "black")) +
          scale_linetype_manual(values=c("Estoque" =  "solid", 
                                         "Vendas" = "solid", 
                                         "Compras" = "solid",
                                         "Estoque Calculado" = "dashed"))
        
        listaEstoqueCalculado[[i]] <- g
        # print (g)
        # dev.off()
        
        dadosMantidos <- c("Vendas")
        # noArq <- paste0("ResumoProduto_",coProduto,"_Loja_",coLoja,"_",dadosMantidos[1],dadosMantidos[2])
        # noFile <- paste0("../Relatorio/",noArq,".pdf")
        # pdf(width=12,height=12,file=noFile)
        g <- geraGraficoLinha(pplotDados = subset(plotDados, Dados %in% dadosMantidos),
                         pDatas = Datas,
                         pTitulo = "Vendas",
                         pCoProduto = coProduto,
                         pCoLoja = coLoja,
                         pFlagPontos)
        g <- g +
          scale_color_manual(values=c("Vendas" = "blue")) +
          scale_linetype_manual(values=c("Vendas" = "solid"))
        listaVendas[[i]] <- g
        # print (g)
        
        dadosMantidos <- c("Compras")
        g <- geraGraficoLinha(pplotDados = subset(plotDados, Dados %in% dadosMantidos),
                         pDatas = Datas,
                         pTitulo = "Compras",
                         pCoProduto = coProduto,
                         pCoLoja = coLoja,
                         pFlagPontos)
        g <- g +
          scale_color_manual(values=c("Compras" = "red")) +
          scale_linetype_manual(values=c("Compras" = "solid"))
        
        listaCompras[[i]] <- g
        
        # print (g)
        # dev.off()
      } else if (pTipoGrafico == "Boxplot") {
        tipoDado <- "Vendas"
        g <- geraBoxPlot(pplotDados = subset(plotDados, Dados == tipoDado), 
                         pTitulo = paste0("Box Plot ",tipoDado),
                         pCoProduto = coProduto,
                         pCoLoja = coLoja)
        listaVendas[[i]] <- g
        tipoDado <- "Compras"
        g <- geraBoxPlot(pplotDados = subset(plotDados, Dados == tipoDado), 
                         pTitulo = paste0("Box Plot ",tipoDado),
                         pCoProduto = coProduto,
                         pCoLoja = coLoja)
        listaCompras[[i]] <- g
      }
    }
    
    if (pTipoGrafico == "Linha") {
      noArq <- paste0("ResumoProdutos_",pNoListaProdutos,"_Loja_",coLoja,"EstoqueCalculado")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaEstoqueCalculado)
      dev.off()
      
      noArq <- paste0("ResumoProdutos_",pNoListaProdutos,"_Loja_",coLoja,"DifComprasVendas")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaDifComprasVendas)
      dev.off()
      
      
      noArq <- paste0("ResumoProdutos_",pNoListaProdutos,"_Loja_",coLoja,"Compras")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaCompras)
      dev.off()
      
      noArq <- paste0("ResumoProdutos_",pNoListaProdutos,"_Loja_",coLoja,"Vendas")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaVendas)
      dev.off()
    } else if (pTipoGrafico == "Boxplot") {
      noArq <- paste0("BoxplotProdutos_",pNoListaProdutos,"_Loja_",coLoja,"Compras")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaCompras)
      dev.off()
      
      noArq <- paste0("BoxplotProdutos_",pNoListaProdutos,"_Loja_",coLoja,"Vendas")
      noFile <- paste0("../Relatorio/",noArq,".pdf")
      pdf(width=12,height=12,file=noFile)
      do.call(what = grid.arrange, args = listaVendas)
      dev.off()
      
    }
  }
    
  tipoGrafico <- "Linha"
  {
    produtosMaisVendidos <- produtosMaisVendidosR[1:6]
    noListaProdutos <- "1:6"
    flagPontos <- FALSE
    printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
    
    produtosMaisVendidos <- produtosMaisVendidosR[7:12]
    noListaProdutos <- "7:12"
    flagPontos <- FALSE
    printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
  
    produtosMaisVendidos <- produtosMaisVendidosR[13:18]
    noListaProdutos <- "13:18"
    flagPontos <- FALSE
    printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
    
    produtosMaisVendidos <- produtosMaisVendidosR[19:21]
    noListaProdutos <- "19:21"
    flagPontos <- FALSE
    printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
  
    for (i in 1:length(produtosMaisVendidosR)) {
      produtosMaisVendidos <- produtosMaisVendidosR[i]
      noListaProdutos <- produtosMaisVendidosR[i]
      flagPontos <- TRUE
      printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
    }
  }
  tipoGrafico <- "Boxplot"
  { 
    i <- 1
    for (i in 1:length(produtosMaisVendidosR)) {
      produtosMaisVendidos <- produtosMaisVendidosR[i]
      noListaProdutos <- produtosMaisVendidosR[i]
      flagPontos <- NULL
      printGraficos(produtosMaisVendidos, noListaProdutos, flagPontos, tipoGrafico)
    }
  }    
}

