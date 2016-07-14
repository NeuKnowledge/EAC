rm(list=ls())
library(data.table)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)

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
  dtAux <- rbind(data.frame(AnoMes = c(201101:201112,
                                       201201:201212,
                                       201301:201312,
                                       201401:201412,
                                       201501:201512), Dados = "Estoque", Qtde = rep(0,60)),
                 data.frame(AnoMes = c(201101:201112,
                                       201201:201212,
                                       201301:201312,
                                       201401:201412,
                                       201501:201512), Dados = "Vendas", Qtde = rep(0,60)),
                 data.frame(AnoMes = c(201101:201112,
                                       201201:201212,
                                       201301:201312,
                                       201401:201412,
                                       201501:201512), Dados = "Compras", Qtde = rep(0,60)),
                 data.frame(AnoMes = c(201101:201112,
                                       201201:201212,
                                       201301:201312,
                                       201401:201412,
                                       201501:201512), Dados = "Dif Compras X Vendas", Qtde = rep(0,60)),
                 data.frame(AnoMes = c(201101:201112,
                                       201201:201212,
                                       201301:201312,
                                       201401:201412,
                                       201501:201512), Dados = "Estoque Calculado", Qtde = rep(0,60)))
  dtAux$AnoMes <- as.character(dtAux$AnoMes)
  
  rEstoque <- 1;   rVendas <- 2 ;   rCompras <- 3
  coLoja <- 1
  for ( i in 1:length(produtosMaisVendidosR)) {
    coProduto <- produtosMaisVendidosR[i]
    resumoProduto <- subset(resumoProdutosMaisVendidos, Produto == coProduto & Loja == coLoja)
    resumoProduto <- resumoProduto[, .(Produto, Loja, Compras, Vendas, Estoque, AnoMes, "Estoque Calculado" = Compras - Vendas, "Dif Compras X Vendas" = Compras - Vendas)]
    for (j in 2:nrow(resumoProduto)) {
      resumoProduto[j,]$"Estoque Calculado" <- resumoProduto[j-1,]$"Estoque Calculado" + resumoProduto[j,]$Compras  - resumoProduto[j,]$Vendas
    }
    plotDadosEstoque <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Estoque")))
    plotDadosVendas <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Vendas")))
    plotDadosCompras <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Compras")))
    plotDadosEstoqueCalculado <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Estoque Calculado")))
    plotDadosDifComprasVendas <- as.data.table(melt(resumoProduto, id.vars = c("AnoMes"), measure.vars = c("Dif Compras X Vendas")))
    plotDados <- rbind(plotDadosEstoque, plotDadosVendas, plotDadosCompras, plotDadosEstoqueCalculado, plotDadosDifComprasVendas)
    plotDados <- as.data.frame(plotDados)
    names(plotDados) <- c("AnoMes","Dados","Qtde")
    plotDados <- as.data.table(rbind(plotDados,dtAux))
    plotDados$AnoMes <- as.character(plotDados$AnoMes)
    setkey(plotDados, AnoMes, Dados)
    
    plotDados <- as.data.frame(plotDados[,.(Qtde = sum(Qtde)), by=.(AnoMes,Dados)])

    noArq <- paste0("ResumoProduto_",coProduto,"_Loja_",coLoja,"Dif")
    noFile <- paste0("../Relatorio/",noArq,".pdf")
    pdf(width=12,height=12,file=noFile)
    
    dadosRetirado <- "Estoque Calculado"
    dadosPermanece <- "Dif Compras X Vendas"
    g <- ggplot(subset(plotDados, Dados != dadosRetirado),aes(x=AnoMes,y=Qtde, group=Dados,label=Qtde)) + 
      geom_line(aes(linetype = Dados, color = Dados)) +
      geom_point(aes(color = Dados)) +
      geom_text(hjust = 0, vjust = -0.1, size=2) +
      ggtitle(paste0("Evolucão produto : ",coProduto," - Loja : ",coLoja)) +
      theme(axis.text.x = element_text(angle=70,hjust=1,size=10),
            plot.title = element_text(lineheight=1.5, face="bold",colour = "black")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5),"cm")) +
      scale_color_manual(values=c("Estoque" = "darkgreen", 
                                  "Vendas" = "blue", 
                                  "Compras" = "red", 
                                  "Dif Compras X Vendas" = "black")) +
      scale_linetype_manual(values=c("Estoque" =  "solid", 
                                     "Vendas" = "solid", 
                                     "Compras" = "solid",
                                     "Dif Compras X Vendas" = "blank")) +
      scale_size_manual(values=c(1, 1.5))
    print (g)
    dev.off()
    noArq <- paste0("ResumoProduto_",coProduto,"_Loja_",coLoja,"EstCalc")
    noFile <- paste0("../Relatorio/",noArq,".pdf")
    pdf(width=12,height=12,file=noFile)
    
    dadosRetirado <- "Dif Compras X Vendas"
    g <- ggplot(subset(plotDados, Dados != dadosRetirado),aes(x=AnoMes,y=Qtde, group=Dados,label=Qtde)) + 
      geom_line(aes(linetype = Dados, color = Dados)) +
      geom_point(aes(color = Dados)) +
      geom_text(hjust = 0, vjust = -0.1, size=2) +
      ggtitle(paste0("Evolucão produto : ",coProduto," - Loja : ",coLoja)) +
      theme(axis.text.x = element_text(angle=70,hjust=1,size=10),
            plot.title = element_text(lineheight=1.5, face="bold",colour = "black")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5),"cm")) +
      scale_color_manual(values=c("Estoque" = "darkgreen", 
                                  "Vendas" = "blue", 
                                  "Compras" = "red", 
                                  "Estoque Calculado" = "black")) +
      scale_linetype_manual(values=c("Estoque" =  "solid", 
                                     "Vendas" = "solid", 
                                     "Compras" = "solid",
                                     "Estoque Calculado" = "dashed")) +
      scale_size_manual(values=c(1, 1.5))
    print (g)
    dev.off()
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
