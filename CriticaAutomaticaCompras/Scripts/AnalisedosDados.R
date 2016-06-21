rm(list=ls())
source('/home/milton/MiltonNote/UFMG-InfoSAS/3 - Construcao/EstimativaRoubo/Douglas/EstimativaRoubo/Relatorio/scripts/f_desc_mes_ano.R', echo = FALSE)
source('/home/milton/MiltonNote/UFMG-InfoSAS/3 - Construcao/EstimativaRoubo/Douglas/EstimativaRoubo/Relatorio/scripts/utilsEvidencia.R', echo = FALSE)
source('/home/milton/MiltonNote/UFMG-InfoSAS/3 - Construcao/EstimativaRoubo/Douglas/EstimativaRoubo/Relatorio/scripts/real.R', echo = FALSE)
source('/home/milton/MiltonNote/UFMG-InfoSAS/3 - Construcao/EstimativaRoubo/Douglas/EstimativaRoubo/Relatorio/scripts/par_grafico.R', echo = FALSE)
source('/home/milton/MiltonNote/UFMG-InfoSAS/3 - Construcao/EstimativaRoubo/Douglas/EstimativaRoubo/Relatorio/scripts/tree_map.R', echo = FALSE)

require(Cairo)
require(gtable)
require(ggplot2)
require(data.table)
require(mapsBR)
require(scales)

#
# 10 Produtos mais vendidos x mais comprados em 2011...2015 : Geral e por loja
#
{
  load(file="../Dados/varejao5anos/top10Vendas2011.RData")
  load(file="../Dados/varejao5anos/top10Vendas2012.RData")
  load(file="../Dados/varejao5anos/top10Vendas2013.RData")
  load(file="../Dados/varejao5anos/top10Vendas2014.RData")
  load(file="../Dados/varejao5anos/top10Vendas2015.RData")
  load(file="../Dados/varejao5anos/produtosMaisVendidos.RData")

  load(file="../Dados/varejao5anos/datas2011Top10.RData")  
  load(file="../Dados/varejao5anos/datas2012Top10.RData")  
  load(file="../Dados/varejao5anos/datas2013Top10.RData")  
  load(file="../Dados/varejao5anos/datas2014Top10.RData")  
  load(file="../Dados/varejao5anos/datas2015Top10.RData")  
  
  load(file="../Dados/varejao5anos/qtdeProd.RData")
  load(file="../Dados/varejao5anos/qtdeRegsEstoque.RData")
  
  load(file="../Dados/varejao5anos/matrizEstoque.RData")
}

#
# Visualização Anual, por semana, do produto mais vendido
#
{
  
  rEstoque <- 1;   rVendas <- 2 ;   rCompras <- 3
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
                  366)
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
  
}

#
# Monta objetos do relatório
#
{
  #
  # Página 1
  #
  {
  rectHeaderPag1 <- grid.rect(gp = gpar(fill = "blue", col = "blue"), 
                        vp = vplayout(1:3, 1:50), 
                        draw = FALSE) 

  gTituloPag1 <- grid.text(label = paste0("Análise descritiva das vendas, compras e estoque do Varejão : 2011-2015"),  
                         vjust = 1, hjust = 0.5, 
                         gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'white', fontsize = 12, alpha = 1), 
                         vp = vplayout(1:2, 1:50), 
                         draw = FALSE) 

  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.4)),
    rowhead = list(fg_params=list(cex = 0.4)))
  
  gTextTop2011 <- grid.text(label = paste0("Os 10 produtos mais vendidos em 2011"),  
                         vjust = 1, hjust = 0.5, 
                         gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                         vp = vplayout(5:6, 5:15), 
                         draw = FALSE) 
  top10Vendas2011 <- top10Vendas2011[,.(PRODUTO,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas2011 <- tableGrob(top10Vendas2011,
                                  vp = vplayout(18,1:20),theme = mytheme)
  top10Vendas2011 <- top10Vendas2011[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  
  gTextTop2012 <- grid.text(label = paste0("Os 10 produtos mais vendidos em 2012"),  
                            vjust = 1, hjust = 0.5, 
                            gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                            vp = vplayout(5:6, 25:50), 
                            draw = FALSE) 
  top10Vendas2012 <- top10Vendas2012[,.(PRODUTO,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas2012 <- tableGrob(top10Vendas2012,
                                  vp = vplayout(18,25:50),theme = mytheme)
  top10Vendas2012 <- top10Vendas2012[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  
  gTextTop2013 <- grid.text(label = paste0("Os 10 produtos mais vendidos em 2013"),  
                            vjust = 1, hjust = 0.5, 
                            gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'green', fontsize = 8, alpha = 1), 
                            vp = vplayout(29:30, 1:50), 
                            draw = FALSE) 
  top10Vendas2013 <- top10Vendas2013[,.(PRODUTO,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas2013 <- tableGrob(top10Vendas2013,
                                  vp = vplayout(42,1:50),theme = mytheme)
  top10Vendas2013 <- top10Vendas2013[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  
  gTextTop2014 <- grid.text(label = paste0("Os 10 produtos mais vendidos em 2014"),  
                            vjust = 1, hjust = 0.5, 
                            gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'blue', fontsize = 8, alpha = 1), 
                            vp = vplayout(53:54, 1:50), 
                            draw = FALSE) 
  top10Vendas2014 <- top10Vendas2014[,.(PRODUTO,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas2014 <- tableGrob(top10Vendas2014,
                                  vp = vplayout(66,1:50),theme = mytheme)
  top10Vendas2014 <- top10Vendas2014[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  
  gTextTop2015 <- grid.text(label = paste0("Os 10 produtos mais vendidos em 2015"),  
                            vjust = 1, hjust = 0.5, 
                            gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'deeppink4', fontsize = 8, alpha = 1), 
                            vp = vplayout(77:78, 1:50), 
                            draw = FALSE) 
  top10Vendas2015 <- top10Vendas2015[,.(PRODUTO,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas2015 <- tableGrob(top10Vendas2015,
                                  vp = vplayout(90,1:50),theme = mytheme)
  top10Vendas2015 <- top10Vendas2015[,.(PRODUTO,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  }
  #
  # Página 2
  #
  {
  rectHeaderpag2 <- grid.rect(gp = gpar(fill = "red", col = 'red'), 
                     vp = vplayout(1:3, 1:50), 
                     draw = FALSE) 
  
  gTituloPag2 <- grid.text(label = paste0("Rank dos 10 produtos mais vendidos : 2011-2015"),  
                         vjust = 1, hjust = 0.5, 
                         gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'white', fontsize = 12, alpha = 1), 
                         vp = vplayout(1:2, 1:50), 
                         draw = FALSE) 

  top10Vendas <- rbind(top10Vendas2011,top10Vendas2012,top10Vendas2013,top10Vendas2014,top10Vendas2015)
  top10Vendas <- top10Vendas[,RANK:=rank(-as.numeric(QTDE),ties.method="first"),by=ANO_VENDA]
  top10Vendas <- top10Vendas[,.(PRODUTO,RANK,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))][order(RANK,ANO_VENDA)]
  
  top10VendasRel <- top10Vendas[,.(PRODUTO,RANK,ANO_VENDA,GRADE,QTDE = format(QTDE,big.mark = ".",decimal.mark = ","),NO_PRODUTO)]
  tabTop10Vendas <- tableGrob(top10VendasRel,rows = NULL,
                              vp = vplayout(4:100,1:50),theme = mytheme)
  top10VendasRel <- top10Vendas[,.(PRODUTO,RANK,ANO_VENDA,GRADE,QTDE,NO_PRODUTO,PRODUTO_GRADE = paste0(PRODUTO,"-",GRADE))]
  
  tabProdMaisVendidos <- tableGrob(produtosMaisVendidos,rows = NULL,
                              vp = vplayout(4:46,1:10),theme = mytheme)
  
  }
  #
  # Página 3
  #
  {
  rectHeaderpag3 <- grid.rect(gp = gpar(fill = "black", col = 'gray'), 
                              vp = vplayout(1:3, 1:50), 
                              draw = FALSE) 
  
  gTituloPag3 <- grid.text(label = paste0("Confronto Vendas x Compras x Estoque dos 10 produtos mais vendidos : 2011-2014"),  
                           vjust = 1, hjust = 0.5, 
                           gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'white', fontsize = 12, alpha = 1), 
                           vp = vplayout(1:2, 1:50), 
                           draw = FALSE) 

  gTextDatas2011 <- grid.text(label = paste0("2011"),  
                            vjust = 1, hjust = 0.5, 
                            gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                            vp = vplayout(4:5, 1:50), 
                            draw = FALSE) 
  tabDatas2011Top10 <- tableGrob(datas2011Top10,rows = NULL,
                                  vp = vplayout(16,1:50),theme = mytheme)
  
  gTextDatas2012 <- grid.text(label = paste0("2012"),  
                              vjust = 1, hjust = 0.5, 
                              gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                              vp = vplayout(27:28, 1:50), 
                              draw = FALSE) 
  tabDatas2012Top10 <- tableGrob(datas2012Top10,rows = NULL,
                                 vp = vplayout(39,1:50),theme = mytheme)
  
  gTextDatas2013 <- grid.text(label = paste0("2013"),  
                              vjust = 1, hjust = 0.5, 
                              gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                              vp = vplayout(50:51, 1:50), 
                              draw = FALSE) 
  
  tabDatas2013Top10 <- tableGrob(datas2013Top10,rows = NULL,
                                 vp = vplayout(62,1:50),theme = mytheme)
  
  gTextDatas2014 <- grid.text(label = paste0("2014"),  
                              vjust = 1, hjust = 0.5, 
                              gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                              vp = vplayout(73:74, 1:50), 
                              draw = FALSE) 
  
  tabDatas2014Top10 <- tableGrob(datas2014Top10,rows = NULL,
                                 vp = vplayout(86,1:50),theme = mytheme)
  }
  #
  # Página 4
  #
  {
    rectHeaderpag4 <- grid.rect(gp = gpar(fill = "black", col = 'gray'), 
                                vp = vplayout(1:3, 1:50), 
                                draw = FALSE) 
    
    gTituloPag4 <- grid.text(label = paste0("Confronto Vendas x Compras x Estoque dos 10 produtos mais vendidos : 2015"),  
                             vjust = 1, hjust = 0.5, 
                             gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'white', fontsize = 12, alpha = 1), 
                             vp = vplayout(1:2, 1:50), 
                             draw = FALSE) 
    
    gTextDatas2015 <- grid.text(label = paste0("2015"),  
                                vjust = 1, hjust = 0.5, 
                                gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                                vp = vplayout(6:7, 1:50), 
                                draw = FALSE) 
    tabDatas2015Top10 <- tableGrob(datas2015Top10,rows = NULL,
                                   vp = vplayout(18,1:50),theme = mytheme)
    

    mytheme2 <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 0.6)),
      colhead = list(fg_params=list(cex = 0.6)),
      rowhead = list(fg_params=list(cex = 0.6)))

    gTextQtdeProd <- grid.text(label = paste0("Quantidade de produtos movimentados"),  
                                vjust = 1, hjust = 0.5, 
                                gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                                vp = vplayout(30:31, 1:19), 
                                draw = FALSE) 
    tabQtdeProd <- tableGrob(qtdeProd,rows = NULL,
                             vp = vplayout(37,1:19),theme = mytheme2)

    gTextQtdeRegsEstoque <- grid.text(label = paste0("Quantidade de registros de produtos em estoque"),  
                               vjust = 1, hjust = 0.5, 
                               gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'red', fontsize = 8, alpha = 1), 
                               vp = vplayout(30:31, 25:50), 
                               draw = FALSE) 

    tabQtdeRegsEstoque <- tableGrob(qtdeRegsEstoque, rows = NULL,
                                    vp = vplayout(38,25:50),theme = mytheme2)
    
  }
  #
  # Página 5
  #
  {
    rectHeaderpag5 <- grid.rect(gp = gpar(fill = "green", col = 'green'), 
                                vp = vplayout(1:3, 1:50), 
                                draw = FALSE) 
    
    gTituloPag5 <- grid.text(label = paste0("Evolução da movimentação (Estoque / Vendas / Compras) do produto mais vendido : 2012"),  
                             vjust = 1, hjust = 0.5, 
                             gp = gpar(fontfamily = fontFamily, fontface = 'bold', col = 'black', fontsize = 12, alpha = 1), 
                             vp = vplayout(1:2, 1:50), 
                             draw = FALSE) 
    
  }  
}  

#
# Gera Relatório
#
{
  Cairo(file = '../Relatorio/AnaliseDados', type = 'pdf',
      units = 'mm', width = 210, height = 297)

  #
  # Página 1
  #
  {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(100, 50)))
  grid.draw(rectHeaderPag1)
  grid.draw(gTituloPag1)
  grid.draw(gTextTop2011)
  grid.draw(tabTop10Vendas2011)
  grid.draw(gTextTop2012)
  grid.draw(tabTop10Vendas2012)
  grid.draw(gTextTop2013)
  grid.draw(tabTop10Vendas2013)
  grid.draw(gTextTop2014)
  grid.draw(tabTop10Vendas2014)
  grid.draw(gTextTop2015)
  grid.draw(tabTop10Vendas2015)
  }
  #
  # Página 2
  #
  {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(100, 50)))
  grid.draw(rectHeaderpag2)
  grid.draw(gTituloPag2)
  grid.draw(tabTop10Vendas)
  grid.draw(tabProdMaisVendidos)
  }
  #
  # Página 3
  #
  {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(100, 50)))
  grid.draw(rectHeaderpag3)
  grid.draw(gTituloPag3)
  grid.draw(gTextDatas2011)
  grid.draw(tabDatas2011Top10)
  grid.draw(gTextDatas2012)
  grid.draw(tabDatas2012Top10)
  grid.draw(gTextDatas2013)
  grid.draw(tabDatas2013Top10)
  grid.draw(gTextDatas2014)
  grid.draw(tabDatas2014Top10)
  }
  #
  # Página 4
  #
  {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(100, 50)))
    grid.draw(rectHeaderpag4)
    grid.draw(gTituloPag4)
    grid.draw(gTextDatas2015)
    grid.draw(tabDatas2015Top10)
    grid.draw(gTextQtdeProd)
    grid.draw(tabQtdeProd)
    grid.draw(gTextQtdeRegsEstoque)
    grid.draw(tabQtdeRegsEstoque)
  }
  #
  # Página 5
  #
  {
    # grid.newpage()
    # pushViewport(viewport(layout = grid.layout(100, 50)))
    # grid.draw(rectHeaderpag5)
    # grid.draw(gTituloPag5)
    # print(gEvolucaoEstoque1Vendas2012, vp = vplayout(5:100, 1:50))
    
  }  
  dev.off()
}
