require(ggplot2)
require(ggthemes)

load("~/Downloads/matrizEstoque.RData")

Quebras <- rep(0, 11)
QTDn_aux <- 0

for(i in 1:12){
  nMes <- ncol(matrizEstoque[,i])
  QTD_mes1 <- matrizEstoque[,i][,1][1]
  QTD_mesn <- matrizEstoque[,i][,nMes]
  QTD_mesn <- QTD_mesn[1] - QTD_mesn[2] + QTD_mesn[3]
  
  if(i != 1){
    Quebras[i-1] <- ((QTDn_aux - QTD_mes1)/QTDn_aux)*100
  }
  
  QTDn_aux <- QTD_mesn
}

meses <- names(matrizEstoque)
breaks <- paste(meses[-12], meses[-1], sep = "/")

df <- data.frame(Mes = factor(breaks), Aux = 1:length(breaks), Quebra = Quebras)

pdf("~/Dropbox/Projetos/Neuk/Relatorio/Quebras.pdf")
ggplot(df, aes(x = Aux, y = Quebra/100)) +
  geom_line(colour = "IndianRed", size = 1.5) +
  xlab("") + ylab("Quebra") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = breaks, breaks = df$Aux) +
  theme_hc()
dev.off()
