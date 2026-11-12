## ----settings, echo = FALSE---------------------------------------------------
#knitr::opts_chunk$set(
#  message = FALSE,
#  fig.width = 7.1,
#  fig.height = 5
#)

## ----fig.width = 8, fig.height = 8, fig.cap = "GGscreeplots"------------------
#library(ggplot2)
#library(gridExtra)
#pcobj <- princomp(USArrests, cor = TRUE)
#p1 <- ggscreeplot(pcobj, type = "pev")
#p2 <- ggscreeplot(pcobj, type = "cev")
#p3 <- ggscreeplot(pcobj, type = "both")
#p4 <- ggscreeplot(pcobj, type = "both") + xlab("PC") + 
#          scale_y_continuous("PEV", limits = 0:1, 
#                             labels = scales::percent) +
#                              scale_color_manual(values=c("blue","orange")) +
#          ggtitle("Screeplot for the PCA of USarrests")
#grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)

## ----simpleExample, warning=FALSE, error=FALSE,message=FALSE------------------
#library(vegan)
#library(ggplot2)
#data(varespec)
#data(varechem)
#vare.cca <- cca(varespec, varechem)
#plot(vare.cca) #standard plot
#ggbiplot.vegan(vare.cca) #ggplot


