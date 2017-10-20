


ggbiplot <- function(x, ...){
  UseMethod("ggbiplot")
}


ggbiplot.princomp <- 
  function (x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, ...) {
  if (length(choices) != 2L) 
    stop("length of choices must be 2")
  if (!length(scores <- x$scores)) 
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
          domain = NA)
  lam <- x$sdev[choices]
  if (is.null(n <- x$n.obs)) 
    n <- 1
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) 
    warning("'scale' is outside [0, 1]")
  if (scale != 0) 
    lam <- lam^scale
  else lam <- 1
  if (pc.biplot) 
    lam <- lam/sqrt(n)
  p <- ggbiplot.default(t(t(scores[, choices])/lam), 
                        t(t(x$loadings[, choices]) * lam), ...)
  p
}



ggbiplot.prcomp <- function(x, choices = 1:2, scale = 1, pc.biplot = FALSE, 
                            colordata = NULL, type = "p", ...){
  if (length(choices) != 2L) 
    stop("length of choices must be 2")
  if (!length(scores <- x$x)) 
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
         domain = NA)
  if (is.complex(scores)) 
    stop("biplots are not defined for complex PCA")
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) 
    warning("'scale' is outside [0, 1]")
  if (scale != 0) 
    lam <- lam^scale
  else lam <- 1
  if (pc.biplot) 
    lam <- lam/sqrt(n)
  p <- ggbiplot.default(t(t(scores[, choices])/lam), t(t(x$rotation[, choices]) * lam), 
                   colordata=colordata, ...)
  p
}


ggbiplot.default <-
function (x, y, choices = 1:2, var.axes = TRUE, col, cex = rep(par("cex"), 2), pch = 16,
          xlabs = NULL, ylabs = NULL, expand = 1, xlim = NULL, ylim = NULL, 
          arrow.len = 0.1, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
          colordata = NULL, type = "p", jitter = 0, 
          ...) 
{
  
  ### prepare data
  
  n <- nrow(x)
  p <- nrow(y)
  if (missing(xlabs)) {
    xlabs <- dimnames(x)[[1L]]
    if (is.null(xlabs)) 
      xlabs <- 1L:n
  }
  xlabs <- as.character(xlabs)
  dimnames(x) <- list(xlabs, dimnames(x)[[2L]])
  if (missing(ylabs)) {
    ylabs <- dimnames(y)[[1L]]
    if (is.null(ylabs)) 
      ylabs <- paste("Var", 1L:p)
  }
  ylabs <- as.character(ylabs)
  dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
  if (length(cex) == 1L) 
    cex <- c(cex, cex)
  if (missing(col)) {
    col <- par("col")
    if (!is.numeric(col)) 
      col <- match(col, palette(), nomatch = 1L)
    col <- c(col, col + 1L)
  }
  else if (length(col) == 1L) 
    col <- c(col, col)
  unsigned.range <- function(x) c(-abs(min(x, na.rm = TRUE)), 
                                  abs(max(x, na.rm = TRUE)))
  rangx1 <- unsigned.range(x[, 1L])
  rangx2 <- unsigned.range(x[, 2L])
  rangy1 <- unsigned.range(y[, 1L])
  rangy2 <- unsigned.range(y[, 2L])
  if (missing(xlim) && missing(ylim)) 
    xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
  else if (missing(xlim)) 
    xlim <- rangx1
  else if (missing(ylim)) 
    ylim <- rangx2
  ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
  
  xlim[1] <- ifelse(xlim[1]>0, xlim[1]*.85, xlim[1]*1.15)
  xlim[2] <- ifelse(xlim[2]<0, xlim[2]*.85, xlim[2]*1.15)
  ylim[1] <- ifelse(ylim[1]>0, ylim[1]*.85, ylim[1]*1.15)
  ylim[2] <- ifelse(ylim[2]<0, ylim[2]*.85, ylim[2]*1.15)

  ### prepare and plot scores
    
    if (length(colordata) && nrow(colordata) == nrow(x)){
      colorname = names(colordata)[1]
      dfx <- cbind(as.data.frame(x), colordata, xlabs = xlabs)
      dfx[,choices[1]] <- jitter(dfx[,choices[1]], factor = jitter)
      dfx[,choices[2]] <- jitter(dfx[,choices[2]], factor = jitter)        
      
      p <- ggplot(dfx, aes_string(x = names(dfx)[1], y = names(dfx)[2])) + 
        xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) + ggtitle(main) + coord_fixed()
      
      
      if (type == "p"){
        p <- p + geom_point(aes_string(color = colorname), shape = pch)
      } else {
        p <- p + geom_text(aes_string(labels = "xlabs", color = colorname))
      }
      
    } else { #if no colordata
      dfx <- cbind(as.data.frame(x), xlabs = xlabs)
      dfx[,choices[1]] <- jitter(dfx[,choices[1]], factor = jitter)
      dfx[,choices[2]] <- jitter(dfx[,choices[2]], factor = jitter)        

      p <- ggplot(dfx, aes_string(x = names(dfx)[choices[1]], y = names(dfx)[choices[2]])) + 
        xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) + ggtitle(main) 
      if (type == "p"){
        p <- p + geom_point(shape = pch)
      } else {
        p <- p + geom_text(aes(label = xlabs))
      }
    }  
  
  
  ### prepare and plot loadings
    
    dfy <- cbind(as.data.frame(y/ratio), vars = rownames(y))
    dfys <- dfy
    dfys[,1:2] <- dfys[,1:2]*0.8
    p <- p + geom_text(data=dfy, aes_string(x = names(dfy)[1], y = names(dfy)[2], label = "vars"), 
                       inherit.aes = FALSE, color = col[2])
    p <- p +  geom_segment(data=dfys, aes_string(x=0,y=0,xend = names(dfy)[1], yend = names(dfy)[2]),
                           arrow = arrow(length = unit(0.2,"cm")), inherit.aes = FALSE, color = col[2])
  
    p #print is nodig omdat de oproepende functies enkel
}



##########################

# Data <- read.csv2("G:\\Mijn Drive\\SUPPORT\\2017\\Kristine\\DataPCAPieter.csv")
# pcadata <- Data[c("UIV","LIV","PL","LE")]
# colordata <- Data[,"moSPadj", drop = FALSE]
# 
# pca <- prcomp(pcadata, scale. = TRUE)
# summary(pca)
# print(pca)
# screeplot(pca)
# 
# ggbiplot(pca)
# ggbiplot(pca, colordata = colordata)
# ggbiplot(pca, pch = 1, colordata = colordata )
# ggbiplot(pca, colordata = colordata, jitter = 3 , pch = 1) + facet_wrap(~moSPadj)
# ggbiplot(pca, choices = c(1,3), colordata = colordata, jitter = 3, pch = 1)
# ggbiplot(pca, choices = c(1,3), colordata = colordata, jitter = 4, pch = 1) + facet_wrap(~moSPadj)
# 
# 
# ggbiplot(prcomp(iris[,1:4]), choices = 1:2, colordata = iris[,5,drop=FALSE])
# ggbiplot(princomp(iris[,1:4]), choices = 1:2, colordata = iris[,5,drop=FALSE])

