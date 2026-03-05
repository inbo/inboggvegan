#
#  ggscreeplot.r
#
#  Copyright 2011 Vincent Q. Vu.
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

#' Create a screeplot for principal components and ordination analyses
#'
#' This function creates a ggplot-based screeplot showing the proportion of variance
#' explained by each component/axis in a multivariate analysis. It works with both
#' standard R PCA objects (prcomp, princomp) and vegan ordination objects (rda, cca, capscale).
#'
#' @param pcobj An ordination or PCA object. Can be one of:
#'   \itemize{
#'     \item prcomp - from \code{stats::prcomp()}
#'     \item princomp - from \code{stats::princomp()}
#'     \item rda, cca, capscale - from vegan package
#'     \item lda - from MASS package
#'   }
#' @param type Character string or vector specifying the type of screeplot:
#'   \itemize{
#'     \item "pev" - Proportion of Explained Variance (eigenvalues divided by total variance)
#'     \item "cev" - Cumulative Explained Variance (cumulative sum of proportions)
#'     \item "both" - Both pev and cev on the same plot (default if not specified)
#'   }
#'
#' @return A ggplot object showing the screeplot. The plot can be further customized
#'   using standard ggplot2 functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with prcomp
#' pca <- prcomp(USArrests, scale. = TRUE)
#' ggscreeplot(pca, type = "pev")
#' ggscreeplot(pca, type = "cev")
#' ggscreeplot(pca, type = "both")
#' 
#' # Customize the plot
#' library(ggplot2)
#' ggscreeplot(pca, type = "both") + 
#'   ggtitle("Screeplot for USArrests PCA") +
#'   theme_minimal()
#' 
#' # Example with vegan rda
#' library(vegan)
#' data(varespec)
#' vare.rda <- rda(varespec)
#' ggscreeplot(vare.rda, type = "both")
#' }
#'
ggscreeplot <- function(pcobj, type = c('pev', 'cev' ,'both'))
{
    type <- match.arg(type)
    if(type == "both"){
      type <- c("pev", "cev")
    }
    if (inherits(pcobj, c("rda", "cca", "capscale"))) {
      d <- c(pcobj$CCA$eig, pcobj$CA$eig)
      df <- expand.grid(
        PC = seq_along(d),
        Type = type
      )
    }
    if (inherits(pcobj, c("princomp", "prcomp", "lda"))) {
      d <- pcobj$sdev^2
      df <- expand.grid(
        PC = seq_along(d),
        Type = type
      )
    }



    df$yvar[df$Type == "pev"] <- d / sum(d)
    df$yvar[df$Type == "cev"] <- cumsum(d) / sum(d)
    df$Type <- factor(
      df$Type,
      levels = c("pev", "cev"),
      labels = c("proportion", "cumulative\nproportion")
    )

    ggplot(data = df, aes(x = PC, y = yvar, colour = Type)) +
        xlab('principal component number') +
        geom_point() + geom_line() +
        scale_y_continuous("proportion of explained variance", limits = 0:1, labels = scales::percent)
}
