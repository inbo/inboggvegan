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

#' Screeplot for Principal Components
#'
#' @param pcobj          an object returned by prcomp() or princomp()
#' @param type           the type of scree plot.  'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. 'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.
#' @export
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
