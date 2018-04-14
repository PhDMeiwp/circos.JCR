#' Circular visualization of Journal Citing/Cited Relationships
#' 
#' circos.JCR is a extension of 'circlize' package for drawing the graph of Journal Citing/Cited Relationships. 
#' Graph example as http://ipscience-help.thomsonreuters.com/incitesLiveJCR/JCRGroup/jcrJournalProfile/jcrJournalProfileEgoNetwork.html

#' @param x Citing or cited data of JCR in sector.index1. The 1st value in dataset 'x' is the sector.index1 as default.
#' @param JCR.name Journal names (N.B., can NOT rename, i.e. No name is repeated each other).
#' @param start.degree For the start degree of the 1st sector.index. see \code{\link[circlize]{circos.par}}.
#' @param col The color for sector.index and circos.link.
#' @param order.by The direction of the circos, 0 = orde by the number of 'x', 1 = order by alphabet for 'JCR.name'.
#' @param text.facing The value for 'facing' in \code{\link[circlize]{circos.text}}.
#' @param text.size The font size of text. Value as cex (e.g., 0.8).
#' @param ... additional parameters to \code{\link[circlize]{circos.link}},such as directional, lty, arr.lty.
#'
#' @import graphics
#' @import stats
#' @import circlize
#' @export
#'
#' @examples
#'
#' library(circlize)
#' library(circos.JCR)
#' x = c(15, 11, 18, 16, 14, 13, 12, 7)  # an example of citing data of journal "Nature"
#' Name =c("Nature", "PLOS ONE", "SCIENCE", "CELL", "ECOLOGY", "OTHER1", "OTHER2", "OTHER3")
#' 
#' circos.JCR(x = x, JCR.name = Name, start.degree = 90, order.by = 0, text.size = 0.7)
#'
#' @author  Weiping Mei <meiweipingg@163.com>
#' @seealso \code{\link[circlize]{circos.link}}

circos.JCR<-
  function(x, JCR.name, start.degree = 90,
           col=1:length(x), order.by = 0,
           text.facing = "outside",text.size = 0.5,
           ...)
{
require(circlize)
set.seed(999)
circos.clear()

if (length(x) != length(JCR.name)){
  stop( "Length of 'x' and 'JCR.name' should be the same!")
  }

# order.by setting
JCR.name = factor(JCR.name)
dataset.orgin = data.frame(x, JCR.name)

data.1row = dataset.orgin[1, ]
colnames(data.1row) <- c("x", "JCR.name")

data.2.N = dataset.orgin[-1, ]
colnames(data.2.N) <- c("x", "JCR.name")

  if (order.by == 0){
  
    dataset0 = data.2.N[order(data.2.N$x, decreasing = TRUE),]
    dataset0 = rbind(data.1row, dataset0)
  
    x = dataset0$x
    Name =dataset0$JCR.name
   }

  if (order.by == 1){
  
    dataset1 = data.2.N[order(data.2.N$JCR.name), ]
    dataset1 = rbind(data.1row, dataset1)

    x = dataset1$x
    Name =dataset1$JCR.name

    }

# basic graphic
  cell_cycle = data.frame(phase = factor(Name, levels = Name),
                        hour = x)  
  color = col
  circos.par(start.degree = start.degree, gap.after=0)
  circos.initialize(cell_cycle$phase, xlim = cbind(rep(0, length(x)), cell_cycle$hour))
  circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2], 
               arrow.head.width = 0, arrow.head.length = 0,
               col = color[CELL_META$sector.numeric.index],
               border = NA)
  circos.text(CELL_META$xcenter, CELL_META$ycenter+ uy(5, "mm"), 
              CELL_META$sector.index,
              facing = text.facing, cex = text.size)
  }, bg.border = NA, track.height = 0.3)

# link width

  for (i in 2:length(x)){
    x = x
    x2 = x[-1]
    x2 = c(0,x2)
    x3 = x2/sum(x2) #sum(x3)==1
    x4 = x3*x[1]  # sum(x4)==x[1]

    circos.link(sector.index1=factor(Name[1]), c(sum(x4[1:i-1]),sum(x4[1:i])), 
              sector.index2=factor(Name[i]), c(0, x4[i]),
              col = color[i],border = NA,
              rou=0.8, ...)
  }
}
