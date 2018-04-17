#' Circular visualization of Journal Citing/Cited Relationships
#'
#' circos.JCR is a extension of 'circlize' package for drawing the graph of Journal Citing/Cited Relationships.
#' Graph example as http://ipscience-help.thomsonreuters.com/incitesLiveJCR/JCRGroup/jcrJournalProfile/jcrJournalProfileEgoNetwork.html

#' @param x Citing or cited data of JCR in sector.index1. The 1st value in dataset 'x' is the sector.index1 as default.
#' @param JCR.name Journal names (N.B., can NOT rename, i.e. No name is repeated each other).
#' @param col The color for sector.index and circos.link.
#' @param transparency Transparency of color.
#' @param order.by The direction of the circos, 0 = orde by the number of 'x', 1 = order by alphabet for 'JCR.name'.
#' @param self.include Whether the link include the parent journal. Default is FALSE.
#' @param start.degree For the start degree of the 1st sector.index. see \code{\link[circlize]{circos.par}}.
#' @param text.size The font size of text. Value as cex (e.g., 0.8).
#' @param track.height The height of one track.index.
#' @param ... additional parameters to \code{\link[circlize]{circos.link}},such as directional, rou, rou1, tou2, lty, arr.lty.
#'
#' @import circlize
#' @import grDevices
#' @import stats

#' @export

#' @examples
#'
#' library(circos.JCR)
#' # an example of citing data of journal "Nature", the top 20 cited journals data included.
#' NatureCiting = c(4665, 2719, 1757, 1279, 713, 630, 556, 445, 441, 413,
#'                  402,  387,  367,  359,  351, 337, 323, 316, 314, 312, 292)
#' # Top 20 cited journals by the parent journal "Nature"
#' JCR.name =c("Nature", "SCIENCE", "P NATL ACAD SCI USA", "CELL", "PHYS REV LETT",
#'             "ASTROPHYS J", "J BIOL CHEM", "NAT COMMUN", "NUCLEIC ACIDS RES", "PLOS ONE",
#'              "NEURON", "J AM CHEM SOC", "J NEUROSCI", "BIOINFORMATICS", "MON NOT R ASTRON.",
#'              "ACTA CRYSTALLOGR D", "NAT METHODS", "MOL CELL", "NAT GENET", "ASTRON ASTROPHYS", "NAT NEUROSCI")
#' # "Citing data (not exact data)" by the top 20 journals.
#' Allciting = c(7156,  6564,  12214, 2818, 968,
#'               19399, 7772, 10489, 6570, 27664,
#'               2251,  3487, 6329,  1475, 28571,
#'               501, 600, 1198, 668, 10747, 2460)
#'
#' circos.JCR(x.cite = NatureCiting, JCR.name = JCR.name, x.allcite = Allciting, start.degree = 90, order.by = 0, text.size = 0.7)
#'
#' @author  Weiping Mei <meiweipingg@163.com>
#' @seealso \code{\link[circlize]{circos.link}}

circos.JCR<-
  function(x.cite, JCR.name, x.allcite,
           col = NULL, transparency = 0.8,
           order.by = 0, text.size = 0.7,
           self.include = FALSE, start.degree = 90,
           draw.line = FALSE,
           ...)
  {

    require(circlize)
    set.seed(999)
    circos.clear()

    if (length(x.cite) != length(JCR.name) | length(JCR.name) != length(x.allcite)){
      stop( "Length of 'x.cite', 'x.allcite', and 'JCR.name' should be the same!")
    }

    if (is.null(x.allcite)){x.allcite = x.cite}

    # order.by setting
    JCR.name = factor(JCR.name)
    dataset.orgin = data.frame(x.cite, JCR.name, x.allcite)

    data.1row = dataset.orgin[1, ]
    colnames(data.1row) <- c("x.cite", "JCR.name", "x.allcite")

    data.2.N = dataset.orgin[-1, ]
    colnames(data.2.N) <- c("x.cite", "JCR.name", "x.allcite")

    if (order.by == 0){

      dataset0 = data.2.N[order(data.2.N$x.cite, decreasing = TRUE),]
      dataset0 = rbind(data.1row, dataset0)

      x.cite = dataset0$x.cite
      Name = dataset0$JCR.name
      x.allcite = dataset0$x.allcite
    }

    if (order.by == 1){

      dataset1 = data.2.N[order(data.2.N$JCR.name), ]
      dataset1 = rbind(data.1row, dataset1)

      x.cite = dataset1$x.cite
      Name = dataset1$JCR.name
      x.allcite = dataset1$x.allcite
    }

    # basic graphic
    cell_cycle = data.frame(JCR.name = factor(Name, levels = Name),
                            x.cite = x.cite, x.allcite = x.allcite)
    color20 = c("#FF9900", "#FFCC00", "#808080", "#C0C0C0", "#0066CC",
                "#3399FF", "#660099", "#9966CC", "#990000", "#CC3300",
                "#339966", "#99CC66", "#646464", "#C0C0C0", "#4B4B4B",
                "#000000", "#FF6600", "#FF9900", "#003399", "#3366FF", "#000000"
    )

    if (is.null(col)){
      col = color20
    }else{
      col=col}

    color = col

    #
    transparency = transparency # c(0, 1)
    color.trans = adjustcolor(color, alpha.f = transparency)
    #
    circos.par(start.degree = start.degree, cell.padding = c(0, 0, 0, 0), gap.degree = 0)

    circos.initialize(cell_cycle$JCR.name, xlim = cbind(rep(0, length(x.allcite)), cell_cycle$x.allcite))

## track for text only
    circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {

      circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2],
                   arrow.head.width = 0, arrow.head.length = 0,
                   col = NULL,
                   border = NA)

      circos.text(CELL_META$xcenter, CELL_META$ycenter*0.5,
                  CELL_META$sector.index,
                  facing = "downward", niceFacing = TRUE,
                  cex = text.size,col = color[CELL_META$sector.numeric.index])
    },
    bg.border = NA, bg.col = NULL, track.height = 0.3) # track.height for text

## end copy1

    circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {

      circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2],
                   arrow.head.width = 0, arrow.head.length = 0,
                   col = color.trans[CELL_META$sector.numeric.index],
                   border = NA)

      if (draw.line == TRUE){
      circos.lines(c(CELL_META$xcenter, CELL_META$xcenter),
                   c(CELL_META$cell.top.radius, CELL_META$cell.top.radius + uy(0.2,"cm")),
                   col = color[CELL_META$sector.numeric.index])
        }else{}
    },
    bg.border = NA, track.height = 0.2)

    # link width using x.cite, NOT x.allcite!

    for (i in 2:length(x.cite)){
      x.cite = x.cite

      if (self.include == FALSE){
        x2 = x.cite[-1]
        x2 = c(0,x2)
      }else{
        x2=x.cite}

      x3 = x2/sum(x2) #sum(x3)==1
      x4 = x3*x.allcite[1]  # sum(x4)==x.allcite[1], devide x.allcite in percentage.

      if (self.include == TRUE){
        circos.link(sector.index1 = factor(Name[1]), c(0, x4[1]),
                    sector.index2 = factor(Name[1]), c(0, x4[1]),
                    col = color.trans[1], border = color.trans[1],
                    rou = 0.52, ...)}

      circos.link(sector.index1 = factor(Name[1]), c(sum(x4[1:i-1]), sum(x4[1:i])),
                  sector.index2 = factor(Name[i]), c(0, x4[i]),
                  col = color.trans[i], border = color.trans[i],
                  rou = 0.52, ...)
    }
  }

# left panel of graph (-90,90)degree: adj=c(1,0)
# right panel of graph (90,270)degree: adj= c(0,1)
