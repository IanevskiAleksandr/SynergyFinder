###################################################################################
##### Calculate synergy surfaces out of scores
###################################################################################

calcsyn <- compiler::cmpfun(function(scores, drug.pairs)
{
  scores.dose <- t(scores)  
  combscores = scores.dose[-1, -1]; combscores[nrow(combscores),ncol(combscores)] <- 'NA'
  summary.score <- round(mean(as.numeric(combscores), na.rm = !0), 3)
  x.conc <- as.numeric(rownames(scores.dose))
  y.conc <- as.numeric(colnames(scores.dose))
  conc.unit <- drug.pairs$concUnit
  unit.text <- paste0("(", conc.unit, ")")
  drug.row <- paste0(drug.pairs$drug.row, " ", unit.text)
  drug.col <- paste0(drug.pairs$drug.col, " ", unit.text)
  color.range <- round(max(abs(max(as.numeric(combscores), na.rm = !0)), abs(min(as.numeric(combscores), na.rm = !0))) + 5, -1)
  start.point <- -color.range; end.point <- color.range  
  pixels.num = 5 * (length(x.conc) - 1) + 2;
  
  # only for visualization  (max BzCl)
  scores.dose[nrow(scores.dose),ncol(scores.dose)] <- max(scores.dose[nrow(scores.dose)-1,ncol(scores.dose)],
                                                           scores.dose[nrow(scores.dose),ncol(scores.dose)-1],
                                                           scores.dose[nrow(scores.dose)-1,ncol(scores.dose)-1])    
  
  kriged =  tryCatch({
    tmp <- cbind(expand.grid(c(0:(length(x.conc) - 1)), c(0:(length(y.conc) - 1))), c(as.matrix(scores.dose)))        
    kriging(tmp[, 1],tmp[, 2], tmp[, 3], lags = ifelse(dim(scores.dose)[1] < 8, 2 ,3), 
            pixels = pixels.num, model = "spherical")
  },error = function(e){
    appro <- function(x, n) approx(x, n=n)$y
     tryCatch({
        m = apply(t(apply(scores.dose, 1, function(x) appro(x, ncol(scores.dose)*2))), 2, function(x) appro(x, nrow(scores.dose)*2))
        tmp2<- cbind(expand.grid(c(0:(nrow(m)-1)),c(0:(ncol(m)-1))), c(as.matrix(m)))
        kriging(tmp2[, 1], tmp2[, 2], tmp2[, 3], lags = ifelse(dim(m)[1] < 8, 2 ,3), pixels = pixels.num, model = "spherical")
      },error = function(e){ 
        m = apply(t(apply(scores.dose, 1, function(x) appro(x, ncol(scores.dose)*3))), 2, function(x) appro(x, nrow(scores.dose)*3))
        tmp2<- cbind(expand.grid(c(0:(nrow(m)-1)),c(0:(ncol(m)-1))), c(as.matrix(m)))
        kriging(tmp2[, 1], tmp2[, 2], tmp2[, 3], lags = ifelse(dim(m)[1] < 8, 2 ,3), pixels = pixels.num, model = "spherical")
      })
  })
  
  xseq <- round(kriged[["map"]]$x/kriged$pixel)
  yseq <- round(kriged[["map"]]$y/kriged$pixel)
  a <- min(xseq):max(xseq); b <- min(yseq):max(yseq)
  na <- length(a); nb <- length(b)
  res1 <- as.double(rep(0, na * nb))
  res2 <- as.integer(rep(0, na * nb))
  res = krig(xseq = xseq, kriged = kriged[["map"]]$pred, a = a, yseq = yseq, b = b,  
             z_len = length(kriged[["map"]]$pred), res1 = res1, na = na, nb = nb, res2 = res2)
  
  res1 = res[[1]]; res1[res[[2]] == 0] <- NA
  cMat <- matrix(res1, na, nb, byrow = !0)
  
  # most synergystic region
  max_ = r_ = c_ = -999;
  for(i in 1:(ncol(scores.dose)-2)){
    for(j in 1:(nrow(scores.dose)-2)){
       mean_ = mean(scores.dose[j:(j+2),i:(i+2)], na.rm = !0)
       if(mean_ > max_) {
         max_ = mean_; r_ = j; c_ = i;
       }
    }
  }

  return(list(c = cMat, conc.unit = conc.unit, drug.row = drug.row, 
              drug.col = drug.col, start.point = start.point, end.point = end.point, 
              summary.score = summary.score, x.conc = x.conc, y.conc = y.conc, pixels.num = pixels.num, r_ = r_, c_ = c_, max_ = round(max_,3)))
})


###################################################################################
##### Plot 2D and 3D synergy interaction maps
###################################################################################

PlotSynergyShiny <- compiler::cmpfun(function (data, type = "2D", graphnumber = 1, brushx = NULL, brushy = NULL, gridsize = 1, gridsize2 = 0, 
                              savee2D = NULL, savee3D = NULL, newscore = NULL, name_3D = NULL, method_ = "ZIP", synScoresMtx = NULL, mostsynarea = 1) 
{
  print("plotinside")
  
  !is.list(data) && {stop("Input data is not a list format!")}
  if (gridsize == -1) {colmap = !0; gridsize = 1} else { colmap = !1 }
  
  summary.score <- data$summary.score; cMat <- data$c
  drug.row <- data$drug.row; drug.col <- data$drug.col
  x.conc <- data$x.conc; y.conc <- data$y.conc
  start.point <- data$start.point; end.point <- data$end.point
  
  if (method_ == "ZIP") {
    if (!is.null(newscore)) {plot.title =  plot.title2 = bquote(~delta ~ " - score: " ~ .(newscore))
    } else { plot.title <- bquote(~delta ~ " - score: " ~ .(summary.score)); plot.title2 <- paste0("delta score: ", summary.score)}
    title3D = paste0(drug.row, " & ", drug.col, " <br>\U03B4 - score: ", summary.score)
  }
  else {
    if (!is.null(newscore)) { plot.title = plot.title2 = paste0(method_, " synergy score: ", newscore)
    } else { plot.title <- paste0(method_, " synergy score: ", summary.score); 
             plot.title2 <- paste0(method_, " synergy score: ",  summary.score);}
    title3D = paste0(drug.row, " & ", drug.col, " <br> ", method_, " synergy score: ", summary.score)
  }
  print(paste0("size- ", gridsize))
  
  if (graphnumber == 1) {

    rotate <- function(x) t(apply(x, 2, rev))
    subX = seq.int(min(x.conc), max(x.conc), length.out = nrow(cMat))
    subY = seq.int(max(y.conc), min(y.conc), length.out = ncol(cMat))
    
    if (gridsize != 0) {
      
      #browser(); browser(); browser();
      
      p = plot_ly(z = rotate(rotate(rotate(cMat))), x = subX, y = subY, type = "surface", 
                  contours = list(y = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap),  
                                  x = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap)), 
                  hoverinfo = "z+name", name = "d - score", 
                  colorbar = list(outlinewidth = 0, title = "\U03B4 - score", len = 0.24, thickness = 19, xpad = 3, showticklabels = !0, 
                                  titlefont = 9, outlinewidth = 0.3, tickcolor = "#fff", tickfont = list(size = 9), ticks = "inside"), 
                  colorscale = list(c(0, "rgb(0, 247, 0)"), c(0.5, "rgb(247, 247, 247)"), c(1, "rgb(247, 0, 0)")), 
                  cauto = F, cmin = start.point, cmax = end.point, 
                  contour = list(show = !1, color = "#222"))
      
      
      # p = plot_ly(z = cMat, x = subX, y = subY, type = "surface", 
      #             contours = list(y = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap),  
      #                             x = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap)), 
      #             hoverinfo = "z+name", name = "d - score", 
      #             colorbar = list(outlinewidth = 0, title = "\U03B4 - score", len = 0.24, thickness = 19, xpad = 3, showticklabels = !0, 
      #                             titlefont = 9, outlinewidth = 0.3, tickcolor = "#fff", tickfont = list(size = 9), ticks = "inside"), 
      #             colorscale = list(c(0, "rgb(0, 247, 0)"), c(0.5, "rgb(247, 247, 247)"), c(1, "rgb(247, 0, 0)")), 
      #             cauto = F, cmin = start.point, cmax = end.point, 
      #             contour = list(show = !1, color = "#222"))
      # 
      # 
      
    } else if (gridsize == 0) {
      p =  plot_ly(z = rotate(rotate(rotate(cMat))), x = subX, y = subY, type = "surface", hoverinfo = "z+name", name = "d - score", 
                   contours = list(y = list(show = !1), x = list(show = !1)), 
                   colorbar = list(outlinewidth = 0, title = "\U03B4 - score", len = 0.24, thickness = 19, xpad = 3, showticklabels = !0, 
                                   titlefont = 9, outlinewidth = 0.3, tickcolor = "#fff", tickfont = list(size = 9), ticks = "inside"), 
                   colorscale = list(c(0, "rgb(0, 247, 0)"), c(0.5, "rgb(247, 247, 247)"), c(1, "rgb(247, 0, 0)")), 
                   cauto = F, cmin = start.point, cmax = end.point, contour = list(show = !1, color = "#222")) 
      
    }
    
    p = p %>% layout(title = "",
                     scene = list(xaxis = list(title = as.character(""), tickmode = "array",
                                               tickvals = seq.int(min(x.conc), max(x.conc), length.out = length(x.conc)),
                                               tickfont = list(family = "serif", size = 12),
                                               ticktext = rep("", length(x.conc)), ticks = "none"), 
                                  yaxis = list(title = as.character(""), tickmode = "array",
                                               tickvals = seq.int(max(y.conc), min(y.conc), length.out = length(y.conc)),
                                               tickfont = list(family = "serif", size = 12),
                                               ticktext = rep("", length(y.conc)), ticks = "none"),
                                  zaxis = list(title = "", range = c(-20, 100), 
                                               tickfont = list(family = "serif", size = 12), ticks = "none",                                                            tickmode = "array", tickvals = seq.int(-20,100,20), ticktext = rep("", 7)),
                                  camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25))
                     ))

    #htmlwidgets::saveWidget(as.widget(p), "index2.html")
    
    # p = p %>% layout(title = title3D,
    #                  scene = list(xaxis = list(title = as.character(drug.row), tickmode = "array",
    #                                            tickvals = seq.int(min(x.conc), max(x.conc), length.out = length(x.conc)),
    #                                            tickfont = list(family = "serif", size = 12),
    #                                            ticks = "outside", ticktext = as.character(y.conc)),
    #                               yaxis = list(title = as.character(drug.col), tickmode = "array",
    #                                            tickvals = seq.int(max(y.conc), min(y.conc), length.out = length(y.conc)),
    #                                            tickfont = list(family = "serif", size = 12),
    #                                            ticks = "outside", ticktext = as.character(x.conc)),
    #                               zaxis = list(title = "\U03B4 - score", range = c(start.point, end.point),
    #                                            tickfont = list(family = "serif", size = 12), ticks = "outside")
    #                  )
    # )
    
    # p = plot_ly(z = rotate(rotate(t(cMat))), x = subX, y = subY, type = "surface", 
    #             contours = list(y = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap),  
    #                             x = list(show = !0, width = gridsize, highlightwidth = 2, usecolormap = colmap)), 
    #             hoverinfo = "z+name", name = "d - score", 
    #             colorbar = list(outlinewidth = 0, title = "\U03B4 - score", len = 0.24, thickness = 19, xpad = 3, showticklabels = !0, 
    #                             titlefont = 9, outlinewidth = 0.3, tickcolor = "#fff", tickfont = list(size = 9), ticks = "inside"), 
    #             colorscale = list(c(0, "rgb(0, 247, 0)"), c(0.5, "rgb(247, 247, 247)"), c(1, "rgb(247, 0, 0)")), 
    #             cauto = F, cmin = start.point, cmax = end.point, 
    #             contour = list(show = !1, color = "#222"))
    # 
    # p = p %>% layout(title = title3D, 
    #                  scene = list(yaxis = list(title = as.character(drug.row), tickmode = "array", 
    #                                            tickvals = seq(0, ncol(cMat), length.out = length(y.conc)),
    #                                            tickfont = list(family = "serif", size = 12), 
    #                                            ticks = "outside", ticktext = as.character(y.conc)),
    #                               xaxis = list(title = as.character(drug.col), tickmode = "array", 
    #                                            tickvals = seq(0, nrow(cMat), length.out = length(x.conc)),
    #                                            tickfont = list(family = "serif", size = 12), 
    #                                            ticks = "outside", ticktext = as.character(x.conc)),
    #                               #zaxis = list(title = "\U03B4 - score", range = c(start.point, end.point), 
    #                               #             tickfont = list(family = "serif", size = 12), ticks = "outside")
    #                               zaxis = list(title = "\U03B4 - scores", tickmode = "array",
    #                                            tickvals = seq(start.point, end.point, length.out = 9),
    #                                            tickfont = list(family = "serif", size = 12),
    #                                            ticks = "outside", ticktext = seq(start.point, end.point, length.out = 9))
    #                  )
    # )
    # 
    # 
    #   htmlwidgets::saveWidget(as.widget(p), "index2.html")
    # 
    if (is.null(savee3D)) p else htmlwidgets::saveWidget(as.widget(p), name_3D)
    
  } else if (graphnumber == 2){
    plot2d = melt(cMat);
    myPalette <- colorRampPalette(c("green2", "white", "red1"))(100)
    names(plot2d) <- c("x","y","z")
    gplot2d <- 
      ggplot(plot2d) + aes(x, y, z = z, fill = z)  + geom_raster(interpolate = !0) + 
      geom_contour(color = "white", alpha = 0.5) + 
      scale_fill_gradientn(expression(delta ~ -score), colours = myPalette, limits = c(start.point, end.point), 
                           values = rescale(c(-3, -1, 0, 1, 3))) + 
      scale_x_continuous(drug.col, expand = c(0, 0), 
                         breaks = seq(min(plot2d$x), max(plot2d$x), by = (max(plot2d$x) - min(plot2d$x))/(length(x.conc) - 1)), 
                         labels = round(x.conc, 2)) + 
      scale_y_continuous(drug.row, expand = c(0, 0), 
                         breaks = seq(min(plot2d$y), max(plot2d$y), by = (max(plot2d$y) - min(plot2d$y))/(length(y.conc) - 1)), 
                         labels = round(y.conc, 2)) + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      theme(axis.text = element_text(size = 10)) + 
      theme(title = element_text(vjust = 12)) + 
      theme(plot.title = element_text(size = 18, margin = margin(b = 25, unit = "pt"))) 
    
    
    byx = (max(plot2d$x) - min(plot2d$x))/(length(x.conc) - 1);
    byy = (max(plot2d$y) - min(plot2d$y))/(length(y.conc) - 1);
    
    if(mostsynarea == 1){
    rect <- data.frame(xmin = byx*(data$r_-1)+1, xmax = byx*((data$r-1)+2)+1, ymin = byy*(data$c_-1)+1, ymax = byy*((data$c_-1)+2)+1)
    gplot2d <- gplot2d + 
      geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey90", alpha=0.1, inherit.aes = !1)
    }
    
    if(gridsize2 != 0)
    {     
     gplot2d <- gplot2d + geom_vline(xintercept = seq(min(plot2d$x), max(plot2d$x), by = byx), linetype = "dotted") + 
                          geom_hline(yintercept = seq(min(plot2d$y), max(plot2d$y), by = byy), linetype = "dotted") 
      
      if (is.null(savee2D)) {
        gplot2d + ggtitle(plot.title) + coord_cartesian(xlim = c(brushx[1], brushx[2]), ylim = c(brushy[1], brushy[2]))  + 
          theme(plot.title = element_text(hjust = 0.5))
      }
      else {
        gplot2d <- gplot2d + ggtitle(plot.title2)  + theme(plot.title = element_text(hjust = 0.5))
        if (!is.null(brushx) & !is.null(brushx)) 
          gplot2d <- gplot2d + coord_cartesian(xlim = c(brushx[1], brushx[2]), ylim = c(brushy[1], brushy[2]))
        pdf(name_3D); print(gplot2d); dev.off()
      }
    }
    else if(gridsize2 == 0)
    {
      if (is.null(savee2D)) { 
        gplot2d + coord_cartesian(xlim = c(brushx[1], brushx[2]), ylim = c(brushy[1], brushy[2])) + ggtitle(plot.title)  + 
          theme(plot.title = element_text(hjust = 0.5))
      }
      else {
        gplot2d <- gplot2d + ggtitle(plot.title2) + theme(plot.title = element_text(hjust = 0.5))
        if (!is.null(brushx) & !is.null(brushx)) 
          gplot2d <- gplot2d + coord_cartesian(xlim = c(brushx[1], brushx[2]), ylim = c(brushy[1], brushy[2]))
        pdf(name_3D); print(gplot2d); dev.off()
      }
    }
  }
})


###################################################################################
##### Plot into the report
###################################################################################

PlotSynergyReport <- compiler::cmpfun(function(data_, scores_, calcSyn_, method, mostsynarea = 1, type = "2D"){

  texfile <- 'result.tex'
  latexBegin <- "\\documentclass{article} \\usepackage[papersize={7in,7in},margin=0.3in]{geometry} \\usepackage{animate} 
                 \\usepackage{pdfpages} \\begin{document}\\includepdf[pages=-]{./first0.pdf}\\newpage\\includepdf[pages=-]{./first1.pdf}\\newpage"
  zz <- 0

  for (i in 1:length(calcSyn_)){
    
    calc <- calcSyn_[[i]];
    plot.title <- paste0(method," synergy score: ", calc$summary.score)     
    zz = zz + 1

  if (type != "none")
  {  
      
  pdf(paste0(LETTERS[zz], ".pdf"),  width = 10, height = 8, onefile = T)

   if (type == "3D")
    {
      fig <- wireframe(calc$c, scales = list(arrows = !1,distance = c(0.8,0.8,0.8),col=1,cex=0.8,
                                        z = list(tick.number=7),
                                        x=list(at=seq(0, calc$pixels.num, 5),labels=round(calc$x.conc, 3)),
                                        y=list(at=seq(0,calc$pixels.num,5),labels=round(calc$y.conc,3))),
                       drape = !0, colorkey = list(space="top",width=0.5),
                       #screen = list(z = 30, x = -55),
                       screen = list(z = 30, x = -55),
                       zlab = list(expression(delta ~ -score),rot=90,cex=1,axis.key.padding = 0),
                       xlab=list(as.character(calc$drug.col),cex=1, rot=20),ylab=list(as.character(calc$drug.row),cex=1,rot=-50),
                       zlim = c(calc$start.point, calc$end.point),
                       col.regions=colorRampPalette(c("green","white","red"))(100),
                       main = plot.title,
                       at=do.breaks(c(calc$start.point, calc$end.point),100),
                       par.settings = list(axis.line=list(col="transparent"),
                                           layout.widths=list(left.padding=0,right.padding=0), layout.heights=list(top.padding=8, bottom.padding=0)),
                       zoom = 0.9
              )
      
      plot(0,type='n',axes=!1,ann=!1)     
      print(fig, position = c(0,0, 1, 1), newpage = F)
      mtext(paste0(calc$drug.col,"  &  ",calc$drug.row), outer=!0,  cex=1.4, line=-1.7, col = "blue")
      
    } else if (type == "2D") { 
           
      plot2d = melt(calc$c) 
      
      myPalette <- colorRampPalette(c("green2","white","red1"))(100)
      names(plot2d) <- c("x", "y", "z")
      
      byx = (max(plot2d$x) - min(plot2d$x)) / (length(calc$x.conc) - 1);
      byy = (max(plot2d$y) - min(plot2d$y)) / (length(calc$y.conc) - 1);
      
      gg2d = ggplot(plot2d) + aes(x, y, z = z, fill = z) +
        ggtitle(plot.title) + theme(plot.title = element_text(size=22, margin=margin(b = 50, unit = "pt"), hjust = 0.5)) +
        #scale_fill_gradient2(, "\U0394 - score",low=rgb(0,255,0, maxColorValue = 255), high=rgb(255,0, 0,  maxColorValue = 255), guide="colorbar") + 
        geom_raster(interpolate=!0) + geom_contour(color = "white", alpha = 0.5) +
        scale_fill_gradientn("\U03B4 - score",colours = myPalette, limits=c(calc$start.point, calc$end.point), 
                             values = rescale(c(-3, -1, 0, 1, 3))) +
        scale_x_continuous(calc$drug.col, expand = c(0,0),
                           breaks = seq(min(plot2d$x), max(plot2d$x), by = byx), labels = round(calc$x.conc, 1)) + 
        scale_y_continuous(calc$drug.row, expand = c(0,0),
                           breaks = seq(min(plot2d$y), max(plot2d$y), by = byy), labels = round(calc$y.conc, 1)) +
        geom_vline(xintercept=seq(min(plot2d$x), max(plot2d$x), by = byx), linetype="dotted", size = 0.25) +
        geom_hline(yintercept=seq(min(plot2d$y), max(plot2d$y), by = byy), linetype="dotted", size = 0.25) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()) +theme(axis.text=element_text(size=10)) +
        theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
        theme(plot.margin=unit(c(2,1,1.5,1.2),"cm"))
      
      if(mostsynarea == 1){
        rect <- data.frame(xmin = byx*(calc$r_-1)+1, xmax = byx*((calc$r-1)+2)+1, ymin = byy*(calc$c_-1)+1, ymax = byy*((calc$c_-1)+2)+1)
        gg2d <- gg2d + 
          geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey90", alpha=0.1, inherit.aes = !1)
      }
      
      plot(0,type='n',axes=!1,ann=!1)
      print(gg2d, newpage = F)
      mtext(paste(calc$drug.col," & ",calc$drug.row, "     "), outer=!0,  cex=1.3, line=-1.5, col = "blue")
    
    } else {

        fig <- wireframe(calc$c, scales = list(arrows = !1,distance = c(0.8,0.8,0.8),col=1,cex=0.8,
                                                z = list(tick.number=7),
                                                x=list(at=seq(0, calc$pixels.num, 5),labels=round(calc$x.conc, 3)),
                                                y=list(at=seq(0,calc$pixels.num,5),labels=round(calc$y.conc,3))),
                         drape = !0, colorkey = list(space="top",width=0.5),
                         #screen = list(z = 30, x = -55),
                         screen = list(z = 30, x = -55),
                         zlab = list(expression(delta ~ -score),rot=90,cex=1,axis.key.padding = 0),
                         xlab=list(as.character(calc$drug.col),cex=1, rot=20),ylab=list(as.character(calc$drug.row),cex=1,rot=-50),
                         zlim = c(calc$start.point, calc$end.point),
                         col.regions=colorRampPalette(c("green","white","red"))(100),
                         main = plot.title,
                         at=do.breaks(c(calc$start.point, calc$end.point),100),
                         par.settings = list(axis.line=list(col="transparent"),
                                             layout.widths=list(left.padding=5,right.padding=3), layout.heights=list(top.padding=8, bottom.padding=0)),
                         zoom = 1
                         #par.settings=list(layout.widths=list(left.padding=0,right.padding=0), layout.heights=list(top.padding=0, bottom.padding=0)) # margin
        )
        
        plot2d = melt(calc$c) 
        
        myPalette <- colorRampPalette(c("green2","white","red1"))(100)
        names(plot2d) <- c("x", "y", "z")
        
        byx = (max(plot2d$x) - min(plot2d$x)) / (length(calc$x.conc) - 1);
        byy = (max(plot2d$y) - min(plot2d$y)) / (length(calc$y.conc) - 1);
        
        gg2d = ggplot(plot2d) + aes(x, y, z = z, fill = z)+
          ggtitle(plot.title) + theme(plot.title = element_text(size=14, face = "bold", margin=margin(b = -15, unit = "pt"), hjust = 0.5)) +
          #scale_fill_gradient2(, "\U0394 - score",low=rgb(0,255,0, maxColorValue = 255), high=rgb(255,0, 0,  maxColorValue = 255), guide="colorbar") + 
          geom_raster(interpolate=!0) + geom_contour(color = "white", alpha = 0.5) +
          scale_fill_gradientn("",colours = myPalette, limits=c(calc$start.point, calc$end.point), 
                               values = rescale(c(-3, -1, 0, 1, 3)), 
                               guide = guide_colorbar(direction = "horizontal", barwidth = 15, 
                                                      barheight = 0.25, title.position = "top",label.position="top")) + 
          theme(legend.position="top") +
          scale_x_continuous(calc$drug.col, expand = c(0,0),
                             breaks = seq(min(plot2d$x), max(plot2d$x), by = byx), labels = round(calc$x.conc, 1)) + 
          scale_y_continuous(calc$drug.row, expand = c(0,0),
                             breaks = seq(min(plot2d$y), max(plot2d$y), by = byy), labels = round(calc$y.conc, 1)) +
          geom_vline(xintercept=seq(min(plot2d$x), max(plot2d$x), by = byx), linetype="dotted", size = 0.1) +
          geom_hline(yintercept=seq(min(plot2d$y), max(plot2d$y), by = byy), linetype="dotted", size = 0.1) +
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank()) +theme(axis.text=element_text(size=10)) +
          theme(axis.title.y=element_text(margin=margin(0,5,0,0))) +
          theme(plot.margin=unit(c(2.5,0.1,2.5,0.3),"cm"))
        
        
        if(mostsynarea == 1){
          rect <- data.frame(xmin = byx*(calc$r_-1)+1, xmax = byx*((calc$r-1)+2)+1, ymin = byy*(calc$c_-1)+1, ymax = byy*((calc$c_-1)+2)+1)
          gg2d <- gg2d + 
            geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey90", alpha=0.1, inherit.aes = !1)
        }
        
        gridExtra::grid.arrange(gg2d,fig, ncol=2)  
      }
      graphics.off()
      #check for last page
      if (i!=length(calcSyn_))
        latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}\\newpage")
      else
        latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}")
    }
  }
  
  latexBegin <- paste0(latexBegin, "\\end{document}")
  cat(latexBegin, sep='', file=texfile)
  system(paste0('pdflatex ', '-output-directory ./ ', texfile))
})


###################################################################################
##### Plot cobined
###################################################################################


PlotSynergyReportcomb <- compiler::cmpfun(function(data_, scores_, calcSyn_, method, mostsynarea = 1){
  
  texfile <- 'result.tex'
  latexBegin <- "\\documentclass{article} \\usepackage[papersize={7in,7in},margin=0.3in]{geometry} \\usepackage{animate} 
                 \\usepackage{pdfpages} \\begin{document}\\includepdf[pages=-]{./first0.pdf}\\newpage"
  zz <- 0
  
  for (i in 1:length(calcSyn_)){
  
    calc <- calcSyn_[[i]];
    plot.title <- paste0(method," synergy score: ", calc$summary.score)     
    zz = zz + 1
    
    pdf(paste0(LETTERS[zz], ".pdf"),  width = 10, height = 8, onefile = T)
    
      response.mat <- data_[[i]]
      data.plot <- melt(response.mat)
      colnames(data.plot) <- c("y","x","Inhibition")
      data.plot$Inhibition <- round(c(response.mat), 2)
      data.plot$x <- as.factor(data.plot$x)
      data.plot$y <- as.factor(data.plot$y)
      axis.x.text <- round(as.numeric(colnames(response.mat)), 1)
      axis.y.text <- round(as.numeric(rownames(response.mat)), 1)
    
      dose.response.p <- ggplot(data.plot, aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "Inhibition")) + 
                                geom_text(aes_string(fill = "Inhibition", label = "Inhibition"), size = 3.5) + 
                                scale_fill_gradient2(low = "green", high = "red", midpoint = 0, name = "",
                                                     guide = guide_colorbar(direction = "horizontal", barwidth = 15, barheight = 0.25, 
                                                                            title.position = "top",label.position="top")) + 
                                theme(legend.position="top") + scale_x_discrete(labels = axis.x.text) + 
                                scale_y_discrete(labels = axis.y.text) + xlab(calc$drug.col) + ylab(calc$drug.row) +
                                theme(plot.margin=unit(c(2.5,0.1,2.5,0.3),"cm")) + ggtitle("inhibition (%)") + 
                                theme(plot.title = element_text(size=12, margin=margin(b = -15, unit = "pt"), hjust = 0.5))

      plot2d = melt(calc$c) 
      myPalette <- colorRampPalette(c("green2","white","red1"))(100)
      names(plot2d) <- c("x", "y", "z")
      
      byx = (max(plot2d$x) - min(plot2d$x)) / (length(calc$x.conc) - 1);
      byy = (max(plot2d$y) - min(plot2d$y)) / (length(calc$y.conc) - 1);
      
      gg2d = ggplot(plot2d) + aes(x, y, z = z, fill = z)+
        ggtitle(plot.title) + theme(plot.title = element_text(size=14, face = "bold", margin=margin(b = -15, unit = "pt"), hjust = 0.5)) +
        #scale_fill_gradient2(, "\U0394 - score",low=rgb(0,255,0, maxColorValue = 255), high=rgb(255,0, 0,  maxColorValue = 255), guide="colorbar") + 
        geom_raster(interpolate=!0) + geom_contour(color = "white", alpha = 0.5) +
        scale_fill_gradientn("",colours = myPalette, limits=c(calc$start.point, calc$end.point), 
                             values = rescale(c(-3, -1, 0, 1, 3)), 
                             guide = guide_colorbar(direction = "horizontal", barwidth = 15, 
                                                    barheight = 0.25, title.position = "top",label.position="top")) + 
        theme(legend.position="top") +
        scale_x_continuous(calc$drug.col, expand = c(0,0),
                           breaks = seq(min(plot2d$x), max(plot2d$x), by = byx), labels = round(calc$x.conc, 1)) + 
        scale_y_continuous(calc$drug.row, expand = c(0,0),
                           breaks = seq(min(plot2d$y), max(plot2d$y), by = byy), labels = round(calc$y.conc, 1)) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +theme(axis.text=element_text(size=10)) +
        theme(axis.title.y=element_text(margin=margin(0,5,0,0))) +
        theme(plot.margin=unit(c(2.5,0.1,2.5,0.3),"cm"))
      
    if(mostsynarea == 1){
      rect <- data.frame(xmin = byx*(calc$r_-1)+1, xmax = byx*((calc$r-1)+2)+1, ymin = byy*(calc$c_-1)+1, ymax = byy*((calc$c_-1)+2)+1)
      gg2d <- gg2d + 
        geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey90", alpha=0.1, inherit.aes = !1)
    }
    
    gg2d <- gg2d + geom_vline(xintercept=seq(min(plot2d$x), max(plot2d$x), by = byx), linetype="dotted", size = 0.1) +
            geom_hline(yintercept=seq(min(plot2d$y), max(plot2d$y), by = byy), linetype="dotted", size = 0.1)
    
    plot(0,type='n',axes=!1,ann=!1)
      gridExtra::grid.arrange(dose.response.p,gg2d, ncol=2, newpage = F)
    mtext(paste(calc$drug.col, " & ", calc$drug.row, "     "), 
          outer = !0, cex = 1.3, line = -1.5, col = "blue")
    
    
    graphics.off()
    
    #check for last page
    if (i!=length(calcSyn_))
      latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}\\newpage")
    else
      latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}")
  }
  
  latexBegin <- paste0(latexBegin, "\\end{document}")
  cat(latexBegin, sep='', file=texfile)
  system(paste0('pdflatex ', '-output-directory ./ ', texfile))
})


###################################################################################
##### Plot into the report "DYNAMIC"
###################################################################################

PlotSynergyReportdynamic <- compiler::cmpfun(function (data_, scores_, calcSyn_, method, mostsynarea = 1, type = "2D") 
{

  texfile <- 'result.tex'
  latexBegin <- "\\documentclass{article} \\usepackage[papersize={7in,7in},margin=0.3in]{geometry} \\usepackage{animate} 
                 \\usepackage{pdfpages} \\begin{document}\\includepdf[pages=-]{./first0.pdf}\\newpage\\includepdf[pages=-]{./first1.pdf}\\newpage"
  zz <- 0
  
  for (i in 1:length(calcSyn_)){
  
    calc <- calcSyn_[[i]];
    plot.title <- paste0(method," synergy score: ", calc$summary.score)     
    zz = zz + 1
    
  
    for (i in 0:7) {  
      
      pdf(paste0(LETTERS[zz], i + 1, ".pdf"), width = 10, height = 8, onefile = T)
      
        if (type == "3D")
        {
          fig <- wireframe(calc$c, scales = list(arrows = !1,distance = c(0.8,0.8,0.8),col=1,cex=0.8,
                                                 z = list(tick.number=7),
                                                 x=list(at=seq(0, calc$pixels.num, 5),labels=round(calc$x.conc, 3)),
                                                 y=list(at=seq(0,calc$pixels.num,5),labels=round(calc$y.conc,3))),
                           drape = !0, colorkey = list(space="top",width=0.5),
                           screen = list(z = 30 + i*45, x = -55),
                           zlab = list(expression(delta ~ -score),rot=90,cex=1,axis.key.padding = 0),
                           xlab=list(as.character(calc$drug.col),cex=1, rot=20),ylab=list(as.character(calc$drug.row),cex=1,rot=-50),
                           zlim = c(calc$start.point, calc$end.point),
                           col.regions=colorRampPalette(c("green","white","red"))(100),
                           main = plot.title,
                           at=do.breaks(c(calc$start.point, calc$end.point),100),
                           par.settings = list(axis.line=list(col="transparent"),
                                               layout.widths=list(left.padding=0,right.padding=0), layout.heights=list(top.padding=8, bottom.padding=0)),
                           zoom = 1
          )
          plot(0,type='n',axes=!1,ann=!1)     
          print(fig, position = c(0,0, 1, 1), newpage = F)
          mtext(paste0(calc$drug.col,"  &  ",calc$drug.row), outer=!0,  cex=1.4, line=-1.7, col = "blue")
          
        } else {
            
          fig <- wireframe(calc$c, scales = list(arrows = !1,distance = c(0.8,0.8,0.8),col=1,cex=0.8,
                                                 z = list(tick.number=7),
                                                 x=list(at=seq(0, calc$pixels.num, 5),labels=round(calc$x.conc, 3)),
                                                 y=list(at=seq(0,calc$pixels.num,5),labels=round(calc$y.conc,3))),
                           drape = !0, colorkey = list(space="top",width=0.5),
                           #screen = list(z = 30, x = -55),
                           screen = list(z = 30 + i*45, x = -55),
                           zlab = list(expression(delta ~ -score),rot=90,cex=1,axis.key.padding = 0),
                           xlab=list(as.character(calc$drug.col),cex=1, rot=20),ylab=list(as.character(calc$drug.row),cex=1,rot=-50),
                           zlim = c(calc$start.point, calc$end.point),
                           col.regions=colorRampPalette(c("green","white","red"))(100),
                           main = plot.title,
                           at=do.breaks(c(calc$start.point, calc$end.point),100),
                           par.settings = list(axis.line=list(col="transparent"),
                                               layout.widths=list(left.padding=5,right.padding=3), layout.heights=list(top.padding=8, bottom.padding=0)),
                           zoom = 1
                           #par.settings=list(layout.widths=list(left.padding=0,right.padding=0), layout.heights=list(top.padding=0, bottom.padding=0)) # margin
          )
              
          plot2d = melt(calc$c) 
          
          myPalette <- colorRampPalette(c("green2","white","red1"))(100)
          names(plot2d) <- c("x", "y", "z")
          
          byx = (max(plot2d$x) - min(plot2d$x)) / (length(calc$x.conc) - 1);
          byy = (max(plot2d$y) - min(plot2d$y)) / (length(calc$y.conc) - 1);
          
          gg2d = ggplot(plot2d) + aes(x, y, z = z, fill = z)+
            ggtitle(plot.title) + theme(plot.title = element_text(size=14, face = "bold", margin=margin(b = -15, unit = "pt"), hjust = 0.5)) +
            #scale_fill_gradient2(, "\U0394 - score",low=rgb(0,255,0, maxColorValue = 255), high=rgb(255,0, 0,  maxColorValue = 255), guide="colorbar") + 
            geom_raster(interpolate=!0) + geom_contour(color = "white", alpha = 0.5) +
            scale_fill_gradientn("",colours = myPalette, limits=c(calc$start.point, calc$end.point), 
                                 values = rescale(c(-3, -1, 0, 1, 3)), 
                                 guide = guide_colorbar(direction = "horizontal", barwidth = 15, 
                                                        barheight = 0.25, title.position = "top",label.position="top")) + 
            theme(legend.position="top") +
            scale_x_continuous(calc$drug.col, expand = c(0,0),
                               breaks = seq(min(plot2d$x), max(plot2d$x), by = byx), labels = round(calc$x.conc, 1)) + 
            scale_y_continuous(calc$drug.row, expand = c(0,0),
                               breaks = seq(min(plot2d$y), max(plot2d$y), by = byy), labels = round(calc$y.conc, 1)) +
            geom_vline(xintercept=seq(min(plot2d$x), max(plot2d$x), by = byx), linetype="dotted", size = 0.1) +
            geom_hline(yintercept=seq(min(plot2d$y), max(plot2d$y), by = byy), linetype="dotted", size = 0.1) +
            theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +theme(axis.text=element_text(size=10)) +
            theme(axis.title.y=element_text(margin=margin(0,5,0,0))) +
            theme(plot.margin=unit(c(2.5,0.1,2.5,0.3),"cm"))
          
          
          if(mostsynarea == 1){
            rect <- data.frame(xmin = byx*(calc$r_-1)+1, xmax = byx*((calc$r-1)+2)+1, ymin = byy*(calc$c_-1)+1, ymax = byy*((calc$c_-1)+2)+1)
            gg2d <- gg2d + 
              geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey90", alpha=0.1, inherit.aes = !1)
          }
          
          gridExtra::grid.arrange(gg2d,fig, ncol=2)  
        }
      graphics.off()
    }
    
    if(type != "2D")
      if (i!=length(calcSyn_))
        latexBegin <- paste0(latexBegin, "\\begin{figure}\\begin{center}\\animategraphics[controls,loop,width=1\\textwidth]{4}{", 
                            LETTERS[zz], "}{1}{8}\\end{center}\\end{figure}\\newpage")
      else latexBegin <- paste0(latexBegin, "\\begin{figure}\\begin{center}\\animategraphics[controls,loop,width=1\\textwidth]{4}{", 
                               LETTERS[zz], "}{1}{8}\\end{center}\\end{figure}")
    else
      if (i!=length(calcSyn_))
        latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}\\newpage")
      else
        latexBegin <- paste0(latexBegin, "\\includepdf[pages=-]{./",paste0(LETTERS[zz], ".pdf"),"}")
  }
  latexBegin <- paste0(latexBegin, "\\end{document}")
  cat(latexBegin, sep = "", file = texfile)
  system(paste0("pdflatex ", "-output-directory ./ ", texfile))
})
