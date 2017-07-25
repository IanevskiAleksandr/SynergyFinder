###################################################################################
##### Plot dr matrix and single curves
###################################################################################

PlotDoseResponseShinyDR <- compiler::cmpfun(function (param = "inhibition", paramplot = 1, response.mat, unit.text, drug.row, drug.col, single.fitted = NULL) 
{
  plot(0, type = "n", axes = !1, ann = !1)

  if (paramplot == 1) { 
    
   data.plot <- melt(response.mat)
   colnames(data.plot) <- c("y","x","Inhibition")
   data.plot$Inhibition <- round(c(response.mat), 2)
   data.plot$x <- as.factor(data.plot$x)
   data.plot$y <- as.factor(data.plot$y)
    
    axis.x.text <- round(as.numeric(colnames(response.mat)), 2)
    axis.y.text <- round(as.numeric(rownames(response.mat)), 2)
    dose.response.p <- ggplot(data.plot, aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "Inhibition")) + 
      theme(title = element_text(face = "bold", size = 10)) + 
      geom_text(aes_string(fill = "Inhibition", label = "Inhibition"), size = 3.5) + 
      scale_fill_gradient2(low = "green", high = "red", midpoint = 0, name = paste0(param, " (%)")) + 
      scale_x_discrete(labels = axis.x.text) + scale_y_discrete(labels = axis.y.text) + 
      xlab(paste(drug.col, unit.text, sep = " ")) + ylab(paste(drug.row, unit.text, sep = " "))
    
    dose.response.p <- dose.response.p + theme(axis.text.x = element_text(color = "red", face = "bold", size = 10))
    dose.response.p <- dose.response.p + theme(axis.text.y = element_text(color = "red", face = "bold", size = 10))
    dose.response.p <- dose.response.p + theme(axis.title = element_text(size = 10))
    dose.response.p <- dose.response.p + ggtitle(paste0("\n\nDose-response matrix (", tolower(param), ")\n")) + 
      theme(plot.title = element_text(hjust = 0.5))
    print(dose.response.p, newpage = F)
    mtext(paste0(drug.col, "  &  ", drug.row, "      "), outer = !0, cex = 1.3, line = -1.5, col = "blue")
    
  } else {
    suppressWarnings(par(mgp = c(3, 0.5, 0))); par(mar = c(5.5, 5, 6.5, 2.5))
    
    if(paramplot == 2) {
      plot(single.fitted$drug.row.model, xlab = paste0("Concentration ", unit.text), ylab = paste0(param, " (%)"), type = "obs", col = "red",  cex = 1.5, pch = 16, xtsty = "base5", ylim=c(-9,109)) 
      plot(single.fitted$drug.row.model, xlab = paste0("Concentration ", unit.text), ylab = paste0(param, " (%)"), type = "none", cex = 1.5, add = T, lwd = 3, ylim=c(-9,109))
    } else {
      plot(single.fitted$drug.col.model, xlab = paste0("Concentration ", unit.text), ylab = paste0(param, " (%)"), type = "obs", col = "red",  cex = 1.5, pch = 16, xtsty = "base5", ylim=c(-9,109)) 
      plot(single.fitted$drug.col.model, xlab = paste0("Concentration ", unit.text), ylab = paste0(param, " (%)"), type = "none", cex = 1.5, add = T, lwd = 3, ylim=c(-9,109))
    }   
    par(mar = c(0, 1, 9, 0))
    title(paste0("Dose-response curve for drug: ", ifelse(paramplot == 2, drug.row, drug.col)),  cex.main = 1)
  }
})



PlotDoseResponseReport <- compiler::cmpfun(function (data_, pairs_, type = "all") 
{ 
  dose.response.mats <- data_
  drug.pairs <- pairs_
  num.pairs <- nrow(pairs_)
  plots <- list()
  
  for (i in 1:num.pairs) {
    response.mat <- dose.response.mats[[i]]
    data.plot <- melt(response.mat)
    colnames(data.plot) <- c("y","x","Inhibition")
    data.plot$Inhibition <- round(c(response.mat), 2)
    data.plot$x <- as.factor(data.plot$x)
    data.plot$y <- as.factor(data.plot$y)
    conc.unit <- drug.pairs$concUnit[i]
    unit.text <- paste0("(", conc.unit, ")")
    drug.row <- drug.pairs$drug.row[i]
    drug.col <- drug.pairs$drug.col[i]
    
    plot.title <- paste0("\n\nDose-response matrix (", tolower("inhibition"), ")\n")
    axis.x.text <- round(as.numeric(colnames(response.mat)), 1)
    axis.y.text <- round(as.numeric(rownames(response.mat)), 1)
  
    #browser(); 
    
    if (type == "all") {
      
      dose.response.p <- ggplot(data.plot, aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "Inhibition")) + 
        theme(title = element_text(face = "bold", size = 10)) +  
        geom_text(aes_string(fill = "Inhibition", label = "Inhibition"), size = 3.5) + 
        scale_fill_gradient2(low = "green", high = "red", midpoint = 0, name = "inhibition (%)") + 
        scale_x_discrete(labels = axis.x.text) + scale_y_discrete(labels = axis.y.text) + 
        xlab(paste(drug.col, unit.text, sep = " ")) + ylab(paste(drug.row, unit.text, sep = " ")) +
        theme(plot.margin=unit(c(1,0.1,1,0.3),"cm"))
      
      dose.response.p <- dose.response.p + theme(axis.text.x = element_text(color = "red",face = "bold", size = 10))
      dose.response.p <- dose.response.p + theme(axis.text.y = element_text(color = "red",face = "bold", size = 10))
      dose.response.p <- dose.response.p + theme(axis.title = element_text(size = 10))
      dose.response.p <- dose.response.p + ggtitle(plot.title)
            
      single.fitted <- FittingSingleDrug(response.mat)

      par(mfrow=c(2,2))
      par(mar=c(4,3,8,3))
      plot(single.fitted$drug.row.model, xlab = paste0("Concentration ", unit.text), ylab = paste0("inhibition (%)"), type = "obs", col = "red",  cex = 1.5, pch = 16, xtsty = "base5", ylim=c(-5,105)) 
      plot(single.fitted$drug.row.model, xlab = paste0("Concentration ", unit.text), ylab = paste0("inhibition (%)"), type = "none", cex = 1.5, add = T, lwd = 3, ylim=c(-5,105))
      par(mar=c(0,0,11,0))
      title(paste0("Dose-response curve for drug: ", drug.row), cex.main = 1)
      plot(0,type='n',axes=!1,ann=!1)
      par(mar=c(8,3,5,3))
      plot(single.fitted$drug.col.model, xlab = paste0("Concentration ", unit.text), ylab = paste0("inhibition (%)"), type = "obs", col = "red",  cex = 1.5, pch = 16, xtsty = "base5", ylim=c(-5,105)) 
      plot(single.fitted$drug.col.model, xlab = paste0("Concentration ", unit.text), ylab = paste0("inhibition (%)"), type = "none", cex = 1.5, add = T, lwd = 3, ylim=c(-5,105))
      title(paste0("Dose-response curve for drug: ", drug.col), cex.main = 1)
      print(dose.response.p, vp = viewport(height = unit(0.95, 
                                                         "npc"), width = unit(0.53, "npc"), just = c("left", 
                                                                                                     "top"), y = 1, x = 0.48))
      #graphics.off();
      mtext(paste(drug.col, " & ", drug.row, "     "), 
            outer = TRUE, cex = 1.3, line = -1.5, col = "blue")
      
    }
    else if (type == "heatmap") {

      dose.response.p <- ggplot(data.plot, aes_string(x = "x", 
                                                      y = "y")) + geom_tile(aes_string(fill = "Inhibition")) + 
        theme(title = element_text(face = "bold", size = 10)) + 
        geom_text(aes_string(fill = "Inhibition", label = "Inhibition"), 
                  size = 3.5) + scale_fill_gradient2(low = "green", 
                                                     high = "red", midpoint = 0, name = paste0("inhibition", 
                                                                                               " (%)")) + scale_x_discrete(labels = axis.x.text) + 
        scale_y_discrete(labels = axis.y.text) + xlab(paste(drug.col, 
                                                            unit.text, sep = " ")) + ylab(paste(drug.row, 
                                                                                                unit.text, sep = " "))
      plot.new()
      print(dose.response.p, vp = viewport(height = unit(0.85, 
                                                         "npc"), width = unit(0.7, "npc"), just = c("left", 
                                                                                                    "top"), y = 0.9, x = 0.15))
      mtext(paste(drug.col, " & ", drug.row, "     "), 
            outer = TRUE, cex = 1.3, line = -1.5, col = "blue")
    }
    else if (type == "curve") {
      single.fitted <- FittingSingleDrug(response.mat)
      par(mfrow = c(2, 1))
      suppressWarnings(par(mgp = c(3, 0.5, 0)))
      x.lab <- paste0("Concentration ", unit.text)
      par(mar = c(1.5, 8, 4.5, 5.5))
      plot(single.fitted$drug.row.model, xlab = x.lab,  ylab = paste0("inhibition", " (%)"), type = "obs", col = "red", cex = 1.5, pch = 16, xtsty = "base5")
      plot(single.fitted$drug.row.model, xlab = x.lab, ylab = paste0("inhibition", " (%)"), type = "none",  cex = 1.5, add = T, lwd = 3)
      par(mar = c(0, 1, 6, 0))
      title(paste0("Dose-response curve for drug: ", drug.row), cex.main = 1)
      par(mar = c(1.5, 8, 4.5, 5.5))
      plot(single.fitted$drug.col.model, xlab = x.lab, ylab = paste0("inhibition", " (%)"), type = "obs", col = "red", cex = 1.5, pch = 16, xtsty = "base5")
      plot(single.fitted$drug.col.model, xlab = x.lab, ylab = paste0("inhibition", " (%)"), type = "none", cex = 1.5, add = T, lwd = 3)
      par(mar = c(0, 1, 6, 0))
      title(paste0("Dose-response curve for drug: ", drug.col), cex.main = 1)
      mtext(paste(drug.col, " & ", drug.row, "     "), 
            outer = TRUE, cex = 1.3, line = -1.5, col = "blue")
    }
  }
})
