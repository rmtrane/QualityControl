#' Bland-Altman Plot with Confidence Interval
#'
#' Creates a plot of the difference between two grades against
#' the mean of the two grades with smooth mean and smoothed 95%
#' bound for continuous type data.
#' Calculates the mean difference, intra-class correlation,
#' within [range1](\%) and within [range2](\%)
#'
#' @param grade1a a vector of grades
#' @param grade2a a corresponding vector of grades
#' @param label1 label of the first grader
#' @param label2 label of the second grader
#' @param xlab label for x-axis
#' @param ylab label for y-axs
#' @param range1 numeric
#' @param range2 numeric
#' @param range3 numeric
#' @param cantgrade character/numeric used to indicate cannot grade
#' @param maintitle main title of graph
#' @param subtitle sub title of graph
#' @param footer footer
#' @param x1,x2 range of x axis -- if -999, the range is calculated based on the data
#' @param y1,y2 range of y axis -- if -999, the range is calculated based on the data
#' @param factor numeric; passed to jitter for plotting of data points. Adjusts how spread out the points are.
#'
#' @export


##            maintitle -- main title of graph
##            subtitle-- sub titles of graphs
##            footer -- footer
## Return:    NA
## See also:  twograders.plot.ssc
## Examples:  sd.plot(FIDI$CNVAREA.F, FIDI$CNVAREA.D ,
## 		      label1="Synthetic Film",label2="Synthetic Digital",range1=0.1,
##		      range2=0.5, maintitle="Area of Classic CNV ", footer="Comparison
##		      of Digital Synthetic Grades and Film Synthetic Grades")
##################################################################################################


sd.plotaLimit <- function(grade1a, grade2a, label1, label2, range1, range2, range3, ptsz=0.7, cantgrade='',
                          maintitle="", subtitle = "", footer="", x1 = -999, x2 = -999, y1 = -999, y2 = -999,
                          factor=1e-8, spline = FALSE,
                          xlab = "Average of the Two Grades",
                          ylab = "Difference of the Two Grades",
                          ylimit = NULL, CI_background = grey(1)){

    ## changing data type from 'factor' type to 'double/numeric' type
    grade1a <- as.numeric(as.character(grade1a))
    grade2a <- as.numeric(as.character(grade2a))

    ## remove missing values
    grade1b <- grade1a[(is.na(grade1a) == F) & (is.na(grade2a) == F)]
    grade2b <- grade2a[(is.na(grade1a) == F) & (is.na(grade2a) == F)]

    ## remove cantgrade values
    grade1 <- grade1b[(grade1b != cantgrade) & (grade2b != cantgrade)]
    grade2 <- grade2b[(grade1b != cantgrade) & (grade2b != cantgrade)]

    ## put in basic outline of plot areas
    plot.new()
    par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)

    polygon(c(0, 0, 1, 1), c(0, 0.05, 0.05, 0), col = 0.1)
    polygon(c(0, 0, 1, 1), c(1, 0.87, 0.87, 1), col = 0.1)
    polygon(c(0, 0, 1, 1), c(0.255, 0.2, 0.2, 0.255), col = 0.05)
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), density = 0, lwd = 3)
    polygon(c(0, 0, 1, 1), c(0, 0.05, 0.05, 0), density = 0, lwd = 3)
    polygon(c(0, 0, 1, 1), c(1, 0.87, 0.87, 1), density = 0, lwd = 3)

    segments(0.2, 0.255, 0.2, 0.05, lty = 1, lwd = 3)
    segments(0.45, 0.255, 0.45, 0.05, lwd = 3)
    segments(0.63, 0.255, 0.63, 0.05, lwd = 3)

    segments(0.81, 0.255, 0.81, 0.05, lwd = 3)

    segments(0, 0.255, 1, 0.255, lwd = 3)
    segments(0, 0.2, 1, 0.2, lwd = 1)

    ## put in title and date signature
    if(grepl(pattern = '#', maintitle, fixed = TRUE)){

      #tmp.maintitle <- gsub(pattern = 'mm^2', x = maintitle, replacement = "#mm^2#", fixed = TRUE)

      new.maintitle <- unlist(strsplit(x = maintitle, split = "#"))

      express <- parse(text = new.maintitle[2])[[1]]

      text(0.5, 0.935, bquote(.(new.maintitle[1]) * .(express) * .(new.maintitle[3])),
           cex = 1.8)
    } else {
      text(0.5, 0.935, paste(maintitle), cex = 1.8)
    }
    text(.5,.895,paste(subtitle),cex=1)
    text(0.12, 0.025, date(), cex = 0.67)
    text(0.5, 0.025, paste(footer), cex = 0.67)

    ## create average grade for x-axis
    avgrade <- (grade1 + grade2)/2

    ## calculate x-range for graph
    if (x1 == -999){
        avinfo <- range(avgrade)
    } else {
        avinfo <- c(x1, x2)
    }

    avrange <- avinfo[2] - avinfo[1]
    ##avmin <- avinfo[1] - (0.05 * avrange)
    ##avmax <- avinfo[2] + (0.05 * avrange)
    avmin <- avinfo[1]
    avmax <- avinfo[2]

    ## calculate y-range for graph
    diff <- grade2 - grade1
    if(y1 == -999){
        diffinfo <- range(diff)
    } else {
        diffinfo <- c(y1, y2)
    }

    diffrang <- max(abs(diffinfo[2]), abs(diffinfo[1]))
    diffmin <- -1.15 * diffrang
    diffmax <- 1.15 * diffrang

    ## get mean and sd of differences for future calcs
    diffmean <- round(mean(diff), digits = 2)
    diffsd <- sqrt(var(diff))
    diffsd2 <- round(diffsd * 1.96, digits = 2)
    ciyhi <- diffmean + diffsd2
    ciylo <- diffmean - diffsd2

    if(ciyhi > diffmax){
      diffmax <- 1.05 * ciyhi
    }

    if(ciylo < diffmin){
      diffmin <- 1.05 * ciylo
    }

    ## plot labels, comments
    text(0.535, 0.295, xlab, cex = 0.8)
    text(0.05, 0.594, ylab, cex = 0.8, srt = 90)

    ## creating the mean box
    ## par(new = TRUE)

    ## bottom text panel
    par(fig = c(0, 0.20, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    text(0.5, 0.85, "Mean", cex = 1)
    text(0.5, 0.55, paste(label2, " is"), cex = 0.8)
    if(diffmean > 0){
        text(0.5, 0.4, paste(diffmean, " HIGHER"), cex = 1)
        text(0.5, 0.25, paste("than ", label1), cex = 0.8)
    } else {
        if(diffmean < 0){
            text(0.5, 0.4, paste( - diffmean, " LOWER"), cex = 1)
            text(0.5, 0.25, paste("than ", label1), cex = 0.8)
        } else {
            if(diffmean == 0){
                text(0.5, 0.4, "the SAME AS", cex = 1)
                text(0.5, 0.25, label1, cex = 0.8)
            }
        }
    }

    ## Creating the 95% Confidence Limit box
    diffLimitL = quantile(diff, probs = 0.025, na.rm = TRUE)
    diffLimitH = quantile(diff, probs = 0.975, na.rm = TRUE)

    diffLimitL = round(diffLimitL, 2)
    diffLimitH = round(diffLimitH, 2)

    ##par(new = TRUE)
    par(fig = c(0.15, 0.5, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)

    text(0.5, 0.85, "Confidence Limit", cex = 1)
    text(0.5, 0.5, paste("(", (diffLimitL), "to ", (diffLimitH), ")"))



    ## creating the within1 box
    ## par(new = TRUE)
    par(fig = c(0.45, 0.63, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    text(0.5, 0.85, paste("Within ", range1), cex = 1)
    text(0.5, 0.5, paste(length(grade2[abs(diff) <= range1]), "of", length(grade2)))
    text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range1])/length(grade2),
                                    digits = 3) * 100, "% )"))

    ## creating the within2 box
    ##par(new = TRUE)

    par(fig = c(0.63, 0.81, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    text(0.5, 0.85, paste("Within ", range2), cex = 1)
    text(0.5, 0.5, paste(length(grade2[abs(diff) <= range2]), "of", length(grade2)))
    text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range2])/length(grade2),
                                    digits = 3) * 100, "% )"))

    ## creating the within3 box
    ##par(new = TRUE)

    par(fig = c(0.81, 1.0, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    text(0.5, 0.85, paste("Within ", range3), cex = 1)
    text(0.5, 0.5, paste(length(grade2[abs(diff) <= range3]), "of", length(grade2)))
    text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range3])/length(grade2),
                                    digits = 3) * 100, "% )"))


    if(is.null(ylimit))
      ylimit <- c(diffmin, diffmax)


    ## plot region definition
    ## par(new = TRUE)
    par(fig = c(0.05, 0.95, 0.275, 0.825), mar = c(4, 4, 0, 0), new = TRUE)
    plot(avgrade, diff, pch = 16, type = "n", xlim = c(avmin, avmax),
         ylim = ylimit,
         xlab = "", ylab = "", las = 1, cex = 0.8)

    ## put in the 2sd zone (with labels), zero line, and mean line
    polygon(c(avinfo[1], avinfo[2], avinfo[2], avinfo[1]),
            c(ciyhi, ciyhi, ciylo, ciylo), border = F, col = CI_background)  # change shading
    ##polygon(c(avinfo[1], avinfo[2], avinfo[2], avinfo[1]), c(ciyhi, ciyhi, ciylo, ciylo), border = F,col = (grey(0.8)))  #change shading
    segments(avinfo[1], ciyhi, avinfo[2], ciyhi, lty = 9)
    segments(avinfo[1], ciylo, avinfo[2], ciylo, lty = 9)
    ##segments(avinfo[1] - (0.04 * avrange), 0, avinfo[2] + (0.04 * avrange), 0, lty = 15)
    segments(avinfo[1], diffmean, avinfo[2], diffmean, lty = 1, col='red')
    ##text(avinfo[1], diffmean, "mean  ", adj = 1, cex = 0.6)
    #text(avinfo[1], ciyhi, paste( "95% bound\n", ciyhi,), adj = 1, cex = 0.6) #add text
    #text(avinfo[1], ciylo, paste( "95% bound\n", ciylo,), adj = 1, cex = 0.6)

    # put in text about too high, too low
    #text((avmax + (0.02 * avrange)), (1.145 * diffrang), paste(label2, " is HIGHER than ", label1), cex = 0.6, adj = 1)
    #text((avmax + (0.02 * avrange)), (1.145 * diffrang * (-1)), paste(label2, " is LOWER than ", label1), cex = 0.6, adj = 1)

    if(spline){
        print("fitting splines")

        # create splined 95% CIs
        splineciyhi <- vector("numeric", length(diff))
        splineciylo <- vector("numeric", length(diff))
        meancoll <- vector("numeric", length(diff))
        iii <- sort.list(avgrade)
        diff <- diff[iii]
        avgrade <- avgrade[iii]
        for(i in 1:length(diff)){
            diffzone <- diff[(max(1, (i - 5))):(min(length(diff), (i + 5)))]
            sigma <- sqrt(var(diffzone))
            meana <- mean(diffzone)
            meancoll[i] <- meana
            splineciyhi[i] <- (meana + (1.96 * sigma))
            splineciylo[i] <- (meana - (1.96 * sigma))
        }

        splinecihi <- smooth.spline(avgrade, splineciyhi, df = 8)
        splinecilo <- smooth.spline(avgrade, splineciylo, df = 8)
        splineavg <- smooth.spline(avgrade, meancoll, df = 8)

        print("finished splines")

        ## put in spline 95% CIs, mean
        lines(splinecihi, lty = 1)
        lines(splinecilo, lty = 1)
        lines(splineavg, lty = 1)
        text(avinfo[2], splinecihi$y[length(splinecihi$y)],
             "  smoothed\n  95% bound", adj = 0, cex = 0.6)
        text(avinfo[2], splinecilo$y[length(splinecilo$y)],
             "  smoothed\n  95% bound", adj = 0, cex = 0.6)
        text(avinfo[2], splineavg$y[length(splineavg$y)],
             "  smoothed\n  mean", adj = 0, cex = 0.6)
    }
    ## put the points in the graph
    points(jitter(avgrade,factor=factor), jitter(diff,factor=factor), pch = 16, cex = ptsz)


    # summary values
    tot <- length(grade2)
    awr1 <- length(grade2[abs(diff) <= range1])
    awr1p <- round(awr1/tot*100)
    awr2 <- length(grade2[abs(diff) <= range2])
    awr2p <- round(awr2/tot*100)

    if (range1 == 0.5 & range2 == 1){
        summary<-c(tot,awr1,awr1p,awr2,awr2p,"-","-","-","-")
    } else {
        if (range1 ==1 & range2 == 5){
            summary<-c(tot,"-","-",awr1,awr1p,"-","-",awr2,awr2p)
        } else {
            summary<-c(tot,"-","-",awr1,awr1p,awr2,awr2p,"-","-")
        }
    }

    return(c(summary,diffsd,diffLimitH, diffLimitL))#ciyhi,ciylo))
}

