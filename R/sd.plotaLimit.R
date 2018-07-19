#' Bland-Altman Plot with Confidence Interval
#'
#' Creates a plot of the difference between two grades against
#' the mean of the two grades with mean and 95% bound for continuous type data.
#' Calculates the mean difference, intra-class correlation, and n (\%) within [range1],
#' [range2], and [range3].
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
#' @param cantgrade character/numeric vector used to indicate cannot grade. Default: c('', 'Not applicable')
#' @param maintitle main title of graph. If units included, expressions can be preserved by surrounding them with #. Example: #mm^2#.
#' @param subtitle sub title of graph
#' @param footer footer
#' @param x1,x2 range of x axis -- if -999 (default), the range is calculated based on the data
#' @param y1,y2 range of y axis -- if -999 (default), the range is calculated based on the data
#' @param factor numeric; passed to jitter for plotting of data points. Adjusts how spread out the points are.
#' @param ptsz numeric; passed to the cex arguments of points. Default is 0.7
#' @param ylimit numeric vector; overwrites y1, y2 for y limits
#' @param CI_background color for confidence interval background
#' @param mar margin passed to par. Set to c(0,0,0,0) for printing to pdf file.
#' @param CI_use_quantiles logical; if FALSE (default), use mean +/- 1.96 SD as confidence interval. If TRUE, use quantiles.
#'
#' @export


sd.plotaLimit <- function(grade1a, grade2a, label1, label2, range1, range2, range3, ptsz=0.7, cantgrade=c('', 'Not applicable'),
                          maintitle="", subtitle = "", footer="", x1 = -999, x2 = -999, y1 = -999, y2 = -999,
                          factor=1e-8, spline = FALSE,
                          xlab = "Average of the Two Grades",
                          ylab = "Difference of the Two Grades",
                          ylimit = NULL, CI_background = grey(1),
                          CI_use_quantiles = FALSE,
                          mar = c(0,0,0,0)){

  ## changing data type from 'factor' type to 'double/numeric' type
  if(is.factor(grade1a))
    grade1a <- as.numeric(as.character(grade1a))

  if(is.factor(grade2a))
    grade2a <- as.numeric(as.character(grade2a))

  ## remove missing values
  grade1b <- grade1a[!is.na(grade1a) & !is.na(grade2a)]
  grade2b <- grade2a[!is.na(grade1a) & !is.na(grade2a)]

  ## remove cantgrade values
  grade1 <- grade1b[!(grade1b %in% cantgrade) & !(grade2b %in% cantgrade)]
  grade2 <- grade2b[!(grade1b %in% cantgrade) & !(grade2b %in% cantgrade)]

  ## Some ad-hoc fixes to make things work when using plot-PlotxsdLimit (i.e. plotting multiple figures at a time).
  par(fig = c(0, 1, 0, 1), mar = mar, usr = c(0, 1, 0, 1), new = FALSE)
  plot.new()
  par(fig = c(0, 1, 0, 1), mar = mar, usr = c(0, 1, 0, 1), new = TRUE)

  ## Put in basic outline of plot areas
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

  ## Put in title and date signature.
  #### If # is present in maintitle, split maintitle and print expressions right.
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

  ## calculate x-range for graph. If specified through x1 and x2, use these values. If not, get range from averages.
  if (x1 == -999 | x2 == -999){
    avinfo <- range(avgrade)
  } else {
    avinfo <- c(x1, x2)
  }

  ## Create range of averages
  avrange <- avinfo[2] - avinfo[1]
  ##avmin <- avinfo[1] - (0.05 * avrange)
  ##avmax <- avinfo[2] + (0.05 * avrange)
  avmin <- avinfo[1]
  avmax <- avinfo[2]

  ## calculate y-range for graph, If specified through y1 and y2, use these values. If not, get range from averages.
  diff <- grade2 - grade1
  if(y1 == -999 | y2 == -999){
    diffinfo <- range(diff)
  } else {
    diffinfo <- c(y1, y2)
  }

  ## To keep y-axis symmetrical around 0, we use this max...
  diffrang <- max(abs(diffinfo[2]), abs(diffinfo[1]))
  ## ... and extend it a bit in both directions.
  diffmin <- -1.15 * diffrang
  diffmax <- 1.15 * diffrang

  ## get mean and sd of differences for future calcs
  diffmean <- round(mean(diff), digits = 2)
  diffsd <- sqrt(var(diff))



  ## Creating the 95% Confidence Limit box
  ## If CI_use_quantiles, calculate CI based on quantiles.
  if(CI_use_quantiles){
    ## Calculating 95% Confidence Limits
    ciylo = quantile(diff, probs = 0.025, na.rm = TRUE)
    ciyhi = quantile(diff, probs = 0.975, na.rm = TRUE)

    ciylo = round(ciylo, 2)
    ciyhi = round(ciyhi, 2)
  } else {
    diffsd2 <- round(diffsd * 1.96, digits = 2)

    ciyhi <- diffmean + diffsd2
    ciylo <- diffmean - diffsd2
  }

  ## If CI is above/below the current max/min of the range for the y-axis, we change the range of the y-axis.
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
  par(fig = c(0, 0.20, 0.05, 0.255), mar = c(0,0,0,0), usr = c(0, 1, 0, 1), new = TRUE)
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


  ## Creating the Confidence Limit box
  par(fig = c(0.15, 0.5, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)

  text(0.5, 0.85, "Confidence Limit", cex = 1)
  text(0.5, 0.5, paste("(", round(ciylo, digits = 2), "to ", round(ciyhi, digits = 2), ")"))



  ## creating the within1 box
  par(fig = c(0.45, 0.63, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
  text(0.5, 0.85, paste("Within ", range1), cex = 1)
  text(0.5, 0.5, paste(length(grade2[abs(diff) <= range1]), "of", length(grade2)))
  text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range1])/length(grade2),
                                  digits = 3) * 100, "% )"))

  ## creating the within2 box
  par(fig = c(0.63, 0.81, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
  text(0.5, 0.85, paste("Within ", range2), cex = 1)
  text(0.5, 0.5, paste(length(grade2[abs(diff) <= range2]), "of", length(grade2)))
  text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range2])/length(grade2),
                                  digits = 3) * 100, "% )"))

  ## creating the within3 box
  par(fig = c(0.81, 1.0, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
  text(0.5, 0.85, paste("Within ", range3), cex = 1)
  text(0.5, 0.5, paste(length(grade2[abs(diff) <= range3]), "of", length(grade2)))
  text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range3])/length(grade2),
                                  digits = 3) * 100, "% )"))

  ## If specific ylimits are NOT specified, use diffmin and diffmax.
  if(is.null(ylimit))
    ylimit <- c(diffmin, diffmax)


  ## plot region definition
  par(fig = c(0.05, 0.95, 0.275, 0.825), mar = c(4, 4, 0, 0), new = TRUE)
  plot(avgrade, diff, pch = 16, type = "n", xlim = c(avmin, avmax),
       ylim = ylimit,
       xlab = "", ylab = "", las = 1, cex = 0.8)

  ## put in the CI zone (with labels), zero line, and mean line
  polygon(c(avinfo[1], avinfo[2], avinfo[2], avinfo[1]),
          c(ciyhi, ciyhi, ciylo, ciylo), border = F, col = CI_background)
  segments(avinfo[1], ciyhi, avinfo[2], ciyhi, lty = 9)
  segments(avinfo[1], ciylo, avinfo[2], ciylo, lty = 9)
  segments(avinfo[1], diffmean, avinfo[2], diffmean, lty = 1, col='red')

  ## Use splines?
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

  ## Plot the data with some jitter
  points(jitter(avgrade,factor=factor), jitter(diff,factor=factor), pch = 16, cex = ptsz)


  # summary values to be returned for summary table
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

  return(c(summary,diffsd,ciyhi,ciylo))
}

