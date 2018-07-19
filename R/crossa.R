#' Cross Tabs with Shading Algorithms
#'
#' This functions allows you to create cross tabs with
#' shading algorithms and fills it with exact agreement,
#' agreement within two steps, kappa, the standard error of kappa,
#' weighted kappa (within one step), and the standard error
#' of weighted kappa.
#'
#' @param data1 a vector of grades from grader 1; not a factor
#' @param data2 a vector of grades from grader 2; not a factor
#' @param lab1 label for data1
#' @param lab2 label for data2
#' @param cantgrade vector containing values that are used for cannot grade and hence
#' should not be included in within one step calculations.
#' @param title1 main title of graph, line one
#' @param title2 main title of graph, line two
#' @param footer footer for graph
#' @param ext.labels if NULL (default), labels printed in squares. Otherwise, character
#' vector with same length as there are levels of the variable considered, with strings
#' to be stand-in for labels in squares while actual labels are printed in the lower left
#' corner. If true, numbers are used as labels and automatically
#' @param cex numeric; passed to par. Default 1.
#' @param PLOT logical; if TRUE (default), figure is plotted. If FALSE, summary returned without any figures.
#' @param mar vector of length four. Is passed to the mar argument of par. Default: c(0,0,0,0)
#'
#' @export
#'
#'
#'
#' @examples
#' \dontrun{
#' crossa(data1a = FIDI$CNVPRS.F,
#'        lab1   = "FILM SYNTHETIC GRADES",
#'        data2a = FIDI$CNVPRS.D,
#'        lab2   = "DIGITAL SYNTHETIC GRADES",
#'        title1 = "Presence of Classic CNV",
#'        footer = "Comparison of Digital Synthetic Grades and
#'                 Film Synthetic Grades")
#'                 }
#'                 data1a,

###############################################################
## Program:   crossa.ssc                                     ##
## Author     Daisy edited on 6/08 for shading               ##
## Created:   August 16, 1995 (cross.fcn)                    ##
##                                                           ##
## Function:  creates crosstabs with shading algorithms;     ##
##            also computes exact agreement, agreement       ##
##            within two steps, kappa,the standard error of  ##
##            kappa, weighted kappa (within one step), and   ##
##            the standard error of weighted kappa.          ##
## Values:    data1 -- a vector of grades; not a factor      ##
##            data2 -- a corresponding vector of grades      ##
##            lab1 -- labels data1                           ##
##            lab2 -- labels data2                           ##
##            cantgrade -- specifies cantgrade values so     ##
##                         they are not included in within   ##
##                         one step calculations             ##
##            title1 -- main title of graph, one line        ##
##            title2 -- main title of graph, two lines       ##
##            footer -- footer                               ##
##                                                           ##
## See:     cross.ssc                                        ##
## Examples:                                                 ##
## crossa(data1a = FIDI$CNVPRS.F,                            ##
##        lab1="FILM SYNTHETIC GRADES",                      ##
##        data2a = FIDI$CNVPRS.D,                            ##
##        lab2="DIGITAL SYNTHETIC GRADES",                   ##
##        title1="Presence of Classic CNV",                  ##
##        footer="Comparison of Digital Synthetic Grades and ##
##                Film Synthetic Grades")                    ##
###############################################################

crossa <- function(data1a,
                   lab1,
                   data2a,
                   lab2,
                   cantgrade,
                   title1="",
                   footer="",
                   title2="",
                   ext.labels = NULL,
                   cex = 1,
                   PLOT = TRUE,
                   mar = c(0,0,0,0)){

  ## remove missing values
  data1b <- data1a[!is.na(data1a) & !is.na(data2a)]
  data2b <- data2a[!is.na(data1a) & !is.na(data2a)]

  ## remove values from cantgrade
  data1 <- data1b[(!data1b %in% cantgrade) & (!data2b %in% cantgrade)]
  data2 <- data2b[(!data1b %in% cantgrade) & (!data2b %in% cantgrade)]


  if(PLOT){
    ## R fix
    par(fig = c(0, 1, 0, 1), mar = mar, usr = c(0, 1, 0, 1), new = FALSE)
    plot.new()
    par(fig = c(0,1,0,1), mar=mar,usr=c(0,1,0,1), new = TRUE, pin = c(11,8), cex = cex)

    ## Creates region for crosstab ##
    polygon(c(0,0,1,1),c(0,1,1,0),density=0,lwd=3)

    par(fig=c(0,.62,.057,.863), mar=mar,usr=c(0,1,0,1), new = TRUE, cex = cex)
  }

  ## Reads in data, determines number of levels ##
  ntot <- length(data1)

  if(is.numeric(data1)){
    lev <- sort(unique(c(data1, data2)))

    data1 <- factor(data1)
    data2 <- factor(data2)


  } else {

    if(!is.factor(data1))
      data1 <- as.factor(data1)

    if(!is.factor(data2))
      data2 <- as.factor(data2)

    lev <- unique(c(levels(data1), levels(data2)))
  }

  ## Get rid of empty level
  lev <- lev[! lev %in% c('', cantgrade)]

  nlev <- length(lev)

  labs <- lev

  ## If ext.labels is NULL, we look at the labels and see if any should be wrapped and printed
  ## at an angle
  if (is.null(ext.labels) || ext.labels == FALSE){
    for (i in 1:nlev){
      ## For each label, check the length. If too long, wrap the labels
      if(nchar(labs[i]) > 14*5/nlev){
        labs[i] <- wrap_sentence(labs[i], width = 18)
      }
    }

    ## If longest label is too long, define angle, adjust and extra x/y off set
    if(max(nchar(labs)) > 14*5/nlev){
      angle <- -45
      adj = c(1,0)
      xoff2 <- 0.02
      yoff2 <- 0.03

      ## If nlev is even, use the longest of the two middle labels to move 'QC'/'ST' labels
      if(nlev%%2 == 0){
        mchar <- max(nchar(unlist(strsplit(labs[(nlev+1)/2], "\n"))),
                     nchar(unlist(strsplit(labs[nlev/2], "\n"))))
      } else {
        ## Otherwise, use middle
        mchar <- nchar(unlist(strsplit(labs[(nlev+1)/2], "\n")))
      }


      lab.xoff <- 0.03 + 0.00375*(mchar+5)/sqrt(2)
      lab.yoff <- 0.04 + 0.00375*(mchar+5)/sqrt(2)

      lab.xoff <- 0.10
      lab.yoff <- 0.11

    } else {
      ## If labels aren't too long, we angle the labels the other way so they fit in squares.
      angle <- 45
      adj <- c(0.5,0.5)
      xoff2 <- yoff2 <- lab.xoff <- lab.yoff <- 0
    }
  } else {
    ## If ext.labels is simply TRUE, simply use 1,2,3.. as placeholders
    if(ext.labels == TRUE){
      ext.labels <- labs
      labs <- 1:length(ext.labels)

      angle <- 45
      adj <- c(0.5, 0.5)
      xoff2 <- yoff2 <- lab.xoff <- lab.yoff <- 0
    } else {
      ## If ext.labels is NOT NULL and not TRUE, we put ext.labels in labs.
      labs <- ext.labels
      angle <- 45
      adj <- c(0.5, 0.5)
      xoff2 <- yoff2 <- lab.xoff <- lab.yoff <- 0
    }
  }



  ## Sets constants,labels for plotting ##
  xoff <- 0.01375
  yoff <- 0.01375
  sepoff <- 0.02
  legoff <- 0.03
  numoff <- 0.01
  x1 <- 0.10
  x2 <- 0.5
  xavg <- x2 - x1
  x3 <- xavg + x2
  xtot <- x3-x1
  y1 <- 0.10
  y2 <- 0.5
  yavg <- y2-y1
  y3 <- yavg+y2
  ytot <- y3-y1
  jx <- 0.86



  #########################################################################
  ## defines segments position and text
  ####################################
  jy <- c(0.79, 0.62, 0.45)
  jytext <- c(0.82, 0.65, 0.48)
  jtext <- c("Exact Agreement","Within One Step", "Within Two Steps")

  max <- 0

  ##  Zeroes some counting variables ##
  max1 <- 0
  tot <- 0

  ## Assigns base shading values to cells ##
  clrmax <- grey(0.6)  ## changed .6 to .35
  clrcnt <- grey(0.7) #clrmax/2
  clrnxt <- grey(0.8) #clrmax/3

  ## Finds the cell of largest exact agreement ##
  for (i in 1:nlev){
    nnn <- length(data1[(data1==lev[i])&(data2==lev[i])])

    if (nnn > max1)
      max1 <- nnn
  }

  ## Assigns colors to each cell; creates shaded boxes on figure; ##
  ## if cell is non-empty, prints cell size ##
  for (i in 1:nlev){
    for (j in 1:nlev){
      nnn1 <- length(data1[(data1==lev[i])&(data2==lev[j])])

      col2 <- 0

      if ((nnn1==max1)&&(i-j==0)){
        col2 <- clrmax
      } else {
        if (i-j == 0){
          col2 <- clrcnt
        } else {
          if (abs(i-j) == 1){
            col2 <- clrnxt
          } else {
            col2 <- 0
          }
        }
      }

      tot <- tot+nnn1
      if(PLOT){
        polygon(c(x1+(((nlev-i+j-1)/(2*nlev))*xtot),
                  x1+(((nlev-i+j)/(2*nlev))*xtot),
                  x1+(((nlev-i+j+1)/(2*nlev))*xtot),
                  x1+(((nlev-i+j)/(2*nlev))*xtot)),
                c(y1+((((2*nlev)+1-i-j)/(2*nlev))*ytot),
                  y1+((((2*nlev)+1-i-j-1)/(2*nlev))*ytot),
                  y1+((((2*nlev)+1-i-j)/(2*nlev))*ytot),
                  y1+((((2*nlev)+1-i-j+1)/(2*nlev))*ytot)),
                col=col2,
                border=T)

        if (nnn1 > 0){
          text(x1+(((nlev-i+j)/(2*nlev))*xtot),
               y1+((((2*nlev)+1-i-j)/(2*nlev))*ytot),
               paste(nnn1),
               cex=1.0) #1.0 to 0.5
        }
      }
    }
  }

  #######################################################################
  ## Creates lines which define the region and borders of the crosstab ##
  #######################################################################
  if(PLOT){
    for (i in 1:(nlev-1)){
      segments(x1+((i/nlev)*xavg),y2+((i/nlev)*yavg),x2+((i/nlev)*xavg),y1+((i/nlev)*yavg),lty=1)
      segments(x1+((i/nlev)*xavg),y2-((i/nlev)*yavg),x2+((i/nlev)*xavg),y3-((i/nlev)*yavg),lty=1)
      segments(x1+((i/nlev)*xavg),y2+((i/nlev)*yavg),x1+((i/nlev)*xavg),y2+sepoff+((i/nlev)*yavg),lty=1,lwd=5)
      segments(x1+((i/nlev)*xavg),y2+sepoff+((i/nlev)*yavg),x1-legoff+((i/nlev)*xavg),y2+legoff+sepoff+((i/nlev)*yavg),lty=1,lwd=4)
      segments(x2+((i/nlev)*xavg),y3-((i/nlev)*yavg),x2+((i/nlev)*xavg),y3+sepoff-((i/nlev)*yavg),lty=1,lwd=5)
      segments(x2+((i/nlev)*xavg),y3+sepoff-((i/nlev)*yavg),x2+legoff+((i/nlev)*xavg),y3+legoff+sepoff-((i/nlev)*yavg),lty=1,lwd=5)
    }

    polygon(c(x1,x2,x3,x2),c(y2,y1,y2,y3),density=0)
    polygon(c(x1,x2,x2,x1),c(y2,y3,y3+sepoff,y2+sepoff),density=0,lwd=5)
    polygon(c(x3,x2,x2,x3),c(y2,y3,y3+sepoff,y2+sepoff),density=0,lwd=5)
    polygon(c(x1,x2,x2-legoff,x1-legoff),c(y2+sepoff,y3+sepoff,y3+sepoff+legoff,y2+sepoff+legoff),density=0,lwd=5)
    polygon(c(x3,x2,x2+legoff,x3+legoff),c(y2+sepoff,y3+sepoff,y3+sepoff+legoff,y2+sepoff+legoff),density=0,lwd=5)

    ############################################
    ## Pastes levels, marginals into crosstab ##
    ############################################
    margin1 <- vector("numeric",nlev)
    margin2 <- vector("numeric",nlev)

    total.n <- 0

    for (j in 1:nlev){
      text(x1-xoff-xoff2+((((nlev-j)+(1/2))/nlev)*xavg),
           y2+yoff+yoff2+sepoff+((((nlev-j)+(1/2))/nlev)*yavg),
           paste(labs[j]), #lev
           cex=0.7, #0.7 to 0.35
           adj = adj[1],
           srt = angle)
      text(x2+xoff+xoff2+(((j-(1/2))/nlev)*xavg),
           y2+yoff+yoff2+sepoff+((((nlev-j)+(1/2))/nlev)*yavg),
           paste(labs[j]), #lev
           cex=0.7,  #0.7 to 0.35
           adj = adj[2],
           srt = -angle)
      margin1[j] <- length(data2[data1==lev[nlev+1-j]])
      margin2[j] <- length(data1[data2==lev[nlev+1-j]])
      text(x2+xoff+0.005+(((j-(1/2))/nlev)*xavg),y2-yoff-((((nlev-j)+(1/2))/nlev)*yavg),paste(margin1[j]),cex=0.8) #0.8 to 0.5
      text(x1-xoff-0.005+((((nlev-j)+(1/2))/nlev)*xavg),y2-yoff-((((nlev-j)+(1/2))/nlev)*yavg),paste(margin2[j]),cex=0.8)  #0.8 to 0.5

      polygon(c(x1+(((nlev-1)/(2*nlev))*xtot),
                x1+(((nlev)/(2*nlev))*xtot),
                x1+(((nlev+1)/(2*nlev))*xtot),
                x1+(((nlev)/(2*nlev))*xtot)),
              c(y1+((((2*nlev)+1-j-j)/(2*nlev))*ytot),
                y1+((((2*nlev)+1-j-j-1)/(2*nlev))*ytot),
                y1+((((2*nlev)+1-j-j)/(2*nlev))*ytot),
                y1+((((2*nlev)+1-j-j+1)/(2*nlev))*ytot)),
              density=0,lwd=4)

      ## Print external labels
      if(!is.null(ext.labels[1]) && ext.labels[1] != FALSE){

        total.n <- total.n + length(unlist(strsplit(split = '\n',
                                                    x = wrap_sentence(lev[nlev - j + 1],
                                                                      width = 34))))

        text(x = 0.02,
             y = 0.01 + 0.0175*(j + total.n - 1),
             labels = paste(labs[nlev - j + 1],'= '),
             cex = 0.8,
             adj = c(0, 1))


        text(x = 0.055,
             y = 0.01 + 0.0175*(j + total.n - 1),
             labels = wrap_sentence(ext.labels[nlev - j + 1], width = 34),
             cex = 0.8,
             adj = c(0, 1))



      }
    }


    ################################
    ## Prints sample size, labels ##
    ################################
    text((x2+x3+x3)/3,y1-0.03,paste("n = ",tot),cex=0.8)

    text(x1+(xavg/2)-xoff-lab.xoff-sepoff-legoff,
         y2+(yavg/2)+yoff+lab.yoff+sepoff+legoff,
         paste(lab1),
         cex=1.15,
         srt=45)
    text(x3-(xavg/2)+xoff+lab.xoff+sepoff+legoff,
         y2+(yavg/2)+yoff+lab.yoff+sepoff+legoff,
         paste(lab2),cex=1.15,srt=(-45))

    ######################################################
    ## Creates shading, puts in text for header, footer ##
    ######################################################
    ## par(new=TRUE)  par(fig=c(0,.62,.057,.863), mar=c(0,0,0,0),usr=c(0,1,0,1), new = TRUE)
    ## par(new = TRUE)
    par(fig = c(0,1,0,1), mar=mar,usr=c(0,1,0,1), new = TRUE)


    points(x = 0.1, y = 0.0, col = 'blue', pch = 16)

    polygon(c(0,0,1,1),c(0,0.05,0.05,0),col=(grey(0.85)))
    polygon(c(0,0,1,1),c(1,0.87,0.87,1),col=(grey(0.85)))
    polygon(c(0,0,1,1),c(0,0.05,0.05,0),density=0,lwd=3)
    polygon(c(0,0,1,1),c(1,0.87,0.87,1),density=0,lwd=3)

    text(.5,.945,paste(title1),cex=1.8)
    text(.5,.895,paste(title2),cex=1.3)
    text(.12,.025,paste(date()),cex=0.67)
    text(.5,.025,paste(footer),cex=0.67)

    ## draw segments to create panel
    segments(0.62, 0.87, 0.62, 0.05,lwd = 3)

    segments(0.62, 0.72, 1, 0.72, lwd = 3)
    segments(0.62, 0.54, 1, 0.54, lwd = 2)
    segments(0.62, 0.38, 1, 0.38, lwd = 1)
  }
  ###############################################################
  ## Calculates cell of largest exact agreement and calculates ##
  ## exact agreement for crosstab                              ##
  ###############################################################

  ## max1 <- 0
  cnt <- 0
  cnt2 <- 0
  nn0 <- 0
  nn1 <- 0
  nn1tot <- 0
  cnt3 <- 0


  for(k in 1:nlev)
  {
    if (lev[k]==0) { nn0 <- length(data1[(data1 == lev[k]) & (data2 == lev[k])]) }   #######agreement on 0
    else { nn1<-length(data1[(data1 == lev[k]) & (data2 == lev[k])]) } ### exact agreement
    if (nn0 > max1) max1<-nn0
    if (nn1 > max1) max1<-nn1
    nn1tot<-nn1tot+nn1
    cnt <- nn1tot+nn0
  }

  ## pctmax <- round(max1/tot, 2)
  pctcnt <- round(cnt/tot, 2)
  pctnn0<-round(nn0/tot,2)
  pctnn1<-round(nn1tot/tot,2)

  #################################################################
  ## Calculates within one step agreement, taking into account   ##
  ##  possible cannot grade values                               ##
  #################################################################

  if (nlev !=1)
  {
    for (m in 1:(nlev-1))
    {
      mmm <- length(data1[(data1==lev[m])&(data2==lev[m+1])&(!(lev[m+1]) %in% cantgrade)])
      nnn <- length(data1[(data1==lev[m+1])&(data2==lev[m])&(!(lev[m+1]) %in% cantgrade)])
      cnt2 <- cnt2+mmm+nnn
    }
  }
  cnt2tot <- cnt2+cnt
  pctcnt2 <- round(cnt2tot/tot,3)


  #################################################################
  ## Calculates within two step agreement, taking into account   ##
  ##  possible cannot grade values                               ##
  #################################################################

  if (nlev >2)
  {
    for (m in 1:(nlev-2))
    {
      mmm <- length(data1[(data1==lev[m])&(data2==lev[m+2])&(!(lev[m+2]) %in% cantgrade)])
      nnn <- length(data1[(data1==lev[m+2])&(data2==lev[m])&(!(lev[m+2]) %in% cantgrade)])
      cnt3 <- cnt3+mmm+nnn
    }
  }
  cnt3tot <- cnt3+cnt2+cnt
  pctcnt3 <- round(cnt3tot/tot,3)

  ################################################################
  ## Creates a vector of sizes and percents for agreement cells ##
  ################################################################

  numb <- c(cnt, cnt2tot, cnt3tot)
  perc <- c(pctcnt * 100, pctcnt2 * 100, pctcnt3*100)

  ############################
  ##  text                  ##
  ############################
  if (nlev > 3 & PLOT){
    for(j in 1:3){
      text(0.74, jytext[j] + 0.01, paste(jtext[j]), cex = 1.05)
      text(0.74, jy[j] , paste("n = ", numb[j], "    (", perc[j], "%)"), cex = 1)
    }
  }

  ## Don't paste Within Two Steps for 4 levels
  if ((1 < nlev) & (nlev < 4) & PLOT)
  {
    for(j in c(1,2))
    {
      text(0.74, jytext[j] + 0.01, paste(jtext[j]), cex = 1.05)
      text(0.74, jy[j], paste("n = ", numb[j], "    (", perc[j], "%)"), cex = 1)
    }
  }

  if (nlev < 2 & PLOT){
    for(j in c(1,2)){
      text(0.74, jytext[j] + 0.01, paste(jtext[j]), cex = 1.05)
      text(0.74, jy[j], paste("n = ", numb[j], "    (", perc[j], "%)"), cex = 1)
    }
  }
  #######################################
  ## draw cells in segments            ##
  #######################################
  if (nlev > 3 & PLOT){
    for( j in c(1,2,3) ){
      if(j ==1){
        for(k in 1:5){   ###  exact agreement cells in segment
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))
        }
      }
      if(j ==2){
        for(k in 1:5)      ###  exact agreement cells in segment
        {
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))
        }
      }
      if(j ==3)
      {
        for(k in 1:5)      ### exact agreement cells in segment
        {
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))
        }
      }

      ##  within one step ##
      if(j == 2)
      {
        for(k in 1:4)
        {
          kk <- k * 0.02

          polygon(c(jx + 0.04, jx + 0.05, jx + 0.04, jx + 0.03), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
          polygon(c(jx + 0.06, jx + 0.07, jx + 0.06, jx + 0.05), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
        }
      }
      ## within one and two steps ##
      if(j == 3)
      {
        for(k in 1:4)
        {
          kk <- k * 0.02

          polygon(c(jx + 0.04, jx + 0.05, jx + 0.04, jx + 0.03), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
          polygon(c(jx + 0.06, jx + 0.07, jx + 0.06, jx + 0.05), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
        }
        for(k in 1:3)
        {
          kk <- k * 0.02

          polygon(c(jx + 0.03, jx + 0.04, jx + 0.03, jx + 0.02), c(jy[j] + 0.05- kk, jy[ j] + 0.04- kk, jy[j] + 0.03- kk, jy[j] + 0.04- kk), col = (grey(0.8)))
          polygon(c(jx + 0.07, jx + 0.08, jx + 0.07, jx + 0.06), c(jy[j] + 0.05- kk, jy[ j] + 0.04- kk, jy[j] + 0.03- kk, jy[j] + 0.04- kk), col = (grey(0.8)))
        }
      }
    }
  }


  ###################################################################################################
  ###################################################################################################
  if ((1 < nlev) & (nlev <  4) & PLOT)  ## don't draw within two steps cells if there are only three levels
  {
    for( j in c(1,2) )
    {
      if(j ==1)
      {
        for(k in 1:5)
        {
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))
        }
      }
      if(j ==2)
      {
        for(k in 1:5)     ## draw exact agreement cells
        {
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))

        }
        for(k in 1:4)    ## draw within one step cells
        {
          kk <- k * 0.02

          polygon(c(jx + 0.04, jx + 0.05, jx + 0.04, jx + 0.03), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
          polygon(c(jx + 0.06, jx + 0.07, jx + 0.06, jx + 0.05), c(jy[j] + 0.06 - kk, jy[ j] + 0.05 - kk, jy[j] + 0.04 - kk, jy[j] + 0.05 - kk), col = (grey(0.7)))
        }
      }
      ## create actual little boxes for exact agree, within one, cell of largest ##
      ## Baoyan
      for(i in 1:5)
      {
        ## segments(jx + ((i - 1) * 0.01), jy[j] - ((i - 1) * 0.01), jx + 0.05 + ((i - 1) * 0.01),jy[j] + 0.05 - ((i - 1) * 0.01), lwd = 1, lty = 0.4)
        ## segments(jx + ((i - 1) * 0.01), jy[j] + ((i - 1) * 0.01), jx + 0.05 + ((i - 1) * 0.01),jy[j] - 0.05 + ((i - 1) * 0.01), lwd = 1, lty = 0.4)
      }
    }
  }

  if (nlev < 2 & PLOT){
    for( j in c(1) )
    {
      if(j ==1)
      {
        for(k in 1:5)
        {
          kk <- k * 0.02
          polygon(c(jx + 0.05, jx + 0.06, jx + 0.05, jx + 0.04), c(jy[j] + 0.07 - kk, jy[ j] + 0.06 - kk, jy[j] + 0.05 - kk, jy[j] + 0.06 - kk), col = (grey(0.6)))
        }
      }

      ## create actual little boxes for exact agree, within one, cell of largest ##

      for(i in 1:5)   ##############
      {
        segments(jx + ((i - 1) * 0.01), jy[j] - ((i - 1) * 0.01), jx + 0.05 + ((i - 1) * 0.01),jy[j] + 0.05 - ((i - 1) * 0.01), lwd = 1, lty = 1)
        segments(jx + ((i - 1) * 0.01), jy[j] + ((i - 1) * 0.01), jx + 0.05 + ((i - 1) * 0.01),jy[j] - 0.05 + ((i - 1) * 0.01), lwd = 1, lty = 1)
      }
    }
  }

  ####################################################################################################
  ####################################################################################################

  ###################################################################
  ## calculate derived "marginals" (for kappa) ##
  ###################################################################
  rawmargin1 <- vector("numeric", nlev)
  rawmargin2 <- vector("numeric", nlev)
  for(i in 1:nlev)
  {
    rawmargin1[i] <- length(data2[data1 == lev[i]])
    rawmargin2[i] <- length(data1[data2 == lev[i]])
    margin1 <- (rawmargin1 + rawmargin2)/2
    margin2 <- margin1
  }

  wgt1 <- diag(1,nlev)
  wgt2 <- block.diag(1,.75,nlev)


  ## wgt3 = block.diag2(1, .75, .5, nlev)

  ## calculate weighted kappa statistic
  tmp1 <- unfolded.kappa(data1,data2, wgt=wgt1, cantgrade = cantgrade)
  tmp2 <- unfolded.kappa(data1,data2, wgt=wgt2, cantgrade = cantgrade)
  ## tmp3 <- unfolded.kappa(data1,data2,wgt=wgt3)


  ## output kappa statistics
  if(PLOT){
    text(0.81, 0.3450, paste("Kappa = ", round(tmp1$kappa,2), " (SE = ", round(tmp1$se,2), ")"), cex =1.05)
    text(0.81, 0.3050, paste("95% CI for Kappa: (", round(tmp1$ci[1],2), ", ", round(tmp1$ci[2],2), ")"), cex = 0.8)
    if (nlev > 2){
      text(0.81, 0.2550, paste("Weighted Kappa = ", round(tmp2$kappa,2), " (SE = ", round(tmp2$se,2), ")"), cex = 1.05)
      text(0.81, 0.2150, paste("95% CI for Weighted Kappa: (", round(tmp2$ci[1],2), ", ", round(tmp2$ci[2],2), ")"), cex = 0.8)
      # }
      # if (nlev > 4)
      # {
      #  text(0.805, 0.1650, paste("Weighted Kappa 2 = ", round(tmp3$kappa,2), " (SE = ", round(tmp3$se,2), ")"), cex = 1.05)
      #  text(0.81, 0.1250, paste("95% CI for Weighted Kappa 2: (", round(tmp3$ci[1],2), ", ", round(tmp3$ci[2],2), ")"), cex = 0.8)
      text(0.795, 0.165, paste("* Weights: 1 for Complete Agreement"), cex = 0.8)
      text(0.815, 0.135, paste("0.75 for One Step Disagreement"), cex = 0.8)
      text(0.815, 0.105, paste("0 for All Other Disagreement"), cex = 0.8)
    }
  }

  exact<-cnt
  pexact<-round(pctcnt*100,2)
  exon0<-nn0
  pexon0<-round(pctnn0*100,2)
  exels<-nn1tot
  pexels<-round(pctnn1*100,2)
  w1s<-cnt2tot
  pw1s<-round(pctcnt2*100,2)

  w2s<-cnt3tot
  pw2s<-round(pctcnt3*100,2)

  kappa<-round(tmp1$kappa,2)

  if (nlev > 2){
    wkappa<-round(tmp2$kappa,2)
  } else {
    wkappa<-kappa
  }


  return(data.frame(tot,kappa,wkappa,exact,pexact, w1s,pw1s,w2s,pw2s))



}
