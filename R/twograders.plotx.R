#' Correlation Plot
#'
#' Creates the plot with x-axis = "Grader 1" vs. Y-axis = "Grader 2" for
#' continuous type data.
#' Calculates the mean difference, intra-class correlaion, within [range1] (\%),
#' and within [range2](\%)
#'
#' @param grade1a vector of grades
#' @param grade2a vector of second grades
#' @param label1 label for first grades
#' @param label2 label for second grades
#' @param range1,range2,range3 numerics; specify the ranges to use for within calculations
#' @param ptsz numeric; passed to cex argument of plot
#' @param maintitle main title of graph
#' @param subtitle sub title of graph
#' @param footer foot text
#' @param cantgrade numeric value specifying what is used for 'Cannot Grades'
#' @param x1,x2 range for x axis -- if -999 the range is calculated from data
#' @param y1,y2 range for y axis -- if -999 the range is calculated from data
#'
#' @export
#'
#'
##################################################################################################
## Program:   twograders.plota1.ssc					    			##
##            Same as Modify/twograders.plot.ssc                                                ##
## Created:   Jan,2010		Xin revised		   					##
## Function:  Similar to sd.plot.ssc								##
##				It creates the plot with X-axis="" vs. Y-axis= "Grader 2"	##
##				for continous type data.					##
##            It calculates the "Mean Difference","Intra-Class Correlation", 	                ##
##				"within [range1](%)", &" Within [range 2] (%)".			##
## Values:    grade1a -- a vector of grades							##
##            grade2a -- a corresponding vector of grades					##
##	      label1  -- label of the first grader						##
##	      label2  -- label of the second grader						##
##	      range1 --										##
##            range2 --                                                                         ##
## 	      range3 -- (if not needed, please input value = 9999)	 	                ##
##            cantgrade										##
##            maintitle -- main title of graph							##
##            subtitle-- sub titles of graphs			                      	        ##
##            footer -- footer                                               	                ##
## Return:    NA	                                                                        ##
## See also:  sd.plot.ssc   	           							##
## Examples:  twograders.plotx(FIDI$CNVAREA.F, FIDI$CNVAREA.D ,					##
## 			       label1="Synthetic Film",label2="Synthetic Digital",range1=0.1,   ##
##			       range2=0.5, maintitle="Area of Classic CNV ", footer="Comparison ##
##			       of Digital Synthetic Grades and Film Synthetic Grades")		##
##################################################################################################

twograders.plotx <- function(grade1a, grade2a, label1, label2, range1, range2, range3=9999,
                          ptsz=0.7,
                          lab=c(5,5,7),
                          cantgrade='',
                          maintitle="", subtitle = "", footer="",
                          x1 = -999, x2 = -999, y1 = -999, y2 = -999)
    ## xlim=c(0,5), ylim=c(0,5),
{

    ## remove missing values
    grade1b <- grade1a[(is.na(grade1a) == F) & (is.na(grade2a) == F)]
    grade2b <- grade2a[(is.na(grade1a) == F) & (is.na(grade2a) == F)]


    ## remove other values
    ##grade1 <- grade1b[(grade1b != cantgrade) & (grade2b != cantgrade)]
    ##grade2 <- grade2b[(grade1b != cantgrade) & (grade2b != cantgrade)]

    grade1 <- grade1b[multijudge(grade1b,cantgrade)==F & multijudge(grade2b,cantgrade)==F]
    grade2 <- grade2b[multijudge(grade1b,cantgrade)==F & multijudge(grade2b,cantgrade)==F]

    ## change data into character format to avoid data converting problem
    ##grade1 <- as.numeric(as.character(grade1))
    ##grade2 <- as.numeric(as.character(grade2))
    ## put in basic outline of plot areas

    plot.new()
    ## par(new = TRUE)
    par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    polygon(c(0, 0, 1, 1), c(0, 0.05, 0.05, 0), col = (grey(0.8)))   #change color
    polygon(c(0, 0, 1, 1), c(1, 0.87, 0.87, 1), col = (grey(0.8)))
    polygon(c(0, 0, 1, 1), c(0.255, 0.2, 0.2, 0.255), col = (grey(0.8)))
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), density = 0, lwd = 3)
    polygon(c(0, 0, 1, 1), c(0, 0.05, 0.05, 0), density = 0, lwd = 3)
    polygon(c(0, 0, 1, 1), c(1, 0.87, 0.87, 1), density = 0, lwd = 3)
    segments(0.25, 0.255, 0.25, 0.05, lty = 1, lwd = 3)
    if (range3 == 9999){
        segments(0.5, 0.255, 0.5, 0.05, lwd = 3)
        segments(0.75, 0.255, 0.75, 0.05, lwd = 3)
    } else{
        segments(0.5, 0.255, 0.5, 0.05, lwd = 3)
        segments(0.67, 0.255, 0.67, 0.05, lwd = 3)
        segments(0.84, 0.255, 0.84, 0.05, lwd = 3)
    }
    segments(0, 0.255, 1, 0.255, lwd = 3)
    segments(0, 0.2, 1, 0.2, lwd = 1)

    ## put in title and date signature
    if(grepl(pattern = "#", maintitle, fixed = TRUE)){

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

    ## range of x-axis
    xmin <- min(grade1,grade2)
    xmax <- max(grade1,grade2)
    ## range of y-axis
    ymin <- min(grade1,grade2)
    ymax <- max(grade1,grade2)

    diff <- grade2-grade1

    ## get mean and sd of differences for future calcs
    diffmean <- round(mean(diff), digits = 2)
    diffsd <- sqrt(var(diff))
    diffsd2 <- round(diffsd * 1.96, digits = 2)
    ciyhi <- diffmean + diffsd2
    ciylo <- diffmean - diffsd2

    ## plot labels, comments
    ##text(0.5, 0.295, paste("Grader:",label1), cex = 0.8)
    text(0.5,0.295,paste(label1),cex=0.95)
    ##text(0.25,0.594,paste("Grader:",label2),cex=0.8,srt=90)
    text(0.25,0.594,paste(label2),cex=0.95,srt=90)

    ## creating the mean box
    ## par(new = TRUE)
    par(fig = c(0, 0.25, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)

#    points(x = 0.1, y = 0.1, pch = 16, col = 'blue')

    text(0.5, 0.85, "Mean", cex = 1)
    text(0.5, 0.55, paste(label2, " is"), cex = 0.8)
    if(diffmean > 0)
    {
        text(0.5, 0.4, paste(diffmean, " HIGHER"), cex = 1)
        text(0.5, 0.25, paste("than ", label1), cex = 0.8)
    } else if(diffmean < 0)
    {
        text(0.5, 0.4, paste( - diffmean, " LOWER"), cex = 1)
        text(0.5, 0.25, paste("than ", label1), cex = 0.8)
    } else if(diffmean == 0)
    {
        text(0.5, 0.4, "the SAME AS", cex = 1)
        text(0.5, 0.25, label1, cex = 0.8)
    }



    ########################################################################################
    ## Creating the intra-class correlation box
    ## par(new = TRUE)
    par(fig = c(0.25, 0.5, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), new = TRUE)
    text(0.5, 0.85, "Intra-Class Correlation", cex = 1)
    msb <- var(grade1 + grade2)/2
    msw <- sum((grade1 - grade2)^2)/(2 * length(grade1))
    icc <- round((msb - msw)/(msb + msw), 3)
    text(0.5, 0.4, icc, cex = 1)

    ## creating the within1 box
    ## par(new = TRUE)
    if (range3 == 9999) {
        par(fig = c(0.5, 0.75, 0.05, 0.255), mar = c(0, 0, 0, 0),
            usr = c(0, 1, 0, 1), new = TRUE)
    } else {
        par(fig = c(0.5, 0.67, 0.05, 0.255), mar = c(0, 0, 0, 0),
            usr = c(0, 1, 0, 1), new = TRUE)
    }
    text(0.5, 0.85, paste("Within ", range1), cex = 1)
    text(0.5, 0.5, paste(length(grade2[abs(diff) <= range1]), "of", length(grade2)))
    text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range1])/length(grade2), digits = 3) * 100, "% )"))

    ## creating the within2 box
    ## par(new = TRUE)
    if (range3 == 9999){
        par(fig = c(0.75, 1, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1),
            new = TRUE)
    } else {
        par(fig = c(0.67, 0.84, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1),
            new = TRUE)
    }
    text(0.5, 0.85, paste("Within ", range2), cex = 1)
    text(0.5, 0.5, paste(length(grade2[abs(diff) <= range2]), "of", length(grade2)))
    text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range2])/length(grade2), digits = 3) * 100, "% )"))

    ## creating the within3 box
    if (range3 != 9999){
        par(fig = c(0.84, 1, 0.05, 0.255), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1),
            new = TRUE)
        text(0.5, 0.85, paste("Within ", range3), cex = 1)
        text(0.5, 0.5, paste(length(grade2[abs(diff) <= range3]), "of", length(grade2)))
        text(0.5, 0.3, paste("(", round(length(grade2[abs(diff) <= range3])/length(grade2), digits = 3) * 100, "% )"))
    }


    ## plot region definition
    ##par(new = TRUE)
    ##par(fig = c(0.05, 0.95, 0.275, 0.825), mar = c(4, 4, 0, 0))
    par(fig=c(0.225,0.775,0.275,0.825),mar=c(4,5,0,5), new = TRUE)
    plot(grade1, grade2, pch = 1, type = "p",
         xlim = c(xmin, xmax),
         ylim = c(ymin,ymax),
         lab=lab,
         xlab = "", ylab = "", las =1, cex = 0.4) ##xlim = xlim, ylim = ylim,
    ## draw diagnol line
    segments(xmin,ymin,xmax,ymax,lty=1, col='red')


    ## summary values
    tot <- length(grade2)
    awr1 <- length(grade2[abs(diff) <= range1])
    awr1p <- round(awr1/tot*100)
    awr2 <- length(grade2[abs(diff) <= range2])
    awr2p <- round(awr2/tot*100)

    ##if (range1 == 0.5 & range2 == 1){
    ##summary<-c(tot,awr1,awr1p,awr2,awr2p,"-","-","-","-",icc)
                                        #}
                                        #else if (range1 ==1 & range2 == 5){
                                        #summary<-c(tot,"-","-",awr1,awr1p,"-","-",awr2,awr2p,icc)
                                        #}
                                        #else {
                                        #summary<-c(tot,"-","-",awr1,awr1p,awr2,awr2p,"-","-",icc)


                                        #}

    return(c(tot,icc))

}
