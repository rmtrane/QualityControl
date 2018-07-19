#' Create Table for End Page of QC
#'
#' Summarizes the statistics into one table
#'
#' @param style 'icc' or 'wkappa'; sets the type of table to be created
#' @param maintitle title of table
#' @param subtitle sub title of table
#' @param footer foot text
#' @param fieldname vector with names for rows in table
#' @param stat0 list of vectors with statistics to include. If NULL, statistics must be provided using stat1, ..., stat12
#' @param stat1,...,stat12 vectors with statistics to include
#' @param width numeric; defuault is 30.
#' @param cex.factor numeric; factor to multiply all cex values by.
#'
#' @export


###############################################################################
## Program:   tabulation2.R                                                  ##
## Created:   Mar, 2016                                                      ##
## Function:  This program is designed to summary statistics into            ##
##            one table                                                      ##
##                                                                           ##
## Values:    style -- type of summarry table icc(icc)                       ##
##                      or weighted kappa (wkappa)                           ##
##            maintitle -- title of the table                                ##
##            fieldname -- vector of all field (row) names                   ##
##            statistics -- vector of statistics, s.t. "sample size", etc.   ##
##                                                                           ##
## Return:    NA                                                             ##
## See also:  Modify/tabulation                                              ##
##                                                                           ##
## Others:     Use with crossb.ssc which return a vector of 7 values.        ##
##             Also updated from tabulation.scc with option to use           ##
##             list with stats as input.                                     ##
## Examples:                                                                 ##
##   maintitle<-c('Table Summarizing Lens Quality Control: Exercise 21')     ##
##   fnames<-c('Iris Pigmentation','Nuclear Color', 'Cortical Fleck',        ##
##          'Vacuole Presence', 'WACOS Presence', 'Mittendorf Dot')          ##
##                                                                           ##
##   tabulation(style='wkappa',maintitle,subtitle='',fnames,stat1, stat2  ,  ##
##               stat3, stat4, stat5, stat6)                                 ##
###############################################################################


tabulation <- function(style = c("icc", "wkappa"),
                       maintitle = "",
                       subtitle = "",
                       footer = "",
                       fieldname = "",
                       cex.factor = 1,
                       width = 30,
                       stat0 = NULL,
                       stat1 = -999,
                       stat2 = -999,
                       stat3 = -999,
                       stat4 = -999,
                       stat5 = -999,
                       stat6 = -999,
                       stat7 = -999,
                       stat8 = -999,
                       stat9 = -999,
                       stat10 = -999,
                       stat11 = -999,
                       stat12 = -999)
{

  #### the column names ###########
  if(style == "icc") {
    ctext <- c("Sample \n Size", "Agree \n Within 0.5", "(%)",
               "Agree \n Within 1.0", "(%)", " Agree \n Within 2.5", "(%)",
               "Agree \n Within 5", "(%)", "Intra-Class \n Correlation")
  } else {
    ## with 4 fewer columns than modify/tabulation
    ctext <- c("Sample \n Size", "Kappa (ICC)",
               "Weighted \n Kappa", "Exact \n Agreement", "(   %)",
               "Within \n One Step", "(  %)", "Within \n Two Steps", "(  %)")#, "")
  }

  ## Row names
  rtext <- fieldname

  ## Dimensions
  rownum <- length(rtext)
  colnum <- length(ctext)

  ## Set layout.
  par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1))
  plot.new()
  par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1),
      new = TRUE, lheight = 0.8)

  ## put in title, footer information ##
  text(0.12, 0.025, date(), cex = 0.67*cex.factor)
  text(0.5, 0.025, paste(footer), cex = 0.67*cex.factor)
  text(0.5, 0.965, paste(maintitle), cex = 1*cex.factor)
  ##1.5
  text(0.5, 0.92, paste(subtitle), cex = 1*cex.factor)
  ## put in summary statistics ##
  for(j in 1:colnum){
    text(0.18 + (0.765/colnum) * j, 0.85, paste(ctext[j]), cex = 0.6*cex.factor)
    ##0.8
    if(is.null(stat0)){
      out <- c(stat1[j], stat2[j], stat3[j],
               stat4[j], stat5[j], stat6[j],
               stat7[j], stat8[j], stat9[j],
               stat10[j], stat11[j], stat12[j])
    } else {
      out <- c()
      for (o in 1:length(stat0)){
        out <- c(out, stat0[[o]][j])
      }
    }

    ## If extra lines are needed due to long row names, this will be used to count them so that y positions can be proberly adjusted.
    extra.lines <- rep(0, rownum)

    row.y <- 0.82

    ## Fill out row names
    for(i in 1:rownum){
      if(grepl(pattern = '#', rtext[i], fixed = TRUE)){

        #tmp.rtext <- gsub(pattern = 'mm^2', x = rtext[i], replacement = "#mm^2#", fixed = TRUE)

        new.rtext <- unlist(strsplit(x = rtext[i], split = "#"))

        express <- parse(text = new.rtext[2])[[1]]

        text(0.14, row.y - 0.06 * i, bquote(.(new.rtext[1])*.(express)*.(new.rtext[3])), cex = 0.7*cex.factor)
      } else {
        extra.lines[i] <-
              str_count(string = wrap_sentence(rtext[i], width = width),
                        pattern = '\n')

        text(0.14, row.y - 0.06 * i, paste(wrap_sentence(rtext[i], width = width)),
             #rtext[i],
             cex = 0.7*cex.factor)
      }

        text(0.18 + (0.765/colnum) * j, 0.82 - 0.06 * i, paste(out[i]), cex = 0.6*cex.factor)

        segments(0.01, 0.785 - 0.06 * (i), 0.99, 0.785 - 0.06 * (i))


    }
  }
  segments(0.01, 0.89, 0.99, 0.89)
  segments(0.01, 0.785, 0.99, 0.785)
  segments(0.01, 0.05, 0.01, 0.89)
  segments(0.99, 0.05, 0.99, 0.89)
  segments(0.01, 0.05, 0.99, 0.05)
}
