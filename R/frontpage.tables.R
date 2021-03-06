#' Table for Front Page of Report
#'
#' Summarizes the data for the front page table. Output is an array with four columns:
#' Trial/Modality, N, Grading Start Date, and Grading End Date. To be used in report.rmd.
#'
#' @param data data set used for analysis
#' @param QCST use QC or ST for PerformedDate variable
#' @param col1 default 'Project'. Indicates what should be in column 1, and hence, what should be the stratifying variable? Usually choose between Project/Study or Grader.
#' @param date_format string specifying the date format.
#'
#' @export

frontpage.table <- function(data, QCST = c('QC', 'ST'), col1 = 'Project', col1.name, date_format = '%Y-%m-%d'){

  ## Vector with column names for output table
  colnames.table <- c(col1.name, 'N', 'Grading Start Date', 'Grading End Date')

  ## Create empty table to fill.
  Tab <- array(NA,
               dim = c(length(unique(data[[col1]]))+2,
                       length(colnames.table)),
               dimnames = list(c(sort(as.character(unique(data[[col1]]))), '', 'Overall'),
                               colnames.table))

  ## Create DATE in data set.
  if(QCST == 'QC'){
    data$DATE <- data$PerformedDate.QC
  } else {
    if(QCST == 'ST'){
      data$DATE <- data$PerformedDate.ST
    } else {
      message("ERROR: QCST must be either 'QC' or 'ST'")
      return()
    }
  }

  ## Loop over values of col1
  for (tri in rownames(Tab)[-(nrow(Tab)-c(1,0))]){
    ## Get subset of data where col1 == tri (current loop value)
    sub.data <- data[data[[col1]] == tri,]

    ## Fill out tri part of table
    Tab[tri,] <- c(tri,
                   nrow(sub.data),
                   format(min(as.Date(sub.data$DATE, format = date_format), na.rm = T),
                          format = "%b %d, %Y"),
                   format(max(as.Date(sub.data$DATE, format = date_format), na.rm = T),
                          format = "%b %d, %Y"))
  }

  ## Fill out 'Overall' part of table
  Tab['Overall',] <- c('Overall',
                       sum(as.numeric(Tab[,'N']), na.rm = T),
                       format(min(as.Date(Tab[,'Grading Start Date'], format = '%b %d, %Y'),
                                  na.rm = T),
                              format = '%b %d, %Y'),
                       format(max(as.Date(Tab[,'Grading End Date'], format = '%b %d, %Y'),
                                  na.rm = T),
                              format = '%b %d, %Y'))

  return(Tab)

}


#' Table for Front Page of Grader Specific Report
#'
#' Creates table with four columns: QC Grader, N, QC Grading Start Date, QC Grading End Date
#'
#' @param data data set used for analysis
#' @param QCST use QC or ST dates?
#'
#' @export


grader.frontpage.table <- function(data, QCST = c('ST', 'QC')){

  data <- subset(data, !is.na(Grader))

  colnames.table <- c('QC Grader', 'N', 'QC Grading Start Date', 'QC Grading End Date')

  Tab <- array(NA,
               dim = c(length(unique(data$Grader))+2,
                       length(colnames.table)),
               dimnames = list(c(unique(data$Grader), '', 'Overall'),
                               colnames.table))

  if(QCST == 'QC'){
    data$DATE <- data$PerformedDate.QC
  } else {
    if(QCST == 'ST'){
      data$DATE <- data$PerformedDate.ST
    } else {
      message("ERROR: QCST must be either 'QC' or 'ST'")
      return()
    }
  }


  for ( gr in rownames(Tab)[-(nrow(Tab)-c(1,0))]){
    sub.data <- subset(data, Grader == gr)
    Tab[gr,] <- c(gr,
                  nrow(sub.data),
                  format(min(as.Date(sub.data$DATE, format = '%m/%d/%Y'), na.rm = T),
                         format = '%b %d, %Y'),
                  format(max(as.Date(sub.data$DATE, format = '%m/%d/%Y'), na.rm = T),
                         format = "%b %d, %Y"))
  }

  Tab['Overall',] <- c('Overall',
                       sum(as.numeric(Tab[,'N']), na.rm = T),
                       format(min(as.Date(Tab[,'QC Grading Start Date'], format = "%b %d, %Y"),
                                  na.rm = T),
                              format = "%b %d, %Y"),
                       format(max(as.Date(Tab[,'QC Grading End Date'], format = "%b %d, %Y"),
                                  na.rm = T),
                              format = "%b %d, %Y"))

  return(Tab)

}


