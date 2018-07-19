#' Load file from data.dir
#'
#' Loads the file called `file` from folder `data.dir` and creates `Project` variable with value filename
#'
#' @param data.dir data directory in which the file is located
#' @param file full name of file to be loaded (i.e. of the form 'filename.xlsx')
#'
#' @export

load.file <- function(data.dir, file){
  out <- read_excel(paste0(data.dir, file)) %>%
    mutate(Project = strsplit(file, fixed = TRUE, split = '.')[[1]][1])

  return(out)
}

#' Recode DR Severity Levels
#'
#' Takes a vector of DR Severity levels and returns the recoded values. 'Not applicable' is turned into NA.
#'
#' @param DRSeverity vector of severity levels to be recoded
#'
#' @export

recode_DRsev <- function(DRSeverity){

  DRSeverity <- case_when(DRSeverity == 'Not applicable' ~ as.character(NA),
                          TRUE ~ str_sub(DRSeverity, start = 1, end = 2))

  recode <- case_when(
    DRSeverity %in% c(10, 12) ~ 1,
    DRSeverity %in% c(14, 15, 20) ~ 2,
    DRSeverity %in% c(35) ~ 3,
    DRSeverity %in% c(43) ~ 4,
    DRSeverity %in% c(47) ~ 5,
    DRSeverity %in% c(53) ~ 6,
    DRSeverity %in% c(60, 61) ~ 7,
    DRSeverity %in% c(65) ~ 8,
    DRSeverity %in% c(71) ~ 9,
    DRSeverity %in% c(75) ~ 10,
    DRSeverity %in% c(81) ~ 11,
    DRSeverity %in% c(85) ~ 12,
    DRSeverity %in% c(90) ~ 90,
    TRUE ~ as.numeric(NA)
  ) %>% factor(levels = c(1:12, 90, NA))

  return(recode)

}


#' Function to replace Absent and Questionable levels with A/Q.
#'
#' Takes a character vector or factor and returns a vector where Absent and Questionable entries are replaced by A/Q.
#'
#' @param x character vector or factor
#' @param levels if NULL (defualt), unique values of x will be used. If provided, will be used as levels.
#'
#' @export

AQFactor <- function(x, levels = NULL){

  if(!is.factor(x))  x <- factor(x)

  wh <- grepl(x = levels(x), pattern = 'Absent|Questionable', ignore.case = TRUE)
  levels(x)[wh] <- 'A/Q'

  if(!is.null(levels)) levels(x) <- c('A/Q', levels)

  message(paste0('Current levels:\n',
                 '---------------\n',
                 paste(levels(x), collapse = '\n')))

  return(x)
}


multijudge=function(data,values){
  index=NULL
  for(i in 1:length(values)){
    index=c(index,which(data==values[i]))
  }
  result=as.logical(rep(0,length(data)))
  result[index]=TRUE
  return(result)
}

#' Interclass Correlation
#'
#' Calculates interclass correlation between two graders.
#'
#' @param grade1 vector containing gradings done by grader 1
#' @param grade2 vector containing gradings done by grader 2
#' @param digits number of digits to round to; default 3
#'
#' @export

ICC <- function(grade1, grade2, digits = 3){

  msb <- var(grade1 + grade2)/2
  msw <- sum((grade1 - grade2)^2)/(2 * length(grade1))
  icc <- round((msb - msw)/(msb + msw), digits)

  return(icc)
}


right.subset <- function(x, var, out, sep = '.', nes = ''){
  wh <- which(!x[,paste(var, out, sep = sep)] %in% nes &
                !x[,paste(var, out, sep = sep)] %in% nes)
  return(x[wh,paste(var, out, sep = sep)])
}

right.subset2 <- function(x, var, out = c('ST', 'QC')){
  wh <- which(x[,paste(var, 'ST', sep = '.')] != '' &
                x[,paste(var, 'QC', sep = '.')] != '')
  return(x[wh,paste(var, out, sep = '.')])
}

#' Right Subset
#'
#' Function to select the right subset of the data
#'
#' @param x data
#' @param var variable to select
#' @param out do you want ST, QC or both ('ST', 'QC')?
#' @param sep how are variables separated from ST/QC specification?
#' @param nes character vector specifying if any strings should be excluded.
#'
#' @export

right.subset.R <- function(x, var, out = c('ST', 'QC'), sep = '.', nes = ''){
  if('dplyr' %in% loadedNamespaces()){
    tmp <- x[, paste(var, c('ST', 'QC'), sep = sep)] %>%
      mutate(ss = apply(., 1, function(x){ sum(x %in% nes) == 0})) %>%
      filter(ss)

    tmp <- tmp[,paste(var, out, sep = sep)]

    if(!is.null(dim(tmp)) & length(out) < 2){
      tmp <- tmp[[1]]

      if(is.factor(tmp)){
        tmp <- factor(tmp, levels = levels(tmp)[!levels(tmp) %in% nes])
      }
    }

    return(tmp)

  } else {
    wh <- which(!x[,paste(var, 'ST', sep = sep)] %in% nes &
                  !x[,paste(var, 'QC', sep = sep)] %in% nes)

    return(x[wh,paste(var, out, sep = sep)])
  }
}


#' Convert Names
#'
#' Take any name from any of the given naming conventions and
#' spit out the wanted name.
#'
#' @param names Names to convert
#' @param out_format Should names be returned as Full Name, First Name, Last Name, or Number
#' @param ig.case ignore case when matching?
#' @param conversion_document either matrix with two columns or path to document containing conversion
#' scheme. (If document, remember 'load = TRUE'.) Should consist of two columns, first with first and
#' last names separated by space, second with everything to match against.
#' @param load logical; if TRUE, conversion_document is assumed to be an excel spreadsheet and will be read using read_excel from the readxl package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' convert_names(names = c('pak', 'sheila'),
#'               out_format = c('Full Name'),
#'               conversion_document = 'K:/Stat/RMT/grader_names.xlsx',
#'               load = TRUE,
#'               ig.case = TRUE)}


convert_names <- function(names,
                          out_format = c('Full Name', 'First Name', 'Last Name', 'Number'),
                          conversion_document = 'K:/Stat/RMT/grader_names.xlsx',
                          load = TRUE,
                          ig.case = TRUE){


  if (load)
    conv.scheme <- readxl::read_excel(conversion_document) %>%
      dplyr::select(`Full Name`, `All Known Names`) %>%
      group_by(`Full Name`) %>%
      filter(row_number() == 1)

  conv.scheme <- conv.scheme %>%
    mutate(`All Known Names` = str_split(`All Known Names`,
                                         pattern = ';')) %>%
    unnest(`All Known Names`) %>%
    mutate(`All Known Names` = str_trim(`All Known Names`))

  full_names <- tibble(`All Known Names` = as.character(names)) %>%
    left_join(conv.scheme, by = 'All Known Names') %>%
    rename(input_name = `All Known Names`) %>%
    mutate(`Names` = `Full Name`) %>%
    separate(`Names`, into = c('First Name', 'Last Name'), sep = ' ', extra = 'merge') %>%
    separate(`Last Name`, into = c('Middle Name', 'Last Name'), fill = 'left') %>%
    left_join(conv.scheme %>% filter(!is.na(as.numeric(`All Known Names`))) %>% rename(Number = `All Known Names`))

  if(length(out_format) == 1){
    ret <- full_names[[out_format]]
  } else{
    ret <- full_names[, out_format]
  }

  return(ret)
}

#' Convert Modalities
#'
#' Takes any modality from any of the modalities listed in the spreadsheet provided and
#' spits out any combination of the following: New Label, Eyes Graded, Minutes.
#'
#' @param modals Modalities to convert
#' @param output Any combination of 'New Labels' and 'Modality'
#' @param conversion_document Document from which the conversion matrix should be loaded.
#' @param sheet If not first sheet in conversion_document, then specify here
#'
#' @export

convert_modals <- function(modals,
                           output = c('New Labels', 'Modality'),
                           conversion_document = "K:/Stat/RMT/Efficiency Reports/Data/modalities_conversion.xlsx",
                           sheet = 'modals_only'){

  tmp <- readxl::read_excel(path = conversion_document,
                            sheet = sheet) %>%
    mutate(Modals = str_split(Modals, pattern = ',')) %>%
    unnest() %>%
    mutate(Modals = str_trim(Modals)) %>%
    dplyr::rename(Modality = Modals)

  if(length(output) == 1){
    tmp <- tmp %>%
      right_join(tibble(Modality = modals)) %>%
      pull(!!output)
  } else {
    tmp <- tmp %>%
      right_join(tibble(Modality = modals)) %>%
      dplyr::select(!!output)
  }

  return(tmp)

}


#' Wrap Sentence
#'
#' This is used to wrap long level names of factors for plotting of crossA plots.
#' It will first split according to commas, then according to words.
#'
#' @param string sentence to wrap
#' @param width wanted width of outcome
#'
#' @export


wrap_sentence1 <- function(string, width) {

  words <- unlist(strsplit(string, " "))

  fullsentence <- words[1]
  checklen <- words[1]
  if(length(words) < 2){
    return(fullsentence)
  }

  for(i in 2:length(words)) {
    checklen <- paste(checklen, words[i])
    ## print(fullsentence)
    ## print(checklen)
    if(nchar(checklen) > width) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- words[i]
    }
    fullsentence <- paste(fullsentence, words[i])
  }

  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}

#' @export
wrap_sentence <- function(string, width) {

  string2 <- gsub(", ", ",#", string)

  bisentences <- as.list(unlist(strsplit(string2, "#", fixed = TRUE)))

  res <- lapply(X = bisentences, FUN = wrap_sentence1, width = width)

  final.sentence <- paste0(unlist(res),
                           collapse = '\n')

  return(final.sentence)

}

# program to create a martix with "x" as diagonal values and "y" as upper and lower
# off-diagonal values.
# usage: block( x,y,ncol): x = value of diagonal
#			   y = 2x1 vector in off diagonal
#			 ncol= numbers of colum
# Example:
#  block.diag(1,2,5) =
#  1    2    0    0    0
#  2    1    2    0    0
#  0    2    1    2    0
#  0    0    2    1    2
#  0    0    0    2    1
#   block.diag(1,c(2,3),5) =
#  1	2	3	0	0
#	2	1	2	3	0
#	3	2	1	2	3
#	0	3	2	1	2
#	0 	0	3 	2	1
#***************************************************************************************
block.diag <- function(x,y,ncol){
  if (ncol < 1 ) stop("numbers of column must be greater than 0")

  if (ncol > 1){
    tmp1 <- diag(x,ncol)
    tmp2 <- diag(y[1],ncol-1)
    tmp2 <- rbind(cbind(0,tmp2),0)
    tmp4 <- t(tmp2)+tmp2

    if (length(y) > 1){
      tmp3 <- diag(y[2], ncol-2)
      tmp3 <- rbind(cbind(0, tmp3),0)
      tmp3 <- rbind(cbind(0, tmp3),0)
      tmp5 <- t(tmp3) + tmp3
      tmp6 <- tmp1 + tmp4 + tmp5
    } else {
      tmp6 <- tmp1 + tmp4
    }

    return(tmp6)
  }

  if (ncol==1) {
    tmp1 <- diag(1,ncol)

    return(tmp1)
  }
}
