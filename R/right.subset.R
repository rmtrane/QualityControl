#'

## Function to select the right subset. Used with var names as input.

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


#'
right.subset.R <- function(x, var, out = c('ST', 'QC'), sep = '.', nes = ''){
  if('dplyr' %in% loadedNamespaces()){
    tmp <- x %>%
      select(contains(var)) %>%
      mutate(ss = apply(., 1, function(x){ sum(x %in% nes) == 0})) %>%
      filter(ss)

    return(tmp[,colnames(tmp) %in% paste(var, out, sep = sep)])

  } else {
    wh <- which(!x[,paste(var, 'ST', sep = sep)] %in% nes &
                !x[,paste(var, 'QC', sep = sep)] %in% nes)

    return(x[wh,paste(var, out, sep = sep)])
  }
}


