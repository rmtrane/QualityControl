#' Wrapper for crossA
#'
#' For a given dataset and a named vector with
#' variables from that dataset, this function creates
#' crossa plots for all variables.
#'
#' @param data dataset that includes columns named <var>.QC and <var>.ST for all <var> in crossAs
#' @param crossAs named vector containing variables from
#' data. Names are used as titles for
#' the plots. If not named, the variable itself
#' will be used.
#' @param vars indicates which variables should be plotted. Default is 'all'
#' @param lab1 lab1 in crossa (default 'ST')
#' @param lab2 lab2 in crossa (default 'QC')
#' @param cannotgrades Named vector indicating how 'cannot grade' is
#' coded. Names should be identical to names(crossAs)
#' @param foottext foot text for plots (default NULL)
#' @param show.plot logical; should the plots be plotted one by one? (click for next, default FALSE)
#' @param sep separation symbol between variable and QC/ST in data. (default '.')
#' @param nes character vector with strings that should be ignored in right.subset. (default c('', NA))
#' @param title2 character string to use as subtitle for plots
#' @param ext.labels if NULL (default), labels printed in squares. Otherwise, character
#' vector with same length as vars, with strings to be stand-in for labels in squares while actual labels
#' are printed in the lower left corner. If elements are not strings, but TRUE/FALSE, numbers are used as labels
#' for TRUE entries. If simply TRUE, external labels will be used for all variables.
#' @param cex numeric; passed to crossa. Default 1.
#' @param PLOT logical; if TRUE (default), figure is plotted. If FALSE, summary returned without any figures.
#'
#' @export
#'


Plot.crossA <-
  function(data,                      # Data
           crossAs = NULL,            # Named vector containing variables from
           # data. Names are used as titles for
           # the plots. If not named, the variable itself
           # will be used.
           vars = 'all',              # Can be used to specify which variable(s) to
           # create plot(s) for. Default is 'all'
           lab1 = 'ST',               #
           lab2 = 'QC',               #
           cannotgrades = NULL,       # Named vector indicating how 'cannot grade' is
           # coded. Names should be identical to names(crossAs)
           foottext = NULL,           # Foot text for plots
           show.plot = FALSE,         # Should plots be showed one by one? (click for next)
           show.progress = TRUE,
           sep = '.',                 # Separation symbol between variable and QC/ST.
           nes = c('', NA),           # Strings to exclude from plots.
           title2 = '',               # Add subtitle to plots.
           # Used for grader specific plots.
           ext.labels = NULL,
           cex = 1,
           PLOT = TRUE,
           mar = c(0,0,0,0)){


    ## Check if current device is 'pdf'. If show, show.plot MUST be FALSE
    if(names(dev.cur()) == 'pdf' & show.plot){
      message("show.plot not allowed when writing to PDF!!!!")
      return('STOP')
    }

    ## Empty list to hold results
    CrossA <- list()

    par(ask = show.plot)

    ## Check vars to be plotted.
    if(vars == 'all'){
      Vars <- crossAs
    } else {
      ## If crossAs is named...
      if(!is.null(names(crossAs))){
        ## ... make sure vars is in names.
        if(!vars %in% names(crossAs)){
          message("'vars' should contain only character strings found among the names of 'crossAs'")
          return()
        } else {
          Vars <- crossAs[vars]
        }
      } else {
        ## ... otherwise make sure vars matches an element of crossAs.
        if(!vars %in% crossAs){
          warning("'vars' should contain only character strings found in 'crossAs'")
        } else {
          Vars <- vars
        }
      }
    }

    ## Check if Vars is named. If it is not, name it
    if(is.null(names(Vars)))
      names(Vars) <- Vars

    ## Some external labels magic. If ext.labels is provided...
    if(!is.null(ext.labels)){
      ## ... but with length one (i.e. logical, i.e. TRUE/FALSE) and not named (since if named, it would just be providing ONE specific variable)
      if(length(ext.labels) == 1 & is.null(names(ext.labels))){
        ## We create vector of same length as Vars, with same names filled with ext.labels (i.e. TRUE or FALSE)
        extlabs <- rep(ext.labels, length(Vars))
        names(extlabs) <- names(Vars)

      } else {
        ## If length is less than the number of variables, we fill out the rest with FALSE.
        if(length(ext.labels) < length(Vars)){
          ## This is done by first creating vector of length as Vars and names as Vars with all entries FALSE
          extlabs <- rep(FALSE, length(Vars))

          names(extlabs) <- names(Vars)

          ## ... and then filling out the ext.labels that are specified.
          extlabs[names(ext.labels)] <- ext.labels

        } else {
          ## If length(ext.labels) == length(Vars), we need not make any corrections
          extlabs <- ext.labels
        }
      }
    } else {
      ## If ext.labels is NULL, simply use FALSE
      extlabs <- rep(FALSE, length(Vars))
      names(extlabs) <- names(Vars)

    }

    ## Loop over all Vars and plot
    for (var in Vars){
      var.name <- names(which(Vars == var))

      if(show.progress){
        print(var.name)
      }

      if (length(right.subset.R(x = data, var = var, out = 'ST', nes = nes)) > 0){

        CrossA[[var.name]] <- crossa(data1a = right.subset.R(x = data,
                                                             var = var,
                                                             out = 'ST',
                                                             sep = sep,
                                                             nes = nes),
                                     lab1 = lab1,
                                     data2a = right.subset.R(x = data,
                                                             var = var,
                                                             out = 'QC',
                                                             sep = sep,
                                                             nes = nes),
                                     lab2 = lab2,
                                     cantgrade = cannotgrades[[var.name]],
                                     title1 = var.name,
                                     title2 = title2,
                                     footer = foottext,
                                     ext.labels = extlabs[[var.name]],
                                     PLOT = PLOT,
                                     cex = cex,
                                     mar = mar)
      }
    }

    return(CrossA)
  }
