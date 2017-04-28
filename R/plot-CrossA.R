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
#' @param angle numeric; should variable categories be angled when added?
#' @param ext.labels if NULL (default), labels printed in squares. Otherwise, character
#' vector with same length as there are levels of the variable considered, with strings
#' to be stand-in for labels in squares while actual labels are printed in the lower left
#' corner. If true, numbers are used as labels and automatically.
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
           ext.labels = NULL){


    if(names(dev.cur()) == 'pdf' & show.plot){
      message("show.plot not allowed when writing to PDF!!!!")
      return('STOP')
    }


    CrossA <- list()

    par(ask = show.plot)

    if(vars == 'all'){
      Vars <- crossAs
    } else {
      if(!is.null(names(crossAs))){
        if(!vars %in% names(crossAs)){
          message("'vars' should contain only character strings found among the names of 'crossAs'")
          return()
        } else {
          Vars <- crossAs[vars]
        }
      } else {
        if(!vars %in% crossAs){
          warning("'vars' should contain only character strings found in 'crossAs'")
        } else {
          Vars <- vars
        }
      }
    }

    if(length(ext.labels) < length(Vars) & !is.null(ext.labels)){
        extlabs <- rep(ext.labels, length(Vars))

        if(!is.null(names(Vars))){
            names.extlabs <- names(Vars)
        } else {
            names.extlabs <- Vars
        }

        names(extlabs) <- names.extlabs
    } else {
        extlabs <- ext.labels
    }

    for (var in Vars){
      var.name <- names(which(Vars == var))
      if(is.null(var.name))
        var.name <- var

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
                                     ext.labels = extlabs[[var.name]])
      }
    }

    return(CrossA)
  }
