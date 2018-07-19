#' Wrapper for twograders.plotx and sd.plotaLimit
#'
#' For a given dataset and a named vector with
#' variables from that dataset, this function creates
#' twograders.plotx and sd.plotaLimit (when appropriate) plots
#' for all variables.
#'
#' @param data dataset
#' @param plotX named vector containing variables from data. Names are used as titles for
#' the plots. If not named, the variable itself will be used.
#' @param vars indicates which variables should be plotted. Default is 'all'
#' @param var.unit named vector containing strings to be included as units. Added to the title in parenthesis. Expressions can be included by enclosing in #. For example: #mm^2#. Names of vector should correspond to names of crossA vector, or if crossA vector is unnamed, to elements of crossA.
#' @param lab1 lab1 in crossa (default 'ST')
#' @param lab2 lab2 in crossa (default 'QC')
#' @param foottext foot text for plots (default NULL)
#' @param show.plot logical; should the plots be plotted one by one? (click for next, default FALSE)
#' @param show.progress logical; should the variable names be printed when plotted? (defualt TRUE)
#' @param sep separation symbol between variable and QC/ST in data. (default '.')
#' @param nes character vector with strings that should be ignored in right.subset. (default c('', NA))
#' @param title2 character string to use as subtitle for plots
#' @param mar margin to be passed to plots. Set to c(0,0,0,0) when printing multiple plots to PDF.
#' @param plot.type character vector specifying which figure(s) to plot. Can be one: "sd.plotaLimit", "twograders.plotx", or c('sd.plotaLimit', 'twograders.plotx') (latter is default)
#' @param CI_use_quantiles logical; passed to sd.plotaLimit.
#'
#' @export
#'
#'


Plot.plotx.sd.Limit <-
  function(data = NULL,               # data
           plotX = NULL,              # Named vector containing variables from
                                      # data. Names are used as titles for
                                      # the plots. If not named, the variable itself
                                      # will be used.
           var.units = NULL,          # Named vector specifying unit for given variable. Added to title in parenthesis.
           vars = 'all',              # Can be used to specify which variable(s) to
                                      # create plot(s) for. Default is 'all'
           lab1 = 'ST',
           lab2 = 'QC',
           sep = '.',
           nes = c('', NA),
           ranges = c(1,2,5),
           cantgrade = 8888, ## UPDATE!!!!!
           foottext = '',
           title2 = '',
           plot.type = c('sd.plotaLimit', 'twograders.plotx'),
           show.progress = TRUE,
           show.plot = FALSE,
           mar = c(0,0,0,0),
           CI_use_quantiles = FALSE){

    ## Check if current device is pdf. If so, show.plot MUST be false.
    if(names(dev.cur()) == 'pdf' & show.plot){
      warning("show.plot not allowed when writing to PDF!!!!")
      return('STOP')
    }

    ## Create empty lists for outputs
    PlotX <- list()
    PlotA <- list()

    par(ask = show.plot)

    ## Check vars to be plotted.
    if(vars == 'all'){
      Vars <- plotX
    } else {
      ## If not all, check if specified strings are in names of plotX
      if(!vars %in% names(plotX)){
        message("'vars' should contain only character strings found among the names of 'plotX'")
        return()
      } else {
        Vars <- plotX[vars]
      }
    }

    ## Loop over Vars
    for (var in Vars){
      var.name <- names(which(Vars == var))

      ## If var.name not specified, use variable.
      if(is.null(var.name))
        var.name <- var

      ## Create maintitle
      maintitle <- var.name

      ## If a vector with units is provided, we paste this to the main title.
      if(!is.null(var.units)){
        if(var.name %in% names(var.units)){
          maintitle <- paste0(var.name, " (#", var.units[[var.name]], "#)")
        }
      }

      if(show.progress){
        print(var.name)
      }
      ## We check to see if we have at least one complete case, i.e. at least
      ## one where we have both QC and ST gradings. !! as.character is necessary to make sure var is NOT a factor, as that messes with as.numeric
      data[[paste(var, 'ST', sep = sep)]] <- as.numeric(as.character(data[[paste(var, 'ST', sep = sep)]]))
      data[[paste(var, 'QC', sep = sep)]] <- as.numeric(as.character(data[[paste(var, 'QC', sep = sep)]]))

      l.sub <- right.subset.R(x = data,
                              var = var,
                              out = c('QC'),
                              nes = c(nes, cantgrade[var.name])) %>%
        length

      ## Plot twograders.plotx if specified
      if('twograders.plotx'%in% plot.type){
        if(!is.null(l.sub) & !is.na(l.sub) & l.sub > 1){
          PlotX[[var.name]] <-
            twograders.plotx(
              grade1a = as.numeric(right.subset.R(x = data,
                                                  var = var,
                                                  out = 'ST',
                                                  nes = nes)),
              grade2a = as.numeric(right.subset.R(x = data,
                                                  var = var,
                                                  out = 'QC',
                                                  nes = nes)),
              label1 = lab1,
              label2 = lab2,
              range1=ranges[1],
              range2=ranges[2],
              range3=ranges[3],
              cantgrade=cantgrade[var.name],
              maintitle= maintitle,
              subtitle = title2,
              footer=foottext,
              mar = mar)
        }
      }


      ## Plot
      if('sd.plotaLimit' %in% plot.type){
        ## sd.plotaLimit. First we check to see if we have more than eight different
        ## means.
        STQC <- right.subset.R(x = data,
                               var = var,
                               out = c('ST', 'QC'),
                               nes = c(nes, cantgrade[var.name]))
        if(!is.null(STQC)){

          means <- STQC %>%
            mutate_all(as.numeric) %>%
            mutate(row_means = rowMeans(., na.rm = TRUE))#apply(as.matrix(STQC), 1, function(x) mean(as.numeric(x)))

          if(length(unique(means$row_means)) > 7){#if(length(unique(means)) > 7){
            PlotA[[var.name]] <-
              sd.plotaLimit(grade1a =
                              as.numeric(unlist(c(right.subset.R(x = data,
                                                                 var = var,
                                                                 out = 'ST',
                                                                 nes = nes)))),
                            grade2a =
                              as.numeric(unlist(c(right.subset.R(x = data,
                                                                 var = var,
                                                                 out = 'QC',
                                                                 nes = nes)))),
                            label1 = lab1,
                            label2 = lab2,
                            range1 = ranges[1],
                            range2 = ranges[2],
                            range3 = ranges[3],
                            cantgrade = cantgrade[var.name],
                            maintitle = maintitle,
                            subtitle = title2,
                            footer = foottext,
                            mar = mar,
                            CI_use_quantiles = CI_use_quantiles)
          }
        }
      }
    }

    return(PlotX)
  }
