#' Wrapper for twograders.plotx and sd.plotaLimit
#'
#' For a given dataset and a named vector with
#' variables from that dataset, this function creates
#' twograders.plotx and sd.plotaLimit (when appropriate) plots
#' for all variables.
#'
#' @param data dataset
#' @param plotX named vector containing variables from
#' data. Names are used as titles for
#' the plots. If not named, the variable itself
#' will be used.
#' @param vars indicates which variables should be plotted. Default is 'all'
#' @param lab1 lab1 in crossa (default 'ST')
#' @param lab2 lab2 in crossa (default 'QC')
#' @param foottext foot text for plots (default NULL)
#' @param show.plot logical; should the plots be plotted one by one? (click for next, default FALSE)
#' @param sep separation symbol between variable and QC/ST in data. (default '.')
#' @param nes character vector with strings that should be ignored in right.subset. (default c('', NA))
#' @param title2 character string to use as subtitle for plots
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
           show.plot = FALSE){

    if(names(dev.cur()) == 'pdf' & show.plot){
      warning("show.plot not allowed when writing to PDF!!!!")
      return('STOP')
    }

    PlotX <- list()
    PlotA <- list()

    par(ask = show.plot)

    if(vars == 'all'){
      Vars <- plotX
    } else {
      if(!vars %in% names(plotX)){
        message("'vars' should contain only character strings found among the names of 'plotX'")
        return()
      } else {
        Vars <- plotX[vars]
      }
    }

    for (var in Vars){
      var.name <- names(which(Vars == var))
      if(is.null(var.name))
        var.name <- var

      if(show.progress){
        print(var.name)
      }
      ## We check to see if we have at least one complete case, i.e. at least
      ## one where we have both QC and ST gradings.
      # l.sub <- apply(right.subset.R(x = data,
      #                               var = var,
      #                               out = c('QC', 'ST'),
      #                               nes = nes),
      #                  2, function(x){
      #                      sum(unique(x) != cantgrade[var.name])
      #                  })


      l.sub <- right.subset.R(x = data,
                              var = var,
                              out = c('QC'),
                              nes = c(nes, cantgrade[var.name])) %>% length

      if('twograders.plotx'%in% plot.type){
        if(!is.null(l.sub) & !is.na(l.sub) & l.sub > 1){#min(as.numeric(l.sub)) > 0){
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
              maintitle= var.name,
              subtitle = title2,
              footer=foottext)
        }
      }

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
                            maintitle = var.name,
                            subtitle = title2,
                            footer = foottext)
          }
        }
      }
    }

    return(PlotX)
  }
