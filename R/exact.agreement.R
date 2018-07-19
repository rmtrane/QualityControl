#' Table with Exact Agreement
#'
#' Calculates agreement table for overall team and for graders
#' individually.
#'
#' @param data Data to use
#' @param crossA vector with variables used for crossA tables. If named, names are used as variable labels
#' @param plotX vector with variables used for twogradersplotx. If named, names are used as variable labels
#' @param grads vector with names or graders
#' @param nes vector with character strings to exclude
#'
#' @export

exact.agree.table <- function(data, crossA, plotX, grads, nes = c('', NA),
                              cantgrade.crossA = NULL, cantgrade.plotX = NULL){


  data <- subset(data, !is.na(Grader))

  if(length(names(crossA)) == 0)
    names(crossA) <- crossA

  if(length(names(plotX)) == 0)
    names(plotX) <- plotX


  exact.agreement <- array(NA,
                           dim = c(length(c(1, names(crossA), names(plotX))), 2,
                                   length(grads) + 1),
                           dimnames = list(Variable = c('Overall',
                                                        names(crossA),
                                                        names(plotX)),
                                           c('Number graded', 'Exact Agreement/ICC'),
                                           Grader = c('Team', grads)))


  exact.agreement['Overall', 'Number graded', 'Team'] <- nrow(data)

  for (i in 1:length(crossA)){

    var <- crossA[i]

    if(!is.null(names(var))){
      vv <- names(var)
    } else {
      vv <- var
    }

    temp <- data %>%
      split(.$Grader) %>%
      map_df(.f = function(x){


        dd <- right.subset.R(x, var, c('ST','QC'), nes = c(nes, cantgrade.crossA[[vv]]))

        if(nrow(dd) > 0){
          ddd <- tibble(V0 = c(dd[,1] == dd[,2])) %>%
            summarise(Grader = unique(x$Grader),
                      V1 = sum(!is.na(V0)),
                      V2 = sum(V0, na.rm = T)) %>%
            mutate(V2 = paste0(V2, ' (',
                               round(V2/V1*100), '%)'))
        } else {
          ddd <- tibble(Grader = unique(x$Grader),
                        V1 = as.character(NA),
                        V2 = as.character(NA))
        }

        return(ddd)
      })

    exact.agreement[names(var),,temp$Grader] <- t(temp[,c('V1', 'V2')])

    dd <- apply(X = right.subset.R(data, var, c('ST', 'QC'), nes = c(nes, cantgrade.crossA[[vv]])),
                MARGIN = 1,
                FUN = function(x) x[1] == x[2])

    ddd <- c(sum(!is.na(dd)),
             sum(dd, na.rm = TRUE))

    ddd[2] <- paste0(ddd[2], ' (',
                     round(ddd[2]/ddd[1]*100), '%)')

    exact.agreement[names(var),,'Team'] <- ddd

  }

  if(!is.null(plotX)){
    for (i in 1:length(plotX)){

      var <- plotX[i]

      if(!is.null(names(var))){
        vv <- names(var)
      } else {
        vv <- var
      }

      temp <- data %>%
        split(.$Grader) %>%
        map_df(.f = function(x){

          dd <- right.subset.R(x, var, c('ST','QC'), nes = c(nes, cantgrade.plotX[[vv]]))

          if(is_tibble(dd)){
            ddd <- tibble(Grader = unique(x$Grader),
                          V1 = nrow(dd),
                          V2 = ICC(as.numeric(dd[[1]]),
                                   as.numeric(dd[[2]])))
          } else {
            ddd <- tibble(Grader = unique(x$Grader),
                          V1 = nrow(dd),
                          V2 = ICC(as.numeric(dd[,1]),
                                   as.numeric(dd[,2])))
          }

          return(ddd)
        })


      exact.agreement[names(var),,temp$Grader] <- t(temp[,c('V1', 'V2')])

      dd <- right.subset.R(data, var, c('ST','QC'),
                           nes = c(nes, cantgrade.plotX[[vv]]))

      if(is_tibble(dd)){
        ddd <- c(nrow(dd),
                 ICC(as.numeric(dd[[1]]),
                     as.numeric(dd[[2]])))
      } else {
        ddd <- c(nrow(dd),
                 ICC(as.numeric(dd[,1]),
                     as.numeric(dd[,2])))
      }

      exact.agreement[names(var),,'Team'] <- ddd
    }
  }

  return(exact.agreement)
}

#' Save Outcome of exact.agreement
#'
#' save.exact writes the results of exact.agree.table to excel spreadsheets. One for each grader together with one containing everything.
#'
#' @param exact.agreement object returned by exact.agree.table
#' @param path character string specifying path for files to be saved to
#' @param file name of file containing all data.
#'
#' @export

save.exact <- function(exact.agreement, path, file){

    vars <- dimnames(exact.agreement)$Variable[-1]
    grads <- dimnames(exact.agreement)$Grader

    exact.all <- array(data = NA,
                       dim = c(length(c(1:3, vars)),
                               2*length(grads)),
                       dimnames = list(c('Grader', '', 'Overall', vars),
                                       rep(grads, each = 2)))


    exact.all[1,] <- rep(c(grads), each = 2)
    exact.all[1,c(1:(ncol(exact.all)/2))*2] <- ''

    exact.all[2,] <- rep(c('n graded', 'Exact Agreement/ICC'), length(grads))

    for (g in dimnames(exact.agreement)$Grader){

        wh.col <- which(colnames(exact.all) == g)

        exact.all[-c(1,2), wh.col] <- exact.agreement[,,g]
    }

    ## Loop over graders and save
    for (g in grads[-which(grads == 'Team')]){

        wh.col <- which(colnames(exact.all) %in% c(g, 'Team'))
        xforxls <- data.frame(exact.all[, wh.col])
        #FILE <- paste0(path,'/', g,'.xlsx')
        FILE2 <- paste0(path,'/', g,'.csv')

        write_excel_csv(x = bind_cols(tibble(X1 = rownames(xforxls)),
                                      xforxls),
                        path = FILE2,
                        na = '',
                        col_names = FALSE)


        # WriteXLS(x = 'xforxls',
        #          ExcelFileName = FILE,
        #          row.names = TRUE,
        #          col.names = FALSE,
        #          AdjWidth = TRUE,
        #          na = '',
        #          perl = "C:/Strawberry/perl/bin/perl.exe")

    }

    exact.all.df <- data.frame(exact.all)

    if(is.null(file))
        file <- paste0(path, '/exact_all.xlsx');file2 <- paste0(path, '/exact_all.csv')

    write_excel_csv(x = bind_cols(tibble(X1 = rownames(exact.all.df)),
                                  exact.all.df),
                    path = file2,
                    na = '',
                    col_names = FALSE)

    # WriteXLS(x = 'exact.all.df',
    #          ExcelFileName = file,
    #          AdjWidth = TRUE,
    #          row.names = TRUE,
    #          col.names = FALSE,
    #          na = '',
    #          perl = 'C:/Strawberry/perl/bin/perl.exe')

}
