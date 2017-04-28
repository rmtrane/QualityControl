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


wrap_sentence <- function(string, width) {

  string2 <- gsub(", ", ",#", string)

  bisentences <- as.list(unlist(strsplit(string2, "#", fixed = TRUE)))

  res <- lapply(X = bisentences, FUN = wrap_sentence1, width = width)

  final.sentence <- paste0(unlist(res),
                           collapse = '\n')

  return(final.sentence)

}
