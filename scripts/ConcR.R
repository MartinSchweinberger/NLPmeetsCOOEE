#' @title Concordancing Loaded Files
#'
#' @description This function creates keyword-in-context (KWIC) displays from a vector of cahracter strings.
#' @param strings A vector of character strings.
#' @param pattern A charater string representing the pattern that the files are searched for which can contain regular expressions.
#' @param context A numeric value which determines how man characters are displayed before and after the search results.
#' @export
#' @keywords Concordancing, Character Srtings, Function.
#' @return NULL
#' @examples \dontrun{
#' #ConcR(corpus, "word", 20)
#' }
ConcR <- function(texts, pattern, context) {
  # activate packages
  require(stringr)
  require(plyr)
  # list files
  conc <- sapply(texts, function(x) {
    # determine length of text
    lngth <- as.vector(unlist(nchar(x)))
    # determine position of hits
    idx <- str_locate_all(x, pattern)
    idx <- idx[[1]]
    ifelse(nrow(idx) >= 1, idx <- idx, return("No hits found"))
    # define start position of hit
    token.start <- idx[,1]
    # define end position of hit
    token.end <- idx[,2]
    # define start position of preceeding context
    pre.start <- ifelse(token.start-context < 1, 1, token.start-context)
    # define end position of preceeding context
    pre.end <- token.start-1
    # define start position of subsequent context
    post.start <- token.end+1
    # define end position of subsequent context
    post.end <- ifelse(token.end+context > lngth, lngth, token.end+context)
    # extract the texts defined by the positions
    PreceedingContext <- substring(x, pre.start, pre.end)
    Token <- substring(x, token.start, token.end)
    SubsequentContext <- substring(x, post.start, post.end)
    conc <- cbind(PreceedingContext, Token, SubsequentContext)
    # return concordance
    return(conc)
    })
  concdf <- ldply(conc, data.frame)
  colnames(concdf)[1]<- "File"
  return(concdf)
}