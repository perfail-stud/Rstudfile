library(stringr)


#' Returns n-gramms for given vector of some elements
#'
#' @param tokens Vector of elements to be n-grammed
#' @param n Size of n-gramm
#' @return List of vector of n-grammed elements
#' @export
#' @examples
#' vec <- c(1, 2, 3, 4, 5)
#' nGramm(vec, 3)
nGramm <- function(tokens, n) {
  stopifnot(is.vector(tokens))
  stopifnot(is.numeric(n))
  n <- n - 1
  stopifnot(length(tokens) > n)

  result <- list()
  index <- 1
  for (i in 1:(length(tokens) - n)) {
    result[[index]] <- tokens[i:(i + n)]
    index <- index + 1
  }

  return (result)
}


#' Returns n-gramms of words for given input string
#'
#' @param input Input string
#' @param n Size of n-gramm
#' @param clearData Boolean - should function clear input data (remove all useless characters)
#' @return List of vector of words
#' @export
#' @examples
#' wordNGramm('Never gonna give you up Never gonna let you down', 3)
wordNGramm <- function(input, n, clearData = TRUE) {
  stopifnot(is.character(input))
  stopifnot(is.numeric(n))
  stopifnot(is.logical(clearData))

  if (clearData) {
    # [^a-zA-Z0-9а-яА-Я ]
    input <- stringr::str_replace(input, '\u005b\u005e\u0061\u002d\u007a\u0041\u002d\u005a\u0030\u002d\u0039\u0430\u002d\u044f\u0410\u002d\u042f \u005d', ' ')
  }
  tokens <- strsplit(input, split=' ', fixed=TRUE)[[1]]

  return (nGramm(tokens, n))
}


#' Returns n-gramms of characters for given input string
#'
#' @param input Input string
#' @param n Size of n-gramm
#' @param strings Boolean - should function return vector of string n-gramms. FALSE - return list of vectors of characters
#' @return Vector of strings or List of vector of characters @seealso strings
#' @export
#' @examples
#' symbolNGramm('hello world', 4)
symbolNGramm <- function(input, n, strings = TRUE) {
  stopifnot(is.character(input))
  stopifnot(is.numeric(n))
  stopifnot(is.logical(strings))

  tokens <- strsplit(input, split='', fixed=TRUE)[[1]]

  ngramms <- nGramm(tokens, n)

  if (strings) {
    result <- c()
    for (i in 1:length(ngramms)) {
      result <- c(result, paste(ngramms[[i]], collapse=''))
    }

    return (result)
  }
  else {
    return (ngramms)
  }
}
