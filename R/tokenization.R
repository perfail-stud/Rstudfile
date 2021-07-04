#' Returns vector of tokens, tokenized by word
#'
#' @param input String to tokenize
#' @return vector of words
#' @export
#' @examples
#' word_tokenizer('hello world and etc.')
#' # c("hello", "world", "and", "etc")
word_tokenizer = function(input) {
  stopifnot(is.character(input))

  result = stringi::stri_split_boundaries(input, type = "word", skip_word_none = TRUE)[[1]]

  return (result)
}


#' Returns vector of tokens, tokenized by character
#'
#' @param input String to tokenize
#' @return vector of characters
#' @export
#' @examples
#' char_tokenizer('hello')
#' # c("h", "e", "l", "l", "o")
char_tokenizer = function(input) {
  stopifnot(is.character(input))

  result = stringi::stri_split_boundaries(input, type = "character")[[1]]

  return (result)
}


#' Returns vector of tokens, tokenization by space
#'
#' @param input String to tokenize
#' @param sep Separator to split (default - space)
#' @return vector of words
#' @export
space_tokenizer = function(input, sep = " ") {
  stopifnot(nchar(sep) == 1)
  stopifnot(is.character(input))

  return (stringi::stri_split_fixed(input, pattern = sep))
}
