library(stringr)


#' Converts special defined lambda expression format to R function
#'
#' @param expr formula to convert
#' @return Function for applying
#' @export
#' @examples
#' lambda_to_func(x ~ {
#'     a <- x + 1
#'     return (a * 2)
#' })(3)
#' # 8
lambda_to_func <- function(expr) {
  this.expr <- as.expression(expr)

  this.expr_str <- as.character(this.expr, collapse='')
  this.expr_str_splitted <- strsplit(this.expr_str, split='~', fixed=TRUE)[[1]]
  this.vars <- c()
  for (this.v in this.expr_str_splitted[1:(length(this.expr_str_splitted) - 1)]) {
    if (this.v != '') {
      this.vars <- c(this.vars, this.v)
    }
  }

  this.vars_string <- ''
  if (length(this.vars) == 0) {
    this.vars_string <- ''
  }
  else if (length(this.vars) == 1) {
    this.vars_string <- as.character(this.vars)
  }
  else {
    for (this.var in this.vars) {
      this.vars_string <- paste(this.vars_string, as.character(this.var), sep=',')
    }
    this.vars_string <- sub('.', '', this.vars_string)
  }

  if (this.vars_string == '') {
    this.identificators <- stringr::str_extract_all(this.expr_str, '[a-zA-Z_.[ \u3d]?]+(<-)?')[[1]]

    this.identificators <- gsub(' ', '', this.identificators, fixed = TRUE)
    this.identificators <- this.identificators[this.identificators != '']
    this.identificators <- unique(this.identificators)

    for (this.identificator in unique(this.identificators)) {
      if (stringr::str_detect(this.identificator, '=') || stringr::str_detect(this.identificator, '<-')) {
        eval(parse(text=paste('.', this.identificator, '0', sep='')))
        next
      }
      if (this.identificator == '')
        next
      if (!exists(this.identificator) && !exists(paste('.', this.identificator, sep=''))) {
        this.vars_string <- paste(this.vars_string, this.identificator, sep=',')
      }
    }
    this.vars_string <- sub('.', '', this.vars_string)
  }

  return (eval(parse(text=paste('(function(', this.vars_string, ') {', this.expr_str_splitted[length(this.expr_str_splitted)], '})'))))
}


#' Returns functor for 'map'-like applying
#'
#' @param init Initial vector
#' @return Functor
#' @export
#' @examples
#' functor(c(1, 2, 3, 4))()
#' # c(1, 2, 3, 4)
#' functor(c(1, 2, 3, 4))(function(x) {x * 2})()
#' # c(2, 4, 6, 8)
functor <- function(init=c(0)) {
  return (function(change=NULL) {
    if (is.null(change)) {
      return (init)
    }
    else if (is.language(change)) {
      change <- lambda_to_func(change)
      return ( functor(change(init)) )
    }
    else {
      return (functor(change(init)))
    }
  })
}


#' map combinator
#'
#' @param x vector to which apply
#' @param f function (or lambda expression) for mapping
#' @return vector with applyed function
#' @export
#' @examples
#' map(c(1, 2, 3, 4), (~ x^2))
#' # c(1, 4, 9, 16)
map <- function(x, f) {
  stopifnot(is.vector(x) || is.list(x))

  if (is.language(f)) {
    return (lambda_to_func(f)(x))
  }

  return (f(x))
}
#' map combinator (infix version)
#'
#' @param x vector to which apply
#' @param f function (or lambda expression) for mapping
#' @return vector with applyed function
#' @export
`%map%` <- map


#' mapN combinator (apply map for each elements of subvectors respectively)
#'
#' @param x list of vectos to which apply
#' @param f function (or lambda expression) for mapping
#' @return vector with applyed function
#' @export
#' @examples
#' mapN(list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)), (x ~ y ~ z ~ x + y + z))
#' # list(c(1, 4, 9), c(16, 25, 36), c(49, 64))
mapN <- function(x, f) {
  stopifnot(is.list(x))

  if (is.language(f)) {
    return (do.call(lambda_to_func(f), x))
  }

  return (do.call(f, x))
}
#' mapN combinator (apply map for each subvector) (infix version)
#'
#' @param x list of vectos to which apply
#' @param f function (or lambda expression) for mapping
#' @return list of vectors with applyed function
#' @export
`%mapN%` <- mapN


#' invoke_map combinator which apply each function to each vector respectively
#'
#' @param x list of vectos to which apply
#' @param f list of functions (or lambda expressions) for mapping
#' @return list of vectors with applyed function
#' @export
#' @examples
#' invoke_map(list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)), list((~ x^2), (~ x^3), (~ x^4)))
invoke_map <- function(x, f) {
  stopifnot(is.list(x))
  stopifnot(is.vector(f))

  if (is.language(f[[1]])) {
    matrix <- sapply(1:length(x), function(i) do.call(lambda_to_func(f[[i]]), x[i]))
    result <- as.list(as.data.frame(matrix))
    names(result) <- NULL
    return (result)
  }

  matrix <- sapply(1:length(x), function(i) do.call(f[[i]], x[i]))
  result <- as.list(as.data.frame(matrix))
  names(result) <- NULL
  return (result)
}
#' invoke_map combinator which apply each function to each vector respectively (infix version)
#'
#' @param x list of vectos to which apply
#' @param f list of functions (or lambda expressions) for mapping
#' @return list of vectors with applyed function
#' @export
`%invoke_map%` <- invoke_map


#' filt combinator which returns filtered vector by predicate
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
#' @examples
#' filt(c(1, 2, 3, 4, 5, 6), (~ x > 3))
filt <- function(x, p) {
  stopifnot(is.vector(x) || is.list(x))

  if (is.language(p)) {
    if (is.vector(x)) {
      return (x[lambda_to_func(p)(x)])
    }
    else {
      return (x[lambda_to_func(p)(x),])
    }
  }

  if (is.vector(x)) {
    return (x[p(x)])
  }
  else {
    return (x[p(x),])
  }
}
#' filt combinator which returns filtered vector by predicate (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
`%filt%` <- filt
#' if combinator which returns filtered vector by predicate as 'filter' (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
`%if%` <- filt


#' filt_not combinator which returns filtered vector by reversed predicate
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
#' @examples
#' filt_not(c(1, 2, 3, 4, 5, 6), (~ x > 3))
filt_not <- function(x, p) {
  stopifnot(is.vector(x) || is.list(x))

  if (is.language(p)) {
    if (is.vector(x)) {
      return (x[!lambda_to_func(p)(x)])
    }
    else {
      return (x[!lambda_to_func(p)(x),])
    }
  }

  if (is.vector(x)) {
    return (x[!p(x)])
  }
  else {
    return (x[!p(x),])
  }
}
#' filt_not combinator which returns filtered vector by reversed predicate (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
`%filt_not%` <- filt_not
#' if_not combinator which returns filtered vector by reversed predicate as 'filt_not' (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return filtered vector
#' @export
`%if_not%` <- filt_not


#' head_while combinator which returns head elements while predicate is TRUE
#'
#' @param x vector to filter
#' @param p predicate
#' @return head filtered
#' @export
#' @examples
#' head_while(c(1, 2, 3, 4, 1, 2), (~ x < 3))
head_while <- function(x, p) {
  stopifnot(is.vector(x) || is.list(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(p)) {
    p <- lambda_to_func(p)
  }

  end <- 0
  for (i in 1:length(x)) {
    if (!p(x[i])) {
      if (end == 0) {
        return (NULL)
      }
      return (x[1:end])
    }
    end <- i
  }

  if (end == 0) {
    return (NULL)
  }
  return (x[1:end])
}
#' head_while combinator which returns head elements while predicate is TRUE (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return head filtered
#' @export
`%head_while%` <- head_while


#' tail_while combinator which returns tail elements while predicate is TRUE
#'
#' @param x vector to filter
#' @param p predicate
#' @return tail filtered
#' @export
#' @examples
#' tail_while(c(1, 2, 3, 4, 1, 2), (~ x < 3))
tail_while <- function(x, p) {
  stopifnot(is.vector(x) || is.list(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(p)) {
    p <- lambda_to_func(p)
  }

  start <- length(x) + 1
  for (i in length(x):1) {
    if (!p(x[i])) {
      if (start > length(x)) {
        return (NULL)
      }
      return (x[start:length(x)])
    }
    start <- i
  }

  if (start > length(x)) {
    return (NULL)
  }
  return (x[start:length(x)])
}
#' tail_while combinator which returns tail elements while predicate is TRUE (infix version)
#'
#' @param x vector to filter
#' @param p predicate
#' @return tail filtered
#' @export
`%tail_while%` <- tail_while


#' any combinator which returns TRUE if any element has TRUE predicate
#'
#' @param x vector to check
#' @param p predicate
#' @return boolean
#' @export
`%any%` <- function(x, p) {
  return (any(x %map% p))
}
#' all combinator which returns TRUE if all elements has TRUE predicate
#'
#' @param x vector to check
#' @param p predicate
#' @return boolean
#' @export
`%all%` <- function(x, p) {
  return (all(x %map% p))
}


#' has operator to check if vector contains element
#'
#' @param x vector to check
#' @param e element to check
#' @return boolean
#' @export
`%has%` <- function(x, e) {
  return (e %in% x)
}


#' flatMap combinator
#'
#' @param x list of vectors to which apply
#' @param f function (or lambda expression) for mapping
#' @return vector with applyed function
#' @export
#' @examples
#' flatMap(list(c(1, 2, 3, 4), c(5, 6), c(7, 8, 9)), (~ x^2))
flatMap <- function(x, f) {
  stopifnot(is.list(x))

  if (length(x) == 0) {
    return (x)
  }

  if (is.language(f)) {
    f <- lambda_to_func(f)
  }

  return (f(unlist(x)))
}
#' flatMap combinator (infix version)
#'
#' @param x list of vectors to which apply
#' @param f function (or lambda expression) for mapping
#' @return vector with applyed function
#' @export
`%flatMap%` <- flatMap


#' identity function which returns element itselves
#'
#' @param x any object
#' @return x parameter
#' @export
identity <- function (x) x


#' flatten converts list of vectors to vector
#'
#' The same as x %flatMap% identity
#'
#' @param x list of vectors
#' @return vector with applyed function
#' @export
#' @examples
#' flatten(list(c(1, 2, 3, 4), c(5, 6), c(7, 8, 9)))
flatten <- function(x) { x %flatMap% identity }
