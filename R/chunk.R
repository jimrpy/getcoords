# chunk a vector by given numbers
#' @title Chunk a Vector by given numbers
#' @description All chunks are uniform with a chunk size \code{n} elements, except for the last; the last will at worst be smaller, never bigger than the chunk size \code{n}.
#' @param x a vector
#' @param n the number of a group elements
#'
#' @return A list divided by \code{n}
#' @export
#'
#' @examples
#' chunk(x = 1:20, n = 6)
chunk <- function(x,n) {
  f <- sort(rep(1:(trunc(length(x)/n)+1),n))[1:length(x)]
  return(split(x,f))
}

