#' Creates a "Hello world!" ASCII art with an animal
#'
#' @param said_by The animal that says "Hello world!"
#'
#' @return The string generated with the ASCII art
#'
#' @export
#'
#' @examples
#' cat(my_hello_world("cow"))
#' cat(my_hello_world("whale"))
#' cat(my_hello_world("stegosaurus"))
my_hello_world <- function(said_by) {
  cowsay::say("Hello world!", by = said_by, type = "string")
}
