log_time <- function(expr) {
  # Capture the expression as a language object
  call_obj <- substitute(expr)

  # Attempt to extract a name for the function or operation
  if (is.call(call_obj)) {
    # If it's a function call, extract the name
    name <- deparse(call_obj[[1]])
  } else {
    # If it's just a variable or symbol, use the name itself
    name <- deparse(call_obj)
  }

  # Abbreviate the name
  short_name <- abbreviate(name, minlength = 32)

  # Timing
  start_time <- Sys.time()
  msg1 <- sprintf("enter %s: %s", short_name, start_time)
  # cli::cli_inform()
  ul <- cli::cli_ul()
  cli::cli_li(msg1)
  # cli_ol(letters[1:3])
  # cli_li("two:")
  # cli_li("three")
  # message(sprintf("entering %s: %s", short_name, start_time))

  # Evaluate the expression
  result <- eval.parent(substitute(expr))

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "secs")
  msg2 <- sprintf("exitt  %s: %s, elapsed: %.4f seconds", short_name, end_time, elapsed)
  cli::cli_li(msg2)
  cli::cli_end(ul)
  # cli::cli_inform(sprintf("exitt  %s: %s, elapsed: %.4f seconds",
  #                         short_name, end_time, elapsed))
  # message(sprintf("exiting %s: %s, elapsed: %.4f seconds",
  #                 short_name, end_time, elapsed))

  return(result)
}

# basic examples
# goo1 <- function() {
#   Sys.sleep(2)
# }
# foo1 <- function() {
#   Sys.sleep(1)
#   log_time(goo1())
#   Sys.sleep(1)
# }
# hoo1 <- function() {
#   Sys.sleep(1)
#   log_time(foo1())
# }
# log_time(hoo1())
#
