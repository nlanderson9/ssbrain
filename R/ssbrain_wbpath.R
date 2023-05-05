.onAttach <- function(...) {
  if (interactive()) {
    if (is.null(getOption("ssbrain_wbpath"))) {
      packageStartupMessage(start_msg())
    }
  }
  
  invisible(NULL)
}

start_msg <- function() {
  "Please make sure to set the path to your Workbench directory using:
  set_wbpath('/path/to/wb')"
}

set_wbpath = function(value)  {
  options(ssbrain_wbpath = value)
}
