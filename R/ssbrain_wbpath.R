#' @title Message on Open
#'
#' @description Present the start message when opening
#'
#' @param ... Items passed to attach

.onAttach <- function(...) {
  if (interactive()) {
    if (is.null(getOption("ssbrain_wbpath"))) {
      packageStartupMessage(start_msg())
    }
  }

  invisible(NULL)
}

#' @title Start Message
#'
#' @description The start message to present when opening

start_msg <- function() {
  "Please make sure to set the path to your Workbench directory using:
  set_wbpath('/path/to/wb')"
}

#' @title Set Workbench Directory
#'
#' @description This allows you to set the path to your Workbench executable file.
#'
#' @param value A string with the full filepath to your Workbench executable. It should end with "wb_command"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_wbpath("/path/to/directory/for/workbench/wb_command")
#' }
#'
#' @details
#' This sets a global option for your R environment. It only needs to be run the first time you start R.
#' It is advised that you put this function at the top of every ssbrain script.

set_wbpath = function(value)  {
  options(ssbrain_wbpath = value)
}
