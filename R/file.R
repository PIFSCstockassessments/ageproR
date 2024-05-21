#' Open file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @param filetype filename extension
#'
#' @importFrom checkmate assert_character
#'
open_file_dialog <- function(filetype) {

  filetype <- validate_filetype(filetype)
  err_msg_dialog_cancelled <- "File choice cancelled"

  # Check Rstudio is used.
  # Note: vscode uses/emulates rstudioapi but not all features rstudioapi are
  # implemented. vscode's rstudio version information is set to '0'.
  # For Rstudio specific code, check for mode "desktop", and version > '0'
  if (rstudioapi::versionInfo()$mode == "desktop" &&
      rstudioapi::versionInfo()$version > as.character(0) ) {
    path <- rstudioapi::selectFile(caption = "Open File",
                                   existing = TRUE,
                                   filter = paste0(filetype[1],
                                                   " (*", filetype[2], ")"))
    #Check if user cancels file dialog window
    tryCatch(
      {
        assert_character(path, null.ok = FALSE)
        path <- path.expand(path)
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )

  }else if (capabilities("tcltk")) {

    path <- tcltk::tclvalue(
      tcltk::tkgetOpenFile(initialdir = here::here(),
                           filetypes = paste0("{{", filetype[1], "} {",
                                               filetype[2], "}}")))
    #Check if user cancels file dialog window
    tryCatch(
      {
        assert_character(path, min.chars = 1, .var.name = "path")
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )

  }else {
    #fallback on file.choose
    path <- file.choose()
  }

  return(path)
}


#' Save file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @importFrom checkmate assert_character
#'
save_file_dialog <- function() {

  filetype <- validate_filetype() # Defaults to "All Files (*)"
  err_msg_dialog_cancelled <- "File choice cancelled"

  # Check Rstudio is used.
  # Note: vscode uses/emulates rstudioapi but not all features rstudioapi are
  # implemented. vscode's rstudio version information is set to '0'.
  # For Rstudio specific code, set version_needed to '1'
  if (rstudioapi::isAvailable(1)) {
    target <- rstudioapi::selectFile(caption = "Save File",
                                     label = "Save",
                                     existing = FALSE,
                                     filter = paste0(filetype[1],
                                                   " (*", filetype[2], ")"))

    #Check if user cancels file dialog window
    tryCatch(
      {
        checkmate::assert_character(target, null.ok = FALSE)
        target <- path.expand(target)
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }

    )

  }else if (capabilities("tcltk")) {
    target <- tcltk::tclvalue(
      tcltk::tkgetSaveFile(initialdir = here::here(),
                           filetypes = paste0("{{", filetype[1], "} {",
                                                       filetype[2], "}}")))
    #Check if user cancels file dialog window
    tryCatch(
      {
        checkmate::assert_character(target, min.chars = 1, .var.name = "target")
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )



  }else {
    #fallback on file.choose
    target <- file.choose()
  }
  return(target)
}

#' Checks the validity of filetype key-value pair.
#'
#' Checks the filetype as a 2 length vector without missing values. If filetype
#' as not passed in the parameter, it will return the default `All files (*.*)`
#' string vector.
#'
#' @details
#' The filetype key-value pair is defined as _fileTypeName_ _extension_. This
#' is used to [specifying flie
#' patterns](https://www.tcl.tk/man/tcl8.0/TkCmd/getOpenFile.html#M11) for
#' Tcl/TK file modules.
#'
#'
#' @param filetype filename extension.
#'
validate_filetype <- function(filetype) {

  if (missing(filetype)) {
    #Default "All Files (*)" file type
    filetype <- c("All Files", "*")
  }

  #Validate filetype string
  checkmate::assert_vector(filetype, all.missing = FALSE, len = 2,
                           null.ok = FALSE)
  return(filetype)

}

#' Asserts if all substrings of AGEPRO's input data file line can be numeric.
#'
#' Validates the string vector via `grepl` if all values match the digit
#' character class. Function will throw an exception if non digit characters
#' were found.
#'
#' @return Converts the input data line string vector as numeric.
#'
#' @template inp_line
#'
#' @keywords internal
#'
assert_numeric_substrings <- function(inp_line) {

  numeric_regex <- "^[-]?[[:digit:]]"

  if (!all(grepl(numeric_regex, inp_line))) {

    non_numerics <- inp_line[!grepl(numeric_regex, inp_line)]
    stop("Line contains a Non Numeric Substring",
         paste(non_numerics, collapse = ", "))
  }

  invisible(as.numeric(inp_line))

}


#' Reads a line of numeric strings from the AGEPRO input file connection.
#'
#' Reads in a line from the open file connection, splits the string
#' into substrings by whitespace, validates for numerical strings, and
#' then converts to numerical vector.
#'
#' @template inp_con
#'
#' @keywords internal
#'
read_inp_numeric_line <- function(inp_con) {

  if (!isOpen(inp_con)) {
    stop("No open file Connection to AGEPRO input file")
  }

  inp_line <-
    unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

  return(assert_numeric_substrings(inp_line))
}

