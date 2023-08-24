#' Open file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @param filtype filename extension
#'
#' @importFrom checkmate assert_character
#'
open_file_dialog <- function(filetype) {

  filetype <- validate_filetype(filetype)
  err_msg_dialog_cancelled <- "File choice cancelled"

  # Check Rstudio is used.
  # Note: vscode uses/emulates rstudioapi but not all features rstudioapi are
  # implemented. vscode's rstudio version information is set to '0'.
  # For Rstudio specific code, set version_needed to '1'
  if (rstudioapi::isAvailable(1)) {
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
#' as not passed in the parameter, it will return the default "{All files} {*}"
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
