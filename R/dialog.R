#' Open file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @param filtype filename extension
#'
open_file_dialog <- function(filetype){

  filetype <- validate_filetype(filetype)

  #Open file dialog
  #Use rstudioapi to show file dialog in front of RStudio.
  #filename <- ifelse(rstudioapi::isAvailable(),
  #                   rstudioapi::selectFile(),
  #                   file.choose(new=TRUE))

  if(rstudioapi::isAvailable()){
    path <- rstudioapi::selectFile(caption = "Open File",
                                   existing = TRUE,
                                   filter = paste0(filetype[1],
                                                   " (*", filetype[2], ")" ))
  }else if(capabilities("tcltk")){
    path <- tcltk::tclvalue(
      tcltk::tkgetOpenFile(initialdir = here::here(),
                           filetypes = paste0( "{{", filetype[1], "} {",
                                                     filetype[2], "}}" )))

    checkmate::assert_character(path, min.chars = 1, .var.name = "path")
  }else{
    #fallback on file.choose
    path <- file.choose()
  }

  return(path.expand(path))
}


#' Save file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @param filetype filename extension
#'
save_file_dialog <- function(filetype){

  filetype <- validate_filetype()

  if(rstudioapi::isAvailable()){
    target <- rstudioapi::selectFile(caption="Save File",
                                     label="Save",
                                     existing=FALSE,
                                     filter=paste0(filetype[1],
                                                   " (*", filetype[2], ")" ))
  }else if(capabilities("tcltk")){
    target <- tcltk::tclvalue(
      tcltk::tkgetSaveFile(initialdir = here::here(),
                           filetypes = paste0( "{{", filetype[1], "} {",
                                                       filetype[2], "}}" )))

    checkmate::assert_character(path, min.chars = 1, .var.name = "target")

  }else{
    #fallback on file.choose
    target <- file.choose()
  }
  return(path.expand(target))
}

#' Checks the validity of filetype key-value pair.
#'
#' Checks the filetype as a 2 length vector without missing values. If filetype
#' as not passed in the parameter, it will return the default "{All files} {*}"
#' string vector.
#'
#' @details
#' The filetype key-value pair is defined as _fileTypeName_ _extension_. This
#' is used to [specifying flie patterns]
#' (https://www.tcl.tk/man/tcl8.0/TkCmd/getOpenFile.html#M11) for Tcl/TK
#' file modules.
#'
#'
#' @param filetype filename extension.
#'
validate_filetype <- function(filetype){

  if(missing(filetype)){
    filetype <- c("All Files", "*")
  }

  #Validate filetype string
  checkmate::assert_vector(filetype, all.missing = FALSE, len = 2,
                           null.ok = FALSE)
  return(filetype)

}
