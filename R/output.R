
#' Output filenames
#'
#' Constructs output file name paths
#'
#' @details
#' Check if current working directory is a R project, in a R package source
#' directory, or has git root subdirectory. Using rprojroot find_root_file
#' criteria, a match will return a path if criteria matches. Otherwise,
#' tryCatch will catch the "not found" error and return FALSE. These values
#' will determine if the current working directory matches these three
#' condtions.
#'
#'
#'
#' @export
#' @importFrom rprojroot find_root from_wd
#' @importFrom rprojroot is_rstudio_project is_git_root is_r_package
#' @importFrom usethis use_build_ignore use_git_ignore
#'
output_fn <- function() {

  has_rproj <- tryCatch({
      find_root(criterion = is_rstudio_project)
    }, error = function(cond) {
      return(FALSE)
    }
  )
  has_git_root <- tryCatch({
    find_root(criterion = is_git_root)
    }, error = function(cond) {
      return(FALSE)
    }
  )
  has_rpkg <- tryCatch({
    find_root(criterion = is_r_package)
    }, error = function(cond) {
      return(FALSE)
    }
  )


  if (any(c(has_rproj, has_git_root, has_rpkg) %in% find_root(from_wd))) {



  }else {
    #TODO: Filename format: date
    format(Sys.time(), "%Y%m%d_%H%M%S")
  }

}

#' Output Subdirectory
#'
#' If the current working directory has an instance or instances of either a
#' R project file, a git root subdirectory, or detected R package source files,
#' then the output will be pointed to the output subdirectory. The output
#' subdirectory will be created. In addition to this, it will add the output
#' subdirectory to `.gitignore` and `.Rbuildignore` if the git root
#' subdirectory and/or R package criteria was
#' found.
#'
#' @param ignore_outdir Append the output subdirectory to .gitignore and
#' .Rbuildignore. In case the target working directory only includes a
#' R project file and doesn't include git and R package files, this option is
#' not needed.
#'
#' @importFrom usethis use_build_ignore use_git_ignore
#' @importFrom rprojroot find_root from_wd
#'
output_subdir <- function(ignore_outdir = TRUE) {
  #create an output subdirectory
  dir.create(file.path(here(), "output"))

  #Add the output subdirectory to .gitignore and .Rbuildingnore

  #

}
