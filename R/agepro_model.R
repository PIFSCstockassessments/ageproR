
#' @title R6 class representing AGEPRO model
#'
#' @description
#' AGEPRO model contains the projection time horizon, age class range, number
#' of fleets, recruitment, and unertainties
#'
#' @details
#' AGEPRO performs stochastic projections on exploited fisheries stock to
#' determine age-structured population over a time period. Brodziak, 2022
#'
#' @template model_num
#'
#' @export
#' @importFrom rprojroot is_rstudio_project is_git_root is_r_package
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number assert_file_exists
agepro_model <- R6Class(
  classname = "agepro_model",
  private = list(

    .ver_legacy_string = NULL,
    .ver_numeric_string = NULL,

    cli_recruit_rule = function() {
      d <- cli_div(theme = list(rule = list(
        color = "cyan",
        "line-type" = "double")))
      cli_rule("Recruitment")
      cli_end(d)
    }

  ),
  public = list(

    #' @field general General Parameters
    general = NULL,

    #' @field recruit AGEPRO Recruitmment Model(s)
    recruit = NULL,

    #' @field case_id Case id
    case_id = NULL,

    #' @field inp_pointer AGEPRO input file pointer
    inp_pointer = NULL,

    #' @field bootstrap Bootstrapping
    bootstrap = NULL,


    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param yr_start First Year of Projection
    #' @param yr_end Last Year of Projection
    #' @param age_begin Age begin
    #' @param age_end Age end
    #' @param num_fleets Number of fleets
    #' @param num_rec_models Number of Recruit Modules
    #' @param num_pop_sims Number of population simuations
    #' @param discards discards. FALSE by default
    #' @param seed Random Number seed. A pesdorandom number is set as default.
    #'
    initialize = function(yr_start,
                           yr_end,
                           age_begin,
                           age_end,
                           num_fleets,
                           num_rec_models,
                           num_pop_sims,
                           discards = FALSE,
                           seed = sample.int(1e8, 1)) {

      ## TODO TODO: Consider a helper function to create a new instance of
      ## AgeproModel

      private$.ver_leagacy_string = "AGEPRO VERSION 4.0"
      private$.ver_numeric_string = "4.0.0.0"

      assert_number(age_begin, lower = 0, upper = 1)
      assert_number(num_fleets, lower = 1)
      assert_number(num_rec_models, lower = 1)
      assert_number(num_pop_sims, lower = 1)

      self$case_id <- case_id$new()

      self$general <- general_params$new(yr_start,
                                        yr_end,
                                        age_begin,
                                        age_end,
                                        num_fleets,
                                        num_rec_models,
                                        num_pop_sims,
                                        discards,
                                        seed)
      private$cli_recruit_rule()
      cli_alert("Creating Default Recruitment Model")
      self$recruit <- recruitment$new(
        rep(0, self$general$num_rec_models), self$general$seq_years)

      self$bootstrap <- bootstrap$new()

    },

    #' @description
    #' Set model's Recruitment model
    set_recruit_model = function(model_num) {

      private$cli_recruit_rule()
      cli_alert("Recruitment Data Setup")
      cli_alert("Using Model Number {.field {model_num}}")

      self$recruit$set_recruit_data(model_num)
      self$recruit$print()


    },

    #' @description
    #' Wrapper function to call bootstrap's set_bootstrap_filename
    #'
    set_bootstrap_filename = function() {

      self$bootstrap$set_bootstrap_filename()
    }

  ), active = list(

    #' @field ver_legacy_string
    #' Version string on AGEPRO input files (*.inp) for version compatibility
    #' with Jon Brodiak's AGEPRO calculation engine.
    ver_legacy_string = function(value){
      if(missing(value)){
        return(private$.ver_legacy_string)
      } else {
        checkmate::assert_character(value,
                                    pattern="AGEPRO VERSION")
        private$.ver_legacy_string <- value
      }
    },

    #' @field ver_numeric_string
    #' Numeric character string based by Semantic-like versioning format.
    ver_numeric_string = function(value){
      if(missing(value)){
        return(private$.ver_numeric_string)
      }else{
        #use as.numeric_version to validate
        cli::cli_alert_info("Version: {as.numeric_version(value)}")
        private$.ver_numeric_string <- value
      }
    }

  )

)


#' AGEPRO Input File Model
#'
#' File Functionality is based on AGEPRO-CoreLib implementation
#'
#' @template inp_line
#' @template inp_con
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_character
#' @importFrom collections dict
#'
agepro_inp_model <- R6Class(
  "agepro_inp_model",
  inherit = agepro_model,
  private = list(

    .pre_v4 = FALSE,
    .supported_inp_versions = "AGEPRO VERSION 4.0",

    .nline = NULL,

    read_case_id = function(con, nline) {
      message("Read Case ID at line ", nline, " ...")
      self$nline <- self$case_id$read_inp_lines(con, nline)
    },

    read_general_params = function(con, nline) {
      self$nline <- self$general$read_inp_lines(con, nline)
    },

    read_recruit = function(con, nline) {
      # Set Recruitment's observation year sequence array using GENERAL's
      # year names from the projection time period
      cli_alert(c("Setting Recruitment data for ",
                "{self$general$yr_start} - {self$general$yr_end} ..."))
      self$recruit$observation_years <- self$general$seq_years
      self$nline <- self$recruit$read_inp_lines(con, nline)
    },

    read_bootstrap = function(con, nline) {
      self$nline <- self$bootstrap$read_inp_lines(con, nline)
    }

  ),
  public = list(

    #' @description
    #' Initializes the input file
    initialize = function() {

      private$.pre_v4 <- FALSE
      private$.nline <- 0

      #TODO: Initialize AGEPRO keyword params

      self$case_id <- case_id$new()
      self$general <- suppressMessages(general_params$new())
      self$recruit <-
        suppressMessages(recruitment$new(0, self$general$seq_years,
                                         cat_verbose = FALSE))
      self$bootstrap <- suppressMessages(bootstrap$new())



    },

    #' @description
    #' Read AGEPRO INP Input Files
    #'
    #' @param inpfile input file name
    read_inp = function(inpfile) {


      if (missing(inpfile)) {

        inpfile <- open_file_dialog(c("AGEPRO input File", ".inp"))
        #Exit Function if user cancels out of file dialog
        if (!test_file_exists(inpfile, access = "r", extension = "inp")) {
          return(invisible(NULL))
        }
      }

      ##Verify that input file location is valid
      assert_file_exists(inpfile, access = "r", extension = "inp")

      tryCatch(
        {
          #(Re)Set File connection to input file
          inp_con <- file(file.path(inpfile), "r")

          self$read_inpfile_values(inp_con)

          #Cleanup and close file connections
          message("Input File Read")
          close(inp_con)

        },
        warning = function(cond) {
          message("Warning. There was an issue reading this file:")
          message(cond)
          close(inp_con)
          invisible()
        },
        error = function(cond) {
          message("There was an error reading this file.")
          message("Error ", cond)
          close(inp_con)
          invisible()
        },
        finally = function(cond) {
          message("Input File Read")
          #close file connections
          close(inp_con)
        }
      )
    },

    #' @description
    #' Read Input file Values
    #'
    #' @export
    #'
    read_inpfile_values = function(inp_con) {

      message("Check Version")

      #check_inputfile_version: assume line 1 is version string
      self$nline <- 1

      message("line ", self$nline, ":")
      self$check_inpfile_version(readLines(inp_con, n = 1, warn = FALSE))

      #loop through inpfile to read in value fore each parameter keyword
      while (TRUE) {
        inp_line <- readLines(inp_con, n = 1, warn = FALSE)
        if (length(inp_line) == 0) {
          break
        }

        self$nline <- self$nline + 1
        self$match_keyword(inp_line, inp_con)

      }

    },

    #' @description
    #' Match Keyword
    #'
    match_keyword = function(inp_line, inp_con) {

      #' TODO: ~~CASEID~~, ~~GENERAL~~, RECRUIT, STOCK_WEIGHT, SSB_WEIGHT,
      #' MEAN_WEIGHT, CATCH_WEIGHT, DISC_WEIGHT, NATMORT, MATURITY,
      #' FISHERY, DISCARD, BIOLOGICAL, ~~BOOTSTRAP~~, HARVEST, REBUILD

      #Tidy evaluation evaluate wrapper functions
      keyword_dict <- dict(list(
        "[CASEID]" = {
            rlang::expr(private$read_case_id(inp_con, self$nline))
          },
        "[GENERAL]" = {
            rlang::expr(private$read_general_params(inp_con, self$nline))
          },
        "[RECRUIT]" = {
            rlang::expr(private$read_recruit(inp_con, self$nline))
          },
        "[BOOTSTRAP]" = {
            rlang::expr(private$read_bootstrap(inp_con, self$nline))
          }
      ))

      message("line ", self$nline, ": ", inp_line)


      if (rlang::eval_tidy(!keyword_dict$has(inp_line))) {
        message(c("Input line ", self$nline,
                  " does not match AGEPRO keyword parameter"))
        invisible() #next
      }else {
        #If there is a match w/ keyword_dict then use the keyword's own
        #readLine function
        data <- list(inp_con = inp_con)
        rlang::eval_tidy(keyword_dict$get(inp_line), data)

      }

    },

    #' @description
    #' Check Input File Version
    #'
    check_inpfile_version = function(inp_line) {
      assert_character(inp_line, len = 1)
      tryCatch(
        {
          message("inp_line:", inp_line)
          inp_line %in% private$.supported_inp_versions
        },
        error = function(cond) {
          message("This version of this input file is not supported : ",
                  inp_line)
          message("Supported verion(s): ", private$.supported_inp_versions)
          message("Error: ", cond)
        }
      )
    },


    #' @description
    #' Throws a Not Implemented exception message. Placeholder function.
    #'
    #' @param keyword keyword
    not_implemented = function(keyword = "") {
      message(keyword, "Not Implemented")
    },

    #' @description
    #' Writes AGEPRO keyword parameter data as a AGEPRO input file (*.inp)
    #'
    write_inp = function(file) {

      if (missing(inpfile)) {

        inpfile <- save_file_dialog(c("AGEPRO input File", ".inp"))
        #Exit Function if user cancels out of file dialog
        if (!test_file_exists(inpfile, access = "r", extension = "inp")) {
          return(invisible(NULL))
        }
      }

      tryCatch(
        {
          list_inplines <- c(

          )
        }

      )

    }


  ),
  active = list(

    #' @field nline nlines
    nline = function(val) {
      if (missing(val)) {
        return(private$.nline)
      }else {
        private$.nline <- val
      }
    }



  )
)

#' AGEPRO JSON model
#'
#' json related
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
#' @importFrom jsonlite toJSON
#' @importFrom utils browseURL
agepro_json_model <- R6Class(
  "agepro_json_model",
  inherit = agepro_model,
  public = list(

    #' @description
    #' Get json
    get_json = function() {

      version_json <- list(
        legacyVer = private$.ver_leagacy_string,
        ver = private$.ver_numeric_string
      )

      #Get VERSION, GENERAL, RECRUIT, and BOOTSTRAP
      agepro_json <- list("version" = version_json,
                          "general" = self$general$json_list_general,
                          "recruit" = self$recruit$json_list_recruit,
                          "bootstrap" = self$bootstrap$json_bootstrap)


      # TODO: use the write() function to write JSON files
      toJSON(agepro_json,
             pretty = TRUE,
             auto_unbox = TRUE)

    },

    #' @description
    #' Write JSON file
    #'
    #' @param show_dir Option to show directory after JSON file is written.
    write_json = function(show_dir = FALSE) {
      tmp <- tempfile("agepro_", fileext = ".json")
      write(self$get_json(), tmp)

      message("Saved at :\n", tmp)
      if (show_dir) {
        browseURL(dirname(tmp))
      }
    }

  )
)
