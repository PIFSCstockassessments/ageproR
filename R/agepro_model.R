
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
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number assert_file_exists
agepro_model <- R6Class(
  classname = "agepro_model",
  private = list(

    .ver_legacy_string = NULL,
    .ver_numeric_string = NULL,

    # AGEPRO keyword parameters
    .general_options = NULL,
    .natural_mortality = NULL,
    .maturity_fraction = NULL,
    .fishery_selectivity = NULL,
    .discard_fraction = NULL,

    .discards_present = NULL,

    cli_recruit_rule = function() {
      d <- cli_div(theme = list(rule = list(
        color = "cyan",
        "line-type" = "double")))
      cli_rule("Recruitment")
      cli_end(d)
    }

  ),
  public = list(

    #' @field recruit AGEPRO Recruitmment Model(s)
    recruit = NULL,

    #' @field case_id Case id
    case_id = NULL,

    #' @field bootstrap Bootstrapping
    bootstrap = NULL,


    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param yr_start First Year of Projection
    #' @param yr_end Last Year of Projection
    #' @param age_begin Age begin
    #' @param age_end Age end
    #' @param num_pop_sims Number of population simulations
    #' @param num_fleets Number of fleets
    #' @param num_rec_models Number of Recruit Modules
    #' @param discards discards. FALSE by default
    #' @param seed Random Number seed. A pesdorandom number is set as default.
    #'
    initialize = function(yr_start,
                           yr_end,
                           age_begin,
                           age_end,
                           num_pop_sims,
                           num_fleets,
                           num_rec_models,
                           discards = FALSE,
                           seed = sample.int(1e8, 1)) {

      ## TODO TODO: Consider a helper function to create a new instance of
      ## AgeproModel

      private$.ver_legacy_string = "AGEPRO VERSION 4.0"
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
                                        num_pop_sims,
                                        num_fleets,
                                        num_rec_models,
                                        discards,
                                        seed)

      private$.discards_present <- self$general$discards

      private$cli_recruit_rule()
      cli_alert("Creating Default Recruitment Model")
      self$recruit <- recruitment$new(
        rep(0, self$general$num_rec_models), self$general$seq_years)

      self$bootstrap <- bootstrap$new()

      self$natmort <- natural_mortality$new(self$general$seq_years,
                                            self$general$num_ages)

      self$maturity <- maturity_fraction$new(self$general$seq_years,
                                             self$general$num_ages)

      self$fishery <- fishery_selectivity$new(self$general$seq_years,
                                              self$general$num_ages,
                                              self$general$num_fleets)
      if(self$general$discards) {
        self$discard <- discard_fraction$new(self$general$seq_years,
                                             self$general$num_ages,
                                             self$general$num_fleets)
      }

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
    #' @param bsnfile bootstrap filename
    set_bootstrap_filename = function(bsnfile) {

      self$bootstrap$set_bootstrap_filename(bsnfile)
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
    },

    #' @field general
    #' General Options
    general = function(value) {
      if(missing(value)){
        return(private$.general_options)
      }else {
        checkmate::assert_r6(value)
        private$.general_options <- value
      }
    },


    #' @field natmort
    #' Natural Mortality
    natmort = function(value){
      if(missing(value)){
        return(private$.natural_mortality)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.natural_mortality <- value
      }
    },

    #' @field maturity
    #' Maturity Fraction
    maturity = function(value){
      if(missing(value)){
        return(private$.maturity_fraction)
      }else {
        checkmate::assert_r6(value, classes= "process_error")
        private$.maturity_fraction <- value
      }
    },

    #' @field fishery \cr
    #' Fishery Selectivity
    fishery = function(value) {
      if(missing(value)) {
        return(private$.fishery_selectivity)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.fishery_selectivity <- value
      }
    },

    #' @field discard \cr
    #' Discard Fraction
    discard = function(value) {
      if(missing(value)) {
        return(private$.discard_fraction)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.discard_fraction <- value
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
#' @template delimiter
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
    .supported_inp_versions = c("AGEPRO VERSION 4.0", "AGEPRO VERSION 4.2"),

    .nline = NULL,

    read_case_id = function(con, nline) {
      self$nline <- self$case_id$read_inp_lines(con, nline)
    },

    read_general_params = function(con, nline) {
      self$nline <- self$general$read_inp_lines(con, nline)
      # Set .discards_present to Input file's "discards" value
      private$.discards_present <- as.logical(self$general$discards)
    },

    read_recruit = function(con, nline) {
      # Set Recruitment's observation year sequence array using GENERAL's
      # year names from the projection time period
      cli_alert_info(c("Setting Recruitment data for ",
                "{self$general$yr_start} - {self$general$yr_end} ..."))
      self$recruit$observation_years <- self$general$seq_years
      self$nline <- self$recruit$read_inp_lines(con, nline)
    },

    read_bootstrap = function(con, nline) {
      self$nline <- self$bootstrap$read_inp_lines(con, nline)
    },

    read_natural_mortality = function(con, nline) {
      cli::cli_alert_info("Reading Natural Mortaility")
      self$nline <- self$natmort$read_inp_lines(con,
                                                nline,
                                                self$general$seq_years,
                                                self$general$num_ages)
    },

    read_maturity_fraction = function(con, nline) {
      cli::cli_alert_info("Reading Maturity Fraction")
      self$nline <- self$maturity$read_inp_lines(con,
                                                 nline,
                                                 self$general$seq_years,
                                                 self$general$num_ages)
    },

    read_fishery_selectivity = function(con, nline) {
      cli::cli_alert_info("Reading Fishery Selectivity")
      self$nline <-
        self$fishery$read_inp_lines(con,
                                    nline,
                                    self$general$seq_years,
                                    self$general$num_ages,
                                    self$general$num_fleets)
    },

    read_discard_fraction = function(con, nline) {

      if(!self$general$discards){
        stop(paste0("Reading Discard Fraction data but ",
                    "'Discards are present' option is FALSE"))
      }
      self$nline <-
        self$discards$read_inp_lines(con,
                                     nline,
                                     self$general$seq_years,
                                     self$general$num_ages,
                                     self$general$num_fleets)

    }

  ),
  public = list(

    #' @description
    #' Initializes the input file
    #'
    initialize = function() {

      private$.pre_v4 <- FALSE
      private$.nline <- 0

      #TODO: Initialize AGEPRO keyword params

      cli::cli_alert("Setting up defualt AGEPRO model w/ default values")

      self$case_id <- case_id$new()

      self$general <- suppressMessages(general_params$new())
      private$.discards_present <- self$general$discards

      self$recruit <-
        suppressMessages(recruitment$new(0, self$general$seq_years,
                                         cat_verbose = FALSE))
      self$bootstrap <- suppressMessages(bootstrap$new())

      self$natmort <-
        suppressMessages(natural_mortality$new(self$general$seq_years,
                                            self$general$num_ages,
                                            enable_cat_print = FALSE))
      self$maturity <-
        suppressMessages(maturity_fraction$new(self$general$seq_years,
                                               self$general$num_ages,
                                               enable_cat_print = FALSE))

      self$fishery <-
        suppressMessages(fishery_selectivity$new(self$general$seq_years,
                                              self$general$num_ages,
                                              self$general$num_fleets,
                                              enable_cat_print = FALSE))

      if(self$general$discard){
        self$discard <-
          suppressMessages(discard_fraction$new(self$general$seq_years,
                                                self$general$num_ages,
                                                self$general$num_fleets,
                                                enable_cat_print = FALSE))
      }

      cli::cli_text("Done")
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
          cli::cli_alert_info("Input File Read")
        #},
        #warning = function(cond) {
          #warning(cond)
          #invisible()
        },
        error = function(cond) {
          message("There was an error reading this file.")
          stop(cond)
          invisible()
        },
        finally = {
          cli::cli_alert_info("Closing connection to file.")
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

      #assert_inpfile_version: assume line 1 is version string
      self$nline <- 1

      div_line1_alert = function() {
        cli::cli_div(class = "tmp",
                     theme = list(.tmp = list(color="darkorange",
                                              "font-weight" = "bold")))
        cli::cli_alert("line {self$nline}:", class = "tmp")
      }
      div_line1_alert()

      self$assert_inpfile_version(readLines(inp_con, n = 1, warn = FALSE))

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
          },
        "[NATMORT]" = {
            rlang::expr(private$read_natural_mortality(inp_con, self$nline))
         },
        "[MATURITY]" = {
            rlang::expr(private$read_maturity_fraction(inp_con, self$nline))
         },
        "[FISHERY]" = {
            rlang::expr(private$read_fishery_selectivity(inp_con, self$nline))
        },
        "[DISCARD]" = {
            rlang::expr(private$read_discard_fraction(inp_con, self$nline))
        }
      ))

      div_keyword_line_alert <- function() {
        cli::cli_div(class = "tmp",
                     theme = list(.tmp = list(color="darkorange",
                                              "font-weight" = "bold")))
        cli::cli_alert("line {self$nline}: {inp_line}", class = "tmp")
      }
      div_keyword_line_alert()


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
    assert_inpfile_version = function(inp_line) {
      assert_character(inp_line, len = 1)

        cli::cli_alert_info("Version: '{inp_line}'")
        if(inp_line %in% private$.supported_inp_versions){
          self$ver_legacy_string <- inp_line
        }else{
          # Throw Unsupported Version Error Message
          stop(paste0(
            "This version of this input file is not supported: ",inp_line,
            "\n - Supported verion(s): ",
            paste(private$.supported_inp_versions,collapse=", ")),
            call.= FALSE)
        }

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
    #' @param inpfile input file path
    #'
    write_inp = function(inpfile, delimiter = "  ") {

      if (missing(inpfile)) {

        inpfile <- save_file_dialog()
        # Exit Function if user cancels out of file dialog
        # User cancelled dialogs return NULL values
        if (is.null(inpfile)) {
          return(invisible(NULL))
        }
      }

      tryCatch(
        {
          list_inplines <- c(
            self$ver_legacy_string,
            self$case_id$inplines_case_id(),
            self$general$inplines_general(delimiter),
            self$recruit$inplines_recruit(delimiter),
            self$bootstrap$inplines_bootstrap(delimiter),
            self$natmort$inplines_process_error(delimiter),
            self$maturity$inplines_process_error(delimiter),
            self$fishery$inplines_process_error(delimiter),
            if(self$general$discard){
              self$discard$inplines_process_error(delimiter)
            }
          )

        }

      )

      #Write list_inplines to inpfile
      sink(inpfile)
      cat(unlist(list_inplines), sep = "\n")
      sink()
      cli::cli_alert_info("Saved to {.file {inpfile}}")

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

#' @title
#' AGEPRO model w/ JSON input file bindings
#'
#' @description
#' File Functionality on experimental JSON input file
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
    #'
    #' @details
    #' See [jsonlite::toJSON] for more details. `NULL` values is written as
    #' `{}`. `NA` values is written as `null`
    #'
    get_json = function() {

      version_json <- list(

        legacyVer = self$ver_legacy_string,
        ver = self$ver_numeric_string
      )

      #Get VERSION, GENERAL, RECRUIT, and BOOTSTRAP
      agepro_json <- as.list(c("version" = version_json,
                          "general" = self$general$json_list_general,
                          "recruit" = self$recruit$json_list_recruit,
                          "bootstrap" = self$bootstrap$json_bootstrap,
                          "natmort" = self$natmort$json_list_process_error,
                          "maturity" = self$maturity$json_list_process_error,
                          "fishery" = self$fishery$json_list_process_error,
                          if(self$general$discards){
                            "discards" = self$discards$json_list_process_error
                          }
                          ))


      # TODO: use the write() function to write JSON files

      toJSON(agepro_json,
             pretty = TRUE,
             auto_unbox = TRUE)

    },

    #' @description
    #' Write JSON file
    #'
    #' @param file input file path
    #' @param show_dir Option to show directory after JSON file is written.
    #'
    write_json = function(file, show_dir = FALSE) {

      if (missing(file)) {

        file <- save_file_dialog()
        # Exit Function if user cancels out of file dialog
        # User cancelled dialogs return NULL values
        if (is.null(file)) {
          return(invisible(NULL))
        }
      }


      write(self$get_json(), file)

      message("Saved at :\n", file)
      if (show_dir) {
        browseURL(dirname(file))
      }
    },

    #' @description
    #' Reads AGEPRO json experimental input file format.
    #'
    #' @param file input file path
    #'
    read_json = function(file) {
      warning("AGEPRO JSON input is in development, and format may change.")
      return(jsonlite::read_json(file, simplifyVector = TRUE))
    }

  )
)
