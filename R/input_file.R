

#' AGEPRO Input File
#'
#' File Functionality is based on AGEPRO-CoreLib implementation
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_character
#' @importFrom collections dict
#'
input_file <- R6Class(
  "input_file",
  private = list(

    .TEMP_CASEID = NULL,
    .pre_v4 = FALSE,
    .supported_inp_versions = "AGEPRO VERSION 4.0",

    .nline_ = NULL,


    read_case_id = function(con, nline) {
      message("Read Case ID at line ",nline," ...")
      self$nline_ <- self$inp_case_id$read_inp_lines(con, nline)
    },

    read_general_params = function(con, nline) {
      message("Line ", nline, " ...")
      self$nline_ <- self$inp_general_params$read_inp_lines(con, nline)
    }


  ),
  public = list(

    #' @field model Agepro model
    model = NULL,

    #' @field inp_case_id Case id
    inp_case_id = NULL, #case_id$new(),

    #' @field inp_general_params General Model Parameters
    inp_general_params = NULL,

    #' @description
    #' Initializes the input file
    initialize = function() {

      private$.pre_v4 <- FALSE
      private$.nline_ <- 0

      self$inp_case_id <- case_id$new()
      self$inp_general_params <- suppressMessages(general_params$new())

    },

    #' @description
    #' Reads in AGEPRPO input file
    #'
    #' @param inpfile input file
    #'
    #' @export
    read_inpfile = function(inpfile) {

      #Verify that input file location is valid

      #Console Message

      tryCatch(
        {
          #(Reset) File connection to input file
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
          message("Error ",cond)
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
    #' Check Input File Version
    #'
    #' @param inpline Input File Line
    #'
    check_inpfile_version = function(inpline) {
      #checkmate::assert_character(inpline, length = 1)
      tryCatch(
        {
          message("inpline:", inpline)
          inpline %in% private$.supported_inp_versions
        },
        error = function(cond) {
          message("This version of this input file is not supported : ",
                  inpline)
          message("Supported verion(s): ", private$.supported_inp_versions)
          message("Error: ", cond)
        }
      )
    },

    #' @description
    #' Read Input file Values
    #'
    #' @param inp_con File connection
    #' @export
    #'
    read_inpfile_values = function(inp_con) {

      message("Check Version")

      #check_inputfile_version
      #assume line 1 is version string
      self$nline_ <- 1

      message("line ", self$nline_ ,":")
      self$check_inpfile_version( readLines(inp_con, n = 1, warn = FALSE) )



      #loop through inpfile to read in value fore each parameter keyword
      #while(length(inp_line <- readLines(inp_con, n = 1, warn = FALSE)) > 0 ) {
      while(TRUE) {
        inp_line <- readLines(inp_con, n = 1, warn = FALSE)
        if(length(inp_line) == 0 ) {
          break
        }

        #nline <- nline + 1
        self$nline_ <- self$nline_ + 1
        self$match_keyword(inp_line, inp_con)

      }

    },




    #' @description
    #' Match Keyword
    #'
    #' @param inp_line line
    #' @param inp_con Stdin text connection
    match_keyword = function(inp_line, inp_con ) {



      # AGEPRO keyword parameter dictionary #TODO: Refactor to function
      # keyword_dict <- dict(list(
      #   "[CASEID]" = #
      #     rlang::exprs((self$placeholder_caseid <- inp_con) , (nline = nline+1) ),#{ rlang::enquo(inp_con) %>% (self$placeholder_caseid <- (!!inp_con) ) },#not_implemented2(inp_line),   # issue with warnings and stops initializing this dictonary
      #   "[GENERAL]" = {{ rlang::expr(self$not_implemented("GENERAL ") ) }}, #
      #   "[RECRUIT]" = {{ rlang::expr(self$not_implemented()) }},
      #   "[STOCK_WEIGHT]" = rlang::expr(self$not_implemented()),
      #   "[SSB_WEIGHT]" = rlang::expr(self$not_implemented()),
      #   "[CATCH_WEIGHT]" = rlang::expr(self$not_implemented()),
      #   "[DISC_WEIGHT]" = rlang::expr(self$not_implemented()),
      #   "[MATURITY]" = rlang::expr(self$not_implemented()),
      #   "[FISHERY]" = rlang::expr(self$not_implemented()),
      #   "[DISCARD]" = rlang::expr(self$not_implemented()),
      #   "[BIOLOGICAL]"  = rlang::expr(self$not_implemented()),
      #   "[BOOTSTRAP]" = { rlang::expr(self$not_implemented()) } ,
      #   "[HARVEST]" = rlang::expr(self$not_implemented()),
      #   "[REFPOINT]" = rlang::expr(self$not_implemented()),
      #   "[BOUNDS]" = rlang::expr(self$not_implemented()),
      #   "[RETROADJUST" = rlang::expr(self$not_implemented()),
      #   "[OPTIONS]" = rlang::expr(self$not_implemented()),
      #   "[SCALE]" = rlang::expr(self$not_implemented()),
      #   "[PERC]" = rlang::expr(self$not_implemented()),
      #   "[PSTAR]" = self$not_implemented
      # ))

      keyword_dict <- dict(list(
         "[CASEID]" =
           {rlang::expr(private$read_case_id(inp_con, self$nline_) ) },
         "[GENERAL]" =
           {rlang::expr(private$read_general_params(inp_con, self$nline_))},
         "[BOOTSTRAP]" = {{ rlang::expr(self$not_implemented()) }}
      ))

      message("line ", self$nline_, ": ", inp_line)



      if(rlang::eval_tidy(!keyword_dict$has(inp_line))){
        message("Input line ",self$nline_, " does not match AGEPRO keyword parameter")
        invisible() #next
      }else{
        #If there is a match w/ keyword_dict then use the keyword's own
        #readLine function
        data <- list(inp_con = inp_con)
        rlang::eval_tidy(keyword_dict$get(inp_line), data)


      }

      #Match line to keyword.

      # if(inp_line == "[CASEID]" ){
      #   self$placeholder_caseid <- inp_con
      #   nline <- nline + 1
      # }else if(inp_line =="[GENERAL]"){
      #   self$not_implemented("GENERAL ")
      # }else if(inp_line =="[RECRUIT]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[SSB_WEIGHT]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[CATCH_WEIGHT]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[DISC_WEIGHT]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[MATURITY]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[FISHERY]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[DISCARD]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[BIOLOGICAL]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[BOOTSTRAP]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[HARVEST]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[REFPOINT]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[BOUNDS]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[RETROADJUST]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[OPTIONS]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[SCALE]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[PERC]"){
      #   self$not_implemented("")
      # }else if(inp_line =="[PSTAR]"){
      #   self$not_implemented("")
      # }else{
      #   invisible()
      # }



    },

    #' @description
    #' Throws a Not Implemented exception
    #'
    #' @param keyword keyword
    not_implemented = function(keyword = "") {
      message(keyword, "Not Implemented")
    }






  ), active = list(

    #' @field nline_ nlines
    nline_ = function(val) {
      if(missing(val)){
        return(private$.nline_)
      }else{
        private$.nline_ <- val
      }
    },


    #' @field placeholder_caseid temp case id
    placeholder_caseid = function(val) {
      if(missing(val)){
        return(private$.TEMP_CASEID)
      }else{
        private$.TEMP_CASEID <- readLines(val, n = 1, warn = FALSE)
        self$nline_ <- self$nline_ + 1
        message("placeholders: ", private$.TEMP_CASEID)
      }
    }
  )

)

