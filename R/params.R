#' Parameters to reuse parameter documentation via inline R code.
#'
#' @noRd

#Example: @param inp_con `r inp_con()`

sep = function() {
  "Character string delimiter seperating values of AGEPRO input file line."
}

inp_con = function() {
  "Open file connection to AGEPRO Input File."
}

inp_line = function() {
  "Line read from the AGEPRO Input File."
}

model_num = function() {
  "AGEPRO Recruitment Model Number"
}

nline = function() {
  "Location of the current line number being read when the file connection to
   the AGEPRO input file is open."
}

num_observations = function() {
  "Number of Empirical Observation Records"
}

num_rec_models = function() {
  "Number of Recruitment Models"
}

alpha = function() {
  "Stock Recruitment Parameter, alpha"
}

beta = function() {
  "Stock Recruitment Parameter, Beta"
}

variance = function() {
  "variance"
}

seq_years = function() {
  "Array representing the Projection Time Horizon."
}

low_recruits = function() {
  "The number of low recruits per spawning stock biomass data points."
}

high_recruits = function() {
  "The number of high recruits per spawning stock biomass data points."
}
