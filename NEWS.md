# ageproR 0.4.1 2023-10-25

* Code refactoring of `general_params`, `case_id`, and `recruitment`
  * general_params
    - Format private members to top of class
    - Actively binded `general_param` fields that was previously only public 
    - Added `keyword_name` and `inp_keyword` active binding
    - Removed `cli_general_rule` for generalized `cli_header_keyword` instead
    - Fix handling of `discard_present` logicals: convert to numeric.
    - Ensure binded fields are numeric during initialization.
    - `read_inp_lines` use `read_inp_numeric_line`
  * case_id
    - Rename `case_id` field to `model_name` to prevent class name-field 
    ambiguity
    - cli tweaks
  * recruitment
    - Added `keyword_name` and `inp_keyword` active binding
    - Removed `cli_recruit_rule` for generalized `cli_header_keyword` instead
    - Actively binded `model_collection_list` and `recruit_model_number_list` 
    to recruitment R6class
* Actively binded `recruit`, `bootstrap`, and `case_id` to `agepro_model` 
R6class
* Renamed `dialog.R` to `file.R`
  - Moved `assert_numeric_substrings` & `read_inp_numeric_line` from `utils.R` 
  to `file.R`
* Renamed `cli_keyword_heading` -> `div_keyword_header`
* Minor code documentation fixes
  

# ageproR 0.4.0 2023-10-04

* Added AGEPRO Keyword parameters for process error for population and 
fishery processes. (#32)
  - `natural_mortality` (`natmort`)
  - `maturity_fraction` (`maturity`)
  - `fishery_selectivity` (`fishery`)
  - `discard_fraction` (`discard`)
  - `jan_stock_weight_age` (`stock_weight`)
  - `spawning_stock_weight_age` (`ssb_weight`)
  - `mean_population_weight_age` (`mean_weight`)
  - `landed_catch_weight_age` (`catch_weight`)
  - `discard_weight_age` (`disc_weight`)
* Implemented AGEPRO input file and experimental JSON input file support for 
process error parameters. (#12),
  - Ensure only "Weight of Age" Process Errors parameters return 
  `input_option` and `time_varying`, if it is a valid weight of age option.
  - Exclude `discards` and `disc_weight` if `agepro_model` doesn't have 
  `discards_present`
  - NOTE: Support for Import process data from auxiliary data file location 
  (`input_option == 1`) is not supported right now.
* Added `[NATMORT]`, `[MATURITY]`, `[FISHERY]`, `[STOCK_WEIGHT]`, 
`[SSB_WEIGHT]`, `[MEAN_WEIGHT]`, and `[CATCH_WEIGHT]` to toy example AGEPRO 
example input data file. Added keyword parameter data is based on AGEPRO GUI's 
example input file _Uku Projection Base_.
* `agepro_model` instantiates Process Error parameters (and `general_params`) as 
active get/set fields
* Added `projection_years`:
  - Handles the ambiguous use of `projection_years`: interpreted as a single 
  integer representing the count of projection years _or_ a vector of sequential 
  values representing a vector of "years".
* `assert_numeric_substrings`: fixed regex numeric string check to include 
negatives.
* `general_params`
  - Renamed `discards` to `discards_present`
  - `seed` defaults to pseudo random number generator
* cli tweaks to print out bootstrap fields and variables (`keyword_name`, 
`inp_keyword`)
* Recruitment
  - Raised default `num_obervations` of **Empirical CDF Recruitment** to `2`
  - Raised default `low_recruits` and `high_recruits` of 
  **Two Stage Empirical Recruitmet w/ SSB** to `2`
* Experimental JSON input file. **Note: This is a developing file format, **
**and it is unsupported with the AGEPRO calculation engine** (#9)
  - If filepath wasn't passed to `write_json`, it will now use the file dialog 
  window. (#11)
  - Added `read_json`.
  - Subset Empirical Recruitment table vector to format as a JSON list in a 
  list object structure
  - Added **Two Stage Empirical Recruitment w/ SSB** JSON list object
  - Fixup formatting of recruit's `type` (recruitment model number) (#29)
* Removed unused `output.R` and `read_input.R`
  - This removes the **rprojroot** and **usethis** dependencies.

# ageproR 0.3.2  2023-08-09

* Fixed `ver_legacy_string` typo that affected the creation of the
`agepro_model` class.

# ageproR 0.3.1  2023-08-03

* Tweaks to R Console cli when loading AGEPRO input file.
  - Improved AGEPRO Input File version validation check.
  - Tweaked Invalid Bootstrap Filename Warning message. 

# ageproR 0.3.0  2023-07-31

Renamed package as ageproR to reflect the package as a supplement to Jon 
Brodziak's original AGEPRO program.

* Export values from AGEPRO Keword parameter exported as a list of character 
strings defined by the _AGEPRO Reference Manual_ input keyword parameter 
data structure (Table 3) and recruitment model structure (Table 4):
 - GENERAL (General Options)
 - CASEID (Case Id)
 - RECRUIT (Recruitment)
 - Recruitment model data:
   - Empirical Recruitment Distribution
   - Empirical Cumulative Distribution Function of Recruitment
   - Two-Stage Empirical Recruits Per Spawning Biomass Distribution
   - Two-Stage Empirical Cumulative Distribution Function of Recruitment
   - Beverton-Holt Curve w/ Lognormal Error
   - Ricker Curve w/ Lognonormal Error
   - Shepherd Curve w/ Lognormal Error
* Implemented method to exported strings (GENERAL, CASEID, RECRUIT & its 
recruit model data, BOOTSTRAP) to AGEPRO input file (*.inp)
* Fixed user cancellation validation check from save file dialog
* Fixed GENERAL parameter order (#22)
* Added LICENSE

# ageproj 0.2.0  2023-06-21

* Added BOOTSTRAP.
  * Added `bootstrap$set_bootstrap_filename` to set bootstrap file from file 
  dialog
* Added functionality to enable a File Dialog Window to load files.
  * Depending on the capabilities of the executed R console, it will use tcltk 
  or rstudioapi library for file dialog windows, or fallback to `file.choose`. 
* cli tweaks

