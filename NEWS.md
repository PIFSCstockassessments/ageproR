# ageproR 0.6.0 2024-03-11

- Added `mortality_fraction_prior_spawn`
  - Data can be imported/exported to AGEPRO Input File/Experimental JSON Input File
  - added `BIOLOGICAL` data to toy Example Agepro Input File
  - changing `time_varying` creates a new `proportion_total_mortality_matrix` with default values and the number of columns depends on the `time_varying` state. The behavior is similar to AGEPRO-GUI  
  - Explicitly state *natural_mortality* and *fishing_mortailty* to `proportion_total_mortality_matrix` to avoid ambiguity with gender abbreviations
- Improved projection_years validation: multiple assertions (from the checkmate package) can be prompted to Rconsole. 
  - Integrated sequence validation error check wrapped as a custom checkmate assertion. 
- Fixed multi recruitment model number support for agepro_model `set_recruit_model` (#49)
  - Included multiple assertions (from checkmate) to check `set_recruit_model` argument is a numeric vector that matches a list of valid AGEPRO recruitment model numbers.
  - Added custom checkmate assertion wrapper to check `set_recruit_model` argument is empty or has multiple arguments.

# ageproR 0.5.1 2024-01-31

* recruitment 
  - Fixed an issue (#39) where a difference of number of recruitment models using the agepro_model function `set_recruit_model` only changed the Recruit Data collection object structure but did not change recruitment probability nor the `num_rec_models` general_param field.
    - Assert that the count of *recruitment model number(s)* (`model_num`) parameter matches `num_recruit_models` parameter at initialization and when importing recruitment data (`read_inp_lines`) from AGEPRO input files. 
      - Added `num_recruit_models` parameter to recruitment initialization
      - Added Sequence years (`seq_years`) parameter to `read_inp_files` to update number of recruit models
    - `agepro_model$set_recruit_model` : This now initializes a new instance of the recruitment class, using the general_params field *number of recruitment models* (`num_rec_models`) value.
    - Use `purrr::map` to validate input for valid recruitment probabilities within the time projection year horizon for each recruitment model in ***Recruitment Probability*** (`recruit_probability`) active binding setter. Recruitment probabilities can be set after Recruitment 
    - Use `purrr::map` to get (and validate) recruitment model numbers from ***Recruitment Model Data*** (`recruit_data`) input and update the ***Recruitment Model Number Vector*** in the `recruit_data` active binding setter.
  - Setter access to general **recruitment** active binding fields: `recruit_scaling_factor`, `ssb_scaling_factor`, `max_recruit_obs`, `recruit_model_num_list`, is set to private. These values can be set using methods of the **recruitment** class, such as during initialization.
  - Improved Recruitment Model Number Validation using the **agepro_model** function `set_recruit_model` 
  - Code cleanup for consistency and clarity
    - Modularize setups for *Recruitment probability*(`recruit_probability`), *Recruitment Model Number vector* (`recruit_model_num_list`), and the *Recruitment Model Data* (`recruit_data`) into private helper functions. 
    - Renamed field `model_collection_list -> recruit_data` for clarity. 
     - Renamed common **recruit_model** active binding `recruit_data -> json_recruit_data`
    - Replaced `observed_years` to `sequence_projection_years` field.
    - Renamed recruitment's `set_recruit_model` function to `initialize_recruit_model` to reflect its intended functionality and set it as a private helper function.
    - Renamed field `max_rec_obs -> max_recruit_obs`
    - Renamed recruitment function `inplines_recruit` (and `inplines_general` from **general_params**) to `get_inp_lines` (#37)
    - Renamed recruitment parameter `cat_verbose -> enable_cat_print`
* Setter access to **general_params** active binding fields: `yr_begin`, `yr_end`, `age_begin`, `age_end`, `num_pop_sims`, `num_fleets`, `num_rec_models`, `discards_present`, `seed` is set to private. These values can be set using methods of the **general_params** class, such as during initialization. This behavior matches AGEPRO-GUI 
* Simplified projection years validation checks to check for numeric 
* Updated roxygen dependency to version 7.3.0
  - Replaced deprecated doctype package in with  **\_PACKAGE\_** roxygen package documentation (`ageproR.R -> ageproR-package.R`)


# ageproR 0.5.0 2024-01-10

* Added `harvest_scenario`
* Added `projection_analyses`
  - Added `standard_projection`, `pstar_projection`, and `rebuild_projection`
  - `set_projection_analyses_type`
    - added agepro_model value ***projection_analyses_type*** to determine the 
    model's projection analyses type. 
    - Most models, use `standard` (default). Agepro models using a 
    **PStar Projection Analyses** or a **Population Rebuilding Projection **
    **Analyses** will use `pstar` and `rebuild` respectfully.
* agepro_model
  - added support for `harvest_scenario`, including the projection analyses 
  classes: `pstar_projection` and `rebuild_projection`
  - allow NULL values for discard keyword parameters (`discard` and 
  `disc_weight`). 
    - This is allow NULLs for discard keyword parameter class validation
  - agepro_json_model
    - added `inport_agepro_inp_model` to import **agepro_inp_model** data to 
    **agepro_json_model**
    - added initialization method to `agepro_json_model` to resolve 
    "Argument is not a function" warning messages 
    - added `case_id` to JSON experimental input file (#30) 
* general_params
  - Improve general_params values validation and error messages 
    - Check that the lowest value of `yr_end` is 1 higher that `yr_begin` 
    and `age_end` is 1 higher than `age_begin`.
    - Check values of `age_begin` and `discards_present` are 0 or 1.
  - Reordered `pop_num_sims` active binding to match function argument order 
- discard_fraction
  - Fixed initialize typo. 
  - This also resolves a issue where discards_present in `agepro_model` is 
  imported and exported to file as a numeric/double but used as a logical for 
  comparable statements.
- recruitment 
  - cli tweaks to `read_inp_lines`
    - removed recruitment `cli_recruit_rule` for utility function 
    `div_keyword_header` 
- added utility function `create_parameter_table` 
- Function documentation changes
 
 
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

