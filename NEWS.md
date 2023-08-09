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

