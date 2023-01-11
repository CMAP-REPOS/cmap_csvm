:: CMAP Commercial Services Vehicle Model Batch File

:: Variable definitions
:: Scenario and Year
set scenarioname="base"
set scenarioyear=2017

:: Reference scenario and year to use for scenario comparison
:: Note: this scenario must have already been run
set reference="base"
set referenceyear=2017

:: Steps of the model to run (TRUE or FALSE, use upper case)
set runfirmsyn="TRUE"
set runcvtm="TRUE"
set runttexp="TRUE"
set rundashboard="TRUE"

:: For reference scenario, if this is the base scenario change to validation
if [%scenarioname%]==["base"] set reference="Validation"
if [%scenarioname%]==["base"] set referenceyear=2017

:: Run CSVM Model For Selected Scenario and Components
Rscript run_cmap_csvm.R %scenarioname% %scenarioyear% %runfirmsyn% %runcvtm% %runttexp% %rundashboard% %reference% %referenceyear% >run_cmap_csvm_log.txt 2>&1

:: Check for errors, exit and return error code if error
if %errorlevel% neq 0 exit /B %errorlevel%

:: Add pause to keep command window open at end of run
pause

