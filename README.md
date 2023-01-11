CMAP Commercial Services Vehicle Model (cmap_csvm)
======================================================================
This repository contains working code for the development of CMAP’s commercial services vehicle model (CSVM). An advanced commerical vehicle touring model using functions from rFreight to simulate commercial vehicle touring in the CMAP modeling region of NE Illinois and adjacent portions of Indiana and Wisconsin.

System Requirements 
======================================================================

Minimum OS: 64-bit Windows 7, 64-bit Windows 8 (8.1) or 64-bit Windows 10 
Processor: Intel CPU Core i5-2500K 3.3GHz / AMD CPU Phenom II X4 940 (or
equivalent) Memory: 32 GB RAM

Recommended OS: 64-bit Windows 7, 64-bit Windows 8 (8.1) or 64-bit Windows 10 
Processor: Intel CPU Core i7 3770 3.4 GHz / AMD CPU AMD FX-8350 4 GHz (or
equivalent) Memory: 64 GB RAM

Software Requirements 
======================================================================

The CSVM runs in ```R 4.1.2```, available from the CRAN R Project website at https://cran.r-project.org/bin/windows/base/R-4.1.2-win.exe.

Installation Instructions 
======================================================================

The current release zip file of the CSVM, ```cmap_csvm-v0.5.0.zip``` is
installed and set up to run using the following steps:

1. Download the ```cmap_csvm-v0.5.0.zip``` file from https://github.com/CMAP-REPOS/cmap_csvm/archive/refs/tags/v0.5.0.zip
2. Extract the contents of ```cmap_csvm-v0.5.0.zip``` into the parent directory that you wish to use for running the CSVM.
3. Copy in three skim OMX files (```htruck_congested_skim.omx```, ```ltruck_congested_skim.omx```, ```mtruck_congested_skim.omx```) to the base scenario's inputs folder (```scenarios/base/inputs```)
4. Install the version of ```R``` that has been tested with this model, which is currently ```R 4.1.2```, from the CRAN R Project website at https://cran.r-project.org/bin/windows/base/R-4.1.2-win.exe.
5. Set the appropriate number of cores for the size of computer that the model is installed on. The file ```lib/scripts/_USER_VARIABLES.R``` constains a setting ```USER_PROCESSOR_CORES```. If this is set to a value more than 1, several of the steps in the model will run in parallel. While this reduces run time, it requires more memory, and, in certain circumstances, the overhead of parallel process data copying and management can reduce or remove the benefit of parallelization. 
- On a machine that just meets the minimum memory requirements for the model, 32GB of RAM: set ```USER_PROCESSOR_CORES``` to 1. 
- On a larger model server, e.g., with 128GB of RAM or more: set ```USER_PROCESSOR_CORES``` to 4. (More than 4 can be used although it has not been found to provide significant reductions in run time). 

Running the Model 
======================================================================
1. The batch file in the root of the CVSM folder, ```run_cmap_csvm.bat``` will run the model when the batch file is double clicked or executed from the command line. The command line arguments that are passed in the batch file identify the name and year of the scenario to be run. The scenario name must match the name of a scenario in the ```scenarios``` directory. A full list of command line arguments that can be passed to the model is described in ```run_cmap_csvm.R```.
2. The base scenario for the CSVM must be run before any alternative scenario can be run as the firm synthesis outputs from the base are required by alternative scenarios. The CSVM model is currently set up with the base scenario folder called "base". The selection of which scenario is the base scenario is a parameter setting, ```BASE_SCENARIO_BASE_NAME``` in ```lib/scripts/_BASE_VARIABLES.R```.

Links to Resources
======================================================================

- Base year visualization dashboard: https://cmap-repos.github.io/cmap_csvm/ReportDashboardBase.html.
- Future year visualization dashboard: https://cmap-repos.github.io/cmap_csvm/ReportDashboardFuture.html.
- User guide: to be added

Creating Scenarios and Making Scenario Specific Adjustments
======================================================================

### Scenario Inputs

This section of the README discusses the scenario specific inputs to the model and how to adjust them to test out alternative scenarios.

Each scenario's inputs and outputs are contained in a directory called ```scenarios\[scenario name]```. The set of input files required in each scenario are as follows:

```
cmap_csvm\scenarios\[scenario name]
* Inputs                         Scenario inputs directory
  + data_emp_control_taz.csv     Land use data describing TAZ employment
  + data_hh.csv                  Land use data describing TAZ households
  + htruck_congested_skims.omx   Heavy truck congested travel time and distance skims
  + ltruck_congested_skims.omx   Light truck congested travel time and distance skims
  + mtruck_congested_skims.omx   Medium truck congested travel time and distance skims
  + scenario_adjustments.R       R script containing scenario specific parameter adjustments
* Ouputs                          Scenario outputs directory

```
### Creating a New Scenario

A new scenario can be added to the model by copying (in File Explorer) a similar scenario that is most like the alternative scenario that is to be tested. For example, if you want to test a future scenario but with an alternative land use forecast, copy the ```scenarios\future``` directory and paste it into the ```scenarios``` directory. Then rename the folder to something like ```future_alt_landuse```. Once the new folder is there the input files in ```scenarios\future_alt_landuse\inputs``` can be adjusted to reflect the specification of the scenario.

### Land Use Scenarios

In order to test alternative land use scenarios, the files ```data_emp_control_taz.csv``` and ```data_hh.csv``` can be edited to increase or decrease the amount of employment of the number of households in each TAZ, respectively.

### Transportation Supply Scenarios

The model is sensitive to travel times based on the zone to zone travel times included in the skim files. In order to evaluate the impact of transportion projects or to test out a more general increase or decrease in region wide congestion, the skims files (```ltruck_congested_skims.omx```, etc.) should be updated (e.g., based on network updates and a new run of the travel demand model)

### Parameter Adjustments

The script file ```scenario_adjustments.R``` allows the model user to assert adjustments to the parameters of the scheduled stop generation model and the vehicle choice model to evaluate higher and lower stop generation by activity and changing patterns in vehicle usage. The script is shown below. As noted in the script comments, positive values for the parameter adjustments will results in an increase in the measure (e.g., number of good stops, use of light vehicles) 
In both cases, the adjustments are to the values of constants in a logit model and as such are not targets. Instead, small changes can be tested experimentally (for example, by making changes to the adjustments in increments of 0.1) to understand the downstream effects of a given changes in the number of stops or percentage change in vehicles choices. 

```
# CMAP CSVM
# Scenario Specific Parameter Adjustments
# This script is read at run time and asserts adjustments to choice models in the CSVM structure

# Stops Generation
# Good deliveries (poitive adjustment = additional goods deliveries)
asc_goods_adj = 0

# Service calls (positive adjustment = additional goods deliveries)
asc_service_adj = 0

# Vehicle Type 
# Mode specific constant adjustments by vehicle type (positive adjustment = trips shift towards that vehicle type)
asc_vehicle_light_adj = 0
asc_vehicle_medium_adj = 0
asc_vehicle_heavy_adj = 0

```

### Running an Alternative Scenario

An alternative scenario is run in the same way as the existing scenarios provided with the model are run, using the batch file in the root of the CVSM folder, ```run_cmap_csvm.bat```. The scenario name in the batch file must match the name of the new scenario created in the ```scenarios``` directory.  
