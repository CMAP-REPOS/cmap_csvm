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

Scenario Specific Adjustments
======================================================================

This section of the README discusses the scenario specific inputs to the model and how to adjust then to test out alternative scenarios.

Each scenario's inputs and outputs are contained in a directory called ```scenarios\[scenario name]```. The set of input files required in each scenario are as follows:

```
cmap_csvm\scenarios\[scenario name]
* Inputs                         Scenario inputs directory
  + data_emp_control_taz.csv     Land use data describing TAZ employment
  + data_hh.csv                  Land use data describing TAZ households
  + htruck_congested_skims       Heavy truck congested travel time and distance skims
  + ltruck_congested_skims       Light truck congested travel time and distance skims
  + mtruck_congested_skims       Medium truck congested travel time and distance skims
  + scenario_adjustments.R       R script containing scenario specific parameter adjustments
* Ouputs                          Scenario outputs directory

```

### Land Use Scenarios

In order to test alternative land use scenarios, the files ```data_emp_control_taz.csv``` and ```data_hh.csv``` can be edited to increase or decrease the amount of employment of the number of households in each TAZ, respectively.

### Transportation Supply Scenarios

The model is sensitive to travel times based on the zone to zone travel times included in the skim files. In order to evaluate the impact of transportion projects or to test out a more general increase or decrease in region wide congestion, the skims file should be updated (e.g., based on network updates and a new run of the travel demand model)

### Parameter Adjustments
