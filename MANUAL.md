# 'ScarFace' Manual

## Prerequisites
ScarFace requires a base R installation (recommended with an installation of RStudio) and installed packages of 'shiny', 'shinydashboard', 'shinyFiles', 'seacarb', 'readr', and 'dplyr'. You may install them all at once with

```{undefined}
install.packages(c("shiny", "shinydashboard", "shinyFiles", "seacarb", "readr", "dplyr"))
```

or separately with

```{undefined}
install.packages("name of the package")
```

## Run the app
Either open 'app.R' in RStudio and push the button "Run App" or call the function
```{undefined}
runApp("~/app.R")
```
Alternatively, the app may be launched by opening a terminal or console window and executed with
```{undefined}
R -e "shiny::runApp('~/app.R')"
```
In both cases, '~/app.R' must be completed with the full path to the location of 'app.R'.

## Bjerrum plot
The Bjerrum plot displays the relative concentrations of carbonate species, with respect to DIC, as a function of pH, when the solution is at equilibrium. The black lines represent the speciation at T=25 °C, S=35, and P=0 bar (at sea surface). By using the sliders or manual inputs, the effects of temperature, salinity and pressure on the dissociation constants *p*K1 and *p*K2 of carbonic acid can be visualized. In the rightmost box the dissociation constants are displayed as numeric values.

## Working Directory
The working directory, i.e. the path to where the output files should be saved, must be defined here by pushing 'Choose' and selecting the destination folder from the dialog window.

## Carbonate chemistry
### manual input
First the input parameters must be entered in the upper left box by choosing the **pair of known carbonate system parameters**, and defining the values of those, which are in µmol/kg (except for pH and pCO2). In addition, define **temperature**, **salinity** and **pressure**. In the upper right box, additional choices can be made, including: <br>

* **Silicate and phosphate concentrations:** leave at 0 when unknown.
* **Constants for K1 and K2:** when 'Auto' is chosen, the Lueker et al. (2000) constants are used, except if T is outside the range 2--35 °C and/or S is outside the range 19--43. In these cases, the constants of Waters et al. (2014) are used.
* **Stability constant of hydrogen fluoride:** when 'Auto' is chosen, the constant of Perez and Fraga (1987) is used, except if T is outside the range 9--33 °C and/or S is outside the range 10--40. In these cases, the constant of Dickson and Riley (1979 in Dickson and Goyet, 1994) is used.
* **Total Boron concentration:** default is Lee et al. (2010).
* **pH scale:** default is 'total scale'.

Once the desired choices were selected, the output values in the following table can be collected in an extra table. This facilitates the comparison of computed values from different input variables. The content of 'Collected output data' can be emptied or saved to the destination path as a csv file.

### batch input
The input data can also be read from an uploaded csv file (here you may choose between comma-, semicolon- or tab-separation). An example is given with 'Batch_example_comma.csv'. In the box 'Carbonate system parameters [INPUT]', define the pair of known carbonate system variables. Then define the input parameters by choosing the corresponding columns of the source table from the dropdown menu. For the pressure, use the radio buttons to select whether pressure or water depth is given in the source data. <br><br>
The usage of 'Additional choices' is equivalent to the 'manual input' (see above). <br><br>
The calculated variables are shown in the table 'Carbonate system parameters [OUTPUT]'. Before continuing, it might be helpful to include additional columns from the source table (e.g. sample name, age, etc.). This can be done by using the checkboxes in the box 'Include additional columns [optional]'. <br><br>
Once all information is included in the output table, it can be saved to the destination path as a csv file, or proceed with 'Error propagation'.

## Error propagation
If propagated errors for the calculated carbonate system parameters should be calculated, define the uncertainties of the input parameters either by using the dropdown menus (if individual errors are given in the source table) or by manually entering general uncertainties. NOTE that manual inputs will override the source table uncertainties. <br><br>
In addition, uncertainties can be given for: <br>

* **Silicate and phosphate uncertainties:** leave at 0 when unknown.
* **Uncertainties in equilibrium constants:** you can either neglect uncertainties of the dissociation constants or choose 'Default'. Then the pre-defined uncertainties from the 'seacarb' package are used (see its documentation for details):
```{undefined}
epK=c(0.002, 0.0075, 0.015, 0.01, 0.01, 0.02, 0.02)
```

* **Total Boron uncertainty:** given as a fractional error (i.e. 0.02 means 2% error). Default is 0.02. <br>

The method of error propagation can be specified in the lower part of the box: <br>

* **Error propagation method:**
    + The **Gaussian** method is the standard technique for estimating a computed variable’s (z) second moment (its variance or standard deviation) based on a first-order approximation to z. More precisely, we use here the basic 1st order, 2nd moment uncertainty analysis (a type of Taylor expansion), assuming no covariance between input variables. This is the approach used by Dickson and Riley (1978). It is the default method.
    + The **Method of Moments** is a more general form of the Gaussian method. But in addition, it also accounts for covariance between input variables. In this case, the ’errors’ routine allows the user to specify a value of the correlation coefficient ’r’, having a value between -1.0 and 1.0, to indicate the correlation between standard uncertainties of the input pair of carbonate system variables. That correlation is used to compute the covariance. But by default, it is assumed that there is no covariance (r=0.0).
    + The **Monte Carlo** method is a brute-force approach relying on repeated random sampling of input errors, adding those to each input variables, calculating the corresponding
output variables for each sample, and finally assessing the standard deviation in each output variables.
* **Correlation coefficient:** correlation coefficient between standard uncertainties of var1 and var2 (only useful with 'Method of Moments', i.e., ignored for the 2 other methods, the default is r=0.0. <br>
* **Monte Carlo repetitions:** Number of random samples, only applied when 'Monte Carlo' method is chosen. The default is 10000. <br>

The calculated propagated errors are displayed in the table 'Standard uncertainties of carbonate system parameters [OUTPUT]'. To combine all data (i.e. the output data from 'batch input' plus calculated errors), this can be achieved by checking the box left of the save button. The entire table can then be saved to the destination path as a csv file.
