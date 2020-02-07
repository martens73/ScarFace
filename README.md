# ScarFace
'ScarFace' is a Shiny web application that has been developed to facilitate the usage of the R-package 'seacarb' (Lavigne, H., Epitalon, J.-M. and Gattuso, J.-P. (2011): seacarb: seawater carbonate chemistry with R. Available from: http://CRAN.R-project.org/package=seacarb). 'seacarb' is used to calculate the carbonate chemistry of seawater requiring a command-line usage. For non-R command liners, 'ScarFace' enables to use 'seacarb' via an user interface (ui) without the need for digging into R coding.
<br><br>
The web app implements the most frequently used functions bjerrum(), carb(), and errors(), which can be simply operated by numerical or slider inputs. In addition to single calculations, batch processing can be performed by uploading csv source tables, where there is no need for pre-defined column names or order. If required, propagated errors can be calculated based on source table or manually entered values.
<br><br>
Requirements for running 'ScarFace' are a base R installation (best in combination with RStudio), as well as the installed packages shiny, shinydashboard, shinyFiles, seacarb, readr, and dplyr. To run the app, open app.R in RStudio and push the 'Run App' button. The Shiny app will then pop up in your default web browser. You may also use 'Batch_example_comma.csv' for testing the batch processing ability.
<br><br>
If you use 'ScarFace' for your published research, please cite it as:
<br><br>
Raitzsch, M. (2020): ScarFace - seacarb calculations with a Shiny user interface. Available from: https://github.com/martens73/ScarFace.
<br><br>
but also please acknowledge the original work of Lavigne et al. (2011), see above.
