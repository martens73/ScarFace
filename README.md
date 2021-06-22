# ScarFace - seacarb calculations with R Shiny user interface
'ScarFace' is a Shiny web application that has been developed to facilitate the usage of the R-package 'seacarb' (http://CRAN.R-project.org/package=seacarb). 'seacarb' is used to calculate the carbonate chemistry of seawater requiring a command-line usage. For non-friends of bare code, 'ScarFace' enables to use 'seacarb' via an user interface (ui) without the need for digging into R.
<br><br>
The web app implements the most frequently used functions bjerrum(), carb(), and errors(), which can be simply operated by numerical or slider inputs. In addition to single calculations, batch processing can be performed by uploading csv source tables, where there is no need for pre-defined column names or order. If required, propagated errors can be calculated based on source table or manually entered values.
<br><br>
Requirements for running 'ScarFace' and a manual can be found in 'MANUAL.md'.
<br><br>
If you use 'ScarFace' for your published research, please cite:
<br><br>
Raitzsch, M. and Gattuso, J.-P., 2020. ScarFace - seacarb calculations with R Shiny user interface. https://doi.org/10.5281/zenodo.3662139.
<br> as well as <br>
Gattuso J.-P., Epitalon J.-M., Lavigne H. and Orr J., 2019. seacarb: seawater carbonate chemistry. R package version 3.2.12. http://CRAN.R-project.org/package=seacarb.
<br><br>
**Acknowledgement**
<br>
Funded by the Deutsche Forschungsgemeinschaft (DFG – German Research Foundation) through a research project (no. RA 2068/4-1) granted to Markus Raitzsch.
