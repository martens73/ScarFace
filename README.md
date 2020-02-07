# ScarFace
'ScarFace' is a Shiny web application that has been developed to facilitate the usage of the R-package 'seacarb' 
(Lavigne, H., Epitalon, J.-M. and Gattuso, J.-P. (2011): seacarb: seawater carbonate chemistry with R. 
Available from: http://CRAN.R-project.org/package=seacarb).
'seacarb' is used to calculate the carbonate chemistry of seawater requiring a command-line usage. However, for non-R users
'ScarFace' enables the usage of 'seacarb' via an user interface (ui) without the need for digging into R coding.
<br><br>
The web app implements the most frequently functions bjerrum(), carb(), and errors(), which can be operated by numerical or 
slider inputs. In addition, batch processing can be performed by uploading *.csv files, where the columns can have any names
and order. If required, propagated errors can be calculated based on source table or manually entered values.<br>
If you use 'ScarFace' for your published research, please cite it as:
<br><br>
Raitzsch, M. (2020): ScarFace - seacarb calculations with a Shiny user interface. Available from: http://xxx xxx.
<br><br>
but also please acknowledge the original work of Lavigne et al. (2011), see above.
