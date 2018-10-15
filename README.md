# RadialPheno - v0.0.1

## Description
RadialPheno is a web application developed using R language and Shiny to support phenology experts in their daily data analysis activities.

It was developed in the context of e-phenology Project and uses radial structures to visualize multivariate cyclical data associated 
with phenology studies. We choose radial visual structures, because it allows the identification of complex temporal patterns 
related to cyclical phenological phenomena. 

This first version can be used for the visualization and interpretations of individual, species and community long-term 
leafing phenolgy data associated with near-surface phenological observations and on-the-ground direct observations. 

If you encounter any bugs, please send a message to cmgreice@gmail.com.

## Usage
This tool was developed and tested only on Windows OS, but we believe that UNIX's users will have no problem to run this application.

After you download the complete tool code and before you run the app in your RStudio environment, you will need to 
install the following libraries (if you have not used them yet):

* shiny
* shinydashboard
* shinyjs
* dplyr
* tidyquant
* RColorBrewer

Once you have installed all libraries, you will need a CSV file with your time series data. You can use the *series_images.csv* 
available at *time series example* folder as an example to explore the use of RadialPheno. This file contains greeness indexes 
obtained from sequences of daily digital vegetation images  taken from a cerrado, a neotropical savanna, 
one of the monitoring sites of the [e-phenology network](http://www.recod.ic.unicamp.br/ephenology).

_IMPORTANT:_ your file must have the same format and the columns *year, month and doy* as in the example.
