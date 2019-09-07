DM-MCDA Implementation in R v 1.0
-------------------------------

DM-MCDA is an R implementation of a web-based platform for data mining and multiple criteria decision analysis: a case study on road accident. The basis functions used by DM-MCDA are Data visualization, association rules analysis and multiple criteria analysis.  The R app provides classes and functions to determine the DM-MCDA basis functions, to perform advanced data analysis using data mining algorithms.

Requirements and installation
-----------------------------

Required software and packages:

- R (3.6)
- Rstudio (Version 1.2.1335)
- rshiny package for graphical interfaces

To install, simply run the file server.R from rstudio.


Examples and Usage
------------------

Minimal data are contained in the source code of the package. For
more detailed examples please have a look at the tutorials.


Demo on shinyapps
------------------
https://aitmlouk.shinyapps.io/osp-armdashboard/


To cite the work use
------------------
```
@article{AITMLOUK2019100323,
title = "DM-MCDA: A web-based platform for data mining and multiple criteria decision analysis: A case study on road accident",
journal = "SoftwareX",
volume = "10",
pages = "100323",
year = "2019",
issn = "2352-7110",
doi = "https://doi.org/10.1016/j.softx.2019.100323",
url = "http://www.sciencedirect.com/science/article/pii/S2352711019301438",
author = "Addi Ait-Mlouk and Tarik Agouti",
keywords = "Data mining, Association rules, Transport, Road accidents, Multiple criteria analysis, Time series, Visualization",
abstract = "Today’s ultra-connected world is generating a huge amount of data stored in databases and cloud environment especially in the era of transportation. These databases need to be processed and analyzed to extract useful information and present it as a valid element for transportation managers for further use, such as road safety, shipping delays, and shipping optimization. The potential of data mining algorithms is largely untapped, this paper shows large-scale techniques such as associations rule analysis, multiple criteria analysis, and time series to improve road safety by identifying hot-spots in advance and giving chance to drivers to avoid the dangers. Indeed, we proposed a framework DM-MCDA based on association rules mining as a preliminary task to extract relationships between variables related to a road accident, and then integrate multiple criteria analysis to help decision-makers to make their choice of the most relevant rules. The developed system is flexible and allows intuitive creation and execution of different algorithms for an extensive range of road traffic topics. DM-MCDA can be expanded with new topics on demand, rendering knowledge extraction more robust and provide meaningful information that could help in developing suitable policies for decision-makers."
}
```
