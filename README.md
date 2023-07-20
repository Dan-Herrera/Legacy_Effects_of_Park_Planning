**Legacy Effects of Park Planning**

*abstract upon publication*

*citation information upon publication*

--- 
Code for data cleaning, data formatting, and analysis can be found within the **scripts** folder.
Data used in this analysis can be found within the **data** folder.

*Please note that the code takes several hours, at minimum, to format the raw data used in this analysis. Analysis-ready data can be found in the **data.cleaned** folder.*


The data cleaning process required the use of several custom functions that may be similarly applicable to other projects. These functions can be found within the script titled **functions** within the **scripts** folder.

All custom functions have an accompanying function that describes the purpose of the function, as well as the arguments required and their necessary formats. Process functions always begin with a verb, while accomanying descriptions (which are themselves also functions) always begin with the word "instructions" before the exact name of the function it describes. Once the **functions** script has been sourced, call the instructions as follows:


```
source(./scripts/functions.R) #load in custom functions

instructions_process_greenspace() #call instructions for a particular function

#output shown below
> This function was originally built for to aid in analyzing the change of urban park systems over time. While they may serve other uses, please remember that they were not written with universal functionality in mind.
> Before you begin, make sure you have all of your shapefiles saved as polygons. Files should be named logically and MUST CONTAIN A SAMPLE YEAR! For instance, a polygon depicting DC's parks in 2022 might be named 'DC_2022.'
> This function will only work if all polygons are stored in the same folder. The function will attempt to analyze all shapefiles contained in the folder, unless one of them is identified as the 'city.file,' in which case it will be utilized for building rasters, but will not be itself analyzed.
>
> The following objects are required for functionality:
> city.file: A shapefile (polygon) of the study area of interest - assumed to be a city footprint.
> data.path: A path to the folder which contains the polygons you wish to analyze - assumed to be park system polgyons from the same city over multiple years.
> city.name: The name which you will use to denote your study area. Does not need to match your filenames, but will be used to name your newly created rasters.
> resolution: The desired raster resolution in meters. For example, a resolution of 100 would yield a raster in which each cell was 100x100 m.
```

One additional function exists which describes each of the landscape metrics calculated within the *process_greenspace* script, called *define_landscape_metrics*, which is used similar to to any of the instuctions functions.


I cannot legally publish the raw maps that were used to create the historic park shapefiles, as some are still protected by copyright law. If you need to obtain the original maps, please refer to the Supplimental Material 1 (on journal website) for a list of maps and their digital or physical location. If you use the historic green space data compiled for this project in a future analysis, I recomend using the data as a raster rather than a polygon.

---
*Direct correspondence to herrerawildlife@gmail.com*
