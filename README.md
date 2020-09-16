This was a summer group case study completed in the University of Missouri - Master of Data Science and Analytics program.

Please open the "Master" jupyter notebook to review.  All other notebooks are linked to from the Master notebook.

If you would like to find my individual contributions you can ctrl+F and search for "Bill" in the Master notebook (and linked notebooks and scripts).

A full description of the project and conclusions drawn can be found in the Final Data Story powerpoint presentation.

For convenience, I will post the conclusion here:

Over the course of this project many conclusions were made about the data set, its variables, and the results of our analysis. 
The  PCA revealed that location variables and variables related to the event such as depth and magnitude were the best at capturing the data set. 
The b-value and time lag analysis revealed that seismic activity does increase as the distance and time to a large event decrease.  
However, the the accuracy of the analysis decreased significantly when looking at data just 100 days prior to the main event. 
The analyses revealed that linear regression was not an effective method of predicting earthquake magnitudes. 
There was far too much variability in the data to draw any meaningful linear relationship.  
Nearest neighbors demonstrated that earthquakes, particularly in regions with many events, are highly linked and related to each other. 
Time series analysis was able to predict the average magnitude of upcoming earthquakes, but any deviations from the average were lost.  
Poisson regression did not yield any more meaningful results than linear regression or time series analysis.  
Like the time series analysis, it mainly predicted earthquakes around the mean.  
If further analysis was to be done, narrowing the field of study and adding more predictive variables would be necessary for a more accurate model. 
Even the use of the test case may not be specific enough to truly create an accurate model. 
