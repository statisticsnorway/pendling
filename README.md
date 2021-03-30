# pendling
Visualisation of commute statistics. Based on Statistic Norway's StatBank table [03321](https://www.ssb.no/statbank/table/03321/)


#### Structure
Code for the application is found in this repository. It consists of a file called app.R containing the main code for the application and a file called Dotmap_Functions.R including additional functions used in the application. 

Data used in the application is located in the data folder. While the tables could be accessed live through Statistic Norway's API we decided to include formatted data in the app for performance reasons. 

#### Updating
To update the application with new data:

1. Open the R script file called make_data.R, included in the repository. This contains functions that can be used to update the application with new data. 
2. Change the `years_all` object to the new year when data is available. 
3. Check if the names of double municipalities needs to be added in the `change_duplicates()` function. 
4. Run the code in make_data.R. THis will collect create data files using the API and also collect and format the municipality map files from Statistic Norway's internal disk (S). 
5. Open the app.R file. Add the new year to the years_all vector.
6. Test the application using Run App.
7. Publish to shinyapps.io with using statisticsnorway account. This can be done by creating your own user and contacting IT to be added to this account. Choose Publish and Other Destination to add a new destination (Choose Add New Account and follow the instructions). 
WHen the application is published a rsconnect folder is created. This should not be pushed to github.