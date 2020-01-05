# lasvegas
ShinyApp of Las Vegas Restaurants and Health Inspections - Final for Data Science at Amherst College
Group Project for Jessica Yu and Kenny Chen

Our project involved downloading extremely large datsets from the yelp data set challenge.
Please download the files from this link:
https://www.yelp.com/dataset/challenge

It requires your name, email address, and signatures. 

Our code reads in the business.json and reviews.json file from yelp which we could not upload to GitHub because of the size.
Please run it on your local machine to get these files.

Our project also involves the Restaurant Inspections from Las Vegas Open Data.

https://opendataportal-lasvegas.opendata.arcgis.com/datasets/restaurant-inspections-open-data

It is updated quite frequently and there is also an API. 

INITIAL STEPS IN THIS PROJECT
1. Initial Folder contains feedback and initial proposal.
2. Revised Folder contains secondary steps such as handpicking/matching Restaurants with Inspections and using FuzzyJoin. 
3. Data Folder contains excerpts of Yelp data 

IMPORTANT FOLDER
- Final Folder contains everything needed for project
- Data intake, cleaning, wrangling, matching was all done in the Data Cleaning.Rmd file. 
- We read in the reviews file and businesses file from Yelp on local server (could not upload to github). But inspections data can be read on server or local.
- Saved restaurants in Las Vegas as "VegasRestaurants.Rds" to be referenced later without having to reread the entire business dataset from Yelp.
- Cleaned the names from Business and Inspection data set to make matching more easy.
- Use `fuzzyjoin` with `stringdist` package to match businesses with inspections.
- Saved the matches as "matches.csv" and then manually audited for wrong matches.
- We then uploaded the fixed matches into the folder named "cleanedMatches.csv"
- We then took "cleanedMatches.csv" and joiend them with the reviews by business_id. 
- We then saved this file as "matches_reviews_fixed.Rds" that contains the businesses, their inspections, and their reviews.
- Sentiment Analysis.Rmd is the file where we looked at the sentiment of restaurant reviews.
- Used `bing` sentiment package

- SHINY APP CAN ASO BE FOUND IN THE FINAL FOLDER 
Shiny App Here: https://stat231-groupc.shinyapps.io/Final/
