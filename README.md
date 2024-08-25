# Data Science Curriculum Explorer

This R Shiny application, **Data Science Curriculum Explorer**, allows users to explore, analyze, and interact with curriculum. The app offers features such as course selection by year and subject code, visualization of course dependencies, analysis of curriculum complexity, grade prediction, and personalized course recommendations. The application integrates with an SQLite database for persistent storage and leverages multiple R packages for data manipulation, visualization, and interaction.

## Features

- **Course Selection**: Choose courses based on year and subject code, with the ability to visualize dependencies and structural complexity.
- **Grade Prediction**: Predict your course grades using a random forest model (work in progress).
- **Course Recommendations**: Get personalized course recommendations based on your selections.
  
## Usage

Ensure you have the following R packages installed from CRAN:

- `CurricularAnalytics`
- `visNetwork`
- `shiny`
- `reticulate`
- `randomForest`
- `stringr`
- `dplyr`
- `readr`
- `DBI`
- `RSQLite`

Then simply open `app.R` and run the script!

