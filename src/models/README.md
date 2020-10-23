# Project2
The Mashable News data set has 60 variables to analyze for trends/predictors for the number of shares for an article.    
There were variables indicating which day of the week the article was published. We created an .md file for each day of the week for our news data set!  
Of course we don't want to repeat the process for each day, we want to automate it!
To do this we set up our parameters and knitted our project with the code below!

```{r setpar}    
#create the weekday variable that we are going to use to render the project!    
data <- readr::read_csv(".../OnlineNewsPopularity.csv")    
data$weekday <- if_else(data$weekday_is_monday ==1 , "Monday",    
                        if_else(data$weekday_is_tuesday == 1, "Tuesday",    
                                if_else(data$weekday_is_wednesday ==1, "Wednesday",    
                                        if_else(data$weekday_is_thursday ==1, "Thursday",    
                                                if_else(data$weekday_is_friday ==1, "Friday",    
                                                        if_else(data$weekday_is_saturday ==1, "Saturday", "Sunday"    
                                                        ))))))    
#set up the parameters    
day <- unique(data$weekday)    
output_file <- paste0(day,"Analysis.md")    
params = lapply(day, FUN = function(x){list(days = x)})    
reports <- tibble(output_file,params)    

#knit our rmarkdown with parameters!!    
library(rmarkdown)    
apply(reports, MARGIN = 1,    
      FUN = function(x){    
        render(input="MondayAnalysis.Rmd", output_file=x[[1]],params=x[[2]])    
      })    
```    


You can check out each weekday with the links below!    
[Monday is available here](MondayAnalysis.md)    
[Tuesday is available here](TuesdayAnalysis.md)    
[Wednesday is available here](WednesdayAnalysis.md)    
[Thursday is available here](ThursdayAnalysis.md)    
[Friday is available here](FridayAnalysis.md)    
[Saturday is available here](SaturdayAnalysis.md)    
[Sunday is available here](SundayAnalysis.md)    
