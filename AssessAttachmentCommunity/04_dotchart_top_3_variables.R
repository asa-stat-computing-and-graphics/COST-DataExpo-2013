# This script calculates the column percentages of the cross tabulation between 
# the communities and the categorical varible for each year and combines it 
# into one dotchart. Statistical test are performed to determine whether there
# are significant changes in the respondents across the three years.

library("ggplot2")
library("plyr")

# The GetCrossTab function calculates the column percentages of the
# crosstabulation between the communites and the categorical variable 
GetCrossTab <- function(vars, data, year){
  # Args:
  # vars: categorical variables used in cross tab
  # data: raw data set containing vars
  # year: which data year?
  
  myTable <- NULL
  myTables <- NULL
  
  for (i in 1:length(vars)) {
    myVar <- data[, vars[i]]
    myTable <- table(myVar, data$qsb)
    myTable <- round((100 * t(myTable) / apply(myTable, 2, sum)), 2)
    
    myTables[[i]] <- cbind(Communities = rep(cities, each = ncol(myTable)), 
                           Percentage = as.numeric(t(myTable)), 
                           Levels = rep(1:ncol(myTable), times = 26), 
                           Year = year)
  }
  
  names(myTables) <- vars
  
  myTables
}

# Convert takes the result from GetCrossTab and converts the data into a data
# frame. The percentages that are outputted as a factor are converted into
# the appropriate data type for further use.
Convert <- function(data){
  # Args:
  # data: data obtain from the GetCrossTab function
  
  data <- as.data.frame(data)
  data[, 2] <- as.numeric(levels(data[, 2]))[data[, 2]]
  
  return(data)
}

# GetDotChart creates the dotchart of the percentage of respondents in each
# community in each year
GetDotChart <- function(data, mytitle, color){
  # Args:
  # data: data containing the percentage of respondents
  # mytitle: main title of dotchart
  # color: color of the different categorical levels
  
  ggplot(data, 
         aes(x = Percentage, y = Communities, colour = Levels, shape = Year)) +
  geom_point(alpha = 0.4, size = 8) +
  labs(title = mytitle, x = "Percentage of Residents\nin Each Community") +
  scale_colour_brewer(palette = color) + 
  scale_x_continuous(limits = c(0, 80)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 18),
        legend.text = element_text(size = 12))
}

data1 <- read.csv("FinalData08.csv")
data2 <- read.csv("FinalData09.csv")
data3 <- read.csv("FinalData10.csv")

data1 <- na.omit(data1)
data2 <- na.omit(data2)
data3 <- na.omit(data3)

cities <- c("Aberdeen, SD",      "Akron, OH",         "Biloxi, MS",       
            "Boulder, CO",       "Bradenton, FL",     "Charlotte, NC",    
            "Columbia, SC",      "Columbus, GA",      "Detroit, MI",      
            "Duluth, MN",        "Fort Wayne, IN",    "Gary, IN",         
            "Grand Forks, ND",   "Lexington, KY",     "Long Beach, CA",   
            "Macon, GA",         "Miami, FL",         "Milledgeville, GA",
            "Myrtle Beach, SC",  "Palm Beach, FL",    "Philadelphia, PA", 
            "San Jose, CA",      "St. Paul, MN",      "State College, PA",
            "Tallahassee, FL",   "Wichita, KS")

data1$qsb <- cities[data1$qsb]

# Important variables determined from the heatmap from the rf_all_communities.R
# script
impVars <- c("q3c", "q5", "q6") #, "q8d", "q7i", "q7m", "q15ab", "q7l")

yr08 <- GetCrossTab(vars = impVars,
                    data = data1,
                    year = 2008)
yr09 <- GetCrossTab(vars = impVars,
                    data = data2,
                    year = 2009)
yr10 <- GetCrossTab(vars = impVars,
                    data = data3,
                    year = 2010)

# Take the results from GetCrossTab and manipulate the data to the right data
# type
yr08 <- lapply(yr08, Convert)
yr09 <- lapply(yr09, Convert)
yr10 <- lapply(yr10, Convert)

for(i in 1:3){
  for(j in 1:nrow(yr08[[i]])){
    # Test the differences of the respondents across the three years
    # using a chi square test
    testData <- c(yr08[[i]]$Percentage[j], yr09[[i]]$Percentage[j], 
                  yr10[[i]]$Percentage[j])
    
    # Save p-values so we can adjust them
    yr08[[i]][j, "PValue"] <- round(chisq.test(testData)$p.value, 4)
  }
  
  # P-values should be the same in the 2009 and 2010 data sets since we 
  # extracted the percentage from 2009 and 2010 to test for differences
  yr09[[i]][, "PValue"] <- yr08[[i]][, "PValue"]
  yr10[[i]][, "PValue"] <- yr08[[i]][, "PValue"]
  
  # Adjust the p-values since we calculated all possible p-values
  yr08[[i]][, "AdjPValue"] <- p.adjust(yr08[[i]][, "PValue"], 
                                       method = "bonferroni")
  yr09[[i]][, "AdjPValue"] <- yr08[[i]][, "AdjPValue"]
  yr10[[i]][, "AdjPValue"] <- yr08[[i]][, "AdjPValue"]
}

# Combine the three dats sets so we can plot the values all in one dotchart
myTables <- list()
for(i in 1:length(yr08)){
  myTables[[i]] <- rbind(yr08[[i]], yr09[[i]], yr10[[i]])
  myTables[[i]][, 1] <- factor(myTables[[i]][, 1], levels = cities[26:1])
}

# Label the categorical levels so we know what the respondent's answer is
# Variable q3c
myTables[[1]]$Levels <- revalue(myTables[[1]]$Levels, 
                                c("1" = "1 - Strongly\n disagree", 
                                  "5" = "5 - Strongly\n agree"))
# Variable q5
labelQ5 = c("\nStay in your\n neighborhood\n", 
            "Move to\n another\n neighborhood\n in your\n community\n", 
            "Move outside\n of your\n community\n but stay in\nthe state", 
            "\nMove to\n another\n city and state\n")

myTables[[2]]$Levels <- mapvalues(myTables[[2]]$Levels, 1:4, labelQ5)

# Variable q6
myTables[[3]]$Levels <- revalue(myTables[[3]]$Levels, 
                                c("1" = "1 - Much \n worse", 
                                  "5" = "5 - Much \n better"))

title1 <- "q3c - Does your community have a good"
title2 <- "reputation\nto outsiders who do not live here?"
png("q3c.png",
    width = 900,
    height = 1300)
print(GetDotChart(data = myTables[[1]],
                  mytitle = paste(title1, title2, sep = " "),
                  color = "RdYlBu"))
dev.off()

title1 <- "q5 - If you had the choice of" 
title2 <- "where to\nlive would you rather ..."
png("q5.png",
    width = 900,
    height = 1300)
print(GetDotChart(data = myTables[[2]],
                  mytitle = paste(title1, title2, sep = " "),
                  color = "PiYG"))
dev.off()

title1 <- "q6 - How would you compare how the community" 
title2 <- "is as a\nplace to live today compared to five years ago"
png("q6.png",
    width = 900,
    height = 1300)
print(GetDotChart(data = myTables[[3]],
                  mytitle = paste(title1, title2, sep = " "),
                  color = "RdYlBu"))
dev.off()
