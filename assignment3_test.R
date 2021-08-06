# Run this script ("Source with Echo") to get the output from your functions
# defined in "assignment3.R" and check it against the examples in the assignment.
# The file "assignment3.R" must be in the same directory.

# Run this after restarting R to avoid contamination by prior assignments.

# You can comment out parts related to unfinished problems.

# This script is just a collection of the same examples in the assignment 
# description. It is not a guarantee of the correctness of your solutions.
# However, if you get errors or wrong results here, your code is almost 
# certainly incorrect.
mergetables <- function (popfile, gdpfile, tfrfile){
  pop_file <- read.csv(popfile)
  gdp_file <- read.csv(gdpfile)
  tfr_file <- read.csv(tfrfile)
  
  aa <- merge(pop_file, gdp_file, by = "name")
  aa <- merge(aa,tfr_file,by="name")
  aa$region.x <- NULL
  aa$region <- NULL
  aa$region.y <- NULL
  colnames(aa)<-c("name","population","RealGDP","TFR")
  
  return(aa)
}

plot_country <- function(df){
  plot((df$RealGDP/df$population)/1000,df$TFR, xlab = "Real GDP per capita ($1000 PPP)", ylab = "Total fertility rate", xlim = c(0,100))
}

readdata <- function(filename){
  wh <- read.csv(filename)
  
  wh$Channel <- factor(wh$Channel, labels = c("Horeca", "Retail"))
  wh$Region <- factor(wh$Region, labels = c("Istanbul", "Ankara", "Other"))
  
  return(wh)
}

annual_revenue <- function(df, channel, region){
  fresh <- 0
  milk <- 0
  grocery <- 0
  frozen <- 0
  detergents <- 0
  delicacies <- 0
  for (i in 1:nrow(df)) {
    if(df[i,1] == channel & df[i,2] == region){
      fresh <- fresh + df[i,3]
      milk <- milk + df[i,4]
      grocery <- grocery + df[i,5]
      frozen <- frozen + df[i,6]
      detergents <- detergents + df[i,7]
      delicacies <- delicacies + df[i,8]
    }
  }
  
  sonuc <- c(fresh,milk,grocery,frozen,detergents,delicacies)
  names(sonuc) <- c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicacies")
  return(sonuc)
}

nclients <- function(df, channel, region){
  number_of_client <- 0
  for (i in 1:nrow(df)) {
    if(df[i,1] == channel & df[i,2] == region){
      number_of_client <- number_of_client + 1
    }
  }
  return(number_of_client)
}

itemtotal <- function(df, item){
  annual_revenue <- function(df, channel, region){
    fresh <- 0
    milk <- 0
    grocery <- 0
    frozen <- 0
    detergents <- 0
    delicacies <- 0
    for (i in 1:nrow(df)) {
      if(df[i,1] == channel & df[i,2] == region){
        fresh <- fresh + df[i,3]
        milk <- milk + df[i,4]
        grocery <- grocery + df[i,5]
        frozen <- frozen + df[i,6]
        detergents <- detergents + df[i,7]
        delicacies <- delicacies + df[i,8]
      }
    }
    
    sonuc <- c(fresh,milk,grocery,frozen,detergents,delicacies)
    names(sonuc) <- c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicacies")
    return(sonuc)
  }
  # your code here
  h1 <- annual_revenue(df,"Horeca","Ankara")[item]
  h2 <- annual_revenue(df,"Horeca","Istanbul")[item]
  h3 <- annual_revenue(df,"Horeca","Other")[item]
  
  r1 <- annual_revenue(df,"Retail","Ankara")[item]
  r2 <- annual_revenue(df,"Retail","Istanbul")[item]
  r3 <- annual_revenue(df,"Retail","Other")[item]
  
  
  city_names <- c("Ankara", "Istanbul", "Other")
  
  item_table <- data.frame(
    Horeca = c(h1,h2,h3),
    Retail = c(r1,r2,r3)
  )
  
  rownames(item_table) <- city_names
  
  return(item_table)
}

read_covid_data <- function(filename){
  xyz <- read.csv(filename)
  abc <- subset(xyz, select = c(date, iso_code, location, total_cases, people_fully_vaccinated))
  return(abc)
}

plot_cases_vacc <- function(df, isocode){
  new_table <- data.frame()
  for(i in 1:nrow(df)){
    if(df[i,2] == isocode){
      new_table <- rbind(new_table, df[i,] )
    }
  }
  new_table$date <- factor(1:nrow(new_table))
  new_table$date <- as.character(new_table$date)
  
  plot(new_table$date, new_table$total_cases, xlab="days", ylab="Number of people",ylim = c(0,max(new_table$people_fully_vaccinated, na.rm = T)) , col = "red")
  points(new_table$date, new_table$people_fully_vaccinated, col = "green")
}


comp_country_plot <- function(df, isocode1, isocode2, column){
  # your code here
  isof <- data.frame()
  for(i in 1:nrow(df)){
    if(df[i,2] == isocode1){
      isof <- rbind(isof, df[i,] )
    }
  }
  isof$date <- factor(1:nrow(isof))
  isof$date <- as.character(isof$date)
  
  isos <- data.frame()
  for(i in 1:nrow(df)){
    if(df[i,2] == isocode2){
      isos <- rbind(isos, df[i,] )
    }
  }
  isos$date <- factor(1:nrow(isos))
  isos$date <- as.character(isos$date)
  if(column == "total_cases"){
    
    plot(isof$date, isof$total_cases,type="p", col = "blue",xlab="days", ylab="Number of people",ylim = c(0,max(isos$total_cases, na.rm = T))  )
    points(isos$date, isos$total_cases, col = "orange")
  }
  
  if(column == "people_fully_vaccinated"){
    plot(isof$date, isof$people_fully_vaccinated,type="p", col = "blue",xlab="days", ylab="Number of people",ylim = c(0,max(isos$people_fully_vaccinated, na.rm = T))  )
    points(isos$date, isos$people_fully_vaccinated, col = "orange")
  }
}




# problem 1
merged <- mergetables("country_pop.csv", "country_realgdp.csv", "country_totalfertilityrate.csv")
head(merged)
tail(merged)
plot_country(merged)

# problem 2
df <- readdata("wholesaledata.csv")
head(df)
summary(df)
annual_revenue(df, "Retail", "Ankara")
nclients(df, "Retail", "Ankara" )
itemtotal(df, "Fresh")

# problem 3
df <- read_covid_data("owid-covid-data.csv")
head(df)
tail(df)
plot_cases_vacc(df, "TUR")
comp_country_plot(df, "DEU","TUR","total_cases")
comp_country_plot(df, "TUR","BRA","people_fully_vaccinated")
