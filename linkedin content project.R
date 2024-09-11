# load readxl package
library(readxl)

# Specify the file path
content <- "C:/Users/HP/Downloads/Content_2024-05-04_2024-08-01_PeaceOluwatayo.xlsx"


# Get all sheet names
sheet_names <- excel_sheets(content)

# Read all sheets into a list of data frames and remove NA values
data_list <- lapply(sheet_names, function(sheet) {
  data <- read_excel(content, sheet = sheet)
  na.omit(data)  # Remove rows with NA values
})

# Assign names to list elements for easier access
names(data_list) <- sheet_names

# Extract specific sheets
top_posts <- data_list[["TOP POSTS"]]
demographics <- data_list[["DEMOGRAPHICS"]]
followers <- data_list[["FOLLOWERS"]]
engagement <- data_list[["ENGAGEMENT"]]


# View specific sheets in RStudio
View(top_posts)
View(demographics)
View(followers)
View(engagement)

# check the data types of the columns in demographics sheet
str(demographics)

# convert the percentage column to numeric data type
demographics$Percentage <- as.numeric(demographics$Percentage)
demographics$Percentage

# remove NA values in demographics data
demographics_complete<- na.omit(demographics)
demographics_complete

# view complete demographics data
View(demographics_complete)

# delete unnecessary rows in followers data
followers_updated <- followers[-1,]
followers_updated

# view complete followers updated data
View(followers_updated)

# load data.table package
library(data.table)

# rename column names in followers_updated data
setnames(followers_updated, old = c("Total followers on 8/1/2024:", "2713.0"), new = c("Date", "New Followers"))
print(followers_updated)

# check the data types of the columns in followers_updated sheet
str(followers_updated)

# convert the date column of the followers_updated sheet to date data type
followers_updated$Date <- strptime(followers_updated$Date, format = "%m/%d/%Y")
followers_updated$Date

# convert the new followers column to numeric data type
followers_updated$`New Followers` <- as.numeric(followers_updated$`New Followers`)
followers_updated$`New Followers`

# view followers_updated data 
View(followers_updated)

# check the data types of the columns in engagement sheet
str(engagement)

# convert the date column of the engagement sheet to date data type
engagement$Date <- strptime(engagement$Date, format = "%m/%d/%Y")
engagement$Date

# view complete engagement data 
View(engagement)

# Load dplyr package
library(dplyr)

# left join engagement data with followers_updated data
followers_engagement <- left_join(engagement, followers_updated, by = "Date")
followers_engagement

# view complete followers_engagement data
View(followers_engagement)

# check data types of the columns in followers_engagement data
str(followers_engagement)

# create month column in followers_engagement data
followers_engagement$Month <- format(followers_engagement$Date, "%b")
followers_engagement$Month

# Ensure the month column is an ordered factor with abbreviated month names
followers_engagement$Month <- factor(followers_engagement$Month, 
                                     levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                                     ordered = TRUE)

# load esquisse package
library(esquisse)


# explore and visualize followers_engagement data
esquisse :: esquisser(followers_engagement)


# load ggplot2 package
library(ggplot2)

# create a line graph to visualize impressions over time
impressions_overtime <- ggplot(followers_engagement) +
  aes(x = Date, y = Impressions) +
  geom_line(colour = "#440154") +
  labs(title = "Content Impressions over Time") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 12L, 
                                                                                                       face = "plain"), axis.title.x = element_text(size = 12L, face = "plain"), axis.text.y = element_text(face = "plain", 
                                                                                                                                                                                                            size = 10L), axis.text.x = element_text(face = "plain", size = 10L), legend.text = element_text(size = 15L), 
        legend.title = element_text(size = 15L))
impressions_overtime

# load plotly package
library(plotly)

# create an interactive plot for impressions over time
ggplotly(impressions_overtime)


# create a bar plot to visualize impressions per month
impressions_per_month <- ggplot(followers_engagement) +
  aes(x = Month, y = Impressions) +
  geom_col(fill = "#440154") +
  labs(title = "Content Impressions per Month") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 12L), 
        axis.title.x = element_text(size = 12L))
impressions_per_month

# create an interactive plot for impressions per month
ggplotly(impressions_per_month)

# create a bar plot to visualize engagements per month
engagements_per_month <- ggplot(followers_engagement) +
  aes(x = Month, y = Engagements) +
  geom_col(fill = "#440154") +
  labs(title = "Engagements per Month") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 12L), 
        axis.title.x = element_text(size = 12L))
engagements_per_month

# create an interactive plot for engagements per month
ggplotly(engagements_per_month)

# create a bar plot to visualize new followers per month
new_followers_per_month <- ggplot(followers_engagement) +
  aes(x = Month, y = `New Followers`) +
  geom_col(fill = "#440154") +
  labs(title = "New Followers per Month") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 12L), 
        axis.title.x = element_text(size = 12L))
new_followers_per_month

# create an interactive plot for new followers per month
ggplotly(new_followers_per_month)

# import number_of_posts data set
number_of_posts <- read_excel("C:/Users/HP/Downloads/number of posts may 4 to aug 1.xlsx")

# view number_of_posts data set
View(number_of_posts)

# check the data types of the columns in number_of_posts sheet
str(number_of_posts)

# Ensure the month column is an ordered factor with abbreviated month names
number_of_posts$Month <- factor(number_of_posts$Month, 
                                levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                                ordered = TRUE)

# explore and visualize number_of_posts data
esquisse :: esquisser(number_of_posts)

# create a bar plot to visualize number of posts per month
no_of_posts_per_month <- ggplot(number_of_posts) +
  aes(x = Month, y = `No of posts`) +
  geom_col(fill = "#440154") +
  labs(title = "Number of Posts per Month") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 12L), 
        axis.title.x = element_text(size = 12L))
no_of_posts_per_month

# create an interactive plot for number of posts per month
ggplotly(no_of_posts_per_month)

# explore and visualize demographics_complete data
esquisse :: esquisser(demographics_complete)

# load dplyr package
library(dplyr)

# create a bar plot to visualize top job titles
top_job_titles <- demographics_complete %>%
 filter(`Top Demographics` %in% "Job titles") %>%
 ggplot() +
 aes(x = `Top Demographics`, y = Percentage, fill = Value) +
 geom_col(position = "dodge2") +
 scale_fill_brewer(palette = "Purples", direction = 1) +
 labs(title = "Top Job Titles", fill = "Keys") +
 theme_classic() +
 theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 10L), 
 axis.title.x = element_text(size = 10L))
top_job_titles

# create an interactive plot for top job titles
ggplotly(top_job_titles)

# create a bar plot to visualize top locations
top_locations <- demographics_complete %>%
  filter(`Top Demographics` %in% "Locations") %>%
  ggplot() +
  aes(x = `Top Demographics`, y = Percentage, fill = Value) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Purples", direction = 1) +
  labs(title = "Top Locations", fill = "Keys") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 10L), 
        axis.title.x = element_text(size = 10L))
top_locations

# create an interactive plot for top locations
ggplotly(top_locations)

# create a bar plot to visualize top industries
top_industries <- demographics_complete %>%
  filter(`Top Demographics` %in% "Industries") %>%
  ggplot() +
  aes(x = `Top Demographics`, y = Percentage, fill = Value) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Purples", direction = 1) +
  labs(title = "Top Industries", fill = "Keys") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 10L), 
        axis.title.x = element_text(size = 10L))
top_industries

# create an interactive plot for top industries
ggplotly(top_industries)

# create a bar plot to visualize seniority level
seniority_level <- demographics_complete %>%
  filter(`Top Demographics` %in% "Seniority") %>%
  ggplot() +
  aes(x = `Top Demographics`, y = Percentage, fill = Value) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Purples", direction = 1) +
  labs(title = "Seniority Level", fill = "Keys") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 10L), 
        axis.title.x = element_text(size = 10L))
seniority_level

# create an interactive plot for seniority level
ggplotly(seniority_level)

# create a bar plot to visualize company size
company_size <- demographics_complete %>%
  filter(`Top Demographics` %in% "Company size") %>%
  ggplot() +
  aes(x = `Top Demographics`, y = Percentage, fill = Value) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Purples", direction = 1) +
  labs(title = "Company Size", fill = "Keys") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 10L), 
        axis.title.x = element_text(size = 10L))
company_size

# create an interactive plot for company size
ggplotly(company_size)
