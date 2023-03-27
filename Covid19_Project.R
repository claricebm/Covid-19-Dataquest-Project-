#loading packages
library(tibble)
library(dplyr)
library(readr)

#COVID-19 Dataquest Project

#This dataset was collected between the 20th of January and the 1st of June 2020. The purpose of this Guided Project is to build our skills and understanding of the data analysis workflow by evaluating the COVID-19 situation through this dataset. We will answer the following question: Which countries have had the highest number of positive cases against the number of tests?

covid_df<-read_csv("covid19.csv") #loading covid19 data

dim(covid_df) #get how much data we have (number of lines and columns)

vector_cols<-colnames(covid_df) #get column names
vector_cols #displaying the variable
class(vector_cols) #character class

#view and summarize content
head(covid_df)
glimpse(covid_df) #this function shows each class type of every column in your dataframe

#We will extract only the country-level data in order to not bias our analyses. To do so, we filter the data to keep only the data related to "All States". "All States" represents the value of the column Province_State to specify that the COVID-19 data is only available at the country level.

#classical way
valid_rows<-which(covid_df$Province_State=="All States")
covid_df_all_states<-covid_df[valid_rows,]
unique(covid_df_all_states$Province_State) #checking whether only "All States" were selected

#removes "Province_State" column
covid_df_all_states<-covid_df_all_states[,-5] #or
covid_df_all_states<-select(covid_df_all_states,-Province_State)

#It can be removed because now we have only the "All States" category selected for that column

#using pipelines
covid_df_all_states<-covid_df%>%
  filter(Province_State=="All States")%>%
  select(-Province_State)

#Revisiting the description of the dataset columns, we can notice that there are columns that provide daily information and others that provide cumulative information. We will work mainly with daily data, to avoid any biased analysis. So let's extract the columns related to the daily measures.

#classical way
covid_df_all_states_daily<-select(covid_df_all_states, Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

#using pipelines
covid_df_all_states_daily<-covid_df_all_states%>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

#Now we want to explore the top ten cases countries. We will summarize the covid_df_all_states_daily dataframe by computing the overall number of tested, positive, active and hospitalized cases. Then, we can arrange this aggregated data by the overall number of tested cases. Finally, we can extract the first ten rows as the top ten tested cases countries.

#summarizing information by country
covid_df_all_states_daily_sum<-covid_df_all_states_daily%>%
  group_by(Country_Region)%>%
  summarise(
    tested=sum(daily_tested),
    positive=sum(daily_positive),
    active=sum(active),
    hospitalized=sum(hospitalizedCurr)
  )%>%
  arrange(-tested)

covid_df_all_states_daily_sum #plot the first 10 results of the new dataframe

#Now we want to answer the following question:  Which countries have had the highest number of positive cases against the number of tests?

#creating the vector of the top 10 covid_df_all_states_daily_sum dataframe
covid_top_10<-head(covid_df_all_states_daily_sum, 10) #or
covid_top_10<-covid_df_all_states_daily_sum[1:10,]

covid_top_10 #plot new dataframe with information on the top 10

#creating vectors
countries<-covid_top_10$Country_Region
tested_cases<-covid_top_10$tested
positive_cases<-covid_top_10$positive
active_cases<-covid_top_10$active
hospitalized_cases<-covid_top_10$hospitalized

#giving names to vectors
names(tested_cases)<-countries
names(positive_cases)<-countries
names(active_cases)<-countries
names(hospitalized_cases)<-countries

#creating vector with the top 3 positive cases per tested cases
ratio<-positive_cases/tested_cases
ordening<-order(ratio,decreasing = T)
positive_tested_top<-ratio[ordening]
positive_tested_top_3<-positive_tested_top[1:3]

positive_tested_top_3 #plot the resulting vector

#Now we want to keep all the information available for the top three countries that have had the highest number of positive cases against the number of tests.

#creating vectors
united_kingdom<-c(0.11, 1473672, 166909, 0, 0)
united_states<-c(0.10, 17282363, 1877179, 0, 0)
turkey<-c(0.08, 2031192, 163941, 2980960, 0)

#creating the matrix
covid_mat<-rbind(united_kingdom,united_states,turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

covid_mat #plotting new matrix

#Final steps of our analysis

question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)

datasets<-list(
  original=covid_df,
  allstates=covid_df_all_states,
  daily=covid_df_all_states_daily,
  top10=covid_top_10)

matrix<-list(covid_mat)

vectors<-list(vector_cols,countries)

data_structure_list <- list("dataframe" = datasets, "matrix" = matrix, "vector" = vectors)

covid_analysis_list <- list(question, answer, data_structure_list)

covid_analysis_list[[2]]
