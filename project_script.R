## ----setup, include=FALSE, message=FALSE ,warning=FALSE , fig.align='left'--------------------
knitr::opts_chunk$set(echo = TRUE)




## ----include=FALSE----------------------------------------------------------------------------

library(tidyverse)

library(dplyr)

df0 <- read.csv("C:/Users/44751/OneDrive/Desktop/200266165/PSYC6422/2020_tensorflow_survey.csv")

df0 <- df0 %>% 
  
  filter(Country == "United Kingdom")


## ---------------------------------------------------------------------------------------------


datafile= "C:/Users/44751/OneDrive/Desktop/200266165/PSYC6422/2020_tensorflow_survey.csv"  # this loads the data

df0 <- read.csv(datafile)

head(df0,5)       #this displays the first 5 rows of the data





## ----echo=FALSE-------------------------------------------------------------------------------

 table <- tibble::tribble(
   ~Variable.name,                               ~Explanation,
         "Gender", "Either Man , Woman , Non-Binary",             
        "Country", "Counrty where respondent currently lives",             
  "JobSat"       , "Level of satisfaction as 5 likert scale of satisfaction " , 
  "WorkWeekHrs", "How many hours does the respondent work per week"
  
  )

require(knitr)
kable(table, digits = 2, row.names = FALSE, align = "l",
              caption = NULL)




## ---------------------------------------------------------------------------------------------
# loading the required libraries

library(dplyr)                                                 
library(outliers)                                              

# filtering , selecting and removing missing values from the dataset


df <- df0 %>% 
  
  filter(Country == "United Kingdom", Gender == "Man" | Gender == "Woman") %>%   # this Subsets Data Frame only retaining Man and Women responses from United kingdom                
  select("Gender","Country","JobSat","WorkWeekHrs") %>%   # this Subsets Data frame to include the specific  variables for our analysis
 
  na.omit(df)   # this removes the missing values from data frame


# removing outlier values from WorkWeekHrs column using the quantiles method

Q1 <- quantile(df$WorkWeekHrs, .25)                           # this calculates the Q1 

Q3 <- quantile(df$WorkWeekHrs, .75)                           # this calculates the Q3

IQR <- IQR(df$WorkWeekHrs)                                    # this calculates the interquantile range


clean_df<- subset(df, df$WorkWeekHrs> (Q1 - 1.5*IQR) & df$WorkWeekHrs< (Q3 + 1.5*IQR)) # this subsets data frame and retain only data between (Q1 - 1.5*IQR) and (Q3 + 1.5*IQR)


head(clean_df,5)                                              #this shows the first 5 rows of the processed dataframe


summary(clean_df)                                             #this shows summary statistics




## ----figure1, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------

# labeling the parameter of the graph 

title <- "Overall  Career Satisfaction among UK Data Scientists "         

ylab <- "Percentage of Responses"      


# setting the limits for x-axis 

x_limits <- c("Very satisfied",     
              "Slightly satisfied",
              "Neither satisfied nor dissatisfied",
              "Slightly dissatisfied",
              "Very dissatisfied")


# drawing a bar graph with JobSatisfaction on X-axis and percentages of responses on y-axis then flip the coordinates 

library(ggplot2)                                                           

figure1 <- ggplot(data=clean_df, mapping = aes(x=JobSat, fill= "JobSat"))  

figure1 + geom_bar(mapping = aes(y=..prop..,group="JobSat"), width = 0.75)+labs(y =ylab,
                                                                                title = title)+
                                                                          scale_x_discrete(limits=x_limits)+
                                                                          scale_y_continuous(labels = scales::percent)+coord_flip()+
                                                                          theme(legend.position = "none",
                                                                                 panel.background = element_blank(),
                                                                                 axis.title.y = element_blank())+
                                                                           scale_fill_brewer(palette = "Dark2")
  




## ----figure2, echo=TRUE, message=FALSE, warning=FALSE , fig.width=10--------------------------


#labeling the parameters of the graph 

title <- " Weekly Work Hours vs level of Satisfaction "
xlab <- "level of Satisfaction "
ylab <- "Weekly Work hours "
y_breaks <- c(30,33,36,39,42,45)


# calculating the mean values of box plot groups

summary_df <- clean_df %>% 
  group_by(JobSat) %>% 
  summarise(means=mean(WorkWeekHrs))


# drawing a box-plot with geom_jitter with JobSatisfaction on X-axis and and Weekly Working hours on Y-axis , and adding mean values as text on every box-plot 

figure2 <- ggplot(data=clean_df,mapping = aes(x=JobSat , y=WorkWeekHrs, color = JobSat))

figure2+geom_boxplot()+ geom_text(data=summary_df, aes(y=means, label=round(means,2)),
                                size= 3,color='black',position=position_dodge(0.8))+
                        geom_jitter(data = clean_df %>% group_by(JobSat) %>% sample_n(150),position = position_jitter(0.5),size = 0.9)+
                        labs(title = title,
                             x=xlab,
                             y=ylab)+
                        scale_x_discrete(limits=x_limits)+
                        scale_y_continuous(breaks = y_breaks)+theme(legend.position = "none",
                                                                plot.title=element_text(size=14, 
                                                                             hjust=0.5),
                                                               axis.line.x = element_line(size = 0.5, colour="black"),
                                                               axis.line.y = element_line(size = 0.5, colour = "black"),
                                                               axis.line = element_line(size=1, colour = "black"),
                                                               panel.grid.major = element_blank(),
                                                               panel.grid.minor = element_blank(),
                                                               panel.border = element_blank(),
                                                               panel.background = element_blank())+scale_fill_brewer(palette = "Dark2") 
                        

  
  

