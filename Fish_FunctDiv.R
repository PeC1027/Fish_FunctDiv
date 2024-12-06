setwd("C:/PEC/EDUCATION/PhD. Model of Complex Networks and Biodiversity/Research/Scripts/North_Sea_FishDiversity/TOT_Data_Lmdiv/FDiv")

########## Packages to be Installed ################

install.packages("ggplot2")         # Install ggplot2 package
install.packages("dplyr")           # Install dplyr package
install.packages("tidyr")           # Install tidyr package
install.packages("plyr")            # Install plyr package
install.packages("plotly")          # Install plotly package
install.packages("tidyverse")       # Install tidyverse package
install.packages("tidymodels")      # Install tidymodels package
install.packages("xts")             # Install xts package
install.packages("stats")           # Install stats package
install.packages("hydroTSM")        # Install hydroTSM package
install.packages("reshape2")        # Install reshape2 package


########## Call packages from Library ################

library(ggplot2)                    # Load ggplot2
library(lubridate)                  # Load lubridate
library(stringr)                    # Load stringr
library(plotly)                     # Load plotly
library(tidyr)                      # Load tidyr
library(plyr)                       # Load plyr
library(tidyverse)                  # Load tidyverse
library(tidymodels)                 # Load tidymodels
library(xts)                        # Load xts
library(stats)                      # Load stats
library(hydroTSM)                   # Load hydroTSM
library(reshape2)                   # Load reshape2
 
##################     Data Treatment    ########################################

# The data to be anlyzed is a time series of functional diversity calculated using the approach 
# by Ryabov et al. (2022) and expanded by Carrasco et al. (2023). The extension of the data is 
# of 41 years (1980-2021), and it expands over the entire North Sea basin. 
# In order to have a good perspective of the variations of functional diversity, the analysis is
# performed at different time resolutions (yearly and seasonally) as well as at different spatial
# resolutions (Areas and Subareas). 
# The data needs to have the Date column to proper Date format, and include a column for season,
# year and month. It is also important to transform columns to factors. 

############ Load data #############

graph1 <-read.table(file="TOT_Fdiv_Lm.csv", 
                    header=TRUE, dec=".", sep=",")          
graph1                                                      #### Call data

graph1$Date <- as.Date(graph1$Date, format="%m/%d/%Y")      #### Transform column Date to format as.Date

str(graph1)                                                 #### Verify format of columns

my_seasons <- time2season(graph1$Date,                      #### Convert dates to seasons
                          out.fmt = "seasons")

x <- as.data.frame(my_seasons)                              #### Arrange the vector as a dataframe


year <- as.numeric(format(graph1$Date,'%Y'))                #### Add a column with the variable Year extracted from Date
year <- as.data.frame(year) 

x1 <- as.numeric(format(graph1$Date,'%m'))                  #### Add a column with the variable Month extracted from Date
month<- month(x1, label = TRUE, abbr = TRUE)
month <- as.data.frame(month)

data = cbind(graph1, x, year,month)                         #### Bind both dataframe now including each data with its season
data

#### Transform Area and SubArea columns to Factor ####

data$Area = factor(data$Area) 
data$SubArea = factor(data$SubArea)


#################  Analysis by Area and Season  ###############################

# Once our Data was treated accordingly we proceed to do the first analysis, asssesing the variation
# of functional diversity monthly/season at each ICES Area (total of 10).

######################## Calculate a Monthly Mean per year and Station  #####################################

BymonthMean = aggregate(Values~Area + month + year + my_seasons, data = data, mean)
BymonthMean

################################ Area 1 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '1'],
        y=~ BymonthMean$Values[BymonthMean$Area == '1'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '1'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 1', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))




################################ Area 2 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '2'],
        y=~ BymonthMean$Values[BymonthMean$Area == '2'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '2'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 2', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)))






################################ Area 3 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '3'],
        y=~ BymonthMean$Values[BymonthMean$Area == '3'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '3'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 3', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)))


################################ Area 4 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '4'],
        y=~ BymonthMean$Values[BymonthMean$Area == '4'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '4'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 4', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)))




################################ Area 5 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '5'],
        y=~ BymonthMean$Values[BymonthMean$Area == '5'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '5'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 5', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))


################################ Area 6 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '6'],
        y=~ BymonthMean$Values[BymonthMean$Area == '6'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '6'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 6', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))


################################ Area 7 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '7'],
        y=~ BymonthMean$Values[BymonthMean$Area == '7'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '7'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 7', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))


################################ Area 8 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '8'],
        y=~ BymonthMean$Values[BymonthMean$Area == '8'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '8'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 8', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))


################################ Area 9 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '9'],
        y=~ BymonthMean$Values[BymonthMean$Area == '9'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '9'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 9', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))



################################ Area 10 ####################################

plot_ly(BymonthMean,
        x=~ BymonthMean$month[BymonthMean$Area == '10'],
        y=~ BymonthMean$Values[BymonthMean$Area == '10'],
        type = 'scatter',
        mode = 'markers',
        color = ~BymonthMean$my_seasons[BymonthMean$Area == '10'],
        marker = list(
          size=15,
          opacity=.9
        )
)%>%
  layout(title = 'Area 10', xaxis = list(title = '', showgrid = F, dtick = 2.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)))



#################  Analysis by Area and Year  ###############################

# We now proceed to do the second analysis, asssesing the variation of functional diversity 
# yearly at each ICES Area (total of 10). We will include a regresion test to verify which trend
# is statistically significant. Due to numerical constraint of the package lm, we calculate the
# regression line using the year column as number first. Once donde, we transform the years to factor
# in order to recalculate the vector monthlyMean in order to plot Year as factors in the x-axis.

######################## Calculate a Seasonal Mean per year and Station  #####################################

monthlyMean = aggregate(Values~Area + year, data = data, mean)
monthlyMean

################################ Area 1 ####################################

###### Regression Test #######
fit_Area1 = lm(monthlyMean$Values[monthlyMean$Area == '1']~ monthlyMean$year[monthlyMean$Area == '1']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area1 = data.frame(monthlyMean$year[monthlyMean$Area == '1'])   #### Isolate the Year column as Dataframe

prediction_Area1 = predict(fit_Area1,fitdata_Area1,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area1$fitted = prediction_Area1$fit

##### Create Confidence Intervals ############

fitdata_Area1$ymin = fitdata_Area1$fitted - 1.96*prediction_Area1$se.fit
fitdata_Area1$ymax = fitdata_Area1$fitted + 1.96*prediction_Area1$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area1 = cor.test(monthlyMean$year[monthlyMean$Area == '1'],monthlyMean$Value[monthlyMean$Area == '1'])[c("estimate","p.value")]
COR_textArea1 = paste(c("R=","p="),signif(as.numeric(COR_Area1,3),3),collapse=" ")


################################ Area 2 ####################################

###### Regression Test #######
fit_Area2 = lm(monthlyMean$Values[monthlyMean$Area == '2']~ monthlyMean$year[monthlyMean$Area == '2']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area2 = data.frame(monthlyMean$year[monthlyMean$Area == '2'])   #### Isolate the Year column as Dataframe

prediction_Area2 = predict(fit_Area2,fitdata_Area1,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area2$fitted = prediction_Area2$fit

##### Create Confidence Intervals ############

fitdata_Area2$ymin = fitdata_Area2$fitted - 1.96*prediction_Area2$se.fit
fitdata_Area2$ymax = fitdata_Area2$fitted + 1.96*prediction_Area2$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area2 = cor.test(monthlyMean$year[monthlyMean$Area == '2'],monthlyMean$Value[monthlyMean$Area == '2'])[c("estimate","p.value")]
COR_textArea2 = paste(c("R=","p="),signif(as.numeric(COR_Area2,3),3),collapse=" ")


################################ Area 3 ####################################

###### Regression Test #######
fit_Area3 = lm(monthlyMean$Values[monthlyMean$Area == '3']~ monthlyMean$year[monthlyMean$Area == '3']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area3 = data.frame(monthlyMean$year[monthlyMean$Area == '3'])   #### Isolate the Year column as Dataframe

prediction_Area3 = predict(fit_Area3,fitdata_Area3,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area3$fitted = prediction_Area3$fit

##### Create Confidence Intervals ############

fitdata_Area3$ymin = fitdata_Area3$fitted - 1.96*prediction_Area3$se.fit
fitdata_Area3$ymax = fitdata_Area3$fitted + 1.96*prediction_Area3$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area3 = cor.test(monthlyMean$year[monthlyMean$Area == '3'],monthlyMean$Value[monthlyMean$Area == '3'])[c("estimate","p.value")]
COR_textArea3 = paste(c("R=","p="),signif(as.numeric(COR_Area3,3),3),collapse=" ")



################################ Area 4 ####################################

###### Regression Test #######
fit_Area4 = lm(monthlyMean$Values[monthlyMean$Area == '4']~ monthlyMean$year[monthlyMean$Area == '4']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area4 = data.frame(monthlyMean$year[monthlyMean$Area == '4'])   #### Isolate the Year column as Dataframe

prediction_Area4 = predict(fit_Area4,fitdata_Area4,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area4$fitted = prediction_Area4$fit

##### Create Confidence Intervals ############

fitdata_Area4$ymin = fitdata_Area4$fitted - 1.96*prediction_Area4$se.fit
fitdata_Area4$ymax = fitdata_Area4$fitted + 1.96*prediction_Area4$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area4 = cor.test(monthlyMean$year[monthlyMean$Area == '4'],monthlyMean$Value[monthlyMean$Area == '4'])[c("estimate","p.value")]
COR_textArea4 = paste(c("R=","p="),signif(as.numeric(COR_Area4,3),3),collapse=" ")


################################ Area 5 ####################################

###### Regression Test #######
fit_Area5 = lm(monthlyMean$Values[monthlyMean$Area == '5']~ monthlyMean$year[monthlyMean$Area == '5']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area5 = data.frame(monthlyMean$year[monthlyMean$Area == '5'])   #### Isolate the Year column as Dataframe

prediction_Area5 = predict(fit_Area5,fitdata_Area5,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area5$fitted = prediction_Area5$fit

##### Create Confidence Intervals ############

fitdata_Area5$ymin = fitdata_Area5$fitted - 1.96*prediction_Area5$se.fit
fitdata_Area5$ymax = fitdata_Area5$fitted + 1.96*prediction_Area5$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area5 = cor.test(monthlyMean$year[monthlyMean$Area == '5'],monthlyMean$Value[monthlyMean$Area == '5'])[c("estimate","p.value")]
COR_textArea5 = paste(c("R=","p="),signif(as.numeric(COR_Area5,3),3),collapse=" ")


################################ Area 6 ####################################

###### Regression Test #######
fit_Area6 = lm(monthlyMean$Values[monthlyMean$Area == '6']~ monthlyMean$year[monthlyMean$Area == '6']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area6 = data.frame(monthlyMean$year[monthlyMean$Area == '6'])   #### Isolate the Year column as Dataframe

prediction_Area6 = predict(fit_Area6,fitdata_Area6,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area6$fitted = prediction_Area6$fit

##### Create Confidence Intervals ############

fitdata_Area6$ymin = fitdata_Area6$fitted - 1.96*prediction_Area6$se.fit
fitdata_Area6$ymax = fitdata_Area6$fitted + 1.96*prediction_Area6$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area6 = cor.test(monthlyMean$year[monthlyMean$Area == '6'],monthlyMean$Value[monthlyMean$Area == '6'])[c("estimate","p.value")]
COR_textArea6 = paste(c("R=","p="),signif(as.numeric(COR_Area6,3),3),collapse=" ")


################################ Area 7 ####################################

###### Regression Test #######
fit_Area7 = lm(monthlyMean$Values[monthlyMean$Area == '7']~ monthlyMean$year[monthlyMean$Area == '7']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area7 = data.frame(monthlyMean$year[monthlyMean$Area == '7'])   #### Isolate the Year column as Dataframe

prediction_Area7 = predict(fit_Area7,fitdata_Area7,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area7$fitted = prediction_Area7$fit

##### Create Confidence Intervals ############

fitdata_Area7$ymin = fitdata_Area7$fitted - 1.96*prediction_Area7$se.fit
fitdata_Area7$ymax = fitdata_Area7$fitted + 1.96*prediction_Area7$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area7 = cor.test(monthlyMean$year[monthlyMean$Area == '7'],monthlyMean$Value[monthlyMean$Area == '7'])[c("estimate","p.value")]
COR_textArea7 = paste(c("R=","p="),signif(as.numeric(COR_Area7,3),3),collapse=" ")


################################ Area 8 ####################################

###### Regression Test #######
fit_Area8 = lm(monthlyMean$Values[monthlyMean$Area == '8']~ monthlyMean$year[monthlyMean$Area == '8']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area8 = data.frame(monthlyMean$year[monthlyMean$Area == '8'])   #### Isolate the Year column as Dataframe

prediction_Area8 = predict(fit_Area8,fitdata_Area8,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area8$fitted = prediction_Area8$fit

##### Create Confidence Intervals ############

fitdata_Area8$ymin = fitdata_Area8$fitted - 1.96*prediction_Area8$se.fit
fitdata_Area8$ymax = fitdata_Area8$fitted + 1.96*prediction_Area8$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area8 = cor.test(monthlyMean$year[monthlyMean$Area == '8'],monthlyMean$Value[monthlyMean$Area == '8'])[c("estimate","p.value")]
COR_textArea8 = paste(c("R=","p="),signif(as.numeric(COR_Area8,3),3),collapse=" ")


################################ Area 9 ####################################

###### Regression Test #######
fit_Area9 = lm(monthlyMean$Values[monthlyMean$Area == '9']~ monthlyMean$year[monthlyMean$Area == '9']
               ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area9 = data.frame(monthlyMean$year[monthlyMean$Area == '9'])   #### Isolate the Year column as Dataframe

prediction_Area9 = predict(fit_Area9,fitdata_Area9,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area9$fitted = prediction_Area9$fit

##### Create Confidence Intervals ############

fitdata_Area9$ymin = fitdata_Area9$fitted - 1.96*prediction_Area9$se.fit
fitdata_Area9$ymax = fitdata_Area9$fitted + 1.96*prediction_Area9$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area9 = cor.test(monthlyMean$year[monthlyMean$Area == '9'],monthlyMean$Value[monthlyMean$Area == '9'])[c("estimate","p.value")]
COR_textArea9 = paste(c("R=","p="),signif(as.numeric(COR_Area9,3),3),collapse=" ")


################################ Area 10 ####################################

###### Regression Test #######
fit_Area10 = lm(monthlyMean$Values[monthlyMean$Area == '10']~ monthlyMean$year[monthlyMean$Area == '10']
                ,data=monthlyMean)    ####### Create a Regression Line model

fitdata_Area10 = data.frame(monthlyMean$year[monthlyMean$Area == '10'])   #### Isolate the Year column as Dataframe

prediction_Area10 = predict(fit_Area10,fitdata_Area10,se.fit=TRUE)                               #### Make the prediction using Lm
fitdata_Area10$fitted = prediction_Area10$fit

##### Create Confidence Intervals ############

fitdata_Area10$ymin = fitdata_Area10$fitted - 1.96*prediction_Area10$se.fit
fitdata_Area10$ymax = fitdata_Area10$fitted + 1.96*prediction_Area10$se.fit

##### Evaluate the Correlation of Variables ############

COR_Area10 = cor.test(monthlyMean$year[monthlyMean$Area == '10'],monthlyMean$Value[monthlyMean$Area == '10'])[c("estimate","p.value")]
COR_textArea10 = paste(c("R=","p="),signif(as.numeric(COR_Area10,3),3),collapse=" ")


######################## Convertion of Year to Factor & Recalculation of   #####################################

data$year = factor(data$year) 
#data$my_seasons = factor(data$my_seasons)
#data$Station = factor(data$Station)
#data$month = factor(data$month)

str(data)

# After covertion of the Year column to factor, we re-calculate the vector monthlyMean, to make
# sure the years will be treated as Factors when plotted.

monthlyMean = aggregate(Values~Area + year, data = data, mean)
monthlyMean

# Now we proceed to plot the results of the regression per ICES Area

################################ Area 1 ####################################

monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '1']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '1'], y = ~monthlyMean$Value[monthlyMean$Area == '1'], size=25) %>%
  add_trace(data=fitdata_Area1,x= ~monthlyMean$year[monthlyMean$Area == '1'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area1, ymin = ~ ymin, ymax = ~ ymax,
            line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 1', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea1,showarrow =FALSE))


################################ Area 2 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '2']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '2'], y = ~monthlyMean$Value[monthlyMean$Area == '2'], size=25) %>%
  add_trace(data=fitdata_Area2,x= ~monthlyMean$year[monthlyMean$Area == '2'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area2, ymin = ~ ymin, ymax = ~ ymax,
            line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 2', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea2,showarrow =FALSE))

################################ Area 3 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '3']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '3'], y = ~monthlyMean$Value[monthlyMean$Area == '3'], size=25) %>%
  add_trace(data=fitdata_Area3,x= ~monthlyMean$year[monthlyMean$Area == '3'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area3, ymin = ~ ymin, ymax = ~ ymax,
           line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 3', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea3,showarrow =FALSE))

################################ Area 4 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '4']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '4'], y = ~monthlyMean$Value[monthlyMean$Area == '4'], size=25) %>%
  add_trace(data=fitdata_Area4,x= ~monthlyMean$year[monthlyMean$Area == '4'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area4, ymin = ~ ymin, ymax = ~ ymax,
             line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 4', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,0.6)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea4,showarrow =FALSE))

################################ Area 5 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '5']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '5'], y = ~monthlyMean$Value[monthlyMean$Area == '5'], size=25) %>%
  add_trace(data=fitdata_Area5,x= ~monthlyMean$year[monthlyMean$Area == '5'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area5, ymin = ~ ymin, ymax = ~ ymax,
            line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 5', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea5,showarrow =FALSE))

################################ Area 6 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '6']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '6'], y = ~monthlyMean$Value[monthlyMean$Area == '6'], size=25) %>%
  add_trace(data=fitdata_Area6,x= ~monthlyMean$year[monthlyMean$Area == '6'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area6, ymin = ~ ymin, ymax = ~ ymax,
            line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 6', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea6,showarrow =FALSE))

################################ Area 7 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '7']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '7'], y = ~monthlyMean$Value[monthlyMean$Area == '7'], size=25) %>%
  add_trace(data=fitdata_Area7,x= ~monthlyMean$year[monthlyMean$Area == '7'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area7, ymin = ~ ymin, ymax = ~ ymax,
             line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 7', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea7,showarrow =FALSE))

################################ Area 8 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '8']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '8'], y = ~monthlyMean$Value[monthlyMean$Area == '8'], size=25) %>%
  add_trace(data=fitdata_Area8,x= ~monthlyMean$year[monthlyMean$Area == '8'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area8, ymin = ~ ymin, ymax = ~ ymax,
             line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 8', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.5,
                            text = COR_textArea8,showarrow =FALSE))

################################ Area 9 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '9']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '9'], y = ~monthlyMean$Value[monthlyMean$Area == '9'], size=25) %>%
  add_trace(data=fitdata_Area9,x= ~monthlyMean$year[monthlyMean$Area == '9'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area9, ymin = ~ ymin, ymax = ~ ymax,
             line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 9', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = '', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 15, y = 0.55,
                            text = COR_textArea9,showarrow =FALSE))

################################ Area 10 ####################################
monthlyMean %>%
  plot_ly(x = ~monthlyMean$year[monthlyMean$Area == '10']) %>%
  add_markers(x=~monthlyMean$year[monthlyMean$Area == '10'], y = ~monthlyMean$Value[monthlyMean$Area == '10'], size=25) %>%
  add_trace(data=fitdata_Area10,x= ~monthlyMean$year[monthlyMean$Area == '10'], y = ~fitted, 
            mode = "lines",type="scatter",line=list(color="black")) %>%
  add_ribbons(data=fitdata_Area10, ymin = ~ ymin, ymax = ~ ymax,
             line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" )%>%
  layout(title = 'Area 10', xaxis = list(title = '', showgrid = F, dtick = 10.0), 
         yaxis = list(title = 'Functional Diversity', showgrid = F, dtick = 0.2, range = c(0,1)), showlegend = F,
         annotations = list(x = 5, y = 0.5,
                            text = COR_textArea10,showarrow =FALSE))


#################  Analysis by SubArea (Variation Functional Diversity/Year) ###############################

# To get a better spatial resolution, the regression analysis was performed by ICES SubArea, using
# the same approach as in the previous step. The results of the slope (Fdiv/year) are stored in the
# file Fd_NS.csv which was worked previously, and can be loaded here treatment and visualization.
# The column FdYear of the previously mentioned file will be transformed to percentage as it is
# currently stored as a rate of functional diversity change.

########## Packages to be Installed ################

install.packages("maps")            # Install maps package
install.packages("sf")              # Install sf package
install.packages("ggOceanMaps")     # Install ggOceanMaps package
install.packages("ggspatial")       # Install ggspatial package
install.packages("geom_sf")         # Install geom_sf package
install.packages("ggmap")           # Install ggmap package
install.packages("metR")            # Install metR package

library(ggmap)                      # Load ggmap
library(metR)                       # Load metR
library(maps)                       # Load maps
library(sf)                         # Load sf
library(ggOceanMaps)                # Load ggOceanMaps
library(ggspatial)                  # Load ggspatial
library(geom_sf)                    # Load geom_sf


############ Load data #############

Diver <-read.table(file="Fd_NS.csv", 
                   header=TRUE, dec=".", sep=",")          

#### Transform the Colum FdYear to a percentage ####

Diver$Per = Diver$FdYear*100

Diver                                              #### Call data

############ Call a WorldMap ############

world_map <- map_data("world")

#Creat a base plot with gpplot2
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="grey", fill="light grey")

base_world_messy

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

base_world

######################  Functional Diversity    ##############################

# Now we create a map in which each sample is colored-coded according to the Fdiv/year(%) of its
# respective ICES SubArea. This will allow us to understand changes at a finer spatial scale.

map_data <- 
  base_world_messy +
  geom_point(data=Diver,
             aes(x=LOG, y=LAT, color = Per), size=1) + 
  coord_sf(xlim = c(-5, 13), ylim = c(50, 63)) +
  #coord_fixed(xlim = c(-10,13), 
  #           ylim = c(43, 63))+
  scale_colour_gradient2(
    low="darkred", mid="floralwhite", high="steelblue", midpoint=mean(Diver$Per), name ="Fdiv/year(%)"
  )+ xlab("Longitude") + ylab("Latitude") + 
  ggtitle("North Sea Fish Functional Diversity") + 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"))+
  theme(legend.position = "right")

map_data
