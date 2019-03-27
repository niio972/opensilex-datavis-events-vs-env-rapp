# from github
# install.packages('remotes')
# remotes::install_github("OpenSILEX/phisWSClientR")
# # or from the zip or tag.gz source
# install.packages("/home/opencpu/scripts/phisWSClientR_1.2.0.tar.gz",
#                  repos=NULL,
#                  type ="source")

# startDate ="2017-05-22"
# endDate ="2017-06-30"
# pageSize=20000
# scientificObjectURI="http://www.opensilex.org/demo/2018/o18000175"


eventVSEnvironmental <- function(scientificObjectURI, startDate ="", endDate="", showPoint = FALSE, pageSize = 1000){
# gathering envrionmental data (wind)
phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
aToken = phisWSClientR::getToken("guest@opensilex.org","guest")
vars <- phisWSClientR::getVariables2(aToken$data)
data = phisWSClientR::getEnvironmentData(
  aToken$data,
  variable ="http://www.opensilex.org/demo/id/variables/v004",
  startDate = paste0(startDate,"T00:00:00+0100"),
  endDate =   paste0(endDate,"T00:00:00+0100"),
  pageSize = pageSize)$data

# gathering event informations
eventData=phisWSClientR::getEvents(
  token=aToken$data,
  type = "http://www.opensilex.org/vocabulary/oeev#ScientificObjectManagement",
  concernsUri=scientificObjectURI,
  dateRangeStart = paste0(startDate,"T00:00:00+0100"),
  dateRangeEnd = paste0(endDate,"T00:00:00+0100") )$data

library(dplyr)

eventDataProcessed <- eventData %>%
  filter(date > as.Date(startDate)) %>%
  filter(date < as.Date(endDate))

#create dataframe which represents environmental values
dataToShow <- data
dates <- lubridate::ymd_hms(dataToShow$date, tz = "UTC")
values <- data$value

dataToShow[["date"]] <- dates
dataToShow[["value"]] <- values
dataToShow[["dateVector"]] <- 1:length(dates)

# create gam model
model <- mgcv::gam(value ~ s(dateVector), data = dataToShow)
selector <- abs(model$residuals) >= min(abs(boxplot.stats(model$residuals)$out))
outliers <- dataToShow[selector, ]

# fromat event informations
eventX=eventDataProcessed[2]
dateT=eventDataProcessed[4]
# display all events names
# substr(eventX[,1],42,nchar(eventX[1,1])) #event name
# extract two first letters
EVname=substr(eventX[,1],42,43) # 2 first letters
# convert event dates to Date object
EVdate=lubridate::ymd_hms(dateT[,1],tz="UTC")
events <-data.frame(date = EVdate, value = EVname)

# create ggplot plot
p <- ggplot2::ggplot(dataToShow, ggplot2::aes(date,value))
if(showPoint == TRUE){
  p <- p + ggplot2::geom_point(colour="blue", size = 0.2)
}
  # ggplot2::geom_point(colour="blue", size = 0.2) +
p <-p + ggplot2::geom_point(data=outliers, ggplot2::aes(x=date, y=value), colour="red", size=2) +
  ggplot2::stat_smooth(method = "gam", formula = y ~ s(x), size = 1, colour = "green" ,n = 160) +
  # ggplot2::geom_segment(data = events, mapping=ggplot2::aes(x=date, y=0.5, xend=date, yend=0.5)) +
  # ggplot2::geom_point(data = events, mapping=ggplot2::aes(x=date,y=0.5), size=3) +
  ggplot2::geom_text(data = events, mapping=ggplot2::aes(x=date, y=0.5, label=value), hjust=-0.1, vjust=0.1, size=5)

p <- plotly::ggplotly(p)
p
}

# eventVSEnvironmental(scientificObjectURI, startDate, endDate,pageSize)
