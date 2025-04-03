#Loading packages

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

#Loading database

tfl_journeys_main <- read_xlsx("tfl-journeys-type-inf4000.xlsx")

#Dataset cleaning

tfl_journeys_working <- filter(tfl_journeys_main, Period_ending >= as.Date("2016-04-30") & Period_ending <= as.Date("2022-12-10"))

tfl_journeys_working1 <- select(tfl_journeys_working, Period_ending, Bus_Journeys_millions,
                                Underground_Journeys_millions, DLR_Journeys_millions,
                                Tram_Journeys_millions, Overground_Journeys_millions,
                                TfL_Rail_Journeys_millions)


#Plotting Radar Chart

library(fmsb)

tfl_journeys_working2019 <- select(tfl_journeys_working1, Bus_Journeys_millions,
                                   Underground_Journeys_millions, DLR_Journeys_millions,
                                   Tram_Journeys_millions, Overground_Journeys_millions,
                                   TfL_Rail_Journeys_millions)

tfl_journeys_working2019 <- filter(tfl_journeys_working1, Period_ending > as.Date("2018-12-08") &
                                     Period_ending < as.Date("2020-01-04"))

tfl_mean2019 = data.frame(Bus = mean(tfl_journeys_working2019$Bus_Journeys_millions), Underground = mean(tfl_journeys_working2019$Underground_Journeys_millions),
                          DLR = mean(tfl_journeys_working2019$DLR_Journeys_millions), Tram = mean(tfl_journeys_working2019$Tram_Journeys_millions),
                          Overground = mean(tfl_journeys_working2019$Overground_Journeys_millions), TfLRail = mean(tfl_journeys_working2019$TfL_Rail_Journeys_millions))

rownames(tfl_mean2019) <- c("Mean")

tfl_maxmin2019 = data.frame(Bus = c(max(tfl_journeys_working2019$Bus_Journeys_millions), min(tfl_journeys_working2019$Bus_Journeys_millions)),
                             Underground = c(max(tfl_journeys_working2019$Underground_Journeys_millions), min(tfl_journeys_working2019$Underground_Journeys_millions)),
                             DLR = c(max(tfl_journeys_working2019$DLR_Journeys_millions), min(tfl_journeys_working2019$DLR_Journeys_millions)),
                             Tram = c(max(tfl_journeys_working2019$Tram_Journeys_millions), min(tfl_journeys_working2019$Tram_Journeys_millions)),
                             Overground = c(max(tfl_journeys_working2019$Overground_Journeys_millions), min(tfl_journeys_working2019$Overground_Journeys_millions)),
                             TfLRail = c(max(tfl_journeys_working2019$TfL_Rail_Journeys_millions), min(tfl_journeys_working2019$TfL_Rail_Journeys_millions)))

rownames(tfl_maxmin2019) <- c("Max", "Min")
tfl_rc2019 <- rbind(tfl_maxmin2019, tfl_mean2019)

create_beautiful_radarchart <- function(data, color = "#0009AB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "black", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "black", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(tfl_rc2019, title = "Mean Number of Trips made on Different TfL Services in 2019\nas a Percentage of the Maximum Number of Trips")
par(op)

#Plotting Line Graph

colors1 <- c("Bus" = "#D01A1E", "Underground" = "#0009AB")
ggplot() + geom_line(data = tfl_journeys_working1, aes(x = Period_ending, y = Bus_Journeys_millions, color = "Bus")) +
  geom_line(data = tfl_journeys_working1, aes(x = Period_ending, y = Underground_Journeys_millions, color = "Underground")) +
  labs(x = "Year", y = "Number of Trips Taken\n(millions)", title = "Number of Trips Taken on TfL Services\nwith Respect to Time",
       color = "Legend") +
  scale_color_manual(values = colors1)

#Plotting Bar Chart
colors2 = c("#E32017", "#FFD300", "#EE7C0E", "#F3A9BB")
tfl_journeys_working2020 <- filter(tfl_journeys_working1, Period_ending > as.Date("2019-12-07") & Period_ending < as.Date("2021-01-09"))
tfl_journeys_working2021 <- filter(tfl_journeys_working1, Period_ending > as.Date("2020-12-12") & Period_ending < as.Date("2022-01-08"))
tfl_journeys_working2022 <- filter(tfl_journeys_working1, Period_ending > as.Date("2021-12-11"))
mean_ujourneys <- c(tfl_mean2019$Underground, mean(tfl_journeys_working2020$Underground_Journeys_millions),
                     mean(tfl_journeys_working2021$Underground_Journeys_millions), mean(tfl_journeys_working2022$Underground_Journeys_millions))
year_range = c(2019, 2020, 2021, 2022)
ubar_data = data.frame(year_range, mean_ujourneys)
ggplot() + geom_col(data = ubar_data, aes(x = year_range, y = mean_ujourneys), fill = colors2)+
  labs(x = "Year", y = "Number of Journeys\n(millions)", title = "Number of Tube Journeys Done in 2019 onwards")+
  scale_color_manual(values = colors2)

#Plotting Pie Chart - Version 2
slices <- pie_data_working$pie_prop
slices_labels <- round(slices/sum(slices) * 100, 1)
slice_labels <- paste(slices_labels, "%", sep = "")
pie(slices, main = "Proportions of Trips Done on TfL Train Services", col = colors3, labels = slices_labels, cex = 0.8)
legend("topleft", pie_data_working$pie_breakdown, cex = 0.7, fill = colors3)