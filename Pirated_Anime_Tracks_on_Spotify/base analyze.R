#technical
library(tidyverse)
library(dplyr)

#data from Airoe Anime illegal remixes
Darling_Ohayo_Remix <-  "data (.csv)\\Darling Ohayo - Remix-timeline.csv"
Darling_Ohayo_Edit <- "data (.csv)\\Darling Ohayo - Edit-timeline.csv"
Nyanpasu <- "data (.csv)\\Nyanpasu - Airoe Remix-timeline.csv"


#Darling Ohayo Remix Analyze Listens + earning
dor <- read.csv(Darling_Ohayo_Remix)
dor$date <- as.Date(dor$date) #changing type of information to date
colnames(dor) <- c("Date", "Streams") #changing names of cols

dor_earn <- mutate(dor, Earning = dor$Streams * 0.004) #mutate make copy | add earning by streams (https://groover.co/en/lp/free-tools/streaming-royalties-calculator/) 
print(dor_earn)
dor_earn_sum <- sum(dor_earn$Earning, na.rm = TRUE)
print(paste("sum of earning:", dor_earn_sum))


#Darling Ohayo Edit Analyze Listens + earning
doe <-  read.csv(Darling_Ohayo_Edit)
doe$date <- as.Date(doe$date) 
colnames(doe) <- c("Date", "Streams") 

doe_earn <- mutate(doe, Earning = doe$Streams * 0.004) 
print(dor_earn)
doe_earn_sum <- sum(doe_earn$Earning, na.rm = TRUE)
print(paste("sum of earning:", doe_earn_sum))


#Nyanpasu Analyze Listens + earning
nya <- read.csv(Nyanpasu)
nya$date <- as.Date(nya$date)
colnames(nya) <- c("Date", "Streams")

nya_earn <- mutate(nya, Earning = nya$Streams * 0.004)
print(nya_earn)
nya_earn_sum <- sum(nya_earn$Earning, na.rm = TRUE)
print(paste("sum of earning:", nya_earn_sum))


#graphics

#DOR
#summarise in month
dor_month <- dor_earn %>%
  mutate(Month = as.Date(cut(Date, "month"))) %>%
  group_by(Month) %>%
  summarise(
    Streams = sum(Streams, na.rm = TRUE),
    Earning = sum(Earning, na.rm = TRUE)
  )

#draw graphics - one side streams, second side earns
plot_dor <- ggplot(dor_month, aes(x = Month)) +
  geom_line(aes(y = Streams), color = "red") +
  geom_point(aes(y = Streams), color = "black", size = 1) +
  geom_smooth(aes(y = Streams), method = "lm", se = TRUE, color = "blue") +
  geom_line(aes(y = Earning / 0.004), color = "red") + #меняем, потому что нам надо считать по стримам, так было бы малые значения
  scale_y_continuous(
    name = 'Streams', 
    sec.axis = sec_axis(~ . * 0.004, name = "Earning")
    )

print(plot_dor)
ggsave("Plots\\dor_plot.png", plot = plot_dor, width = 25, height = 20, dpi = 300)


#DOE
#summarise in month
doe_month <- doe_earn %>%
  mutate(Month = as.Date(cut(Date, "month"))) %>%
  group_by(Month) %>%
  summarise(
    Streams = sum(Streams, na.rm = TRUE),
    Earning = sum(Earning, na.rm = TRUE)
  )

#draw graphics - one side streams, second side earns
plot_doe <- ggplot(doe_month, aes(x = Month)) +
  geom_line(aes(y = Streams), color = "red") +
  geom_point(aes(y = Streams), color = "black", size = 1) +
  geom_smooth(aes(y = Streams), method = "lm", se = TRUE, color = "blue") +
  geom_line(aes(y = Earning / 0.004), color = "red") +
  scale_y_continuous(
    name = "Streams",
    sec.axis = sec_axis(~ . * 0.004, name = "Earning")
  )

print(plot_doe)
ggsave("Plots\\doe_plot.png", plot = plot_doe, width = 25, height = 20, dpi = 300)


#Nyanpasu
#summarise in month
nay_month <- nya_earn %>%
  mutate(Month = as.Date(cut(Date, "month"))) %>%
  group_by(Month) %>%
  summarise(
    Streams = sum(Streams, na.rm = TRUE),
    Earning = sum(Earning, na.rm = TRUE)
  )

#draw graphics - one side streams, second side earns
plot_nya <- ggplot(nay_month, aes(x = Month)) +
  geom_line(aes(y = Streams), color = "red") +
  geom_point(aes(y = Streams), color = "black", size = 1) +
  geom_smooth(aes(y = Streams), method = "lm", se = TRUE, color = "blue") +
  geom_line(aes(y = Earning / 0.004), color = "red") +
  scale_y_continuous(
    name = "Streams", 
    sec.axis = sec_axis(~ . * 0.004, name = "Earning")
  )

print(plot_nya)
ggsave("Plots\\nya_plot.png", plot = plot_nya, width = 25, height = 20, dpi = 300)
  