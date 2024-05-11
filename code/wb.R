#import needed libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#import the data
file_path <- "data/World Bank.xls"
wb <- read_excel(file_path)

#clean the columns and rows
new_col_names <- wb[3, ]
colnames(wb) <- as.character(new_col_names) 
wb <- wb[-c(1:3), ]


#To only select the three columns of interest: Country Name, 2019, 2020
wb <- wb |>
  select('Country Name','2019':'2020')

#Drop any rows with missing data
wb <- na.omit(wb)

#Converting the columns 2019 and 2020 to integers
wb$'2019' <- as.integer(wb$'2019')
wb$'2020' <- as.integer(wb$'2020')
 
################################################################################
#HERE, we are going to do some statistical analysis

#Calculate the min, max, median, and mean for the 2019 column
average_2019 <- mean(wb$'2019')
median_2019 <- median(wb$'2019')
min_2019 <- min(wb$'2019')
max_2019 <- max(wb$'2019')

cat("Average number of tourists in 2019:", average_2019, "\n")
cat("Median number of tourists in 2019:", median_2019, "\n")
cat("Minimum number of tourists in 2019:", min_2019, "\n")
cat("Maximum number of tourists in 2019:", max_2019, "\n")

#Now, let's filter the data to keep the countries with statistical significance
#We chose the ones that have more tourists in 2019 than the median of the column
wb <- subset(wb, wb$'2019' > median_2019)
head(wb)

#out of these countries, we will examine the ones that had the highest percentage drop
wb$PercentageChange <- ((wb$'2020' - wb$'2019') / wb$'2019') * 100
wb <- wb[order(wb$PercentageChange), ]
wb <- head(wb, 10)
wb
################################################################################
#NOW, we will visualize the data

sorted_2019 <- wb[order(-wb$'2019'), ]
ggplot(sorted_2019, aes(x = reorder(`Country Name`, -`2019`), y = `2019`)) +
  geom_bar(stat = "identity", fill = "#2ca02c") +
  labs(title = "2019 Tourist Numbers (Ordered Decreasingly)",
       x = "Country",
       y = "Tourist Numbers") +
  scale_y_continuous(labels = label_number()) +  # Avoid scientific notation
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#To show the 2020 stats, ordered
sorted_2020 <- wb[order(-wb$'2020'), ]
ggplot(sorted_2020, aes(x = reorder(`Country Name`, -`2020`), y = `2020`)) +
  geom_bar(stat = "identity", fill = "#d62728") +
  labs(title = "2020 Tourist Numbers (Ordered Decreasingly)",
       x = "Country",
       y = "Tourist Numbers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#To show the difference, ordered with respect to the drop percentage.
wb_ordered <- wb[order(wb$PercentageChange), ]
wb_long_ordered <- gather(wb_ordered, key = "Year", value = "Tourist_Numbers", -`Country Name`)
wb_long_ordered$`Country Name` <- factor(wb_long_ordered$`Country Name`, levels = wb_ordered$`Country Name`)
ggplot(wb_long_ordered, aes(x = `Country Name`, y = Tourist_Numbers, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Tourist Numbers: 2019 vs 2020 (Ordered by Percentage Change)",
       x = "Country",
       y = "Tourist Numbers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(name = "Year", values = c("2019" = "#2ca02c", "2020" = "#d62728"), guide = guide_legend())


#To show the percentage change, ordered
wb$`Country Name` <- factor(wb$`Country Name`, levels = wb$`Country Name`[order(wb$PercentageChange, decreasing = FALSE)])
ggplot(wb, aes(x = `Country Name`, y = -PercentageChange)) +
  geom_bar(stat = "identity",  fill = "#ffa100") +
  labs(
    title = "Percentage Change by Country",
    x = "Country",
    y = "Percentage Change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

###############################

