#import needed libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#import the data
oecd <- read_excel("data/OECD.xls", skip = 4)
head(oecd)


#select the columns of interest and rename them
oecd <- oecd[, c(1,3,4, (ncol(oecd) - 2):ncol(oecd))]
colnames(oecd)[3] <- "Subvariable"
colnames(oecd)[4] <- "2019"
colnames(oecd)[5] <- "2020"
colnames(oecd)[6] <- "2021"

#fill the missing rows
oecd <- oecd |>
  fill(Country,Variable)

#select the observations of interest
oecd_tr <- oecd |>
  filter(Country == "Türkiye")

#we can delete the country column since we filtered all countries
oecd_tr <- subset(oecd, select = -Country)


#to only keep the countries that has 2021 data
oecd <- oecd |>
  filter(`2021` != "..")



#Analysis of Turkey#############################################################

filtered_data <- oecd |>
  filter(Country == "Türkiye" & Variable == "Total international arrivals") %>%
  select(Country, Variable, `2019`, `2020`)

# Reshape the data using pivot_longer
reshaped_data <- filtered_data %>%
  pivot_longer(cols = c(`2019`, `2020`), names_to = "Year", values_to = "Value")

# Convert 'Value' column to numeric
reshaped_data$Value <- as.numeric(reshaped_data$Value)

#Graph 1: 2019 vs 2020 
ggplot(reshaped_data, aes(x = Year, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Comparison of Total International Arrivals in 2019 and 2020 for Türkiye",
       x = "Year", y = "Value") +
  scale_fill_manual(values = c("#006400", "#FF2400"), labels = c("2019", "2020")) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))


filtered_data <- oecd |>
  filter(Country == "Türkiye" & Variable == "Total international arrivals") %>%
  select(Country, Variable, `2020`, `2021`)

# Reshape the data using pivot_longer
reshaped_data <- filtered_data %>%
  pivot_longer(cols = c(`2020`, `2021`), names_to = "Year", values_to = "Value")

# Convert 'Value' column to numeric
reshaped_data$Value <- as.numeric(reshaped_data$Value)


#Graph 2: 
ggplot(reshaped_data, aes(x = Year, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Comparison of Total International Arrivals in 2020 and 2021 for Türkiye",
       x = "Year", y = "Value") +
  scale_fill_manual(values = c( "#FF2400","#006400"), labels = c("2020", "2021")) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))


filtered_data <- oecd |>
  filter(Country == "Türkiye" & Variable == "Total international arrivals") %>%
  select(Country, Variable,`2019`, `2020`, `2021`)

# Convert columns to numeric
filtered_data$`2019` <- as.numeric(as.character(filtered_data$`2019`))
filtered_data$`2020` <- as.numeric(as.character(filtered_data$`2020`))
filtered_data$`2021` <- as.numeric(as.character(filtered_data$`2021`))

# Calculate differences between 2019 and 2020, and 2020 and 2021
diff_19_20 <- filtered_data$`2020` - filtered_data$`2019`
diff_20_21 <- filtered_data$`2021` - filtered_data$`2020`

# Create a data frame for differences
differences <- data.frame(
  Year = c("2019 to 2020 (Loss)", "2020 to 2021 (Gain)"),
  Difference = c(abs(diff_19_20), abs(diff_20_21))  # Take absolute values
)

# Plotting the absolute differences in a bar graph with adjusted y-axis scale
ggplot(differences, aes(x = Year, y = Difference, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Absolute Difference in Total International Arrivals between Years",
    x = "Year Comparison", y = "Absolute Difference"
  ) +
  scale_fill_manual(values = c("#FF2400", "#006400")) +
  theme_minimal() +
  geom_text(aes(label = Difference), vjust = ifelse(differences$Difference > 0, -0.5, 0.5), size = 4) +
  scale_y_continuous(limits = c(0, 40000000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))

rec_per <- abs(diff_20_21) / abs(diff_19_20)
cat("Turkiye recovered", rec_per * 100, "% of the tourists it lost")

################################################################################
oecd_int_arr = oecd |>
  filter(Variable == "Total international arrivals") |>
  select(Country, `2019`, `2020`, `2021`) |>
  mutate(Diff = as.numeric(`2021`) - as.numeric(`2020`))

oecd_int_arr <- oecd_int_arr |>
  distinct(Country, .keep_all = TRUE)
oecd_int_arr <- oecd_int_arr[-nrow(oecd_int_arr), ]


oecd_diff = oecd_int_arr |>
  select(Country, Diff)


#Plot Difference
ggplot(oecd_int_arr, aes(x = reorder(Country, Diff), y = Diff, fill = Diff < 0)) +
  geom_bar(stat = "identity", 
           aes(y = ifelse(Diff < 0, -Diff, Diff)),  # Negate negative values for correct orientation
           position = "identity", width = 0.7) +
  scale_fill_manual(values = c("#006400", "#FF2400"), guide = FALSE) +
  labs(title = "Diff Value for Each Country", x = "Country", y = "Diff") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 1))  # Adjust y-axis labels to display without scientific notation
    
oecd_neg <- oecd_int_arr |>
  filter(Diff < 0)


# Convert columns '2019', '2020', and '2021' to numeric if they are not already numeric
oecd_neg$`2019` <- as.numeric(oecd_neg$`2019`)
oecd_neg$`2020` <- as.numeric(oecd_neg$`2020`)
oecd_neg$`2021` <- as.numeric(oecd_neg$`2021`)

# Calculate the differences
oecd_neg$Diff_2019_2020 <- abs(oecd_neg$`2019` - oecd_neg$`2020`)
oecd_neg$Diff_2020_2021 <- abs(oecd_neg$`2020` - oecd_neg$`2021`)

# Stack Bar Graph for Negative diffrence
ggplot(oecd_neg, aes(x = reorder(Country, Diff_2019_2020), y = Diff_2019_2020)) +
  geom_bar(aes(fill = "2019-2020"), stat = "identity", position = "stack") +  # Bar for (2019-2020) differences
  geom_bar(aes(y = Diff_2020_2021, fill = "2020-2021"), stat = "identity", position = "stack") +  # Stacked bar for (2020-2021) differences
  scale_fill_manual(values = c("2019-2020" = "grey", "2020-2021" = "red"), name = "Difference") +
  labs(title = "Stacked Bar: Differences between 2019-2020 and 2020-2021 for Each Country", x = "Country", y = "Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  # Format y-axis without scientific notation


# Calculate the differences between 2019-2020 and 2020-2021 for each country
oecd_neg <- oecd_neg %>%
  mutate(Diff_2019_2020 = `2019` - `2020`,
         Diff_2020_2021 = `2020` - `2021`)


# Calculate the percentage of red compared to grey for each country
percentages <- oecd_neg %>%
  mutate(Percentage = (Diff_2020_2021 / (Diff_2019_2020 + Diff_2020_2021)) * 100) %>%
  select(Country, Percentage)

print(percentages)


oecd_pos <- oecd_int_arr |>
  filter(Diff > 0)


oecd_pos$'2019' <- as.numeric(oecd_pos$'2019')
oecd_pos$'2021' <- as.numeric(oecd_pos$'2021')

# Assuming 'oecd_pos' is your dataset with columns 'Country', '2019', and '2021'

# Create a stacked bar plot for values of 2019 and 2021 for each country in 'oecd_pos'
ggplot(oecd_pos, aes(x = reorder(Country, `2019`), y = `2019`)) +
  geom_bar(aes(fill = "2019"), stat = "identity") +  # Bar for values of 2019
  geom_bar(aes(y = `2021`, fill = "2021"), stat = "identity") +  # Stacked bar for values of 2021
  scale_fill_manual(values = c("2019" = "grey", "2021" = "#006400"), name = "Values") +  # Green color (#006400) for 2021 values
  labs(title = "Stacked Bar: Values of 2019 (Grey) and 2021 (Green) for Each Country",
       x = "Country", y = "Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  # Format y-axis without scientific notation



# Calculate the total values for '2019' and '2021' for each country
oecd_pos_totals <- oecd_pos %>%
  group_by(Country) %>%
  summarise(total_2019 = sum(`2019`), total_2021 = sum(`2021`))

# Calculate the percentage of '2021' values concerning '2019' values
oecd_pos_percentages <- oecd_pos_totals %>%
  mutate(percentage_2021_2019 = (total_2021 / total_2019) * 100)

# Print the calculated percentages alongside the countries
print(oecd_pos_percentages)


