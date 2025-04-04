
library(zoo)
library(tidyverse)
library(ggplot2)
library(infer)

data = read_csv("Documents/GitHub/Air-Quality-Analysis-Project/2. Correlations & Sensor Calibration/final_cleaned_data.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Month = format(Date, "%m"),
         Year = format(Date, "%Y"),
         Season = case_when(
           Month %in% c("12", "01", "02") ~ "Winter",
           Month %in% c("03", "04", "05") ~ "Spring",
           Month %in% c("06", "07", "08") ~ "Summer",
           Month %in% c("09", "10", "11") ~ "Fall"
           )
  )

# Define AQI breakpoints as a data frame
aqi_breakpoints = data.frame(
  Category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  APLo = c(0, 51, 101, 151, 201, 301),
  APHi = c(50, 100, 150, 200, 300, 500),
  CO_Lo = c(0, 4.4, 9.4, 12.4, 15.4, 30.4),
  CO_Hi = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4),
  NOx_Lo = c(0, 53, 100, 360, 649, 1249),
  NOx_Hi = c(53, 100, 360, 649, 1249, 2049),
  NO2_Lo = c(0, 53, 100, 360, 649, 1249),
  NO2_Hi = c(53, 100, 360, 649, 1249, 2049),
  C6H6_Lo = c(0, 3, 7, 10, 15, 20),
  C6H6_Hi = c(3, 7, 10, 15, 20, 30)
)


# Function to compute AQI for a given pollutant
compute_AQI = function(CP, BPLo, BPHi, APLo, APHi) {
  AQI = ((APHi - APLo) / (BPHi - BPLo)) * (CP - BPLo) + APLo
  return(AQI)
}

# Function to get AQI for a given pollutant
get_AQI = function(CP, pollutant) {
  for (i in 1:nrow(aqi_breakpoints)) {
    if (CP >= aqi_breakpoints[[paste0(pollutant, "_Lo")]][i] && CP < aqi_breakpoints[[paste0(pollutant, "_Hi")]][i]) {
      BPLo = aqi_breakpoints[[paste0(pollutant, "_Lo")]][i]
      BPHi = aqi_breakpoints[[paste0(pollutant, "_Hi")]][i]
      APLo = aqi_breakpoints$APLo[i]
      APHi = aqi_breakpoints$APHi[i]
      return(compute_AQI(CP, BPLo, BPHi, APLo, APHi))
    }
  }
  return(NA)  # Return NA if value is outside the defined range
}

calculate_rolling = function(x, window, FUN = mean) {
  zoo::rollapplyr(x, width = window, FUN = FUN, fill = NA, partial = TRUE)
}

categorize_AQI = function(AQI) {
  if (AQI <= 50) {
    return("Good")
  } else if (AQI <= 100) {
    return("Moderate")
  } else if (AQI <= 150) {
    return("Unhealthy for Sensitive Groups")
  } else if (AQI <= 200) {
    return("Unhealthy")
  } else if (AQI <= 300) {
    return("Very Unhealthy")
  } else {
    return("Hazardous")
  }
}

data = data %>%
  mutate(
    CO_8h_avg = calculate_rolling(CO.GT., 8),
    C6H6_24h_avg = calculate_rolling(C6H6.GT., 24)
  )

# Apply AQI function to each row in dataset
data = data %>%
  mutate(
    AQI_CO = sapply(CO_8h_avg, get_AQI, pollutant = "CO"),
    AQI_C6H6 = sapply(C6H6_24h_avg, get_AQI, pollutant = "C6H6"),
    AQI_NOx = sapply(NOx.GT., get_AQI, pollutant = "NOx"),
    AQI_NO2 = sapply(NO2.GT., get_AQI, pollutant = "NO2")
  ) %>%
  mutate(AQI = pmax(AQI_CO, AQI_C6H6, AQI_NOx, AQI_NO2, na.rm = TRUE),
         Dominant_Pollutant = case_when(
            AQI_CO == AQI  ~ "CO",
            AQI_C6H6 == AQI ~ "C6H6",
            AQI_NOx == AQI ~ "NOx",
            AQI_NO2 == AQI ~ "NO2",
            TRUE ~ "Other"
          ),
         Category = sapply(AQI, categorize_AQI)
  )

table(data$Category)
## ----------------------------------------------------------------------------------------------------------

daily_dominant = data %>%
  group_by(Date) %>%
  summarise(
    # Count hourly occurrences of each dominant pollutant
    CO_hours = sum(Dominant_Pollutant == "CO", na.rm = TRUE),
    C6H6_hours = sum(Dominant_Pollutant == "C6H6", na.rm = TRUE),
    NOx_hours = sum(Dominant_Pollutant == "NOx", na.rm = TRUE),
    NO2_hours = sum(Dominant_Pollutant == "NO2", na.rm = TRUE),
    Total_hours = sum(!is.na(Dominant_Pollutant)),  # Total valid hours
    
    # Determine daily dominant (mode)
    Dominant_Pollutant = names(which.max(c(
      CO = CO_hours,
      C6H6 = C6H6_hours,
      NOx = NOx_hours,
      NO2 = NO2_hours
    ))),
    
    # Calculate dominant percentage
    Dominant_Hours = max(CO_hours, C6H6_hours, NOx_hours, NO2_hours),
    Dominant_Pct = Dominant_Hours / Total_hours * 100,
    .groups = 'drop'
  )

## -----------------------------------------------------------------------------------------------------------

ggplot(daily_dominant, aes(x = Date, y = Dominant_Pct, color = Dominant_Pollutant)) +
  geom_point() +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_color_manual(values = c("CO" = "#377eb8", "C6H6" = "#4daf4a", 
                                "NOx" = "#ff7f00", "NO2" = "#e41a1c")) +
  labs(title = "Daily Dominant Pollutant Consistency",
       subtitle = "Percentage of hours the dominant pollutant was leading",
       y = "% of Hours as Dominant") +
  theme_minimal()
## -----------------------------------------------------------------------------------------------------------

aqi_trend = data %>%
  group_by(Date, Season, Year) %>%
  summarise(AQI_CO = mean(AQI_CO, na.rm=TRUE),
            AQI_C6H6 = mean(AQI_C6H6, na.rm=TRUE),
            AQI_NOx = mean(AQI_NOx, na.rm=TRUE),
            AQI_NO2 = mean(AQI_NO2, na.rm=TRUE),
            AQI = pmax(AQI_CO, AQI_C6H6, AQI_NOx, AQI_NO2, na.rm = TRUE)) %>%
  mutate(AQI_Category = sapply(AQI, categorize_AQI),
         Month_Range = case_when(
           Season == "Spring" & Year == 2004 ~ "Mar-May 2004", # Spring = Mar-May
           Season == "Spring" & Year == 2005 ~ "Mar-Apr 2005",
           Season == "Summer" ~ "Jun-Aug 2004",  # Summer = Jun-Aug
           Season == "Fall"   ~ "Sep-Nov 2004",  # Fall = Sep-Nov
           Season == "Winter" ~ "Dec-Feb 2004"    # Winter = Dec-Feb
         )) %>%
  left_join(daily_dominant, by = "Date") %>%
  select(Date, Season, Month_Range, Year, 
         AQI_CO, AQI_C6H6, AQI_NOx, AQI_NO2, AQI, AQI_Category,
         Dominant_Pollutant, Dominant_Hours)
## -----------------------------------------------------------------------------------------------------------
ggplot(aqi_trend, aes(x = Date, y = AQI)) +
  # Main trend line
  geom_line(color = "blue") +
  
  # Category threshold lines (using APLo from your breakpoints)
  geom_hline(
    data = aqi_breakpoints,
    aes(
      yintercept = APLo,
      color = factor(Category, 
                     levels = c("Good", "Moderate", 
                                "Unhealthy for Sensitive Groups",
                                "Unhealthy", "Very Unhealthy", 
                                "Hazardous")),
      linetype = factor(Category, 
                        levels = c("Good", "Moderate",
                                   "Unhealthy for Sensitive Groups",
                                   "Unhealthy", "Very Unhealthy",
                                   "Hazardous"))
    ),
    alpha = 0.7,
    linewidth = 0.5
  ) +
  
  # EPA color scheme with ordered levels
  scale_color_manual(
    name = "AQI Category Thresholds",
    values = c(
      "Good" = "#00E400",
      "Moderate" = "#FFFF00",
      "Unhealthy for Sensitive Groups" = "#FF7E00",
      "Unhealthy" = "#FF0000",
      "Very Unhealthy" = "#8F3F97",
      "Hazardous" = "#7E0023"
    ),
    breaks = c("Good", "Moderate", 
               "Unhealthy for Sensitive Groups",
               "Unhealthy", "Very Unhealthy",
               "Hazardous")  # Explicit order for legend
  ) +
  
  # Custom linetype scale to match
  scale_linetype_manual(
    name = "AQI Category Thresholds",
    values = rep("dashed", 6),  
    breaks = c("Good", "Moderate",
               "Unhealthy for Sensitive Groups",
               "Unhealthy", "Very Unhealthy",
               "Hazardous")
  ) +
  
  # Labels and theme
  labs(
    title = "AQI Trend with Category Thresholds",
    x = "Date",
    y = "AQI",
    caption = "Horizontal lines show lower bounds of each AQI category"
  ) +
  scale_y_continuous(
    breaks = seq(0, 350, by = 50),
    limits = c(0, 350)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(1.5, "cm")  # Wider legend keys for lines
  ) +
  guides(
    color = guide_legend(order = 1),  # Ensures color legend comes first
    linetype = guide_legend(order = 1)  # Matches color legend
  )

## -----------------------------------------------------------------------------------------------------------
# Define exact seasonal periods (covering full dataset range)
season_periods <- data.frame(
  start = as.Date(c("2004-03-10", "2004-06-01", "2004-09-01", "2004-12-01", 
                    "2005-03-01")),
  end = as.Date(c("2004-05-31", "2004-08-31", "2004-11-30", "2005-02-28", 
                  "2005-04-04")),
  season = c("Spring", "Summer", "Fall", "Winter", "Spring")  
)

# Create the plot
ggplot(aqi_trend) +
  # 1. Add seasonal background shading
  geom_rect(
    data = season_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
    alpha = 0.1
  ) +
  
  # 2. AQI trend lines
  geom_segment(
    aes(x = Date, xend = lead(Date),
        y = AQI, yend = lead(AQI),
        color = Dominant_Pollutant)
  ) +
  
  # 3. Vertical lines for season transitions
  geom_vline(
    data = season_periods[-1, ],  # Exclude first start date
    aes(xintercept = start),
    linetype = "dashed",
    color = "gray30", 
    size = 0.3,
    alpha = 0.5
  ) +
  
  # 4. Season labels
  geom_text(
    data = season_periods,
    aes(x = start + (end - start)/2 + 3,
        y = max(aqi_trend$AQI, na.rm = TRUE) * 1.05,
        label = season),
    size = 3.5, 
    fontface = "bold",
    color = "black",
    show.legend = FALSE
  ) +
  
  # 5. Color scales
  scale_fill_manual(
    values = c("Spring" = "#66c2a5", 
               "Summer" = "#fc8d62",
               "Fall" = "#8da0cb",
               "Winter" = "#e78ac3"),
    guide = "none"  # Hide fill legend
  ) +
  scale_color_manual(
    values = c("CO" = "#377eb8", 
               "C6H6" = "#4daf4a",
               "NOx" = "#ff7f00", 
               "NO2" = "#e41a1c"),
    name = "Dominant Pollutant"
  ) +
  
  # Adjust date axis
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    limits = c(min(aqi_trend$Date), max(aqi_trend$Date))
  ) +
  
  # Labels
  labs(
    title = "AQI Trend March 2004 - April 2005",
    subtitle = "Colored by Dominant Pollutant and with Seasonal Transitions Vertical lines",
    x = "Date",
    y = "AQI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
## -----------------------------------------------------------------------------------------------------------

seasonal_dominance = table(aqi_trend$Month_Range, aqi_trend$Dominant_Pollutant)
row_percentages = round(prop.table(seasonal_dominance, margin = 1) * 100, 2)
row.names(row_percentages) = c("Spring 2004", "Summer 2004", "Fall 2004", "Winter 2004", "Spring 2005")
row_percentages
## -----------------------------------------------------------------------------------------------------------
table(aqi_trend$AQI_Category, aqi_trend$Month_Range)

aqi_trend$AQI_Category = factor(aqi_trend$AQI_Category, 
                                levels = rev(aqi_breakpoints$Category), 
                                ordered = TRUE)
aqi_trend$Month_Range = factor(aqi_trend$Month_Range,
                                levels = c("Mar-May 2004", "Jun-Aug 2004", "Sep-Nov 2004", "Dec-Feb 2004", "Mar-Apr 2005")
                                )

ggplot(aqi_trend, aes(x = Month_Range, fill = AQI_Category)) +
  geom_bar(position = "fill") + # For proportions
  # geom_bar() # For counts
  scale_fill_brewer(palette = "YlOrRd", direction = -1) + # Color matches AQI severity
  labs(title = "AQI Category Distribution by Season",
       y = "Proportion",
       x = "Season (Month Range)",
       fill = "AQI Category") +
  theme_minimal()
