library(readxl)
library(tidyverse)

df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx")


df1 <- df |>
             select(`Health Board / Trust`, contains("202")) |>
  
               rename("February 2023"= "Monthly Progress Score - February 2023 (MSSW only)",
                   "March 2023" = "Monthly Progress Score - March 2023 (MSSW ONLY)",
                   "April 2023" = "Monthly Progress Score - April 2023",
                   "May 2023" = "Monthly Progress Score - May 2023",
                   "June 2023" = "Monthly Progress Score - June 2023",
                   "July 2023" = "Monthly Progress Score - July 2023",
                   "August 2023" = "Monthly Progress Score - August 2023",
                   "September 2023" = "Monthly Progress Score - September 2023",
                   "October 2023" = "Monthly Progress Score - October 2023 (Due 21/11/23)",
                   "November 2023" = "Monthly Progress Score - November 2023 (Due 13th Dec)",
                   "December 2023" = "Monthly Progress Score - December 2023 (due 17th January)",
                   "January 2024" = "Monthly Progress Score - January 2024 (due 13th Feb 2024)",
                   "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)") |>
  
              mutate(across(everything(), ~if_else(. == "" | . == "NO Report received" | . == "No report received", NA, .))) |>
  
              mutate(
                  across(
                    .cols = matches("202"),
                    .fns = ~parse_number(.)
                  )
              ) |>
  
                filter(!if_all(everything(), is.na))

month_order <- c("January 2023","February 2023", "March 2023", "April 2023", "May 2023", 
                 "June 2023", "July 2023", "August 2023", "September 2023", 
                 "October 2023", "November 2023", "December 2023", 
                 "January 2024")


df2 <- df1 |>
  pivot_longer(cols = contains("202"),
               names_to = "month", 
               values_to = "score") |>
  mutate(month = factor(month, levels = month_order)) |>
  group_by(`Health Board / Trust`, month) |>
  summarise(count_sub = sum(!is.na(score)),
            median_score = median(score, na.rm = TRUE)) |>
  arrange(`Health Board / Trust`, month)


            

at <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/Contact List/SCC Master Distribution List .xlsx")

at1 <- at |>
  select(Organisation, Workstream, 21:40) |>
  filter(!(Organisation %in% c("Public Health Wales", "IHI", "RSM", "NHS Wales Executive", "Improvement Cymru", "NHS Executive", "HEIW", "Assitant Director of Quailty and Nursing"))) |>
  mutate(across(3:22, ~ifelse(grepl("^[Yy]", .) | grepl("^[Yy]$", .), "Y", NA)),
         `Learning Session 4(November 2023)` = ifelse(!is.na(`LS4 Day 2`) | !is.na(`LS4 Cl Ex`) | !is.na(`LS4 Day 1 (AB, CTM, HD)`), "Y", NA),
         `Learning Session 3(September 2023)` = ifelse(!is.na(`LS3`) | !is.na(`LS3 Cl Ex`), "Y", NA),
         Organisation = case_when(
           Organisation %in% c("Aneurin Bevan UHB Monmouthshire Gov", "Monmouthshire Gov", "Aneurin Bevan UHB") ~ "Aneurin Bevan University Health Board",
           Organisation %in% c("BCUHB/FCC/WCBC/Flinshire CBC","Betsi Cadwaladr UHB")  ~ "Betsi Cadwaladr University Health Board",
           Organisation %in% c("Cardiff & Vale UHB & Acute Response Team","Cardiff & Vale UHB")  ~ "Cardiff & Vale University Health Board",
           Organisation %in% c("Swansea Council","Swansea Bay UHB") ~ "Swansea Bay University Health Board",
           Organisation %in% c("NHS Velindre Trust - Welsh Blood Service", "Velindre University NHS Trust", "Velindre") ~ "Velindre NHS Trust",
           Organisation == "Welsh Ambulance Service NHS Trust" ~ "Welsh Ambulance Service Trust",
           Organisation == "Hywel Dda UHB" ~ "Hywel Dda University Health Board",
           Organisation ==  "Powys THB" ~ "Powys Teaching Health Board",
           Organisation == "Cwm Taf Morgannwg UHB" ~ "Cwm Taf Morgannwg University Health Board",
           TRUE ~ Organisation
         )) |>
  rename(
    `Lead Coaching Call(January 2024)` = `Jan Lead CC`,
    `Coaching Call(January 2024)` = `Jan CC 24`,
    `Lead Coaching Call(October 2023)` = `Oct Lead CC`,
    `Coaching Call(October 2023)` = `Oct CC`,
    `Lead Coaching Call(August 2023)` = `Aug Lead CC`,
    `Coaching Call(August 2023)` = `Aug CC`,
    `Lead Coaching Call(July 2023)` = `July Lead CC`,
    `Coaching Call(July 2023)` = `11`,
    `Learning Session 2(June 2023)` = `LS2`,
    `Coaching Call(May 2023)` = `May CC`,
    `Coaching Call(April 2023)` = `Apr CC`,
    `Learning Session 1.2(March 2023)` = `LS 1.2`,
    `Coaching Call(February 2023)` = `Feb CC`,
    `Coaching Call(January 2023)` = `Jan CC`,
    `Health Board / Trust` = `Organisation`
  ) |>
  select(-c("LS4 Day 2", "LS4 Cl Ex", "LS4 Day 1 (AB, CTM, HD)","LS3", "LS3 Cl Ex"))

session_order <- c("Coaching Call(January 2023)", "Coaching Call(February 2023)", "Learning Session 1.2(March 2023)", "Coaching Call(April 2023)", "Coaching Call(May 2023)" , "Learning Session 2(June 2023)",
                   "Lead Coaching Call(July 2023)", "Coaching Call(July 2023)" , "Lead Coaching Call(August 2023)","Coaching Call(August 2023)" , "Learning Session 3(September 2023)" ,
                   "Coaching Call(October 2023)","Lead Coaching Call(October 2023)","Learning Session 4(November 2023)","Lead Coaching Call(January 2024)", "Coaching Call(January 2024)")
          
calc_count <- function(session_name) {
  at1 |>
    select(`Health Board / Trust`, matches(session_name)) |>
    filter(!is.na(`Health Board / Trust`)) |> # Drop rows with NA in Organisation
    pivot_longer(
      cols = matches(session_name),
      names_to = "session",
      values_to = "attendance"
    ) |>
    mutate(session = factor(session, levels = session_order)) |>
    group_by(`Health Board / Trust`, session) |>
    summarise(count = sum(attendance == "Y", na.rm = TRUE))|>
    mutate(month = str_extract(session, "\\(([^)]+)")) |>
    mutate(month = str_remove_all(month, "[()]")) 
} 

cc <- calc_cumulative("^C")
cc_lead <- calc_cumulative("^Lead")
ls <- calc_cumulative("^Lear")


cc <- cc |> rename(`count_cc` = `count`) |> select(-session)
cc_lead <- cc_lead |> rename(`count_cclead` = `count`) |> select(-session) 
ls <- ls |> rename(`count_ls` = `count`) |> select(-session)


coaching_call <- full_join(cc,df2,by = c( "month","Health Board / Trust"))

coac_lead <- full_join(coaching_call, cc_lead, by = c("month", "Health Board / Trust"))

final_df <- full_join( coac_lead, ls,by = c("month", "Health Board / Trust"))

months <- c("January 2023","February 2023", "March 2023", "April 2023", "May 2023", "June 2023", "July 2023", "August 2023",
            "September 2023", "October 2023", "November 2023","December 2023","January 2024")

finale_df <- final_df |> 
            mutate(
              month = factor(month, levels = months, ordered = TRUE)) |>
            select(
              `Health Board / Trust`, month, count_sub, median_score, count_cc, count_cclead, count_ls)|>
            mutate(
              across(c(starts_with("c"), "median_score"), ~if_else(.x == 0, NA_real_, .x))
            ) |>
            arrange(
              `Health Board / Trust`, month)

################## COMBO OF BAR GRAPH AND LINE GRAPH #############################

fin <- finale_df |> filter(`Health Board / Trust` == "Aneurin Bevan University Health Board")

# Transform data for cumulative values and median_score
finale_df_long <- fin %>%
  pivot_longer(cols = c(count_cc, count_cclead, count_ls),
               names_to = "category", 
               values_to = "value") %>%
  filter(!is.na(value))


# Update category levels with more descriptive names
finale_df_long$category <- factor(finale_df_long$category,
                                  levels = c("count_cc", "count_cclead", "count_ls"),
                                  labels = c("Coaching Call", "Coaching Call (Leadership)", "Learning Session"))

# Adjust the plot
latest <- ggplot(data = fin, aes(x = month)) +
  geom_bar(data = filter(finale_df_long, category != "median_score"),
           aes(x = month, y = value / max(finale_df_long$value) * 6, fill = category), # Scale cumulatives
           stat = "identity", position = "stack") +
  geom_line(aes(y = count_sub, group = 1), color = "#4A7986", size = 1) +
  geom_point(aes(y = count_sub, group = 1), shape = 21, size = 4, fill = "white", color = "#4A7986", stroke = 0.5) +
  scale_y_continuous(name = "Median Score", 
                     sec.axis = sec_axis(~ . * max(finale_df_long$value) / 6, name = "Count"),
                     expand = c(0, 0)) + # Remove padding to ensure bars rest on the x-axis line
  scale_x_discrete(limits = months) +
  scale_fill_manual(values = c("Coaching Call" = "#FFE18A", 
                               "Coaching Call (Leadership)" = "#BCB1C7", 
                               "Learning Session" = "#99D7D8")) +
  labs(title = "Number of submissions v/s number of attendees in coaching calls and learning sessions for Aneurin Bevan UHB ", fill = "Category")+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
    axis.text.y = element_text(color = "darkgray"),
    axis.ticks.x = element_line(color = "gray"),
    axis.ticks.length = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(family = "Arial", size = 12, color = "#1b5768", hjust = 0.5),
    axis.line.x = element_line(color = "darkgray", size = 0.5),
    legend.position = "bottom"
  )

latest <- latest + 
  theme(plot.margin = unit(c(0.5, 1, 2, 1), "cm"), # Top, Right, Bottom, Left margins
        legend.position = c(0,-0.38), # X, Y coordinates for legend position (0,0 is bottom-left; 1,1 is top-right)
        legend.justification = c(0, 0), # Anchor point for legend position
        legend.box.margin = margin(0, 0, 0, 0), # Adjust if more space is needed
        legend.background = element_blank(), # Optional: Adjust legend background
        legend.title = element_blank(), # Hide legend title
        legend.key = element_blank()) # Optional: Adjust legend key background

print(latest)


# Export the plot to a PNG file with 300 dpi
CairoPNG("plot_bar_latest_count.png", width = 10, height = 7, units = "in", dpi = 300)
print(latest) # Make sure to print the plot
dev.off() # Close the device

############## BOXPLOTS PER ORGANISATION ##########################

bp_df <- df1 |>
          pivot_longer(
            cols = contains("202"),
            names_to = "month", 
            values_to = "score"
          ) |>
          mutate(
            month = factor(month, levels = months, ordered = TRUE)
          ) |>
          filter(
            !is.na(month)
          )|>
          arrange(
            `Health Board / Trust`, month
          )
          


# Plotting with dynamic titles for each Health Board / Trust
bp <- ggplot(bp_df, aes(x = month, y = score)) +
  geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5), limits = c(0, 5)) +
  labs(caption = "Source: SCC National Planning File") +
  facet_wrap(~`Health Board / Trust`, scales = "free_y", labeller = labeller(`Health Board / Trust` = function(x) paste("Box Plot of Monthly Progress Scores for", x))) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
    plot.caption = element_text(size = 7, hjust = 1, color = "gray"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
    axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(color = "gray"),
    axis.line.y = element_line(color = "gray"),
    axis.ticks.x = element_line(color = "gray"),
    axis.ticks.y = element_line(color = "gray"),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.margin = unit(0.2, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.caption.position = "plot"
  ) 

# Print the plot
print(bp)


CairoPNG("box_plot.png", width = 25, height = 15, units = "in", dpi = 300)
print(bp) # Make sure to print the plot
dev.off() 


# Assuming df1 is your dataframe and it has been prepared up to the point of arranging

unique_boards <- unique(df1$`Health Board / Trust`)

plot_list_bp <- list()
# Iterate over each unique Health Board / Trust to create individual plots
for(board in unique_boards) {
  # Filter the dataframe for the current board
  board_df <- df1 %>%
    filter(`Health Board / Trust` == board) %>%
    pivot_longer(
      cols = contains("202"),
      names_to = "month",
      values_to = "score"
    ) %>%
    mutate(
      month = factor(month, levels = months, ordered = TRUE)
    ) %>%
    filter(!is.na(month)) %>%
    arrange(month)
  

  # Plotting
  bp <- ggplot(board_df, aes(x = month, y = score)) +
    geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0,5)) +
    labs(
      title = paste("Box Plot of Monthly Progress Scores for", board),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
      plot.caption = element_text(size = 7, hjust = 1, color = "gray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(color = "gray"),
      axis.line.y = element_line(color = "gray"),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_line(color = "gray"),
      axis.ticks.length = unit(0.1, "cm"),
      axis.ticks.margin = unit(0.2, "cm"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption.position = "plot"
    )
  
  # Print the plot
  print(bp)
  plot_list_bp[[board]] <- bp
}



############# LINE GRAPH OF NUMBER OF SCORES SUBMITTED ###################################

# Assuming finale_df is your dataframe and it contains a `Health Board / Trust` column

unique_boards <- unique(finale_df$`Health Board / Trust`)

# Placeholder for storing plots if needed
plot_list <- list()
for(board in unique_boards) {
  num_scores <- finale_df |>
    filter(!is.na(month), `Health Board / Trust` == board, month != "January 2023")
  
  # Determine the maximum value of count_sub for the current board and add 2
  max_count_sub <- max(num_scores$count_sub, na.rm = TRUE) + 2
  
  # Create the line graph for the current health board
  num <- ggplot(num_scores, aes(x = month, y = count_sub, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, max_count_sub, by = 1), limits = c(0, max_count_sub)) +
    labs(
      title = paste("Number of submissions per Month by", board),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 1, color = "darkgray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Print the line graph
  print(num)
  
  # Optionally, store the plot in a list if you want to access it later
  plot_list[[board]] <- num
}


############# BAR GRAPH OF ATTENDANCE #################################

bg <- finale_df |>
        select(-count_sub, -median_score) |>
          pivot_longer(
            cols = starts_with("c"),
              names_to = "session",
                values_to = "number"
          )


# Unique Health Board / Trust values for iteration
unique_boards <- unique(finale_df$`Health Board / Trust`)

library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming bg is your prepared dataframe and unique_boards has been defined

# Placeholder for storing plots, if needed
plot_list_att <- list()

for(board in unique_boards) {
  # Filter data for the current board
  board_data <- bg %>%
    filter(`Health Board / Trust` == board, !is.na(month), month != "December 2023")
  
  # Determine dynamic y-axis limits and breaks
  max_count <- max(board_data$number, na.rm = TRUE) + 2
  y_breaks <- seq(0, max_count, by = max(1, floor((max_count - 0) / 5))) # Adjust the divisor for different granularity
  
  # Plot
  att <- ggplot(board_data, aes(x = month, y = number, fill = session)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("count_cc" = "#FFE18A", "count_cclead" = "#BCB1C7", "count_ls" = "#99D7D8"),
                      labels = c("Coaching Call", "Coaching Call (Leadership)", "Learning Session")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_count), breaks = y_breaks) +
    labs(title = paste("Number of attendees in Learning Session and Coaching Call for", board),
         x = "Month",
         y = "",
         fill = "Session Type") +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Arial", size = 14, hjust = 0.5, color = "#4A7986"),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "darkgray"),
      axis.text.y = element_text(colour = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0, -0.42),
      legend.justification = c(0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "darkgray"),
      plot.margin = unit(c(0.5, 1, 2, 1), "cm"),
      axis.ticks.x = element_line(colour = "gray"),
      axis.ticks.length = unit(0.1, "cm"),
      axis.ticks.margin = unit(0.2, "cm")
      
    )
  
  # Print the plot
  print(att)
  
  # Optionally, store the plot in a list if you want to access it later
  plot_list_att[[board]] <- att
}



############### LINE GRAPH OF DIFFERENCE OF SCORES ###################################

diff <- finale_df |>
          select(
            `Health Board / Trust`, month, median_score) |>
            filter(!is.na(month), month != "January 2023")

# Preprocess to fill NA median_scores with the next available non-NA values within each group
df_diff <- diff %>%
  group_by(`Health Board / Trust`) %>%
  mutate(filled_median_score = na.locf(median_score, na.rm = FALSE, fromLast = TRUE)) %>%
  ungroup()

# Calculate the difference in filled_median_score for each Health Board / Trust, using next available non-NA value
df_diff <- df_diff %>%
  group_by(`Health Board / Trust`) %>%
  mutate(difference = lead(filled_median_score, default = last(filled_median_score)) - filled_median_score) %>%
  ungroup()


library(dplyr)
library(lubridate)

# Assuming your dataframe is named 'diff'
# Ensure 'month' is in date format if it's not already

diff <- diff %>%
       arrange(`Health Board / Trust`, month) %>%
      group_by(`Health Board / Trust`) %>%
       mutate(last_observed_median = na.locf(median_score, na.rm = FALSE),
                         difference = median_score - lag(last_observed_median, default = NA_real_))
 
   # Cleaning up intermediate column if not needed
   diff <- diff %>%
       select(-last_observed_median)
 View(diff)




unique_boards <- unique(finale_df$`Health Board / Trust`)

# Placeholder for storing plots if needed
plot_list_diff <- list()
for(board in unique_boards) {
  new_diff <- df_diff %>%
    filter(`Health Board / Trust` == board, month != "January 2023")
  
  # Determine the range for y-axis to ensure it includes 0
  y_range <- range(new_diff$difference, na.rm = TRUE)
  y_range[1] <- min(0, y_range[1])  # Ensure 0 is included as the lower limit if not already
  
  # Create the line graph for the current health board
  plot_diff <- ggplot(new_diff, aes(x = month, y = difference, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  # Add horizontal line at y=0
    scale_x_discrete() +
    scale_y_continuous(limits = y_range) +  # Ensure y-axis always includes 0
    labs(
      title = paste("Difference in median of monthly progress scores", board),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 1, color = "darkgray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Print the line graph
  print(plot_diff)
  
  # Optionally, store the plot in a list if you want to access it later
  plot_list_diff[[board]] <- plot_diff
}





################# ATTEMPTS TO TRY AND FIT GRAPHS #######################
install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(gridExtra)

# Assuming you have already created your dataframes and plots

# Set up PDF output
pdf("Organizations_Graphs.pdf", width = 11, height = 8.5)

unique_boards <- unique(finale_df$`Health Board / Trust`)

for(board in unique_boards) {
  # Create your plots for each board here
#############1 
  new_diff <- df_diff %>%
    filter(`Health Board / Trust` == board, month != "January 2023")
  
  # Determine the range for y-axis to ensure it includes 0
  y_range <- range(new_diff$difference, na.rm = TRUE)
  y_range[1] <- min(0, y_range[1])  # Ensure 0 is included as the lower limit if not already
  
  # Create the line graph for the current health board
  plot_diff <- ggplot(new_diff, aes(x = month, y = difference, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  # Add horizontal line at y=0
    scale_x_discrete() +
    scale_y_continuous(limits = y_range) +  # Ensure y-axis always includes 0
    labs(
      title = paste("Difference in median of monthly progress scores", board),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 1, color = "darkgray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
  #############2
  
  
  # Filter data for the current board
  board_data <- bg %>%
    filter(`Health Board / Trust` == board, !is.na(month), month != "December 2023")
  
  # Determine dynamic y-axis limits and breaks
  max_count <- max(board_data$number, na.rm = TRUE) + 2
  y_breaks <- seq(0, max_count, by = max(1, floor((max_count - 0) / 5))) # Adjust the divisor for different granularity
  
  # Plot
  att <- ggplot(board_data, aes(x = month, y = number, fill = session)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("count_cc" = "#FFE18A", "count_cclead" = "#BCB1C7", "count_ls" = "#99D7D8"),
                      labels = c("Coaching Call", "Coaching Call (Leadership)", "Learning Session")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_count), breaks = y_breaks) +
    labs(title = paste("Number of attendees in Learning Session and Coaching Call for", board),
         x = "Month",
         y = "",
         fill = "Session Type") +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Arial", size = 14, hjust = 0.5, color = "#4A7986"),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "darkgray"),
      axis.text.y = element_text(colour = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0, -0.42),
      legend.justification = c(0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "darkgray"),
      plot.margin = unit(c(0.5, 1, 2, 1), "cm"),
      axis.ticks.x = element_line(colour = "gray"),
      axis.ticks.length = unit(0.1, "cm"),
      axis.ticks.margin = unit(0.2, "cm")
      
    )
  
  ######################3
  for(board in unique_boards) {
    num_scores <- finale_df |>
      filter(!is.na(month), `Health Board / Trust` == board, month != "January 2023")
    
    # Determine the maximum value of count_sub for the current board and add 2
    max_count_sub <- max(num_scores$count_sub, na.rm = TRUE) + 2
    
    # Create the line graph for the current health board
    num <- ggplot(num_scores, aes(x = month, y = count_sub, group = 1)) +
      geom_line(color = "#4A7986", size = 1) +
      geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
      scale_x_discrete() +
      scale_y_continuous(breaks = seq(0, max_count_sub, by = 1), limits = c(0, max_count_sub)) +
      labs(
        title = paste("Number of submissions per Month by", board),
        caption = "Source: SCC National Planning File"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, hjust = 1, color = "darkgray"),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
        axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour = "gray"),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(color = "gray"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)
      )
  
########################4
    # Filter the dataframe for the current board
    board_df <- df1 %>%
      filter(`Health Board / Trust` == board) %>%
      pivot_longer(
        cols = contains("202"),
        names_to = "month",
        values_to = "score"
      ) %>%
      mutate(
        month = factor(month, levels = months, ordered = TRUE)
      ) %>%
      filter(!is.na(month)) %>%
      arrange(month)
    
    
    # Plotting
    bp <- ggplot(board_df, aes(x = month, y = score)) +
      geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
      scale_x_discrete() +
      scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0,5)) +
      labs(
        title = paste("Box Plot of Monthly Progress Scores for", board),
        caption = "Source: SCC National Planning File"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
        plot.caption = element_text(size = 7, hjust = 1, color = "gray"),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
        axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.ticks.x = element_line(color = "gray"),
        axis.ticks.y = element_line(color = "gray"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks.margin = unit(0.2, "cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.caption.position = "plot"
      )
    
  
  # Combine plots
  combined_plots <- plot_diff + att + num + bp +
    plot_layout(ncol = 2, nrow = 2) # Adjust layout as needed
  
  # Print combined plots to the current page of the PDF
  print(combined_plots)
}

# Close the PDF device
dev.off()


################# MEDIAN OF DIFFERENCES PER PROJECT PER ORGANISATION ####################################



df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx")


df1_diff_med <- df |>
  select(`Health Board / Trust`,`Unique ID`  , contains("202")) |>
  
  rename("February 2023"= "Monthly Progress Score - February 2023 (MSSW only)",
         "March 2023" = "Monthly Progress Score - March 2023 (MSSW ONLY)",
         "April 2023" = "Monthly Progress Score - April 2023",
         "May 2023" = "Monthly Progress Score - May 2023",
         "June 2023" = "Monthly Progress Score - June 2023",
         "July 2023" = "Monthly Progress Score - July 2023",
         "August 2023" = "Monthly Progress Score - August 2023",
         "September 2023" = "Monthly Progress Score - September 2023",
         "October 2023" = "Monthly Progress Score - October 2023 (Due 21/11/23)",
         "November 2023" = "Monthly Progress Score - November 2023 (Due 13th Dec)",
         "December 2023" = "Monthly Progress Score - December 2023 (due 17th January)",
         "January 2024" = "Monthly Progress Score - January 2024 (due 13th Feb 2024)",
         "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)") |>
  
  mutate(
    across(everything(), 
           ~if_else(. == "" | . == "NO Report received" | . == "No report received", NA, .))) |>
  
  mutate(
    across(
      .cols = matches("202"),
      .fns = ~parse_number(.)
    )
  ) |>
  
  filter(
    !if_all(everything(), is.na))

month_order <- c("January 2023","February 2023", "March 2023", "April 2023",
                 "May 2023","June 2023", "July 2023",
                 "August 2023", "September 2023","October 2023", 
                 "November 2023", "December 2023", "January 2024")


df2_diff_med <- df1_diff_med |>
              pivot_longer(
                cols = contains("202"),
                    names_to = "month", 
                        values_to = "score") |>
              mutate(
                month = factor(month, levels = month_order)) 

df3_diff_med <- df2_diff_med |>
                    arrange(
                      `Health Board / Trust`, `Unique ID`, fct_inorder(month)) |>
                    group_by(
                      `Health Board / Trust`, `Unique ID`) |>
                    mutate(
                      last_observed_score = na.locf(score, na.rm = FALSE),
                           difference = score - lag(last_observed_score, default = NA_real_))

# Cleaning up intermediate column if not needed
df3_diff_med <- df3_diff_med |>
                  select(-last_observed_score) 

df3_diff_med<- df3_diff_med |>
                  filter(!is.na(`Health Board / Trust`))

median_diff_df <- df3_diff_med |>
                    group_by(
                      `Health Board / Trust`, month) |>
                    summarize(
                      median_diff = median(difference, na.rm = TRUE), .groups = 'drop') |>
                    filter(
                      month != "February 2023")
