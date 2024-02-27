library(readxl)
library(tidyverse)
library(zoo)
library(gridExtra)
library(grid)
library(Cairo)
library(purrr)

################# PREPARING DATA FRAMES FOR MEDIANS, COUNTS AND BOX PLOTS #################################

df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx")


df1 <- df |>
          select(
            `Health Board / Trust`, 
              `Unique ID` ,
                contains("202")) |>
          mutate(
            `Health Board / Trust` = str_remove_all(
            `Health Board / Trust`, " University| Trust| Health| Board| NHS| Teaching"))|>
          rename(
                "February 2023"= "Monthly Progress Score - February 2023 (MSSW only)",
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
                 "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)")|>
          mutate(
            across(
              everything(),
              ~if_else(. == "" | . == "NO Report received" | . == "No report received", NA, .)))|>
          mutate(
            across(
              .cols = matches("202"),
              .fns = ~parse_number(.))) |>
          filter(!if_all(everything(), is.na))

# Set levels for months 
month_order <- c("January 2023","February 2023", "March 2023", "April 2023", "May 2023", 
                 "June 2023", "July 2023", "August 2023", "September 2023", 
                 "October 2023", "November 2023", "December 2023", 
                 "January 2024")


df2 <- df1 |>
        pivot_longer(
          cols = contains("202"),
                     names_to = "month", 
                     values_to = "score") |>
        mutate(
          month = factor(month, levels = month_order)) |>
        group_by(
          `Health Board / Trust`, month) |>
        summarise(
          count_sub = sum(!is.na(score)),
                  median_score = median(score, na.rm = TRUE)) |>
        arrange(
          `Health Board / Trust`, month)


############### PREPARING DATA FRAMES FOR ENGAGEMENT ANALYSIS ################################

at <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/Contact List/SCC Master Distribution List .xlsx")

at1 <- at |>
        select(
          Organisation,21:40) |>
        # REMOVING INTERNAL organisation COUNTS FROM ANALYSIS
        filter(
          !(Organisation %in% c("Public Health Wales", "IHI", "RSM", "NHS Wales Executive", 
                                "Improvement Cymru", "NHS Executive", "HEIW", "Assitant Director of Quailty and Nursing"))) |>
        mutate(
          across(2:21, ~ifelse(grepl("^[Yy]", .) | grepl("^[Yy]$", .), "Y", NA)),
          `Learning Session 4(November 2023)` = ifelse(!is.na(`LS4 Day 2`) | !is.na(`LS4 Cl Ex`) | !is.na(`LS4 Day 1 (AB, CTM, HD)`), "Y", NA),
          `Learning Session 3(September 2023)` = ifelse(!is.na(`LS3`) | !is.na(`LS3 Cl Ex`), "Y", NA),
          Organisation = case_when(
            Organisation %in% c("Aneurin Bevan UHB Monmouthshire Gov", "Monmouthshire Gov", "Aneurin Bevan UHB") ~ "Aneurin Bevan",
            Organisation %in% c("BCUHB/FCC/WCBC/Flinshire CBC","Betsi Cadwaladr UHB")  ~ "Betsi Cadwaladr",
            Organisation %in% c("Cardiff & Vale UHB & Acute Response Team","Cardiff & Vale UHB")  ~ "Cardiff & Vale",
            Organisation %in% c("Swansea Council","Swansea Bay UHB") ~ "Swansea Bay",
            Organisation %in% c("NHS Velindre Trust - Welsh Blood Service", "Velindre University NHS Trust", "Velindre") ~ "Velindre",
            Organisation %in%c("Welsh Ambulance Service NHS Trust", "Welsh Ambulance Service Trust") ~ "Welsh Ambulance Service",
            Organisation == "Hywel Dda UHB" ~ "Hywel Dda",
            Organisation ==  "Powys THB" ~ "Powys",
            Organisation == "Cwm Taf Morgannwg UHB" ~ "Cwm Taf Morgannwg",
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
        select(
          -c("LS4 Day 2", "LS4 Cl Ex", "LS4 Day 1 (AB, CTM, HD)","LS3", "LS3 Cl Ex"))
      

# Enhanced function to calculate counts and format the data frame
calc_counts_and_format <- function(session_pattern, count_column_name) {
        at1 |>
          select(`Health Board / Trust`, matches(session_pattern)) |>
          filter(!is.na(`Health Board / Trust`)) |> # Drop rows with NA in Organisation
          pivot_longer(
            cols = matches(session_pattern),
            names_to = "session",
            values_to = "attendance"
          ) |>
          group_by(`Health Board / Trust`, session) |>
          summarise(count = sum(attendance == "Y", na.rm = TRUE)) |>
          mutate(month = str_extract(session, "\\(([^)]+)")) |>
          mutate(month = str_remove_all(month, "[()]")) |>
          rename(!!count_column_name := count) |>
          select(-session)
      }

# Using the enhanced function to create individual data frames
#renaming columns for clarity later after the full join
cc <- calc_counts_and_format("^C", "count_cc")
cc_lead <- calc_counts_and_format("^Lead", "count_cclead")
ls <- calc_counts_and_format("^Lear", "count_ls")


# List all data frames you want to join
data_frames <- list(cc, df2, cc_lead, ls)

# Use reduce with full_join to merge all data frames
final_df <- reduce(data_frames, full_join, by = c("month", "Health Board / Trust"))

### data frame that will have number of submissions, median scores, and attendance in one data frame 
finale_df <- final_df |> 
                mutate(
                  month = factor(month, levels = month_order, ordered = TRUE))|> #using levels created before
                select(
                  `Health Board / Trust`, month, count_sub, median_score, count_cc, count_cclead, count_ls)|> #reordering columns for a better view
                mutate(
                  across(
                    c(starts_with("c")), ~if_else(.x == 0, NA_real_, .x)))|> #converting attendance values from 0 to NA for plotting purposes and uniformity as some other 0 values are already NA
                arrange(
                  `Health Board / Trust`, month)|>
                filter(
                  !is.na(`Health Board / Trust`) &
                    !is.na(month)
                )


############## DATA FRAME PREP FOR BOXPLOTS ##########################

bp_df <- df1 |>
          pivot_longer(
            cols = contains("202"),
            names_to = "month", 
            values_to = "score"
          ) |>
          mutate(
            month = factor(month, levels = month_order, ordered = TRUE)
          ) |>
          filter(
            !is.na(`Health Board / Trust`) & !is.na(month)
          )|>
          select(
            - `Unique ID`
          )|>
          arrange(
            `Health Board / Trust`, month
          )
############### DATA FRAME PREP FOR PLOTTING ATTENDANCE #######################

bg <- finale_df |>
        select(
          -count_sub, 
          -median_score) |>
        pivot_longer(
          cols = starts_with("c"),
          names_to = "session",
          values_to = "number")|>
          filter(
            !is.na(
              `Health Board / Trust`), 
                 month != "December 2023") # NO EVENT IN DECEMBER)



############## DATA FRAME PREP FOR DIFFERENCE OF MEDIAN ####################### 
diff <- finale_df |>
          select(
            `Health Board / Trust`, month, median_score) |>
          filter(
            !is.na(month) & !is.na(`Health Board / Trust`), month != "January 2023")|># FILTERING OUT JAN AS SCORE SUBMISSIONS BEGAN IN FEB 2023
          arrange(
            `Health Board / Trust`,
            month) |>
          group_by(
            `Health Board / Trust`) |>
          mutate(
            last_observed_median = na.locf(median_score, na.rm = FALSE), #na.locf(median_score, na.rm = FALSE): It uses the na.locf function from the zoo package to carry the last observed (non-NA) median_score forward. This is useful for filling in missing data points with the most recent non-missing value within each group.
            difference = median_score - lag(last_observed_median,
                                            default = NA_real_)) #It calculates the difference in median_score from the previous month's median_score for each health board/trust. The lag function shifts the last_observed_median series down by one, aligning each month's score with the previous month's score for subtraction. The default = NA_real_ ensures that the first observation in each group has a default NA value for the lagged series, as there's no preceding month to compare it with.
                                                                 
         
# Cleaning up intermediate column if not needed
diff <- diff |>
          select(
            -last_observed_median) |>
            filter(
              month != "February 2023") # as there is no preceding month to this, this can be omitted for plotting


################DATA FRAME PREP FOR MEDIAN OF DIFFERENCE PER PROJECT PER MONTH################################

df1_diff_med <- df1|>
                pivot_longer(
                  cols = contains("202"),
                  names_to = "month", 
                  values_to = "score") |>
                mutate(
                  month = factor(month, levels = month_order))|>
                  arrange(
                    `Health Board / Trust`, `Unique ID`, fct_inorder(month)) |>
                  group_by(
                    `Health Board / Trust`, `Unique ID`) |>
                  mutate(
                    last_observed_score = na.locf(score, na.rm = FALSE),
                    difference = score - lag(last_observed_score, default = NA_real_))|>
                  filter(!is.na(`Health Board / Trust`))

# Cleaning up intermediate column if not needed
df3_diff_med <- df1_diff_med |>
                 select(-last_observed_score) 



median_diff_df <- df3_diff_med |>
                  group_by(
                    `Health Board / Trust`, month) |>
                  summarize(
                    median_diff = median(difference, na.rm = TRUE), .groups = 'drop') |>
                  filter(
                    month != "February 2023")

################ PLOTTING BEGINS #######################################


#Listing all Unique Values of Health Board / Trust for loop
unique_boards <- unique(finale_df$`Health Board / Trust`)


# Iterate over each unique Health Board / Trust to create individual plots
for(board in unique_boards) {
  
#############PLOTTING LOGIC FOR BOX PLOTS#################  
  board_df <- bp_df |>
    filter(`Health Board / Trust` == board)

  
  # Plotting
  bp <- ggplot(board_df, aes(x = month, y = score)) +
    geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0,5)) +
    labs(
      title = paste("Box Plot of Monthly Progress Scores for", board)
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5, color = "#1b5768"),
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
  
  #############PLOTTING LOGIC FOR NUMBER OF SCORES SUBMITTED################# 
  
num_scores <- finale_df |>
                filter(
                    `Health Board / Trust` == board,
                       month != "January 2023") # SCORE SUBMISSIONS BEGAN FROM FEB 23
  

  
  # Calculate max_count_sub with a fallback default value
  max_count_sub <- max(num_scores$count_sub, na.rm = TRUE)
  if(!is.finite(max_count_sub)) {
    max_count_sub <- 10 # Default fallback value, adjust as appropriate
  } else {
    max_count_sub <- max_count_sub + 2
  }
  
  # Adjust breaks based on the range of count_sub
  breaks_value <- if(is.finite(max_count_sub)) seq(0, max_count_sub, by = 1) else 1
  
  # Create the line graph for the current health board
  # Use dynamic values for limits and breaks
  num <- ggplot(num_scores, aes(x = month, y = count_sub, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    scale_x_discrete() +
    scale_y_continuous(breaks = breaks_value, limits = c(0, max_count_sub)) +
    labs(title = paste("Number of submissions per month by", board))+
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5, color = "#1b5768"),
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

  #############PLOTTING LOGIC FOR BAR GRAPH OF ATTENDANCE################# 

  bg_plot <- bg |>
              filter(
                `Health Board / Trust` == board)

  # Ensure max_count is a finite number
  max_count <- max(bg_plot$number, na.rm = TRUE)
  
  if(!is.finite(max_count)) {
    max_count <- 10 # A default fallback value, adjust as needed
  } else {
    max_count <- max_count + 2 # Only add 2 if max_count is finite
  }
  
  # Now, max_count is guaranteed to be finite, so we can safely use it in seq()
  # Also, ensure that by argument of seq() does not evaluate to zero
  step_size <- max(1, floor((max_count - 0) / 5))
  if (step_size > 0) {
    y_breaks <- seq(0, max_count, by = step_size)
  } else {
    # Fallback in case step_size calculation fails to produce a positive number
    y_breaks <- seq(0, max_count, by = 1) # Default to increment by 1
  }
  
  # Plot
  att <- ggplot(bg_plot, aes(x = month, y = number, fill = session)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("count_cc" = "#FFE18A", "count_cclead" = "#BCB1C7", "count_ls" = "#99D7D8"),
                      labels = c("Coaching Call", "Coaching Call (Leadership)", "Learning Session")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_count), breaks = y_breaks) +
    labs(title = paste("Number of attendees in Learning Session and Coaching Call from", board),
         x = "Month",
         y = "",
         fill = "Session Type") +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 7.5, hjust = 0.5, color = "#1b5768"),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "darkgray"),
      axis.text.y = element_text(colour = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom", # Adjust the legend position if needed
      legend.direction = "horizontal", # Make legend items appear in a single line
      legend.box = "horizontal", # Ensure the legend box aligns items horizontally
      legend.background = element_blank(),
      legend.text = element_text(size = 6), # Reduce the size of the legend text
      legend.key.size = unit(0.5, "cm"),
      legend.key = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "darkgray"),
      axis.ticks.x = element_line(colour = "gray"),
      axis.ticks.length = unit(0.1, "cm"),
      axis.ticks.margin = unit(0.2, "cm")
      
    )

##################### PLOTTING LOGIC OF DIFFERENCE OF MEDIAN ###################################

new_diff <- diff |>
            filter(
              `Health Board / Trust` == board) 
   
  # Determine the range for y-axis to ensure it includes 0
  y_range <- range(new_diff$difference, na.rm = TRUE)
  y_range[1] <- min(0, y_range[1])  # Ensure 0 is included as the lower limit if not already
  
  # Create the line graph for the current health board
  plot_diff <- ggplot(new_diff, aes(x = month, y = difference, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") + #### Add horizontal line at y=0(IMPROVES READABILITY)
    scale_x_discrete() +
    scale_y_continuous(limits = y_range) +  # Ensure y-axis always includes 0
    labs(
      title = paste("Difference in median monthly progress scores of", board)
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5, color = "#1b5768"),
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
  
########## PLOTTING LOGIC FOR MEDIAN OF DIFFERENCE ###############################

diff_med <- median_diff_df |>
             filter(
               `Health Board / Trust` == board)
  

  # Determine the range for y-axis to ensure it includes 0
  y_range <- range(diff_med$median_diff, na.rm = TRUE)
  y_range[1] <- min(0, y_range[1])  # Ensure 0 is included as the lower limit if not already
  
  # Create the line graph for the current health board
  plot_diff_med <- ggplot(diff_med, aes(x = month, y = median_diff, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  ############# Add horizontal line at y=0 (IMPROVES READABILITY)
    scale_x_discrete() +
    scale_y_continuous(limits = y_range) +  # Ensure y-axis always includes 0
    labs(
      title = paste("Median of difference of monthly progress scores of", board)) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5, color = "#1b5768"),
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
  
############### logic for creating a combined plot in a grid ###############################

  # Create the combined plot with grid.arrange() but don't automatically draw it
  combined_plot <- arrangeGrob(bp, plot_diff, att, num, plot_diff_med, ncol = 3, nrow = 2)
  
  # Specify the file name dynamically based on the Health Board / Trust name
  file_name <- paste0("combined_plot_for_", gsub(" ", "_", board), ".png")
  
  # Open a PNG device with 300 dpi
  png(filename = file_name, width = 13.33, height = 6.58, units = "in", res = 300)
  grid.draw(combined_plot) # Draw the combined plot
  
  
  # instead of a caption for all plots, one common caption for all charts
  grid.text("Source: SCC National Planning File and Master Distribution List", x = unit(0.98, "npc"), y = unit(0.02, "npc"), 
            just = "right", gp = gpar(col = "darkgray", fontsize = 8))
  dev.off() # Close the device
} #Close the loop



###########THIS IS THE SAMPLE OF THE BAR GRAPH AND LINE GRAPH WE Said WE'LL GIVE TO DOM AS AN OPTION


################## COMBO OF BAR GRAPH AND LINE GRAPH #############################

fin <- finale_df |>
         filter(`Health Board / Trust` == "Aneurin Bevan")

# Transform data for count values and attendance
finale_df_long <- fin |>
                  pivot_longer(
                    cols = c(count_cc, count_cclead, count_ls),
                    names_to = "category", 
                    values_to = "value") |>
                       filter(!is.na(value))

# Update category levels with more descriptive names
finale_df_long$category <- factor(finale_df_long$category,
                                  levels = c("count_cc", "count_cclead", "count_ls"),
                                  labels = c("Coaching Call", "Coaching Call (Leadership)", "Learning Session"))

latest <- ggplot(data = fin, aes(x = month)) +
  geom_bar(data = finale_df_long,
           aes(y = value / max(finale_df_long$value) * 6, fill = category), # Scale count
           stat = "identity", position = "stack") +
  geom_line(aes(y = count_sub, group = 1), color = "#4A7986", size = 1) +
  geom_point(aes(y = count_sub, group = 1), shape = 21, size = 4, fill = "white", color = "#4A7986", stroke = 0.5) + # changing count_sub to median_score will produce the other graph
                                                                                                                     # Have attached it in the task but not written it here to avoid duplicate codes
  scale_y_continuous(name = "Count", 
                     sec.axis = sec_axis(~ . * max(finale_df_long$value) / 6, name = "Count"),
                     expand = c(0, 0)) + # Remove padding to ensure bars rest on the x-axis line
  scale_x_discrete(limits = month_order) +
  scale_fill_manual(values = c("Coaching Call" = "#FFE18A", 
                               "Coaching Call (Leadership)" = "#BCB1C7", 
                               "Learning Session" = "#99D7D8")) +
  labs(title = "Number of submissions v/s number of attendees in coaching calls and learning sessions for Aneurin Bevan UHB ", fill = "Category") +
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
    plot.title = element_text(family = "Arial", size = 10, color = "#1b5768", hjust = 0.5),
    axis.line.x = element_line(color = "darkgray", size = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom", # Adjust the legend position if needed
    legend.direction = "horizontal", # Make legend items appear in a single line
    legend.box = "horizontal", # Ensure the legend box aligns items horizontally
    legend.background = element_blank(),
    legend.text = element_text(size = 6), # Reduce the size of the legend text
    legend.key.size = unit(0.5, "cm"),
    legend.key = element_blank(),
  )


print(latest)


# Export the plot to a PNG file with 300 dpi
#CairoPNG("plot_bar_latest_count.png", width = 10, height = 7, units = "in", dpi = 300)
#print(latest) # Make sure to print the plot
#dev.off() # Close the device



