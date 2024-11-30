library(ggplot2)
library(dplyr)
library(arrow)
library(ggrepel)

### 1.
#Unzipping in a folder called "PSET4 External Folder"
zippath <- "/Users/tonanziht/Downloads/PSET4 External Folder/"
zipF <- paste0(zippath, "nursing-home-inspect-data.zip")
unzip(zipF,exdir=zippath)

### 2
#creating parquet
csv_path <- "/Users/tonanziht/Downloads/PSET4 External Folder/"
nursing_data_parquet <- open_dataset(
  sources = paste0(csv_path, "nursing-home-inspect-data"),
  col_types = schema(facility_id = string()),
  format = "csv"
)
#lazy display of 10 csv files
nursing_data_parquet
glimpse(nursing_data_parquet)

### 3
#fixing miscategorization of column "facility_id" from int64 to string
nursing_partitioned_state <- nursing_data_parquet %>% group_by(state) %>% collect()

#data is partitioned into the external folder by state
write_dataset(nursing_partitioned_state, path = csv_path, format = "parquet")

### 4
#creating a funct that can time the pull from the parquet and partition data for a given state
compare_state_speed <- function(state_name) {
  # Time parquet (non-partitioned) data
  parquet_start <- Sys.time()
  parquet_summary <- nursing_data_parquet %>%
    filter(state == state_name) %>%
    group_by(scope_severity) %>%
    summarise(deficiency_count = n()) %>%
    arrange(scope_severity) %>%
    collect()
  parquet_end <- Sys.time()
  parquet_time <- as.numeric(parquet_end - parquet_start, units = "secs")
  
  # Time partition data
  partition_start <- Sys.time()
  partition_summary <- nursing_partitioned_state %>%
    filter(state == state_name) %>%
    group_by(scope_severity) %>%
    summarise(deficiency_count = n()) %>%
    arrange(scope_severity) %>%
    collect()
  partition_end <- Sys.time()
  partition_time <- as.numeric(partition_end - partition_start, units = "secs")
  
  #  difference
  time_diff <- parquet_time - partition_time
  
  cat("\nProcessing times for state:", state_name, "\n")
  cat("Parquet processing time:", round(parquet_time, 3), "seconds\n")
  cat("Partition processing time:", round(partition_time, 3), "seconds\n")
  cat("Time difference (Parquet - Partition):", round(time_diff, 3), "seconds\n")
  
  return(list(
    parquet_summary = parquet_summary,
    partition_summary = partition_summary,
    parquet_time = parquet_time,
    partition_time = partition_time,
    time_difference = time_diff
  ))
}

compare_state_speed("NJ") #example

###5
get_all_states_performance <- function(nursing_data_parquet, nursing_partitioned_state) {
  states <- nursing_data_parquet %>%
    select(state) %>%
    distinct() %>%
    collect() %>%
    pull(state)
  
  results <- list()
  
  for (state_name in states) {
    state_size <- nursing_data_parquet %>%
      filter(state == state_name) %>%
      tally() %>%
      collect() %>%
      pull(n)
    
    # time parquet (non-partitioned) data
    parquet_start <- Sys.time()
    parquet_summary <- nursing_data_parquet %>%
      filter(state == state_name) %>%
      group_by(scope_severity) %>%
      summarise(deficiency_count = n()) %>%
      arrange(scope_severity) %>%
      collect()
    parquet_end <- Sys.time()
    parquet_time <- as.numeric(parquet_end - parquet_start, units = "secs")
    
    # time partition data
    partition_start <- Sys.time()
    partition_summary <- nursing_partitioned_state %>%
      filter(state == state_name) %>%
      group_by(scope_severity) %>%
      summarise(deficiency_count = n()) %>%
      arrange(scope_severity) %>%
      collect()
    partition_end <- Sys.time()
    partition_time <- as.numeric(partition_end - partition_start, units = "secs")
    
    # speed improvement ratio/x times
    speed_ratio <- parquet_time / partition_time
    
    # save results
    results[[state_name]] <- data.frame(
      state = state_name,
      data_size = state_size,
      speed_ratio = speed_ratio
    )
  }
  
  results_df <- do.call(rbind, results)
  return(results_df)
}

#all states
performance_data <- get_all_states_performance(nursing_data_parquet, nursing_partitioned_state)

p <- ggplot(performance_data, aes(x = data_size, y = speed_ratio)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_text_repel(
    aes(label = state),
    size = 3,
    box.padding = 0.5,
    segment.color = "grey50",
    max.overlaps = Inf
  ) +
  labs(
    title = "Processing Speed Improvement: Partitioned and Non-Partitioned",
    x = "Number of Observations in State",
    y = "Speed Improvement by X Times (Parquet (Non-Partition) Time / Partition Time)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave("state_speed_performance.png", p, width = 10, height = 8, dpi = 300)


###6
get_all_states_timing <- function(nursing_data_parquet, nursing_partitioned_state) {
  states <- nursing_data_parquet %>%
    select(state) %>%
    distinct() %>%
    collect() %>%
    pull(state)
  
  results <- list()
  
  for (state_name in states) {
    # Time parquet (non-partitioned)
    parquet_start <- Sys.time()
    parquet_summary <- nursing_data_parquet %>%
      filter(state == state_name) %>%
      group_by(scope_severity) %>%
      summarise(deficiency_count = n()) %>%
      arrange(scope_severity) %>%
      collect()
    parquet_end <- Sys.time()
    parquet_time <- as.numeric(parquet_end - parquet_start, units = "secs")
    
    # Time partition data
    partition_start <- Sys.time()
    partition_summary <- nursing_partitioned_state %>%
      filter(state == state_name) %>%
      group_by(scope_severity) %>%
      summarise(deficiency_count = n()) %>%
      arrange(scope_severity) %>%
      collect()
    partition_end <- Sys.time()
    partition_time <- as.numeric(partition_end - partition_start, units = "secs")
    
    # time difference
    time_diff <- parquet_time - partition_time
    
    results[[state_name]] <- data.frame(
      state = state_name,
      parquet_time = parquet_time,
      partition_time = partition_time,
      time_difference = time_diff
    )
  }
  
  results_df <- do.call(rbind, results)
  return(results_df)
}

# timing data for all states
timing_data <- get_all_states_timing(nursing_data_parquet, nursing_partitioned_state)

# reshape data for plotting
timing_long <- timing_data %>%
  pivot_longer(
    cols = c(parquet_time, partition_time, time_difference),
    names_to = "metric",
    values_to = "time"
  ) %>%
  mutate(
    metric = factor(metric, 
                    levels = c("parquet_time", "partition_time", "time_difference"),
                    labels = c("Non-partitioned Time", "Partitioned Time", "Time Saved"))
  )

p <- ggplot(timing_long, aes(x = reorder(state, time), y = time, color = metric)) +
  geom_point(size = 3) +
  geom_line(aes(group = metric)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  labs(
    title = "Processing Time Comparison by State",
    x = "State",
    y = "Time (seconds)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("state_timing_comparison.png", p, width = 12, height = 8, dpi = 300)
