pacman::p_load(dplyr, lubridate, stringr)

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Polish_parliamentary_election"
page <- read_html(url)

# Extract all tables and get the fourth one
tables <- page %>% html_table(fill = TRUE)
polls <- tables[[4]]

# Function to parse fieldwork dates and create start and end dates
parse_fieldwork_dates <- function(date_string) {
  # Clean the date string
  date_string <- str_trim(date_string)
  date_string <- str_replace_all(date_string, "\"", "")  # Remove quotes
  
  # Handle different date formats
  if (grepl("–", date_string)) {
    # Date range with en-dash
    parts <- str_split(date_string, "–")[[1]]
    start_part <- str_trim(parts[1])
    end_part <- str_trim(parts[2])
    
    # Parse the end date first (it usually has the full format)
    if (grepl("\\d+ \\w+", end_part)) {
      # End part has day and month
      end_date <- dmy(paste(end_part, "2025"))
      
      # Parse start part
      if (grepl("\\d+ \\w+", start_part)) {
        # Start part also has day and month
        start_date <- dmy(paste(start_part, "2025"))
      } else {
        # Start part only has day, use end date's month and year
        start_day <- as.numeric(start_part)
        start_date <- dmy(paste(start_day, format(end_date, "%B"), year(end_date)))
      }
    } else {
      # Try other formats
      return(list(start_date = NA, end_date = NA))
    }
  } else {
    # Single date
    single_date <- dmy(paste(date_string, "2025"))
    start_date <- single_date
    end_date <- single_date
  }
  
  return(list(start_date = start_date, end_date = end_date))
}

# Function to extract just the polling agency name
extract_pollster_name <- function(pollster_string) {
  # Remove quotes and trim whitespace
  pollster_string <- str_trim(str_replace_all(pollster_string, "\"", ""))
  
  # Extract everything before the first " / " (which separates pollster from news org)
  pollster_name <- str_split(pollster_string, " / ")[[1]][1]
  
  # Clean up any remaining artifacts
  pollster_name <- str_trim(pollster_name)
  pollster_name <- str_replace_all(pollster_name, ",,", "")  # Remove double commas
  
  return(pollster_name)
}

# Function to standardize polling organization names
standardize_pollster_name <- function(pollster_name) {
  # Convert to lowercase for comparison, but preserve original case for output
  pollster_lower <- tolower(pollster_name)
  
  # Standardize similar organization names
  if (grepl("opinia\\s*24", pollster_lower)) {
    return("Opinia 24")
  } else if (grepl("^ipsos", pollster_lower)) {
    return("IPSOS")
  } else if (grepl("^ibris", pollster_lower)) {
    return("IBRiS")
  } else if (grepl("united\\s*surveys", pollster_lower)) {
    return("United Surveys")
  } else if (grepl("^cbos", pollster_lower)) {
    return("CBOS")
  } else if (grepl("research\\s*partner", pollster_lower)) {
    return("Research Partner")
  } else if (grepl("^pollster", pollster_lower)) {
    return("Pollster")
  } else if (grepl("social\\s*changes", pollster_lower)) {
    return("Social Changes")
  } else if (grepl("^ogb", pollster_lower)) {
    return("OGB")
  } else {
    # Return original name if no standardization rule applies
    return(pollster_name)
  }
}

# Main cleaning process
clean_polish_poll_data <- function(polls) {
  
  # Remove the unnamed columns (keep only first 14 columns)
  polls_clean_cols <- polls[, 1:14]
  
  # Step 1: Initial cleaning and filtering
  cleaned_data <- polls_clean_cols %>%
    # Remove header row, empty rows, presidential election rows, and text elements
    filter(!is.na(`Polling firm/Link`) & 
             `Polling firm/Link` != "" & 
             `Polling firm/Link` != "Polling firm/Link" &
             !grepl("Presidential election|2023 parliamentary election|The Third Way|Confederation", 
                    `Polling firm/Link`, ignore.case = TRUE) &
             !grepl("The Third Way|Confederation", Fieldworkdate, ignore.case = TRUE)) %>%
    
    # Select and rename columns
    select(
      pollster_raw = `Polling firm/Link`,
      fieldwork_date = Fieldworkdate,
      united_right = `United Right`,
      civic_coalition = `Civic Coalition`,
      poland_2050 = `Poland 2050`,
      polish_peoples_party = `Polish People's Party`,
      the_left = `The Left`,
      together = Together,
      confederation = Confederation,
      confederation_crown = `Confederation of the Polish Crown`,
      dont_know = `Don't know`
    ) %>%
    
    # Extract clean pollster names and standardize them
    mutate(
      pollster_raw = sapply(pollster_raw, extract_pollster_name),
      pollster = sapply(pollster_raw, standardize_pollster_name)
    ) %>%
    
    # Remove the raw pollster column
    select(-pollster_raw) %>%
    
    # Convert numeric columns from character to numeric
    mutate(
      united_right = as.numeric(united_right),
      civic_coalition = as.numeric(civic_coalition),
      poland_2050 = as.numeric(poland_2050),
      polish_peoples_party = as.numeric(polish_peoples_party),
      the_left = as.numeric(the_left),
      together = as.numeric(together),
      confederation = as.numeric(confederation),
      confederation_crown = as.numeric(confederation_crown),
      dont_know = as.numeric(dont_know)
    ) %>%
    
    # Handle missing values by setting them to 0 for calculations
    mutate(
      the_left = ifelse(is.na(the_left), 0, the_left),
      together = ifelse(is.na(together), 0, together),
      polish_peoples_party = ifelse(is.na(polish_peoples_party), 0, polish_peoples_party),
      confederation_crown = ifelse(is.na(confederation_crown), 0, confederation_crown),
      dont_know = ifelse(is.na(dont_know), 0, dont_know)  # Code missing DK as 0
    )
  
  # Step 2: Parse dates
  date_results <- lapply(cleaned_data$fieldwork_date, parse_fieldwork_dates)
  
  # Extract start and end dates
  cleaned_data$start_date <- sapply(date_results, function(x) as.character(x$start_date))
  cleaned_data$end_date <- sapply(date_results, function(x) as.character(x$end_date))
  
  # Convert back to Date objects
  cleaned_data$start_date <- as.Date(cleaned_data$start_date)
  cleaned_data$end_date <- as.Date(cleaned_data$end_date)
  
  # Step 3: Create final dataset with combined columns
  final_data <- cleaned_data %>%
    mutate(
      # Combine Lewica (The Left + Together)
      lewica = the_left + together,
      
      # Create date thresholds
      june_17_2025 = as.Date("2025-06-17"),
      march_10_2025 = as.Date("2025-03-10"),
      
      # For Poland 2050: add Polish People's Party results for entries after 17th June
      poland_2050_combined = ifelse(end_date > june_17_2025, 
                                    poland_2050 + polish_peoples_party, 
                                    poland_2050),
      
      # Confederation handling: include Crown before March 10, ignore Crown after March 10
      confederation_clean = ifelse(end_date <= march_10_2025,
                                   confederation,  # Include Crown before March 10
                                   confederation)  # Use Confederation as-is after March 10 (ignore Crown column)
    ) %>%
    
    # Calculate total of all parties to determine Other
    mutate(
      total_parties = united_right + civic_coalition + poland_2050_combined + 
        lewica + confederation_clean + dont_know,  # dont_know is now always numeric (0 if missing)
      Other = pmax(0, 100 - total_parties)  # Ensure no negative values
    ) %>%
    
    # Select final columns with new names
    select(
      org = pollster,
      startDate = start_date,
      endDate = end_date,
      PiS = united_right,
      KO = civic_coalition,
      TD = poland_2050_combined,
      Lewica = lewica,
      Konfederacja = confederation_clean,
      DK = dont_know,
      Other
    ) %>%
    
    # Remove any rows with missing essential data
    filter(!is.na(startDate) & !is.na(endDate) & !is.na(PiS)) %>%
    
    # Order by end date (most recent first)
    arrange(desc(endDate))
  
  return(final_data)
}

# Apply the cleaning function to polls
polls <- clean_polish_poll_data(polls)

# Display summary of cleaned data
cat("Cleaned Polish polling data summary:\n")
cat("Total polls:", nrow(polls), "\n")
cat("Date range:", as.character(min(polls$startDate)), "to", as.character(max(polls$endDate)), "\n")
cat("Missing 'DK' values:", sum(is.na(polls$DK)), "\n\n")

# Show column ranges
cat("Column ranges:\n")
numeric_cols <- c("PiS", "KO", "TD", "Lewica", "Konfederacja", "DK", "Other")
for(col in numeric_cols) {
  values <- polls[[col]]
  cat(sprintf("%-20s: %5.1f - %5.1f (Missing: %d)\n", 
              col, 
              min(values, na.rm = TRUE), 
              max(values, na.rm = TRUE),
              sum(is.na(values))))
}

# Display first few rows
cat("\nFirst 5 rows of cleaned data:\n")
print(head(polls, 5))