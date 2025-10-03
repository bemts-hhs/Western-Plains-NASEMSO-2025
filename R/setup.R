###_____________________________________________________________________________
### Custom functions and other utilities for the Annual Trauma Report 2024 ----
### For any analyses, these data must be loaded for the applicable section
### This script should be reviewed and ran first before going to any other
### scripts in the project
###_____________________________________________________________________________

# Utilize the air package for code formatting

### packages ----

# these packages are utilized in this project and must be loaded
# install.packages(c(
#   'renv',
#   'usethis',
#   'devtools',
#   'tidyverse',
#   'traumar',
#   'nemsqar',
#   'naniar',
#   'cli',
#   'ggrepel',
#   'ggthemes',
#   'janitor',
#   'gt',
#   'gtsummary',
#   'gtExtras',
#   'webshot2',
#   'svglite',
#   'leaflet',
#   'quarto'
# ))

###_____________________________________________________________________________
# showtext setup ----
###_____________________________________________________________________________

# # run showtext auto, use throughout project
# showtext::showtext_auto()

# # get 300 dpi with showtext
# showtext::showtext_opts(dpi = 300)

# # get work sans fonts of interest
# all_fonts <- systemfonts::system_fonts()

# # regular
# work_sans <- all_fonts |>
#   dplyr::filter(name == "WorkSans-Regular") |>
#   dplyr::pull(path)

# # semibold
# work_sans_semibold <- all_fonts |>
#   dplyr::filter(name == "WorkSans-SemiBold") |>
#   dplyr::pull(path)

# # extrabold
# work_sans_extrabold <- all_fonts |>
#   dplyr::filter(name == "WorkSans-ExtraBold") |>
#   dplyr::pull(path)

# # use sysfonts to load the fonts
# sysfonts::font_add(
#   family = "Work Sans",
#   regular = work_sans,
#   bold = work_sans_extrabold
# )

###_____________________________________________________________________________
# Plot / table messages ----
###_____________________________________________________________________________

# statistics messages
small_count_message <- "Small counts < 6 are masked to protect confidentiality."

# patient level
patient_count_message <- "All counts and other measures are at the patient level, only."
patient_proportion_message <- "Refers to the proportion of injuries attributed to a specified group of patients in a given timespan."

# injury level
injury_message <- "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan."

# case level

###_____________________________________________________________________________
# Get palettes ----
###_____________________________________________________________________________

# easy to access list of qualitative palettes
good_palettes <- paletteer::palettes_d_names |>
  dplyr::filter(grepl(pattern = "blind", x = package, ignore.case = TRUE)) |>
  dplyr::arrange(package, desc(length))

# palettes for continuous data
quant_palettes <- paletteer::palettes_c_names

###_____________________________________________________________________________
# Custom functions for statistics ----
###_____________________________________________________________________________

# Calculate critical values, useful in statistics and useful to shade plots
# Define the custom function to calculate critical values
calculate_critical_values <- function(data, stat_col, alpha = 0.05) {
  data %>%
    dplyr::summarise(
      lower = quantile({{ stat_col }}, probs = alpha / 2),
      upper = quantile({{ stat_col }}, probs = 1 - (alpha / 2))
    )
}

# Function to compute the statistical mode of a vector
# Returns the most frequently occurring value in x
# Handles NA values by removing them before computation (optional)
stat_mode <- function(x, na.rm = TRUE) {
  # Optionally remove missing values from the vector
  if (na.rm) {
    # Remove missing values to avoid skewing counts and errors
    x <- stats::na.omit(x)
  } else if (any(is.na(x))) {
    # If NAs present and not removing, return NA as mode is undefined
    return(NA)
  }

  # Handle empty input gracefully
  if (length(x) == 0) {
    return(NA)
  }

  # Get all unique values from the vector
  # These are the candidate mode values
  ux <- unique(x)

  # Find the position of each element of x in the unique values vector
  # This creates an integer index vector corresponding to ux
  idx <- match(x, ux)

  # Count the number of times each unique value appears in x
  # The result is an integer vector with counts aligned to ux
  counts <- tabulate(idx)

  # Determine the index of the unique value with the highest count
  # This identifies the position of the mode in ux
  mode_index <- which.max(counts)

  # Return the mode value itself, i.e., the unique value with the highest frequency
  ux[mode_index]
}

# The function modifies several aspects of the `{gt}` table:
# - Row Groups: Custom styling for row group text and background fill.
# - Column Labels & Spanners: Adjusted font size, color, and alignment.
# - Table Body: Formats text with different alignments and font styles.
# - Borders: Adds top borders to row groups and left borders to selected
#   columns.
# - Source Notes: Includes `{fontawesome}` icons and relevant metadata.
tab_style_hhs <- function(
  gt_object,
  row_groups = 14,
  column_labels = 14,
  title = 20,
  subtitle = 18,
  spanners = 16,
  body = 14,
  source_note = 12,
  footnote = 12,
  message_text,
  row_group_fill = "#E0A624",
  row_group_fill_alpha = 0.5,
  bold_first_col = 1,
  border_cols,
  border_color1 = "#19405B",
  border_color2 = "#70C8B8"
) {
  out <- gt_object |>

    # Set the font for the table
    gt::opt_table_font(
      font = "Work Sans",
      stack = NULL,
      weight = NULL,
      style = NULL,
      add = TRUE
    ) |>

    # Style the stub (row names) section
    gt::tab_style(
      locations = gt::cells_stub(),
      style = gt::cell_text(
        size = gt::px(body),
        font = "Work Sans SemiBold",
        color = "black",
        align = "left"
      )
    ) |>

    # Style the row groups
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(row_groups),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "left"
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Apply background color to row groups
    gt::tab_style(
      style = gt::cell_fill(
        color = row_group_fill,
        alpha = row_group_fill_alpha
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Add top border to row groups
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = border_color1,
        weight = gt::px(3) # Adjust thickness as needed
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Style column labels
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(column_labels),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "center",
        style = "italic"
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>

    # Style the table title
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans ExtraBold",
        color = "#19405B",
        size = gt::px(title)
      ),
      locations = gt::cells_title(groups = "title")
    ) |>

    # Style the table subtitle
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#70C8B8",
        size = gt::px(subtitle)
      ),
      locations = gt::cells_title(groups = "subtitle")
    ) |>

    # Style the spanner labels (column headers spanning multiple columns)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#03617A",
        size = gt::px(spanners),
        align = "center"
      ),
      locations = gt::cells_column_spanners()
    ) |>

    # Style the first column (typically used for labels)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "black",
        size = gt::px(body),
        align = "left"
      ),
      locations = gt::cells_body(columns = {{ bold_first_col }})
    ) |>

    # Style all other body cells (except first column)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans",
        color = "black",
        size = gt::px(body),
        align = "center"
      ),
      locations = gt::cells_body(columns = -1)
    ) |>

    # Style row names (stub)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans Black",
        size = gt::px(body),
        color = "black",
        align = "left"
      ),
      locations = gt::cells_stub(rows = gt::everything())
    ) |>

    # Style footnotes text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(footnote),
        color = "#19405B"
      ),
      locations = gt::cells_source_notes()
    ) |>

    # Style source note text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(source_note),
        color = "#19405B"
      ),
      locations = gt::cells_footnotes()
    ) |>

    # Add a left-side border to specified columns
    gt::tab_style(
      locations = gt::cells_body(columns = {{ border_cols }}),
      style = gt::cell_borders(
        sides = c("left"),
        weight = gt::px(2),
        color = border_color2
      )
    ) |>

    # Align all columns except the first one to the center
    gt::cols_align(align = "center", columns = 2)

  return(out)
}

###_____________________________________________________________________________
# Function to calculate age-adjusted EMS run rates by county and BH district
# This function computes directly standardized (age-adjusted) rates using
# pre-aggregated data that includes event counts, local population estimates,
# and standard population weights.
#
# Assumptions:
#   • The input data has already been grouped (e.g., by County and Age Group).
#   • {{ count }}, {{ local_population }}, and {{ standard_population_weight }}
#     are scalar fields within these grouped rows.
#   • The user provides any necessary grouping variables via ... to summarize
#     results at the desired geographic or demographic resolution.
#
# This function returns both crude and age-adjusted rates per specified
# multiplier (default is per 100,000 population).
#
# Inputs:
#   • data — a grouped or ungrouped data.frame or tibble with input variables
#   • count — unquoted column name representing the event count
#   • local_population — unquoted column name for the local population (e.g., county-age)
#   • standard_population_weight — unquoted column name for the standard weight (proportional)
#   • ... — grouping variables to aggregate final rates (e.g., County, District)
#   • rate — numeric value for scaling rates (default: 100,000)
#
# Output:
#   • A tibble with Count, Crude_Rate, and Age_Adjusted_Rate per grouping
###_____________________________________________________________________________

calc_age_adjusted_rate <- function(
  data, # input tibble or data.frame with grouped or stratified rows
  count, # unquoted column name for observed event count (e.g., EMS runs)
  local_population, # unquoted column name for stratum-specific local population
  standard_population_weight, # unquoted column name for proportional weight from standard population
  .by = NULL, # grouping variables for aggregating final rates (e.g., 'County', 'District')
  rate = 100000 # rate multiplier (e.g., per 1000, 10000, or 100000)
) {
  # Step 1: Calculate age-specific crude rate and weighted contribution
  rate_data <- data |>
    dplyr::mutate(
      crude_rate = ({{ count }} / {{ local_population }}) * rate, # rate within each age group
      weighted_rate = crude_rate * {{ standard_population_weight }} # contribution to adjusted rate
    )

  # Step 2: Aggregate to final grouping level
  rate_summary <- rate_data |>
    dplyr::summarize(
      Count = sum({{ count }}, na.rm = TRUE), # total count of events
      Crude_Rate = sum({{ count }}, na.rm = TRUE) /
        sum({{ local_population }}, na.rm = TRUE) *
        rate, # overall crude rate
      Age_Adjusted_Rate = sum(weighted_rate, na.rm = TRUE), # final adjusted rate
      .by = tidyselect::all_of({{ .by }}) # group by user-supplied variables (e.g., County)
    )

  return(rate_summary)
}

###_____________________________________________________________________________
### Helper function to compute period-to-period change in counts
### Adds three columns:
###   - change: raw numeric difference in count (n - lag(n))
###   - prop_change: proportional change ((n - lag(n)) / lag(n))
###   - prop_label: formatted percent label using traumar::pretty_percent()
### This assumes that the input data is already grouped and counted, with a column `n`
###_____________________________________________________________________________
add_change_metrics <- function(df) {
  df |>
    dplyr::mutate(
      # Compute raw numeric change from previous row
      change = n - dplyr::lag(n),

      # Compute proportional change (as a fraction)
      prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),

      # Generate a human-readable percent label (e.g., "14.2%")
      # Use NA_character_ if proportional change is NA (e.g., first row)
      prop_label = ifelse(
        !is.na(prop_change),
        traumar::pretty_percent(prop_change, n_decimal = 2),
        NA_character_
      )
    )
}

###_____________________________________________________________________________
### Helper to summarize reinjury counts after filtering for reinjuries (n > 1)
### Input:
###   - df: tibble with counts column `n` (injury counts per patient per group)
###   - grouping_vars: character vector for grouping variable names (for .by)
### Output:
###   - tibble summarizing min, max, mode, quartiles, and median of reinjury counts
###_____________________________________________________________________________
summarize_reinjury_stats <- function(df, grouping_vars = grouping_vars) {
  df |>
    dplyr::filter(n > 1) |> # retain only reinjuries (counts > 1)
    dplyr::summarize(
      Min_Reinjury = min(n, na.rm = TRUE),
      Max_Reinjury = max(n, na.rm = TRUE),
      Mode_Reinjury = stat_mode(n, na.rm = TRUE),
      Q25_Reinjury = stats::quantile(n, probs = 0.25, na.rm = TRUE, type = 7),
      Median_Reinjury = stats::quantile(n, probs = 0.5, na.rm = TRUE, type = 7),
      Q75_Reinjury = stats::quantile(n, probs = 0.75, na.rm = TRUE, type = 7),
      .by = dplyr::all_of(grouping_vars)
    )
}


###_____________________________________________________________________________
# Custom functions for counts ----
###_____________________________________________________________________________

{
  ###_____________________________________________________________________________
  ### Custom function to get unique count of injury events in IPOP claims dataset
  ### Optionally includes descriptive statistics based on reinjury patterns
  ###_____________________________________________________________________________
  ipop_injury_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = FALSE
  ) {
    # Capture the grouping variable(s) from ... as tidy symbols and strings
    # NOTE: This function is designed to work with only one grouping variable for now
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Ensure a valid value was supplied for `which` argument
    which <- match.arg(which, choices = c("Inpatient", "Outpatient", "IPOP"))

    #___________________________________________________________________________
    # Step 1: Apply filtering and distinct logic based on requested care setting
    #___________________________________________________________________________

    if (which == "Inpatient") {
      # Retain only inpatient records and deduplicate by patient + date
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization."
      )
    } else if (which == "Outpatient") {
      # Retain only outpatient records and deduplicate by patient + date
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to outpatient visits."
      )
    } else if (which == "IPOP") {
      # Use all records (inpatient + outpatient) and deduplicate by patient + date
      temp <- df |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization and outpatient visits."
      )
    }

    #___________________________________________________________________________
    # Step 2: If descriptive_stats = FALSE, return basic counts
    #___________________________________________________________________________
    if (!descriptive_stats) {
      return(temp |> dplyr::count(!!!grouping_syms))
    }

    #___________________________________________________________________________
    # Step 3: Calculate reinjury descriptive statistics when requested
    # Each "reinjury" is defined as a repeated injury event per patient per group
    #___________________________________________________________________________

    # 3a. Count number of injuries per patient per group, and filter to reinjuries
    stat <- temp |>
      dplyr::filter(!is.na(Patient_Linking)) |>
      dplyr::count(!!!grouping_syms, Patient_Linking) |>
      summarize_reinjury_stats(grouping_vars = grouping_vars)

    # 3b. Get counts of injury events by group and compute period-to-period changes
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics() |>
      dplyr::left_join(stat, by = grouping_vars)

    cli::cli_alert_success(
      "Returning the count(s) of unique injury events and reinjury statistics from the IPOP dataset."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of injury cases in IPOP claims dataset
  ### Analysis at the case level; reinjury not measured here
  ###_____________________________________________________________________________
  ipop_case_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = FALSE
  ) {
    # Capture grouping variable from ... (only one supported currently)
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Validate the 'which' argument to be one of the allowed values
    which <- match.arg(which)

    #___________________________________________________________________________
    # Step 1: Filter and deduplicate records based on the selected care setting
    # Unique cases defined by unique Patient_Linking, Hospital_Number, and Date_of_Service
    #___________________________________________________________________________
    if (which == "Inpatient") {
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        )

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases."
      )
    } else if (which == "Outpatient") {
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        )

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury outpatient visits."
      )
    } else if (which == "IPOP") {
      temp <- df |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        )

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury inpatient cases and injury outpatient visits."
      )
    }

    #___________________________________________________________________________
    # Step 2: Return simple counts or add proportional change when descriptive_stats = TRUE
    #___________________________________________________________________________
    if (!descriptive_stats) {
      return(temp |> dplyr::count(!!!grouping_syms))
    }

    # When descriptive_stats = TRUE, compute proportional change between rows
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics()

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of injured patients in IPOP claims dataset
  ### Analysis at the patient level; reinjury not measured here, only unique patients
  ###_____________________________________________________________________________
  ipop_patient_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = FALSE
  ) {
    # Capture grouping variable(s) from bare names (only one supported currently)
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Validate 'which' argument
    which <- match.arg(which)

    #___________________________________________________________________________
    # Step 1: Filter and deduplicate to get unique patients (within Year) per setting
    # Unique patient defined by Patient_Linking; Year grouping ensures per-year uniqueness
    #___________________________________________________________________________
    if (which == "Inpatient") {
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::group_by(Year) |> # ensure unique per year
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        dplyr::ungroup()

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an inpatient case event."
      )
    } else if (which == "Outpatient") {
      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::group_by(Year) |>
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        dplyr::ungroup()

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an outpatient visit."
      )
    } else if (which == "IPOP") {
      temp <- df |>
        dplyr::group_by(Year) |>
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        dplyr::ungroup()

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an inpatient case event or outpatient visit."
      )
    }

    #___________________________________________________________________________
    # Step 2: Produce final summary: simple counts or counts + change metrics
    #___________________________________________________________________________
    if (!descriptive_stats) {
      # Basic count of unique patients by the user-specified grouping
      return(temp |> dplyr::count(!!!grouping_syms))
    }

    # Descriptive stats: proportional change and label
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics()

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of injury cases in Patient Registry
  ### Equivalent to counting unique inpatient visits based on Unique_Incident_ID
  ### Supports dynamic grouping via tidy evaluation (bare column names in ...)
  ###_____________________________________________________________________________
  injury_case_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Create temporary dataset with unique incident rows
    # Unique incident defined by Unique_Incident_ID (one row per inpatient visit)
    temp <- df |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE)

    if (!descriptive_stats) {
      # Simple count of unique cases by user-defined grouping variables
      out <- temp |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, add change metrics (numeric and percent change)
    out <- temp |>
      dplyr::count(...) |>
      add_change_metrics()

    cli::cli_alert_success(
      "Returning the count(s) of total unique inpatient injury cases and descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of injuries in Patient Registry
  ### A true estimation of the number of incidents (not encounters or records)
  ###_____________________________________________________________________________
  injury_incident_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables from bare column names (e.g., Year, County)
    grouping_syms <- rlang::ensyms(...) # capture as symbols for tidy eval
    grouping_vars <- sapply(grouping_syms, rlang::as_string) # convert to character for .by and join

    # Create a temporary object with one row per unique injury event
    # A unique event is defined as a unique combination of Incident_Date and Unique_Patient_ID
    # This removes duplicated encounters for the same incident
    temp <- df |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (!descriptive_stats) {
      # Return a simple count of injury events by grouping variable(s)
      out <- temp |>
        dplyr::count(!!!grouping_syms)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury events leading to a trauma center visit."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, include additional summary statistics
    # This captures reinjury characteristics for patients who appear >1 time per group

    # Step 1: Identify reinjury patterns (count > 1 per group)
    stat <- temp |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
      summarize_reinjury_stats(grouping_vars = grouping_vars)

    # Step 2: Count events and calculate change metrics
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics() |>
      dplyr::left_join(stat, by = grouping_vars) # <- this fixes the join

    cli::cli_alert_success(
      "Returning the count(s) of total unique injury events leading to a trauma center visit and descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of patients in Patient Registry
  ### Estimates number of unique patients with trauma center visits per group
  ### Assumes Unique_Patient_ID is consistent across years (mostly)
  ### Groups first, then deduplicates per group to count unique patients correctly
  ### Supports flexible grouping via tidy evaluation (bare column names in ...)
  ###_____________________________________________________________________________
  injury_patient_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Prepare dataset: remove rows with missing patient IDs, group by user variables,
    # then select distinct Unique_Patient_ID within each group to avoid counting duplicates
    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::group_by(!!!grouping_syms) |>
      dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::ungroup()

    if (!descriptive_stats) {
      # Basic count of unique patients per group
      out <- temp |>
        dplyr::count(!!!grouping_syms)

      cli::cli_alert_success(
        "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, add change metrics for counts
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics()

    cli::cli_alert_success(
      "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID and descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of reinjured patients in Patient Registry
  ### Reinjury defined as patient with >1 injury event in a given year
  ### Counts patients with non-missing Unique_Patient_ID, counts reinjured patients by year
  ### Optionally returns descriptive statistics including reinjury counts, proportions, and change metrics
  ###_____________________________________________________________________________
  reinjury_patient_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Step 1: Prepare data - remove missing patient IDs to avoid overcounting
    # Deduplicate by Incident_Date and Unique_Patient_ID to get unique injury events
    # Count injury events per patient per year
    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
      dplyr::mutate(reinjury = n > 1) # flag reinjuries (more than 1 injury in year)

    # Step 2: Prepare reinjury descriptive summary
    if (!descriptive_stats) {
      # Return counts of unique reinjured patients per year
      out <- temp |>
        dplyr::summarize(
          reinjured_patients = sum(reinjury, na.rm = TRUE),
          .by = grouping_vars
        )

      cli::cli_alert_success(
        "Returning counts of patients with more than 1 injury event in a given year (non-null IDs)."
      )

      return(out)
    }

    # If descriptive_stats = TRUE, return counts plus proportions and change metrics
    stat <- temp |>
      dplyr::summarize(
        n = sum(reinjury, na.rm = TRUE),
        .by = grouping_vars
      )

    # Descriptive statistics on the counts
    summary <- temp |>
      dplyr::filter(reinjury) |>
      summarize_reinjury_stats(grouping_vars = grouping_vars)

    # get overall count of patients
    counts <- df |>
      injury_patient_count(!!!grouping_syms) |>
      dplyr::rename(n_patients = n)

    # compute overall statistics
    out <- stat |>
      dplyr::left_join(counts, by = grouping_vars) |> # join total patient counts by year
      add_change_metrics() |>
      dplyr::mutate(
        prop_reinjured = n / n_patients,
        prop_reinjured_label = traumar::pretty_percent(
          variable = prop_reinjured,
          n_decimal = 2
        )
      ) |>
      dplyr::rename(reinjured_patients = n) |>
      dplyr::left_join(summary, by = grouping_vars) # join reinjury descriptive stats

    cli::cli_alert_success(
      "Returning counts and proportions of reinjured patients with descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of reinjury *cases* in Patient Registry
  ### Reinjury case defined as a case (Unique_Incident_ID) belonging to a patient who
  ### had >1 injury event in that year.  Analysis is at the case level for reinjured patients.
  ### Optionally returns descriptive statistics including reinjury case counts, proportions,
  ### and change metrics.
  ###_____________________________________________________________________________
  reinjury_case_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    #___________________________________________________________________________
    # Step 0: Identify reinjured patients by year
    #   - Remove duplication of injury events (Incident_Date + Unique_Patient_ID)
    #   - Count injury events per patient per year
    #   - Flag patients with more than one injury in a year (reinjury)
    #   - Reduce to minimal table of reinjured status for join
    #___________________________________________________________________________
    init <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |> # drop missing patient IDs
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
      dplyr::mutate(reinjury = n > 1) # flag reinjured patients

    reinjured_patients <- init |>
      dplyr::select(!!!grouping_syms, Unique_Patient_ID, reinjury) |> # keep only relevant fields
      dplyr::distinct(!!!grouping_syms, Unique_Patient_ID, reinjury) # deduplicate just in case

    #___________________________________________________________________________
    # Step 1: Attach reinjury flag to the case-level dataset
    #   - Start from unique cases (one row per Unique_Incident_ID)
    #   - Join reinjured patient indicator (by Year + patient)
    #   - Deduplicate again to ensure case-level uniqueness
    #___________________________________________________________________________
    temp <- df |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
      dplyr::left_join(
        reinjured_patients,
        by = c(grouping_vars, "Unique_Patient_ID")
      ) |>
      dplyr::group_by(!!!grouping_syms) |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
      dplyr::ungroup()

    #___________________________________________________________________________
    # Step 2: If no descriptive statistics requested, return simple reinjury case counts
    #___________________________________________________________________________
    if (!descriptive_stats) {
      out <- temp |>
        dplyr::filter(reinjury == TRUE) |> # keep only cases for reinjured patients
        dplyr::count(!!!grouping_syms) # count reinjury cases per year

      cli::cli_alert_success(
        "Returning count(s) of injury cases of patients that had more than 1 injury event in a given year."
      )

      return(out)
    }

    #___________________________________________________________________________
    # Step 3: Descriptive statistics path
    # 3a. Reinforced reinjured patient case distribution (cases per reinjured patient)
    #     - Among reinjured patients only, count their case recurrence (n > 1) and summarize
    #___________________________________________________________________________
    reinjured_case_summary <- temp |>
      dplyr::filter(reinjury == TRUE, !is.na(Unique_Patient_ID)) |> # focus on reinjured patients with valid IDs
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |> # count cases per reinjured patient per year
      dplyr::filter(n > 1) |> # keep repeated cases per reinjured patient
      summarize_reinjury_stats(grouping_vars = grouping_vars)

    # 3b. Aggregate reinjury case-level counts and proportions by year
    stat <- temp |>
      dplyr::summarize(
        Reinjury_cases = sum(reinjury == TRUE, na.rm = TRUE), # total reinjury cases
        Total_cases = dplyr::n(), # all cases (including non-reinjured)
        prop = Reinjury_cases / Total_cases, # proportion of cases that are reinjury cases
        prop_label = traumar::pretty_percent(prop, n_decimal = 2),
        .by = grouping_vars
      ) |>
      dplyr::mutate(
        # Year-over-year proportional change in reinjury cases
        change = (Reinjury_cases - dplyr::lag(Reinjury_cases)) /
          dplyr::lag(Reinjury_cases),
        change_label = traumar::pretty_percent(change, n_decimal = 2)
      ) |>
      # 3c. Join the reinjured patient case recurrence summaries
      dplyr::left_join(reinjured_case_summary, by = grouping_vars)

    cli::cli_alert_success(
      "Returning count(s) and descriptive statistics of injury cases of patients that had more than 1 injury event in a given year."
    )

    return(stat)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of reinjury *injury events* in Patient Registry
  ### Reinjury defined as a patient having >1 unique injury event in a given year.
  ### This function aggregates injury events among those reinjured patients,
  ### optionally returning descriptive statistics including counts, proportions, and change.
  ###_____________________________________________________________________________
  reinjury_injury_count <- function(df, ..., descriptive_stats = FALSE) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    #___________________________________________________________________________
    # Step 0: Define unique injury events and flag reinjured patients
    #   - Deduplicate by Incident_Date + Unique_Patient_ID to get unique injury events
    #   - Count events per patient per year
    #   - Flag patients with more than one injury in a year (reinjury)
    #___________________________________________________________________________
    init <- df |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    patient_year <- init |>
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
      dplyr::mutate(reinjury = n > 1) # flag reinjured patients

    #___________________________________________________________________________
    # Non-descriptive path: total injury events among reinjured patients per year
    #___________________________________________________________________________
    if (!descriptive_stats) {
      out <- patient_year |>
        dplyr::filter(reinjury) |>
        dplyr::summarize(
          Reinjury = sum(n, na.rm = TRUE), # sum of injury events for reinjured patients
          .by = grouping_vars
        )

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events related to reinjury."
      )

      return(out)
    }

    #___________________________________________________________________________
    # Descriptive path
    # Step 1: Summary statistics of reinjury event counts among reinjured patients
    #   (distribution of number of injury events per reinjured patient)
    #___________________________________________________________________________
    summary_stats <- patient_year |>
      dplyr::filter(reinjury, !is.na(Unique_Patient_ID)) |> # focus on reinjured patients with valid IDs
      summarize_reinjury_stats(grouping_vars = grouping_vars) # min/max/median/mode/quartiles

    # Step 2: Total reinjury event counts per year (sum of n for reinjured patients)
    reinjury_events <- patient_year |>
      dplyr::filter(reinjury) |>
      dplyr::summarize(
        Reinjury = sum(n, na.rm = TRUE),
        .by = grouping_vars
      )

    # Step 3: Overall injury event counts per year (from original data)
    total_events <- df |>
      injury_incident_count(!!!grouping_syms) # mirrors original; includes all events (not deduped)

    # Step 4: Combine reinjury counts, total counts, compute proportions and change
    out <- reinjury_events |>
      dplyr::left_join(total_events, by = grouping_vars) |> # join total event counts (column `n`)
      dplyr::mutate(
        prop = Reinjury / n, # proportion of injury events that are reinjury-related
        prop_label = traumar::pretty_percent(prop, n_decimal = 2),
        change = (Reinjury - dplyr::lag(Reinjury)) / dplyr::lag(Reinjury), # year-over-year change
        change_label = traumar::pretty_percent(change, n_decimal = 2)
      ) |>
      dplyr::left_join(summary_stats, by = grouping_vars) # attach reinjury distribution summaries

    cli::cli_alert_success(
      "Returning counts and statistics of unique injury events."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Helper to add within-year case proportions and labels
  ### Input:
  ###   - df: tibble with at least columns Year and cases
  ### Behavior:
  ###   - For each Year, compute the share of `cases` among total cases that year
  ###   - Provide both numeric proportion and formatted label
  ###_____________________________________________________________________________
  case_mutate <- function(df, col, group) {
    out <- df |>
      dplyr::mutate(
        # Total cases in the same Year; protect against zero to avoid division by zero
        total_cases = sum({{ col }}, na.rm = TRUE),

        # Proportion of cases within the year; if total is zero, set NA
        percent = dplyr::if_else(
          total_cases > 0,
          {{ col }} / total_cases,
          NA_real_
        ),

        # Human-readable label from the proportion; suppress if percent is NA
        label = dplyr::if_else(
          !is.na(percent),
          traumar::pretty_percent(percent, n_decimal = 2),
          NA_character_
        ),

        # Operate by Year so sums and proportions are scoped correctly
        .by = {{ group }}
      ) |>
      # Drop the temporary total_cases column used for computation
      dplyr::select(-total_cases)

    return(out)
  }

  ###_____________________________________________________________________________
  ### Helper to add within-year injury event proportions and labels
  ### Input:
  ###   - df: tibble with at least columns Year and col
  ### Behavior:
  ###   - For each Year, compute the share of col and format it
  ###_____________________________________________________________________________
  injury_mutate <- function(df, col, group) {
    out <- df |>
      dplyr::mutate(
        # Total injury events in the same Year; guard against zero
        total_injuries = sum(
          {{ col }},
          na.rm = TRUE
        ),

        # Proportion of injury events within the year
        percent = dplyr::if_else(
          total_injuries > 0,
          {{ col }} /
            total_injuries,
          NA_real_
        ),

        # Label the proportion if available
        label = dplyr::if_else(
          !is.na(percent),
          traumar::pretty_percent(percent, n_decimal = 2),
          NA_character_
        ),

        .by = {{ group }}
      ) |>
      dplyr::select(-total_injuries)

    return(out)
  }
}
