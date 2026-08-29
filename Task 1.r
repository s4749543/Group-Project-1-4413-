### 0. Packages and Setup ###

packages.install <- c("dplyr", "readr","tidyr", "ggplot2", "lubridate", "broom")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(broom)

### 1. Import and Clean Data ###
crsp_data <- read_csv("seo_sample_version_2_marketadj_crsp_data.csv", show_col_types = FALSE)
seo_events <- read_csv("seo_sample_version_2_marketadj.csv", show_col_types = FALSE)

# Inspect imported datasets
    # Number of observations and variables in each dataset
dim(seo_events)
dim(crsp_data)

    # Variable names in each dataset
names(seo_events)
names(crsp_data)

    # Preview each dataset
head(seo_events)
head(crsp_data)


 # Clean date variables
    # Convert dates to Date format for R
seo_events <- seo_events %>% mutate(filing_dt = dmy(filing_dt))
crsp_data <- crsp_data %>% mutate(DLYCALDT = dmy(DLYCALDT))

head(seo_events$filing_dt, 10)
sum(is.na(seo_events$filing_dt))


# Check for missing values in key variables
    # Assignment states there should be 500 SEO events ---> can rmove in the end
nrow(seo_events)

    # Number of unique firms in the SEO sample
n_distinct(seo_events$PERMNO)

    # Check whether any SEO_ID values are duplicated
sum(duplicated(seo_events$SEO_ID))

    # Check missing event dates
sum(is.na(seo_events$filing_dt))

    # Check missing PERMNOs
sum(is.na(seo_events$PERMNO))

# Sort CRSP data by PERMNO and DLYCALDT to ensure proper chronological order for event study analysis
crsp_data <- crsp_data %>%
  arrange(PERMNO, DLYCALDT)



### 2. Task 1: Identify day 0 ###
day0_dates <- seo_events %>%
  
  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    Issuer_Borrower_Name_Full
  ) %>%
  
  inner_join(
    crsp_data %>% select(PERMNO, DLYCALDT),
    by = join_by(
      PERMNO,
      filing_dt <= DLYCALDT
    ),
    relationship = "many-to-many"
  ) %>%
  
  group_by(SEO_ID) %>%
  
  slice_min(
    order_by = DLYCALDT,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup() %>%
  
  rename(day0_date = DLYCALDT)

# Add Day 0 back to the event dataset
seo_events_matched <- seo_events %>% left_join(day0_dates %>% select(SEO_ID, day0_date),by = "SEO_ID") %>%

    # Calendar-day difference is useful for checking whether
    # filing dates occur on weekends/holidays.
  mutate(
    day0_calendar_gap =
      as.integer(day0_date - filing_dt)
  )


# Preview the matched events
seo_events_matched %>%
  select(
    SEO_ID,
    PERMNO,
    Issuer_Borrower_Name_Full,
    filing_dt,
    day0_date,
    day0_calendar_gap
  ) %>%
  head(20)


# Check that every event has been assigned a Day 0
unmatched_events <- seo_events_matched %>%
  filter(is.na(day0_date))

nrow(unmatched_events)

# View unmatched events if any exist
unmatched_events

# Create a trading-day index for each firm
crsp_indexed <- crsp_data %>%
  arrange(PERMNO, DLYCALDT) %>%
  group_by(PERMNO) %>%
  mutate(
    trading_day_index = row_number()
  ) %>%
  ungroup()

# Find the trading-day index corresponding to Day 0
day0_index <- seo_events_matched %>%
  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    day0_date
  ) %>%

  left_join(
    crsp_indexed %>%
      select(
        PERMNO,
        DLYCALDT,
        trading_day_index
      ),
    by = c(
      "PERMNO",
      "day0_date" = "DLYCALDT"
    )
  ) %>%

  rename(
    day0_index = trading_day_index
  )
# Match every SEO to the firm's trading history

# Each SEO now receives a copy of the relevant firm's daily history. This is necessary because one PERMNO may have more 
    #than one SEO event, with each event having its own Day 0.
event_history <- seo_events_matched %>%

  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    day0_date,
    Issuer_Borrower_Name_Full,
    SDC_Deal_Number,
    Gross_Proceeds,
    utility_flag,
    financial_flag,
    realestate_flag,
    sample_version
  ) %>%

  inner_join(
    crsp_indexed,
    by = "PERMNO",
    relationship = "many-to-many"
  ) %>%

  left_join(
    day0_index %>%
      select(
        SEO_ID,
        day0_index
      ),
    by = "SEO_ID"
  ) %>%

  mutate(
    event_day = trading_day_index - day0_index
  )

event_history %>%
  count(SEO_ID, event_day) %>%
  filter(n > 1)

# 13. Calculate relative trading day
# ------------------------------------------------------------

# Relative trading day is:
#
#   current trading-day index - Day 0 trading-day index
#
# Thus:
#   event_day =  0  -> announcement day
#   event_day = -1  -> trading day immediately before
#   event_day = +1  -> trading day immediately after

event_history <- event_history %>%

  mutate(
    event_day =
      trading_day_index - day0_index
  )

# 14. Check the event-day alignment --> Can Remove
# ------------------------------------------------------------

# Every event should have exactly one Day 0 observation.

day0_check <- event_history %>%

  filter(event_day == 0) %>%

  count(SEO_ID, name = "number_day0_rows")


# All values should equal 1
table(day0_check$number_day0_rows)


# Check that the Day 0 CRSP date matches the assigned Day 0 date
day0_alignment_check <- event_history %>%

  filter(event_day == 0) %>%

  select(
    SEO_ID,
    filing_dt,
    day0_date,
    DLYCALDT
  ) %>%

  mutate(
    correctly_aligned =
      day0_date == DLYCALDT
  )

table(day0_alignment_check$correctly_aligned)

# 15. Inspect several events manually ----> Can remove
# ------------------------------------------------------------

# This is a useful sanity check before proceeding.

example_events <- seo_events$SEO_ID[1:3]

event_history %>%

  filter(
    SEO_ID %in% example_events,
    event_day >= -5,
    event_day <= 5
  ) %>%

  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    DLYCALDT,
    event_day,
    DLYRET,
    DLYPRC,
    SHROUT,
    SPRTRN
  ) %>%

  arrange(SEO_ID, event_day)

# ============================================================
# TASK 1 SUMMARY STATISTICS - CORRECTED VERSION
# ============================================================


# ------------------------------------------------------------
# A. Count SEO events per firm
# ------------------------------------------------------------

# IMPORTANT:
# Use PERMNO only as the firm identifier.
# Do not group by company name because the same PERMNO may appear
# under slightly different issuer names.

events_per_firm <- seo_events_matched %>%
  count(
    PERMNO,
    name = "number_of_SEOs"
  )


# Number of firms that completed more than one SEO
firms_multiple_events <- events_per_firm %>%
  filter(number_of_SEOs > 1)

number_firms_multiple_SEOs <- nrow(firms_multiple_events)


# Number of repeat SEO events beyond each firm's first SEO
repeat_SEO_events <- sum(
  events_per_firm$number_of_SEOs - 1
)


# ------------------------------------------------------------
# B. Tight-window completeness (-2, +2)
# ------------------------------------------------------------

tight_window_check <- event_history %>%
  filter(
    event_day >= -2,
    event_day <= 2
  ) %>%
  group_by(SEO_ID) %>%
  summarise(
    n_days = n_distinct(event_day),
    min_day = min(event_day),
    max_day = max(event_day),

    complete_tight =
      n_distinct(event_day) == 5 &
      min(event_day) == -2 &
      max(event_day) == 2,

    .groups = "drop"
  )

number_complete_tight <- sum(
  tight_window_check$complete_tight
)


# ------------------------------------------------------------
# C. Wide-window completeness (-10, +10)
# ------------------------------------------------------------

wide_window_check <- event_history %>%
  filter(
    event_day >= -10,
    event_day <= 10
  ) %>%
  group_by(SEO_ID) %>%
  summarise(
    n_days = n_distinct(event_day),
    min_day = min(event_day),
    max_day = max(event_day),

    complete_wide =
      n_distinct(event_day) == 21 &
      min(event_day) == -10 &
      max(event_day) == 10,

    .groups = "drop"
  )

number_complete_wide <- sum(
  wide_window_check$complete_wide
)


# ------------------------------------------------------------
# D. Distribution of SEO events by year
# ------------------------------------------------------------

events_by_year <- seo_events_matched %>%
  mutate(
    filing_year = lubridate::year(filing_dt)
  ) %>%
  count(
    filing_year,
    name = "number_of_events"
  ) %>%
  arrange(filing_year)


# ------------------------------------------------------------
# E. Main Task 1 summary table
# ------------------------------------------------------------

task1_summary <- tibble(

  Statistic = c(
    "Number of SEO events",
    "Number of unique firms",
    "Number of firms with more than one SEO",
    "Number of repeat SEO events beyond each firm's first SEO",
    "Number of usable events for tight window (-2,+2)",
    "Number of usable events for wide window (-10,+10)",
    "Events where filing date = Day 0",
    "Events where Day 0 occurs after filing date"
  ),

  Value = c(
    n_distinct(seo_events_matched$SEO_ID),

    n_distinct(seo_events_matched$PERMNO),

    number_firms_multiple_SEOs,

    repeat_SEO_events,

    number_complete_tight,

    number_complete_wide,

    sum(
      seo_events_matched$day0_calendar_gap == 0,
      na.rm = TRUE
    ),

    sum(
      seo_events_matched$day0_calendar_gap > 0,
      na.rm = TRUE
    )
  )
)


# ------------------------------------------------------------
# F. Add yearly event counts
# ------------------------------------------------------------

year_summary <- events_by_year %>%
  transmute(
    Statistic = paste0(
      "Number of events in ",
      filing_year
    ),
    Value = number_of_events
  )


# ------------------------------------------------------------
# G. Combine into final Task 1 summary
# ------------------------------------------------------------

task1_full_summary <- bind_rows(
  task1_summary,
  year_summary
)


# View final table
task1_full_summary

# ------------------------------------------------------------
# 24. Save the processed data
# ------------------------------------------------------------
write_csv(
  event_history,
  paste0(
    "outputs/task1_event_history_sample.csv"
  )
)

write_csv(
  task1_summary,
  paste0(
    "outputs/task1_summary_sample.csv"
  )
)

write_csv(
  events_by_year,
  paste0(
    "outputs/task1_events_by_year_sample.csv"
  )
)


# ============================================================
# TASK 2: CONSTRUCT EVENT WINDOWS
# ============================================================


# ------------------------------------------------------------
# 1. Construct the wide event window (-10, +10)
# ------------------------------------------------------------

wide_window <- event_history %>%
  filter(
    event_day >= -10,
    event_day <= 10
  ) %>%
  arrange(
    SEO_ID,
    event_day
  )


# Preview the wide window
wide_window %>%
  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    day0_date,
    DLYCALDT,
    event_day,
    DLYRET,
    SPRTRN
  ) %>%
  head(25)

# ------------------------------------------------------------
# 2. Construct the tight event window (-2, +2)
# ------------------------------------------------------------

tight_window <- event_history %>%
  filter(
    event_day >= -2,
    event_day <= 2
  ) %>%
  arrange(
    SEO_ID,
    event_day
  )


# Preview the tight window
tight_window %>%
  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    day0_date,
    DLYCALDT,
    event_day,
    DLYRET,
    SPRTRN
  ) %>%
  head(20)

  # ------------------------------------------------------------
# 3. Check completeness of tight window
# ------------------------------------------------------------

tight_window_check <- tight_window %>%
  group_by(SEO_ID) %>%
  summarise(

    # Number of unique event days
    n_event_days = n_distinct(event_day),

    # Earliest and latest event days
    min_event_day = min(event_day),
    max_event_day = max(event_day),

    # Check whether all five required days exist
    has_all_required_days =
      all(c(-2, -1, 0, 1, 2) %in% event_day),

    # Check whether stock returns are available
    no_missing_stock_returns =
      all(!is.na(DLYRET)),

    # Check whether market returns are available
    no_missing_market_returns =
      all(!is.na(SPRTRN)),

    .groups = "drop"
  ) %>%

  mutate(

    # Event is eligible only if the complete five-day
    # tight window is available
    complete_tight =
      n_event_days == 5 &
      min_event_day == -2 &
      max_event_day == 2 &
      has_all_required_days &
      no_missing_stock_returns &
      no_missing_market_returns
  )

  table(tight_window_check$complete_tight)

  number_complete_tight <- sum(
  tight_window_check$complete_tight
)

number_complete_tight


# ------------------------------------------------------------
# 4. Identify events with incomplete tight windows
# ------------------------------------------------------------

ineligible_tight_events <- tight_window_check %>%
  filter(!complete_tight)

ineligible_tight_events

nrow(ineligible_tight_events)

# ------------------------------------------------------------
# 5. Identify eligible SEO events
# ------------------------------------------------------------

eligible_events <- tight_window_check %>%
  filter(complete_tight) %>%
  select(SEO_ID)

  tight_window_final <- tight_window %>%
  semi_join(
    eligible_events,
    by = "SEO_ID"
  )

  n_distinct(tight_window_final$SEO_ID)
nrow(tight_window_final)

nrow(tight_window_final) ==
  n_distinct(tight_window_final$SEO_ID) * 5


  # ------------------------------------------------------------
# 6. Check completeness of wide window
# ------------------------------------------------------------

wide_window_check <- wide_window %>%
  group_by(SEO_ID) %>%
  summarise(

    n_event_days = n_distinct(event_day),

    min_event_day = min(event_day),
    max_event_day = max(event_day),

    has_all_required_days =
      all((-10:10) %in% event_day),

    no_missing_stock_returns =
      all(!is.na(DLYRET)),

    no_missing_market_returns =
      all(!is.na(SPRTRN)),

    .groups = "drop"
  ) %>%

  mutate(

    complete_wide =
      n_event_days == 21 &
      min_event_day == -10 &
      max_event_day == 10 &
      has_all_required_days &
      no_missing_stock_returns &
      no_missing_market_returns
  )

  table(wide_window_check$complete_wide)

  number_complete_wide <- sum(
  wide_window_check$complete_wide
)

number_complete_wide


wide_window_check %>%
  filter(!complete_wide)

  incomplete_wide_ids <- wide_window_check %>%
  filter(!complete_wide) %>%
  pull(SEO_ID)

wide_window %>%
  filter(
    SEO_ID %in% incomplete_wide_ids
  ) %>%
  select(
    SEO_ID,
    PERMNO,
    Issuer_Borrower_Name_Full,
    filing_dt,
    DLYCALDT,
    event_day,
    DLYRET,
    SPRTRN
  ) %>%
  arrange(
    SEO_ID,
    event_day
  )

# ------------------------------------------------------------
# 7. Check for duplicate event-day observations
# ------------------------------------------------------------

event_day_duplicates <- event_history %>%
  count(
    SEO_ID,
    event_day
  ) %>%
  filter(n > 1)

event_day_duplicates

nrow(event_day_duplicates)


# ------------------------------------------------------------
# 8. Task 2 summary table
# ------------------------------------------------------------

task2_summary <- tibble(

  Statistic = c(
    "Total SEO events",
    "Events with complete tight window (-2,+2)",
    "Events excluded due to incomplete tight window",
    "Events with complete wide window (-10,+10)",
    "Events with incomplete wide window (-10,+10)"
  ),

  Value = c(
    n_distinct(seo_events_matched$SEO_ID),

    sum(
      tight_window_check$complete_tight
    ),

    sum(
      !tight_window_check$complete_tight
    ),

    sum(
      wide_window_check$complete_wide
    ),

    sum(
      !wide_window_check$complete_wide
    )
  )
)

task2_summary


# ------------------------------------------------------------
# 9. Save Task 2 outputs
# ------------------------------------------------------------

write_csv(
  tight_window_final,
  "outputs/task2_tight_window.csv"
)

write_csv(
  wide_window,
  "outputs/task2_wide_window.csv"
)

write_csv(
  task2_summary,
  "outputs/task2_summary.csv"
)


# ============================================================
# TASK 3: ABNORMAL RETURNS AND CUMULATIVE ABNORMAL RETURNS
# Wide Event Window (-10, +10)
# ============================================================


# ------------------------------------------------------------
# 1. Calculate abnormal return for every event-day observation
# ------------------------------------------------------------

# AR_i,t = stock return - market return

wide_window <- wide_window %>%
  mutate(
    AR = DLYRET - SPRTRN
  )


# Inspect the result
wide_window %>%
  select(
    SEO_ID,
    PERMNO,
    event_day,
    DLYCALDT,
    DLYRET,
    SPRTRN,
    AR
  ) %>%
  arrange(SEO_ID, event_day) %>%
  head(25)

  # ------------------------------------------------------------
# 2. Calculate day-by-day abnormal-return statistics
# ------------------------------------------------------------

task3_table <- wide_window %>%

  group_by(event_day) %>%

  summarise(

    # Number of events contributing a valid abnormal return
    N = sum(!is.na(AR)),

    # Mean abnormal return
    mean_AR = mean(
      AR,
      na.rm = TRUE
    ),

    # Standard deviation of abnormal returns
    sd_AR = sd(
      AR,
      na.rm = TRUE
    ),

    # Standard error of mean AR
    se_AR = sd_AR / sqrt(N),

    # t-statistic for H0: mean AR = 0
    t_stat = mean_AR / se_AR,

    # Two-sided p-value
    p_value = 2 * pt(
      -abs(t_stat),
      df = N - 1
    ),

    .groups = "drop"
  ) %>%

  arrange(event_day) %>%

  # Cumulative average abnormal return
  mutate(
    CAR = cumsum(mean_AR)
  )

  task3_table

# ------------------------------------------------------------
# 3. Create report-ready Task 3 table
# ------------------------------------------------------------

task3_report_table <- task3_table %>%
  transmute(

    `Event Day` = event_day,

    `N` = N,

    `Mean AR (%)` = round(
      mean_AR * 100,
      3
    ),

    `t-statistic` = round(
      t_stat,
      3
    ),

    `p-value` = round(
      p_value,
      4
    ),

    `CAR (%)` = round(
      CAR * 100,
      3
    )
  )


task3_report_table

wide_window %>%
  count(SEO_ID, event_day) %>%
  filter(n > 1)

wide_window %>%
  group_by(event_day) %>%
  summarise(
    rows = n(),
    unique_events = n_distinct(SEO_ID)
  )

event_history %>%
  count(SEO_ID, event_day) %>%
  filter(n > 1)

