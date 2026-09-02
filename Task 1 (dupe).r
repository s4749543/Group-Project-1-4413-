# ============================================================
# FINM4413 GROUP ASSIGNMENT
# EVENT STUDY: SEASONED EQUITY OFFERINGS
#
# Tasks completed in this script:
#   Task 1 - Import data, identify Day 0, describe matched sample
#   Task 2 - Construct (-10,+10) and (-2,+2) event windows
#   Task 3 - Calculate abnormal returns and cumulative ARs
# ============================================================



## Need to:
    # Add summarys after each task (not at the end)
    # clean up code and remove unnecessary comments
    # clea up summary tables and make them more readable / presentable using kableExtra and other packages


# ============================================================
# 0. PACKAGES
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)


# ============================================================
# 1. FILE PATHS
# ============================================================
event_file <- "seo_sample_version_2_marketadj.csv"

crsp_file <- "seo_sample_version_2_marketadj_crsp_data.csv"


# ============================================================
# 2. IMPORT RAW DATA
# ============================================================

# Import SEO event-level data.
#
# filing_dt is initially forced to character because its raw
# format is DDMMMYYYY, e.g. "15NOV2016".

seo_events <- read_csv(
  event_file,
  show_col_types = FALSE,
  col_types = cols(
    filing_dt = col_character()
  )
)


# Import CRSP daily stock-return data.
#
# DLYCALDT is initially forced to character because its raw
# format is DD/MM/YYYY, e.g. "17/08/2015".

crsp_data <- read_csv(
  crsp_file,
  show_col_types = FALSE,
  col_types = cols(
    DLYCALDT = col_character()
  )
)


# ============================================================
# 3. CLEAN AND FORMAT DATES
# ============================================================

seo_events <- seo_events %>% mutate(filing_dt = dmy(filing_dt))
crsp_data <- crsp_data %>% mutate(DLYCALDT = dmy(DLYCALDT))


# ============================================================
# 4. INITIAL DATA CHECKS
# ============================================================

# Inspect column names
names(seo_events)
names(crsp_data)


# Confirm date conversion worked
head(seo_events$filing_dt)
head(crsp_data$DLYCALDT)


# Check for failed date conversions
sum(is.na(seo_events$filing_dt))
sum(is.na(crsp_data$DLYCALDT))


# Check sample size
nrow(seo_events)


# Number of unique firms
n_distinct(seo_events$PERMNO)


# Check SEO_ID uniqueness
seo_id_duplicates <- seo_events %>%
  count(SEO_ID) %>%
  filter(n > 1)

seo_id_duplicates


# ============================================================
# 5. CLEAN CRSP DUPLICATES
# ============================================================

# The supplied CRSP dataset contains a small number of exact
# duplicate rows.
#
# Remove only rows that are exact duplicates across all columns.

crsp_data <- crsp_data %>%
  distinct()


# Check that each PERMNO-date combination now appears once.

crsp_duplicate_check <- crsp_data %>%
  count(
    PERMNO,
    DLYCALDT
  ) %>%
  filter(n > 1)


crsp_duplicate_check


# Stop the script if duplicates remain.

if (nrow(crsp_duplicate_check) > 0) {
  stop(
    "Duplicate PERMNO-DLYCALDT observations remain in CRSP data."
  )
}


# ============================================================
# TASK 1
# IDENTIFY DAY 0 AND MATCH EVENT DATA
# ============================================================


# ============================================================
# 6. SORT CRSP DATA
# ============================================================

crsp_data <- crsp_data %>%
  arrange(
    PERMNO,
    DLYCALDT
  )


# ============================================================
# 7. IDENTIFY DAY 0
# ============================================================

# The assignment defines Day 0 as:
#
# the FIRST CRSP trading date on or after filing_dt.
#
# This handles filings occurring on weekends or market holidays.

day0_dates <- seo_events %>%

  select(
    SEO_ID,
    PERMNO,
    filing_dt,
    Issuer_Borrower_Name_Full
  ) %>%

  inner_join(
    crsp_data %>%
      select(
        PERMNO,
        DLYCALDT
      ),

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

  rename(
    day0_date = DLYCALDT
  )


# ============================================================
# 8. ADD DAY 0 TO EVENT DATA
# ============================================================

seo_events_matched <- seo_events %>%

  left_join(
    day0_dates %>%
      select(
        SEO_ID,
        day0_date
      ),
    by = "SEO_ID"
  ) %>%

  mutate(
    day0_calendar_gap =
      as.integer(
        day0_date - filing_dt
      )
  )


# ============================================================
# 9. CHECK THAT ALL EVENTS MATCHED
# ============================================================

unmatched_events <- seo_events_matched %>%
  filter(
    is.na(day0_date)
  )


nrow(unmatched_events)

unmatched_events


# ============================================================
# 10. INSPECT DAY 0 MATCHING
# ============================================================

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


# Distribution of gap between filing date and Day 0

table(
  seo_events_matched$day0_calendar_gap,
  useNA = "ifany"
)


# Events where filing date itself was not a trading day

seo_events_matched %>%

  filter(
    day0_calendar_gap > 0
  ) %>%

  select(
    SEO_ID,
    PERMNO,
    Issuer_Borrower_Name_Full,
    filing_dt,
    day0_date,
    day0_calendar_gap
  )


# ============================================================
# 11. CREATE TRADING-DAY INDEX
# ============================================================

# Trading days, not calendar days, define event time.
#
# Each firm's CRSP history is therefore numbered sequentially.

crsp_indexed <- crsp_data %>%

  arrange(
    PERMNO,
    DLYCALDT
  ) %>%

  group_by(PERMNO) %>%

  mutate(
    trading_day_index =
      row_number()
  ) %>%

  ungroup()


# ============================================================
# 12. FIND DAY 0 TRADING-DAY INDEX
# ============================================================

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
    day0_index =
      trading_day_index
  )


# ============================================================
# 13. BUILD FULL EVENT HISTORY
# ============================================================

# Every SEO receives the corresponding firm's CRSP trading history.
#
# A firm may have more than one SEO, so SEO_ID remains the
# event-level identifier.

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
  )


# ============================================================
# 14. CALCULATE RELATIVE EVENT DAY
# ============================================================

event_history <- event_history %>%

  mutate(
    event_day =
      trading_day_index - day0_index
  )


# ============================================================
# 15. VALIDATE EVENT-DAY ALIGNMENT
# ============================================================

# Every SEO should have exactly one Day 0.

day0_check <- event_history %>%

  filter(
    event_day == 0
  ) %>%

  count(
    SEO_ID,
    name = "number_day0_rows"
  )


table(
  day0_check$number_day0_rows
)


# Day 0 date should equal the CRSP observation date.

day0_alignment_check <- event_history %>%

  filter(
    event_day == 0
  ) %>%

  transmute(
    SEO_ID,
    filing_dt,
    day0_date,
    DLYCALDT,
    correctly_aligned =
      day0_date == DLYCALDT
  )


table(
  day0_alignment_check$correctly_aligned
)


# ============================================================
# 16. CHECK FOR DUPLICATE EVENT-DAY OBSERVATIONS
# ============================================================

event_day_duplicates <- event_history %>%

  count(
    SEO_ID,
    event_day
  ) %>%

  filter(
    n > 1
  )


event_day_duplicates


# Stop if any remain.

if (nrow(event_day_duplicates) > 0) {
  stop(
    "Duplicate SEO_ID-event_day observations detected."
  )
}


# ============================================================
# 17. COUNT EVENTS PER FIRM
# ============================================================

# PERMNO is the firm identifier.
#
# Do NOT group by issuer name because names may vary across
# observations for the same security.

events_per_firm <- seo_events_matched %>%

  count(
    PERMNO,
    name = "number_of_SEOs"
  )


# Firms with more than one SEO

firms_multiple_events <- events_per_firm %>%

  filter(
    number_of_SEOs > 1
  )


number_firms_multiple_SEOs <-
  nrow(firms_multiple_events)


# Repeat offerings beyond each firm's first offering.

repeat_SEO_events <- sum(
  events_per_firm$number_of_SEOs - 1
)


# ============================================================
# 18. EVENT DISTRIBUTION BY YEAR
# ============================================================

events_by_year <- seo_events_matched %>%

  mutate(
    filing_year =
      year(filing_dt)
  ) %>%

  count(
    filing_year,
    name = "number_of_events"
  ) %>%

  arrange(
    filing_year
  )


events_by_year


# ============================================================
# TASK 2
# CONSTRUCT EVENT WINDOWS
# ============================================================


# ============================================================
# 19. CREATE WIDE WINDOW (-10,+10)
# ============================================================

wide_window <- event_history %>%

  filter(
    event_day >= -10,
    event_day <= 10
  ) %>%

  arrange(
    SEO_ID,
    event_day
  )


# ============================================================
# 20. CREATE TIGHT WINDOW (-2,+2)
# ============================================================

tight_window <- event_history %>%

  filter(
    event_day >= -2,
    event_day <= 2
  ) %>%

  arrange(
    SEO_ID,
    event_day
  )


# ============================================================
# 21. CHECK TIGHT-WINDOW COMPLETENESS
# ============================================================

tight_window_check <- tight_window %>%

  group_by(SEO_ID) %>%

  summarise(

    n_event_days =
      n_distinct(event_day),

    min_event_day =
      min(event_day),

    max_event_day =
      max(event_day),

    has_all_required_days =
      all(
        c(-2, -1, 0, 1, 2) %in%
          event_day
      ),

    no_missing_stock_returns =
      all(
        !is.na(DLYRET)
      ),

    no_missing_market_returns =
      all(
        !is.na(SPRTRN)
      ),

    .groups = "drop"
  ) %>%

  mutate(

    complete_tight =

      n_event_days == 5 &

      min_event_day == -2 &

      max_event_day == 2 &

      has_all_required_days &

      no_missing_stock_returns &

      no_missing_market_returns
  )


table(
  tight_window_check$complete_tight
)


number_complete_tight <- sum(
  tight_window_check$complete_tight
)


# ============================================================
# 22. IDENTIFY INELIGIBLE TIGHT-WINDOW EVENTS
# ============================================================

ineligible_tight_events <- tight_window_check %>%

  filter(
    !complete_tight
  )


ineligible_tight_events


# ============================================================
# 23. CREATE FINAL ELIGIBLE TIGHT-WINDOW SAMPLE
# ============================================================

eligible_events <- tight_window_check %>%

  filter(
    complete_tight
  ) %>%

  select(
    SEO_ID
  )


tight_window_final <- tight_window %>%

  semi_join(
    eligible_events,
    by = "SEO_ID"
  )


# Check expected dimensions.

n_distinct(
  tight_window_final$SEO_ID
)

nrow(
  tight_window_final
)


# If all 500 events are eligible:
#
# 500 events x 5 days = 2,500 observations.

nrow(tight_window_final) ==
  n_distinct(tight_window_final$SEO_ID) * 5


# ============================================================
# 24. CHECK WIDE-WINDOW COMPLETENESS
# ============================================================

wide_window_check <- wide_window %>%

  group_by(SEO_ID) %>%

  summarise(

    n_event_days =
      n_distinct(event_day),

    min_event_day =
      min(event_day),

    max_event_day =
      max(event_day),

    has_all_required_days =
      all(
        (-10:10) %in% event_day
      ),

    no_missing_stock_returns =
      all(
        !is.na(DLYRET)
      ),

    no_missing_market_returns =
      all(
        !is.na(SPRTRN)
      ),

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


table(
  wide_window_check$complete_wide
)


number_complete_wide <- sum(
  wide_window_check$complete_wide
)


# Inspect incomplete wide-window events.

incomplete_wide_events <- wide_window_check %>%

  filter(
    !complete_wide
  )


incomplete_wide_events


# ============================================================
# 25. CHECK NUMBER OF EVENTS CONTRIBUTING BY EVENT DAY
# ============================================================

wide_window_counts <- wide_window %>%

  group_by(event_day) %>%

  summarise(

    rows =
      n(),

    unique_events =
      n_distinct(SEO_ID),

    .groups = "drop"
  )


wide_window_counts


# There should NEVER be more than 500 unique events.

if (
  any(
    wide_window_counts$unique_events > 500
  )
) {

  stop(
    "More than 500 events detected on an event day."
  )
}


# ============================================================
# 26. TASK 1 SUMMARY TABLE
# ============================================================

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

    n_distinct(
      seo_events_matched$SEO_ID
    ),

    n_distinct(
      seo_events_matched$PERMNO
    ),

    number_firms_multiple_SEOs,

    repeat_SEO_events,

    number_complete_tight,

    number_complete_wide,

    sum(
      seo_events_matched$
        day0_calendar_gap == 0,
      na.rm = TRUE
    ),

    sum(
      seo_events_matched$
        day0_calendar_gap > 0,
      na.rm = TRUE
    )
  )
)


# Add yearly observations.

year_summary <- events_by_year %>%

  transmute(

    Statistic =
      paste0(
        "Number of events in ",
        filing_year
      ),

    Value =
      number_of_events
  )


task1_full_summary <- bind_rows(

  task1_summary,

  year_summary
)


task1_full_summary


# ============================================================
# 27. TASK 2 SUMMARY TABLE
# ============================================================

task2_summary <- tibble(

  Statistic = c(

    "Total SEO events",

    "Events with complete tight window (-2,+2)",

    "Events excluded due to incomplete tight window",

    "Events with complete wide window (-10,+10)",

    "Events with incomplete wide window (-10,+10)"
  ),

  Value = c(

    n_distinct(
      seo_events_matched$SEO_ID
    ),

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


# ============================================================
# TASK 3
# ABNORMAL AND CUMULATIVE ABNORMAL RETURNS (-10,+10)
# ============================================================


# ============================================================
# 28. CALCULATE ABNORMAL RETURNS
# ============================================================

# Market-adjusted abnormal return:
#
# AR_i,t = DLYRET_i,t - SPRTRN_t

wide_window <- wide_window %>%

  mutate(
    AR =
      DLYRET - SPRTRN
  )


# Inspect calculated abnormal returns.

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

  arrange(
    SEO_ID,
    event_day
  ) %>%

  head(25)


# ============================================================
# 29. CREATE TASK 3 DAILY SUMMARY
# ============================================================

task3_table <- wide_window %>%

  group_by(event_day) %>%

  summarise(

    # Number of valid observations
    N =
      sum(
        !is.na(AR)
      ),

    # Mean abnormal return
    mean_AR =
      mean(
        AR,
        na.rm = TRUE
      ),

    # Standard deviation
    sd_AR =
      sd(
        AR,
        na.rm = TRUE
      ),

    # Standard error
    se_AR =
      sd_AR / sqrt(N),

    # One-sample t-statistic:
    # H0: mean AR = 0
    t_stat =
      mean_AR / se_AR,

    # Two-sided p-value
    p_value =
      2 * pt(
        -abs(t_stat),
        df = N - 1
      ),

    .groups = "drop"
  ) %>%

  arrange(event_day) %>%

  # Running cumulative average abnormal return
  mutate(
    CAR =
      cumsum(mean_AR)
  )


task3_table


# ============================================================
# 30. TASK 3 VALIDATION CHECKS
# ============================================================

# Should be 21 event days.

nrow(
  task3_table
)


# Should range from -10 to +10.

range(
  task3_table$event_day
)


# No event day should have N > 500.

if (
  any(
    task3_table$N > 500
  )
) {

  stop(
    "Task 3 contains more than 500 observations on an event day."
  )
}


# Inspect number of observations on each event day.

task3_table %>%

  select(
    event_day,
    N
  )


# ============================================================
# 31. CREATE REPORT-READY TASK 3 TABLE
# ============================================================

task3_report_table <- task3_table %>%

  mutate(

    significance = case_when(

      p_value < 0.01 ~ "***",

      p_value < 0.05 ~ "**",

      p_value < 0.10 ~ "*",

      TRUE ~ ""
    )
  ) %>%

  transmute(

    `Event Day` =
      event_day,

    `N` =
      N,

    `Mean AR (%)` =
      round(
        mean_AR * 100,
        3
      ),

    `t-statistic` =
      round(
        t_stat,
        3
      ),

    `p-value` =
      round(
        p_value,
        4
      ),

    `Significance` =
      significance,

    `CAR (%)` =
      round(
        CAR * 100,
        3
      )
  )


task3_report_table


# ============================================================
# 32. IDENTIFY STATISTICALLY SIGNIFICANT EVENT DAYS
# ============================================================

significant_days <- task3_table %>%

  filter(
    p_value < 0.05
  ) %>%

  select(
    event_day,
    N,
    mean_AR,
    t_stat,
    p_value,
    CAR
  )


significant_days


# ============================================================
# 33. INSPECT CENTRAL ANNOUNCEMENT WINDOW
# ============================================================

task3_report_table %>%

  filter(
    `Event Day` >= -2,
    `Event Day` <= 2
  )


# ============================================================
# 34. TASK 3 GRAPH - MEAN ABNORMAL RETURNS
# ============================================================

ar_plot <- ggplot(

  task3_table,

  aes(
    x = event_day,
    y = mean_AR * 100
  )

) +

  geom_col() +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title =
      "Mean Abnormal Returns Around SEO Announcements",

    x =
      "Event Day",

    y =
      "Mean Abnormal Return (%)"
  ) +

  scale_x_continuous(
    breaks = -10:10
  ) +

  theme_minimal()


ar_plot


# ============================================================
# 35. TASK 3 GRAPH - CUMULATIVE AVERAGE ABNORMAL RETURN
# ============================================================

car_plot <- ggplot(

  task3_table,

  aes(
    x = event_day,
    y = CAR * 100
  )

) +

  geom_line(
    linewidth = 1
  ) +

  geom_point() +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title =
      "Cumulative Average Abnormal Return Around SEO Announcements",

    x =
      "Event Day",

    y =
      "Cumulative Abnormal Return (%)"
  ) +

  scale_x_continuous(
    breaks = -10:10
  ) +

  theme_minimal()


car_plot


# ============================================================
# 36. SAVE OUTPUTS
# ============================================================

write_csv(
  task1_full_summary,
  "outputs/task1_full_summary.csv"
)


write_csv(
  task2_summary,
  "outputs/task2_summary.csv"
)


write_csv(
  wide_window,
  "outputs/task2_wide_window.csv"
)


write_csv(
  tight_window_final,
  "outputs/task2_tight_window_final.csv"
)


write_csv(
  task3_table,
  "outputs/task3_full_results.csv"
)


write_csv(
  task3_report_table,
  "outputs/task3_report_table.csv"
)


ggsave(
  "outputs/task3_mean_AR_plot.png",
  plot = ar_plot,
  width = 9,
  height = 5,
  dpi = 300
)


ggsave(
  "output/task3_CAR_plot.png",
  plot = car_plot,
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================================
# TASK 4
# ANNOUNCEMENT CAR AND OFFERING DILUTION
# ============================================================


# ============================================================
# 37. CALCULATE ABNORMAL RETURNS IN THE TIGHT WINDOW
# ============================================================

# AR_i,t = stock return - S&P 500 return

tight_window_final <- tight_window_final %>%
  mutate(
    AR = DLYRET - SPRTRN
  )


# ============================================================
# 38. CALCULATE EACH EVENT'S (-2,+2) ANNOUNCEMENT CAR
# ============================================================

# Each SEO should have exactly five observations:
# -2, -1, 0, +1, +2

event_CAR <- tight_window_final %>%
  group_by(SEO_ID) %>%
  summarise(

    PERMNO = first(PERMNO),

    Issuer_Borrower_Name_Full =
      first(Issuer_Borrower_Name_Full),

    filing_dt = first(filing_dt),

    Gross_Proceeds =
      first(Gross_Proceeds),

    utility_flag =
      first(utility_flag),

    financial_flag =
      first(financial_flag),

    realestate_flag =
      first(realestate_flag),

    announcement_CAR =
      sum(AR),

    .groups = "drop"
  )


# Inspect
head(event_CAR)


# ============================================================
# 39. TASK 4A - SUMMARY OF ANNOUNCEMENT CAR
# ============================================================

task4_CAR_summary <- event_CAR %>%
  summarise(

    N = n(),

    mean_CAR =
      mean(
        announcement_CAR,
        na.rm = TRUE
      ),

    median_CAR =
      median(
        announcement_CAR,
        na.rm = TRUE
      ),

    sd_CAR =
      sd(
        announcement_CAR,
        na.rm = TRUE
      ),

    se_CAR =
      sd_CAR / sqrt(N),

    t_stat =
      mean_CAR / se_CAR,

    p_value =
      2 * pt(
        -abs(t_stat),
        df = N - 1
      )
  )


task4_CAR_summary

# ============================================================
# 40. BUILT-IN ONE-SAMPLE T-TEST
# ============================================================

task4_ttest <- t.test(
  event_CAR$announcement_CAR,
  mu = 0,
  alternative = "two.sided"
)

task4_ttest

# ============================================================
# 41. EXTRACT MARKET EQUITY ON EVENT DAY -11
# ============================================================

market_equity_day11 <- event_history %>%

  filter(
    event_day == -11
  ) %>%

  transmute(

    SEO_ID,

    PERMNO,

    price_day_minus_11 =
      abs(DLYPRC),

    SHROUT_day_minus_11 =
      SHROUT,

    # SHROUT is reported in thousands of shares
    market_equity_day_minus_11 =
      abs(DLYPRC) * SHROUT * 1000
  )


# Check one row per SEO
market_equity_day11 %>%
  count(SEO_ID) %>%
  filter(n > 1)

nrow(market_equity_day11)

n_distinct(market_equity_day11$SEO_ID)

# ============================================================
# 42. MERGE CAR WITH PRE-ANNOUNCEMENT MARKET EQUITY
# ============================================================

task4_event_data <- event_CAR %>%

  left_join(
    market_equity_day11,
    by = c(
      "SEO_ID",
      "PERMNO"
    )
  )

# ============================================================
# 43. CALCULATE OFFERING DILUTION
# ============================================================

task4_event_data <- task4_event_data %>%

  mutate(

    dilution =
      (
        announcement_CAR *
        market_equity_day_minus_11
      ) /
      Gross_Proceeds
  )
  # Check Missing values
task4_event_data %>%
  summarise(
    missing_CAR =
      sum(is.na(announcement_CAR)),

    missing_market_equity =
      sum(is.na(market_equity_day_minus_11)),

    missing_gross_proceeds =
      sum(is.na(Gross_Proceeds)),

    zero_gross_proceeds =
      sum(
        Gross_Proceeds == 0,
        na.rm = TRUE
      )
  )


# ============================================================
# 44. TASK 4B - DILUTION SUMMARY
# ============================================================

task4_dilution_summary <- task4_event_data %>%

  filter(
    !is.na(dilution),
    is.finite(dilution)
  ) %>%

  summarise(

    N = n(),

    mean_dilution =
      mean(dilution),

    median_dilution =
      median(dilution),

    sd_dilution =
      sd(dilution),

    min_dilution =
      min(dilution),

    max_dilution =
      max(dilution)
  )


task4_dilution_summary

task4_report_summary <- tibble(

  Statistic = c(
    "Number of usable events",
    "Mean announcement CAR (%)",
    "Median announcement CAR (%)",
    "CAR t-statistic",
    "CAR p-value",
    "Mean offering dilution (%)",
    "Median offering dilution (%)"
  ),

  Value = c(

    task4_CAR_summary$N,

    task4_CAR_summary$mean_CAR * 100,

    task4_CAR_summary$median_CAR * 100,

    task4_CAR_summary$t_stat,

    task4_CAR_summary$p_value,

    task4_dilution_summary$mean_dilution * 100,

    task4_dilution_summary$median_dilution * 100
  )
)


task4_report_summary

# Small Gross Proceeds Relative to Firm Size
task4_event_data %>%
  arrange(dilution) %>%
  select(
    SEO_ID,
    Issuer_Borrower_Name_Full,
    Gross_Proceeds,
    market_equity_day_minus_11,
    announcement_CAR,
    dilution
  ) %>%
  head(10)


task4_event_data %>%
  arrange(desc(dilution)) %>%
  select(
    SEO_ID,
    Issuer_Borrower_Name_Full,
    Gross_Proceeds,
    market_equity_day_minus_11,
    announcement_CAR,
    dilution
  ) %>%
  head(10)

# ============================================================
# TASK 5
# INDUSTRIAL VS UTILITY ISSUERS
# ============================================================


# ============================================================
# 45. CREATE INDUSTRY GROUP LABEL
# ============================================================

task5_data <- task4_event_data %>%

  filter(
    !is.na(announcement_CAR),
    !is.na(utility_flag)
  ) %>%

  mutate(

    issuer_group =
      if_else(
        utility_flag == 1,
        "Utility",
        "Non-utility"
      )
  )

# ============================================================
# 46. GROUP SUMMARY STATISTICS
# ============================================================

task5_summary <- task5_data %>%

  group_by(issuer_group) %>%

  summarise(

    N = n(),

    mean_CAR =
      mean(
        announcement_CAR
      ),

    median_CAR =
      median(
        announcement_CAR
      ),

    sd_CAR =
      sd(
        announcement_CAR
      ),

    .groups = "drop"
  )


task5_summary

task5_report_table <- task5_summary %>%

  transmute(

    Group =
      issuer_group,

    N,

    `Mean CAR (%)` =
      round(
        mean_CAR * 100,
        3
      ),

    `Median CAR (%)` =
      round(
        median_CAR * 100,
        3
      ),

    `Standard Deviation (%)` =
      round(
        sd_CAR * 100,
        3
      )
  )


task5_report_table

# ============================================================
# 47. WELCH TWO-SAMPLE T-TEST
# ============================================================

task5_ttest <- t.test(

  announcement_CAR ~ issuer_group,

  data = task5_data,

  alternative = "two.sided",

  var.equal = FALSE
)


task5_ttest

task5_test_summary <- tibble(

  Statistic = c(
    "Welch t-statistic",
    "Degrees of freedom",
    "p-value"
  ),

  Value = c(
    unname(task5_ttest$statistic),
    unname(task5_ttest$parameter),
    task5_ttest$p.value
  )
)


task5_test_summary

task5_summary %>%
  select(
    issuer_group,
    N,
    sd_CAR
  )

# ============================================================
# 48. TASK 5 GRAPH
# ============================================================

task5_plot <- ggplot(

  task5_data,

  aes(
    x = issuer_group,
    y = announcement_CAR * 100
  )

) +

  geom_boxplot() +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title =
      "Announcement CARs: Utility vs Non-Utility Issuers",

    x =
      NULL,

    y =
      "Announcement CAR (-2,+2) (%)"
  ) +

  theme_minimal()


task5_plot

# ============================================================
# TASK 6
# OFFER SIZE AND MARKET REACTION
# ============================================================


# ============================================================
# 49. CALCULATE RELATIVE OFFERING SIZE
# ============================================================

task6_data <- task4_event_data %>%

  mutate(

    offer_size =
      Gross_Proceeds /
      market_equity_day_minus_11
  ) %>%

  filter(

    !is.na(announcement_CAR),

    !is.na(offer_size),

    is.finite(offer_size),

    offer_size > 0
  )

# Inspect It

task6_data %>%

  select(
    SEO_ID,
    Issuer_Borrower_Name_Full,
    Gross_Proceeds,
    market_equity_day_minus_11,
    offer_size,
    announcement_CAR
  ) %>%

  head(20)

# Descriptive statistics
task6_data %>%

  summarise(

    N = n(),

    mean_offer_size =
      mean(offer_size),

    median_offer_size =
      median(offer_size),

    min_offer_size =
      min(offer_size),

    max_offer_size =
      max(offer_size)
  )

# ============================================================
# 50. OLS REGRESSION
#
# CAR_i = alpha + beta * OfferSize_i + error_i
# ============================================================

task6_model <- lm(

  announcement_CAR ~ offer_size,

  data = task6_data
)


summary(task6_model)

library(broom)


task6_coefficients <- tidy(
  task6_model
)


task6_coefficients

# ============================================================
# 51. TASK 6 SCATTERPLOT AND OLS LINE
# ============================================================

task6_plot <- ggplot(

  task6_data,

  aes(
    x = offer_size * 100,
    y = announcement_CAR * 100
  )

) +

  geom_point(
    alpha = 0.5
  ) +

  geom_smooth(
    method = "lm",
    se = TRUE
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(

    title =
      "Relative Offering Size and SEO Announcement CAR",

    x =
      "Offer Size (% of Pre-Announcement Market Equity)",

    y =
      "Announcement CAR (-2,+2) (%)"
  ) +

  theme_minimal()


task6_plot

# Check for Extremes

task6_data %>%

  arrange(
    desc(offer_size)
  ) %>%

  select(
    SEO_ID,
    Issuer_Borrower_Name_Full,
    offer_size,
    announcement_CAR,
    Gross_Proceeds,
    market_equity_day_minus_11
  ) %>%

  head(20)

task6_diagnostics <- task6_data %>%

  mutate(
    cooks_distance =
      cooks.distance(task6_model)
  )


task6_diagnostics %>%

  arrange(
    desc(cooks_distance)
  ) %>%

  select(
    SEO_ID,
    Issuer_Borrower_Name_Full,
    offer_size,
    announcement_CAR,
    cooks_distance
  ) %>%

  head(10)

# ============================================================
# 52. SAVE TASKS 4-6 OUTPUTS
# ============================================================

write_csv(
  event_CAR,
  "outputs/task4_event_CAR.csv"
)


write_csv(
  task4_event_data,
  "outputs/task4_event_data.csv"
)


write_csv(
  task4_report_summary,
  "outputs/task4_summary.csv"
)


write_csv(
  task5_report_table,
  "outputs/task5_group_summary.csv"
)


write_csv(
  task5_test_summary,
  "outputs/task5_ttest.csv"
)


write_csv(
  task6_data,
  "outputs/task6_regression_data.csv"
)


write_csv(
  task6_report_table,
  "outputs/task6_regression_summary.csv"
)


ggsave(
  "outputs/task5_utility_comparison.png",
  plot = task5_plot,
  width = 7,
  height = 5,
  dpi = 300
)


ggsave(
  "outputs/task6_offer_size_regression.png",
  plot = task6_plot,
  width = 8,
  height = 5,
  dpi = 300
)
