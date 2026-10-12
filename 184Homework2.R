library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_clean <- sales_raw |>
  mutate(
    item_clean = stringr::str_to_lower(stringr::str_squish(stringr::str_trim(item_raw)))
  )

sales_clean <- sales_clean |>
  mutate(
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100
  )

sales_clean <- sales_clean |>
  mutate(
    net_amount = amount * (1 - discount_rate)
  )
sales_clean <- sales_clean |>
  mutate(
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )
sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

#partb

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)

invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(
    cols = code_clean,
    delim = "-",
    names = c("channel", "number")
  ) |>
  select(sale_id, channel, number, valid_code)

valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(
    cols = tags,
    delim = ";"
  )

sale_tags

# separate_wider_delim() splits delimited text into multiple columns,
# while separate_longer_delim() splits delimited text into multiple rows.

#partc
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (known_n == 0) {
      NA_real_
    } else {
      large_n / known_n
    },
    .by = customer_id
  ) |>
  arrange(customer_id)

customer_summary

# For customer C1, large_share = 0 / 1 = 0.
# The numerator is 0 because C1 has no known net amounts of at least 20.
# The denominator is 1 because C1 has one known net amount (10);
# the other sale has a missing net amount and is excluded.

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)

sales_factors |>
  count(item_factor)

levels(sales_factors$item_group)

sales_factors |>
  count(item_group)

# fct_infreq() changes the order of factor levels by frequency,
# while fct_lump_n() combines less frequent categories into "Other".

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Part A: Clean and add missing reports

# Part A(a): Fill missing customer IDs within each form.

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_labeled


# Part A(b): Clean amounts and use backup only when primary is missing.

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(
    form, line, customer_id, round,
    primary_amount, backup_amount, amount
  )

# A recorded primary amount, including zero, is kept because coalesce()
# uses the first non-missing value and only falls back to backup_amount
# when primary_clean is missing.


# Part A(c): Complete all customer-round combinations.

# Mark original rows before adding missing combinations.
reports_original <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE)

reports_complete <- reports_original |>
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )

reports_complete

# Count original rows, added rows, and missing amounts before completion.
original_row_count <- nrow(reports_original)
added_row_count <- 0
missing_amounts_before <- sum(is.na(reports_original$amount))

# Count original rows, added rows, and missing amounts after completion.
original_rows_after <- sum(reports_complete$recorded_row)
added_rows_after <- sum(!reports_complete$recorded_row)
missing_amounts_after <- sum(is.na(reports_complete$amount))

data.frame(
  stage = c("Before completion", "After completion"),
  original_rows = c(original_row_count, original_rows_after),
  added_rows = c(added_row_count, added_rows_after),
  missing_amounts = c(missing_amounts_before, missing_amounts_after)
)

# An explicit missing amount is the report for C3, round 1, whose amount
# remains NA because both primary_amount and backup_amount are missing.
# An implicit missing customer-round row is C1, round 2, which did not
# have a report in the original data.

# Part B(a): Check for duplicate or missing IDs and duplicate reports.

customer_id_checks <- customers |>
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = sum(duplicated(customer_id))
  )

customer_id_checks

duplicate_customer_rounds <- reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)

duplicate_customer_rounds


# Part B(b): Join customer metadata to reports.

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)


# Part B(c): Find IDs present in only one table.

reports_only_ids <- reports_clean |>
  distinct(customer_id) |>
  anti_join(
    customers_marked |> distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only_ids <- customers_marked |>
  distinct(customer_id) |>
  anti_join(
    reports_clean |> distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only_ids
customers_only_ids

# C4 has customer_found = FALSE because C4 is absent from customers.
# C3 has customer_found = TRUE even though its region is NA, because C3
# exists in customers but its region information is missing.

# Part C(b): Explain the join relationship.

# With relationship = "many-to-one", the join would raise an error if
# multiple rows in customers_bad matched the same customer ID in reports_clean.
# Allowing "many-to-many" permits multiple matches but does not resolve
# conflicting customer information or prevent duplicated reports.


# Part C(c): Reshape completed reports from long to wide format.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide

# Count the rows that remain when pivoting back to long form and
# dropping rows with missing amounts.
reports_long_nonmissing <- reports_wide |>
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  )

nrow(reports_long_nonmissing)

# Five rows should remain because five report amounts are known.
# Dropping missing values removes both newly added unknown reports and
# explicitly missing amounts; it cannot recover the original unknown values.

#problem 3
# Part A(a-b): Create the two-sample t-test function.

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  s2_x <- var(x)
  s2_y <- var(y)
  
  if (equal_var) {
    s2_p <- ((nx - 1) * s2_x + (ny - 1) * s2_y) /
      (nx + ny - 2)
    
    SE <- sqrt(s2_p * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
    
  } else {
    SE <- sqrt(s2_x / nx + s2_y / ny)
    
    df <- (s2_x / nx + s2_y / ny)^2 /
      ((s2_x / nx)^2 / (nx - 1) +
         (s2_y / ny)^2 / (ny - 1))
    
    method <- "Welch"
  }
  
  t_stat <- (x_bar - y_bar) / SE
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}


# Part A(c): Verify the function against t.test().

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)

welch_reference <- t.test(x_check, y_check, var.equal = FALSE)
pooled_reference <- t.test(x_check, y_check, var.equal = TRUE)

welch_result
pooled_result

welch_differences <- c(
  t_stat = abs(welch_result$t_stat - unname(welch_reference$statistic)),
  df = abs(welch_result$df - unname(welch_reference$parameter)),
  p_value = abs(welch_result$p_value - welch_reference$p.value)
)

pooled_differences <- c(
  t_stat = abs(pooled_result$t_stat - unname(pooled_reference$statistic)),
  df = abs(pooled_result$df - unname(pooled_reference$parameter)),
  p_value = abs(pooled_result$p_value - pooled_reference$p.value)
)

welch_differences
welch_differences < 1e-8

pooled_differences
pooled_differences < 1e-8

# The differences should all be smaller than 1e-8 if the calculations
# match the reference results to the required tolerance.
# Part B(a): Run the simulation.

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Set the seed once before the loops.
set.seed(2026)

# Allocate storage for all 8,000 results.
simulation_results <- data.frame(
  scenario = character(4 * B * 2),
  replicate = integer(4 * B * 2),
  method = character(4 * B * 2),
  t_stat = numeric(4 * B * 2),
  df = numeric(4 * B * 2),
  p_value = numeric(4 * B * 2),
  reject = logical(4 * B * 2)
)

row_index <- 1

for (i in 1:nrow(scenarios)) {
  for (b in 1:B) {
    
    x <- rnorm(
      n = n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n = n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i], b, welch$method,
      welch$t_stat, welch$df, welch$p_value, welch$reject
    )
    row_index <- row_index + 1
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i], b, pooled$method,
      pooled$t_stat, pooled$df, pooled$p_value, pooled$reject
    )
    row_index <- row_index + 1
  }
}

head(simulation_results)
nrow(simulation_results)


# Part B(b): Summarize simulation results.

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  )

simulation_summary |>
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )


# Part B(c): Short report.

# Under the null scenarios, rejection rates should be near 0.05, which is
# the intended Type I error rate, although random simulation variation
# can cause small differences. When the population means differ, rejection
# rates generally increase, indicating greater power to detect a difference.
# Higher power alone is not enough to judge a test because a test that
# rejects too often under the null may have an inflated Type I error rate.
# Comparing equal- and unequal-variance scenarios helps assess whether each
# method maintains an appropriate Type I error rate under different conditions.
