library(tidyverse)
library(lubridate)
library(arrow)  # Modern replacement for feather
library(gtools)
library(lifecontingencies)

options(
  width = 80L,
  warn  = 1
)

set.seed(421)

source("lib_data_generation.R")


## ----initialise_parameters, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------
# N is the policy count you want to generate
N <- 1500000

# data_startdate is the first date at which we start creating policies
# data_snapshotdate is the date we take the policy 'snapshot'
data_startdate    <- as.Date('1990-01-01')
data_snapshotdate <- as.Date('2015-12-31')

yearchng_df <- tibble(
  year = c(1990, 1994, 1995, 1996, 1997, 1998, 1999, 2008, 2009, 2014),
  chng = c(   1,    1,    1,    1,    1,    1,    1,   -4,   -1,    1)
)

month_weight <- c(
  "Jan" =  3, "Feb" = 10, "Mar" = 10, "Apr" = 10, "May" = 10, "Jun" =  5,
  "Jul" =  5, "Aug" =  5, "Sep" =  7, "Oct" =  7, "Nov" =  7, "Dec" =  2
  )

dow_weight <- c(
  "Mon" = 5, "Tue" = 7, "Wed" = 7, "Thu" = 7, "Fri" = 5, "Sat" = 0, "Sun" = 0
  )

holiday_datestr <- c("0101", "0317", "1225", "1226")

# prem_freq is distributed such that almost all are monthly payments
# though a few are yearly
prem_freq_prop   <- c("12" = 0.9, "4" = 0.02, "1" = 0.08)
prem_freq_cutoff <- as.Date('2005-01-01')

# Select which cluster count we are using for our policy generation
use_cluster_level = 'n6'

# Parameters for the gamma distribution used to generate the prem_ape variable
# Protection policies will later be calculated for protection policies
premape_shape <- 1.1
premape_scale <- 500

# Set the gender proportion for the book and different smoking levels
# In order, the labels are S, Q, and N
male_prop   <- 0.7
smoker_prop <- list(
  "M" = c(0.30, 0.20, 0.50),
  "F" = c(0.25, 0.40, 0.35)
  )

# Set proportions for various protection policies
jointlife_prop       <- 0.3
lifeonly_prop        <- 0.75
large_amount_prop    <- 0.05

mortgage_status_prop <- c(
  "TERM"     = 0.4,
  "MORTDECR" = 0.5,
  "MORTFULL" = 0.1
  )

# Add 40% to the premium to account for profit margin and expenses
premium_expense_ratio <- 0.4

# Interest rate for discounting time value of money
interest_rate <- 0.04

# We load some lifetable and morbidity data to price the protection policies
load("data/lifemorb_tables.rda")

act_tables <- list(
  m_lifetable       = m_lifetable,
  f_lifetable       = f_lifetable,
  mn_morb_lifetable = mn_morb_lifetable,
  fn_morb_lifetable = fn_morb_lifetable
  )

output_csv      <- sprintf("df_policy_%08d.csv", N)
output_parquet  <- sprintf("df_policy_%08d.parquet", N)
output_json     <- sprintf("lifeins_policybook_%08d.json", N)
output_rds      <- sprintf("lifeins_policybook_%08d.rds", N)

output_intermediate_rds  <- sprintf("lifeins_policybook_%08d.rds", N)

print(paste(output_csv))


## ----load_datafiles, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------
# Load up the smallarea data that comes with the census GIS data
df_smallarea <- read_csv("data/smallarea_gisdb.csv") %>%
  mutate(sa_id = geogid)

# Load up the clustering info for the small areas
df_clustersmallarea <- read_csv("data/aai_censusclusterids.csv") %>%
  pivot_longer(cols = -sa_id, names_to = "variable", values_to = "value") %>%
  mutate(
    cluster_id = paste0(variable, "_c", value),
    cluster_level = variable
  ) %>%
  select(sa_id, cluster_level, cluster_id)

# Load the cluster proportions in the policy book
df_clusterpolicyprop <- read_csv("data/cluster_policy_proportions.csv")

# Load the cluster propensities for the four product types
df_clusterproductmapping <- read_csv("data/cluster_product_mapping.csv")

# Load product data
df_productdata    <- read_csv("data/product_input_data.csv")
df_productdurprop <- read_csv("data/product_duration_data.csv")


## ----generate_policy_ids, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
policyid_N <- 10^floor(log10(N))
policy_id <- sample((policyid_N*10):(policyid_N*100), N, replace = FALSE)
policy_id <- sprintf("C%09d", policy_id)


## ----create_cluster_data, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
df_smallarea <- df_smallarea %>%
  left_join(
    df_clustersmallarea %>% filter(cluster_level %in% use_cluster_level),
    by = 'sa_id'
  )

df_policyprop <- df_clusterpolicyprop %>%
  filter(cluster_level == use_cluster_level)

cluster_count <- nrow(df_policyprop)

cluster_idx <- sample(1:cluster_count, N, prob = df_policyprop$proportion, replace = TRUE)
cluster_id  <- df_policyprop$cluster_id[cluster_idx]

df_clusterprod <- df_clusterproductmapping %>%
  pivot_longer(
    cols = -c(cluster_level, cluster_id, cluster_name),
    names_to = "variable",
    values_to = "value"
  )

# Create policy data by sampling from each cluster
df_policy <- map_dfr(unique(cluster_id), ~{
  sample_cluster_data(.x, df_smallarea, df_clusterprod, cluster_id)
}) %>%
  mutate(policy_id = policy_id)


## ----generate_product_data, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------
# For each prod_type we create some data and then bind the tibbles together
unique_prod_types <- df_policy %>% arrange(prod_type) %>% pull(prod_type) %>% unique()

df_product <- map_dfr(unique_prod_types, ~{
  sample_product_data(.x, df_productdata,
                      premape_shape = premape_shape,
                      premape_scale = premape_scale)
}) %>%
  mutate(policy_id = rep(df_policy$policy_id, length.out = n()))

df_policy <- df_policy %>%
  select(-prod_type) %>%
  left_join(df_product, by = 'policy_id')


## ----generate_policy_startdates, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
allyears_df <- tibble(year = year(data_startdate):year(data_snapshotdate))

year_df <- allyears_df %>%
  left_join(yearchng_df, by = 'year') %>%
  mutate(
    chng = if_else(is.na(chng), 0, chng),
    weight = cumsum(chng)
  )

month_df <- tibble(
  month = names(month_weight),
  weight = month_weight
)

dow_df <- tibble(
  dow = names(dow_weight),
  weight = dow_weight
)

startdates <- generate_random_startdates(nrow(df_policy),
                                         data_startdate,
                                         data_snapshotdate,
                                         holiday_datestr,
                                         year_df,
                                         month_df,
                                         dow_df)

df_policy <- df_policy %>%
  mutate(
    policy_startdate = startdates,
    dob_life1 = policy_startdate - days(365 * policy_startage),
    dob_life1 = dob_life1 - days(sample(1:365, N, replace = TRUE) - 1)
  )


## ----generate_gender_smoker_data, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
# Select the prem_freq for RP policies
df_policy <- df_policy %>%
  mutate(
    prem_freq = case_when(
      prem_type == "RP" ~ sample(names(prem_freq_prop), n(),
                                 replace = TRUE, prob = prem_freq_prop),
      TRUE ~ prem_freq
    )
  ) %>%
  mutate(
    prem_freq = case_when(
      prem_type == 'RP' & prem_freq == '4' & policy_startdate >= prem_freq_cutoff ~ '12',
      TRUE ~ prem_freq
    )
  )

# Create gender and gender-related variables
df_policy <- df_policy %>%
  mutate(
    gender_life1 = sample(c("M", "F"), n(), replace = TRUE,
                          prob = c(male_prop, 1 - male_prop)),
    smoker_life1 = case_when(
      gender_life1 == 'M' ~ sample(c("S", "Q", "N"), n(), replace = TRUE,
                                   prob = smoker_prop[["M"]]),
      gender_life1 == 'F' ~ sample(c("S", "Q", "N"), n(), replace = TRUE,
                                   prob = smoker_prop[["F"]])
    )
  )


## ----determine_policy_duration, echo=TRUE---------------------------------------------------------------------------------------------------------------------------
df_duration <- df_policy %>%
  group_by(prod_type) %>%
  group_modify(~{
    use_df <- df_productdurprop %>% filter(prod_type == .y$prod_type)

    if(nrow(use_df) > 0) {
      policy_duration <- sample(use_df$duration, nrow(.x), replace = TRUE, prob = use_df$prop)
    } else {
      policy_duration <- rep(as.integer(NA), nrow(.x))
    }

    .x %>% mutate(policy_duration = policy_duration)
  }) %>%
  ungroup()

df_policy <- df_policy %>%
  left_join(df_duration %>% select(policy_id, policy_duration), by = 'policy_id') %>%
  mutate(policy_duration = if_else(policy_duration == -1, NA_integer_, policy_duration))

# Add some sanity checks for policy holder age
df_policy <- df_policy %>%
  mutate(
    policy_duration = case_when(
      prod_type == 'protection' & policy_startage > 65 ~ 5L,
      prod_type == 'protection' & (policy_startage + policy_duration > 75) ~
        as.integer(75 - policy_startage),
      prod_type == 'protection' & policy_startdate < as.Date('2002-01-01') &
        policy_duration > 20 ~ 20L,
      TRUE ~ policy_duration
    )
  )

# Set policy end dates
df_policy <- df_policy %>%
  mutate(
    policy_enddate = case_when(
      is.na(policy_duration) ~ dob_life1 + years(120),
      TRUE ~ policy_startdate + years(policy_duration)
    )
  )

# Create plot for protection policies
df_policy %>%
  filter(prod_type == 'protection', policy_startage >= 55) %>%
  ggplot() +
  geom_point(aes(x = policy_startage, y = policy_duration))

# Add extra data for mortgage-linked protection policies
df_policy <- df_policy %>%
  mutate(
    mortgage_status = case_when(
      prod_type == 'protection' ~ sample(names(mortgage_status_prop), n(),
                                         replace = TRUE, prob = mortgage_status_prop),
      TRUE ~ mortgage_status
    ),
    isjointlife = case_when(
      prod_type == 'protection' ~ rbinom(n(), 1, jointlife_prop) == 1,
      TRUE ~ NA
    ),
    islifeonly = case_when(
      prod_type == 'protection' ~ rbinom(n(), 1, lifeonly_prop) == 1,
      TRUE ~ NA
    )
  )

# Add mortality rating to protection policies
df_policy <- df_policy %>%
  group_by(cluster_id) %>%
  mutate(
    mort_rating = case_when(
      prod_type == 'protection' ~ {
        cluster_info <- df_clusterpolicyprop %>% filter(cluster_id == first(cluster_id))
        calculate_mort_rating(n(), cluster_info$mort_shape, cluster_info$mort_scale)
      },
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    mort_rating = case_when(
      prod_type == 'protection' & smoker_life1 == "S" ~ mort_rating + 100,
      prod_type == 'protection' & smoker_life1 == "Q" ~ mort_rating + 50,
      TRUE ~ mort_rating
    )
  )

# Display first few rows
df_policy %>% slice_head(n = 3) %>% t()


## ----calculate_policy_premia, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------
df_policy <- df_policy %>%
  mutate(policy_startage = as.numeric(floor(as.numeric(policy_startdate - dob_life1) / 365)))

# Calculate premium factors for protection policies
df_policy <- df_policy %>%
  group_by(policy_id) %>%
  mutate(
    premium_factor = case_when(
      prod_type == 'protection' ~ calculate_protection_premium(
        policy_startage, policy_duration, isjointlife, islifeonly,
        acttables = act_tables, interest_rate = interest_rate
      ),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Calculate sum assured and risk premium
df_policy <- df_policy %>%
  mutate(
    sum_assured = case_when(
      prod_type == 'protection' ~ {
        base_amount <- 100000 + 50000 * sample(0:8, n(), replace = TRUE)
        if_else(runif(n()) < large_amount_prop, base_amount * 10, base_amount)
      },
      TRUE ~ NA_real_
    ),
    prem_risk = case_when(
      prod_type == 'protection' ~ sum_assured * premium_factor * mort_rating / 100,
      TRUE ~ NA_real_
    ),
    prem_risk = case_when(
      prod_type == 'protection' & mortgage_status == 'MORTDECR' ~ prem_risk * 0.7,
      TRUE ~ prem_risk
    )
  )


## ----add_expenses_profit_margin, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
# Add expenses and profit margin
df_policy <- df_policy %>%
  mutate(
    prem_ape = case_when(
      prod_type == 'protection' ~ round(prem_risk * (1 + premium_expense_ratio), 2),
      TRUE ~ prem_ape
    )
  )

# Check for policies with missing duration
df_policy %>%
  filter(prod_type == 'protection', is.na(policy_duration)) %>%
  nrow()


## ----generate_geographical_data, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
# Add geographical information
df_policy <- df_policy %>%
  left_join(
    df_smallarea %>% select(sa_id, countyname, edname, nuts3name),
    by = 'sa_id'
  )

df_output <- df_policy %>%
  select(
    policy_id, countyname, edname, nuts3name, sa_id, cluster_id, prod_type,
    prem_type, prem_freq, prem_ape, prem_risk, policy_startdate, policy_enddate,
    policy_duration, mort_rating, sum_assured, dob_life1, gender_life1,
    smoker_life1, isjointlife, islifeonly, mortgage_status
    ) %>%
  arrange(policy_id)

# Calculate median premium by product type
df_output %>%
  group_by(prod_type) %>%
  summarise(median_prem_ape = median(prem_ape, na.rm = TRUE))

# Create density plot for protection policies
df_output %>%
  filter(prod_type == 'protection') %>%
  ggplot() +
    geom_density(aes(x = prem_ape))


## ----output_data, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------
# Output data in various formats
write_parquet(df_output, sink = paste0('output/', output_parquet))

