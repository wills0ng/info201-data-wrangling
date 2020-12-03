library(lintr)
# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - <https://4.files.edl.io/b9e2/07/12/19/142839-a23788fb-1d3a-4665-9dc4-33bfd442c296.pdf>


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.
ks_proj_df <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)
is.data.frame(ks_proj_df)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
colnames(ks_proj_df)

# - How many rows is the data frame?
nrow(ks_proj_df)

# - How many columns are in the data frame?
ncol(ks_proj_df)

# Use the `summary` function to get some summary information
summary(ks_proj_df)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type

get_col_info <- function(colname, df) {
  # Get the column contents
  # for the specified column in the dataframe
  col <- df[[colname]]

  # For integer-valued columns,
  # summarize by providing min, max, and mean
  if (typeof(col) == "integer") {
    return(list(
      min = min(col),
      max = max(col),
      mean = mean(col)
    ))
  # For character-valued columns...
  } else {
    # Get unique values vector
    # and number of unique values
    unique_values <- unique(col)
    n_values <- length(unique_values)

    # For columns with < 10 unique values
    # summarize by providing the
    # number of unique values
    # and a vector of the unique values themselves
    if (n_values < 10) {
      return(list(
        n_values = n_values,
        unique_values = unique_values
      ))
    # Else for columns >= 10 unique values
    # summarize by providing the
    # number of unique values
    # and a vector of a random sample of 10 unique values
    } else {
      return(list(
        n_values = n_values,
        sample_values = sample(unique_values, 10)
      ))
    }
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
currency_info <- get_col_info("currency", ks_proj_df)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  summary <- sapply(
    colnames(df),
    get_col_info,
    df,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  return(summary)
}


# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
ks_proj_df_summary <- get_summary_info(ks_proj_df)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# COMMENTS BELOW:
# 1. The highest pledged total amounted to 20+ million USD!
# 2. The mean of ks goals was $49080.79
# 3. There are only 14 different currencies that contributed to
# ks projects, considering how there are 180 different currencies in
# the world.
# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currency.


# What was the name of the project(s) with the highest goal?
ks_proj_df %>%
  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE))

# What was the category of the project(s) with the lowest goal?
ks_proj_df %>%
  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
ks_proj_df %>%
  filter(substr(deadline, 1, 4) == 2018) %>%
  nrow()

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
ks_proj_df %>%
  filter(state != "successful") %>%
  nrow() /
  ks_proj_df %>%
    nrow()

# What was the amount pledged for the project with the most backers?
ks_proj_df %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
ks_proj_df %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
  pull(name)

# How much total money was pledged to projects that weren't marked successful?
ks_proj_df %>%
  filter(state != "successful") %>% # get row
  pull(usd_pledged_real) %>% # get column
  sum(na.rm = TRUE)
  
# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
ks_proj_df %>%
  group_by(category) %>%
  summarise(amt_pledged = sum(usd_pledged_real)) %>%
  filter(amt_pledged == max(amt_pledged, na.rm = TRUE)) %>%
  pull(category)

# Which country had the most backers?
ks_proj_df %>%
  group_by(country) %>%
  summarise(backers = sum(backers)) %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
ks_proj_df %>%
  mutate(year_launched = substr(launched, 1, 4)) %>%
  group_by(year_launched) %>%
  summarise(pledged = sum(usd_pledged_real)) %>%
  filter(pledged == max(pledged)) %>%
  pull(year_launched)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# I felt like choosing launcehd dates for practice since the previous questions
# were deadlines

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
ks_proj_df %>%
  group_by(main_category) %>%
  summarise(backers = sum(backers)) %>%
  arrange(desc(backers)) %>%
  head(3) %>%
  pull(main_category)
  
# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
ks_proj_df %>%
  mutate(weekday = weekdays(as.Date(launched))) %>%
  group_by(weekday) %>%
  summarise(n = n()) %>%
  filter(n == max(n, na.rm = TRUE)) %>%
  pull(weekday)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"

ks_proj_df %>%
  mutate(weekday = weekdays(as.Date(launched))) %>%
  group_by(weekday) %>%
  summarise(n_launched = n(),
            n_successful = sum(state == 'successful'),
            prop_successful = n_successful / n_launched) %>%
  filter(prop_successful == min(prop_successful,
                                na.rm = TRUE)) %>%
  pull(weekday)
