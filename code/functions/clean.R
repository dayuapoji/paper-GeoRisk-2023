# CLEAN DATA ####

# find in ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

# set data frame ---------------------------------------------------------------

set_df <- function(df) {
  df <- df %>%
    # remove units
    .[-c(1), ] %>%
    lapply(., as.numeric) %>%
    as_tibble(.)
  return(df)
}

# replace zero -----------------------------------------------------------------

replace_zero <- function(x) {
  y <- x
  y[x == 0] <- NA
  return(y)
}

replace_zero_df <- function(df) {
  df <- lapply(df, replace_zero) %>% 
    as_tibble(.)
  return(df)
}

# replace negative -------------------------------------------------------------

replace_negative <- function(x) {
  y <- x
  y[x < 0] <- NA
  return(y)
}

replace_negative_df <- function(df) {
  df <- lapply(df, replace_negative) %>% 
    as_tibble(.)
  return(df)
}

# replace outliers -------------------------------------------------------------
replace_outliers <- function(x, na.rm = TRUE) {
  # interquartile method
  # Q1 <- quantile(x, probs = 0.25, na.rm = na.rm)
  # Q3 <- quantile(x, probs = 0.75, na.rm = na.rm)
  # IQR = Q3-Q1
  # 
  # lower_limit = Q1 - (IQR * 1.5)
  # upper_limit = Q3 + (IQR * 1.5)
  
  # standard deviation method
  upper_limit <- mean(x) + (5 * sd(x)) # 5 sd approx 96% of data
  
  y <- x
  # y[x < lower_limit] <- NA
  y[x > upper_limit] <- NA
  return(y)
}

replace_outliers_df <- function(df) {
  df <- lapply(df, replace_outliers) %>% 
    as_tibble(.)
  return(df)
}

# remove duplicates ------------------------------------------------------------

remove_duplicate_chainage <- function(df, sort) {
  # remove rows with duplicated chainage  
  if (sort == 'with ring') {
    df_result <- df %>%
      arrange(Chainage, Ring) %>%
      filter(duplicated(round(Chainage, 2)) == F)
  } else if (sort == 'without ring') {
    df_result <- df %>%
      arrange(Chainage) %>%
      filter(duplicated(round(Chainage, 2)) == F)
  }
  return(df_result)
}

# remove constant --------------------------------------------------------------

remove_constant_var <- function(df) {
  # remove columns with constant values  
  df <- df %>% .[ ,apply(., 2, var, na.rm=TRUE) != 0]
  return(df)
}

# standardize volume -----------------------------------------------------------

standardize_vol <- function(df) {
  # normalize volume to net stroke (m3/m)
  colnames <- df %>% select(contains('vol')) %>% names(.)
  for (i in colnames) {
    df[i] <- df[i] / (df['ThrustStrokeNet'] * 0.001)
  }
  return(df)
}

# create variable names --------------------------------------------------------

create_var_names <- function(name, num) {
  var_names <- NULL
  for (i in 1:num) {
    var_names <- append(var_names, paste0(name,i))
  }
  return(var_names)
}

# remove single measurement ----------------------------------------------------

remove_single_measurement <- function(df, feature = NULL, num) {
  for (i in 1:num) {
    df <- df %>% select(!matches(toString(paste0(feature, i))))
  }
  return(df)
}
