# import libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gghighlight)
library(mdthemes)

# load raw data
df = read.csv("relay-foods.csv")

# adjust date formatting of OrderDate column
df$OrderDate = format(as.Date(df$OrderDate, '%m/%d/%Y'), '%Y/%m/%d')
df$OrderDate = gsub('00','20',df$OrderDate)

#c reate OrderMonth column from OrderDate (to  make monthly cohort)
df$OrderMonth = str_sub(df$OrderDate,end = 7)

# create reference data frame of cohort group (month level)
cohort_group_df = df %>% group_by(UserId) %>% summarize(CohortGroup = min(OrderDate))
cohort_group_df$CohortGroup = str_sub(cohort_group_df$CohortGroup,end = 7)

# join with initial df
df = inner_join(df, cohort_group_df, by = 'UserId')

# create reference data frame of total users for each cohort group
base_cohort_df = df %>% group_by(CohortGroup) %>%
  summarise(
    TotalUsers = n_distinct(UserId)
  )

# create purchase activity data frame
activity_cohort_df = df %>% group_by(CohortGroup, OrderMonth) %>% 
  summarise(
    BuyingUsers = n_distinct(UserId)
  )


# join activity_cohort_df and base_cohort_df
user_cohort_df = inner_join(activity_cohort_df, base_cohort_df, by = 'CohortGroup')

# substitute of purchase activity month (to become x-axis when plotting)
user_cohort_df = user_cohort_df %>% group_by(CohortGroup) %>%
  mutate(MonthNumber = 1:n())

# prepare subset of data frame to plot (to avoid clutter/overplotting)
plot_user_cohort_df = inner_join(base_cohort_df[seq(1,11,2),c("CohortGroup")], user_cohort_df, by = "CohortGroup")

# dummy column for in-place legend (gghighlight)
plot_user_cohort_df$dummy_col = 1

# plotting
ggplot(plot_user_cohort_df) +
  geom_line(aes(x = MonthNumber,
                y = BuyingUsers/TotalUsers,
                col = CohortGroup)) +
  gghighlight(dummy_col == 1) +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  mdthemes::md_theme_solarized() +
  labs(
    title = "**Monthly User Purchasing Cohort**",
    caption = "*Data: Relay Food order details (Source: github.com/ethen8181)*", 
    x = "K-th Month",
    y = "Retention"
  ) 
