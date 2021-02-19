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

# create OrderMonth column from OrderDate (to  make monthly cohort)
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

# dummy column for in-place legend (utilize gghighlight)
plot_user_cohort_df$dummy_col = 1

# plotting (line plot version)
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

# create base dataframe for heat map visualization
cohort_heatmap_df = user_cohort_df %>% select(CohortGroup, MonthNumber, BuyingUsers) %>%
          spread(MonthNumber, BuyingUsers)

# the percetage version of the dataframe
cohort_heatmap_df_pct = data.frame(
  cohort_heatmap_df$CohortGroup,
  cohort_heatmap_df[,2:ncol(cohort_heatmap_df)] / cohort_heatmap_df[["1"]]
)

# assign the same column names
colnames(cohort_heatmap_df_pct) = colnames(cohort_heatmap_df)

# melt the dataframes for plotting
plot_data_abs = gather(cohort_heatmap_df, "MonthNumber", "BuyingUsers", 2:ncol(cohort_heatmap_df))
plot_data_pct = gather(cohort_heatmap_df_pct, "MonthNumber", "Retention", 2:ncol(cohort_heatmap_df_pct))

# prepare label names containing absolute number of buyers for the first month
# and retention percentages for the rest months
label_names = c(plot_data_abs$BuyingUsers[1:(ncol(cohort_heatmap_df)-1)],
                plot_data_pct$Retention[(ncol(cohort_heatmap_df_pct)):(nrow(plot_data_pct))])

# beautify percentage labels.
beauty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") 
}

# create dataframe ready for plotting
plot_data = data.frame(
  CohortGroup = plot_data_pct$CohortGroup,
  MonthNumber = plot_data_pct$MonthNumber,
  Retention = plot_data_pct$Retention,
  Label = beauty_print(label_names)
)
plot_data$MonthNumber = as.numeric(plot_data$MonthNumber)

# plotting (heatmap version)
ggplot(plot_data) +
  geom_raster(aes(x = MonthNumber,
                  y = reorder(CohortGroup, desc(CohortGroup)),
                  fill = Retention)) +
  scale_fill_continuous(guide = FALSE, type = "gradient",
                        low = "deepskyblue", high = "darkblue") +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 1),
                    expand = c(0,0)) +
  geom_text(aes(x = MonthNumber,
                y = reorder(CohortGroup, desc(CohortGroup)),
                label = Label), col = "white") +
  mdthemes::md_theme_gray() +
  labs(
    title = "**Monthly User Purchasing Cohort**",
    caption = "*Data: Relay Food order details (Source: github.com/ethen8181)*", 
    x = "K-th Month",
    y = "Cohort Group"
  ) 

