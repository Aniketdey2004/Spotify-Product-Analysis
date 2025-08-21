library(forcats)
library(dplyr)
library(corrplot)
library(plotly)
library(tidyr)
getwd()
df<-read.csv("D:/rproject/Spotify-Product-Analysis/Spotify analysis/data.csv");

# Convert Age into factor with proper order
df$Age <- factor(df$Age, 
                 levels = c("15-20", "21-30", "31-40", "Above 40"), 
                 labels = c(0, 1, 2, 3))

# Convert factor to numeric (if you want integer encoding)
df$Age <- as.numeric(as.character(df$Age))

# View the first few rows
head(df$Age)
View(df)
df$Gender <- factor(df$Gender,
                    levels = c("Male", "Female", "Prefer not to say", "Other"),
                    labels = c(0, 1, 2, 3))

# Convert factor to numeric
df$Gender <- as.numeric(as.character(df$Gender))

df$Area.of.residence <- factor(df$Area.of.residence,
                               levels = c("Urban", "Suburban", "Rural","other"),
                               labels = c(0, 1, 2, 3))

# Convert factor to numeric
df$Area.of.residence <- as.numeric(as.character(df$Area.of.residence))

df$Education.level <- factor(df$Education.level,
                             levels = c("School (class 10-12)", 
                                        "Undergraduate", 
                                        "Postgraduate", 
                                        "Working Professional", 
                                        "Other"),
                             labels = c(0, 1, 2, 3, 4))

# Convert factor to numeric
df$Education.level <- as.numeric(as.character(df$Education.level))

# Encode 'How many hours per day do you listen to music?'
df$How.many.hours.per.day.do.you.listen.to.music. <- factor(df$How.many.hours.per.day.do.you.listen.to.music.,
                           levels = c("Less than 1 hour", 
                                      "1-2 Hours", 
                                      "2-4 Hours", 
                                      "More than 4 Hours"),
                           labels = c(0, 1, 2, 3))

# Convert factor to numeric
df$How.many.hours.per.day.do.you.listen.to.music. <- as.numeric(as.character(df$How.many.hours.per.day.do.you.listen.to.music.))

unique(df$Your.favorite.time.to.listen.to.music.)

df$Your.favorite.time.to.listen.to.music. <- factor(
  df$Your.favorite.time.to.listen.to.music.,
  levels = c("While commuting",
             "Travelling ",
             "While Driving",
             "While studying",
             "At work",
             "While written ",
             "While doing shit like writing lab ",
             "Before sleep",
             "During workouts",
             "before sleep and during workouts",
             "Preferably, Always ",
             "all the time",
             "Anytime",
             "Music",
             "Depends on my mood",
             "I prefer having a separate time allocated for listening to music"),
  labels = c(0, 0, 0,  # Levels 1-3 -> Label 0
             1, 1, 1, 1, # Levels 4-7 -> Label 1
             2, 2, 2,    # Levels 8-10 -> Label 2
             3, 3, 3, 3, # Levels 11-14 -> Label 3
             4,          # Level 15 -> Label 4
             5)          # Level 16 -> Label 5
)

# Check unique values first (to confirm all categories)
unique(df$Do.you.pay.for.your.preferred.music.platform.)

# Encode the column
df$Do.you.pay.for.your.preferred.music.platform. <- factor(df$Do.you.pay.for.your.preferred.music.platform.,
                       levels = c("Yes", 
                                  "No, I prefer free version with ads",
                                  "No, I prefer downloading music",
                                  "Yes if the platform is exceptionally good",
                                  "No, I prefer version with ads",
                                  "Pirated app with no ads"
                                  ),
                       labels = c(0, 1,2,3,4,5))

# Convert factor to numeric
df$Do.you.pay.for.your.preferred.music.platform. <- as.numeric(as.character(df$Do.you.pay.for.your.preferred.music.platform.))

df$If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.<- factor(df$If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.,
                            levels = c("Yes", "No", "Maybe", ""),
                            labels = c(0, 1, 2, 3))

# Convert factor to numeric
df$If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.<- as.numeric(as.character(df$If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.))


# Find the 5 "Rate yourself on the following statements" columns
cols <- grep("^Rate.yourself.on.the.following.statements", names(df), value = TRUE)

# Encode them in place
for (col in cols) {
  df[[col]] <- factor(df[[col]],
                      levels = c("Strongly disagree", 
                                 "Disagree", 
                                 "Neutral", 
                                 "Agreed", 
                                 "Strongly agree"),
                      labels = c(0, 1, 2, 3, 4))
  
  # Convert factor to numeric (overwrite existing column)
  df[[col]] <- as.numeric(as.character(df[[col]]))
}

# the column in place
df$Do.you.associate.specific.genres.with.specific.moods. <- factor(
  df$Do.you.associate.specific.genres.with.specific.moods.,
  levels = c("Yes, strongly", "Sometimes", "No","Never thought about it", ""),
  labels = c(0, 1, 2, 3, 4)
)

# Convert factor to numeric
df$Do.you.associate.specific.genres.with.specific.moods. <- 
  as.numeric(as.character(df$Do.you.associate.specific.genres.with.specific.moods.))

cat("Encoding 'Proportion.on.music' column...\n")
proportion_levels <- c("None", "25%", "25-50%","50%", "More than 50%","NA")
df$What.proportion.of.your.entertainment.budget.goes.to.music.related.spending. <- as.numeric(factor(df$What.proportion.of.your.entertainment.budget.goes.to.music.related.spending., levels = proportion_levels, ordered = TRUE))

cat("Encoding 'Monthly.budget' column...\n")
budget_levels <- c("None", "0-199", "200-499", "500-1000", "More than 1000")
df$What.is.your.monthly.budget.for.leisure.or.entertainment..Rs. <- as.numeric(factor(df$What.is.your.monthly.budget.for.leisure.or.entertainment..Rs., levels=budget_levels,ordered = TRUE))

cat("Encoding 'Spotify.raised.Premium' column...\n")
spotify_levels <- c("No", "I use free platforms anyway", "Yes but reluctantly", "Yes, happily")
spotify_col_name <- "If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay."
df[[spotify_col_name]] <- as.numeric(factor(df[[spotify_col_name]], levels = spotify_levels, ordered = TRUE))
View(df)

cat("Encoding 'Concert.or.headphones' column...\n")
concert_col_name <- "You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones."
df[[concert_col_name]] <- as.numeric(as.factor(df[[concert_col_name]]))
View(df)

cat("Encoding 'Influence.on.spending' column...\n")
influence_col_name <- "What.influences.your.decision.to.spend.money.on.music.the.most."
df[[influence_col_name]] <- as.numeric(as.factor(df[[influence_col_name]]))
View(df)

unique(df$Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.)
df$Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world. <- factor(df$Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.)
df$Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world. <- fct_collapse(
  df$Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.,
  "0" = c("", "na", "No"),
  "1" = c("Private", "Private ", "It is my commodity ", "Private. But it shouldn't be."),
  "2" = c("Public", "public", "Public ", "Public Commodity ", "Public commodity", "Yes", "Absolutely yes. People nowadays should attend. It's important to build up a community.",
          "It should be made public", "According to me music is a public commodity in today's digital world .",
          "Public commodity. Also Spotify. Stop paying for your Spotify subscription. "),
  "3" = c("Both", "Both based on context ", "Public private according to the surrounding ")
)

df <- df[, -c(1, 17, 32)]
View(df)

df_numeric <- as.data.frame(lapply(df, function(x) {
  as.numeric(as.character(x))
}))

# Now calculate correlation
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Basic heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        symm = TRUE)

colnames(df)

response_colors <- c("1" = "red", "2" = "orange", "3" = "lightblue", "4" = "green")
response_counts <- df %>%
  count(If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  mutate(Percentage = n / sum(n) * 100)

response_plot <- plot_ly(
  data = response_counts,
  x = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  y = ~n,
  type = "bar",
  marker = list(color = ~response_colors[If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.]),
  text = ~paste("Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Distribution of Responses to Spotify Price Increase<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Response Code"),
    yaxis = list(title = "Count")
  )

response_plot

age_plot <- plot_ly(
  data = df,
  x = ~as.numeric(as.character(Age)),
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "box",
  boxpoints = "all",
  jitter = 0.3,
  pointpos = 0,
  hoverinfo = "x+y+name"
) %>%
  layout(
    title = "Age Distribution by Payment Response Code<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Age"),
    yaxis = list(title = "Response Code"),
    boxmode = "group"
  )

age_plot

gender_data <- df %>%
  count(Gender, If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Gender) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()
gender_plot <- plot_ly(
  data = gender_data,
  x = ~Gender,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Gender:", Gender, "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Gender Distribution by Payment Response Code<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Gender"),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

gender_plot

education_data <- df %>%
  count(Education.level, If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Education.level) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

education_plot <- plot_ly(
  data = education_data,
  x = ~Education.level,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Education:", Education.level, "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Education Level by Payment Response Code<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Education Level", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

education_plot

residence_data <- df %>%
  count(Area.of.residence, If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Area.of.residence) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

residence_plot <- plot_ly(
  data = residence_data,
  x = ~Area.of.residence,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Residence:", Area.of.residence, "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Area of Residence by Payment Response Code<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Area of Residence", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

residence_plot

music_hours_data <- df %>%
  count(How.many.hours.per.day.do.you.listen.to.music., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(How.many.hours.per.day.do.you.listen.to.music.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

music_hours_plot <- plot_ly(
  data = music_hours_data,
  x = ~How.many.hours.per.day.do.you.listen.to.music.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Hours:", How.many.hours.per.day.do.you.listen.to.music., "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Daily Music Listening Hours by Payment Response Code<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Hours per Day", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

music_hours_plot

payment_platform_data <- df %>%
  count(Do.you.pay.for.your.preferred.music.platform., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Do.you.pay.for.your.preferred.music.platform.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

payment_platform_plot <- plot_ly(
  data = payment_platform_data,
  x = ~Do.you.pay.for.your.preferred.music.platform.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Pay for Platform:", Do.you.pay.for.your.preferred.music.platform., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Current Payment for Music Platform vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Currently Pays for Platform", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

payment_platform_plot


budget_data <- df %>%
  count(What.is.your.monthly.budget.for.leisure.or.entertainment..Rs., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(What.is.your.monthly.budget.for.leisure.or.entertainment..Rs.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

budget_plot <- plot_ly(
  data = budget_data,
  x = ~What.is.your.monthly.budget.for.leisure.or.entertainment..Rs.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Monthly Budget:", What.is.your.monthly.budget.for.leisure.or.entertainment..Rs., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Monthly Leisure Budget vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Monthly Budget (Rs.)", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

budget_plot


music_budget_data <- df %>%
  count(What.proportion.of.your.entertainment.budget.goes.to.music.related.spending., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(What.proportion.of.your.entertainment.budget.goes.to.music.related.spending.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

music_budget_plot <- plot_ly(
  data = music_budget_data,
  x = ~What.proportion.of.your.entertainment.budget.goes.to.music.related.spending.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Music Budget %:", What.proportion.of.your.entertainment.budget.goes.to.music.related.spending., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Music Budget Proportion vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Proportion of Budget for Music", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

music_budget_plot


spending_influence_data <- df %>%
  count(What.influences.your.decision.to.spend.money.on.music.the.most., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(What.influences.your.decision.to.spend.money.on.music.the.most.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

spending_influence_plot <- plot_ly(
  data = spending_influence_data,
  x = ~What.influences.your.decision.to.spend.money.on.music.the.most.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Spending Influence:", What.influences.your.decision.to.spend.money.on.music.the.most., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Spending Decision Influencers vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Primary Spending Influence", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

spending_influence_plot


concert_vs_headphones_data <- df %>%
  count(You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

concert_vs_headphones_plot <- plot_ly(
  data = concert_vs_headphones_data,
  x = ~You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Choice:", You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Concert vs Headphones Choice vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "₹1000 Spending Choice", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

concert_vs_headphones_plot


artist_compensation_data <- df %>%
  count(Rate.how.fairly.you.think.streaming.platforms.compensate.artists...1.very.bad..5.very.good., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Rate.how.fairly.you.think.streaming.platforms.compensate.artists...1.very.bad..5.very.good.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

artist_compensation_plot <- plot_ly(
  data = artist_compensation_data,
  x = ~Rate.how.fairly.you.think.streaming.platforms.compensate.artists...1.very.bad..5.very.good.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Artist Compensation Rating:", Rate.how.fairly.you.think.streaming.platforms.compensate.artists...1.very.bad..5.very.good., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Artist Compensation Perception vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Artist Compensation Fairness Rating (1-5)", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

artist_compensation_plot


music_commodity_data <- df %>%
  count(Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world., 
        If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  # Convert all columns to character for Plotly compatibility
  mutate(
    Commodity_View = as.character(Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.),
    Payment_Response = as.character(If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.)
  )

response_decoder <- c("1" = "No", "2" = "Free Platform User", "3" = "Hesitant Yes", "4" = "Yes")
music_commodity_data$Response_Label <- response_decoder[music_commodity_data$Payment_Response]

music_commodity_plot <- plot_ly(
  data = music_commodity_data,
  x = ~Commodity_View,
  y = ~n,
  color = ~Payment_Response,  # Use the 1-4 codes for coloring
  colors = response_colors,
  type = "bar",
  text = ~paste(
    "Commodity View:", Commodity_View, 
    "<br>Response:", Response_Label, " (", Payment_Response, ")",
    "<br>Count:", n, 
    "<br>Percentage:", round(Percentage, 1), "%"
  ),
  hovertemplate = "%{text}<extra></extra>",
  hoverinfo = "text"
) %>%
  layout(
    title = "Music Commodity Perception vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Music Commodity View", tickangle = -45, type = "category"),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )
music_commodity_plot


emotional_impact_data <- df %>%
  count(Rate.yourself.on.the.following.statements..Music.affects.my.emotional.state.easily., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Rate.yourself.on.the.following.statements..Music.affects.my.emotional.state.easily.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

emotional_impact_plot <- plot_ly(
  data = emotional_impact_data,
  x = ~Rate.yourself.on.the.following.statements..Music.affects.my.emotional.state.easily.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Emotional Impact Rating:", Rate.yourself.on.the.following.statements..Music.affects.my.emotional.state.easily., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Emotional Connection to Music vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Emotional Impact Rating", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

emotional_impact_plot

productivity_impact_data <- df %>%
  count(Rate.yourself.on.the.following.statements..Music.helps.me.concentrate.or.improve.productivity., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(Rate.yourself.on.the.following.statements..Music.helps.me.concentrate.or.improve.productivity.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

productivity_impact_plot <- plot_ly(
  data = productivity_impact_data,
  x = ~Rate.yourself.on.the.following.statements..Music.helps.me.concentrate.or.improve.productivity.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Productivity Impact Rating:", Rate.yourself.on.the.following.statements..Music.helps.me.concentrate.or.improve.productivity., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Productivity Ratio Connection to Music vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Emotional Impact Rating", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

productivity_impact_plot

Willing_to_go_to_concert <- df %>%
  count(If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go., If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.) %>%
  group_by(If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

Willing_to_go_to_concert_plot <- plot_ly(
  data = Willing_to_go_to_concert,
  x = ~If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go.,
  y = ~n,
  color = ~If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
  colors = response_colors,
  type = "bar",
  text = ~paste("Productivity Impact Rating:", If.you.could.attend.a.concert.of.your.preferred.genre..will.you.go., 
                "<br>Response:", If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay., "<br>Count:", n, 
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Productivity Ratio Connection to Music vs Willingness to Pay More<br>1=No, 2=Free Platform User, 3=Hesitant Yes, 4=Yes",
    xaxis = list(title = "Emotional Impact Rating", tickangle = -45),
    yaxis = list(title = "Count"),
    barmode = "stack"
  )

Willing_to_go_to_concert_plot


df <- df %>%
  mutate(
    Willingness_To_Pay = If.Spotify.raised.Premium.from...119.to...199.month..would.you.still.pay.,
    Current_Payment = Do.you.pay.for.your.preferred.music.platform.,
    Leisure_Budget = What.is.your.monthly.budget.for.leisure.or.entertainment..Rs.,
    Spending_Influence = What.influences.your.decision.to.spend.money.on.music.the.most.,
    Artist_Compensation = Rate.how.fairly.you.think.streaming.platforms.compensate.artists...1.very.bad..5.very.good.,
    Concert_VS_Headphones = You.have...1000..Would.you.attend.a.concert.or.buy.new.headphones.,
    Music_Commodity_View = Do.you.think.music.is.a.public.or.private.commodity.in.today.s.digital.world.,
    Emotional_Impact = Rate.yourself.on.the.following.statements..Music.affects.my.emotional.state.easily.,
    Daily_Listening_Hours = How.many.hours.per.day.do.you.listen.to.music.,
    Age_Numeric = as.numeric(as.character(Age)),
    Education_Level = Education.level,
    Area_of_Residence = Area.of.residence
  )

response_colors <- c("1" = "red", "2" = "orange", "3" = "lightblue", "4" = "green")
response_labels <- c("1" = "No", "2" = "Free Platform User", "3" = "Hesitant", "4" = "Yes")

combo1_data <- df %>%
  count(Current_Payment, Leisure_Budget, Willingness_To_Pay) %>%
  group_by(Current_Payment, Leisure_Budget) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Current_Payment), !is.na(Leisure_Budget))

combo1_plot <- plot_ly(
  data = combo1_data,
  x = ~Leisure_Budget,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Current:", Current_Payment,
                "<br>Budget:", Leisure_Budget,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Current_Payment
) %>%
  layout(
    title = "Willingness to Pay by Current Payment Status and Leisure Budget",
    xaxis = list(title = "Monthly Leisure Budget (₹)", categoryorder = "array", 
                 categoryarray = c("0-500", "500-1000", "1000-2000", "2000-5000", "5000+")),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

combo1_plot


combo2_data <- df %>%
  count(Artist_Compensation, Music_Commodity_View, Willingness_To_Pay) %>%
  group_by(Artist_Compensation, Music_Commodity_View) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Artist_Compensation), !is.na(Music_Commodity_View))

combo2_plot <- plot_ly(
  data = combo2_data,
  x = ~Artist_Compensation,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Compensation Rating:", Artist_Compensation,
                "<br>Commodity View:", Music_Commodity_View,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Psychology of Payment: Compensation Fairness vs Commodity View",
    xaxis = list(title = "Artist Compensation Rating (1-5)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

combo2_plot

combo3_data <- df %>%
  count(Emotional_Impact, Daily_Listening_Hours, Willingness_To_Pay) %>%
  group_by(Emotional_Impact, Daily_Listening_Hours) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Emotional_Impact), !is.na(Daily_Listening_Hours))

combo3_plot <- plot_ly(
  data = combo3_data,
  x = ~Emotional_Impact,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Emotional Impact:", Emotional_Impact,
                "<br>Daily Hours:", Daily_Listening_Hours,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Daily_Listening_Hours
) %>%
  layout(
    title = "Engagement Depth: Emotional Connection vs Listening Hours",
    xaxis = list(title = "Emotional Impact Rating (1-5)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

combo3_plot


combo4_plot <- plot_ly(
  data = df %>% filter(!is.na(Age_Numeric), !is.na(Current_Payment)),
  x = ~Willingness_To_Pay,  
  y = ~Age_Numeric,         
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = "box",
  boxpoints = "all",
  jitter = 0.3,
  pointpos = 0,
  text = ~paste("Age:", round(Age_Numeric, 1),
                "<br>Current:", Current_Payment,
                "<br>Response:", response_labels[Willingness_To_Pay]),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = "Age Distribution by Current Payment Status and Willingness to Pay",
    xaxis = list(title = "Willingness to Pay", 
                 ticktext = response_labels,
                 tickvals = names(response_labels)),
    yaxis = list(title = "Age"),
    boxmode = "group"
  ) %>%
  add_markers(
    frame = ~Current_Payment,
    showlegend = TRUE
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Current Payment: ")
  )

combo4_plot

combo5_data <- df %>%
  count(Concert_VS_Headphones, Leisure_Budget, Willingness_To_Pay) %>%
  group_by(Concert_VS_Headphones, Leisure_Budget) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Concert_VS_Headphones), !is.na(Leisure_Budget))

combo5_plot <- plot_ly(
  data = combo5_data,
  x = ~Leisure_Budget,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Choice:", Concert_VS_Headphones,
                "<br>Budget:", Leisure_Budget,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Concert_VS_Headphones
) %>%
  layout(
    title = "Spending Choice (Concert vs Headphones) by Budget and Willingness to Pay",
    xaxis = list(title = "Monthly Leisure Budget (₹)", categoryorder = "array"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo5_plot

combo6_data <- df %>%
  count(Spending_Influence, Artist_Compensation, Willingness_To_Pay) %>%
  group_by(Spending_Influence, Artist_Compensation) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Spending_Influence), !is.na(Artist_Compensation))

combo6_plot <- plot_ly(
  data = combo6_data,
  x = ~Artist_Compensation,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Spending Influence:", Spending_Influence,
                "<br>Compensation Rating:", Artist_Compensation,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Spending_Influence
) %>%
  layout(
    title = "Spending Drivers vs Artist Compensation Perception",
    xaxis = list(title = "Artist Compensation Rating (1-5)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo6_plot

combo7_data <- df %>%
  count(Area_of_Residence, Current_Payment, Willingness_To_Pay) %>%
  group_by(Area_of_Residence, Current_Payment) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Area_of_Residence), !is.na(Current_Payment))

combo7_plot <- plot_ly(
  data = combo7_data,
  x = ~Current_Payment,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Residence:", Area_of_Residence,
                "<br>Current Payment:", Current_Payment,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Area_of_Residence
) %>%
  layout(
    title = "Geographic Trends: Residence Area and Current Payment Status",
    xaxis = list(title = "Current Payment Status", tickangle = -45),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo7_plot

combo8_data <- df %>%
  count(Favorite_Listening_Time, Daily_Listening_Hours, Willingness_To_Pay) %>%
  group_by(Favorite_Listening_Time, Daily_Listening_Hours) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Favorite_Listening_Time), !is.na(Daily_Listening_Hours))

combo8_plot <- plot_ly(
  data = combo8_data,
  x = ~Daily_Listening_Hours,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Favorite Time:", Favorite_Listening_Time,
                "<br>Daily Hours:", Daily_Listening_Hours,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Favorite_Listening_Time
) %>%
  layout(
    title = "Listening Context: Favorite Time vs Daily Hours",
    xaxis = list(title = "Daily Listening Hours"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo8_plot

combo9_data <- df %>%
  count(Music_Budget_Proportion, Leisure_Budget, Willingness_To_Pay) %>%
  group_by(Music_Budget_Proportion, Leisure_Budget) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Budget_Proportion), !is.na(Leisure_Budget))

combo9_plot <- plot_ly(
  data = combo9_data,
  x = ~Leisure_Budget,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Music Budget %:", Music_Budget_Proportion,
                "<br>Total Budget:", Leisure_Budget,
                "<br>Response:", response_labels[Willingness_To_Pay],
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Budget_Proportion
) %>%
  layout(
    title = "Budget Allocation: Music Proportion vs Total Leisure Budget",
    xaxis = list(title = "Total Monthly Leisure Budget (₹)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo9_plot

combo10_data <- df %>%
  count(Music_Commodity_View, Artist_Compensation, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Artist_Compensation) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Artist_Compensation))

combo10_plot <- plot_ly(
  data = combo10_data,
  x = ~Artist_Compensation,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Compensation Rating:", Artist_Compensation,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Core Psychology: Commodity View vs Artist Compensation",
    xaxis = list(title = "Artist Compensation Fairness (1-5)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo10_plot

combo12_data <- df %>%
  count(Music_Commodity_View, Current_Payment, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Current_Payment) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Current_Payment))

combo12_plot <- plot_ly(
  data = combo12_data,
  x = ~Current_Payment,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Current Payment:", Current_Payment,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Belief vs Action: Commodity View vs Current Payment Behavior",
    xaxis = list(title = "Current Payment Status", tickangle = -45),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo12_plot

combo13_data <- df %>%
  count(Music_Commodity_View, Leisure_Budget, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Leisure_Budget) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Leisure_Budget))

combo13_plot <- plot_ly(
  data = combo13_data,
  x = ~Leisure_Budget,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Budget:", Leisure_Budget,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Principle vs Means: Commodity View by Financial Capacity",
    xaxis = list(title = "Monthly Leisure Budget (₹)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo13_plot

combo14_data <- df %>%
  count(Music_Commodity_View, Spending_Influence, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Spending_Influence) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Spending_Influence))

combo14_plot <- plot_ly(
  data = combo14_data,
  x = ~Spending_Influence,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Spending Influence:", Spending_Influence,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Psychology: Commodity View vs Spending Drivers",
    xaxis = list(title = "Primary Spending Influence", tickangle = -45),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo14_plot

combo15_data <- df %>%
  count(Music_Commodity_View, Concert_VS_Headphones, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Concert_VS_Headphones) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Concert_VS_Headphones))

combo15_plot <- plot_ly(
  data = combo15_data,
  x = ~Concert_VS_Headphones,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Concert vs Headphones:", Concert_VS_Headphones,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Experience vs Product: Commodity View by Spending Choice",
    xaxis = list(title = "₹1000 Spending Choice", tickangle = -45),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo15_plot

combo16_data <- df %>%
  count(Music_Commodity_View, Emotional_Impact, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Emotional_Impact) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Emotional_Impact))

combo16_plot <- plot_ly(
  data = combo7_data,
  x = ~Emotional_Impact,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Emotional Impact:", Emotional_Impact,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Emotional Value: Commodity View vs Emotional Connection",
    xaxis = list(title = "Emotional Impact Rating (1-5)"),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo16_plot

combo17_data <- df %>%
  count(Music_Commodity_View, Area_of_Residence, Willingness_To_Pay) %>%
  group_by(Music_Commodity_View, Area_of_Residence) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(!is.na(Music_Commodity_View), !is.na(Area_of_Residence))

combo17_plot <- plot_ly(
  data = combo17_data,
  x = ~Area_of_Residence,
  y = ~Percentage,
  color = ~Willingness_To_Pay,
  colors = response_colors,
  type = 'bar',
  text = ~paste("Commodity View:", Music_Commodity_View,
                "<br>Residence:", Area_of_Residence,
                "<br>Response:", Willingness_To_Pay,
                "<br>Count:", n,
                "<br>Percentage:", round(Percentage, 1), "%"),
  hovertemplate = "%{text}<extra></extra>",
  frame = ~Music_Commodity_View
) %>%
  layout(
    title = "Geographic Culture: Commodity View by Area of Residence",
    xaxis = list(title = "Area of Residence", tickangle = -45),
    yaxis = list(title = "Percentage within Group", range = c(0, 100)),
    barmode = "stack"
  ) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE)

combo17_plot