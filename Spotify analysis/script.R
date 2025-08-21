getwd()
df<-read.csv('/home/pikubha/r-projects/Spotify-Product-Analysis/data.csv');

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
                            levels = c("Yes", "No", "Maybe"),
                            labels = c(0, 1, 2))

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
  levels = c("Yes, strongly", "Sometimes", "No","Never thought about it"),
  labels = c(0, 1, 2,3)
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