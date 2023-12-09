################################################################################################################
#
# Statistical analysis of Lit Fam book scores
#
################################################################################################################

# Preamble - gather packages and associated scripts

library(tidyverse)
library(lubridate)

source("scripts/lit.plots.R")

file.suffix <- "2023"

################################################################################################################

# Set up main data set

lit.data <- read.csv("data/litfam_scores_2023.csv")
names(lit.data) <- c("fam", "date", "year", "title", "author", "gender", "nationality", "score")
lit.data$date <- as.Date(lit.data$date, format = "%d/%m/%Y")

# Unique titles, Xmas swap books removed, in order of meeting, total number of titles
titles <- lit.data %>% filter(!is.na(gender)) %>% arrange(date) %>% distinct(title) %>% pull(title)
n.t    <- length(titles)

# Unique participants' names and number of people
fams   <- lit.data %>% arrange(fam) %>% distinct(fam) %>% pull(fam)
n.f    <- length(fams)

# Define 'this year' for highlights
this.year <- max(lit.data$year)

# Assign a book index to each observation
lit.data$book.index <- NA
for (i in seq_along(titles)) {
  lit.data$book.index[lit.data$title == titles[i]] <- i
}

################################################################################################################

## Book-level calculations

book.data <- lit.data %>%
  filter(!is.na(gender)) %>%
  group_by(title, book.index, author, gender, nationality, year) %>%
  summarise(mean = mean(score),
            range = max(score) - min(score),
            sd = sd(score),
            attending = n(),
            .groups = "drop")
View(book.data)

# Make label titles that are no more than 20 characters
titles[nchar(titles) > 23]
lab.titles    <- as.character(titles)
lab.titles[titles == "My Year of Rest and Relaxation"]     <- "My Year of R & R"
lab.titles[titles == "The Dictionary of Lost Words"]       <- "Dict'y of Lost Words"
lab.titles[titles == "The Ministry of Utmost Happiness"]   <- "M'stry Utmost Happiness"
lab.titles[titles == "The Dangers of Smoking in Bed"]      <- "Dangers of Smoking in Bed"
lab.titles[titles == "We Have Always Lived in the Castle"] <- "We 'Lived in the Castle"
cbind(titles, lab.titles, nchar(lab.titles))

##############################
## Author gender
gender.year <- book.data %>%
  group_by(year) %>%
  summarise(woman = sum(gender == "woman"),
            man = sum(gender == "man"),
            ratio = woman/(woman + man))

png(filename = paste0("plots/gender.year.", file.suffix, ".png"),
    width = 15, height = 9, units = "cm", res = 300)
barplot(height = t(as.matrix(gender.year[, 2:3])),
        names.arg = gender.year$year,
        legend.text = names(gender.year)[2:3], args.legend = list(x = 1.5, y = 8.5, bty = "n", cex = 1),
        col = c(light.purple, light.blue), 
        ylab = "Number of books", cex.names = 1.2)
dev.off()

##############################
## Author nationality
nation.year <- book.data %>%
  group_by(year, nationality) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = nationality, values_from = n, values_fill = list(n = 0))

png(filename = paste0("plots/nation.year.", file.suffix, ".png"),
    width = 15, height = 9, units = "cm", res = 300)
barplot(height = t(as.matrix(nation.year[, -1])),
        names.arg = nation.year$year,
        legend.text = names(nation.year[ , -1]), args.legend = list(x = 1.5, y = 10, bty = "n", cex = 0.8),
        col = c(light.blue, light.purple, light.red, light.green, light.orange, dark.red, dark.purple, dark.blue), 
        ylab = "Number of books")
dev.off()

##############################
## Scores across years
score.year <- lit.data %>%
  group_by(year) %>%
  summarise(mean.score = mean(score, na.rm = TRUE),
            lwr.score = quantile(score, probs = 0.05, na.rm = TRUE),
            upr.score = quantile(score, probs = 0.95, na.rm = TRUE),
            .groups = "drop")

png(filename = paste0("plots/score.year.", file.suffix, ".png"),
    width = 15, height = 9, units = "cm", res = 300)
par(mar = c(4, 5, 1, 2))
plot(lit.data$year, lit.data$score, 
     xlim = c(min(lit.data$year) - 0.5, max(lit.data$year) + 0.5), ylim = c(0, 10), lab = c(4, 6, 7), las = 1, 
     xlab = "Year", ylab = "Score", cex.lab = 1.5, 
     pch = 16, col = med.grey)
polygon(x = c(score.year$year, rev(score.year$year)),
        y = c(score.year$lwr.score, rev(score.year$upr.score)),
        border = NA, col = light.blue)
lines(score.year$year, score.year$mean.score,
     col = dark.blue, lwd = 2)
legend("bottomleft", bty = "n",
       c("All scores", "Annual mean score", "90% of scores"), cex = 0.8,
       col = c(med.grey, dark.blue, NA), 
       pch = c(16, NA, NA), fill = c(NA, NA, light.blue), lwd = c(NA, 2, NA), border = NA)
dev.off()


##############################
## Most read book
(HL.book.thisyear <- book.data %>% filter(year == this.year) %>% filter(attending == max(attending)))
(HL.book.alltime  <- book.data %>% filter(attending == max(attending)))
# Scores to highlight
HL.thisyear       <- book.data %>% filter(title == HL.book.thisyear$title) %>% select(attending, index = book.index)
HL.alltime        <- book.data %>% filter(title == HL.book.alltime$title)  %>% select(attending, index = book.index)
# Plot scores and highlight feature book
png(filename = paste0("plots/mostread.book.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
par(mar = c(12, 6, 1, 8))
plot(0, 0, pch = NA_integer_,
     xlim = c(0.5, n.t+0.5), ylim = c(0, HL.book.alltime$attending), axes = FALSE,
     xlab = "", ylab = "Number scores submitted", cex.lab = 1.5,
     main = "")
axis(1, at = 1:n.t,
     labels = lab.titles, las = 2)
axis(2, labels = TRUE, las = 1)
box()
for (i in seq_along(titles)) {
  book.scores <- book.data %>% filter(title == titles[i]) %>% pull(attending)
  book.col    <- ifelse(book.data$year[which(book.data$title == titles[i])] < this.year,
                        med.grey, light.blue)
  points(rep(i, length(book.scores)), book.scores,
         pch = 16, col = book.col)
}
points(HL.thisyear$index, HL.thisyear$attending,
       pch = 16, col = dark.purple, cex = 2)
points(HL.alltime$index, HL.alltime$attending,
       pch = 16, col = dark.red, cex = 2)
legend(n.t+1.5, 8, 
       c(paste0("pre-", this.year), this.year, paste0(this.year, " winner"), "all-time winner"),
       col = c(med.grey, light.blue, dark.purple, dark.red),
       pch = 16, bty = "n", xpd = TRUE)
dev.off()


##############################
## Most agreed-upon book
# Smallest range
book.data %>% filter(year == this.year) %>% filter(range == min(range))
book.data %>% filter(range == min(range))
# Smallest standard deviation
(HL.book.thisyear <- book.data %>% filter(year == this.year) %>% filter(sd == min(sd)))
(HL.book.alltime  <- book.data %>% filter(sd == min(sd)))
# Scores to highlight
HL.thisyear      <- lit.data %>% filter(title == HL.book.thisyear$title) %>% select(score, index = book.index)
HL.alltime       <- lit.data %>% filter(title == HL.book.alltime$title)  %>% select(score, index = book.index)
# Plot scores and highlight feature book
png(filename = paste0("plots/agreed.book.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data, 
                HL.thisyear = HL.thisyear, HL.alltime = HL.alltime,
                lab.titles = lab.titles)
dev.off()


##############################
## Most controversial book
# Largest range
book.data %>% filter(year == this.year) %>% filter(range == max(range))
book.data %>% filter(range == max(range))
# Larget standard deviation
(HL.book.thisyear <- book.data %>% filter(year == this.year) %>% filter(sd == max(sd)))
(HL.book.alltime  <- book.data %>% filter(sd == max(sd)))
# Scores to highlight
HL.thisyear       <- lit.data %>% filter(title == HL.book.thisyear$title) %>% select(score, index = book.index)
HL.alltime        <- lit.data %>% filter(title == HL.book.alltime$title)  %>% select(score, index = book.index)
# Plot scores and highlight feature books
png(filename = paste0("plots/controversial.book.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data, 
                HL.thisyear = HL.thisyear, HL.alltime = HL.alltime,
                lab.titles = lab.titles)
dev.off()



##############################
## Least popular book
# Lowest mean score
(HL.book.thisyear <- book.data %>% filter(year == this.year) %>% filter(mean == min(mean)))
(HL.book.alltime  <- book.data %>% filter(mean == min(mean)))
# Scores to highlight
HL.thisyear       <- book.data %>% filter(title == HL.book.thisyear$title) %>% select(score = mean, index = book.index)
HL.alltime        <- book.data %>% filter(title == HL.book.alltime$title)  %>% select(score = mean, index = book.index)
# Plot scores and highlight feature book
png(filename = paste0("plots/leastpopular.book.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data, 
                HL.thisyear = HL.thisyear, HL.alltime = HL.alltime,
                lab.titles = lab.titles)
dev.off()


##############################
## Most popular book
# Highest mean score
(HL.book.thisyear <- book.data %>% filter(year == this.year) %>% filter(mean == max(mean)))
(HL.book.alltime  <- book.data %>% filter(mean == max(mean)))
# Scores to highlight
HL.thisyear       <- book.data %>% filter(title == HL.book.thisyear$title) %>% select(score = mean, index = book.index)
HL.alltime        <- book.data %>% filter(title == HL.book.alltime$title)  %>% select(score = mean, index = book.index)
# Plot scores and highlight feature book
png(filename = paste0("plots/mostpopular.book.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data, 
                HL.thisyear = HL.thisyear, HL.alltime = HL.alltime,
                lab.titles = lab.titles)
dev.off()



################################################################################################################

## Person-level calculations

fam.data <- lit.data %>%
  filter(!is.na(score)) %>%
  group_by(fam) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            median = median(score, na.rm = TRUE),
            range = max(score, na.rm = TRUE) - min(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            attending = n(),
            recent_attending = max(year),
            .groups = "drop") %>%
  filter(recent_attending >= this.year - 1) %>%
  mutate(fam.index = row_number())
fam.data.thisyear <- lit.data %>%
  filter(year == this.year, !is.na(score)) %>%
  group_by(fam) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            median = median(score, na.rm = TRUE),
            range = max(score, na.rm = TRUE) - min(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            attending = n(),
            .groups = "drop") %>%
  left_join(fam.data %>% dplyr::select(fam, fam.index))

lab.fams <- fam.data$fam


# Individuals' deviations from the group mean
lit.data <- left_join(lit.data, book.data %>% dplyr::select(title, bk.mean = mean), by = "title") %>%
  mutate(sq.dev = (score - bk.mean)^2) %>%
  left_join(fam.data %>% dplyr::select(fam, fam.index), by = "fam")
fam.data <- left_join(fam.data, (lit.data %>%
                                   filter(!is.na(gender)) %>%
                                   group_by(fam) %>%
                                   summarise(SS.dev = sum(sq.dev), n.scores = n())
                                 )
                      ) %>%
  mutate(sd.group = sqrt(SS.dev/n.scores))

fam.data.thisyear <- left_join(fam.data.thisyear, (lit.data %>%
                                                     filter(!is.na(gender), year == this.year) %>%
                                                     group_by(fam) %>%
                                                     summarise(SS.dev = sum(sq.dev), n.scores = n())
                                                  )
                                ) %>%
  mutate(sd.group = sqrt(SS.dev/n.scores))




##############################
## Our book optimist
# Highest median
fam.data.thisyear %>% filter(attending >= 2) %>% filter(median == max(median))
fam.data.thisyear %>% arrange(desc(median))
fam.data %>% arrange(desc(median))
# Highest mean
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(mean == max(mean)))
fam.data.thisyear %>% arrange(desc(mean))
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(mean == max(mean)))
fam.data %>% arrange(desc(mean))
# Scores to highlight
HL.thisyear      <- fam.data %>% filter(fam == HL.fam.thisyear$fam) %>% select(score = mean, index = fam.index)
HL.alltime       <- fam.data %>% filter(fam == HL.fam.alltime$fam)  %>% select(score = mean, index = fam.index)
# Plot scores and highlight feature people
png(filename = paste0("plots/optimist.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
fam.score.plot(lit.data = lit.data, fam.data = fam.data,
               HL.thisyear = HL.thisyear, HL.alltime = HL.alltime)
dev.off()


##############################
## Our book pessimist
# Lowest median
fam.data.thisyear %>% filter(attending >= 2) %>% filter(median == min(median))
fam.data.thisyear %>% arrange(median)
fam.data %>% arrange(desc(median))
# Lowest mean
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(mean == min(mean)))
fam.data.thisyear %>% arrange(mean)
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(mean == min(mean)))
fam.data %>% arrange(mean)
# Scores to highlight
HL.thisyear      <- fam.data %>% filter(fam == HL.fam.thisyear$fam) %>% select(score = mean, index = fam.index)
HL.alltime       <- fam.data %>% filter(fam == HL.fam.alltime$fam)  %>% select(score = mean, index = fam.index)
# Plot scores and highlight feature person
png(filename = paste0("plots/pessimist.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
fam.score.plot(lit.data = lit.data, fam.data = fam.data,
               HL.thisyear = HL.thisyear, HL.alltime = HL.alltime)
dev.off()


##############################
## Our wildest scorer
# Largest score range
fam.data.thisyear %>% filter(attending >= 2) %>% filter(range == max(range))
fam.data.thisyear %>% arrange(desc(range))
fam.data %>% arrange(desc(range))
# Largest standard deviation
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(sd == max(sd, na.rm = TRUE)))
fam.data.thisyear %>% arrange(desc(sd))
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(sd == max(sd, na.rm = TRUE)))
fam.data %>% arrange(desc(sd))
# Scores to highlight
HL.thisyear      <- lit.data %>% filter(fam == HL.fam.thisyear$fam, year == this.year) %>% select(score, index = fam.index)
HL.alltime       <- lit.data %>% filter(fam == HL.fam.alltime$fam)                     %>% select(score, index = fam.index)
# Plot scores and highlight feature person
png(filename = paste0("plots/wildscore.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
fam.score.plot(lit.data = lit.data, fam.data = fam.data,
               HL.thisyear = HL.thisyear, HL.alltime = HL.alltime)
dev.off()


##############################
## Our most stable scorer
# Smallest score range
fam.data.thisyear %>% filter(attending >= 2) %>% filter(range == min(range))
fam.data %>%          filter(attending >= 5) %>% filter(range == min(range))
# Smallest standard deviation
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(sd == min(sd, na.rm = TRUE)))
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(sd == min(sd, na.rm = TRUE)))
# Scores to highlight
HL.thisyear      <- lit.data %>% filter(fam == HL.fam.thisyear$fam, year == this.year) %>% select(score, index = fam.index)
HL.alltime       <- lit.data %>% filter(fam == HL.fam.alltime$fam)                     %>% select(score, index = fam.index)
# Plot scores and highlight feature person
png(filename = paste0("plots/stablescore.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
fam.score.plot(lit.data = lit.data, fam.data = fam.data,
               HL.thisyear = HL.thisyear, HL.alltime = HL.alltime)
dev.off()


##############################
## Most agreeable person
# Smallest deviation from group mean
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(sd.group == min(sd.group)))
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(sd.group == min(sd.group)))
# Scores to highlight
HL.thisyear      <- lit.data %>% filter(!is.na(gender), fam == HL.fam.thisyear$fam, year == this.year) 
HL.alltime       <- lit.data %>% filter(!is.na(gender), fam == HL.fam.alltime$fam)                     
# Plot scores and highlight feature person
png(filename = paste0("plots/agreeable.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data,
                HL.thisyear = HL.thisyear %>% select(score, index = book.index), 
                HL.alltime  = HL.alltime  %>% select(score, index = book.index),
                lab.titles = lab.titles)
# Add in mean book scores as black points
points(book.data$book.index, book.data$mean,
       pch = 16, col = "black")
# Draw a line to join each highlighted observation to the book mean
segments(x0 = rep(HL.thisyear$book.index, 2), 
         y0 = HL.thisyear$bk.mean, y1 = HL.thisyear$score,
         lwd = 1, col = dark.purple)
segments(x0 = rep(HL.alltime$book.index, 2), 
         y0 = HL.alltime$bk.mean, y1 = HL.alltime$score,
         lwd = 1, col = dark.red)
dev.off()



##############################
## Rebel scorer
# Largest deviation from group mean
(HL.fam.thisyear <- fam.data.thisyear %>% filter(attending >= 2) %>% filter(sd.group == max(sd.group)))
(HL.fam.alltime  <- fam.data          %>% filter(attending >= 5) %>% filter(sd.group == max(sd.group)))
# Scores to highlight
HL.thisyear      <- lit.data %>% filter(!is.na(gender), fam == HL.fam.thisyear$fam, year == this.year) 
HL.alltime       <- lit.data %>% filter(!is.na(gender), fam == HL.fam.alltime$fam)                     
# Plot scores and highlight feature person
png(filename = paste0("plots/rebel.fam.", file.suffix, ".png"),
    width = 27, height = 14, units = "cm", res = 300)
book.score.plot(lit.data = lit.data, book.data = book.data,
                HL.thisyear = HL.thisyear %>% select(score, index = book.index), 
                HL.alltime  = HL.alltime  %>% select(score, index = book.index),
                lab.titles = lab.titles)
# Add in mean book scores as black points
points(book.data$book.index, book.data$mean,
       pch = 16, col = "black")
# Draw a line to join each highlighted observation to the book mean
segments(x0 = rep(HL.thisyear$book.index, 2), 
         y0 = HL.thisyear$bk.mean, y1 = HL.thisyear$score,
         lwd = 1, col = dark.purple)
segments(x0 = rep(HL.alltime$book.index, 2), 
         y0 = HL.alltime$bk.mean, y1 = HL.alltime$score,
         lwd = 1, col = dark.red)
dev.off()


##############################
## Most faithful attendance
# Highest number of scores submitted
fam.data.thisyear %>% filter(attending == max(attending))
fam.data %>% filter(attending == max(attending))


##############################
## Correlations among people's scores

# Table scores as title vs fam
corr.data <- lit.data %>%
  filter(!is.na(gender)) %>%
  filter(fam %in% (fam.data %>% filter(attending >= 7) %>% pull(fam))) %>%
  #filter(fam %in% (fam.data.2021 %>% filter(attending >= 2) %>% pull(fam))) %>%
  select(fam, title, score) %>%
  pivot_wider(names_from = fam, values_from = score)

# Tidy up data
corr.fam <- names(corr.data)[-1]
corr.data <- as.matrix(corr.data)[ , -1]
corr.data <- apply(corr.data, 1, as.numeric)
# Calculate pairwise correlations
corr.df <- cor(t(corr.data), use = "pairwise.complete.obs")
corr.df <- data.frame(corr.df, row.names = corr.fam)
names(corr.df) <- corr.fam

# Which pair have the highest agreement?
corr.df[corr.df == 1] <- NA
max(corr.df, na.rm = TRUE)
which(corr.df == max(corr.df, na.rm = TRUE))
min(corr.df, na.rm = TRUE)
which(corr.df == min(corr.df, na.rm = TRUE))

# Write the correlation matrix to file, for network plotting
write.csv(corr.df, paste0("data/correlationmatrix_", this.year, ".csv"))
