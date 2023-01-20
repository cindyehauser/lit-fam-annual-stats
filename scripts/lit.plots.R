############################################################################################################
#
# Function that support plotting for Lit Fam end-of-year stats
#
############################################################################################################

# Set up colours

library(RColorBrewer)
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
#                   colorblindFriendly=TRUE)

# Set up colours
dark.blue    <- brewer.pal(n = 12, name = "Paired")[2]
light.blue   <- rgb(t(col2rgb(dark.blue)), alpha = 70, max = 255)
dark.red     <- brewer.pal(n = 12, name = "Paired")[6]
light.red    <- rgb(t(col2rgb(dark.red)), alpha = 70, max = 255)
dark.purple  <- brewer.pal(n = 12, name = "Paired")[10]
light.purple <- rgb(t(col2rgb(dark.purple)), alpha = 70, max = 255)
med.grey     <- grey(level = 0.3, alpha = 0.5)

dark.green   <- brewer.pal(n = 12, name = "Paired")[4]
light.green  <- rgb(t(col2rgb(dark.green)), alpha = 70, max = 255)
dark.orange  <- brewer.pal(n = 12, name = "Paired")[8]
light.orange <- rgb(t(col2rgb(dark.orange)), alpha = 70, max = 255)

############################################################################################################

book.score.plot <- function(lit.data, book.data, 
                            HL.thisyear = data.frame(), HL.alltime = data.frame(),
                            lab.titles) {
  
  # Summary variables
  this.year <- max(lit.data$year)
  titles    <- lit.data %>% filter(!is.na(gender)) %>% arrange(date) %>% distinct(title) %>% pull(title)
  n.t       <- length(titles)
  
  # Set up axes
  par(mar = c(12, 6, 1, 8))
  plot(0, 0, pch = NA_integer_,
       xlim = c(0.5, n.t+0.5), ylim = c(0, 10), axes = FALSE,
       xlab = "", ylab = "Score", cex.lab = 1.5,
       main = "")
  axis(1, at = 1:n.t,
       labels = lab.titles,  
       las = 2) #padj = 1)
  axis(2, labels = TRUE, las = 1)
  box()
  
  # Plot all scores for all books, colour coding according to year
  for (i in seq_along(titles)) {
    book.scores <- lit.data %>% filter(title == titles[i]) %>% pull(score)
    book.col    <- ifelse(book.data$year[which(book.data$title == titles[i])] < this.year,
                          med.grey, light.blue)
    points(rep(i, length(book.scores)), book.scores,
           pch = 16, col = book.col)
  }
  
  # If given, plot highlighted scores for this year
  if (nrow(HL.thisyear) > 0) {
    p.cex <- ifelse(nrow(HL.thisyear) == 1, 2, 1)
    points(HL.thisyear$index, HL.thisyear$score,
           pch = 16, col = dark.purple, cex = p.cex)
  }
  
  # If given, plot highlighted scores for all time
  if (nrow(HL.alltime) > 0) {
    p.cex <- ifelse(nrow(HL.alltime) == 1, 2, 1)
    points(HL.alltime$index, HL.alltime$score,
           pch = 16, col = dark.red, cex = p.cex)
  }
  
  # Add legend to the side
  legend(n.t+1.5, 8, 
         c(paste0("pre-", this.year), this.year, paste0(this.year, " winner"), "all-time winner"),
         col = c(med.grey, light.blue, dark.purple, dark.red),
         pch = 16, bty = "n", xpd = TRUE)
  
}


############################################################################################################

fam.score.plot <- function(lit.data, fam.data,
                           HL.thisyear = data.frame(), HL.alltime = data.frame()) {
  
  # Summary variables
  this.year <- max(lit.data$year)
  fams      <- lit.data %>% arrange(fam) %>% distinct(fam) %>% pull(fam)
  n.f       <- length(fams)
  
  # Set up axes
  par(mar = c(7, 6, 1, 8))
  plot(0, 0, pch = NA_integer_,
       xlim = c(0.5, n.f+0.5), ylim = c(0, 10), axes = FALSE,
       xlab = "", ylab = "Score", cex.lab = 1.5,
       main = "")
  axis(1, at = 1:n.f,
       labels = fams, las = 2)
  axis(2, labels = TRUE, las = 1)
  box()
  
  # Plot all scores for all fam, colour coding according to year
  for (i in seq_along(fams)) {
    book.scores <- lit.data %>% filter(fam == fams[i]) %>% pull(score)
    book.col    <- ifelse(lit.data$year[which(lit.data$fam == fams[i])] < this.year,
                          med.grey, light.blue)
    points(rep(i, length(book.scores)), book.scores,
           pch = 16, col = book.col)
  }
  
  # If given, plot highlighted scores for this year
  if (nrow(HL.thisyear) > 0) {
    #p.cex <- ifelse(nrow(HL.thisyear) == 1, 2, 1)
    points(HL.thisyear$index, HL.thisyear$score,
           pch = 16, col = dark.purple, cex = 2)
  }
  
  # If given, plot highlighted scores for all time
  if (nrow(HL.alltime) > 0) {
    #p.cex <- ifelse(nrow(HL.alltime) == 1, 2, 1)
    points(HL.alltime$index, HL.alltime$score,
           pch = 16, col = dark.red, cex = 2)
  }
  
  legend(n.f+1.5, 8, 
         c(paste0("pre-", this.year), this.year, paste0(this.year, " winner"), "all-time winner"),
         col = c(med.grey, light.blue, dark.purple, dark.red),
         pch = 16, bty = "n", xpd = TRUE)
  
}