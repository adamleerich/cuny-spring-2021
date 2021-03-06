pluto <- read.csv('pluto_20v8.csv')


numfloors <- pluto$numfloors
yearbuilt <- pluto$yearbuilt

yb_breaks <- as.integer((0:17) * 10 + 1850)
yb_breaks_start <- yb_breaks[-length(yb_breaks)]
yb_breaks_end <- yb_breaks[-1] - 1
yb_labels <- paste(yb_breaks_start, yb_breaks_end, sep = '-')



nf10 <- cut(numfloors, breaks = (0:12) * 10, right = FALSE)
yb10 <- cut(
  as.integer(yearbuilt), 
  breaks = as.integer((0:17) * 10 + 1850), 
  labels = yb_labels,
  right = FALSE)

s <- data.frame(nf10, yb10) %>% 
  group_by(yb10, nf10) %>% 
  summarize(Count = n())


print(s)



p20 <- read.csv('pluto_20v8.csv', nrows = 20)

to_use <- c(
  'borough', 'block', 'lot', 
  'bbl', 'numfloors', 'yearbuilt', 
  'bldgclass', 'lotarea', 'bldgarea', 
  'xcoord', 'ycoord')

write.csv(p20[, to_use], row.names = FALSE, file = 'pluto20rows.csv')

