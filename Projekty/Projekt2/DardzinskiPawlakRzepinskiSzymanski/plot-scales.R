######################################################################
# Scale importance
######################################################################

trial <- c("A","B","C")
people <- c(10, 100, 1000)

pd <- data.frame(trial, people)

# bad
ggplot(pd, aes(x=trial, y=people)) + geom_bar(stat="identity")

# good
ggplot(pd, aes(x=trial, y=people)) + geom_bar(stat="identity") + scale_y_log10()

# The value of trial A?
# The value of trial B?

# Scales are important!