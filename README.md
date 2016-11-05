Steps to reproduce this project
-------------------------------

1. Copy run_analysis.r into UCI HAR Dataset folder which contains all data and instruction for this assignment.
2. Open the R Studio
3. Set working directory to your data set.
3. Run the R script `source("run_analysis.R")`.


Outputs produced
----------------
* Tidy dataset file `output.txt`
* Codebook file `codebook.md`


Explain the run_analysis.R script
---------------------------------

Loading required packages
```{r}
require(plyr)
require(dplyr)
require(data.table)
```
print("running analysis")
Read train and test set and merge them into one
```{r}
subject_total <- rbind(read.table("train/subject_train.txt"), read.table("test/subject_test.txt"))
X_total <- rbind(read.table("train/X_train.txt"), read.table("test/X_test.txt"))
y_total <- rbind(read.table("train/y_train.txt"), read.table("test/y_test.txt"))
```

Convert to data table. We want to use setkey later
```{r}
dt <- data.table(X_total)
```

Extract only mean and standard values
```{r}
dtFeatures <- fread("features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grep("\\bmean()\\b|\\bstd()\\b", dtFeatures$featureName), ]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]
```

Append subject and activity info into dt
```{r}
activityNum <- y_total[, 1]
dt <- cbind(activityNum, dt)
subject <- subject_total[, 1]
dt <- cbind(subject, dt)
```

Naming activity.
```{r}
dtActivityNames <- fread("activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
setkey(dt, subject, activityNum)
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
```

Reshape dt. All feature values are vertical form.
```{r}
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
```

Grep function for feature code
```{r}
mygrep <- function (regex) {
  grepl(regex, dt$feature)
}
```

Append columns extracted from feature data. For summary info, we don't assign value directly, but give it a label
```{r}
dt <- mutate(dt, 
             featDomain = ifelse(mygrep("^t"), 1, ifelse(mygrep("^f"), 2, 0)),
             featInstrument = ifelse(mygrep("Acc"), 1, ifelse(mygrep("Gyro"), 2, 0)),
             featAcceleration = ifelse(mygrep("BodyAcc"), 1, ifelse(mygrep("GravityAcc"), 2, 0)),
             featVariable = ifelse(mygrep("mean()"), 1, ifelse(mygrep("std()"), 2, NA)),
             featJerk = ifelse(mygrep("Jerk"), 1, 0),
             featMagnitude = ifelse(mygrep("Mag"), 1, 0),
             featAxis = ifelse(mygrep("-X"), 1, ifelse(mygrep("-Y"), 2, ifelse(mygrep("-Z"), 3, 0))))
```

Convert dt to table again. Mutate converted it into data frame. Then set label for column data
```{r}
dt <- data.table(dt)
dt$featDomain = factor(dt$featDomain, levels = c(1,2), labels = c("Time", "Freq"))
dt$featInstrument = factor(dt$featInstrument, levels = c(1,2), labels = c("Accelerometer", "Gyroscope"))
dt$featAcceleration = factor(dt$featAcceleration, levels = c(1,2), labels = c("Body", "Gravity"))
dt$featVariable = factor(dt$featVariable, levels = c(1,2), labels = c("Mean", "SD"))
dt$featJerk = factor(dt$featJerk, levels = c(1), labels = c("Jerk"))
dt$featMagnitude = factor(dt$featMagnitude, levels = c(1), labels = c("Magnitude"))
dt$featAxis = factor(dt$featAxis, levels = c(1,2,3), labels = c("X", "Y", "Z"))
```

Sort data and select needing field for tidy data
```{r}
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
```

Write output to file
```{r}
f <- file.path("output.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)
```