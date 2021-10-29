rm(list = ls())
gc()

library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
#library(sonarsatd)


### Here we load the data. This is already interleaved data containing all of Sonar Issues

# File-level
df_a <- read.csv(file = "./data/comments/todo_fixme_file_data_added_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)
df_d <- read.csv(file = "./data/comments/todo_fixme_file_data_deleted_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)

######### Commit level

### Here we load the data. This is already interleaved data containing all of Sonar Issues

df_a <- read.csv(file = "./data/comments/todo_fixme_commit_data4_added_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)
df_d <- read.csv(file = "./data/comments/todo_fixme_commit_data4_deleted_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)
df_a <- df_a[,1:4]
df_d <- df_d[,1:4]

# Replacing the empty columns
df_a$commit_added <- sub("^$", "NO_PARENT", df_a$commit_added)
df_d$commit_deleted <- sub("^$", "NO_PARENT", df_d$commit_deleted)

# Choose here the df, you want to use
df <- df_a
df <- df_d
########## Combine with Sonarqube measures
sonar_measures_all <- read.csv(file = "data/tdd_10/sonar_measures_all.csv", stringsAsFactors = FALSE, strip.white = TRUE)
sonar_measures_all$commitHash <- str_trim(sonar_measures_all$commitHash)
sonar_measures_all$projectID <- str_trim(sonar_measures_all$projectID)
# ncloc, reliability, security
sonar_measures_all <- sonar_measures_all[,c(1:3,34, 48, 56, 58)]

# Depending on, whether you analyze additions or deletions, choose the right join operation
df <- left_join(df, sonar_measures_all, by = c("commit_added" = "commitHash", "projectID"))
df <- left_join(df, sonar_measures_all, by = c("commit_deleted" = "commitHash", "projectID"))

# Eliminating the pairs, where at least one didn't have an analysis date
df <- df[!is.na(df$SQAnalysisDate),]
na_ids <- df$id
na_ids <- na_ids[duplicated(na_ids)]
df <- subset(df, (id %in% na_ids))

# Eliminating the pairs, where commit level metrics were all zero
df <- df[df$ncloc != 0 & df$sqaleIndex != 0 & df$reliabilityRemediationEffort != 0 & df$securityRemediationEffort != 0,]
na_ids <- df$id
na_ids <- na_ids[duplicated(na_ids)]
df <- subset(df, (id %in% na_ids))

# Normalization within projects
df$ncloc_norm <- ave(df$ncloc, df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
df$sqaleIndex_norm <- ave(df$sqaleIndex, df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
df$reliabilityRemediationEffort_norm <- ave(df$reliabilityRemediationEffort, df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
df$securityRemediationEffort_norm <- ave(df$securityRemediationEffort, df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))

### Making the Table 1 for the paper

# Depending on, whether you analyze additions or deletions, choose the right join operation
df_agg <- aggregate(df$SATD, df[,c("projectID", "commit_added")], FUN=length)
df_agg <- aggregate(df$SATD, df[,c("projectID", "commit_deleted")], FUN=length)

### Table 1 for paper
# Number of pairs
nrow(df)/2
#KL-SATD in Commits summaries
summary(df_agg$x)
#Normality test
shapiro.test(df_agg$x)

### Creating the formula for mixed models
df_col_names <- colnames(df)
list_to_remove <- c("commit_added", "commit_deleted", "file_added", "file_deleted", "SATD", "weights", "weights_norm", "id", "commitHash", "projectID", "SQAnalysisDate", "functionComplexityDistribution",
                    "fileComplexityDistribution", "lastCommitDate", "nclocLanguageDistribution", "alertStatus",
                    "qualityGateDetails", "qualityProfiles", "sqaleIndex", "reliabilityRemediationEffort", "securityRemediationEffort", "ncloc")
df_col_names <- df_col_names[!df_col_names %in% list_to_remove]
column_formula <- paste(df_col_names, collapse = " + ")
column_formula <- paste("SATD ~ ", column_formula, sep = "")
column_formula <- paste(column_formula, " + (1|projectID/id)", sep = "")
column_formula <- as.formula(column_formula)

### Finding the possibly redundant commit-level metrics
# p.12 "Before running the analysis on commit-level with Sqale Index, and the 2 Remediation Efforts, 
# we wanted to avoid multi-collinearity issues with different metrics. This was done by utilizing redun-function
# from Hmisc-package in R. We used the default threshold for cutoff (R\textsuperscript{2} 0.9).
library(Hmisc)
column_formula2 <- as.formula("~ ncloc_norm + sqaleIndex_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm")
redun(column_formula2, data=df)

### This is the main part, running these with either commit-level additions or deletions makes the tables 1-6
## Normalized metrics
library(lme4)
lmer_model_all_in <- glmer(SATD ~ ncloc_norm + sqaleIndex_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm  + (1 | projectID/id), data = df, family = "binomial")
summary(lmer_model_all_in)

lmer_model_minus_ncloc <- glmer(SATD ~ sqaleIndex_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm  + (1 | projectID/id), data = df, family = "binomial")
summary(lmer_model_minus_ncloc)

lmer_model_minus_sqale <- glmer(SATD ~ ncloc_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm  + (1 | projectID/id), data = df, family = "binomial")
summary(lmer_model_minus_sqale)



######## File level

### Here we load the data. This is already interleaved data containing all of Sonar Issues

# File-level
df_a <- read.csv(file = "./data/comments/todo_fixme_file_data_added_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)
df_d <- read.csv(file = "./data/comments/todo_fixme_file_data_deleted_june_21.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE)

# Choose here the df, you want to use
df <- df_a
df <- df_d

########## Combine with Sonarqube measures
sonar_measures_all <- read.csv(file = "data/tdd_10/sonar_measures_all.csv", stringsAsFactors = FALSE, strip.white = TRUE)
sonar_measures_all$commitHash <- str_trim(sonar_measures_all$commitHash)
sonar_measures_all$projectID <- str_trim(sonar_measures_all$projectID)
# ncloc, reliability, security
sonar_measures_all <- sonar_measures_all[,c(1:3,34, 48, 56, 58)]

# Depending on, whether you analyze additions or deletions, choose the right join operation
df <- left_join(df, sonar_measures_all, by = c("commit_added" = "commitHash", "projectID"))
df <- left_join(df, sonar_measures_all, by = c("commit_deleted" = "commitHash", "projectID"))

# Eliminating the pairs, where at least one didn't have an analysis date
df <- df[!is.na(df$SQAnalysisDate),]
na_ids <- df$id
na_ids <- na_ids[duplicated(na_ids)]
df <- subset(df, (id %in% na_ids))

# Eliminating the pairs, where commit level metrics were all zero
df <- df[df$ncloc != 0 & df$sqaleIndex != 0 & df$reliabilityRemediationEffort != 0 & df$securityRemediationEffort != 0,]
na_ids <- df$id
na_ids <- na_ids[duplicated(na_ids)]
df <- subset(df, (id %in% na_ids))

## Now for the file level
df_agg <- aggregate(df$SATD, df[,c("projectID", "commit_added", "file_added")], FUN=length)
df_agg <- aggregate(df$SATD, df[,c("projectID", "commit_deleted", "file_deleted")], FUN=length)

### Table 1 for paper
# Number of pairs
nrow(df)/2
#KL-SATD in Commits summaries
summary(df_agg$x)
# Normality test
shapiro.test(df_agg$x)

# We remove the SQUID.1135 and SQUID.1134 columns, which are SonarQube's
# issues for TODO and FIXME comments. So they might have huge coefficients.
df <- df[-grep("squid\\.S1134", names(df))]
df <- df[-grep("squid\\.S1135", names(df))]

### Normalize all the issues
odd_indexes<-seq(1,nrow(df),2)
df <- arrange(df, id, desc(SATD))
df[is.na(df)] <- 0
df <- df[, colSums(df != 0) > 0]
# Choose the right depending on which df you are using
project_and_file_column <- c("projectID", "file_added")
project_and_file_column <- c("projectID", "file_deleted")

list_of_columns <- colnames(df)
list_of_columns <- list_of_columns[list_of_columns %like% "^code_smells" | list_of_columns %like% "^squid" | list_of_columns %like% "^common.java"]
for (column_name in list_of_columns){
  df[,paste0(column_name, "_norm")] <- NA
  df[,paste0(column_name, "_norm")] <- ave(df[,column_name], df[,project_and_file_column], FUN=function(x) (x-min(x))/(max(x) - min(x)))
}

# Replace possible NaNs
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- 0

# Creating the Issue formulas
list_of_columns <- colnames(df)
list_of_columns <- list_of_columns[list_of_columns %like% "_norm$"]
list_of_columns_no_fixme_todo <- list_of_columns[!list_of_columns %in% c("squid.S1134_norm", "squid.S1135_norm")]
column_formula <- paste(list_of_columns_no_fixme_todo, collapse = " + ")

column_formula <- paste("SATD ~ ", column_formula, sep = "")
column_formula <- paste(column_formula, " + (1|projectID/file_added/id)", sep = "")
column_formula <- paste(column_formula, " + (1|projectID/file_deleted/id)", sep = "")

column_formula <- as.formula(column_formula)

# Running the issue analysis
# Tables 9 & 11
lmer_model_no_todo_fixme_norm <- glmer(column_formula, data = df, family = "binomial")
summary(lmer_model_no_todo_fixme_norm)
