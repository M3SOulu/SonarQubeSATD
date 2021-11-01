rm(list = ls())
gc()

library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(Hmisc)
library(lme4)
###### Functions to load and preprocess the data #####

# Loading the data from csv-files, and processing it
load_and_process_data_file <- function(level="commit", satd_type="add", data_filename = "")
{
  df_temp <- read.csv(data_filename, fileEncoding = "utf-8", stringsAsFactors = FALSE)
  if (level=="commit")
    {
    df_temp <- df_temp[,1:4]
    }
  if(satd_type=="add")
    {
    names(df_temp)[names(df_temp)=="commit_added"] <- "commitHash"
    }
  else if(satd_type == "del")
    {
    names(df_temp)[names(df_temp)=="commit_deleted"] <- "commitHash"
    }
  else
    {
      stop("Wrong satd_type")
      }
  df_temp$commitHash <- sub("^$", "NO_PARENT", df_temp$commitHash)
  return(df_temp)
}

# Eliminate data pairs, where at least one has missing values
eliminate_redundant_data <- function(data_df = df)
{
  # Eliminating the pairs, where at least one didn't have an analysis date
  data_df <- data_df[!is.na(data_df$SQAnalysisDate),]
  na_ids <- data_df$id
  na_ids <- na_ids[duplicated(na_ids)]
  data_df <- subset(data_df, (id %in% na_ids))
  
  # Eliminating the pairs, where commit level metrics were all zero
  data_df <- data_df[data_df$ncloc != 0 & data_df$sqaleIndex != 0 & data_df$reliabilityRemediationEffort != 0 & data_df$securityRemediationEffort != 0,]
  na_ids <- data_df$id
  na_ids <- na_ids[duplicated(na_ids)]
  data_df <- subset(data_df, (id %in% na_ids))
  data_df[is.na(data_df)] <- 0
  return(data_df)
}

# Loading ncloc, SqaleIndex, ReliabilityRemediationEffort, and SecurityRemediationEffort
load_sonar_measures <- function(data_filename = ""){
  sonar_measures_all <- read.csv(file = data_filename, stringsAsFactors = FALSE, strip.white = TRUE)
  sonar_measures_all$commitHash <- str_trim(sonar_measures_all$commitHash)
  sonar_measures_all$projectID <- str_trim(sonar_measures_all$projectID)
  # Keep only ncloc, sqale, reliability, security
  sonar_measures_all <- sonar_measures_all[,c(1:3,34, 48, 56, 58)]
  return(sonar_measures_all)
}

# Normalize the data within projects
normalize_data_within_projects <- function(data_df = df)
{
  data_df$ncloc_norm <- ave(data_df$ncloc, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$sqaleIndex_norm <- ave(data_df$sqaleIndex, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$reliabilityRemediationEffort_norm <- ave(data_df$reliabilityRemediationEffort, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$securityRemediationEffort_norm <- ave(data_df$securityRemediationEffort, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  return(data_df)
}

# Creating Table 1
create_table_1 <- function(data_df = df)
{
  df_agg <- aggregate(df$SATD, df[,c("projectID", "commitHash")], FUN=length)
  # Number of pairs
  print(paste0("Number of pairs: ", nrow(data_df)/2))
  #KL-SATD in Commits summaries
  print("Commit summary: ")
  print(summary(df_agg$x))
  #Normality test
  shapiro_test <- shapiro.test(df_agg$x)
  print(paste0("Normality test: ", shapiro_test$method, " , W = ", shapiro_test$statistic, " , p-value = ", shapiro_test$p.value))
}

find_redundant_metrics <- function(data_df = df)
{
  column_formula2 <- as.formula("~ ncloc_norm + sqaleIndex_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm")
  print(redun(column_formula2, data=data_df))
}
##### End of functions #####

# 1.0 Load the data and combine with sonar measures
# Parameters are as follows:
# level = "commit" or "file"
# satd_type = "add" or "del"
# data_filename = path_to_the_data_file, e.g. "./data/comments/commit_added.csv"
df <- load_and_process_data_file(level = "commit", satd_type = "add", data_filename = "./data/comments/commit_added.csv")
#df <- load_and_process_data_file(level = "commit", satd_type = "del", data_filename = "./data/comments/commit_deleted.csv")

# 1.1. Load the sonar measures
# data_filename = path_to_the_data_file, e.g. "./data/tdd_10/sonar_measures_all.csv"
sonar_measures <- load_sonar_measures(data_filename = "data/tdd_10/sonar_measures_all.csv")

# 1.2. Join the df and sonar measures
df <- left_join(df, sonar_measures, by = c("commitHash", "projectID"))

# 1.3. Eliminate redundant / incomplete data
df <- eliminate_redundant_data(data_df = df)

# 1.4. Normalize data within projects
df <- normalize_data_within_projects(data_df = df)

# 2.0 Making the Table 1 & 2 for the paper
# Table 1 addition row is made with commit-level added data, and deletion row with commit-level deleted data
create_table_1(data_df = df)

# 2.1 Finding the possibly redundant commit-level metrics
# p.12 "Before running the analysis on commit-level with Sqale Index, and the 2 Remediation Efforts, 
# we wanted to avoid multi-collinearity issues with different metrics. This was done by utilizing redun-function
# from Hmisc-package in R. We used the default threshold for cutoff (R\textsuperscript{2} 0.9).
find_redundant_metrics(data_df = df)

# 3.0 Running the commit-level analyses
# This is the main part, running these with commit-level additions makes the Tables 3,4, and 5
# Running these with commit-level deletions makes the Tables 6,7, and 8
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
