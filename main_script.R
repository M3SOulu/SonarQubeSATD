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
  else if(level=="file")
  {
    # Removing the issues of TODO and FIXME
    df_temp <- df_temp[-grep("squid\\.S1134", names(df_temp))]
    df_temp <- df_temp[-grep("squid\\.S1135", names(df_temp))]
  }
  else
  {
    stop("Wrong level.")
  }
  if(satd_type=="add")
    {
    names(df_temp)[names(df_temp)=="commit_added"] <- "commitHash"
    if(level=="file"){
      names(df_temp)[names(df_temp)=="file_added"] <- "fileName"
    }
  }
  else if(satd_type == "del")
    {
    names(df_temp)[names(df_temp)=="commit_deleted"] <- "commitHash"
    if(level=="file"){
      names(df_temp)[names(df_temp)=="file_deleted"] <- "fileName"
    }
  }
  else
    {
      stop("Wrong satd_type.")
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

# Normalize commit data within projects
normalize_data_within_projects <- function(data_df = df)
{
  data_df[is.na(data_df)] <- 0
  data_df$ncloc_norm <- ave(data_df$ncloc, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$sqaleIndex_norm <- ave(data_df$sqaleIndex, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$reliabilityRemediationEffort_norm <- ave(data_df$reliabilityRemediationEffort, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  data_df$securityRemediationEffort_norm <- ave(data_df$securityRemediationEffort, data_df$projectID, FUN=function(x) (x-min(x))/(max(x) - min(x)))
  return(data_df)
}

# Creating Tables 1 & 2
create_tables_1_and_2 <- function(data_df = df, level="commit")
{
  if(level=="commit")
  {
    df_agg <- aggregate(df$SATD, df[,c("projectID", "commitHash")], FUN=length)
  }
  else if(level=="file")
  {
    df_agg <- aggregate(df$SATD, df[,c("projectID", "commitHash", "fileName")], FUN=length)
  }
  else if(level!="file" &  level!="commit")
  {
    stop("Wrong level.")
  }
  
  # Number of pairs
  print(paste0("Number of pairs: ", nrow(data_df)/2))
  #KL-SATD in Commits summaries
  print("Commit summary: ")
  print(summary(df_agg$x))
  #Normality test
  shapiro_test <- shapiro.test(df_agg$x)
  print(paste0("Normality test: ", shapiro_test$method, " , W = ", shapiro_test$statistic, " , p-value = ", shapiro_test$p.value))
}

# Finding redundant metrics with redun
find_redundant_metrics <- function(data_df = df)
{
  column_formula2 <- as.formula("~ ncloc_norm + sqaleIndex_norm + reliabilityRemediationEffort_norm + securityRemediationEffort_norm")
  print(redun(column_formula2, data=data_df))
}

# Creating file-level issue forumula
create_issue_formula <- function(data_df = df)
{
  list_of_columns <- colnames(df)
  list_of_columns <- list_of_columns[list_of_columns %like% "_norm$"]
  # Removing TODO and FIXME issues
  list_of_columns_no_fixme_todo <- list_of_columns[!list_of_columns %in% c("squid.S1134_norm", "squid.S1135_norm")]
  column_formula <- paste(list_of_columns_no_fixme_todo, collapse = " + ")
  column_formula <- paste("SATD ~ ", column_formula, sep = "")
  column_formula <- paste(column_formula, " + (1 | projectID / fileName / id)", sep = "")
  column_formula <- as.formula(column_formula)
  return(column_formula)
}

# Normalize issue data within projects and files
normalize_data_within_files_and_projects <- function(data_df = df)
{
  ### Normalize all the issues
  odd_indexes<-seq(1,nrow(data_df),2)
  data_df <- arrange(data_df, id, desc(SATD))
  data_df[is.na(data_df)] <- 0
  data_df <- data_df[, colSums(df != 0) > 0]
  project_and_file_column <- c("projectID", "fileName")
  
  list_of_columns <- colnames(data_df)
  list_of_columns <- list_of_columns[list_of_columns %like% "^code_smells" | list_of_columns %like% "^squid" | list_of_columns %like% "^common.java"]
  for (column_name in list_of_columns){
    data_df[,paste0(column_name, "_norm")] <- NA
    data_df[,paste0(column_name, "_norm")] <- ave(data_df[,column_name], data_df[,project_and_file_column], FUN=function(x) (x-min(x))/(max(x) - min(x)))
  }
  # Replace possible NaNs
  is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
  data_df[is.nan(data_df)] <- 0
  return(data_df)
}


##### End of functions #####

### Start of commit-level analyses
# 1.0 Load the data and combine with sonar measures
# Parameters are as follows:
# level = "commit" or "file"
# satd_type = "add" or "del"
# data_filename = path_to_the_data_file, e.g. "./data/comments/commit_added.csv"
df <- load_and_process_data_file(level = "commit", satd_type = "add", data_filename = "./data/comments/commit_added.csv")
#df <- load_and_process_data_file(level = "commit", satd_type = "del", data_filename = "./data/comments/commit_deleted.csv")

# 1.1. Load the sonar measures
# Parameter is as follows:
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
create_tables_1_and_2(data_df = df, level="commit")

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

### Start of file-level analyses
# 4.0 Loading the data (similarly as with commit-level)
df <- load_and_process_data_file(level = "file", satd_type = "add", data_filename = "./data/comments/file_added.csv")
df <- load_and_process_data_file(level = "file", satd_type = "del", data_filename = "./data/comments/file_deleted.csv")

# 4.1. Load the sonar measures
# Parameter is as follows:
# data_filename = path_to_the_data_file, e.g. "./data/tdd_10/sonar_measures_all.csv"
sonar_measures <- load_sonar_measures(data_filename = "data/tdd_10/sonar_measures_all.csv")

# 4.2. Join the df and sonar measures
df <- left_join(df, sonar_measures, by = c("commitHash", "projectID"))

# 4.3. Eliminate redundant / incomplete data
df <- eliminate_redundant_data(data_df = df)

# 4.4. Normalize data within projects
df <- normalize_data_within_files_and_projects(data_df = df)

# 5.0 Making the Table 2 for the paper
# Table 2 addition row is made with file-level added data, and deletion row with file-level deleted data
create_tables_1_and_2(data_df = df, level = "file")

# 6.0 Creating the file-level formula
issue_formula <- create_issue_formula(data_df = df)

# 6.0  Running the file-level analyses
# This is the main part, running this with file-level additions makes the Table 9.
# Running this with commit-level deletions makes the Table 11.
lmer_model <- glmer(issue_formula, data = df, family = "binomial")
summary(lmer_model)
