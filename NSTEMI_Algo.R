library(tidytext)
library(topicmodels)
library(readxl)
library(tm)
library(dplyr)
library(Matrix)
library(data.table)
library(text2vec)
library(lubridate)
library(dplyr)

MASTERFILE_Notes_Only <- read_excel("<path_to_file>")

df <- MASTERFILE_Notes_Only

#___________________________________________

#subsetting dataset (df)
df$Note_Dttm <- as.Date(df$Note_Dttm, format = "%m/%d/%Y %I:%M:%S %p")
df$Service_Dt <- as.Date(df$Service_Dt, format = "%m/%d/%Y %I:%M:%S %p")

df <- df[as.Date(df$Note_Dttm) == as.Date(df$Service_Dt), ]

#___________________________________________

#subsetting dataset (new_df)
new_df <- df %>%
  group_by(Patient) %>%
  arrange(Note_Dttm) %>%
  slice(1) %>%
  ungroup()

set.seed(1)

#___________________________________________

#creating corpus and pre-processing for NLP (new_df)
corpus <- VCorpus(VectorSource(new_df$Notes))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "continue", "home", "daily", "will", "likely", 
                                        "code", "given", "monitor", "issues","active", "list", "patinet", 
                                        "take", "tablet","history", "recent","day", "night", "end", "result", 
                                        "date", "value", "range", "sentence","relevent", "reason", "planning", 
                                        "current", "past", "days", "oral", "per", "lab", "labs", "insulin",
                                        "medication", "medications", "mgdl", "glucose","today", "bid", "prn", 
                                        "total", "result", "plan","thouul", "collection", "ref", "times", "time",
                                        "results", "current", "past", "prior","admission", "work", "family", 
                                        "follow", "call", "discharge","care", "needs", "disease", "every", "found", 
                                        "pending", "full", "without", "significant", "medicine", "sodium", "last", "tid",
                                        "units", "calcium", "note", "temp", "meds", "injection", "previous", "cath", "height",
                                        "mmoll", "kul", "index", "hospital", "normal", "sedation", "use", "dose", "temporal",
                                        "needed", "social", "phone", "pager", "inpatient", "services", "yes", "no", "medical", 
                                        "patient", "patients", "function", "lvef", "cardiac", "coronary",
                                        "procedure", "artery", "data", "hours", "nstemi", "basename", "hour", "metoprolol",
                                        "diet", "dvt", "anticipatory", "guidance", "impression", "exam", "descriptive", "site", 
                                        "amp", "imaging", "status", "report", "relevant", "events", "tele", 
                                        "gtt", "placed", "started", "completed", "chart", "bed", "review", "daughter", 
                                        "reviewed", "please", "provider", "writer", "capsule", "nightly", "vitals", "tube",
                                        "mcg", "temperature", "rate", "cardiology", "components", "component", "catheterization",
                                        "foley", "blood", "mmhg", "report", "final", "findings", "months", "single", "now", 
                                        "due", "setting", "need", "service", "wife", "class", "information", "room", "none",
                                        "route", "sitting", "assist", "comments", "additional", "mobility", "intravenous",
                                        "physical", "allergies", "icu", "intubated", "assessment", "snf", "pharmcy", "lmsw",
                                        "management", "nurse", "coordinator", "one", "questions", "bmp", "poct", "chloride", "visit",
                                        "pulse", "continuous", "recommendations", "tested", "therapy", "hold", "pharmacy",
                                        "pts", "following", "provided", "also", "ready", "team", "team", "rbc", "high", "surgeon",
                                        "admitted", "discussed", "appreciate", "see", "unit", "order", "aampox", "hematoma", "lmin",
                                        "treatment", "precautions", "chair", "notified", "bedside", "standing", "stand", "sit",
                                        "mlkg", "narrative", "nursing", "transfers", "walker", "device", "condition", "transport",
                                        "office", "smh", "subcutaneous", "son", "health", "contact", "improved", "interpretation",
                                        "view", "images", "standard", "done"))

corpus_dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
corpus_dtm_sparse <- sparseMatrix(i = corpus_dtm$i, j = corpus_dtm$j, x = corpus_dtm$v, dims = dim(corpus_dtm), dimnames = dimnames(corpus_dtm))

nonzero_rows <- rowSums(corpus_dtm_sparse) > 0

corpus_dtm_filtered <- corpus_dtm_sparse[nonzero_rows, ] 

topic_model <- LDA(corpus_dtm_filtered, k = 10, method = "Gibbs")
terms(topic_model, 10)[, 1:5]

#___________________________________________

#creating corpus and pre-processing for NLP (df)
corpus <- VCorpus(VectorSource(df$Notes))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "continue", "home", "daily", "will", "likely", 
                                                    "code", "given", "monitor", "issues","active", "list", "patinet", 
                                                    "take", "tablet","history", "recent","day", "night", "end", "result", 
                                                    "date", "value", "range", "sentence","relevent", "reason", "planning", 
                                                    "current", "past", "days", "oral", "per", "lab", "labs", "insulin",
                                                    "medication", "medications", "mgdl", "glucose","today", "bid", "prn", 
                                                    "total", "result", "plan","thouul", "collection", "ref", "times", "time",
                                                    "results", "current", "past", "prior","admission", "work", "family", 
                                                    "follow", "call", "discharge","care", "needs", "disease", "every", "found", 
                                                    "pending", "full", "without", "significant", "medicine", "sodium", "last", "tid",
                                                    "units", "calcium", "note", "temp", "meds", "injection", "previous", "cath", "height",
                                                    "mmoll", "kul", "index", "hospital", "normal", "sedation", "use", "dose", "temporal",
                                                    "needed", "social", "phone", "pager", "inpatient", "services", "yes", "no", "medical", 
                                                    "patient", "patients", "function", "lvef", "cardiac", "coronary",
                                                    "procedure", "artery", "data", "hours", "nstemi", "basename", "hour", "metoprolol",
                                                    "diet", "dvt", "anticipatory", "guidance", "impression", "exam", "descriptive", "site", 
                                                    "amp", "imaging", "status", "report", "relevant", "events", "tele", 
                                                    "gtt", "placed", "started", "completed", "chart", "bed", "review", "daughter", 
                                                    "reviewed", "please", "provider", "writer", "capsule", "nightly", "vitals", "tube",
                                                    "mcg", "temperature", "rate", "cardiology", "components", "component", "catheterization",
                                                    "foley", "blood", "mmhg", "report", "final", "findings", "months", "single", "now", 
                                                    "due", "setting", "need", "service", "wife", "class", "information", "room", "none",
                                                    "route", "sitting", "assist", "comments", "additional", "mobility", "intravenous",
                                                    "physical", "allergies", "icu", "intubated", "assessment", "snf", "pharmcy", "lmsw",
                                                    "management", "nurse", "coordinator", "one", "questions", "bmp", "poct", "chloride", "visit",
                                                    "pulse", "continuous", "recommendations", "tested", "therapy", "hold", "pharmacy",
                                                    "pts", "following", "provided", "also", "ready", "team", "team", "rbc", "high", "surgeon",
                                                    "admitted", "discussed", "appreciate", "see", "unit", "order", "aampox", "hematoma", "lmin",
                                                    "treatment", "precautions", "chair", "notified", "bedside", "standing", "stand", "sit",
                                                    "mlkg", "narrative", "nursing", "transfers", "walker", "device", "condition", "transport",
                                                    "office", "smh", "subcutaneous", "son", "health", "contact", "improved", "interpretation",
                                                    "view", "images", "standard", "done"))

corpus_dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
corpus_dtm_sparse <- sparseMatrix(i = corpus_dtm$i, j = corpus_dtm$j, x = corpus_dtm$v, dims = dim(corpus_dtm), dimnames = dimnames(corpus_dtm))

nonzero_rows <- rowSums(corpus_dtm_sparse) > 0

corpus_dtm_filtered <- corpus_dtm_sparse[nonzero_rows, ] 

topic_model <- LDA(corpus_dtm_filtered, k = 10, method = "Gibbs")
terms(topic_model, 10)[, 1:5]
