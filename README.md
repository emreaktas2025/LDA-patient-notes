# Topic Models from Electronic Health Records Notes to Classify NSTEMI Patients Based on the Presence of an Occluded Culprit Artery
Clinical notes transcribed during the first 72 hours of hospitalization were extracted from the EHR among NSTEMI patients who presented to an academic medical center  between 2015-2020. This data is not in the repository, only the source code used to generate the topic models.

The data was preprocessed by removing punctuation and numbers, removing stop words, and reducing words to their root form.  We used Latent Dirichlet Allocation (LDA), a type of deep learning Bayesian network, to generate 5 topics. Five topics was determined based on multiple iterations and coherence value. We annotated whether the first 100 key terms for each class were associated with an occluded artery or not. Descriptive statistics were also generated. All analyses were completed in RStudio (4.3.0) using the packages topicmodels and tidyverse.

