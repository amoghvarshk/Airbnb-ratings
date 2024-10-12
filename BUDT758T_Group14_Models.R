###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Model - 1- Winning Model

############################################################################################################################

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")

dataScale <- function(data , seed = 1){
  
  preProcValues <- preProcess(data[numerical_vars], method = c("center", "scale"))
  df_scaled <- predict(preProcValues, data[numerical_vars])
  data[numerical_vars] <- df_scaled
  
  data<- data%>%
    mutate(
      i1 = price_per_person*high_end_amenitiesTRUE,
      i2 = response_time_categoricalmoderate * host_duration,
      i2_1 = response_time_categoricalslow * host_duration,
      i3 = host_duration * host_verifications_ratio,
      i4 = booking_availability_ratio_30 * booking_availability_ratio_365,
      i5 = booking_availability_ratio_30 * urban_settingTRUE,
      i6 = amenity_count * child_friendlyTRUE,
      i7 = urban_settingTRUE * public_transit_accessTRUE,
      i8 = rural_settingTRUE * pet_friendlyTRUE,
      i9 = flexible_cancellationTRUE * multi_listing_hostTRUE,
      i10 = response_time_categoricalmoderate * booking_availability_ratio_365,
      i10_1 = response_time_categoricalslow * booking_availability_ratio_365,
      i11 = response_time_categoricalmoderate * urban_settingTRUE,
      i11_1 = response_time_categoricalslow * urban_settingTRUE,
      i12 = booking_availability_ratio_30 * price_per_person,
      i13 = booking_availability_ratio_30 * flexible_cancellationTRUE,
      i14 = rural_settingTRUE * high_end_amenitiesTRUE,
      i15 = multi_listing_hostTRUE * host_verifications_ratio,
      i16 = host_duration * has_cleaning_fee.YES
      
    )
  
  
}

#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)




############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x_xg <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
valid_x_xg <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
test_x_xg <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X_xg <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x_xg, fullRank = TRUE)
train_x_xg <- data.frame(predict(dummy, newdata = train_x_xg))



dummy <- dummyVars( ~ . , data=valid_x_xg, fullRank = TRUE)
valid_x_xg <- data.frame(predict(dummy, newdata = valid_x_xg))


dummy <- dummyVars( ~ . , data=test_x_xg, fullRank = TRUE)
test_x_xg <- data.frame(predict(dummy, newdata = test_x_xg))


#Scaling
train_x_xg <- dataScale(train_x_xg)
valid_x_xg <- dataScale(valid_x_xg)
test_x_xg <- dataScale(test_x_xg)

#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X_xg, fullRank = TRUE)
test_X_xg <- data.frame(predict(dummy, newdata = test_X_xg))

test_X_xg <- dataScale(test_X_xg)
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x_xg))
sum(is.na(valid_x_xg))
sum(is.na(test_x_xg))


train_x_xg[is.na(train_x_xg)] <- 0
valid_x_xg[is.na(valid_x_xg)] <- 0
test_x_xg[is.na(test_x_xg)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X_xg[is.na(test_X_xg)] <- 0

#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
##Text Classification
#############################################################################################################################


train_data_a <- train_data %>%
  select(doc_ids, amenities, perfect_rating_score)

Valid_data_a <- Valid_data %>%
  select(doc_ids, amenities, perfect_rating_score)

#############################################################################################################################
#############################################################################################################################
# Real Test dataset
test_real_a <- test_X %>%
  select(doc_ids, amenities)
#############################################################################################################################
#############################################################################################################################


#feature
train_data_f <- train_data %>%
  select(doc_ids, features, perfect_rating_score)

Valid_data_f <- Valid_data %>%
  select(doc_ids, features, perfect_rating_score)

#############################################################################################################################
#############################################################################################################################
# Real Test dataset
test_real_f <- test_X %>%
  select(doc_ids, features)
#############################################################################################################################
#############################################################################################################################

# Amenities Column
train_data_a$perfect_rating_score <- as.factor(train_data$perfect_rating_score)
Valid_data_a$perfect_rating_score <- as.factor(Valid_data$perfect_rating_score)

# Features Column
train_data_f$perfect_rating_score <- as.factor(train_data$perfect_rating_score)
Valid_data_f$perfect_rating_score <- as.factor(Valid_data$perfect_rating_score)

# Preprocess & Tokenize
cleaning_tokenizer <- function(v) {
  v %>%
    space_tokenizer(sep = ',')
}


# Iterate over the individual documents and convert them to tokens.
it_train <- itoken(train_data_f$features,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   ids = train_data_f$doc_ids,
                   progressbar = FALSE)
# Amenities Column
it_test_real_a <- itoken(test_real_a$amenities,
                         preprocessor = tolower,
                         tokenizer = cleaning_tokenizer,
                         ids = test_real_a$doc_ids,
                         progressbar = FALSE)
# Features Column
it_test_real_f <- itoken(test_real_f$features,
                         preprocessor = tolower,
                         tokenizer = cleaning_tokenizer,
                         ids = test_real_f$doc_ids,
                         progressbar = FALSE)

# Create the vocabulary
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
vocab_small <- prune_vocabulary(vocab, term_count_min = 100, doc_proportion_max = 0.5)


# Create a vectorizer object 
vectorizer <- vocab_vectorizer(vocab_small)

# Convert the training articles into a DTM
dtm_train_amenities <- create_dtm(it_train, vectorizer)
dim(dtm_train_amenities)

dtm_test_real_amenities <- create_dtm(it_test_real_a, vectorizer)
dim(dtm_test_real_amenities)

# Convert the validation articles into a DTM
it_valid <- itoken(Valid_data_f$features,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   id = Valid_data_f$doc_ids,
                   progressbar = FALSE)

dtm_valid_amenities <- create_dtm(it_valid, vectorizer)
dim(dtm_valid_amenities)


#features
dtm_train_features <- create_dtm(it_train, vectorizer)
dim(dtm_train_features)

dtm_test_real_features <- create_dtm(it_test_real_f, vectorizer)
dim(dtm_test_real_features)

dtm_valid_features <- create_dtm(it_valid, vectorizer)
dim(dtm_valid_features)



# Make a TFIDF DTM: first line creates a tf-idf model and second line generates the tf-idf
# matrix using the model
tfidf_model <- TfIdf$new()



dtm_train_amenities_tfidf <- fit_transform(dtm_train_amenities, tfidf_model)
dtm_valid_amenities_tfidf <- fit_transform(dtm_valid_amenities, tfidf_model)





dtm_train_features_tfidf <- fit_transform(dtm_train_features, tfidf_model)
dtm_valid_features_tfidf <- fit_transform(dtm_valid_features, tfidf_model)



#############################################################################################################################
#############################################################################################################################
#Real test 
dtm_test_real_amenities_tfidf <- fit_transform(dtm_test_real_amenities, tfidf_model)
dtm_test_real_features_tfidf <- fit_transform(dtm_test_real_features, tfidf_model)
#############################################################################################################################
#############################################################################################################################




#########################################################################
# xgboost+ amenities + features
#########################################################################
dense_matrix_train_amenities <- as.matrix(dtm_train_amenities_tfidf)
dense_matrix_valid_amenities <- as.matrix(dtm_valid_amenities_tfidf)




#features
dense_matrix_train_features <- as.matrix(dtm_train_features_tfidf)
dense_matrix_valid_features <- as.matrix(dtm_valid_features_tfidf)


#############################################################################################################################
#############################################################################################################################
# Real test amenities
dense_matrix_test_real_amenities <- as.matrix(dtm_test_real_amenities_tfidf)
# Real test features
dense_matrix_test_real_features <- as.matrix(dtm_test_real_features_tfidf)



#############################################################################################################################
#############################################################################################################################


# Convert the dense matrix to a data frame
dtm_df_train_amenities <- as.data.frame(dense_matrix_train_amenities)
dtm_df_valid_amenities <- as.data.frame(dense_matrix_valid_amenities)
#dtm_df_test_amenities <- as.data.frame(dense_matrix_test_amenities)



#############################################################################################################################
#############################################################################################################################
dtm_df_test_real_amenities <- as.data.frame(dense_matrix_test_real_amenities)
#############################################################################################################################
#############################################################################################################################

#features
dtm_df_train_features <- as.data.frame(dense_matrix_train_features)
dtm_df_valid_features <- as.data.frame(dense_matrix_valid_features)
#dtm_df_test <- as.data.frame(dense_matrix_test)


#############################################################################################################################
#############################################################################################################################
dtm_df_test_real_features <- as.data.frame(dense_matrix_test_real_features)
#############################################################################################################################
#############################################################################################################################


comb_train <- cbind(train_x_xg, dtm_df_train_amenities, dtm_df_train_features)
comb_valid <- cbind(valid_x_xg, dtm_df_valid_amenities, dtm_df_valid_features)
#comb_test <- cbind(test_x_xg, dtm_df_test_amenities)


#############################################################################################################################
#############################################################################################################################
comb_test_real <- cbind(test_X_xg, dtm_df_test_real_amenities, dtm_df_test_real_features)
#############################################################################################################################
#############################################################################################################################
set.seed(1)

xgboost_bst <- xgboost(
  data = as.matrix(comb_train),
  label = as.numeric(train_y$perfect_rating_score),
  max.depth = 6,               # Reduced model complexity
  eta = 0.05,                   # Fine-tuned learning rate
  subsample = 0.6,             # Reduced subsample ratio
  colsample_bytree = 0.6,      # Reduced column subsampling
  lambda = 0.5,                # Increased L2 regularization
  alpha = 0.1,                 # Increased L1 regularization
  min_child_weight = 18,        # Increased minimum child weight
  gamma = 0.6,                 # Adjusted gamma
  scale_pos_weight = 3.5,        # Optimized class weights
  nrounds = 250,
  objective = "binary:logistic"
)


#############################################################################################################################


vip(xgboost_bst)


#############################################################################################################################

preds_xg <- predict(xgboost_bst, as.matrix(comb_valid))
valid_y_xg_boost <- as.numeric(valid_y$perfect_rating_score)


#preds_xg_test <- predict(xgboost_bst, as.matrix(test_x_xg))
#preds_xg_test <- as.numeric(as.character(preds_xg_test))





#############################################################################################################################
#############################################################################################################################
#FINAL TEST DATA
preds_xg_test <- predict(xgboost_bst, as.matrix(comb_test_real))
preds_xg_test <- as.numeric(as.character(preds_xg_test))

bin_classify_xg_boost <- ifelse(preds_xg_test > 0.7382946, 1, 0)

classifications_perfect <- factor(ifelse(bin_classify_xg_boost == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)

#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################

# 1. create an ROCR "prediction" object
#turns your set of predictions into several vectors
#each one tabulates values for every possible cutoff in your data
pred_full <- prediction(preds_xg, valid_y_xg_boost)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.12))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=8%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value


#############################################################################################################################
# TUNING
#############################################################################################################################
full_dataset<- cbind(comb_train, train_y$perfect_rating_score)

set.seed(1)  # for reproducibility
sample_index <- sample(seq_len(nrow(full_dataset)), size = 15000)  # example: 10,000 records
data_subset <- full_dataset[sample_index, ]


# Define the grid of hyperparameters to search
param_grid <- expand.grid(
  max_depth = c(6),         # Depths of the trees
  eta = c(0.05),        # Learning rate
  subsample = c(0.6),   # Subsample ratio
  colsample_bytree = c(0.6), # Column subsample ratio
  min_child_weight = c(18),  # Minimum sum of instance weight needed in a child
  gamma = c(0.6),         # Minimum loss reduction required to make a further partition
  lambda = c(0.5),           # L2 regularization term on weights
  alpha = c(0.1, 0.5, 1),              # L1 regularization term on weights
  scale_pos_weight = c(3.5),
  list = FALSE
)


# Prepare data
data_feat <- data_subset[, !(names(data_subset) %in% "train_y$perfect_rating_score")]
dtrain <- xgb.DMatrix(data = as.matrix(data_feat), label = as.numeric(data_subset$`train_y$perfect_rating_score`))



best_score <- 0
best_params <- NULL

for(i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  # Convert to list if necessary
  params_list <- as.list(params)
  
  cv_results <- xgb.cv(
    params = params_list,
    data = dtrain,
    nrounds = 4000, # Max number of boosting rounds
    nfold = 5,      # Number of folds in CV
    metrics = "auc",
    early_stopping_rounds = 200,
    seed = 1,
    print_every_n = 100
  )
  
  # Obtain the best score observed during CV
  best_iteration_score <- max(cv_results$evaluation_log$test_auc_mean)  # Change to max and test_auc_mean
  if (best_iteration_score > best_score) {  # Change to greater than for maximizing AUC
    best_score <- best_iteration_score
    best_params <- params
    best_params$nrounds <- cv_results$best_iteration  # Capture the optimal number of rounds
  }
}

print("Best Parameters Found:")
print(best_params)
print(paste("Best Score:", best_score))






#############################################################################################################################

#############################################################################################################################



#############################################################################################################################
# FITTING CURVE
#############################################################################################################################
# Load necessary library
library(ROCR)

# Initialize variables for loop and storage
nrounds_seq <- seq(3, 19, by = 3)
tpr_values <- numeric(length(nrounds_seq))
fpr_target <- 0.10

# Loop over different nrounds
for (i in seq_along(nrounds_seq)) {
  # Train xgboost model
  xgboost_bst <- xgboost(
    data = as.matrix(comb_train),
    label = as.numeric(train_y$perfect_rating_score),
    max.depth = nrounds_seq[i],               
    eta = 0.05,                   
    subsample = 0.6,             
    colsample_bytree = 0.6,      
    lambda = 0.5,                
    alpha = 0.1,                 
    min_child_weight = 18,        
    gamma = 0.6,                 
    scale_pos_weight = 3.5,        
    nrounds = 250,
    objective = "binary:logistic"
  )
  
  # Make predictions
  preds <- predict(xgboost_bst, as.matrix(comb_valid))
  
  # Create ROCR prediction object
  pred_obj <- prediction(preds, valid_y_xg_boost)
  
  # Create ROCR performance object
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  
  # Find index of the threshold closest to 10% FPR
  fpr_at_threshold <- perf_obj@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - fpr_target))
  
  # Store TPR at this FPR threshold
  tpr_values[i] <- perf_obj@y.values[[1]][threshold_index]
}



# Plotting TPR vs nrounds
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "Number of Rounds", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of nrounds on Model Performance")

# Plotting TPR vs lambda
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "Lambda", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of lambda on Model Performance")

# Plotting TPR vs max_depth
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "max_depth", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of max_depth on Model Performance")





#############################################################################################################################
# LEARNING CURVE
#############################################################################################################################


# Set seed for reproducibility
set.seed(1)

# Define the proportion of data to use
train_proportions <- seq(0.1, 1, by = 0.1)  # from 10% to 100% of the data

# Prepare to store TPR values
tpr_values <- numeric(length(train_proportions))
fpr_target <- 0.10  # FPR target

# Loop over different proportions of training data
for (i in seq_along(train_proportions)) {
  # Determine the number of samples to use
  sample_size <- floor(nrow(comb_train) * train_proportions[i])
  indices <- sample(1:nrow(comb_train), size = sample_size)
  
  # Subset the training data
  data_matrix_subset <- as.matrix(comb_train[indices, ])
  label_vector_subset <- as.numeric(train_y$perfect_rating_score[indices])
  
  set.seed(1)
  
  # Train xgboost model
  xgboost_bst <- xgboost(
    data = data_matrix_subset,
    label = label_vector_subset,
    max.depth = 6,
    eta = 0.05,
    subsample = 0.6,
    colsample_bytree = 0.6,
    lambda = 0.5,
    alpha = 0.1,
    min_child_weight = 18,
    gamma = 0.6,
    scale_pos_weight = 3.5,
    nrounds = 250,
    objective = "binary:logistic",
    verbose = 0  # Turn off verbose output
  )
  
  # Make predictions on the validation set
  preds <- predict(xgboost_bst, as.matrix(comb_valid))
  
  # Create an ROCR prediction object
  pred <- prediction(preds, as.numeric(valid_y$perfect_rating_score))
  
  # Create ROCR performance object
  perf <- performance(pred, "tpr", "fpr")
  
  # Find index of the threshold closest to 10% FPR
  fpr_at_threshold <- perf@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - fpr_target))
  
  # Store TPR at this FPR threshold
  tpr_values[i] <- perf@y.values[[1]][threshold_index]
}

# Plotting the learning curve: TPR vs. proportion of training data used
plot(train_proportions, tpr_values, type = "o", col = "blue", pch = 19,
     xlab = "Proportion of Training Data Used", ylab = "True Positive Rate at 10% FPR",
     main = "Learning Curve: TPR vs. Training Data Proportion")





###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
#Model - 2 - Logistic Regression

# Load the necessary libraries
library(readr)
library(dplyr)
library(caret)
library(pROC)
library(lubridate)

# Set working directory
setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')

# Load datasets
train_X <- read_csv("airbnb_train_x_2024.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_x_2024.csv")

# Merge the training features and labels
train_data <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))

# Split data into training and validation sets (70% training, 20% validation, 10% test)
dataSplit <- function(data_aggre, train_prop, valid_prop, seed = 1) {
  set.seed(seed)
  total_rows <- nrow(data_aggre)
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

# Perform data splitting
results <- dataSplit(train_data, train_prop = 0.7, valid_prop = 0.2, 1)
train <- results$train
valid <- results$valid
test <- results$test

# Data cleaning function
dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy))
      ))
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    mutate(ppp_ind = as.factor(ifelse(price_per_person > median(price_per_person), 1, 0)))
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      price_per_accommodate = as.numeric(sub("\\$", "", price)) / accommodates,
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}

# Clean the training, validation, and test sets
train <- dataClean(train)
valid <- dataClean(valid)
test <- dataClean(test)

# Train a logistic regression model
model_logistic <- glm(perfect_rating_score ~  availability_30 + availability_365 + 
                        availability_60 + availability_90 + bathrooms + bedrooms + beds + 
                        has_cleaning_fee + has_extra_people + guests_included  +
                        host_listings_count + host_response_rate + host_total_listings_count +
                        latitude +  minimum_nights + price_per_person + bed_category + 
                        cancellation_policy+has_security_deposit+ ppp_ind + property_category, 
                      data = train, family = "binomial")



cf_func <- function(valid_data, model_trained, cutoff){
  predictions <- predict(model_trained, newdata = valid_data, type = "response")
  predictions_class <- factor(ifelse(predictions > cutoff, "YES", "NO"), levels = c("YES", "NO"))
  actual <- factor(valid_data$perfect_rating_score, levels = c("YES", "NO"))
  confusion_matrix <- confusionMatrix(data = predictions_class, reference = actual)
  return(confusion_matrix)
}


predictions_prob <- predict(model_logistic, newdata = valid, type = "response")

cf_logistic <- cf_func(valid, model_logistic, 0.42)
print(cf_logistic)


TP <- cf_logistic$table[1,1]
TN <- cf_logistic$table[2,2]
FP <- cf_logistic$table[1,2]
FN <- cf_logistic$table[2,1]


accuracy <- (TN + TP)/ (TN + TP + FP + FN)
print(accuracy)

TPR <- TP/(TP+FN)
TNR <- TN/(TN+FP)
FPR <- 1-TNR

print(TPR)
print(FPR)



actual_binary <- as.factor(ifelse(valid$perfect_rating_score == "YES", 1, 0))

# Compute the ROC curve
roc_obj <- roc(response = actual_binary, predictor = predictions_prob)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Logistic Regression", col = "#1c61b6", lwd = 2)

####################################################



###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Model -3- Logistic with Lasso



############################################################################################################################

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")



#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)




############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x <- subset(train_x, select = c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
valid_x <- subset(valid_x, select = c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
test_x <- subset(test_x, select = c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X <- subset(test_X, select = c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x, fullRank = TRUE)
train_x <- data.matrix(data.frame(predict(dummy, newdata = train_x)))



dummy <- dummyVars( ~ . , data=valid_x, fullRank = TRUE)
valid_x <- data.matrix(data.frame(predict(dummy, newdata = valid_x)))


dummy <- dummyVars( ~ . , data=test_x, fullRank = TRUE)
test_x <- data.matrix(data.frame(predict(dummy, newdata = test_x)))




#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X, fullRank = TRUE)
test_X <- data.matrix(data.frame(predict(dummy, newdata = test_X)))


#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x))
sum(is.na(valid_x))
sum(is.na(test_x))


train_x[is.na(train_x)] <- 0
valid_x[is.na(valid_x)] <- 0
test_x[is.na(test_x)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X[is.na(test_X)] <- 0


grid <- 10^seq(-1,-4,length=20)



k<-5




#alpha = 1 yields the lasso penalty
cv.out <- cv.glmnet(train_x, as.matrix(train_y), family="binomial", alpha=1, lambda=grid, nfolds=k)
plot(cv.out)


#get the lambda that gave the lowest cross-validated error
bestlam <- cv.out$lambda.min
coeffs <- coef(cv.out, s = "lambda.min")


#you have to add type="response" to get probabilities for logistic regression
pred <- predict(cv.out, s=bestlam, newx = valid_x,type="response")

#you can also plot a fitting curve
lambdas <- cv.out$lambda
errors <- cv.out$cvm

plot(lambdas, errors)

#with this particular set of lambdas, you can see the curve easier if you plot them on a log-log scale
plot(log(lambdas), errors)


classifications <- ifelse(pred > .5, "YES", "NO")

classifications_perfect <- factor(ifelse(classifications == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)


#ROC
pred_prob <- predict(cv.out, s = bestlam, newx = valid_x, type = "response")

# Create an ROC object
roc_obj <- roc(valid_y$perfect_rating_score, pred_prob)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "#1c61b6", lwd = 2)
















pred_full <- prediction(pred, valid_y)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.10))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=8%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value



#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#Model - 4 - Logistic with Ridge

############################################################################################################################

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")



#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)




############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
valid_x <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
test_x <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x, fullRank = TRUE)
train_x <- data.matrix(data.frame(predict(dummy, newdata = train_x)))



dummy <- dummyVars( ~ . , data=valid_x, fullRank = TRUE)
valid_x <- data.matrix(data.frame(predict(dummy, newdata = valid_x)))


dummy <- dummyVars( ~ . , data=test_x, fullRank = TRUE)
test_x <- data.matrix(data.frame(predict(dummy, newdata = test_x)))




#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X, fullRank = TRUE)
test_X <- data.matrix(data.frame(predict(dummy, newdata = test_X)))


#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x))
sum(is.na(valid_x))
sum(is.na(test_x))


train_x[is.na(train_x)] <- 0
valid_x[is.na(valid_x)] <- 0
test_x[is.na(test_x)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X[is.na(test_X)] <- 0


grid <- 10^seq(-1,-4,length=20)



k<-5

#alpha = 1 yields the Ridge penalty
cv.out <- cv.glmnet(train_x,as.matrix(train_y), family="binomial", alpha=0, lambda=grid, nfolds=k)
plot(cv.out)


#get the lambda that gave the lowest cross-validated error
bestlam <- cv.out$lambda.min
coeffs <- coef(cv.out, s = "lambda.min")
coeffs

#you have to add type="response" to get probabilities for logistic regression
pred <- predict(cv.out, s=bestlam, newx = valid_x,type="response")

#you can also plot a fitting curve
lambdas <- cv.out$lambda
errors <- cv.out$cvm

plot(lambdas, errors)

#with this particular set of lambdas, you can see the curve easier if you plot them on a log-log scale
plot(log(lambdas), errors)


classifications <- ifelse(pred > .5, "YES", "NO")

classifications_perfect <- factor(ifelse(classifications == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)

#ROC
pred_prob <- predict(cv.out, s = bestlam, newx = valid_x, type = "response")

# Create an ROC object
roc_obj <- roc(valid_y$perfect_rating_score, pred_prob)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "#1c61b6", lwd = 2)


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

# Model -5 - Random Forest

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)
library(randomForest)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")

dataScale <- function(data , seed = 1){
  
  preProcValues <- preProcess(data[numerical_vars], method = c("center", "scale"))
  df_scaled <- predict(preProcValues, data[numerical_vars])
  data[numerical_vars] <- df_scaled
  
  data<- data%>%
    mutate(
      i1 = price_per_person*high_end_amenitiesTRUE,
      i2 = response_time_categoricalmoderate * host_duration,
      i2_1 = response_time_categoricalslow * host_duration,
      i3 = host_duration * host_verifications_ratio,
      i4 = booking_availability_ratio_30 * booking_availability_ratio_365,
      i5 = booking_availability_ratio_30 * urban_settingTRUE,
      i6 = amenity_count * child_friendlyTRUE,
      i7 = urban_settingTRUE * public_transit_accessTRUE,
      i8 = rural_settingTRUE * pet_friendlyTRUE,
      i9 = flexible_cancellationTRUE * multi_listing_hostTRUE,
      i10 = response_time_categoricalmoderate * booking_availability_ratio_365,
      i10_1 = response_time_categoricalslow * booking_availability_ratio_365,
      i11 = response_time_categoricalmoderate * urban_settingTRUE,
      i11_1 = response_time_categoricalslow * urban_settingTRUE,
      i12 = booking_availability_ratio_30 * price_per_person,
      i13 = booking_availability_ratio_30 * flexible_cancellationTRUE,
      i14 = rural_settingTRUE * high_end_amenitiesTRUE,
      i15 = multi_listing_hostTRUE * host_verifications_ratio,
      i16 = host_duration * has_cleaning_fee.YES
      
    )
  
  
}

#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)





############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x_xg <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
valid_x_xg <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
test_x_xg <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X_xg <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x_xg, fullRank = TRUE)
train_x_xg <- data.frame(predict(dummy, newdata = train_x_xg))



dummy <- dummyVars( ~ . , data=valid_x_xg, fullRank = TRUE)
valid_x_xg <- data.frame(predict(dummy, newdata = valid_x_xg))


dummy <- dummyVars( ~ . , data=test_x_xg, fullRank = TRUE)
test_x_xg <- data.frame(predict(dummy, newdata = test_x_xg))


#Scaling
train_x_xg <- dataScale(train_x_xg)
valid_x_xg <- dataScale(valid_x_xg)
test_x_xg <- dataScale(test_x_xg)

#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X_xg, fullRank = TRUE)
test_X_xg <- data.frame(predict(dummy, newdata = test_X_xg))

test_X_xg <- dataScale(test_X_xg)
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x_xg))
sum(is.na(valid_x_xg))
sum(is.na(test_x_xg))


train_x_xg[is.na(train_x_xg)] <- 0
valid_x_xg[is.na(valid_x_xg)] <- 0
test_x_xg[is.na(test_x_xg)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X_xg[is.na(test_X_xg)] <- 0


# Checking dimensions






rf.mod <- randomForest(x=train_x_xg,
                       y=as.matrix(train_y),
                       mtry=6, ntree=120,
                       importance=TRUE)

rf_preds <- predict(rf.mod, newdata=valid_x_xg)





preds <- predict(rf.mod, valid_x_xg)
valid_y <- as.numeric(valid_y$perfect_rating_score)






#############################################################################################################################
#############################################################################################################################
#FINAL TEST DATA
preds_xg_test <- predict(xgboost_bst, as.matrix(comb_test_real))
preds_xg_test <- as.numeric(as.character(preds_xg_test))

bin_classify_ <- ifelse(preds > 0.7382946, 1, 0)

classifications_perfect <- factor(ifelse(bin_classify == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)




pred_full <- prediction(rf_preds, valid_y)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.10))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=8%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value

























train_y <- factor(train_y, levels = c("0", "1"))
valid_y <- factor(valid_y, levels = c("0", "1"))

# Sequence of mtry values
mtry_values <- seq(2, 10, by = 2)
tpr_values_mtry <- numeric(length(mtry_values))

for (i in seq_along(mtry_values)) {
  model <- randomForest(x = train_x_xg, y = as.matrix(train_y), mtry = mtry_values[i], ntree = 2)
  preds <- predict(model, newdata=valid_x_xg)
  pred_obj <- prediction(preds, (valid_y))
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  
  # Extract TPR at 10% FPR
  fpr_at_threshold <- perf_obj@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - 0.10))
  tpr_values_mtry[i] <- perf_obj@y.values[[1]][threshold_index]
}

# Plotting TPR vs mtry
df_mtry <- data.frame(mtry = mtry_values, TPR = tpr_values_mtry)
ggplot(df_mtry, aes(x = mtry, y = TPR)) +
  geom_line() +
  geom_point() +
  labs(title = "TPR vs. mtry", x = "mtry", y = "TPR")







# Sequence of ntree values
ntree_values <- seq(100, 1000, by = 100)
tpr_values_ntree <- numeric(length(ntree_values))

for (i in seq_along(ntree_values)) {
  model <- randomForest(x = train_x_xg, y = as.matrix(train_y), mtry = 6, ntree = ntree_values[i])
  preds <- predict(model, valid_x_xg, type = "prob")[,2]
  pred_obj <- prediction(preds, as.matrix(valid_y))
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  
  # Extract TPR at 10% FPR
  fpr_at_threshold <- perf_obj@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - 0.10))
  tpr_values_ntree[i] <- perf_obj@y.values[[1]][threshold_index]
}

# Plotting TPR vs ntree
df_ntree <- data.frame(ntree = ntree_values, TPR = tpr_values_ntree)
ggplot(df_ntree, aes(x = ntree, y = TPR)) +
  geom_line() +
  geom_point() +
  labs(title = "TPR vs. ntree", x = "ntree", y = "TPR")



best_model <- randomForest(x = train_x_xg, y = train_y, mtry = 6, ntree = 550)
preds <- predict(best_model, valid_x_xg, type = "prob")[,2]
roc_data <- prediction(preds, valid_y)
roc_performance <- performance(roc_data, measure = "tpr", x.measure = "fpr")

# Plot ROC curve
plot(roc_performance, main = "ROC Curve", col = "#1c61b6", lwd = 2)
abline(0, 1, lty = 2, col = "red")  # Line of no discrimination


################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################

#Model - 6 - Xgboost


############################################################################################################################

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")

dataScale <- function(data , seed = 1){
  
  preProcValues <- preProcess(data[numerical_vars], method = c("center", "scale"))
  df_scaled <- predict(preProcValues, data[numerical_vars])
  data[numerical_vars] <- df_scaled
  
  data<- data%>%
    mutate(
      i1 = price_per_person*high_end_amenitiesTRUE,
      i2 = response_time_categoricalmoderate * host_duration,
      i2_1 = response_time_categoricalslow * host_duration,
      i3 = host_duration * host_verifications_ratio,
      i4 = booking_availability_ratio_30 * booking_availability_ratio_365,
      i5 = booking_availability_ratio_30 * urban_settingTRUE,
      i6 = amenity_count * child_friendlyTRUE,
      i7 = urban_settingTRUE * public_transit_accessTRUE,
      i8 = rural_settingTRUE * pet_friendlyTRUE,
      i9 = flexible_cancellationTRUE * multi_listing_hostTRUE,
      i10 = response_time_categoricalmoderate * booking_availability_ratio_365,
      i10_1 = response_time_categoricalslow * booking_availability_ratio_365,
      i11 = response_time_categoricalmoderate * urban_settingTRUE,
      i11_1 = response_time_categoricalslow * urban_settingTRUE,
      i12 = booking_availability_ratio_30 * price_per_person,
      i13 = booking_availability_ratio_30 * flexible_cancellationTRUE,
      i14 = rural_settingTRUE * high_end_amenitiesTRUE,
      i15 = multi_listing_hostTRUE * host_verifications_ratio,
      i16 = host_duration * has_cleaning_fee.YES
      
    )
  
  
}

#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)




############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x_xg <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
valid_x_xg <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))
test_x_xg <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X_xg <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x_xg, fullRank = TRUE)
train_x_xg <- data.frame(predict(dummy, newdata = train_x_xg))



dummy <- dummyVars( ~ . , data=valid_x_xg, fullRank = TRUE)
valid_x_xg <- data.frame(predict(dummy, newdata = valid_x_xg))


dummy <- dummyVars( ~ . , data=test_x_xg, fullRank = TRUE)
test_x_xg <- data.frame(predict(dummy, newdata = test_x_xg))


#Scaling
train_x_xg <- dataScale(train_x_xg)
valid_x_xg <- dataScale(valid_x_xg)
test_x_xg <- dataScale(test_x_xg)

#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X_xg, fullRank = TRUE)
test_X_xg <- data.frame(predict(dummy, newdata = test_X_xg))

test_X_xg <- dataScale(test_X_xg)
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x_xg))
sum(is.na(valid_x_xg))
sum(is.na(test_x_xg))


train_x_xg[is.na(train_x_xg)] <- 0
valid_x_xg[is.na(valid_x_xg)] <- 0
test_x_xg[is.na(test_x_xg)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X_xg[is.na(test_X_xg)] <- 0

#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#############################################################################################################################


comb_train <- cbind(train_x_xg)
comb_valid <- cbind(valid_x_xg)
comb_test <- cbind(test_x_xg)


#############################################################################################################################
#############################################################################################################################
comb_test_real <- cbind(test_X_xg)
#############################################################################################################################
#############################################################################################################################
set.seed(1)


xgboost_bst <- xgboost(
  data = as.matrix(comb_train),
  label = as.numeric(train_y$perfect_rating_score),
  max.depth = 6,               # Reduced model complexity
  eta = 0.05,                   # Fine-tuned learning rate
  subsample = 0.6,             # Reduced subsample ratio
  colsample_bytree = 0.6,      # Reduced column subsampling
  lambda = 0.5,                # Increased L2 regularization
  alpha = 0.1,                 # Increased L1 regularization
  min_child_weight = 18,        # Increased minimum child weight
  gamma = 0.6,                 # Adjusted gamma
  scale_pos_weight = 3.5,        # Optimized class weights
  nrounds = 250,
  objective = "binary:logistic"
)


#############################################################################################################################


vip(xgboost_bst)


#############################################################################################################################

preds_xg <- predict(xgboost_bst, as.matrix(comb_valid))
valid_y_xg_boost <- as.numeric(valid_y$perfect_rating_score)







#############################################################################################################################
#############################################################################################################################
#FINAL TEST DATA
preds_xg_test <- predict(xgboost_bst, as.matrix(comb_test_real))
preds_xg_test <- as.numeric(as.character(preds_xg_test))

bin_classify_xg_boost <- ifelse(preds_xg_test > 0.7382946, 1, 0)

classifications_perfect <- factor(ifelse(bin_classify_xg_boost == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)

#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################

# 1. create an ROCR "prediction" object
#turns your set of predictions into several vectors
#each one tabulates values for every possible cutoff in your data
pred_full <- prediction(preds_xg, valid_y_xg_boost)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.10))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=8%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value




#############################################################################################################################
# FITTING CURVE
#############################################################################################################################
# Load necessary library
library(ROCR)

# Initialize variables for loop and storage
nrounds_seq <- seq(3, 19, by = 3)
tpr_values <- numeric(length(nrounds_seq))
fpr_target <- 0.10

# Loop over different nrounds
for (i in seq_along(nrounds_seq)) {
  # Train xgboost model
  xgboost_bst <- xgboost(
    data = as.matrix(comb_train),
    label = as.numeric(train_y$perfect_rating_score),
    max.depth = nrounds_seq[i],               
    eta = 0.05,                   
    subsample = 0.6,             
    colsample_bytree = 0.6,      
    lambda = 0.5,                
    alpha = 0.1,                 
    min_child_weight = 18,        
    gamma = 0.6,                 
    scale_pos_weight = 3.5,        
    nrounds = 250,
    objective = "binary:logistic"
  )
  
  # Make predictions
  preds <- predict(xgboost_bst, as.matrix(comb_valid))
  
  # Create ROCR prediction object
  pred_obj <- prediction(preds, valid_y_xg_boost)
  
  # Create ROCR performance object
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  
  # Find index of the threshold closest to 10% FPR
  fpr_at_threshold <- perf_obj@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - fpr_target))
  
  # Store TPR at this FPR threshold
  tpr_values[i] <- perf_obj@y.values[[1]][threshold_index]
}



# Plotting TPR vs nrounds
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "Number of Rounds", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of nrounds on Model Performance")

# Plotting TPR vs lambda
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "Lambda", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of lambda on Model Performance")

# Plotting TPR vs max_depth
plot(nrounds_seq, tpr_values, type = "b", col = "blue", pch = 19,
     xlab = "max_depth", ylab = "True Positive Rate at 10% FPR",
     main = "Effect of max_depth on Model Performance")




data_matrix <- as.matrix(comb_train)
labels <- as.numeric(train_y$perfect_rating_score)
valid_matrix <- as.matrix(comb_valid)
valid_labels <- as.numeric(valid_y$perfect_rating_score)

# Parameters for plotting
fpr_target <- 0.10







nrounds_seq <- seq(50, 500, by = 50)
tpr_nrounds <- numeric(length(nrounds_seq))

for (i in seq_along(nrounds_seq)) {
  model <- xgboost(data = data_matrix, label = labels, max.depth = 6,
                   eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
                   lambda = 1, alpha = 0.1, min_child_weight = 1,
                   gamma = 0.1, scale_pos_weight = 1, nrounds = nrounds_seq[i],
                   objective = "binary:logistic", verbose = 0)
  preds <- predict(model, valid_matrix)
  pred_obj <- prediction(preds, valid_labels)
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  tpr_nrounds[i] <- perf_obj@y.values[[1]][which.min(abs(perf_obj@x.values[[1]] - fpr_target))]
}
plot(nrounds_seq, tpr_nrounds, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Boosting Rounds", ylab = "TPR at 10% FPR", 
     main = "TPR vs Number of Boosting Rounds")











lambda_seq <- seq(0, 1, by = 0.1)
tpr_lambda <- numeric(length(lambda_seq))

for (i in seq_along(lambda_seq)) {
  model <- xgboost(data = data_matrix, label = labels, max.depth = 6,
                   eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
                   lambda = lambda_seq[i], alpha = 0.1, min_child_weight = 1,
                   gamma = 0.1, scale_pos_weight = 1, nrounds = 250,
                   objective = "binary:logistic", verbose = 0)
  preds <- predict(model, valid_matrix)
  pred_obj <- prediction(preds, valid_labels)
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  tpr_lambda[i] <- perf_obj@y.values[[1]][which.min(abs(perf_obj@x.values[[1]] - fpr_target))]
}
plot(lambda_seq, tpr_lambda, type = "b", col = "blue", pch = 19,
     xlab = "Lambda ", ylab = "TPR at 10% FPR",
     main = "TPR vs Lambda")






max_depth_seq <- seq(3, 15, by = 1)
tpr_max_depth <- numeric(length(max_depth_seq))

for (i in seq_along(max_depth_seq)) {
  model <- xgboost(data = data_matrix, label = labels, max.depth = max_depth_seq[i],
                   eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
                   lambda = 1, alpha = 0.1, min_child_weight = 1,
                   gamma = 0.1, scale_pos_weight = 1, nrounds = 250,
                   objective = "binary:logistic", verbose = 0)
  preds <- predict(model, valid_matrix)
  pred_obj <- prediction(preds, valid_labels)
  perf_obj <- performance(pred_obj, "tpr", "fpr")
  tpr_max_depth[i] <- perf_obj@y.values[[1]][which.min(abs(perf_obj@x.values[[1]] - fpr_target))]
}
plot(max_depth_seq, tpr_max_depth, type = "b", col = "blue", pch = 19,
     xlab = "Max Depth", ylab = "TPR at 10% FPR",
     main = "TPR vs Max Depth")













#############################################################################################################################
# LEARNING CURVE
#############################################################################################################################


# Set seed for reproducibility
set.seed(1)

# Define the proportion of data to use
train_proportions <- seq(0.1, 1, by = 0.1)  # from 10% to 100% of the data

# Prepare to store TPR values
tpr_values <- numeric(length(train_proportions))
fpr_target <- 0.10  # FPR target

# Loop over different proportions of training data
for (i in seq_along(train_proportions)) {
  # Determine the number of samples to use
  sample_size <- floor(nrow(comb_train) * train_proportions[i])
  indices <- sample(1:nrow(comb_train), size = sample_size)
  
  # Subset the training data
  data_matrix_subset <- as.matrix(comb_train[indices, ])
  label_vector_subset <- as.numeric(train_y$perfect_rating_score[indices])
  
  set.seed(1)
  
  # Train xgboost model
  xgboost_bst <- xgboost(
    data = data_matrix_subset,
    label = label_vector_subset,
    max.depth = 6,
    eta = 0.05,
    subsample = 0.6,
    colsample_bytree = 0.6,
    lambda = 0.5,
    alpha = 0.1,
    min_child_weight = 18,
    gamma = 0.6,
    scale_pos_weight = 3.5,
    nrounds = 250,
    objective = "binary:logistic",
    verbose = 0  # Turn off verbose output
  )
  
  # Make predictions on the validation set
  preds <- predict(xgboost_bst, as.matrix(comb_valid))
  
  # Create an ROCR prediction object
  pred <- prediction(preds, as.numeric(valid_y$perfect_rating_score))
  
  # Create ROCR performance object
  perf <- performance(pred, "tpr", "fpr")
  
  # Find index of the threshold closest to 10% FPR
  fpr_at_threshold <- perf@x.values[[1]]
  threshold_index <- which.min(abs(fpr_at_threshold - fpr_target))
  
  # Store TPR at this FPR threshold
  tpr_values[i] <- perf@y.values[[1]][threshold_index]
}

# Plotting the learning curve: TPR vs. proportion of training data used
plot(train_proportions, tpr_values, type = "o", col = "blue", pch = 19,
     xlab = "Proportion of Training Data Used", ylab = "True Positive Rate at 10% FPR",
     main = "Learning Curve: TPR vs. Training Data Proportion")


#############################################################################################################################
# TUNING
#############################################################################################################################
full_dataset<- cbind(comb_train, train_y$perfect_rating_score)

set.seed(1)  # for reproducibility
sample_index <- sample(seq_len(nrow(full_dataset)), size = 15000)  # example: 10,000 records
data_subset <- full_dataset[sample_index, ]


# Define the grid of hyperparameters to search
param_grid <- expand.grid(
  max_depth = c(6),         # Depths of the trees
  eta = c(0.05),        # Learning rate
  subsample = c(0.6),   # Subsample ratio
  colsample_bytree = c(0.6), # Column subsample ratio
  min_child_weight = c(18),  # Minimum sum of instance weight needed in a child
  gamma = c(0.6),         # Minimum loss reduction required to make a further partition
  lambda = c(0.5),           # L2 regularization term on weights
  alpha = c(0.1, 0.5, 1),              # L1 regularization term on weights
  scale_pos_weight = c(3.5),
  list = FALSE
)


# Prepare data
data_feat <- data_subset[, !(names(data_subset) %in% "train_y$perfect_rating_score")]
dtrain <- xgb.DMatrix(data = as.matrix(data_feat), label = as.numeric(data_subset$`train_y$perfect_rating_score`))



best_score <- 0
best_params <- NULL

for(i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  # Convert to list if necessary
  params_list <- as.list(params)
  
  cv_results <- xgb.cv(
    params = params_list,
    data = dtrain,
    nrounds = 4000, # Max number of boosting rounds
    nfold = 5,      # Number of folds in CV
    metrics = "auc",
    early_stopping_rounds = 200,
    seed = 1,
    print_every_n = 100
  )
  
  # Obtain the best score observed during CV
  best_iteration_score <- max(cv_results$evaluation_log$test_auc_mean)  # Change to max and test_auc_mean
  if (best_iteration_score > best_score) {  # Change to greater than for maximizing AUC
    best_score <- best_iteration_score
    best_params <- params
    best_params$nrounds <- cv_results$best_iteration  # Capture the optimal number of rounds
  }
}

print("Best Parameters Found:")
print(best_params)
print(paste("Best Score:", best_score))






#############################################################################################################################

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#Model - 7 - Ensemble model Random forest + Xgboost




############################################################################################################################

set.seed(1)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
train_X <- read_csv("airbnb_train_x_2024.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_x_2024.csv")






############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 1 )
{
  set.seed(seed)
  
  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate),
         doc_ids = row_number())


#Final Test
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.7,valid_prop = 0.2, 1)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.7,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test



############################################################################################################################
############################################################################################################################

# No splitting for the Real test data

############################################################################################################################
############################################################################################################################




#Data Cleaning

dataClean  <- function(data , seed = 1 )
  
{
  data_Staging <- data %>%
    mutate(
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
    )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
  
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")

dataScale <- function(data , seed = 1){
  
  preProcValues <- preProcess(data[numerical_vars], method = c("center", "scale"))
  df_scaled <- predict(preProcValues, data[numerical_vars])
  data[numerical_vars] <- df_scaled
  
  data<- data%>%
    mutate(
      i1 = price_per_person*high_end_amenitiesTRUE,
      i2 = response_time_categoricalmoderate * host_duration,
      i2_1 = response_time_categoricalslow * host_duration,
      i3 = host_duration * host_verifications_ratio,
      i4 = booking_availability_ratio_30 * booking_availability_ratio_365,
      i5 = booking_availability_ratio_30 * urban_settingTRUE,
      i6 = amenity_count * child_friendlyTRUE,
      i7 = urban_settingTRUE * public_transit_accessTRUE,
      i8 = rural_settingTRUE * pet_friendlyTRUE,
      i9 = flexible_cancellationTRUE * multi_listing_hostTRUE,
      i10 = response_time_categoricalmoderate * booking_availability_ratio_365,
      i10_1 = response_time_categoricalslow * booking_availability_ratio_365,
      i11 = response_time_categoricalmoderate * urban_settingTRUE,
      i11_1 = response_time_categoricalslow * urban_settingTRUE,
      i12 = booking_availability_ratio_30 * price_per_person,
      i13 = booking_availability_ratio_30 * flexible_cancellationTRUE,
      i14 = rural_settingTRUE * high_end_amenitiesTRUE,
      i15 = multi_listing_hostTRUE * host_verifications_ratio,
      i16 = host_duration * has_cleaning_fee.YES
      
    )
  
  
}

#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)




############################################################################################################################
#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
############################################################################################################################
#############################################################################################################################




#############################################################################################################################



train_x_xg <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity"))
valid_x_xg <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity"))
test_x_xg <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity"))



#############################################################################################################################
#############################################################################################################################
#Real Test Data

test_X_xg <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity"))

#############################################################################################################################
#############################################################################################################################



train_y <- data.frame(perfect_rating_score = train_y)
valid_y <- data.frame(perfect_rating_score = valid_y)
test_y <- data.frame(perfect_rating_score = test_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))









dummy <- dummyVars( ~ . , data=train_x_xg, fullRank = TRUE)
train_x_xg <- data.frame(predict(dummy, newdata = train_x_xg))



dummy <- dummyVars( ~ . , data=valid_x_xg, fullRank = TRUE)
valid_x_xg <- data.frame(predict(dummy, newdata = valid_x_xg))


dummy <- dummyVars( ~ . , data=test_x_xg, fullRank = TRUE)
test_x_xg <- data.frame(predict(dummy, newdata = test_x_xg))


#Scaling
train_x_xg <- dataScale(train_x_xg)
valid_x_xg <- dataScale(valid_x_xg)
test_x_xg <- dataScale(test_x_xg)

#############################################################################################################################
#############################################################################################################################

# Real Test dataset
dummy <- dummyVars( ~ . , data=test_X_xg, fullRank = TRUE)
test_X_xg <- data.frame(predict(dummy, newdata = test_X_xg))

test_X_xg <- dataScale(test_X_xg)
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################

# Check for missing values


sum(is.na(train_x_xg))
sum(is.na(valid_x_xg))
sum(is.na(test_x_xg))


train_x_xg[is.na(train_x_xg)] <- 0
valid_x_xg[is.na(valid_x_xg)] <- 0
test_x_xg[is.na(test_x_xg)] <- 0




#############################################################################################################################
#############################################################################################################################

test_X_xg[is.na(test_X_xg)] <- 0

#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
##Text Classification
#############################################################################################################################


train_data_a <- train_data %>%
  select(doc_ids, amenities, perfect_rating_score)

Valid_data_a <- Valid_data %>%
  select(doc_ids, amenities, perfect_rating_score)

test_data_a <- test_data %>%
  select(doc_ids, amenities, perfect_rating_score)

#############################################################################################################################
#############################################################################################################################
# Real Test dataset
test_real_a <- test_X %>%
  select(doc_ids, amenities)

#############################################################################################################################
#############################################################################################################################

# Amenities Column
train_data_a$perfect_rating_score <- as.factor(train_data$perfect_rating_score)
Valid_data_a$perfect_rating_score <- as.factor(Valid_data$perfect_rating_score)


# Preprocess & Tokenize
cleaning_tokenizer <- function(v) {
  v %>%
    space_tokenizer(sep = ',')
}


# Iterate over the individual documents and convert them to tokens.
it_train <- itoken(train_data_a$amenities,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   ids = train_data_a$doc_ids,
                   progressbar = FALSE)
# Amenities Column
it_test_real_a <- itoken(test_real_a$amenities,
                         preprocessor = tolower,
                         tokenizer = cleaning_tokenizer,
                         ids = test_real_a$doc_ids,
                         progressbar = FALSE)


# Create the vocabulary
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
vocab_small <- prune_vocabulary(vocab, term_count_min = 100, doc_proportion_max = 0.5)


# Create a vectorizer object 
vectorizer <- vocab_vectorizer(vocab_small)

# Convert the training articles into a DTM
dtm_train_amenities <- create_dtm(it_train, vectorizer)
dim(dtm_train_amenities)

dtm_test_real_amenities <- create_dtm(it_test_real_a, vectorizer)
dim(dtm_test_real_amenities)

# Convert the validation articles into a DTM
it_valid <- itoken(Valid_data_a$amenities,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   id = Valid_data_a$doc_ids,
                   progressbar = FALSE)

dtm_valid_amenities <- create_dtm(it_valid, vectorizer)
dim(dtm_valid_amenities)

# Convert the test articles into a DTM
it_test <- itoken(test_data_a$amenities,
                  preprocessor = tolower,
                  tokenizer = cleaning_tokenizer,
                  id = test_data_a$doc_ids,
                  progressbar = FALSE)

dtm_test_amenities <- create_dtm(it_test, vectorizer)
dim(dtm_valid_amenities)




# Make a TFIDF DTM: first line creates a tf-idf model and second line generates the tf-idf
# matrix using the model
tfidf_model <- TfIdf$new()



dtm_train_amenities_tfidf <- fit_transform(dtm_train_amenities, tfidf_model)
dtm_valid_amenities_tfidf <- fit_transform(dtm_valid_amenities, tfidf_model)
dtm_test_amenities_tfidf <- fit_transform(dtm_test_amenities, tfidf_model)





#############################################################################################################################
#############################################################################################################################
#Real test 
dtm_test_real_amenities_tfidf <- fit_transform(dtm_test_real_amenities, tfidf_model)

#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
# RANGER
#############################################################################################################################
library(ranger)
# Training a random forest model
rf_model <- ranger(x = dtm_train_amenities_tfidf, y = train_data$perfect_rating_score,
                   mtry = 17, num.trees = 650,
                   importance = "impurity", probability = TRUE)

preds_rf <- predict(rf_model, data = dtm_valid_amenities_tfidf)$predictions[, 2]
preds_rf_test <- predict(rf_model, data = dtm_test_real_amenities_tfidf)$predictions[, 2]


# Calculate the AUC for the random forest model
preds_rf_rocr <- prediction(preds_rf, Valid_data$perfect_rating_score)
auc_rf <- performance(preds_rf_rocr, "auc")@y.values[[1]]
cat("Validation AUC for Random Forest:", auc_rf, "\n")

# Variable importance plot
vip(rf_model)



#############################################################################################################################
#############################################################################################################################




#########################################################################
# xgboost
#########################################################################


comb_train <- cbind(train_x_xg)
comb_valid <- cbind(valid_x_xg)
comb_test <- cbind(test_x_xg)


#############################################################################################################################
#############################################################################################################################
comb_test_real <- cbind(test_X_xg)
#############################################################################################################################
#############################################################################################################################
set.seed(1)

xgboost_bst <- xgboost(
  data = as.matrix(comb_train),
  label = as.numeric(train_y$perfect_rating_score),
  max.depth = 6,               # Reduced model complexity
  eta = 0.05,                   # Fine-tuned learning rate
  subsample = 0.6,             # Reduced subsample ratio
  colsample_bytree = 0.6,      # Reduced column subsampling
  lambda = 0.5,                # Increased L2 regularization
  alpha = 0.1,                 # Increased L1 regularization
  min_child_weight = 18,        # Increased minimum child weight
  gamma = 0.6,                 # Adjusted gamma
  scale_pos_weight = 3.5,        # Optimized class weights
  nrounds = 250,
  objective = "binary:logistic"
)


#############################################################################################################################


vip(xgboost_bst)


#############################################################################################################################

preds_xg <- predict(xgboost_bst, as.matrix(comb_valid))
valid_yst <- as.numeric(valid_y$perfect_rating_score)



preds_comb <- (preds_rf + preds_xg)/2





#############################################################################################################################
#############################################################################################################################
#FINAL TEST DATA
preds_xg_test <- predict(xgboost_bst, as.matrix(comb_test_real))
preds_xg_test <- as.numeric(as.character(preds_xg_test))

preds_comb_test <- (preds_xg_test + preds_rf_test)/2

bin_classify_xg_boost <- ifelse(preds_comb_test > 0.5551366, 1, 0)

classifications_perfect <- factor(ifelse(bin_classify_xg_boost == 1, "YES", "NO"), levels = c("YES", "NO"))

#############################################################################################################################
#output your predictions

write.table(classifications_perfect, "fpr_12_0.1.csv", row.names = FALSE)

#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################

# 1. create an ROCR "prediction" object
#turns your set of predictions into several vectors
#each one tabulates values for every possible cutoff in your data
pred_full <- prediction(preds_comb, valid_y)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.10))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=10%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value



