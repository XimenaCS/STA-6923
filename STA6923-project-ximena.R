#packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(e1071)
library(AppliedPredictiveModeling)
library(caret)

df<-read.csv("train.csv")

dim(df)
head(df)
summary(df)

#Missing values
sum(is.na(df))
missing_tbl <- data.frame(
  missing = sapply(df, function(x) sum(is.na(x)))
) %>%
  mutate(na_percent = round(100 * missing / nrow(df), 2)) %>%
  filter(missing > 0) %>%  
  arrange(desc(na_percent))
missing_tbl

#delete columns with >50% na's: PoolQc, MiscFeature, Alley, Fence
keep_cols <- names(df)[sapply(df, function(x) mean(is.na(x)) <= 0.50)]
df_clean <- df[, keep_cols, drop = FALSE]

#work NA's
for (v in intersect(c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2"), names(df_clean))) {
  df_clean[[v]][is.na(df_clean[[v]])] <- "None"}
if (all(c("Fireplaces","FireplaceQu") %in% names(df_clean)))
  df_clean$FireplaceQu[is.na(df_clean$FireplaceQu) & df_clean$Fireplaces == 0] <- "None"
for (v in intersect(c("GarageType","GarageFinish","GarageQual","GarageCond"), names(df_clean))) {
  df_clean[[v]][is.na(df_clean[[v]])] <- "None"}
if ("GarageYrBlt" %in% names(df_clean) && "YearBuilt" %in% names(df_clean))
  df_clean$GarageYrBlt[is.na(df_clean$GarageYrBlt)] <- df_clean$YearBuilt[is.na(df_clean$GarageYrBlt)]
df_clean$MasVnrType[is.na(df_clean$MasVnrType)] <- "None"
df_clean$MasVnrArea[is.na(df_clean$MasVnrArea)] <- 0
elec_mode <- names(sort(table(df_clean$Electrical), decreasing = TRUE))[1]
df_clean$Electrical[is.na(df_clean$Electrical)] <- elec_mode

df_clean <- df_clean %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(LotFrontage, na.rm = TRUE),
                              LotFrontage)) %>%  ungroup()

#verify no NA's
sum(is.na(df_clean))

df_clean$MSSubClass <- factor(df_clean$MSSubClass)
cat_cols <- sapply(df_clean, function(x) is.character(x) || is.factor(x))
cat_df <- df_clean[, cat_cols]
num_cols <- sapply(df_clean, is.numeric)
num_df <- df_clean[, num_cols]
num_df <- num_df %>% select(-Id)

#check skewness
skew_tbl <- sapply(df_clean[num_cols], function(x) skewness(x, na.rm = TRUE, type = 2))
skew_tbl <- sort(skew_tbl, decreasing = TRUE)
limit <- 20
skewed <- names(skew_tbl[skew_tbl > limit])

#log-transform


#correlation
corr_vec <- cor(num_df, use = "pairwise.complete.obs")
sp_corr <- sort(corr_vec[ , "SalePrice"], decreasing = TRUE)
corrplot(corr_vec)
sp_corr_top <- head(sp_corr[ names(sp_corr) != "SalePrice" ], 15) #top15
sp_corr_top


#SalePrice distribution
g1 <- ggplot(df_clean, aes(x = SalePrice)) +
  geom_histogram(fill = "blue") +
  labs(title = "Distribution of SalePrice", x = "SalePrice", y = "Count") +
  theme_minimal()
g1

#Price vs. GrLivArea
g2 <- ggplot(df_clean, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color = "red") +
  labs(title = "SalePrice vs. GrLivArea", x = "GrLivArea", y = "SalePrice") +
  theme_minimal()
g2

#price vs totalbsmtsf
g3 <- ggplot(df_clean, aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point(color = "green") +
  labs(title = "SalePrice vs. TotalBsmtSF", x = "TotalBsmtSF", y = "SalePrice") +
  theme_minimal()
g3

#price vs garagecats
g4 <- ggplot(df_clean, aes(x = GarageCars, y = SalePrice)) +
  geom_point(color = "purple") +
  labs(title = "SalePrice vs. GarageCars", x = "GarageCars", y = "SalePrice") +
  theme_minimal()
g4


#Boxplot SalePrice by Neighborhood (Top 20)
top_nb20 <- df_clean %>% count(Neighborhood, sort = TRUE) %>% slice_head(n = 20) %>% pull(Neighborhood)
houses_top20 <- df_clean %>% filter(Neighborhood %in% top_nb20)

box_nb <- ggplot(houses_top20, aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice)) +
  geom_boxplot(fill = "darkblue", outlier.alpha = 0.5) +
  coord_flip() +
  labs(title = "SalePrice by Neighborhood (Top 20)", x = "Neighborhood", y = "SalePrice") +
  theme_minimal()
box_nb