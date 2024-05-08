## FALL 23 BSA Article

### Evaluating when to go for it on 4th down in the NFL
###   STEP 1: create logistic regressions model for all 4th down tries ~2015-2021 (based on yds to go)
###   STEP 2: test model on all tries last year, show success rate of model (should prolly split 70/30 instead, can show known/interesting examples)
###           - sort by field position for controversial decisions
###           - model will suggest ti go for short yardage on any place on the field, independent of score
###           - thus we need wpa - i.e. 4th and 1 on own 15 while up 2 points and little time left
###   STEP 3: introduce wpa into evaluation of decision:
###           - 2 models:
###             -  wpa on successful conversions based on field position and yds to go (predicts wpa if converted)
###             -  wpa on unsuccesful conversions based on field position and yds to go (predicts wpa if not converted)
###           - Predict wpa_if_converted and wpa_if_failed for all 4th downs (converted/unsuccesful/no try)
###           - Create variable should_go_for_it:8
###             -  if wpa_if_converted > wpa_if_failed ~ TRUE
###             -  if wpa_if_converted < wpa_if_failed ~ FALSE
###             -  if wpa_if_failed == wpa_if_converted ~ NA
###           - Pick certain instances (include summary of outcome) to describe in which:
###             - should_go_for_it == TRUE & went for it
###             - should_go_for_it == TRUE & didn't go for it
###             - should_go_for_it == FALSE & went for it
###             - should_go_for_it == FALSE & didn't go for it

library(dplyr, warn.conflicts = FALSE)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(stringr)
library(nflfastR)

options(scipen = 9999)

pbp_current <- load_pbp(2012:2022)

pbp_current <- pbp_current %>%
  mutate(year = str_sub(game_id, 1, 4))

names(pbp_current)

## subset by: play == 1, down == 4

### Relevant Columns: game_id, ydstogo, year, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed

set.seed(93)

pbp_4th_downs <- pbp_current %>% 
  select(posteam, wp, field_goal_result, half_seconds_remaining, game_seconds_remaining, epa, wpa, game_id, qtr, score_differential, season_type, penalty, end_clock_time, year, ydstogo, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed, special_teams_play, down) %>%
  filter(season_type == "REG", penalty != 1, play_type != "no_play", down == 4, year %in% (2012:2022))

pbp_4th_downs <- pbp_4th_downs %>% 
  mutate(went_for_it = play_type %in% c("pass", "run"),
         fgm = "")

attempts <- pbp_4th_downs %>%
  filter(play_type %in% c("pass", "run"), special_teams_play != 1, year %in% (2012:2022))

# subset attempts to meaningful ones (no mail mary's, blowouts, kneel downs, must-convert scenarios inside 5 mins, etc.) -  5+% win prob, 10 or less yards to go, 30+ seconds remaining in the half

meaningful_fourth_downs <- pbp_4th_downs %>%
  filter(half_seconds_remaining >= 30, 
         game_seconds_remaining >= 300, 
         wp >= .02, ydstogo <= 30, 
         qtr %in% (1:4), 
         wp <= 0.99)

meaningful_attempts <- meaningful_fourth_downs %>%
  filter(play_type %in% c("pass", "run"), 
         special_teams_play != 1, 
         year %in% (2012:2022))


### FG and PUNT

meaningful_fg <- meaningful_fourth_downs %>%
  filter(play_type == "field_goal") %>%
  mutate(fgm = as.numeric(field_goal_result == "made"))


meaningful_fg$fgm

fg_train_test <- meaningful_fg %>%
  mutate(id = row_number())

fg_train <- fg_train_test %>% sample_frac(.70)

# Test set
fg_test  <- anti_join(fg_train_test, fg_train, by = 'id')






#############

# FG LOGISTIC REGRESSION GRAPH


plot(conversion_prob ~ ydstogo, data = meaningful_fourth_downs) #



fg_regression <- glm(fgm ~ yardline_100, data = fg_train, family = binomial)
plot(fgm ~ yardline_100, data=fg_train, pch = 16,
     col = rgb(red = 0, green = 0, blue = 0, alpha = 0.06),
     main = "Probability of Making a Field Goal",
     ylab = "Probability of FGM",
     xlab = "Yard Line")

newdat <- data.frame(yardline_100 =seq(min(fg_train_test$yardline_100), max(fg_train_test$yardline_100),len=100))
newdat$fgm = predict(fg_regression, newdata=newdat, type="response")
lines(fgm ~ yardline_100, data=newdat, col="dodgerblue", lwd=2)
legend(2, 0.7, legend = c("Predicted Probability", "Field Goal Attempts"), 
       lty = c(1, NA), 
       pch = c(NA, 16),
       col = c("dodgerblue", "black"))

#############



#############

# FG LOGISTIC REGRESSION SUCCESS %

fg_test_predictor <- predict(fg_regression, newdata=fg_test, type="response")
fg_test_predicted <- cbind(fg_test, fg_test_predictor)
fg_test_predicted <- fg_test_predicted %>%
  mutate(pred_outcome = round(fg_test_predictor, 0)) %>%
  mutate(correct_pred = (pred_outcome == fgm))

fg_test_results <- with(fg_test_predicted, table(correct_pred))

fg_succcess_rate <- fg_test_results[2]/(fg_test_results[1] + fg_test_results[2])

#############

meaningful_fourth_downs$fgm_prob <- predict(fg_regression, newdata = meaningful_fourth_downs, type = "response")




# need to predict on 2023 here - just for interesting/known scenarios






wpa_fgm <- meaningful_fg %>%
  select(fgm, epa, wpa, half_seconds_remaining, game_seconds_remaining, game_id, qtr, score_differential, penalty, end_clock_time, year, ydstogo, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed, special_teams_play, down) %>%
  filter(fgm == 1)

head(wpa_fgm)

wpa_fgm_train_test <- wpa_fgm %>%
  mutate(id = row_number())

set.seed(93)

wpa_fgm_train <- wpa_fgm_train_test %>% sample_frac(.70)

# Test set
wpa_fgm_test  <- anti_join(wpa_fgm_train_test, wpa_fgm_train, by = 'id')



# take into account score differential, results are better

# do individual models so I can graph (maybe just use the better one)

wpa_fgm_model <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining + ydstogo, data = wpa_fgm_train) # removed ydstogo(?)
wpa_fgm_model_noyds <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining, data = wpa_fgm_train) # removed ydstogo(?)
wpa_fgm_model1 <- lm(wpa ~ abs(score_differential), data = wpa_fgm_train)
wpa_fgm_model2 <- lm(wpa ~ ydstogo, data = wpa_fgm_train)
wpa_fgm_model3 <- lm(wpa ~ yardline_100, data = wpa_fgm_train)
wpa_fgm_model4 <- lm(wpa ~ game_seconds_remaining, data = wpa_fgm_train)

summary(wpa_fgm_model_noyds)
summary(wpa_fgm_model1)
summary(wpa_fgm_model2)
summary(wpa_fgm_model3)
summary(wpa_fgm_model4)





#############

# INDIVIDUAL FGM PLOTS

par( mfrow= c(2,2) , mar = c(7,4,4,2))

plot(wpa ~ ydstogo, data = wpa_fgm_train, sub = "R = 0.37",
     ylim = c(-0.25, 0.25),
     xlim = c(0, 30),
     main = "WPA vs. Yards to Go",
     ylab = "WPA",
     xlab = "Yards to Go")
abline(wpa_fgm_model2, col = "dodgerblue")


plot(wpa ~ abs(score_differential), data = wpa_fgm_train, sub = "R = -0.12",
     ylim = c(-0.25, 0.25),
     xlim = c(0, 30),
     main = "WPA vs. Score Differential",
     ylab = "WPA",
     xlab = "Score Differential")
abline(wpa_fgm_model1, col = "dodgerblue")


plot(wpa ~ game_seconds_remaining, data = wpa_fgm_train, sub = "R = -0.06",
     ylim = c(-0.25, 0.25),
     xlim = c(0, 3600),
     main = "WPA vs. Seconds Remaining",
     ylab = "WPA",
     xlab = "Seconds Remaning")
abline(wpa_fgm_model4, col = "dodgerblue")


plot(wpa ~ yardline_100, data = wpa_fgm_train, sub = "R = 0.62",
     ylim = c(-0.25, 0.25),
     xlim = c(0, 45),
     main = "WPA vs. Yard Line",
     ylab = "WPA",
     xlab = "Yard Line")
abline(wpa_fgm_model3, col = "dodgerblue")

#############




#############

# FGM WPA vs PRED PLOT

par(mfrow = c(1,1), mar = c(8, 8, 5, 5))

wpa_fgm_predict <- predict(wpa_fgm_model_noyds, newdata = wpa_fgm_test)

wpa_fgm_test$wpa_pred <- wpa_fgm_predict

sqrt(.3786)

plot(wpa ~ wpa_pred, data = wpa_fgm_test, ylim = c(-0.3, 0.3), xlim = c(-0.3, 0.3), sub = "R = 0.62",
     main = "Predicted WPA vs. WPA\n(Field Goal Makes)",
     ylab = "WPA",
     xlab = "Predicted WPA")

wpa_fgm_accuracy <- lm(wpa ~ wpa_pred, data = wpa_fgm_test)

summary(wpa_fgm_accuracy)


abline(wpa_fgm_accuracy, col = "dodgerblue")

#############





## FG MISS

wpa_fg_miss <- meaningful_fg %>%
  select(fgm, epa, wpa, half_seconds_remaining, game_seconds_remaining, game_id, qtr, score_differential, penalty, end_clock_time, year, ydstogo, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed, special_teams_play, down) %>%
  filter(fgm == 0)

head(wpa_fg_miss)

wpa_fg_miss_train_test <- wpa_fg_miss %>%
  mutate(id = row_number())

set.seed(93)

wpa_fg_miss_train <- wpa_fg_miss_train_test %>% sample_frac(.70)

# Test set
wpa_fg_miss_test  <- anti_join(wpa_fg_miss_train_test, wpa_fg_miss_train, by = 'id')



# take into account score differential, results are better

# do individual models so I can graph (maybe just use the better one)

wpa_fg_miss_model <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining + ydstogo, data = wpa_fg_miss_train) # removed ydstogo(?)
wpa_fg_miss_model_noyds <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining, data = wpa_fg_miss_train) # removed ydstogo(?)
wpa_fg_miss_model1 <- lm(wpa ~ abs(score_differential), data = wpa_fg_miss_train)
wpa_fg_miss_model2 <- lm(wpa ~ ydstogo, data = wpa_fg_miss_train)
wpa_fg_miss_model3 <- lm(wpa ~ yardline_100, data = wpa_fg_miss_train)
wpa_fg_miss_model4 <- lm(wpa ~ game_seconds_remaining, data = wpa_fg_miss_train)

summary(wpa_fg_miss_model_noyds)
summary(wpa_fg_miss_model1)
summary(wpa_fg_miss_model2)
summary(wpa_fg_miss_model3)
summary(wpa_fg_miss_model4)

sqrt(0.4124)





#############

# INDIVIDUAL FG MISS GRAPHS

par( mfrow= c(2,2) , mar = c(7,4,4,2))
plot(wpa ~ ydstogo, data = wpa_fg_miss_train, sub = "R = 0.20",
     main = "WPA vs. Yards to Go",
     ylab = "WPA",
     xlab = "Yards to Go",
     ylim = c(-.45, .05),
     xlim = c(0, 30))
abline(wpa_fg_miss_model2, col = "dodgerblue")

plot(wpa ~ abs(score_differential), data = wpa_fg_miss_train, sub = "R = 0.60",
     main = "WPA vs. Score Differential",
     ylab = "WPA",
     xlab = "Score Differential",
     ylim = c(-.45, .05),
     xlim = c(0, 30))
abline(wpa_fg_miss_model1, col = "dodgerblue")

plot(wpa ~ game_seconds_remaining, data = wpa_fg_miss_train, sub = "R = -0.02",
     main = "WPA vs. Seconds Remaining",
     ylab = "WPA",
     xlab = "Seconds Remaining",
     ylim = c(-.45, .05),
     xlim = c(0, 3600))
abline(wpa_fg_miss_model4, col = "dodgerblue")

plot(wpa ~ yardline_100, data = wpa_fg_miss_train, sub = "R = 0.15",
     main = "WPA vs. Yard Line",
     ylab = "WPA",
     xlab = "Yard Line",
     ylim = c(-.45, .05),
     xlim = c(0, 45))
abline(wpa_fg_miss_model3, col = "dodgerblue")



#############





#############

# FG MISS WPA vs PRED PLOT

par(mfrow = c(1,1), mar = c(8, 8, 5, 5))

wpa_fg_miss_predict <- predict(wpa_fg_miss_model_noyds, newdata = wpa_fg_miss_test)

wpa_fg_miss_test$wpa_pred <- wpa_fg_miss_predict

plot(wpa ~ wpa_pred, data = wpa_fg_miss_test, ylim = c(-0.3, 0.3), xlim = c(-0.3, 0.3), sub = "R = 0.64",
     main = "Predicted WPA vs. WPA\n(Field Goal Misses)",
     ylab = "WPA",
     xlab = "Predicted WPA")

wpa_fg_miss_accuracy <- lm(wpa ~ wpa_pred, data = wpa_fg_miss_test)

summary(wpa_fg_miss_accuracy)

sqrt(0.4046)

abline(wpa_fg_miss_accuracy, col = "dodgerblue")

#############






pbp_wpa_if_fgm <- predict(wpa_fgm_model_noyds, meaningful_fourth_downs)

meaningful_fourth_downs$wpa_if_fgm <- pbp_wpa_if_fgm

pbp_wpa_if_fg_miss <- predict(wpa_fg_miss_model_noyds, meaningful_fourth_downs)

meaningful_fourth_downs$wpa_if_fg_miss <- pbp_wpa_if_fg_miss

meaningful_fourth_downs <- meaningful_fourth_downs %>%
  mutate(fg_expected_wpa = (wpa_if_fgm * fgm_prob) + (wpa_if_fg_miss * (1 - fgm_prob)))








#############


# 4th down attempted rate and success rate 2012-2022

year_succ_table <- with(meaningful_attempts, table(year, fourth_down_converted))
year_succ_table1 <- with(attempts, table(year, fourth_down_converted))


succ_perc_by_year <- data.frame("Year" = 2012:2022,
                                "Success Rate" = year_succ_table[,2]/(year_succ_table[,1] + year_succ_table[,2]))
succ_perc_by_year1 <- data.frame("Year" = 2012:2022,
                                "Success Rate" = year_succ_table1[,2]/(year_succ_table1[,1] + year_succ_table1[,2]))

year_freq_table <- with(pbp_4th_downs, table(year, went_for_it))
year_freq_table1 <- with(meaningful_fourth_downs, table(year, went_for_it))

attempt_perc_by_year <- data.frame("Year" = 2012:2022,
                                   "Attempt Rate" = year_freq_table[,2]/(year_freq_table[,1] + year_freq_table[,2]))
attempt_perc_by_year1 <- data.frame("Year" = 2012:2022,
                                   "Attempt Rate" = year_freq_table1[,2]/(year_freq_table1[,1] + year_freq_table1[,2]))

par(mfrow = c(1, 1), mar = c(8,8,8 ,8))


plot(Success.Rate ~ Year, data = succ_perc_by_year, ylim = c(0, 0.7),
     pch = 16,
     ylab = "4th Down Conversion Rate",
     main = "NFL 4th Down Conversion Rate by Year\n(Meaningful Attempts)")



plot(Attempt.Rate ~ Year, data = attempt_perc_by_year, ylim = c(0, 0.25), pch = 16,
     ylab = "4th Down Attempt Rate",
     main = "NFL 4th Down Attempt Rate by Year\n(Meaningful Attempts)")




#############




# Train and test ydstogo model



train_test <- meaningful_attempts %>%
  mutate(id = row_number())

train <- train_test %>% sample_frac(.70)

# Test set
test  <- anti_join(train_test, train, by = 'id')


regression <- glm(fourth_down_converted ~ ydstogo, data = train, family = binomial)


#############


# YDSTOGO 4TH DOWN LOGSTIC MODEL GRAPH

plot(fourth_down_converted ~ ydstogo, data=train, pch = 16,
     col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09),
     main = "Probability of Converting a 4th Down",
     ylab = "Probability of Conversion",
     xlab = "Yards to Go")

newdat <- data.frame(ydstogo=seq(min(train_test$ydstogo), max(train_test$ydstogo),len=100))
newdat$fourth_down_converted = predict(regression, newdata=newdat, type="response")
lines(fourth_down_converted ~ ydstogo, data=newdat, col="dodgerblue", lwd=2)
legend(15, 0.9, legend = c("Predicted Probability", "4th Down Attempts"), 
       lty = c(1, NA), 
       pch = c(NA, 16),
       col = c("dodgerblue", "black"))


#############






#############

# YDSTOGO LOGISTIC REGRESSION SUCCESS %


test_predictor <- predict(regression, newdata=test, type="response")
test_predicted <- cbind(test, test_predictor)
test_predicted <- test_predicted %>%
  mutate(pred_outcome = round(test_predictor, 0)) %>%
  mutate(correct_pred = (pred_outcome == fourth_down_converted))

test_results <- with(test_predicted, table(correct_pred))

succcess_rate <- test_results[2]/(test_results[1] + test_results[2])

#############

meaningful_fourth_downs$conversion_prob <- predict(regression, newdata = meaningful_fourth_downs, type = "response")










### WPA Attempt Models

wpa_conv <- meaningful_attempts %>%
  select(epa, wpa, half_seconds_remaining, game_seconds_remaining, game_id, qtr, score_differential, penalty, end_clock_time, year, ydstogo, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed, special_teams_play, down) %>%
  filter(fourth_down_converted == 1)
  
head(wpa_conv)

wpa_conv_train_test <- wpa_conv %>%
  mutate(id = row_number())

set.seed(93)

wpa_conv_train <- wpa_conv_train_test %>% sample_frac(.70)

# Test set
wpa_conv_test  <- anti_join(wpa_conv_train_test, wpa_conv_train, by = 'id')

# take into account score differential, results are better

# do individual models so I can graph (maybe just use the better one)

wpa_conv_model <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining + ydstogo, data = wpa_conv_train) # removed ydstogo(?)
wpa_conv_model_noyds <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining, data = wpa_conv_train) # removed ydstogo(?)
wpa_conv_model1 <- lm(wpa ~ abs(score_differential), data = wpa_conv_train)
wpa_conv_model2 <- lm(wpa ~ ydstogo, data = wpa_conv_train)
wpa_conv_model3 <- lm(wpa ~ yardline_100, data = wpa_conv_train)
wpa_conv_model4 <- lm(wpa ~ game_seconds_remaining, data = wpa_conv_train)

summary(wpa_conv_model_noyds)
summary(wpa_conv_model1)
summary(wpa_conv_model2)
summary(wpa_conv_model3)
summary(wpa_conv_model4)

sqrt(0.01865)


#############


# INDIVIDUAL CONV GRAPHS

par( mfrow= c(2,2), mar = c(7,4,4,2))
plot(wpa ~ ydstogo, data = wpa_conv_train, sub = "R = 0.09",
     main = "WPA vs. Yards to Go",
     ylab = "WPA",
     xlab = "Yards to Go",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 20))
abline(wpa_conv_model2, col = "dodgerblue")


plot(wpa ~ abs(score_differential), data = wpa_conv_train, sub = "R = -0.52",
     main = "WPA vs. Score Differential",
     ylab = "WPA",
     xlab = "Score Differential",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 30))
abline(wpa_conv_model1, col = "dodgerblue")

plot(wpa ~ game_seconds_remaining, data = wpa_conv_train, sub = "R = 0.14",
     main = "WPA vs. Seconds Remaining",
     ylab = "WPA",
     xlab = "Seconds Remaining",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 3600))
abline(wpa_conv_model4, col = "dodgerblue")

plot(wpa ~ yardline_100, data = wpa_conv_train, sub = "R = -0.12",
     main = "WPA vs. Yard Line",
     ylab = "WPA",
     xlab = "Yard Line",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 85))
abline(wpa_conv_model3, col = "dodgerblue")


#############




#############

# CONV WPA vs PRED PLOT

par(mfrow = c(1,1), mar = c(8, 8, 5, 5))

wpa_conv_predict <- predict(wpa_conv_model_noyds, newdata = wpa_conv_test)

wpa_conv_test$wpa_pred <- wpa_conv_predict

wpa_conv_accuracy <- lm(wpa ~ wpa_pred, data = wpa_conv_test)

summary(wpa_conv_accuracy)

plot(wpa ~ wpa_pred, data = wpa_conv_test, ylim = c(-0.3, 0.3), xlim = c(-0.3, 0.3), sub = "R = 0.52",
     main = "Predicted WPA vs. WPA\n(Fourth Down Conversions)",
     ylab = "WPA",
     xlab = "Predicted WPA")

abline(wpa_conv_accuracy, col = "dodgerblue")


#############







wpa_fail <- meaningful_attempts %>%
  select(game_seconds_remaining, epa, wpa, game_id, qtr, score_differential, penalty, end_clock_time, year, ydstogo, yardline_100, play_type, down, fourth_down_converted, fourth_down_failed, special_teams_play, down) %>%
  filter(fourth_down_failed == 1)

head(wpa_fail)

wpa_fail_train_test <- wpa_fail %>%
  mutate(id = row_number())

set.seed(90)

wpa_fail_train <- wpa_fail_train_test %>% sample_frac(.70)

# Test set
wpa_fail_test  <- anti_join(wpa_fail_train_test, wpa_fail_train, by = 'id')

wpa_fail_model <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining + ydstogo, data = wpa_fail_train) 
wpa_fail_model_noyds <- lm(wpa ~ abs(score_differential) + yardline_100 + game_seconds_remaining, data = wpa_fail_train)# removed ydstogo(?)
wpa_fail_model1 <- lm(wpa ~ abs(score_differential), data = wpa_fail_train)
wpa_fail_model2 <- lm(wpa ~ ydstogo, data = wpa_fail_train)
wpa_fail_model3 <- lm(wpa ~ yardline_100, data = wpa_fail_train)
wpa_fail_model4 <- lm(wpa ~ game_seconds_remaining, data = wpa_fail_train)

summary(wpa_fail_model_noyds)
summary(wpa_fail_model1)
summary(wpa_fail_model2)
summary(wpa_fail_model3)
summary(wpa_fail_model4)


#############

# INDIVIDUAL FAIL GRAPHS


sqrt(0.04505)

par( mfrow= c(2,2), mar = c(7,4,4,2))
plot(wpa ~ ydstogo, data = wpa_fail_train, sub = "R = 0.36",
     main = "WPA vs. Yards to Go",
     ylab = "WPA",
     xlab = "Yards to Go",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 20))
abline(wpa_fail_model2, col = "dodgerblue")

plot(wpa ~ abs(score_differential), data = wpa_fail_train, sub = "R = 0.67",
     main = "WPA vs. Score Differential",
     ylab = "WPA",
     xlab = "Score Differential",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 30))
abline(wpa_fail_model1, col = "dodgerblue")

plot(wpa ~ game_seconds_remaining, data = wpa_fail_train, sub = "R = -0.21",
     main = "WPA vs. Seconds Remaining",
     ylab = "WPA",
     xlab = "Seconds Remaining",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 3600))
abline(wpa_fail_model4, col = "dodgerblue")

plot(wpa ~ yardline_100, data = wpa_fail_train, sub = "R = 0.29",
     main = "WPA vs. Yard Line",
     ylab = "WPA",
     xlab = "Yard Line",
     ylim = c(-0.3, 0.3),
     xlim = c(0, 85))
abline(wpa_fail_model3, col = "dodgerblue")

#############



#############

# FAIL WPA vs PRED PLOT

par(mfrow = c(1,1), mar = c(8, 8, 5, 5))

wpa_fail_predict <- predict(wpa_fail_model_noyds, newdata = wpa_fail_test)

wpa_fail_test$wpa_pred <- wpa_fail_predict

wpa_fail_accuracy <- lm(wpa ~ wpa_pred, data = wpa_fail_test)

summary(wpa_fail_accuracy)

sqrt(0.5017)

plot(wpa ~ wpa_pred, data = wpa_fail_test, ylim = c(-0.3, 0.3), xlim = c(-0.3, 0.3), sub = "R = 0.71",
     main = "Predicted WPA vs. WPA\n(Failed Fourth Down Attempts)",
     ylab = "WPA",
     xlab = "Predicted WPA")

abline(wpa_fail_accuracy, col = "dodgerblue")


#############





# use both models to predict on all fourth downs:

pbp_wpa_if_fail <- predict(wpa_fail_model_noyds, meaningful_fourth_downs)

meaningful_fourth_downs$wpa_if_fail <- pbp_wpa_if_fail

pbp_wpa_if_conv <- predict(wpa_conv_model_noyds, meaningful_fourth_downs)

meaningful_fourth_downs$wpa_if_conv <- pbp_wpa_if_conv

meaningful_fourth_downs <- meaningful_fourth_downs %>%
  mutate(expected_wpa = (wpa_if_conv * conversion_prob) + (wpa_if_fail * (1 - conversion_prob)))

meaningful_fourth_downs <- meaningful_fourth_downs %>%
  mutate(should_go_for_it1 = (expected_wpa > fg_expected_wpa) & (expected_wpa > 0),
         should_kick = (fg_expected_wpa > expected_wpa) & (fg_expected_wpa > 0),
         should_punt = !should_go_for_it1 & !should_kick)



meaningful_fourth_downs


# can show % of 4th downs in which they follow the advice (should have and did, shouldn't have and didn't)
#     - only shows if this is how how NFL coaches think

meaningful_fourth_downs$followed_model <- rep(NA,30189)


meaningful_fourth_downs[meaningful_fourth_downs$went_for_it,]$followed_model <- meaningful_fourth_downs[meaningful_fourth_downs$went_for_it,]$should_go_for_it1
meaningful_fourth_downs[meaningful_fourth_downs$play_type == "field_goal",]$followed_model <- meaningful_fourth_downs[meaningful_fourth_downs$play_type == "field_goal",]$should_kick
meaningful_fourth_downs[meaningful_fourth_downs$play_type == "punt",]$followed_model <- meaningful_fourth_downs[meaningful_fourth_downs$play_type == "punt",]$should_punt

#############

# %  MODEL FOLLOWED

meaningful_fourth_downs %>% pull(followed_model) %>% table

with(meaningful_fourth_downs, barplot(table(followed_model)/nrow(meaningful_fourth_downs),
                                      ylim = c(0, 0.7),
                                      col = "gray",
                                      main = "Does the Model Agree With the Coach's 4th Down Decision?",
                                      xlab = "Model Followed",
                                      ylab = "Proportion"),
     )

with(meaningful_fourth_downs, table(followed_model)/nrow(meaningful_fourth_downs))


#############




meaningful_fourth_downs %>% pull(play_type) %>% table

meaningful_fourth_downs %>% filter(!followed_model) %>% with(wpa <= 0) %>% table() # % of plays not following the model that have negative wpa and lower wpa than model expected


meaningful_fourth_downs %>% filter(!followed_model) %>% with(wpa >= 0) %>% table() # % of plays following the model that have positive wpa

(meaningful_fourth_downs %>% with(wpa >= 0) %>% table()) / nrow(meaningful_fourth_downs)


#############

# EFFECTIVENESS OF MODEL

(meaningful_fourth_downs %>% with(wpa >= 0) %>% table()) / nrow(meaningful_fourth_downs) # Coach's decisions are good 54.3% of the time

(10766 + 7427) / nrow(meaningful_fourth_downs) # Coach's decisions are good 62.8% of the time when following the model

(10766 / 17137) # Coach's decisions are good 62.8% of the time when following the model

5625 / 13052 # Coach's decisions are good 43.1% of the time when not following the model

#############



meaningful_fourth_downs %>% filter(play_type != "qb_kneel") %>% pull(followed_model) %>% table


# COACHES FOLLOW NO YDSTOGO MODEL MORE THAN YDSTOGO MODEL - YDSTOGO RECCOMENDS TO GO FOR IT A LOT LESS

meaningful_fourth_downs[meaningful_fourth_downs$went_for_it,]$should_go_for_it1 %>% table()


meaningful_fourth_downs[meaningful_fourth_downs$went_for_it,] %>% identical(meaningful_fourth_downs[meaningful_fourth_downs$play_type %in% c("pass", "run") & meaningful_fourth_downs$special_teams_play != 1,])

meaningful_fourth_downs$should_go_for_it1 %>% table()




#############

# TABLES AND GRAPHS OF PLAY TYPE DISTRIBUTION, REAL AND RECOMMENDED

rec_breakdown <- data.frame("Field Goal" = (meaningful_fourth_downs$should_kick %>% table())[2] / nrow(meaningful_fourth_downs),
                            "Conv. Attempt" = (meaningful_fourth_downs$should_go_for_it1 %>% table())[2] / nrow(meaningful_fourth_downs),
                            "Punt" = (meaningful_fourth_downs$should_punt %>% table())[2] / nrow(meaningful_fourth_downs))

real_breakdown <- data.frame("Field Goal" = (meaningful_fourth_downs %>% pull(play_type) %>% table)[1] / nrow(meaningful_fourth_downs),
                             "Conv. Attempt" = ((meaningful_fourth_downs %>% pull(play_type) %>% table)[2] + (meaningful_fourth_downs %>% pull(play_type) %>% table)[4]) / nrow(meaningful_fourth_downs),
                             "Punt" = (meaningful_fourth_downs %>% pull(play_type) %>% table)[3] / nrow(meaningful_fourth_downs))

is.matrix(table(meaningful_fourth_downs$qtr, meaningful_fourth_downs$should_go_for_it1))


barplot(matrix(unlist(rbind(real_breakdown[1, , drop = TRUE], rec_breakdown[1, , drop = TRUE])), nrow = 2, ncol = 3), 
        ylim = c(0, 1),
        beside = TRUE,
        main = "4th Down Play Type Breakdown \n(2012-2022)",
        names = c("Field Goal", "Conv. Attempt", "Punt"),
        col = c("gray","dodgerblue"),
        ylab = "Proportion",
        xlab = "Play Type")
legend("topleft", 
       legend = c("Real", "Recommended"), 
       col = c("gray","dodgerblue"),
       fill = c("gray","dodgerblue"))

barplot(unlist(rec_breakdown[1, , drop = TRUE]), 
        ylim = c(0, 0.7), 
        main = "Model Recommendation \n(2012-2022)", 
        names = c("Field Goal", "Conv. Attempt", "Punt"),
        col = "dodgerblue",
        ylab = "Proportion",
        xlab = "Play Type")


#############



#############

# eWPA vs. eFGWPA VIZ

plot(expected_wpa ~ abs(score_differential), data = meaningful_fourth_downs,
     ylab = "eWPA",
     xlab = "Score Differential",
     main = "Expected Win Probability Added vs. Score Differential",
     col = "gray") # low score idfferential prefers field goals
points(fg_expected_wpa ~ abs(score_differential), data = meaningful_fourth_downs,
     col = rgb(blue = 0.7, red = 0.4, green = 0.2, alpha = 0.2))
legend("bottomright", 
       pch = 16, 
       col = c(rgb(blue = 0.7, red = 0.4, green = 0.2), "gray"),
       legend = c("eFGWPA", "eAWPA"),
       cex = 1.5)


# fg wpa expected is more moderate at lower score differentials and both get more moderate until 14, but around 14, fg wpa expected begins to become more extreme than going for it
# the lowest expected wpa for each sharply increases

plot(expected_wpa ~ yardline_100, data = meaningful_fourth_downs %>% filter(abs(score_differential) <= 2)) # low score idfferential prefers field goals
points(fg_expected_wpa ~ yardline_100, data = meaningful_fourth_downs %>% filter(abs(score_differential) <= 16, game_seconds_remaining <= 120),
       col = rgb(blue = 1, red = 0, green = 0, alpha = 0.2)) # when it makes it a 3 possession game


plot(fgm_prob ~ yardline_100, data = meaningful_fourth_downs) 

plot(expected_wpa ~ game_seconds_remaining, data = meaningful_fourth_downs) # low score differential prefers field goals
points(fg_expected_wpa ~ game_seconds_remaining, data = meaningful_fourth_downs,
       col = rgb(blue = 1, red = 0, green = 0, alpha = 0.2)) 

#############







# can really only evaluate the cases of when they do go for it (evaluates if the difference in wpa predictions are valid)
#     - if they should have but didn't get it, is abs(wpa_if_conv) > abs(wpa)? (correct if TRUE)
#     - if they should have and got it, is abs(wpa) > abs(wpa_if_fail)? (correct if TRUE)
#     - if they should not have but got it, is abs(wpa) < abs(wpa_if_fail)? (correct if TRUE)
#     - if they should not have and didn't get it, is abs(wpa) > abs(wpa_if_conv)? (correct if TRUE)


# subset by the above cases and input T or F
# evaluate wpa estimation based on accuracy % of results (whether they actually should have gone for it assuming wpa is motivating factor)
meaningful_attempts1 <- meaningful_fourth_downs %>%
  filter(play_type %in% c("pass", "run"), special_teams_play != 1, year %in% (2012:2022))


meaningful_attempts1$wpa_risk_accuracy <- TRUE
meaningful_attempts1$wrong_var <- ""
meaningful_attempts1$wpa_error <- numeric(1)




should_but_fail <- meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1 & meaningful_attempts1$should_go_for_it,]
meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1 & meaningful_attempts1$should_go_for_it,] <- should_but_fail %>%
  mutate(wpa_risk_accuracy = abs(wpa_if_conv) >= abs(wpa), wrong_var = "wpa_if_fail too low")

should_n_conv <- meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1 & meaningful_attempts1$should_go_for_it,]
meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1 & meaningful_attempts1$should_go_for_it,] <- should_n_conv %>%
  mutate(wpa_risk_accuracy = abs(wpa_if_fail) <= abs(wpa), wrong_var = "wpa_if_conv too high")

shouldnt_but_conv <- meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1 & !meaningful_attempts1$should_go_for_it,]
meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1 & !meaningful_attempts1$should_go_for_it,] <- shouldnt_but_conv %>%
  mutate(wpa_risk_accuracy = abs(wpa_if_fail) >= abs(wpa), wrong_var = "wpa_if_conv too low")

shouldnt_n_fail <- meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1 & !meaningful_attempts1$should_go_for_it,]
meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1 & !meaningful_attempts1$should_go_for_it,] <- shouldnt_n_fail %>%
  mutate(wpa_risk_accuracy = abs(wpa_if_conv) <= abs(wpa), wrong_var = "wpa_if_fail too high")






failed <- meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1,]
meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1,] <- failed %>%
  mutate(wpa_error = abs(wpa_if_fail) - abs(wpa))

conv <- meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1,]
meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1,] <- conv %>%
  mutate(wpa_error = abs(wpa_if_conv) - abs(wpa))

with(meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1,], mean(wpa_error))
with(meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1,], mean(wpa_error))

# error for failure is higher than error for conversion for NOYDS model

plot(wpa_error ~ abs(wpa), data = meaningful_attempts1[meaningful_attempts1$fourth_down_converted == 1,])
abline(0, 0)
plot(wpa_error ~ abs(wpa), data = meaningful_attempts1[meaningful_attempts1$fourth_down_failed == 1,])
abline(0, 0)

# more cases where abs(wpa_if_conv) is overestimated, but less off from real wpa than when wpa_if_conv is underestimated
# more cases where abs(wpa_if_fail) is underestimated, and these cases more extremely miscalculate wpa

# overall, the model seems to bias towards going for it on 4th down

should_go_table <- meaningful_fourth_downs$should_go_for_it1 %>% table()

should_go_table[2]/sum(should_go_table)

# Should go for it breakdown:
# NOYDS model: 37.2%
# YDS model: 72.6%

nrow(meaningful_attempts1[meaningful_attempts1$conversion_prob >= 0.5,])/nrow(meaningful_attempts1)

# both models indicate that coaches should start attempting more 4th downs

meaningful_attempts1[meaningful_attempts1$wpa_risk_accuracy,]$wrong_var <- NA

wpa_accuracy <- meaningful_attempts1 %>% pull(wpa_risk_accuracy) %>% table()

meaningful_attempts1 %>% pull(wrong_var) %>% table()

## This table shows that the model's errors in evaluating wpa occurs most often when it says a team should go for it, regardless of result (most often when they shpuld go for it and they get it)
## in other words it overestimates wpa if converted or underestimates wpa if fail 

nrow(meaningful_attempts1)

1336/nrow(meaningful_attempts1)

# YDSTOGO model better approximates wpa, but no YDS model more closely follows how NFL coaches currently make decisions and is more conservative 
# Show how YDSTOGO model in practical situations may be extreme, can get a coach fired in this era - worth it to trail blaze this much?








#############

# New Attempts Plot

rbind(attempt_perc_by_year, c(2023, should_go_table[2]/sum(should_go_table)))

plot(Attempt.Rate ~ Year, data = attempt_perc_by_year, 
     pch = 16,
     xlim = c(2012, 2023),
     ylim = c(0, 0.4),
     ylab = "4th Down Attempt Rate",
     main = "NFL 4th Down Attempt Rate by Year\n(Meaningful Attempts)")
points(x = 2023, y = should_go_table[2]/sum(should_go_table), col = "dodgerblue",
       pch = 16)

#############

#############

# Team Analysis

team_plot <- meaningful_fourth_downs %>%
  filter(play_type %in% c("pass", "run"), special_teams_play != 1) %>%
  group_by(posteam, year) %>%
  summarize(pct_success = mean(fourth_down_converted)) %>%
  arrange(posteam)

col_add <- meaningful_fourth_downs %>%
  #filter(year == 2022) %>%
  group_by(posteam, year) %>%
  summarize(pct_followed = mean(followed_model), try_pct = mean(went_for_it), no_tries = sum(went_for_it), no_opps = n()) %>%
  arrange(posteam)


team_plot <- full_join(team_plot, col_add, by = c("posteam", "year"))


with(team_plot, tapply(pct_followed, year, mean))




#############

# Model followed pct by year

plot(y = with(team_plot, tapply(pct_followed, year, mean)), x = 2012:2022,
     ylim = c(0, 0.7),
     main = "League-Wide % of 4th Down Plays Following Model",
     ylab = "% Followed",
     xlab = "Year",
     sub = "R = 0.93")

by_year_analysis <- cbind("pct_followed" = with(team_plot, tapply(pct_followed, year, mean)), "year" = 2012:2022)

team_regression <- lm(pct_followed ~ year, data = data.frame(by_year_analysis))

abline(team_regression, col = "dodgerblue")


### R:

cor(y = with(team_plot, tapply(pct_followed, year, mean)), x = 2012:2022)

summary(team_regression)$coefficients

# increases by an estimated 0.75% per year

############

abline(h = succ_perc_by_year[11,2], v = attempt_perc_by_year[11,2])


############

# 2022 % followed by team


barplot(pct_followed ~ reorder(posteam, -pct_followed), data = team_plot %>%
          filter(year == 2022) %>%
          arrange(desc(pct_followed)),
        col = rgb(red = 0, green = 0, blue = 1, alpha = seq(1, 0, length = 32)),
        cex.names = 0.8,
        las = 2
        )

last_year_followed <- team_plot %>% filter(year == 2022) %>% arrange(desc(pct_followed))

last_year_followed$alpha <- seq(1, 0, length = 32)

last_year_followed <- last_year_followed %>% arrange(desc(pct_success))

barplot(pct_success ~ reorder(posteam, -pct_success), data = last_year_followed,
        col = rgb(red = 0, green = 0, blue = 1, alpha = last_year_followed$alpha),
        cex.names = 0.8,
        las = 2
)

blues9

############




############

# % followed and % success plot

team_2022 <- team_plot %>% 
  filter(year == 2022) 

team_2022$W <- c(0.235,
               .412,
               .588,
               .813,
               .412,
               .176,
               .750,
               .412,
               .706,
               .294,
               .529,
               .471,
               .206,
               .265,
               .529,
               .824,
               .294,
               .588,
               .353,
               .529,
               .765,
               .471,
               .412,
               .559,
               .412,
               .824,
               .529,
               .529,
               .765,
               .471,
               .412,
               .500)



plot(pct_success ~ pct_followed, data = team_2022,
     pch = 16,
     ylim = c(0, 1),
     xlim = c(0, 1))
abline(h = mean(team_2022$pct_success), v = mean(team_2022$pct_followed))


############

plot(pct_success ~ pct_followed, data = team_2022,
     pch = 16)
abline(h = mean(team_2022$pct_success), v = mean(team_2022$pct_followed))

plot(try_pct ~ pct_followed, data = team_2022,
     pch = 16)
abline(h = mean(team_2022$try_pct), v = mean(team_2022$pct_followed))

plot(W ~ pct_followed, data = team_2022,
     pch = 16,
     main = "Team Win % vs. Model Followed %\n(2022)",
     ylab = "Win %",
     xlab = "Model Followed %")
abline(h = mean(team_2022$W), v = mean(team_2022$pct_followed))


####### Correlations

summary(lm(pct_success ~ pct_followed, data = team_2022))
summary(lm(try_pct ~ pct_followed, data = team_2022))
summary(lm(W ~ pct_followed, data = team_2022))

sqrt(0.09955)

#######

#### Model Weaknesses
####    - wpa model is not very successful in predicting which outcome will result in a higher change in wpa 
####        - (it skews toward overestimating wpa_conv and underestimating wpa_fail)
####    - doesn't consider pass/rush tendencies and strengths in certain teams
####    - doesn't consider coaching style - some game strategies lend itself to more conservative, some to more aggressive approach (ex. strength of and reliance on defense to win game)


#### Further Direction
####    - analyze what scenarios the model under/over estimates wpa for certain play types, edit model accordingly


meaningful_fourth_downs %>% select(yardline_100, ydstogo, game_seconds_remaining)

predict(wpa_conv_model_noyds, list(yardline_100 = 8, ydstogo = 4, game_seconds_remaining = 1900, score_differential = 10))
predict(wpa_fail_model_noyds, list(yardline_100 = 8, ydstogo = 4, game_seconds_remaining = 1900, score_differential = 10))

# models needed to run the function: conversion prob logstic model, fgm logistic model, wpa_conv, wpa_fail, wpa_fgm, wpa_fg_miss

fourth_down_recommend_function <- function(yds, ydline, seconds_left, score_differential) {

eAWPA <- (predict(regression, list(ydstogo = yds), type = "response") * predict(wpa_conv_model_noyds, list(yardline_100 = ydline, ydstogo = yds, game_seconds_remaining = seconds_left, score_differential = score_differential))) + ((1 - predict(regression, list(ydstogo = yds), type = "response")) * predict(wpa_fail_model_noyds, list(yardline_100 = ydline, ydstogo = yds, game_seconds_remaining = seconds_left, score_differential = score_differential))
)
eFGWPA <- (predict(regression, list(ydstogo = 4), type = "response") * predict(wpa_fgm_model, list(yardline_100 = ydline, ydstogo = yds, game_seconds_remaining = seconds_left, score_differential = score_differential))) + ((1 - predict(regression, list(ydstogo = yds), type = "response")) * predict(wpa_fg_miss_model, list(yardline_100 = ydline, ydstogo = yds, game_seconds_remaining = seconds_left, score_differential = score_differential))
)

if ((eAWPA < 0) & (eFGWPA < 0) & (ydline > 35)) {
  "punt"
} else if (eAWPA > eFGWPA) {
  "go for it"
} else if (eFGWPA > eAWPA) {
  "field goal"
}

c(eAWPA, eFGWPA)

}


# vectorize it

vapply(1:15, fourth_down_recomemnd_function, numeric(2), ydline = 20, seconds_left = 420, score_differential = 8)

vapply(1:15, fourth_down_recommend_function, character(1), ydline = 20, seconds_left = 420, score_differential = 8)

# Plan for the future: Create interactive web app to input scenario (i.e. filed position, yards to go, time remaining) and obtain a predicted WPA value



