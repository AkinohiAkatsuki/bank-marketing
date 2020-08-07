## Basic Step Statistics Project Based Learning
## Bank Marketing

library(precrec)
library(ggplot2)

# 出力したCSVデータを読み込めます
bank_marketing_train <- read.csv("data/bank_marketing_train.csv")
head(bank_marketing_train)

# データカラムを確認
colnames(bank_marketing_train)

#データの要約統計量を確認
summary(bank_marketing_train)

full_model = glm(y~.-duration,
                 data = bank_marketing_train,
                 family = "binomial")

summary(full_model)
exp(full_model$coefficients)
unique(bank_marketing_train$job) # job"admin."を基準にjobの倍率を表示していることがわかる

# =====================================
# ここからペルソナ分析

hist(bank_marketing_train$age)
esquisse::esquisser(bank_marketing_train)
ggplot(bank_marketing_train) +
  aes(x = age, fill = y) +
  geom_histogram(bins = 17L) +
  scale_fill_hue() +
  labs(fill = "marketing result") +
  theme_gray()

# 成約率の高い年齢が若年層と高齢者層に偏っていることが確認できた
# ageを年代別にカテゴリカル変数にする
# と同時にモデル選択に備えて変数を除外しやすいようにダミー変数化して列を分けておく
install.packages("devtools")
library(makedummies)
bank_marketing_persona <- bank_marketing_train

bank_marketing_persona <- transform(bank_marketing_train,
                                    "age_group"=bank_marketing_persona$age %/% 10)
bank_marketing_persona <- makedummies(bank_marketing_persona)
head(bank_marketing_persona)

library(dplyr)

df_persona <- select(bank_marketing_persona, -housing_unknown,-age,-duration,-campaign,-pdays,
                     -emp.var.rate,-cons.price.idx,-cons.conf.idx,-euribor3m,-nr.employed)
df_persona$age_group <- as.factor(df_persona$age_group)
age_group <- makedummies(df_persona, col="age_group", basal_level=TRUE)
df_persona <- cbind(df_persona, age_group)
df_persona <- select(df_persona, -age_group)
head(df_persona)

# ペルソナ分析のロジスティック回帰モデル
# age_groupの多重共線性を避けるために最も成約率の低い40代を除外（40代をオッズ比の基準にする）
persona_model <- glm(y~.-age_group_4 , data = df_persona, family = "binomial")

# モデルの評価
summary(persona_model) #z検定結果有意でないものが多い

# 多重共線性の確認
library(car)
car::vif(persona_model) #いずれも5未満なので問題なさそう

# モデルの改善
# まず有意でない変数のうち "." が付いていないものをすべて取り除きモデルを更新する
persona_model2 <- update(persona_model,
                         ~ .-job_entrepreneur-job_housemaid-job_management-job_retired-`job_self-employed`
                         -job_unemployed-job_unknown-marital_married-marital_unknown-education_basic.6y
                         -education_basic.9y-education_high.school-education_professional.course
                         -education_university.degree-default_yes-housing_yes-loan_unknown
                         -day_of_week_thu-day_of_week_tue-day_of_week_wed-age_group_3-age_group_9)
summary(persona_model2)

# 更新したモデルでも有意でない変数のうち "." が付いていないものを取り除く
persona_model3 <- update(persona_model2, ~ .-education_illiterate)
summary(persona_model3)

# 再度更新したモデルでも有意でない変数を取り除く
persona_model4 <- update(persona_model3, ~ .-loan_yes)
summary(persona_model4)

# 多重共線性を再チェック
car::vif(persona_model4) #いずれも5未満なので問題なさそう

# ステップワイズで変数を調整する
p_model = step(persona_model4)
summary(p_model)

# ペルソナ分析用のオッズ比可視化
par(las=1,mar=c(3, 7, 2, 1)) # グラフのサイズ、軸の設定
barplot(sort(exp(p_model$coefficients)),
        horiz=T, main = "Odds Ratio")
abline(v=1)
sort(exp(p_model$coefficients)) # 数値の一覧でも見る


# 以前のキャンペーンで成約したグループについての分析
# df_persona から poutcome_success==1 の行だけのデータフレームを作る
p_success <- df_persona[df_persona$poutcome_success==1, ]

# 以下、全体のペルソナ分析のロジスティック回帰モデルと同じ手順を踏む
# age_groupの多重共線性を避けるために最も成約率の低い40代を除外（40代をオッズ比の基準にする）
# poutcome_success==1 が前提なので他の poutcome_ も除外
p_success_model <- glm(y~.-age_group_4-poutcome_success-poutcome_nonexistent , data = p_success, family = "binomial")

# モデルの評価
summary(p_success_model) #z検定結果有意でないものが多い

# 多重共線性の確認
car::vif(persona_model) #いずれも5未満なので問題なさそう

# モデルの改善
# アスタリスクまたは"." が付いてるものだけにモデルを更新する
p_success_model2 <- glm(y~job_unknown+education_basic.9y+education_high.school+default_unknown+day_of_week_mon+previous,
                        data = p_success, family = "binomial")

summary(p_success_model2)

# 更新したモデルで有意でない変数を取り除く
p_success_model3 <- update(p_success_model2, ~ .-education_high.school)
summary(p_success_model3)

# 多重共線性を再チェック
car::vif(p_success_model3) #いずれも1.0程度なので問題ない

# ステップワイズで変数を調整する
p_success_model4 = step(p_success_model3)
summary(p_success_model4) #説明変数の増減なし

# ペルソナ分析用のオッズ比可視化
par(las=1,mar=c(3, 7, 2, 1)) # グラフのサイズ、軸の設定
barplot(sort(exp(p_success_model4$coefficients)),
        horiz=T, main = "Odds Ratio")
abline(v=1)
sort(exp(p_success_model4$coefficients)) # 数値の一覧でも見る





# 60〜80歳のグループについての分析
# df_persona から age_group が 6〜8 の行だけのデータフレームを作る
age_60_80 <- df_persona[df_persona$age_group_6==1 | df_persona$age_group_7==1 | df_persona$age_group_8==1,]

# 以下、全体のペルソナ分析のロジスティック回帰モデルと同じ手順を踏む
# age_group が 6〜8 が前提なので除外
age_60_80_model <- glm(y~.-age_group_6-age_group_7-age_group_8 , data = age_60_80, family = "binomial")

# モデルの評価
summary(age_60_80_model) #z検定結果有意でないものが多い

# モデルの改善
# アスタリスクまたは"." が付いてるものだけにモデルを更新する
age_60_80_model2 <- glm(y~`job_blue-collar`+job_unknown+education_basic.6y+education_basic.9y+education_university.degree
                        +education_unknown+default_unknown+housing_yes+contact+day_of_week_tue+previous
                        +poutcome_nonexistent+poutcome_success,
                        data = age_60_80, family = "binomial")

summary(age_60_80_model2)

# 更新したモデルでアスタリスクもドットもない変数を取り除く
age_60_80_model3 <- update(age_60_80_model2, ~ .-education_basic.6y)
summary(age_60_80_model3)

# 更新したモデルでドット付きの変数を取り除く
age_60_80_model4 <- update(age_60_80_model3, ~ .-housing_yes-poutcome_nonexistent)
summary(age_60_80_model4)

# 更新したモデルでドットもない変数を取り除く
age_60_80_model5 <- update(age_60_80_model4, ~ .-previous)
summary(age_60_80_model5)

# 更新したモデルでドット付きの変数を取り除く
age_60_80_model6 <- update(age_60_80_model5, ~ .-education_unknown)
summary(age_60_80_model6)

# 多重共線性をチェック
car::vif(age_60_80_model6) #いずれも1.0程度なので問題ない

# ステップワイズで変数を調整する
aged_model = step(age_60_80_model6)
summary(aged_model) #説明変数の増減なし

# ペルソナ分析用のオッズ比可視化
par(las=1,mar=c(3, 7, 2, 1)) # グラフのサイズ、軸の設定
barplot(sort(exp(aged_model$coefficients)),
        horiz=T, main = "Odds Ratio")
abline(v=1)
sort(exp(aged_model$coefficients)) # 数値の一覧でも見る



# 10代のグループについての分析
# df_persona から age_group が 1 の行だけのデータフレームを作る
age_10 <- df_persona[df_persona$age_group_1==1, ]

# 以下、全体のペルソナ分析のロジスティック回帰モデルと同じ手順を踏む
# age_group が 6〜8 が前提なので除外
age_10_model <- glm(y~.-age_group_1 , data = age_10, family = "binomial")

# モデルの評価
summary(age_10_model) #z検定結果有意でないものが多い

# モデルの改善
# アスタリスクもドットもない変数を取り除く
age_10_model2 <- glm(y~day_of_week_thu,
                     data = age_10, family = "binomial")
summary(age_10_model2)

# 有意な変数なし
# age_10_model2 を p < 0.5 まで許容して残すモデルにして少しずつ減らしていっても同じ結論




# 学生のグループについての分析
# df_persona から job_student が 1 の行だけのデータフレームを作る
job_student <- df_persona[df_persona$job_student ==1, ]

# 以下、全体のペルソナ分析のロジスティック回帰モデルと同じ手順を踏む
# job_student が前提なので除外
job_student_model <- glm(y~.-job_student, data = job_student, family = "binomial")

# モデルの評価
summary(job_student_model) #z検定結果有意でないものが多い

# モデルの改善
# アスタリスクかドットのあるのものだけにモデルを更新する
job_student_model2 <- glm(y~default_unknown+loan_unknown+loan_yes+contact+day_of_week_mon+poutcome_success,
                          data = job_student, family = "binomial")
summary(job_student_model2)

# 更新したモデルでアスタリスクもドットもない変数を取り除く
job_student_model3 <- update(job_student_model2, ~ .-day_of_week_mon)
summary(job_student_model3)

# 更新したモデルでドット付きの変数を取り除く
job_student_model4 <- update(job_student_model3, ~ .-loan_unknown-loan_yes)
summary(job_student_model4)

# 多重共線性をチェック
car::vif(job_student_model4) #いずれも1.0程度なので問題ない

# ステップワイズで変数を調整する
student_model = step(job_student_model4)
summary(student_model) #説明変数の増減なし

# ペルソナ分析用のオッズ比可視化
par(las=1,mar=c(3, 7, 2, 1)) # グラフのサイズ、軸の設定
barplot(sort(exp(student_model$coefficients)),
        horiz=T, main = "Odds Ratio")
abline(v=1)
sort(exp(student_model$coefficients)) # 数値の一覧でも見る


