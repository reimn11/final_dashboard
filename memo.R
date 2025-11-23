### Relationship Physical Activity and Sleep duration and quality ###
{r}

sleep_pa_long <- sleep %>% 
  select(`Physical Activity Level`, `Sleep Duration`, `Quality of Sleep`) |>
  pivot_longer(
    cols = c(`Sleep Duration`, `Quality of Sleep`),
    names_to = "sleep_measure",
    values_to = "value"
  )

sleep_pa_long %>% 
  ggplot(aes(x = `Physical Activity Level`, y = value)) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  facet_wrap(~ sleep_measure, scales = "free_y") +
  labs(
    title = "Relationship between Pysical Activity level and sleep duration/quality",
    x = "Physical Activity Level",
    y = "Value"
  ) +
  theme_minimal()


##### Heat map (not blood pressure) #####

{r}
cor_long <- cor_mat |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlation"
  )

cor_long |>
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0
  ) +
  coord_fixed() +
  labs(
    title = "Correlation between numeric variables in sleep dataset",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### Heat map (with blood pressure) ###
{r}
library(tidyverse)
library(tidyr)

# 1. Split Blood Pressure into numeric columns
sleep_bp <- sleep |>
  separate(
    `Blood Pressure`,
    into = c("Systolic", "Diastolic"),
    sep = "/",
    convert = TRUE   # makes them numeric
  )

# 2. Choose numeric variables for correlation (including BP)
num_sleep <- sleep_bp |>
  select(
    `Sleep Duration`,
    `Quality of Sleep`,
    `Physical Activity Level`,
    `Stress Level`,
    `Heart Rate`,
    Systolic,
    Diastolic
    # you can add Age, BMI, etc. here if you want
  )



# 5. Heatmap including Systolic & Diastolic
cor_long |>
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0
  ) +
  coord_fixed() +
  labs(
    title = "Correlation between numeric variables in sleep dataset",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



{r}
### Stacked bar charts: sleep duration & quality by sleep disorder ###

sleep_long_disorder <- sleep |>
  mutate(
    Duration_cat = case_when(
      `Sleep Duration` < 7 ~ "< 7 hours(1-6)",
      TRUE ~ "7+ hours"
    ),
    Quality_cat = case_when(
      `Quality of Sleep` <= 6 ~ "Low (1–6)",
      TRUE ~ "High (7–10)"
    )
  ) |>
  select(`BMI Category`, Duration_cat, Quality_cat) |>
  pivot_longer(
    cols = c(Duration_cat, Quality_cat),
    names_to = "measure",
    values_to = "category"
  ) |>
  mutate(
    measure = recode(
      measure,
      "Duration_cat" = "Sleep Duration",
      "Quality_cat" = "Sleep Quality"
    )
  )

{r}
sleep_long_disorder |>
  ggplot(aes(x = `BMI Category`, fill = category)) +
  geom_bar(position = "fill") +  # stacked to 100%
  facet_wrap(~ measure) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Sleep duration and quality distribution by BMI Category",
    x = "Sleep disorder",
    y = "Percentage within group",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



##### Relationship between blood pressure and sleep duration and quality ###
{r}
library(tidyr)  # if not already loaded via tidyverse

sleep_bp <- sleep |>
  separate(
    `Blood Pressure`,
    into = c("Systolic", "Diastolic"),
    sep = "/",
    convert = TRUE   # turns them into numeric
  )


{r}
sleep_bp_long <- sleep_bp |>
  pivot_longer(
    cols = c(Systolic, Diastolic),
    names_to = "BP_type",
    values_to = "BP_value"
  )

sleep_bp_long |>
  ggplot(aes(x = BP_value, y = `Sleep Duration`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ BP_type, scales = "free_x") +
  labs(
    title = "Sleep duration vs blood pressure",
    x = "Blood pressure (mmHg)",
    y = "Sleep duration (hours)"
  ) +
  theme_minimal()



### Physical activity good for BMI? ###
{r}
sleep_pa_bmi <- sleep |>
  mutate(
    PA_cat = case_when(
      `Physical Activity Level` < 30 ~ "Low (<30)",
      `Physical Activity Level` < 60 ~ "Medium (30–59)",
      TRUE ~ "High (70+)"
    )
  ) |>
  filter(!is.na(PA_cat), !is.na(`BMI Category`))


{r}
sleep_pa_bmi |>
  ggplot(aes(x = `BMI Category`,
             y = `Physical Activity Level`)) +
  geom_vi() +
  labs(
    title = "Physical activity level by BMI category",
    x = "BMI Category",
    y = "Physical Activity Level (minutes / score)"
  ) +
  theme_minimal()


### Stress, BMI ###

{r}
sleep_bp |>
  ggplot(aes(x = `Physical Activity Level`,
             y = `Sleep Duration`,
             color = `BMI Category`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(
    title = "Sleep Duration vs. Stress Level by BMI category",
    x = "Pysical Activity",
    y = "SLEEP DURATION",
    color = "BMI Category"
  ) +
  theme_minimal()



### Sleep Disorder, Stress ###

{r}
sleep |>
  ggplot(aes(x = `Sleep Disorder`,
             y = `Stress Level`,
             fill = `Sleep Disorder`)) +
  geom_violin(alpha = 0.7) +
  labs(
    title = "Stress level across BMI categories",
    x = "Sleep Disorder",
    y = "Stress Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )


### Sleep Disorder, Physical Activity ###

{r}
sleep |>
  ggplot(aes(x = `Sleep Disorder`,
             y = `Physical Activity Level`,
             fill = `Sleep Disorder`)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(
    title = "Physical activity distribution by sleep disorder",
    x = "Sleep Disorder",
    y = "Physical Activity Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


### Physical Activity, blood pressure ###
{r}
sleep_bp |>
  ggplot(aes(x = `Physical Activity Level`,
             y = `Systolic`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(
    title = "Sleep Duration vs. Stress Level by BMI category",
    x = "Physical Activity",
    y = "Systolic ",
    color = "BMI Category"
  ) +
  theme_minimal()



### Pysical Activity, Streess by Sleep disorder### 

{r}
sleep_bp |>
  ggplot(aes(x = `Physical Activity Level`,
             y = `Stress Level`,
             color = `Sleep Disorder`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(
    title = "Physical Activity Lelevel vs. Stress Level by Sleep Disorder",
    x = "Physical Activity Lelevel",
    y = "Stress Level",
    color = "Sleep Disorder"
  ) +
  theme_minimal()
