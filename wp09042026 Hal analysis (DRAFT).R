# ====================================================
# END‑TO‑END SCRIPT
# Hallucination cases – penalties + AI hallucination attribution
# Cut‑off: end of March 2026
# ====================================================

# ---- Housekeeping ----
rm(list = ls())

# ---- Step 1: Load libraries ----
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

# ---- Step 2: Load data (authoritative source) ----
url <- "https://www.damiencharlotin.com/hallucinations/hallucinations/download.csv"
df  <- read_csv(url, show_col_types = FALSE)

# ---- Step 3: Robust date parsing ----
df <- df %>%
  mutate(
    Date = parse_date_time(
      Date,
      orders = c(
        "ymd", "dmy", "mdy",
        "ymd HMS", "dmy HMS", "mdy HMS"
      )
    ),
    Month = floor_date(Date, "month")
  ) %>%
  filter(!is.na(Month))

# ---- Step 3b: Hard cutoff at end of March 2026 ----
df <- df %>%
  filter(Month <= as.Date("2026-03-31"))

# ---- Step 4: Multi‑penalty classification ----
df_penalty <- df %>%
  mutate(
    penalty_warning = ifelse(
      str_detect(
        tolower(Outcome),
        "warning|admonish|caution|show cause(?!.*monetary)"
      ),
      1, 0
    ),
    penalty_financial = ifelse(
      !is.na(`Monetary Penalty`) & str_detect(`Monetary Penalty`, "\\d"),
      1, 0
    ),
    penalty_procedural = ifelse(
      str_detect(
        tolower(Outcome),
        "struck|dismiss|default judgment|filing restriction|case dismissed"
      ),
      1, 0
    ),
    penalty_professional = ifelse(
      str_detect(
        tolower(Outcome),
        "bar referral|professional sanction|revoked|disqualif|pro hac vice"
      ),
      1, 0
    )
  )

# ---- Step 5: Monthly total cases ----
monthly_cases <- df_penalty %>%
  group_by(Month) %>%
  summarise(TotalCases = n(), .groups = "drop")

# ---- Step 6: Long format penalties ----
penalty_monthly_long <- df_penalty %>%
  select(
    Month,
    penalty_warning,
    penalty_financial,
    penalty_procedural,
    penalty_professional
  ) %>%
  pivot_longer(
    cols = starts_with("penalty_"),
    names_to = "PenaltyType",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  group_by(Month, PenaltyType) %>%
  summarise(Count = n(), .groups = "drop")

# ---- Clean labels ----
penalty_monthly_long$PenaltyType <- recode(
  penalty_monthly_long$PenaltyType,
  penalty_warning      = "Warning",
  penalty_financial    = "Financial Penalty",
  penalty_procedural   = "Procedural Penalty",
  penalty_professional = "Professional Penalty"
)

# ---- Step 7: Monthly penalty composition (100%) ----
penalty_monthly_share <- penalty_monthly_long %>%
  group_by(Month) %>%
  mutate(
    TotalPenalties = sum(Count),
    Share = Count / TotalPenalties
  ) %>%
  ungroup()

# ---- Step 8: Monthly composition of AI hallucination cases
#              Top 3 parties + Other (100%) ----

# Identify AI‑related hallucination cases
ai_cases <- df %>%
  mutate(
    AI_used = ifelse(!is.na(`AI Tool`) & `AI Tool` != "", 1, 0)
  ) %>%
  filter(AI_used == 1)

# Determine TOP 3 parties globally (entire sample)
top3_parties <- ai_cases %>%
  count(`Party(ies)`, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(`Party(ies)`)

# Aggregate monthly counts with "Other"
ai_party_monthly_long <- ai_cases %>%
  mutate(
    Party_grouped = ifelse(
      `Party(ies)` %in% top3_parties,
      `Party(ies)`,
      "Other"
    )
  ) %>%
  group_by(Month, Party_grouped) %>%
  summarise(Count = n(), .groups = "drop")

# Compute monthly shares (100%)
ai_party_monthly_share <- ai_party_monthly_long %>%
  group_by(Month) %>%
  mutate(
    Total_AI_cases = sum(Count),
    Share = Count / Total_AI_cases
  ) %>%
  ungroup()

# ====================================================
# PLOT 1 — Monthly stacked penalty counts + total cases
# ====================================================
p1 <- ggplot() +
  geom_col(
    data = penalty_monthly_long,
    aes(x = Month, y = Count, fill = PenaltyType),
    alpha = 0.85
  ) +
  geom_line(
    data = monthly_cases,
    aes(x = Month, y = TotalCases, color = "Total Cases"),
    linewidth = 1.2
  ) +
  geom_point(
    data = monthly_cases,
    aes(x = Month, y = TotalCases, color = "Total Cases"),
    size = 2.2
  ) +
  scale_color_manual(values = c("Total Cases" = "black")) +
  labs(
    title = "Monthly Penalty Counts and Total Cases",
    x = "Month",
    y = "Number",
    fill = "Penalty Type",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ====================================================
# PLOT 2 — Monthly penalty composition (100%)
# ====================================================
p2 <- ggplot(
  penalty_monthly_share,
  aes(x = Month, y = Share, fill = PenaltyType)
) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Penalty Types (100%)",
    x = "",
    y = "Share",
    fill = "Penalty Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ====================================================
# PLOT 3 — Monthly composition of AI hallucination cases (Top 3 + Other)
# ====================================================
p3 <- ggplot(
  ai_party_monthly_share,
  aes(x = Month, y = Share, fill = Party_grouped)
) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Party Responsible for AI Hallucinations (Top 3 + Other, 100%)",
    x = "",
    y = "Share",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ====================================================
# FINAL OUTPUT — vertical stack
# ====================================================
p1 / p3
