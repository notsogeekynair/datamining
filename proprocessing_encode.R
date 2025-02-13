df <- df %>%
  mutate(
    MARHYP = case_when(
      is.na(MARHYP) ~ 0,               # Never Married/Under 15 (NA) → 0
      MARHYP == 1944 ~ 1,              # 1944 or Earlier
      between(MARHYP, 1945, 1964) ~ 2, # 1945–1964
      between(MARHYP, 1965, 1984) ~ 3, # 1965–1984
      between(MARHYP, 1985, 2004) ~ 4, # 1985–2004
      between(MARHYP, 2005, 2023) ~ 5, # 2005–2023
      TRUE ~ NA_real_                  # Handle unexpected values
    )
  )