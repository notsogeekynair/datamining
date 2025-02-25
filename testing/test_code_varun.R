df$PERNP
WKHP,WKWN,PERNP,PINCP,POVPIP

df$WKHP
h <- hist(df$WKHP, breaks=5, plot=FALSE)
h$breaks

hist(df$WKHP)
unique(df$WKHP)
df$WKHP
df$WKHP <- ifelse(df$WKHP == 0, 0,  # NA values already converted to 0
                  ifelse(df$WKHP >= 1 & df$WKHP <= 20, 1,  # Between 1 and 20 hours
                         ifelse(df$WKHP > 20 & df$WKHP <= 40, 2,  # Between 20 and 40 hours
                                ifelse(df$WKHP > 40 & df$WKHP <= 60, 3,  # Between 40 and 60 hours
                                       ifelse(df$WKHP > 60 & df$WKHP <= 98, 4,  # Between 60 and 98 hours
                                              ifelse(df$WKHP >= 99, 5, NA))))))  # 99 or more hours


hist(df$WKHP)
df$WKHP

#For WKWN
df$WKWN
hist(df$WKWN,breaks=5)
unique(df$WKWN)
df$WKWN <- ifelse(df$WKWN == 0, 0,  # NA values already converted to 0
                  ifelse(df$WKWN >= 1 & df$WKWN <= 13, 1,  # 1-13 weeks (1st quarter)
                         ifelse(df$WKWN > 13 & df$WKWN <= 26, 2,  # 14-26 weeks (2nd quarter)
                                ifelse(df$WKWN > 26 & df$WKWN <= 39, 3,  # 27-39 weeks (3rd quarter)
                                       ifelse(df$WKWN > 39 & df$WKWN <= 51, 4,  # 40-51 weeks (almost full year)
                                              ifelse(df$WKWN == 52, 5, NA))))))  # 52 weeks (full year)
df$WKWN
hist(df$WKWN)

#foR pernp
hist(df$PERNP)

hist(df$PERNP, breaks =5)
unique(df$PERNP)
df$PERNP_bin <- ifelse(df$PERNP == 0,0,  # No earnings
                       ifelse(df$PERNP == -10000, 1,  # Significant loss
                              ifelse(df$PERNP >= -9999 & df$PERNP <= -1, 2,  # Small losses
                                     ifelse(df$PERNP >= 1 & df$PERNP <= 10000, 3,  # Low earners
                                            ifelse(df$PERNP > 10000 & df$PERNP <= 50000, 4,  # Middle earners
                                                   ifelse(df$PERNP > 50000 & df$PERNP <= 200000, 5,  # High earners
                                                          ifelse(df$PERNP > 200000, 6,  # Very high earners
                                                                 NA)))))))  
df$PERNP_bin
hist(df$PERNP_bin)


#POVPIP
test$POVPIP
hist(test$POVPIP)
unique(test$POVPIP)
test$POVPIP_bin <- ifelse(df$POVPIP == -1, 0,  # NA (Under 15 or select living conditions)
                          ifelse(df$POVPIP >= 0 & df$POVPIP <= 99, 1,  # 0 to 99% of the poverty level
                                 ifelse(df$POVPIP >= 100 & df$POVPIP <= 199, 2,  # 100 to 199% of the poverty level
                                        ifelse(df$POVPIP >= 200 & df$POVPIP <= 299, 3,  # 200 to 299% of the poverty level
                                               ifelse(df$POVPIP >= 300 & df$POVPIP <= 399, 4,  # 300 to 399% of the poverty level
                                                      ifelse(df$POVPIP >= 400 & df$POVPIP <= 500, 5,  # 400 to 500% of the poverty level
                                                             ifelse(df$POVPIP == 501, 6, NA)))))))  # 501% or more of the poverty level
test$POVPIP_bin
hist(test$POVPIP_bin)
