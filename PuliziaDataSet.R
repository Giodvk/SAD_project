df <- read.csv(file.choose(), head = TRUE, sep =",", dec = ".")
str(df)
df_clean <- df[!duplicated(df[, c("body_acc_x", "body_acc_y", "body_acc_z",
                                  "body_gyro_x", "body_gyro_y", "body_gyro_z")]), ]
nrow(df)
nrow(df_clean)
duplicated <- duplicated(df_clean[, c("body_acc_x", "body_acc_y", "body_acc_z",
                                "body_gyro_x", "body_gyro_y", "body_gyro_z")])
sum(duplicated)
write.csv(df_clean, "DataSet_Clean.csv", row.names = FALSE)
