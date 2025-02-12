
#Correlazione tra body_acc_x e l'attività
plot(df$body_acc_x, df$Activity, main ="Correlazione tra accelerazione asse x ed Attività"
     ,ylab = "Attività", xlab = "Accelerazione asse x")
corr_acc_x <- cor(df$Activity, df$body_acc_x,method ="spearman")


#Correlazione tra body_acc_y e l'attività
plot(df$body_acc_y, df$Activity, main = "Correlazione tra accelerazione asse y ed Attivita",
     ylab = "Attività", xlab = "Accelerazione asse y")
corr_acc_y <- cor(df$body_acc_y, df$Activity, method = "spearman")


#Correlazione tra body_acc_z e l'attività
plot(df$body_acc_z, df$Activity, main ="Correlazione tra accelerazione asse z e Attività",
     ylab = "Attività", xlab = "Accelerazione asse z")
corr_acc_z <- cor(df$body_acc_z, df$body_acc_z, method = "spearman")


#Correlazione tra body_gyro_x e l'attività
plot(df$body_gyro_x, df$Activity, main ="Correlazione tra giroscopio asse x ed Attività",
     ylab = "Attività", xlab = "Giroscopio asse x")
corr_gyro_x <- cor(df$body_gyro_x, df$Activity, method = "spearman")


#Correlazione tra body_gyro_y e l'attività
plot(df$body_gyro_y, df$Activity, main = "Correlazione tra giroscopio asse y ed Attività",
     ylab = "Attività", xlab ="Giroscopio asse y")
corr_gyro_y <- cor(df$body_gyro_y, df$Activity, method = "spearman")


#Correlazione tra body_gyro_z e l'attività
plot(df$body_gyro_z, df$Activity, main = "Correlazione tra giroscopio asse z ed Attività",
     ylab = "Attività", xlab ="Giroscopio asse z")
corr_gyro_z <- cor(df$body_gyro_z, df$Activity, method = "spearman")



#Correlazione tra total_acc_x e l'attività
plot(df$total_acc_x, df$Activity, main ="Correlazione tra acc totale asse x ed Attività",
     ylab = "Attività", xlab = "Accelerazione totale asse x")
corr_tot_x <- cor(df$total_acc_x, df$Activity, method ="spearman")



#Correlazione tra total_acc_y e l'attività
plot(df$total_acc_y, df$Activity, main = "Correlazione tra acc totale asse y ed Attività",
     ylab = "Attività", xlab = "Accelerazione totale asse y")
corr_tot_y <- cor(df$total_acc_y, df$Activity, method = "spearman")


#Correlazione tra total_acc_z e l'attività
plot(df$body_acc_z, df$Activity, main = "Correlazione tra acc totale asse z ed Attività",
     ylab = "Attività", xlab = "Accelerazione totale asse z")
corr_tot_z <- cor(df$total_acc_z, df$body_acc_z, method = "spearman")
