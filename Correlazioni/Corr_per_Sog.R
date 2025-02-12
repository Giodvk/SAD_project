#Correlazioni delle singole variabili con i soggetti


#Correlazione tra body_acc_x ed i soggetti
plot(df$body_acc_x, df$Subject, main ="Correlazione tra soggetti con accelerazione asse x",
     ylab = "Soggetto", xlab = "Accelerazione asse x")
corr_acc_x <- cor(df$body_acc_x, df$Subject, method = "spearman")

#Correlazione tra body_acc_y ed i soggetti 
plot(df$body_acc_y, df$Subject, main ="Correlazione tra soggetti con accelerazione asse y",
     ylab = "Soggetto", xlab = "Accelerazione asse y")
corr_acc_y <- cor(df$body_acc_y, df$Subject, method = "spearman")

#Correlazione tra body_acc_z ed i soggetti 
plot(df$body_acc_z, df$Subject, main = "Correlazione tra soggetti con accelerazione asse z",
     ylab = "Soggetto", xlab = "Accelerazione asse z")
corr_acc_z <- cor(df$body_acc_z, df$Subject, method = "spearman")

#Correlazione tra body_gyro_x ed i soggetti
plot(df$body_gyro_x, df$Subject, main = "Correlazione tra soggetti con giroscopio asse x",
     ylab = "Soggetto", xlab = "Giroscopio asse x")
corr_gyro_x <- cor(df$body_gyro_x, df$Subject, method = "spearman")

#Correlazione tra body_gyro_y ed i soggetti
plot(df$body_gyro_y, df$Subject, main = "Correlazione tra soggetti con giroscopio asse y",
     ylab = "Soggetto", xlab = "Giroscopio asse y")
corr_gyro_y <- cor(df$body_gyro_y, df$Subject, method = "spearman")

#Correlazione tra body_gyro_z ed i soggetti
plot(df$body_gyro_z, df$Subject, main = "Correlazione tra soggetti con giroscopio asse z",
     ylab = "Soggetto", xlab = "Giroscopio asse z")
corr_gyro_z <- cor(df$body_gyro_z, df$Subject, method = "spearman")

#Correlazione tra total_acc_x ed i soggetti
plot(df$total_acc_x, df$Subject, main = "Correlazione tra soggetti ed acc totale asse x",
     ylab = "Soggetto", xlab = "Accelerazione totale asse x")
corr_tot_x <- cor(df$total_acc_x, df$Subject, method = "spearman")

#Correlazione total_acc_y ed i soggetti
plot(df$total_acc_y, df$Subject, main = "Correlazione tra soggetti ed acc totale asse y",
     ylab = "Soggetto", xlab = "Accelerazione totale asse y")
corr_tot_y <- cor(df$total_acc_y, df$Subject, method = "spearman")

#Correlazione total_acc_z ed i soggetti
plot(df$total_acc_z, df$Subject, main = "Correlazione tra soggetti ed acc totale asse z",
     ylab = "Soggetto", xlab = "Accelerazione totale asse z")
corr_tot_z <- cor(df$total_acc_z, df$Subject, method = "spearman")


