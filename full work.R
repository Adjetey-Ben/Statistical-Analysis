setwd("/home/rscc/Downloads/carol/Climate Dataset/aaa/")
setwd("/media/stenzy/706A-2272/carol/Climate Dataset/aaa/final data")
library(tidyverse)
library(readxl)
library(corrplot)
library(car)
library(broom)

read_csv("Kete Krachi_1.csv") -> krachi
read_csv("Kintampo_1.csv") -> kintampo
read_csv("Wenchi_1.csv") -> wenchi
read_csv("Ejura_1.csv") -> ejura
read_csv("Atebubu_1.csv") -> atebubu




########### Data Tidy


file_paths <- list.files(path = "/home/rscc/Downloads/carol/Climate Dataset/aaa", 
                         pattern = "Temperature.xlsx", full.names = TRUE)

all_data <- list()

for (file in file_paths) {
 data <- read_xlsx(file, sheet = "Kete Krachi")
 
 num_cols <- names(data)[1:30]  
 data <- data %>%
  mutate(across(all_of(num_cols), as.numeric, .names = "{col}"))
 
 data <- pivot_longer(data, cols = 1:30, names_to = "Year", values_to = "Value")
 
 file_name <- tools::file_path_sans_ext(basename(file))
 data <- rename(data, !!file_name := Value)
 
 data <- data %>% select(Year, !!file_name)
 
 all_data[[file_name]] <- data
}
combined_data <- bind_cols(all_data) 

combined_data |> 
 select(Year...1, Sunshine, Rainfall, EvapoTranspiration, `Relative humidity`, `Soil Moisture`) -> a

write.csv(a, file = "Kete Krachi.csv", row.names = F)


############## addition data tidy


for (file in file_paths) {
 data <- read_xlsx(file, sheet = "Kete Krachi_AVG")
 
 num_cols <- names(data)[1:30] 
 data <- data %>%
  mutate(across(all_of(num_cols), as.numeric, .names = "{col}"))
 
 data <- pivot_longer(data, cols = 1:30, names_to = "Year", values_to = "Value")
 
 file_name <- tools::file_path_sans_ext(basename(file))
 data <- rename(data, !!file_name := Value)
 
 data <- data %>% select(Year, !!file_name)
 
 all_data[[file_name]] <- data
}


combined_data <- bind_cols(all_data) 


combined_data |> 
 select(Year...1, Sunshine, Rainfall, EvapoTranspiration, `Relative humidity`, `Soil Moisture`) -> a

write.csv(a, file = "Kete Krachi_AVG.csv", row.names = F)


######### Adding the first data to that of the second
read.csv("Ejura_AVG.csv") -> w1
read.csv("Ejura.csv") -> w2

cbind(w2,w1)-> zx
write.csv(zx, file = "Ejura_final.csv", row.names = F)

########### Pre-processing
read_csv("Ejura_final.csv", na= "9988") -> data
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

data |> 
 group_by(Year) |> 
 summarise(avg_temp = mean(Temperature, na.rm = T),
           avg_rh = mean(Relative.humidity, na.rm =T),
           avg_sunshine = mean(Sunshine, na.rm =T),
           avg_soil_moisture = mean(Soil.Moisture, na.rm = T),
           total_rainfall = sum(Rainfall, na.rm = T),
           avg_evapo = mean(EvapoTranspiration, na.rm = T)) -> wm

write.csv(wm, file = "Ejura_1.csv", row.names = F)

################## Making sure all columns are numeric
df <- wm %>%
 select(-Year) %>%
 mutate(across(everything(), as.numeric))

##########Yield (Extra Information added)
 data.frame(Yield =	c(0.9,	1.8,	1.8,	2.2,	1.5,	2.2,	2.8,	1.8,	1.5,	1.5,	1.5,	0.73,	1.2,	1.315,	1.22,	1.32,	2.1,	2.35,	2.4,	1.185,	
                       1.21,	1.23,	1.455,	1.395,	1.495,	1.55,	2.06,	3.56,	2.246,	2.76)) -> z
cbind(a, z) ->zz

######## Correlation plot
cor(kin_uniform, use = "complete.obs") -> cor_matrix
corrplot(cor_matrix, method = "color", type = "full", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Plot", mar = c(0, 0, 1, 0))

write.csv(zz, file = "Kete Krachi_1.csv", row.names = FALSE)

################ Collinearity
ejura |> 
  select(-Year) -> ejura

kintampo |> 
  select(-Year) -> kintampo

wenchi |> 
  select(-Year) -> wenchi

atebubu |> 
  select(-Year) -> atebubu

krachi |> 
  select(-Year) -> krachi


####### Finding the VIF with the collinear variable available
vif_model <- lm(Yield~ avg_temp+ avg_rh + avg_sunshine + avg_soil_moisture+ total_rainfall+ avg_evapo, data = kin_uniform)

VIF_values <- vif(vif_model)

print(VIF_values)


as.data.frame(VIF_values) -> a
write.csv(a, file = "VIF values with collinearity_krachi.csv", row.names = TRUE)
####### Finding the VIF without the collinear variable available
vif_model <- lm(Yield~ avg_temp+  avg_sunshine + avg_soil_moisture+ total_rainfall+ avg_evapo, data = atebubu)

VIF_values <- vif(vif_model)

print(VIF_values)

as.data.frame(VIF_values) -> a
write.csv(a, file = "VIF values without collinearity_krachi.csv", row.names = TRUE)

######## PCA

pca <- prcomp(atebubu[, -which(names(atebubu) == "Yield")], scale. = TRUE)

print(summary(pca))

pca_scores <- data.frame(pca$x)
model_pca <- lm(ejura$Yield ~ pca_scores$PC1 + pca_scores$PC2+ pca_scores$PC3+ 
                  pca_scores$PC4+ pca_scores$PC5+ pca_scores$PC6)

print(summary(model_pca))
 
model_pca <- lm(Yield~., data = cbind(Yield = atebubu$Yield, pca_scores))



##################### Multi Regression
vif_model <- lm(Yield~ avg_temp+  avg_sunshine + avg_soil_moisture+ total_rainfall+ avg_evapo, data = kintampo)
summary(vif_model)
predict(vif_model) -> kintampo$Predicted_Yield

plot(kintampo$Yield, kintampo$Predicted_Yield, xlab = "Observed Yield", ylab = "Predicted Yield",
     main = "Observed vs Predicted Yield") 
abline(0, 1, col = "red")



plot <- ggplot(krachi, aes(x = Yield, y = Predicted_Yield)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Yield_krachi", x = "Actual Yield", y = "Predicted Yield")
png("krachi_regression.png")
print(plot)
dev.off()

kintampo
#### Residual analysis
par(mfrow = c(2,2))
plot(vif_model)


############ Normal plot
ggplot(data = atebubu, aes(x = atebubu$Year, y = atebubu$total_rainfall)) +
  geom_point(col = "blue")+
  geom_line(col = "blue") +
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Rainfall Plot_atebubu", x= "Year", y= "Rainfall")+
  theme_minimal()

ggplot(data = atebubu, aes(x = atebubu$Year, y = atebubu$avg_temp)) +
  geom_point(col = "red")+
  geom_line(col = "red") +
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Average Temperature Plot_atebubu", x= "Year", y= "Temperature")+
  theme_minimal()

########### Anomaly Plot
kintampo$total_rainfall-mean(kintampo$total_rainfall) -> kintampo$anomaly
kintampo$avg_temp-mean(kintampo$avg_temp) -> kintampo$anomaly_temp

ggplot(data = kintampo, aes(x = kintampo$Year, y = kintampo$anomaly)) +
  geom_point(col = "blue")+
  geom_line(col = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Rainfall Anomaly Plot_kintampo", x= "Year", y= "Rainfall Anomaly")+
  theme_minimal()


ggplot(data = kintampo, aes(x = kintampo$Year, y = kintampo$anomaly_temp)) +
  geom_point(col = "red")+
  geom_line(col = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Temperature Anomaly Plot_kintampo", x= "Year", y= "Temperature Anomaly")+
  theme_minimal()

###### Trend line and P-value
a <- lm(wenchi$total_rainfall ~ wenchi$Year, data = wenchi)
b <- lm(wenchi$avg_temp ~ wenchi$Year, data = wenchi)

rain_p <- tidy(a)$p.value[2]
temp_p <- tidy(b)$p.value[2]


zz <- ggplot(data = wenchi, aes(x = wenchi$Year, y = wenchi$total_rainfall)) +
  geom_point(col = "blue")+
  geom_line(col = "blue") +
  geom_smooth(method = "lm", se = F, col = "black")+
  annotate("text", x = 2020, y = max(wenchi$total_rainfall), 
           label = paste("p-value:", format(rain_p, digits = 5)), col = "black", size= 6)+
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Rainfall Plot_wenchi", x= "Year", y= "Rainfall")+
  theme_minimal()
png("wenchi_rain pvalue.png", width = 821, height = 407)
print(zz)
dev.off()

mm <- ggplot(data = wenchi, aes(x = wenchi$Year, y = wenchi$avg_temp)) +
  geom_point(col = "red")+
  geom_line(col = "red") +
  geom_smooth(method = "lm", se = F, col = "black")+
  annotate("text", x = 2020, y = max(wenchi$avg_temp), 
           label = paste("p-value:", format(temp_p, digits = 4)), col = "black", size =6)+
  scale_x_continuous(breaks = seq(1992, 2022, by = 5), limits = c(1992, 2022))+
  labs(title = "Average Temperature Plot_wenchi", x= "Year", y= "Temperature")+
  theme_minimal()
png("wenchi_temp pvalue.png", width = 821, height = 407)
print(mm)
dev.off()


###### Rainfall Anomaly
ejura$Rainfall_Index <- (ejura$total_rainfall - mean(ejura$total_rainfall)) / sd(ejura$total_rainfall)
a <- ggplot(ejura, aes(x = Year, y = Rainfall_Index, fill = Rainfall_Index >= 0)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  scale_x_continuous(breaks = seq(1990, 2022, by = 3), limits = c(1990, 2022))+
  labs(x = "Year", y = "Rainfall Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Rainfall_Anomaly Plot_ejura") +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

png("Rainfall_Anomaly Plot_ejura.png")
print(a)
dev.off()
a

##### Temperature Aanomaly
krachi$Temp_Index <- (krachi$avg_temp - mean(krachi$avg_temp)) / sd(krachi$avg_temp)
b <- ggplot(krachi, aes(x = Year, y = Temp_Index, fill = Temp_Index > 0)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("brown", "darkgreen"), guide = FALSE) +
  scale_x_continuous(breaks = seq(1990, 2022, by = 3), limits = c(1990, 2022))+
  labs(x = "Year", y = "Temp Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Temperature_Anomaly Plot_krachi")+
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


png("Temperature_Anomaly Plot_krachi.png")
print(b)
dev.off()
b


