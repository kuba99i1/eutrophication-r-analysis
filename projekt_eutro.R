library(tidyverse)
library(ggplot2)
library(tidyr)
library(rMR)
library(readxl)

BUG <- read_excel("BUG.xls")
View(BUG)
k = 1.2
m = 1.5


BUG <- 
  BUG %>% 
  mutate(aDO = (DO.saturation(TLEN_ROZP, TEMP_WOD, elevation.m = 163.2))*100)

BUG <- BUG %>%
  mutate(minN = (AZ_AMONOW + AZ_AZOTAN + AZ_AZOTYN) * 1000) %>%
  mutate(TRIX = (log(((CHLOROFI_A) * abs(100 - aDO) * minN * (FSFR_OGP * 1000)), 10) + k) / m)


linear_model <- lm(BUG$ODCZYN ~ BUG$TLEN_ROZP)


a <- linear_model$coefficients[2]

################################ INDEKS ITS ###################################################
n=1
BUG <-
  BUG %>% 
  mutate(ITS = BUG$ODCZYN/1+a*(100-BUG$TLEN_ROZP)/n)

#################################  INDEKS TSI ####################################################

BUG <- BUG  %>% mutate("TSI(Chl)" =  9.81 * log(CHLOROFI_A,exp(2.71)) + 30.6)

BUG <- BUG %>% mutate("TSI(TP)" = 14.42 * log(FSFR_OGP*1000,exp(2.71)) + 4.15)


nowa_ramka <- data.frame(DATA = BUG$DATA,
  CHLOROFI_A = BUG$CHLOROFI_A,
  FSFR_OGP = BUG$FSFR_OGP,
  aDO = BUG$aDO,
  minN = BUG$minN,
  k = k,
  m = m,
  ODCZYN = BUG$ODCZYN,
  a = a,
  TLEN_ROZP = BUG$TLEN_ROZP,
  n = n,
  row.names = NULL
)
nowa_ramka
view(nowa_ramka)

indeksy <- data.frame(DATA = BUG$DATA,
  `TSI(CHL)` = BUG$`TSI(Chl)`,
  `TSI(TP)` = BUG$`TSI(TP)`,TRIX = BUG$TRIX,ITS = BUG$ITS
)

View(indeksy)
########################################### WARTOSCI OCEN DLA INDEKSÓW ##############################
 
indeksy$ITS_ocena <- ifelse(BUG$ITS > 8.0, "eutrofia", NA)

indeksy$TRIX_ocena <- as.factor(ifelse(BUG$TRIX < 2, "ultraoligotrofia",
                                       ifelse(BUG$TRIX >= 2 & BUG$TRIX < 4, "oligotrofia",
                                              ifelse(BUG$TRIX >= 4 & BUG$TRIX < 5, "mezotrofia",
                                                     ifelse(BUG$TRIX >= 5 & BUG$TRIX < 6, "mezoeutrofia",
                                                            ifelse(BUG$TRIX >= 6 & BUG$TRIX <= 8, "eutrofia", NA))))))


indeksy$TSI_TP_ocena <- ifelse(BUG$`TSI(TP)` <= 4, "ultraoligotrofia",
                               ifelse(BUG$`TSI(TP)` < 10, "oligotrofia",
                                      ifelse(BUG$`TSI(TP)` < 35, "mezotrofia",
                                             ifelse(BUG$`TSI(TP)` < 100, "eutrofia", "hipereutrofia"))))
indeksy$TSI_CHL_ocena <- ifelse(is.na(BUG$`TSI(Chl)`), NA,
                                ifelse(BUG$`TSI(Chl)` <= 1, "ultraoligotrofia",
                                       ifelse(BUG$`TSI(Chl)` < 2.5, "oligotrofia",
                                              ifelse(BUG$`TSI(Chl)` < 8, "mezotrofia",
                                                     ifelse(BUG$`TSI(Chl)` < 25, "eutrofia", "hipereutrofia")))))


View(indeksy)
######################################### LICZEBNOśĆ konkretnych wartośći indeksów ###############

stat_oceny_TRIX <- table(indeksy$TRIX_ocena)
stat_oceny_TRIX

stat_oceny_ITS<- table(indeksy$ITS_ocena)
stat_oceny_ITS


stat_oceny_TSI_CHL <- table(indeksy$TSI_CHL_ocena)
stat_oceny_TSI_CHL


stat_oceny_TSI_TP <- table(indeksy$TSI_TP_ocena)
stat_oceny_TSI_TP

liczba_ocen <- data.frame(ITS = stat_oceny_ITS, TRIX = stat_oceny_TRIX, TSI_TP = stat_oceny_TSI_TP, TSI_CHL = stat_oceny_TSI_CHL)
liczba_ocen


ITS_srednioroczne <- aggregate(ITS ~ format(DATA, "%Y"), data = indeksy, mean)
ITS_srednioroczne



ggplot(BUG, aes(x = TLEN_ROZP, y = ODCZYN)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "TLEN_ROZP", y = "ODCZYN",title = "Zależność między TLEN_ROZP a ODCZYN") +
  theme_minimal()+ theme(panel.border = element_rect(color = "black", fill = NA),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.line = element_line(color = "black"))



ggplot(BUG, aes(x = CHLOROFI_A, y = FSFR_OGP)) +
  geom_hex(color = "steelblue",size = 0.5) +
 labs(x = "CHLOROFI_A", y = "FSFR_OGP", title = "Zależność między CHLOROFI_A a FSFR_OGP") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

ggplot(data = indeksy) +
  geom_bar(aes(x = TRIX_ocena, fill = "TRIX"), width = 0.2, position = "dodge") +
  geom_bar(aes(x = ITS_ocena, fill = "ITS"), width = 0.2, position = "dodge") +
  geom_bar(aes(x = TSI_CHL_ocena, fill = "TSI_CHL"), width = 0.2, position = "dodge") +
  geom_bar(aes(x = TSI_TP_ocena, fill = "TSI_TP"), width = 0.2, position = "dodge") +
  labs(x = "Ocena", y = "Liczba ocen") +
  ggtitle("Liczba ocen dla poszczególnych indeksów") +
  scale_fill_manual(values = c("TRIX" = "steelblue", "ITS" = "darkorange",
                               "TSI_CHL" = "forestgreen", "TSI_TP" = "purple")) +
  theme_minimal()+ 
  scale_y_continuous(breaks = seq(0, max(stat_oceny_ITS, stat_oceny_TRIX, stat_oceny_TSI_TP, stat_oceny_TSI_CHL), by = 2))