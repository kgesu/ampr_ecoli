# Load libraries ====
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape)
library(splitstackshape)
library(Hmisc)
library(data.table) 
library(DT)
library(gt)
library(paletteer)
library(RColorBrewer)
library(scales)

# LCC isolate data tidy ====
lcc_rawdata <- read.csv("ecoli_testlog_lcc.csv", header=TRUE, na.strings=c("","NA"))
head (lcc_rawdata)

lcc_clean <- subset(lcc_rawdata, select=c(ï..isolate,citrate,phenol_red_lac,S,I,M,MR,VP,ecoli_confirm))
lcc_clean <- rename(lcc_clean,c(
  "ï..isolate" = "Isolate",
  "citrate" = "Simmons Citrate",
  "phenol_red_lac" = "Phenol Red with Lactose",
  "S" = "Sulfur",
  "I" = "Indole",
  "M" = "Motility",
  "MR" = "MR",
  "VP" = "VP"))
head(lcc_clean)

# MB isolates data tidy ====
mb_rawdata <- read.csv("ecoli_testlog_mb.csv", header=TRUE)
head (mb_rawdata)

mb_clean <- subset(mb_rawdata, select=c(ï..isolate,citrate,phenol_red_lac,S,I,M,MR,VP,ecoli_confirm))
#mb_clean$MR<-as.numeric(as.character(mb_clean$MR))
#mb_clean$VP<-as.numeric(as.character(mb_clean$VP))
mb_clean$phenol_red_lac[which(mb_clean$phenol_red_lac == "")] <- "IP"
mb_clean$S[which(mb_clean$S == "")] <- "IP"
mb_clean$I[which(mb_clean$I == "")] <- "IP"
mb_clean$M[which(mb_clean$M == "")] <- "IP"
mb_clean[is.na(mb_clean)] <- "IP"
head(mb_clean)
mb_clean <- rename(mb_clean,c(
  "ï..isolate" = "Isolate",
  "phenol_red_lac" = "Phenol Red with Lactose",
  "S" = "Sulfur",
  "I" = "Indole",
  "M" = "Motility",
  "MR" = "MR",
  "VP" = "VP"))
head(mb_clean)

# (gt)able for LCC isolates ====
lcc_table <- 
  lcc_clean %>% 
  gt() %>% 
  tab_header(title = "Biochemical Test Results of Isolates Collected from Los Cerritos Channel") %>% 
  tab_source_note("NA = Ruled out due the result of other test(s)") %>%  #md() and `` to change font to Courier
  cols_label("Simmons Citrate" = html("Simmons<br>Citrate"),
             "Phenol Red with Lactose" = html("Phenol Red<br>with Lactose"),
             ecoli_confirm = "E. coli?") %>% 
  cols_align("center") %>% 
  tab_style(
    style = list(
      cell_fill(color = "#E0ECF4")),
    locations = cells_body(
      rows = ecoli_confirm == "Y")) %>% 
   tab_spanner(label = "Biochemial Tests",
              columns = vars("Simmons Citrate", "Phenol Red with Lactose", "Sulfur", "Indole", "Motility", "MR", "VP"))

# (gt)able for MB isolates ----
mb_table <- 
  mb_clean %>% 
  gt() %>% 
  tab_header(title = "Biochemical Test Results of Isolates Collected from Mother's Beach in Alamitos Bay") %>% 
  tab_source_note(md("IP = Biochemical test in progress")) %>%  #md() and ` ` to change font to Courier, ** ** for bold, * * for italic
  cols_label("citrate" = html("Simmons<br>Citrate"),
             "Phenol Red with Lactose" = html("Phenol Red<br>with Lactose"),
             ecoli_confirm = "E. coli?") %>% 
  cols_align("center") %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FFF7EC")),
    locations = cells_body(
      rows = citrate == "-")) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF")),
    locations = cells_body(
      rows = ecoli_confirm == "N")) %>% 
  tab_spanner(label = "Biochemial Tests",
              columns = vars("citrate", "Phenol Red with Lactose", "Sulfur", "Indole", "Motility", "MR", "VP"))

# Save tables as .png ----
gtsave(lcc_table, "lcc_table.png", path = "/Users/Kristine Gesulga/OneDrive - csulb/DILLON LAB/ampr_ecoli")
gtsave(mb_table, "mb_table.png", path = "/Users/Kristine Gesulga/OneDrive - csulb/DILLON LAB/ampr_ecoli")
