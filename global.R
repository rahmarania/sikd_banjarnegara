library(flexdashboard)
library(shiny)
library(shinydashboard)
library(dplyr) # manipulation
library(DT)
library(ggplot2)
library(glue)
library(plotly)
library(shinythemes)
library(tm)
library(wordcloud2)

karakteristik <- read.csv('model/karakteristik.csv')
pendanaan <- read.csv('model/pendanaan.csv')
potensi_desa <- read.csv('model/potensi_desa.csv')
peningkatan_pad <- read.csv('model/pad.csv')
perekonomian_desa <- read.csv('model/perekonomian_desa.csv')
program_wisata <- read.csv('model/program_wisata.csv')

model_tanpa_potensi <- data.frame(c(karakteristik, pendanaan, peningkatan_pad, perekonomian_desa, program_wisata))

model_tanpa_potensi <- model_tanpa_potensi %>% mutate(
  # karakteristik
  jk = ifelse(jk == 1, "Perempuan", "Laki-laki"),
  skala_usaha = case_when(skala_usaha == 1 ~ "Mikro/home industry",
                          skala_usaha == 2 ~ "Kecil",
                          skala_usaha == 3 ~ "Menengah",
                          skala_usaha == 4 ~ "Besar"),
  tahu_pendanaan = ifelse(tahu_pendanaan == 1, "Tahu", "Tidak tahu"),
  tahu_dana_desa = ifelse(tahu_dana_desa == 1, "Tahu", "Tidak tahu"),
  tahu_dana_csr = ifelse(tahu_dana_csr == 1, "Tahu", "Tidak tahu"),
  modal_dari = case_when(modal_dari == 1 ~ "Modal sendiri",
                         modal_dari == 2 ~ "Pinjam saudara",
                         modal_dari == 3 ~ "Pinjam bank",
                         modal_dari == 4 ~ "Bantuan desa",
                         modal_dari == 5 ~ "Bantuan dana CSR"),
  tahu_pltm = ifelse(tahu_pltm == 1, "Tahu", "Tidak tahu"),
  pltm_beri_bantuan = ifelse(pltm_beri_bantuan == 1, "Ya", "Tidak"),
  pltm_jenis_bantuan = case_when(pltm_jenis_bantuan == 1 ~ "Sumbangan tidak mengikat (hibah)",
                                 pltm_jenis_bantuan == 2 ~ "Sumbangan bergulir",
                                 pltm_jenis_bantuan == 3 ~ "Pelatihan",
                                 pltm_jenis_bantuan == 4 ~ "Bimbingan teknis usaha",
                                 TRUE ~ as.character(pltm_jenis_bantuan)),
  desa_beri_bantuan = ifelse(desa_beri_bantuan == 1, "Ya", "Tidak"),
  desa_jenis_bantuan = case_when(desa_jenis_bantuan == 1 ~ "Sumbangan tidak mengikat (hibah)",
                                 desa_jenis_bantuan == 2 ~ "Sumbangan bergulir",
                                 desa_jenis_bantuan == 3 ~ "Pelatihan",
                                 desa_jenis_bantuan == 4 ~ "Bimbingan teknis usaha",
                                 TRUE ~ as.character(desa_jenis_bantuan)),
  dana_desa_utk_pembangunan = case_when(dana_desa_utk_pembangunan == 1 ~ "Tidak berperan",
                                        dana_desa_utk_pembangunan == 2 ~ "Cukup berperan",
                                        dana_desa_utk_pembangunan == 3 ~ "Berperan"),
  dana_desa_utk_infrastruktur = case_when(dana_desa_utk_infrastruktur == 1 ~ "Tidak berperan",
                                          dana_desa_utk_infrastruktur == 2 ~ "Cukup berperan",
                                          dana_desa_utk_infrastruktur == 3 ~ "Berperan"),
  dana_desa_utk_bumdes = case_when(dana_desa_utk_bumdes == 1 ~ "Tidak berperan",
                                   dana_desa_utk_bumdes == 2 ~ "Cukup berperan",
                                   dana_desa_utk_bumdes == 3 ~ "Berperan"),
  dana_csr_utk_pembangunan = case_when(dana_csr_utk_pembangunan == 1 ~ "Tidak berperan",
                                       dana_csr_utk_pembangunan == 2 ~ "Cukup berperan",
                                       dana_csr_utk_pembangunan == 3 ~ "Berperan"),
  dana_csr_utk_infrastruktur = case_when(dana_csr_utk_infrastruktur == 1 ~ "Tidak berperan",
                                         dana_csr_utk_infrastruktur == 2 ~ "Cukup berperan",
                                         dana_csr_utk_infrastruktur == 3 ~ "Berperan"),
  dana_csr_utk_bumdes = case_when(dana_csr_utk_bumdes == 1 ~ "Tidak berperan",
                                  dana_csr_utk_bumdes == 2 ~ "Cukup berperan",
                                  dana_csr_utk_bumdes == 3 ~ "Berperan"),
  
  dana_desa_ekonomi_rakyat = case_when(dana_desa_ekonomi_rakyat == 1 ~ "Tidak berperan",
                                       dana_desa_ekonomi_rakyat == 2 ~ "Cukup berperan",
                                       dana_desa_ekonomi_rakyat == 3 ~ "Berperan"),
  dana_desa_penghasilan_rakyat = case_when(dana_desa_penghasilan_rakyat == 1 ~ "Tidak berperan",
                                           dana_desa_penghasilan_rakyat == 2 ~ "Cukup berperan",
                                           dana_desa_penghasilan_rakyat == 3 ~ "Berperan"),
  dana_desa_modal_rakyat
  = case_when(dana_desa_modal_rakyat == 1 ~ "Tidak berperan",
              dana_desa_modal_rakyat == 2 ~ "Cukup berperan",
              dana_desa_modal_rakyat == 3 ~ "Berperan"),
  dana_csr_ekonomi_rakyat = case_when(dana_csr_ekonomi_rakyat == 1 ~ "Tidak berperan",
                                      dana_csr_ekonomi_rakyat == 2 ~ "Cukup berperan",
                                      dana_csr_ekonomi_rakyat == 3 ~ "Berperan"),
  dana_csr_penghasilan_rakyat = case_when(dana_csr_penghasilan_rakyat == 1 ~ "Tidak berperan",
                                          dana_csr_penghasilan_rakyat == 2 ~ "Cukup berperan",
                                          dana_csr_penghasilan_rakyat == 3 ~ "Berperan"),
  dana_csr_modal_rakyat = case_when(dana_csr_modal_rakyat == 1 ~ "Tidak berperan",
                                    dana_csr_modal_rakyat == 2 ~ "Cukup berperan",
                                    dana_csr_modal_rakyat == 3 ~ "Berperan"),
  dana_desa_utk_wisata = case_when(dana_desa_utk_wisata == 1 ~ "Tidak berperan",
                                   dana_desa_utk_wisata == 2 ~ "Cukup berperan",
                                   dana_desa_utk_wisata == 3 ~ "Berperan"),
  dana_csr_utk_wisata = case_when(dana_csr_utk_wisata == 1 ~ "Tidak berperan",
                                  dana_csr_utk_wisata == 2 ~ "Cukup berperan",
                                  dana_csr_utk_wisata == 3 ~ "Berperan"),
)

model_tanpa_potensi <- model_tanpa_potensi %>% mutate(jk = as.factor(jk),
                                                      pendidikan = as.factor(pendidikan),
                                                      skala_usaha = as.factor(skala_usaha),
                                                      tahu_pendanaan = as.factor(tahu_pendanaan),
                                                      tahu_dana_desa = as.factor(tahu_dana_desa),
                                                      tahu_dana_csr = as.factor(tahu_dana_csr),
                                                      modal_dari = as.factor(modal_dari),
                                                      tahu_pltm = as.factor(tahu_pltm),
                                                      pltm_beri_bantuan = as.factor(pltm_beri_bantuan),
                                                      pltm_jenis_bantuan = as.factor(pltm_jenis_bantuan),
                                                      desa_beri_bantuan = as.factor(desa_beri_bantuan),
                                                      desa_jenis_bantuan = as.factor(desa_jenis_bantuan),
                                                      dana_desa_utk_pembangunan = as.factor(dana_desa_utk_pembangunan),
                                                      dana_desa_utk_infrastruktur = as.factor(dana_desa_utk_infrastruktur),
                                                      dana_desa_utk_bumdes = as.factor(dana_desa_utk_bumdes),
                                                      dana_csr_utk_pembangunan = as.factor(dana_csr_utk_pembangunan),
                                                      dana_csr_utk_infrastruktur = as.factor(dana_csr_utk_infrastruktur),
                                                      dana_csr_utk_bumdes = as.factor(dana_csr_utk_bumdes),
                                                      dana_desa_ekonomi_rakyat = as.factor(dana_desa_ekonomi_rakyat),
                                                      dana_desa_penghasilan_rakyat = as.factor(dana_desa_penghasilan_rakyat),
                                                      dana_desa_modal_rakyat = as.factor(dana_desa_modal_rakyat),
                                                      dana_csr_ekonomi_rakyat = as.factor(dana_csr_ekonomi_rakyat),
                                                      dana_csr_penghasilan_rakyat = as.factor(dana_csr_penghasilan_rakyat),
                                                      dana_csr_modal_rakyat = as.factor(dana_csr_modal_rakyat),
                                                      dana_desa_utk_wisata = as.factor(dana_desa_utk_wisata),
                                                      dana_csr_utk_wisata = as.factor(dana_csr_utk_wisata))

write.csv(model_tanpa_potensi, file = "mdl_eda.csv", row.names = FALSE)

mdl <- read.csv("mdl_eda.csv")
sikd <- read.csv("sikd_eda.csv")

sikd$pendapatan <- gsub(",", ".", sikd$pendapatan)

sikd$usia <- sikd$usia <- as.numeric(gsub(" tahun", "", sikd$usia))

mdl$usia <- mdl$usia <- as.numeric(gsub(" tahun", "", mdl$usia))

#columns_to_convert <- c("tahu_pendanaan","tahu_dana_desa","tahu_dana_csr","dana_desa_utk_pembangunan","dana_desa_utk_infrastruktur","dana_desa_utk_bumdes","dana_csr_utk_pembangunan","dana_csr_utk_infrastruktur","dana_csr_utk_bumdes",
#                        "dana_desa_ekonomi_rakyat","dana_desa_penghasilan_rakyat","dana_desa_modal_rakyat","dana_csr_ekonomi_rakyat","dana_csr_penghasilan_rakyat","dana_csr_modal_rakyat","dana_desa_utk_wisata","dana_csr_utk_wisata")

#mdl <- mdl %>%
#  mutate_at(columns_to_convert, as.factor)

gender_sum <- sikd %>% 
  group_by(jk) %>% 
  summarise(total = n())

usia_ <- sikd %>% group_by(usia) %>%
  summarise(total = n()) %>% 
  ungroup() %>% 
  mutate(kelompok_usia = cut(usia, breaks = c(20, 30, 40, 50, 60, 70), labels = c("20-30", "31-40", "41-50", "51-60", "61-70"), include.lowest = TRUE))

main_job <- mdl %>% 
  group_by(pekerjaan_utama) %>% 
  summarise(total = n(),
            persen = total/nrow(mdl) * 100)

side_job <- mdl %>% 
  group_by(pekerjaan_sampingan) %>% 
  summarise(total = n(),
            persen = total/nrow(mdl) * 100) %>% 
  mutate(pekerjaan_sampingan = ifelse(pekerjaan_sampingan == "", "Tidak memiliki pekerjaan sampingan", pekerjaan_sampingan))

eco <- sikd %>% 
  group_by(peluang_ekonomi, lap_kerja) %>% 
  summarise(total = n(),
            persen = total/nrow(sikd) * 100) %>% 
  ungroup() 

trans <- sikd %>% group_by(transport_umum) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% mutate(label = glue("Total : {total} orang"))

fas <- sikd %>% 
  group_by(akses_fasil) %>% 
  summarise(total = n()) %>% 
  ungroup() %>%
  mutate(akses_fasil = ifelse(akses_fasil == is.na(akses_fasil), "jalan kaki", akses_fasil),
         label = glue("Total : {total} orang")) 

sikd$transport_umum <- tolower(sikd$transport_umum)

sikd$akses_fasil <- tolower(sikd$akses_fasil)

sikd$akses_fasil <- ifelse(is.na(sikd$akses_fasil), "jalan kaki", sikd$akses_fasil)

sikd$jarak_pusat_kota <- as.numeric(gsub("km", "", sikd$jarak_pusat_kota))

# Extract text from 'saran' column in sikd
s1 <- sikd %>% select(saran)

# Extract text from 'mengharapkan_bantuan_pltm' and 'mengharapkan_bantuan_desa' columns in model_tanpa_potensi
s2 <- model_tanpa_potensi %>% select(mengharapkan_bantuan_pltm, mengharapkan_bantuan_desa)

# Combine text from both sources into a single character vector
text_combined <- paste(s1$saran, s2$mengharapkan_bantuan_pltm, s2$mengharapkan_bantuan_desa, sep = " ")

# Create a corpus from the combined text
corpus <- Corpus(VectorSource(text_combined))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c("yang", "dan", "perlu", "adanya", "dalam", "dengan","bagi","diberikan","melakukan","untuk","setiap","mana","guna","dari","menuju","mengubah","dimiliki","kepada","lebih","terkait","seperti","terutama","dibutuhkan","memberikan"))

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Convert dtm to a matrix
dtm_matrix <- as.matrix(dtm)

# Get word frequencies
word_freq <- colSums(dtm_matrix)
