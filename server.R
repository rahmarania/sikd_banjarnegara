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
library(scales)


server <- function(input, output){
  output$peta <- renderImage({
    
    list(src = "peta.png",
         width = "100%",
         height = 600)
  })
  
  output$dt <- DT::renderDataTable({
    Uraian <- c("Listrik","Jumlah Faskes","Jumlah dokter dan nakes","Keberadaan Posyandu/Posbindu","Jumlah kader Posyandu/Posbindu","Gedung Serba guna/sejenisnya","Jalan raya","Lampu jalan","Internet (E, 3G, 4G, 5G)","Pasar","Sekolah (Paud, SD, SMP, SMA/SMK)")
    Keterangan <- c("Merata di enam dusun", "2 Puskesmas", "Bidan 2 orang dan 1 orang dokter","Posyandu ada 4 untuk 6 dusun, posbindu ada kegiatannya di balai","29 orang untuk kader Bengkat A dan Bengkat B","Tidak ada","Jalan utama sudah diaspal, jalan kecil masih belum baik","Penerangan kurang","Di beberapa tempat 4G Telkomsel dan XL","Pasar Wanadadi","Terdapat Paud, SD, dan SMP. Untuk SMA perlu ke desa seberang")
    fasil <- data.frame(Uraian, Keterangan)
    
    datatable(fasil, options = list(pageLength = 5), colnames = colnames(fasil)) 
  })
  
  output$inc <- renderPlotly({
    sikd$pendapatan <- gsub(",", ".", sikd$pendapatan)
    pendapatan <- sikd %>% group_by(pendapatan) %>% summarise(total = n()) %>% ungroup() %>% mutate(label = glue("Pendapatan : {pendapatan}
      Total : {total} orang"))
    
    pl <- ggplot(pendapatan, aes(x = reorder(pendapatan, total), y = total)) +
      geom_col(aes(fill = pendapatan)) +
      geom_point(aes(col = pendapatan, text = label)) +
      labs(title = "Pendapatan Bulanan",
           subtitle = 'Sample of 25 people',
           y = 'Total',
           x = "Income") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(pl, tooltip = "text")
  })
  
  output$ec <- renderPlotly({
    eco <- sikd %>% 
      group_by(peluang_ekonomi, lap_kerja) %>% 
      filter(peluang_ekonomi %in% input$econom) %>%
      summarise(total = n(),
                persen = total/nrow(sikd) * 100) %>% 
      ungroup() 
    
    p2 <- ggplot(eco, aes(x = reorder(interaction(input$econom, lap_kerja), -total), y = total, fill = lap_kerja)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = peluang_ekonomi, lap_kerja)) +
      #geom_text(aes(label = total), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
      labs(title = "Peluang Ekonomi dan Ketersediaan Lapangan Pekerjaan",
           x = "Lapangan Pekerjaan",
           y = "Total") +
      theme_gray() + theme(plot.title = element_text(face = "bold", hjust = 0.5),
                           plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(p2)
  })
  
  output$ages <- renderPlotly({
    usia_ <- sikd %>% group_by(usia) %>% 
      filter(usia >= input$umur[1] & usia <= input$umur[2]) %>% 
                      summarise(total = n()) %>% 
                      ungroup() %>% 
                      mutate(kelompok_usia = cut(usia, breaks = c(20, 30, 40, 50, 60, 70), labels = c("20-30", "31-40", "41-50", "51-60", "61-70"), include.lowest = TRUE))
    
    ggplot(usia_, aes(x = usia, y = total)) +
      geom_bar(stat = "identity", alpha = 0.7, fill = "cornflowerblue") +
      labs(title = "Distribusi Usia Responden",
           x = "Usia",
           y = "Jumlah") +
      theme_bw() + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  
  output$educ <- renderPlotly({
    edu <- sikd %>% group_by(pendidikan) %>% summarise(total = n()) %>% ungroup() %>% mutate(label = glue("Total : {total} orang"))
    
    p3 <- ggplot(edu, aes(x = reorder(pendidikan, total), y = total)) +
      geom_col(aes(fill = pendidikan)) +
      geom_point(aes(col = pendidikan, text = label)) +
      labs(title = "Tingkat Pendidikan",
           subtitle = 'Sample masyarakat Dea Tanjungtirta',
           y = 'Total',
           x = "Pendidikan") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(p3, tooltip = "text")
  })
  
  output$job_m <- renderPlot({
    main_job <- mdl %>% 
      group_by(pekerjaan_utama) %>% 
      summarise(total = n(),
                persen = total/nrow(mdl) * 100)
    
    ggplot(main_job, aes(x = "", y = persen, fill = pekerjaan_utama)) +
      geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.8) +
      geom_bar(width = 2, stat = "identity", fill = "white", alpha = 0.2) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(legend.position = "right",legend.text = element_text(size = 15)) +
      labs(title = "Pekerjaan Utama") + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$job_s <- renderPlot({
    side_job <- mdl %>% 
      group_by(pekerjaan_sampingan) %>% 
      summarise(total = n(),
                persen = total/nrow(mdl) * 100) %>% 
      mutate(pekerjaan_sampingan = ifelse(pekerjaan_sampingan == "", "Tidak memiliki pekerjaan sampingan", pekerjaan_sampingan))
    
    ggplot(side_job, aes(x = "", y = persen, fill = pekerjaan_sampingan)) +
      geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.8) +
      geom_bar(width = 2, stat = "identity", fill = "white", alpha = 0.2) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(legend.position = "left",legend.text = element_text(size = 13)) +
      labs(title = "Pekerjaan Sampingan") + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$akt <- renderPlotly({
    keg <- sikd %>% 
      group_by(freq_kegiatan, jk) %>% 
      filter(jk %in% input$sosial) %>%
      summarise(total = n()) %>% 
      ungroup() %>% 
      mutate(label = paste("Frekuensi:", freq_kegiatan, input$sosial, "\nTotal:", total, "orang"))
    
    p4 <- ggplot(keg, aes(y = freq_kegiatan, x = total, text = label)) +
      geom_bar(stat = "identity", position = "stack", aes(fill = jk),show.legend = FALSE) +
      labs(title = paste("Aktif Kegiatan Responden", input$sosial),
           x = "Total",
           y = "Frekuensi") +
      theme_gray() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggplotly(p4, tooltip = "text")
  })
  
  output$duk <- renderPlotly({
    sikd %>% 
      group_by(jk, dukungan_sosial) %>% 
      filter(jk %in% input$sosial) %>%
      summarise(total = n(), persen = total / nrow(sikd) * 100) %>% 
      ungroup() %>%  
      mutate(label = paste("Dukungan :", dukungan_sosial, input$sosial, "\nTotal:",persen, "%")) %>% 
      ggplot(aes(x = dukungan_sosial, y = total)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = input$sosial, text = label),show.legend = FALSE) +
      labs(title = "Dukungan Sosial",
           x = "Adanya dukungan sosial",
           y = "Total") +
      theme_gray() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggplotly(tooltip = "text")
  })
  
  output$infr <- renderPlotly({
    inf <- sikd %>% 
      group_by(infrastruktur_jln) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang")) 
    
    ggplot(inf, aes(x = infrastruktur_jln, y = total, text = label, fill = infrastruktur_jln)) +
      geom_bar(stat = "identity") +
      labs(title = "Kondisi Jalan",
           x = "Kendaraan",
           y = "Total") +
      theme_gray() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) 
    
    ggplotly(tooltip = "text")
  })
  
  output$jar <- renderPlotly({
    ggplot(sikd, aes(x = jarak_pusat_kota)) +
      geom_histogram(binwidth = 5, fill = "cornflowerblue", color = "black", alpha = 0.7) +
      labs(title = "Jarak ke Pusat Kota Terdekat (km)",
           subtitle = "Pemahaman responden jarak menuju Desa Wanadadi, Kota Banjarnegara, Tapen, dan sebagainya",
           x = "Jarak",
           y = "Frekuensi") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$trns <- renderPlotly({
    trans <- sikd %>% group_by(transport_umum) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"))
    
    ggplot(trans, aes(x = transport_umum, y = total)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = transport_umum, text = label)) +
      labs(title = "Transportasi Umum",
           subtitle = "Kemudahan masyarakat dalam mengakses transportasi umum",
           x = "Transportasi Umum",
           y = "Total") +
      theme_gray() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(tooltip = "text")
  })
  
  output$fasil <- renderPlotly({
    fas <- sikd %>% 
      group_by(akses_fasil) %>% 
      summarise(total = n()) %>% 
      ungroup() %>%
      mutate(akses_fasil = ifelse(akses_fasil == is.na(akses_fasil), "jalan kaki", akses_fasil),
             label = glue("Total : {total} orang")) 
    
      ggplot(fas, aes(x = akses_fasil, y = total, text = label, fill = akses_fasil)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Kendaraan Pribadi",
           subtitle = "Kendaraan masyarakat mengakses fasilitas umum",
           x = "Kendaraan",
           y = "Total") +
      theme_gray() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) 
    
    ggplotly(tooltip = "text")
  })
  
  output$gabung <- renderWordcloud2({
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
    
    # Create word cloud
    wordcloud2(data = data.frame(word = names(word_freq), freq = word_freq))
  })
  
  output$th1 <- renderPlotly({
    pd <- mdl %>% 
      group_by(tahu_pendanaan) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang")) %>% 
      ggplot(aes(x = tahu_pendanaan, y = total, fill = tahu_pendanaan)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
      ggplotly(tooltip = "text")
    
    pd_d <- mdl %>% 
      group_by(tahu_dana_desa) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% 
      ggplot(aes(x = tahu_dana_desa, y = total, fill = tahu_dana_desa)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c <- mdl %>% 
      group_by(tahu_dana_csr) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% 
      ggplot(aes(x = tahu_dana_csr, y = total, fill = tahu_dana_csr)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    subplot(pd, pd_d, pd_c, nrows = 1)
    
  })
  
  
  output$p1 <- renderPlotly({
    pd_d_p <- mdl %>% 
      group_by(dana_desa_utk_pembangunan) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_utk_pembangunan = ifelse(is.na(dana_desa_utk_pembangunan), "Tidak Mengisi", dana_desa_utk_pembangunan)) %>% 
      ggplot(aes(x = dana_desa_utk_pembangunan, y = total, text = label, fill = dana_desa_utk_pembangunan)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_i <- mdl %>% 
      group_by(dana_desa_utk_infrastruktur) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_utk_infrastruktur = ifelse(is.na(dana_desa_utk_infrastruktur), "Tidak Mengisi", dana_desa_utk_infrastruktur)) %>% 
      ggplot(aes(x = dana_desa_utk_infrastruktur, y = total, text = label, fill = dana_desa_utk_infrastruktur)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_b <- mdl %>% 
      group_by(dana_desa_utk_bumdes) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_utk_bumdes = ifelse(is.na(dana_desa_utk_bumdes), "Tidak Mengisi", dana_desa_utk_bumdes)) %>% 
      ggplot(aes(x = dana_desa_utk_bumdes, y = total, text = label, fill = dana_desa_utk_bumdes)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_e <- mdl %>% 
      group_by(dana_desa_ekonomi_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_ekonomi_rakyat = ifelse(is.na(dana_desa_ekonomi_rakyat), "Tidak Mengisi", dana_desa_ekonomi_rakyat)) %>% 
      ggplot(aes(x = dana_desa_ekonomi_rakyat, y = total, text = label, fill = dana_desa_ekonomi_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_pr <- mdl %>% 
      group_by(dana_desa_penghasilan_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_penghasilan_rakyat = ifelse(is.na(dana_desa_penghasilan_rakyat), "Tidak Mengisi", dana_desa_penghasilan_rakyat)) %>% 
      ggplot(aes(x = dana_desa_penghasilan_rakyat, y = total, text = label, fill = dana_desa_penghasilan_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_m <- mdl %>% 
      group_by(dana_desa_modal_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_modal_rakyat = ifelse(is.na(dana_desa_modal_rakyat), "Tidak Mengisi", dana_desa_modal_rakyat)) %>% 
      ggplot(aes(x = dana_desa_modal_rakyat, y = total, text = label, fill = dana_desa_modal_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_d_w <- mdl %>% 
      group_by(dana_desa_utk_wisata) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_desa_utk_wisata = ifelse(is.na(dana_desa_utk_wisata), "Tidak Mengisi", dana_desa_utk_wisata)) %>% 
      ggplot(aes(x = dana_desa_utk_wisata, y = total, text = label, fill = dana_desa_utk_wisata)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan Desa",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
      subplot(pd_d_p, pd_d_i, pd_d_b, pd_d_e, pd_d_pr, pd_d_m, pd_d_w, nrows = 4)
  })
  
  output$p2 <- renderPlotly({
    pd_c_p <- mdl %>% 
      group_by(dana_csr_utk_pembangunan) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_utk_pembangunan = ifelse(is.na(dana_csr_utk_pembangunan), "Tidak Mengisi", dana_csr_utk_pembangunan)) %>% 
      ggplot(aes(x = dana_csr_utk_pembangunan, y = total, text = label, fill = dana_csr_utk_pembangunan)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_i <- mdl %>% 
      group_by(dana_csr_utk_infrastruktur) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_utk_infrastruktur = ifelse(is.na(dana_csr_utk_infrastruktur), "Tidak Mengisi", dana_csr_utk_infrastruktur)) %>% 
      ggplot(aes(x = dana_csr_utk_infrastruktur, y = total, text = label, fill = dana_csr_utk_infrastruktur)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_b <- mdl %>% 
      group_by(dana_csr_utk_bumdes) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_utk_bumdes = ifelse(is.na(dana_csr_utk_bumdes), "Tidak Mengisi", dana_csr_utk_bumdes)) %>% 
      ggplot(aes(x = dana_csr_utk_bumdes, y = total, text = label, fill = dana_csr_utk_bumdes)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_e <- mdl %>% 
      group_by(dana_csr_ekonomi_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_ekonomi_rakyat = ifelse(is.na(dana_csr_ekonomi_rakyat), "Tidak Mengisi", dana_csr_ekonomi_rakyat)) %>% 
      ggplot(aes(x = dana_csr_ekonomi_rakyat, y = total, text = label, fill = dana_csr_ekonomi_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_pr <- mdl %>% 
      group_by(dana_csr_penghasilan_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_penghasilan_rakyat = ifelse(is.na(dana_csr_penghasilan_rakyat), "Tidak Mengisi", dana_csr_penghasilan_rakyat)) %>% 
      ggplot(aes(x = dana_csr_penghasilan_rakyat, y = total, text = label, fill = dana_csr_penghasilan_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_m <- mdl %>% 
      group_by(dana_csr_modal_rakyat) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_modal_rakyat = ifelse(is.na(dana_csr_modal_rakyat), "Tidak Mengisi", dana_csr_modal_rakyat)) %>% 
      ggplot(aes(x = dana_csr_modal_rakyat, y = total, text = label, fill = dana_csr_modal_rakyat)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    pd_c_w <- mdl %>% 
      group_by(dana_csr_utk_wisata) %>% 
      summarise(total = n()) %>% 
      ungroup() %>% mutate(label = glue("Total : {total} orang"),
                           dana_csr_utk_wisata = ifelse(is.na(dana_csr_utk_wisata), "Tidak Mengisi", dana_csr_utk_wisata)) %>% 
      ggplot(aes(x = dana_csr_utk_wisata, y = total, text = label, fill = dana_csr_utk_wisata)) +
      geom_bar(stat = "identity") +
      labs(title = "Pendanaan CSR",
           x = "",
           y = "Total") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = FALSE)
    
    subplot(pd_c_p,pd_c_i,pd_c_b,pd_c_e,pd_c_pr,pd_c_m,pd_c_w, nrows = 4)
  })
  
  output$perta <- DT::renderDataTable({
    pert <- c("Pala","Cabai rawit","Cabai merah","Buncis","Singkong","Jagung","Talas","Terong","Labu Siam")
    pert_ung <- c("Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan")
    bid1 <- c("Pertanian","Pertanian","Pertanian","Pertanian","Pertanian","Pertanian","Pertanian","Pertanian","Pertanian")
    
    perk <- c("Kopi","Kapulaga","Kelapa","Alpukat","Durian","Salak","Pisang Cavendish","Vanili")
    perk_ung <- c("Unggulan","Unggulan","Unggulan","Bukan Unggulan","Bukan Unggulan","Unggulan","Bukan Unggulan","Bukan Unggulan")
    bid2 <- c("Perkebunan","Perkebunan","Perkebunan","Perkebunan","Perkebunan","Perkebunan","Perkebunan","Perkebunan")
    
    peri <- c("Lele Jumbo","Mujair","Bawal","Nila","Melem")
    peri_ung <- c("Unggulan","Bukan Unggulan","Unggulan","Bukan Unggulan","Bukan Unggulan")
    bid3 <- c("Perikanan","Perikanan","Perikanan","Perikanan","Perikanan")
    
    perh <- c("Sengon","Saman","Jati","Mahoni")
    perh_ung <- c("Unggulan","Bukan Unggulan","Bukan Unggulan","Bukan Unggulan")
    bid4 <- c("Perhutanan","Perhutanan","Perhutanan","Perhutanan")
    
    pete <- c("Sapi","Kambing","Domba","Ayam","Ayam Petelur","Entog","Bebek")
    pete_ung <- c("Unggulan","Unggulan","Unggulan","Bukan Unggulan","Unggulan","Bukan Unggulan","Bukan Unggulan")
    bid5 <- c("Peternakan","Peternakan","Peternakan","Peternakan","Peternakan","Peternakan","Peternakan")
    
    pari <- "Curug Muncar"
    pari_ung <- "Unggulan"
    bid6 <- "Pariwisata"
    
    pertanian <- data.frame(pert, bid1, pert_ung)
    colnames(pertanian) <- c("Nama Potensi","Bidang","Keterangan")
    
    perkebunan <- data.frame(perk, bid2, perk_ung)
    colnames(perkebunan) <- c("Nama Potensi","Bidang","Keterangan")
    
    perikanan <- data.frame(peri, bid3, peri_ung)
    colnames(perikanan) <- c("Nama Potensi","Bidang","Keterangan")
    
    perhutanan <- data.frame(perh, bid4, perh_ung)
    colnames(perhutanan) <- c("Nama Potensi","Bidang","Keterangan")
    
    peternakan <- data.frame(pete, bid5, pete_ung)
    colnames(peternakan) <- c("Nama Potensi","Bidang","Keterangan")
    
    pariwisata <- data.frame(pari, bid6, pari_ung)
    colnames(pariwisata) <- c("Nama Potensi","Bidang","Keterangan")
    
    potensi <- rbind(pariwisata, peternakan, pertanian, perkebunan, perikanan, perhutanan)
    
    datatable(potensi, options = list(pageLength = 10), colnames = colnames(potensi)) 
  })
}
