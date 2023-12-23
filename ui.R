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

ui <- fluidPage(
  dashboardPage(
    skin = "purple",
    dashboardHeader(
      title = "SIKD Tanjungtirta"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Informasi Umum", tabName = "info", icon = icon("info")),
        menuItem(text = "Identitas Responden", tabName = "idnt", icon = icon("person")),
        menuItem(text = "Ekonomi", tabName = "eko", icon = icon("money-bill-wave")),
        menuItem(text = "Sosial Masyarakat", tabName = "sos", icon = icon("people-arrows")),
        menuItem(text = "Infrastruktur", tabName = "inf", icon = icon("road")),
        menuItem("Pembangunan Desa",
                 tabName = "menu_2", icon = icon("archway"),
                 menuSubItem("Saran Masyarakat", tabName = "pem", icon = icon("comments")),
                 menuSubItem("Pendanaan", tabName = "pen", icon = icon("gem"))
        ),
        menuItem(text = "Potensi Desa", tabName = "pot", icon = icon("pagelines"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'info',
                fluidPage(
                  box(
                    width = 12,
                    h2(strong("Sistem Informasi Keterbukaan Desa Tanjung Tirta")),
                    HTML("<h6>Build and modified by <a href='https://github.com/rahmarania'>Rahma Fairuz Rania</a> J0303201065</h6>"),
                    br(),
                    p("Sistem Informasi Keterbukaan Desa merupakan sistem yang dapat memberikan informasi desa, dalam studi kasus ini sistem dibangun menggunakan Shiny RStudio. Sistem Informasi Keterbukaan Desa ini diharapkan dapat membantu pihak-pihak terkait untuk pengambilan keputusan maupun sebagai monitoring informasi yang ada di desa Tanjungtirta. Data-data yang terdapat di sistem ini merupakan hasil dari kegiatan pengumpulan data melalui kuesioner, observasi, dan wawancara yang dilakukan selama dua bulan (September sampai Oktober) tahun 2023 di desa Tanjungtirta, Punggelan, Jawa Tengah. Apabila ada data yang kurang atau perlu untuk dikoreksi, hubungi rahfairuzran@gmail.com sebagai bentuk evaluasi dalam penyusunan Sistem Informasi Keterbukaan Desa yang lebih baik.", align = "justify"),
                    hr(),
                    h4(strong("Sejarah Desa")),
                    p("Desa Tanjung Tirta merupakan desa di Kecamatan Punggelan, Kabupaten Banjarnegara, Provinsi Jawa Tengah. Desa ini berbatasan dengan desa Tlaga, desa Bondolharjo, desa Petuguran, dan sungai Pekacangan. Desa Tanjung Tirta merupakan hasil dari re-grouping dua desa, yaitu Siwaru dan Bengkat. desa Siwaru terdiri dari desa Siwaru, Siwalan, dan Menggora. Sedangkan desa Bengkat terdiri dari desa Dogleg, Bengkat A, dan Bengkat B. Tujuan dari re-grouping adalah untuk efisiensi wilayah desa karena jumlah penduduknya yang masih sedikit.", align = "justify"),
                    br(),
                    h4(strong("Sekilas tentang desa")),
                    DT::dataTableOutput('dt')
                  )
                )
        ),
        tabItem(tabName = "idnt",
                fluidPage(
                  box(
                    width = 12,
                    sliderInput(inputId = "umur", 
                                label = "Range umur", 
                                min = min(usia_$usia), 
                                max = max(usia_$usia),
                                value = c(min(sikd$usia), max(sikd$usia)),
                                step = 1,
                                animate = TRUE),
                    plotlyOutput("ages"),
                  )
                ),
                fluidPage(
                  box(
                    width = 12,
                    plotlyOutput("educ")
                  )
                ),
                fluidPage(
                  box(
                    width = 12,
                    valueBox(
                      width = 4,
                      value = paste0(round(max(main_job$persen)), "%"), 
                      subtitle = "Pekerjaan utama sebagai Pedagang",
                      icon = icon("briefcase"),
                      color = 'green'
                    ),   
                    valueBox(
                      width = 4,
                      value = paste0(round(side_job$persen[5]), "%"), 
                      subtitle = "Pekerjaan sampingan responden Petani",
                      icon = icon("briefcase"),
                      color = 'blue'
                    ),   
                    valueBox(
                      width = 4,
                      value = paste0(round(side_job$persen[6]), "%"), 
                      subtitle = "Pekerjaan sampingan responden Peternak",
                      icon = icon("briefcase"),
                      color = 'purple'
                    ),   
                  ),
                  box(
                    width = 6,
                    plotOutput("job_m")
                  ),
                  box(
                    width = 6,
                    plotOutput("job_s")
                  )
                )
        ),
        tabItem(tabName = 'eko',
                fluidRow(
                  valueBox(
                    width = 4,
                    value = n_distinct(sikd$pendapatan),
                    subtitle = "Total Kategori Pendapatan",
                    icon = icon("line-chart"),
                    color = 'teal'
                  ),                  
                  valueBox(
                    width = 4,
                    value = nrow(sikd),
                    subtitle = "Sampel Responden",
                    icon = icon("user"),
                    color = 'yellow',
                  ),
                  valueBox(
                    width = 4,
                    value = paste0(max(eco$persen), "% menilai"), 
                    subtitle = "Tidak ada dan tidak terbukanya lapangan pekerjaan",
                    icon = icon("poll"),
                    color = 'red'
                  ),   
                ),
                fluidPage(
                  box(
                    width = 12,
                    plotlyOutput("inc")
                  ),
                  box(
                    width = 12,
                    checkboxGroupInput(inputId = "econom",
                                       label = "Apakah ada peluang ekonomi di desa?", 
                                       choices = unique(sikd$peluang_ekonomi)),
                    plotlyOutput("ec")
                  )
                )
        ),
        tabItem(tabName = 'sos',
                fluidPage(
                  box(
                    width = 12,
                    valueBox(
                      width = 5,
                      value = gender_sum[1,2], 
                      subtitle = "Responden Laki-laki",
                      icon = icon("male"),
                      color = 'orange'
                    ), 
                    valueBox(
                      width = 5,
                      value = gender_sum[2,2], 
                      subtitle = "Responden Perempuan",
                      icon = icon("female"),
                      color = 'teal'
                    ), 
                    radioButtons(inputId = "sosial",
                                 label = "Jenis Kelamin", 
                                 choices = unique(sikd$jk)),
                  ),
                  box(
                    width = 12,
                    br(),
                    plotlyOutput("akt"),
                    br(),
                    plotlyOutput("duk")
                  )
                )
        ),
        tabItem(tabName = "inf",
                fluidRow(
                  box(width = 6,
                      plotlyOutput("infr")
                  ),
                  box(
                    width = 6,
                    plotlyOutput("jar")
                  )
                ),
                fluidRow(
                  box(
                    width = 4,
                    imageOutput("peta"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
                  ),
                  box(
                    width = 8,
                    plotlyOutput("trns")
                  ),
                  box(
                    width = 8,
                    valueBox(
                      width = 6,
                      value = paste0(max(trans$total)," Orang"), 
                      subtitle = "Menilai tidak ada transportasi umum, dibuktikan dengan observasi tim secara langung. Jumlah transportasi umum seperti angkot, ojek pangkalan, maupun ojek online sangat sedikit bahkan tidak ada.",
                      icon = icon("bus"),
                      color = 'teal'
                    ), 
                    valueBox(
                      width = 6,
                      value = paste0(fas$total[4]," Orang"), 
                      subtitle = "Memiliki kendaraan pribadi sepeda motor untuk transportasi di desa. Jarak menuju fasilitas di desa cukup jauh, kendaraan utama masyarakat mayoritas sepeda motor",
                      icon = icon("motorcycle"),
                      color = 'blue'
                    )
                  ),
                  box(
                    width = 12,
                    plotlyOutput("fasil")
                  )
                )
            ),
          tabItem(tabName = "pem",
                  fluidPage(
                    box(
                      width = 12,
                      br(),
                      br(),
                      wordcloud2Output("gabung"),
                      br(),
                      br(),
                      p("Beberapa hal yang dapat disimpulkan dari grafik wordcloud di atas sebagai bahan pertimbangan pengambilan keputusan lanjutan"),
                      p("• Bantuan dalam bidang pertanian (alat seperti traktor dan sebagainya), peternakan, perekonomian, dan usaha masyarakat seperti bantuan modal"),
                      p("• Peningkatan Sumber Daya Manusia, jangan terlalu memanjakan masyarakat dengan bantuan seperti bantuan PKH"),
                      p("• Peningkatan lapangan kerja, dapat dilakukan pelatihan terlebih dahulu untuk meningkatkan kemampuan masyarakat"),
                      p("• Bantuan penerangan lampu jalan, karena di Desa Tanjungtirta apabila menjelang malam, beberapa titik lokasi sangat gelap")
                    )
                  )
          ),
        tabItem(tabName = "pen",
                fluidPage(
                  box(
                    width = 12,
                    plotlyOutput("th1"),
                    br(),
                    br(),
                    plotlyOutput("p1"),
                    br(),
                    br(),
                    plotlyOutput("p2"),
                    br(),
                    br(),
                    p("Perlunya informasi transparansi lebih lanjut terkait pendanaan Desa maupun CSR. Responden yang tidak mengisi, terdapat kemungkinan tidak sepenuhnya paham terhadap pendanaan dalam aspek pembangunan desa.", align = "justify")
                  )
                )
        ),
        tabItem(tabName = "pot",
                fluidPage(
                  box(
                    width = 12,
                    h4(strong("Potensi di Desa Tanjungtirta"), align = "center"),
                    br(),
                    DT::dataTableOutput('perta'),
                    br(),
                    p("Curug Muncar menjadi satu-satunya objek pariwisata di desa Tanjungtirta namun akses menuju curug sangat sulit dan berbahaya. Peternakan menjadi salah satu potensi yang cukup banyak ditekuni masyarakat. Kambing jawa dan kambing peranakan etawa menjadi potensi yang dijaga sampai saat ini. Masyarakat memiliki lahan yang ditanam berbagai jenis tanaman, sehingga sedikit jenis tanaman yang menjadi unggulan. Salak, kapulaga, dan kelapa merupakan tanaman yang cukup banyak dibudidayakan di desa. Tanaman sengon yang terdapat di desa ini memiliki masalah hama yang belum ditemukan solusinya.", align = "justify")
                  )
                )
        )
      )
    )
  )
)
