# app.R
library(shiny)
library(bslib)
library(dplyr)
library(fst)
library(reactable)
library(scales)
library(reactablefmtr)
library(htmltools)
library(echarts4r)
library(bsicons)
library(googlesheets4)
library(lubridate)
library(writexl) 
library(tidyr)

srikandi_surat <- read.fst("data/srikandi_surat.fst")
srikandi_pegawai <- read.fst("data/srikandi_pegawai.fst")

# Tema kustom (opsional)
my_theme <- bs_theme(
  bootswatch = "cosmo",  # bisa diganti: "darkly", "cosmo", dll.
)


ui <- page_navbar(
  fillable = F,
  title = "Dashboard Rekap Srikandi",
  theme = my_theme,
  
  # Menu pertama
  nav_panel(
    title = "Menu Pegawai",
    layout_columns(
      #col_widths = c(2, 2, 2),  # tiga kolom sama lebar
      value_box(
        title = "Jumlah Surat Terdisposisi",
        value = textOutput("jumlah_surat_terdisposisi"),
        showcase = bsicons::bs_icon("envelope-at"),
        theme = "primary"
      ),
      value_box(
        title = "Pengumpulan Data",
        value = textOutput("tanggal_scraping"),
        showcase = bsicons::bs_icon("calendar3"),
        theme = value_box_theme(bg = "#f4de79", fg = "black")
      ),
      value_box(
        title = "% Pegawai Membaca Surat",
        value = textOutput("surat_terbaca"),
        showcase = bsicons::bs_icon("envelope-paper"),
        theme = "primary"
      ),
      value_box(
        title = "% Pegawai Menindaklanjuti",
        value = textOutput("surat_ditl"),
        showcase = bsicons::bs_icon("envelope-check"),
        theme = value_box_theme(bg = "#f4de79", fg = "black")
      )
    ),
    # Tambahan konten bisa diletakkan di sini
    navset_card_underline(
      full_screen = F,
      title = "Klik ✔ ",
      nav_panel(
        "Tabel",
        # card_title("Tabel Rekap Aktivitas Srikandi Pegawai"),
        h1("Tabel Rekap Aktivitas Srikandi Pegawai", style="text-align: center;"),
        h6("Sumber Data: srikandi.arsip.go.id (diakses tanggal 28 Februari 2026)", style="text-align: center;"),
        layout_columns(
          col_widths = 2,
          downloadButton("download_excel_srikandi_pegawai", "Download Excel")
        ),
        reactableOutput("skor_srikandi_pegawai")
      ),
      nav_panel(
        "Grafik",
        layout_columns(
          card(full_screen = T,
               echarts4rOutput("histogram_baca")
          ),
          card(full_screen = T,
               echarts4rOutput("histogram_tl")
          )
        ),
        layout_columns(
          card(
            full_screen = T,
            echarts4rOutput("scatter_plot")
          )
        )
      ),
      nav_panel(
        "Penjelasan",
        titlePanel(
          h1("Metodologi Perhitungan Skor Aktivitas Srikandi", 
             style = "color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px;")
        ),
        
        # Container utama
        div(
          style = "max-width: 900px; margin: auto; padding: 20px;",
          
          # Box pendahuluan
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 5px solid #3498db;",
            h4("Pendahuluan", style = "color: #2c3e50; margin-top: 0;"),
            p("Untuk mengukur tingkat keaktifan dan ketepatan pengguna dalam mengelola surat di aplikasi Srikandi, 
        kami menggunakan sebuah indikator kuantitatif yang disebut ", 
              strong("Skor Aktivitas Srikandi"), 
              ". Skor ini memberikan gambaran komprehensif tentang perilaku pengguna, dengan memberikan bobot lebih 
        pada tindakan proaktif dan memberikan pengurangan (penalti) atas keterlambatan atau kelalaian.")
          ),
          
          br(),
          
          # Box rumus
          wellPanel(
            style = "background-color: #ebf5fb; border: 1px solid #3498db;",
            h4("Rumus Perhitungan", style = "color: #2c3e50; margin-top: 0;"),
            
            # Rumus dalam box terpisah
            div(
              style = "background-color: white; padding: 15px; border-radius: 5px; font-family: 'Courier New'; 
                 border: 1px solid #bdc3c7; text-align: center;",
              HTML("Skor Aktivitas Srikandi = <strong>(Jumlah Surat Masuk × 0.1) + (Jumlah Sudah Baca × 0.3) + 
             (Jumlah Sudah Tindaklanjut × 0.6) - (Jumlah Belum Baca × 0.15) - (Jumlah Belum Tindaklanjut × 0.3)</strong>")
            ),
            
            br(),
            p(em("Nilai akhir dari perhitungan ini kemudian akan dibulatkan hingga dua angka di belakang koma (desimal) 
            untuk memudahkan pembacaan dan perbandingan."), style = "font-size: 0.9em; color: #7f8c8d;")
          ),
          
          br(),
          
          # Box penjelasan komponen
          h4("Penjelasan Komponen dan Bobot", style = "color: #2c3e50;"),
          p("Setiap komponen dalam rumus ini dipilih untuk merepresentasikan tahapan penting dalam alur kerja surat. 
      Bobot yang diberikan mencerminkan seberapa penting suatu tindakan dalam menentukan skor akhir."),
          
          # Tabel dengan styling
          div(
            style = "margin: 20px 0; overflow-x: auto;",
            tableOutput("komponen_table")
          ),
          
          br(),
          
          # Box ringkasan
          wellPanel(
            style = "background-color: #e8f5e9; border-left: 5px solid #27ae60;",
            h4("Ringkasan", style = "color: #2c3e50; margin-top: 0;"),
            
            p(strong("Secara sederhana, Skor Aktivitas Srikandi adalah sebuah nilai yang dihitung dengan cara:")),
            
            tags$ul(
              tags$li(HTML("<strong>Menjumlahkan</strong> poin dari setiap surat yang berhasil diproses 
                (membaca dan menindaklanjuti), dengan bobot lebih besar pada proses 
                <strong>tindak lanjut</strong>.")),
              tags$li(HTML("<strong>Mengurangkan</strong> poin untuk setiap surat yang terbengkalai 
                (belum dibaca atau belum ditindaklanjuti), dengan pengurangan terbesar pada surat 
                yang sudah dibaca tapi <strong>tidak/belum ditindaklanjuti</strong>."))
            ),
            
            p("Dengan demikian, skor ini tidak hanya mengukur kuantitas, tetapi juga kualitas dan ketepatan waktu 
    dalam pengelolaan surat. Skor yang tinggi mencerminkan pengguna yang responsif, aktif, dan efektif 
    dalam menyelesaikan alur kerja surat mereka di Srikandi."),
            # Baris credit untuk Deepseek AI
            br(),
            p(em("Disusun oleh Deepseek AI"), 
              style = "text-align: left; font-size: 0.85em; color: #7f8c8d; border-top: 1px dashed #a5d6a7; padding-top: 10px; margin-bottom: 0;")
          )
        )
      )
    )
  ),
  
  # Menu kedua
  nav_panel(
    title = "Menu Permintaan Data",
    h2("Daftar Permintaan Data", style="text-align: center;"),
    br(),
    fluidRow(
      column(
        4,
        value_box( 
          title = "Jumlah Permintaan Data", 
          textOutput("jumlah_permintaan_data"),
          showcase = bs_icon("people-fill"),
          theme = "primary"
        )
      ),
      column(
        4,
        value_box( 
          title = "Telah Ditindaklanjuti", 
          textOutput("jumlah_tl_pm"),  
          showcase = bs_icon("emoji-heart-eyes-fill"),
          theme = "primary" 
        )
      ),
      column(
        4,
        value_box( 
          title = "Belum Ditindaklanjuti", 
          textOutput("jumlah_belum_tl_pm"),  
          showcase = bs_icon("emoji-tear-fill"),
          theme = "danger" 
        )
      )
    ),
    navset_card_underline(
      title = "Klik ✔ ",
      nav_panel(
        "Tabel",
        br(),
        layout_columns(
          col_widths = 2,
          downloadButton("download_excel_pd", "Download Excel")
        ),
        card(
          reactableOutput("tabel_permintaan_data")
        )
      ),
      nav_panel(
        "Grafik",
        card(full_screen = T,
             echarts4rOutput("grafik_pd")
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  
  # Data dummy (bisa diganti dengan data nyata)
  output$jumlah_surat_terdisposisi <- renderText({ 
    comma(
      length(unique(srikandi_surat$`Nomor Naskah`)),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  output$tanggal_scraping <- renderText({ "28 Februari 2026"})
  
  output$surat_terbaca <- renderText({ 
    paste0(comma(
      median(srikandi_pegawai$`Persen Baca`),
      big.mark = ".",
      decimal.mark = ","
    ), "%")
  })
  
  output$surat_ditl <- renderText({ 
    paste0(comma(
      median(srikandi_pegawai$`Persen Tindaklanjut`),
      big.mark = ".",
      decimal.mark = ","
    ), "%")
  })
  
  # Plot dummy
  output$skor_srikandi_pegawai <- renderReactable({
    reactable(
      srikandi_pegawai,
      defaultColDef = colDef(
        align = "center"
      ),
      columns = list(
        Nama = colDef(align = "left"),
        NIP = colDef(align = "left"),
        `Jumlah Sudah Baca` = colDef(name = "Sudah"),
        `Jumlah Belum Baca` = colDef(name = "Belum"),
        `Persen Baca` = colDef(name = "Persentase (%)", 
                               style = color_scales(srikandi_pegawai, colors = c("#F44336", "#4CAF50")), 
                               format = colFormat(suffix = "%")),
        #`Persen Belum Baca` = colDef(name = "% Belum", style = color_scales(srikandi_pegawai)),
        `Jumlah Sudah Tindaklanjut` = colDef(name = "Sudah"),
        `Jumlah Belum Tindaklanjut` = colDef(name = "Belum"),
        `Persen Tindaklanjut` = colDef(name = "Persentase (%)",
                                       style = color_scales(srikandi_pegawai, colors = c("#F44336", "#4CAF50")), 
                                       format = colFormat(suffix = "%"))
        #`Persen Belum Tindaklanjut` = colDef(name = "% Belum")
      ),
      columnGroups = list(
        colGroup(name = "Baca Surat", columns = c("Jumlah Sudah Baca", "Jumlah Belum Baca", "Persen Baca")),
        colGroup(name = "Tindaklanjut Surat", columns = c("Jumlah Sudah Tindaklanjut", "Jumlah Belum Tindaklanjut", "Persen Tindaklanjut"))
      ),
      bordered = TRUE, striped = TRUE, highlight = TRUE,searchable = T,
      theme = reactableTheme(
        highlightColor = "#99ccff"  # Biru muda untuk hover
      )
    ) 
  })
  
  output$download_excel_srikandi_pegawai <- downloadHandler(
    
    
    filename = function() {
      paste("skor-srikandi-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(srikandi_pegawai, file)
    }
  )
  
  # Membuat tabel komponen di server
  output$komponen_table <- renderTable({
    data.frame(
      Komponen = c("Jumlah Surat Masuk", "Jumlah Sudah Baca", "Jumlah Sudah Tindaklanjut", 
                   "Jumlah Belum Baca", "Jumlah Belum Tindaklanjut"),
      Bobot = c("+0,1", "+0,3", "+0,6", "-0,15", "-0,3"),
      Penjelasan = c(
        "Dasar perhitungan, merepresentasikan volume pekerjaan.",
        "Menunjukkan pengguna telah merespons dan mengetahui isi surat.",
        "Inti dari produktivitas - telah mengambil aksi nyata. Bobot tertinggi.",
        "Penalti untuk keterlambatan merespons.",
        "Penalti terbesar - menunjukkan hambatan dalam penyelesaian pekerjaan."
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lcc")
  
  output$histogram_baca <- renderEcharts4r({
    #df_final_srikandi = df_final_srikandi
    # Membuat histogram
    srikandi_pegawai |>
      e_charts() |>
      e_histogram(`Persen Baca`, name = "Jumlah Pegawai") |>
      e_title("Histogram Persen Baca", "") |>
      e_x_axis(name = "Persen Baca") |>
      e_y_axis(name = "Jumlah Pegawai") |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE)
  })
  
  output$histogram_tl <- renderEcharts4r({
    # Membuat histogram
    #df_final_srikandi = df_final_srikandi
    srikandi_pegawai |>
      e_charts() |>
      e_histogram(`Persen Tindaklanjut`, name = "Jumlah Pegawai") |>
      e_title("Histogram Persen Tindaklanjut", "") |>
      e_x_axis(name = "Persen Tindaklanjut") |>
      e_y_axis(name = "Jumlah Pegawai") |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE)
  })
  
  output$scatter_plot <- renderEcharts4r({
    median_surat = median(srikandi_pegawai$`Jumlah Surat Masuk`)
    median_skor <- median(srikandi_pegawai$`Skor Aktivitas Srikandi`)
    
    min_surat = min(srikandi_pegawai$`Jumlah Surat Masuk`) * 0.90
    min_skor <- min(srikandi_pegawai$`Skor Aktivitas Srikandi`) * 0.9
    
    max_surat = max(srikandi_pegawai$`Jumlah Surat Masuk`) * 1.1
    max_skor <- max(srikandi_pegawai$`Skor Aktivitas Srikandi`) * 1.1
    
    data_scatter = srikandi_pegawai |>
      select(Nama, `Jumlah Surat Masuk`, `Skor Aktivitas Srikandi`) |>
      mutate(Kuadran = case_when(
        `Jumlah Surat Masuk` >= median_surat & `Skor Aktivitas Srikandi` >= median_skor ~ "Kuadran I",
        `Jumlah Surat Masuk` < median_surat & `Skor Aktivitas Srikandi` >= median_skor ~ "Kuadran II",
        `Jumlah Surat Masuk` < median_surat & `Skor Aktivitas Srikandi` < median_skor ~ "Kuadran III",
        `Jumlah Surat Masuk` >= median_surat & `Skor Aktivitas Srikandi` < median_skor ~ "Kuadran IV"
      ))
    
    # Membuat scatter plot dengan garis di 50 dan warna per kuadran
    data_scatter |>
      echarts4r::group_by(Kuadran) %>% 
      e_charts(`Jumlah Surat Masuk`) |>
      e_scatter(
        serie = `Skor Aktivitas Srikandi`,
        symbol_size = 12,
        # name = "Data",
        bind = Nama  # Menambahkan bind agar nama muncul di tooltip
      ) |>
      e_color(
        c("#66BB6A", "#42A5F5", "#EF5350", "#FFA726")
      ) |>
      e_x_axis(
        name = "Surat Masuk",
        min = round(min_surat),
        max = round(max_surat),
        axisLine = list(onZero = FALSE), # Memaksa sumbu X tetap di bawah
        nameLocation = "middle",         # Mengatur posisi judul sumbu di tengah
        nameGap = 35                     # Memberi jarak antara judul dan angka sumbu
      ) |>
      e_y_axis(
        name = "Skor Aktivitas Srikandi",
        min = round(min_skor),
        max = round(max_skor),
        axisLine = list(onZero = FALSE), # Mencegah sumbu Y bergeser ke tengah jika ada X negatif
        nameGap = 10
      )|>
      # Menambahkan garis vertikal di 50
      e_mark_line(
        data = list(xAxis = median_surat),
        title = "",
        lineStyle = list(color = "gray", type = "dashed", width = 2)
      ) |>
      # Menambahkan garis horizontal di 50  
      e_mark_line(
        data = list(yAxis = median_skor),
        title = "",
        lineStyle = list(color = "gray", type = "dashed", width = 2)
      ) |>
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("
      function(params){
        return(
          '<b>' + params.name + '</b><br/>' + 
          'Surat Masuk: ' + params.value[0] + '<br/>' + 
          'Skor: ' + params.value[1]
        )
      }
    ")
      ) |>
      e_title("Grafik Perbandingan Jumlah Surat Masuk dan Skor Aktivitas Srikandi") |>
      e_legend(show = T) |>  # Legend dimatikan dulu karena grouping bermasalah
      e_grid(containLabel = TRUE, left = "15%", right = "10%")
    
  })
  
  #### permintaan data
  permintaan_data <- reactive({
    gs4_deauth()
    # Baca dengan spesifikasi tipe kolom
    permintaan_data <- read_sheet(
      "https://docs.google.com/spreadsheets/d/1YODn-MxkMwIyyBcGTw3MDA7QRp6o3dx06LgFkhZWqXA/edit?gid=1630425889#gid=1630425889",
      sheet = "PERMINTAAN DARI PUSAT 2026" 
    ) 
    
    # Vektor bulan Indonesia
    bulan_indonesia <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni",
                         "Juli", "Agustus", "September", "Oktober", "November", "Desember")
    
    # Ubah format
    hari_ini <- today()
    permintaan_data <- permintaan_data %>%
      mutate(
        selisih = interval(hari_ini, `TANGGAL DEADLINE`) %/% days(1),
        `HARI H DEADLINE` = case_when(
          selisih < 0 ~ paste0("Sudah lewat ", abs(selisih), " hari"),
          selisih == 0 ~ "H0",
          selisih > 0 ~ paste0("Sisa ", abs(selisih), " hari lagi")
        )
      ) %>%
      select(-selisih) |>  # hapus kolom selisih jika tidak diperlukan
      mutate(
        tanggal_date = ymd(`TANGGAL NASKAH`),
        `TANGGAL NASKAH` = paste(day(tanggal_date), 
                                 bulan_indonesia[month(tanggal_date)], 
                                 year(tanggal_date)),
        tanggal_date = ymd(`TANGGAL DEADLINE`),
        `TANGGAL DEADLINE` = paste(day(tanggal_date), 
                                   bulan_indonesia[month(tanggal_date)], 
                                   year(tanggal_date))
      ) %>%
      select(-c(tanggal_date, `LINK SURAT`, `JAM DEADLINE`)) |> # Hapus kolom bantuan jika tidak diperlukan
      relocate(`HARI H DEADLINE`, .after = 8) |> # Meletakkan kolom H di posisi ke-4 (setelah kolom ke-3)
      mutate(
        TINDAKLANJUT_COLS = case_when(TINDAKLANJUT == "SELESAI" ~ "#4682B4",
                                      TRUE ~ "#B22222")
      )
    
    return(permintaan_data)
  })
  
  output$jumlah_permintaan_data <- renderText({
    
    permintaan_data() %>%
      distinct(`NOMOR NASKAH`) %>%
      nrow()
  })
  
  output$jumlah_tl_pm <- renderText({
    
    permintaan_data() |>
      filter(TINDAKLANJUT == "SELESAI") |>
      distinct(`NOMOR NASKAH`) %>%
      nrow()
  })
  
  output$jumlah_belum_tl_pm <- renderText({
    
    permintaan_data() |>
      filter(TINDAKLANJUT != "SELESAI") |>
      distinct(`NOMOR NASKAH`) |>
      nrow()
  })
  
  output$tabel_permintaan_data <- renderReactable({
    
    permintaan_data = permintaan_data()
    reactable(permintaan_data, 
              filterable = TRUE, 
              striped = TRUE,
              columns = list(
                TINDAKLANJUT = colDef(
                  maxWidth = 130,
                  align = "center",
                  cell = pill_buttons(permintaan_data, color_ref = "TINDAKLANJUT_COLS", opacity = 0.7)
                ),
                TINDAKLANJUT_COLS = colDef(show = FALSE),
                NO = colDef(align = "center", maxWidth = 70),
                `ISI RINGKAS SURAT` = colDef(minWidth = 240)
              ),
              theme = fivethirtyeight(),
              defaultColDef = colDef(minWidth = 120, align = "left")
              ,
              # pagination = F,
              # virtual = TRUE,
              
              showPagination = TRUE#,
              # rowStyle = function(index) {
              #   if (permintaan_data[index, "TINDAKLANJUT"] == "ON PROSES") list(background = "rgb(255, 153, 153)")
              # }
    )
  })
  
  output$download_excel_pd <- downloadHandler(
    
    
    filename = function() {
      paste("data-permintaan-data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(permintaan_data(), file)
    }
  )
  
  output$grafik_pd <- renderEcharts4r({
    # Membuat vector berdasarkan nilai pada gambar
    vector_timker <- c(
      "Umum dan Pengelolaan BMN",
      "Perencanaan Manajemen Kinerja dan Keuangan",
      "Pengendalian Kependudukan dan Kebijakan Strategi",
      "ZI WBK dan SPIP",
      "Pengelolaan Kepegawaian dan Pembinaan Tenaga Lini Lapangan dan IMP",
      "Ketahanan Keluarga",
      "Akses Kualitas Layanan KBKR",
      "Pelaporan dan Statistik dan Pengelolaan TIK",
      "Humas Informasi Layanan Publik",
      "Peningkatan Kompetensi dan Pengembangan SDM Dukbangga",
      "Peran Serta Masyarakat dan Pendayaguna Mitra Kerja /LOM"
    )
    
    # Memisahkan baris berdasarkan koma
    permintaan_data = permintaan_data()
    bar_timker <- permintaan_data %>%
      separate_rows(`TIM KERJA TERKAIT`, sep = ",\\s*")
    
    bar_timker <- bar_timker |>
      dplyr::group_by(`TIM KERJA TERKAIT`, TINDAKLANJUT) |>
      summarise(JUMLAH = n()) |>
      dplyr::group_by(`TIM KERJA TERKAIT`) |>
      mutate(TOTAL_DISPO = sum(JUMLAH)) |>
      ungroup() |>
      dplyr::arrange(TOTAL_DISPO)
    
    
    tindaklanjut_level <- c("SELESAI", "BELUM SELESAI")
    kostum_warna <- c(
      "SELESAI" = "#4682B4",          # Hijau Muda
      "BELUM SELESAI" = "#B22222"  # Oranye
    )
    # Pilih warna yang relevan berdasarkan data yang difilter
    plot_colors <- kostum_warna[names(kostum_warna) %in% unique(bar_timker$TINDAKLANJUT)]
    
    bar_timker |>
      echarts4r::group_by(TINDAKLANJUT) |> 
      e_charts(`TIM KERJA TERKAIT`) |>
      
      e_bar(JUMLAH, stack = "stack", legend = TRUE) |>
      
      # Menerapkan Warna Kustom
      e_color(color = unname(plot_colors)) |>
      e_legend(
        bottom = 0, # Posisikan legend di bagian paling bawah
        orient = "horizontal", # Atur orientasi horizontal (default)
        padding = c(0, 0, 0, 0) # Atur padding jika perlu
      ) |>
      e_title("Status Tindak Lanjut Permintaan Data per Tim Kerja") |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords() |>
      e_theme("westeros")
    
  })
  
}

shinyApp(ui, server)