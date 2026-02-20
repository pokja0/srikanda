library(bs4Dash)
library(dplyr)
library(fst)
library(reactable)
library(googlesheets4)
library(waiter)
library(bslib)
library(bsicons)
library(writexl) 
library(lubridate)
library(reactablefmtr)
library(tidyr)
library(bslib)
library(echarts4r)
library(fresh)

tema <- create_theme(
  bs4dash_status(light = "#272c30"),
  
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#bec5cb",
    navbar_light_hover_color = "#bec5cb"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_sidebar_light(
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#4682B4", danger = "#B22222", light = "#272c30"
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dashboardHeader(
    title = "Permintaan Data & Zoom"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = "Daftar Pemintaan Data",
        tabName = "tab1"
      )#,
      # menuItem(
      #   text = "Daftar Zoom",
      #   tabName = "tab2"
      # )
    )
  ),
  dashboardBody(
    use_theme(tema),
    tabItems(
      tabItem(
        tabName = "tab1",
        # Boxes need to be put in a row (or column)
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
        tabsetPanel(
          tabPanel(
            "Tabel",
            br(),
            downloadButton("download_excel_pd", "Download Excel"),
            card(
              reactableOutput("tabel_permintaan_data")
            )
          ),
          tabPanel(
            "Grafik",
            card(full_screen = T,
                 echarts4rOutput("grafik_pd")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab2",
        h2("Status Pengiriman Bukti Dukung SPT25 - PKB/PLKB", style="text-align: center;"),
        br(),
        fluidRow(
          column(
            3,
            value_box( 
              title = "Jumlah ASN PKB/PLKB", 
              "413",
              showcase = bs_icon("people"),
              theme = "bg-gradient-indigo-purple",
              p("Sumber: SIMSDM 30 Januari 2026")
            )
          ),
          column(
            3,
            value_box( 
              title = "Sudah Mengirim", 
              textOutput("pkb_sudah"),  
              showcase = bs_icon("emoji-heart-eyes"),
              theme = "bg-gradient-indigo-purple" ,
              
            )
          ),
          column(
            3,
            value_box( 
              title = "Belum Mengirim", 
              textOutput("pkb_belum"),  
              showcase = bs_icon("emoji-tear"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            3,
            value_box(
              title = "Sisa Hari Hingga Batas Waktu",
              value = textOutput("sisa_hari_pkb"),
              showcase = bs_icon("calendar-week"),
              theme = "primary",
              p("Batas akhir: 28 Februari 2026"),
              p("Nota Dinas Kepala Perwakilan")
            )
          )
        ),
        br(),
        h2("text"),
        downloadButton("download_excel_penyuluh", "Download Excel"),
        card(
          reactableOutput("tabel_penyuluh")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$reload, {
    session$reload()
  })
  
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

# Run the application 
shinyApp(ui = ui, server = server)
