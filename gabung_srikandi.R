library(tidyverse)
gs4_deauth()

asn_perwakilan <- fst::read.fst("data/asn_perwakilan.fst")

# asn_perwakilan <- asn_perwakilan |>
#   mutate(`Nama Lengkap` = if_else(`Nama Lengkap` == "EDWIN BARA, S.Psi, M.A.P, M.Psi., Psikolog", "EDWIN BARA, S.Psi, M.A.P", `Nama Lengkap`))
# 
# 
# fst::write_fst(asn_perwakilan, "data/asn_perwakilan.fst")

data_anggota <- read_sheet(
  "https://docs.google.com/spreadsheets/d/10l2UvQvffWZ5EeM3K9JtfQMmS4i3f8T6VBDWcjrrPm0/edit?gid=1498096290#gid=1498096290",
  sheet = "Anggota" 
) 

data_ketua <- read_sheet(
  "https://docs.google.com/spreadsheets/d/10l2UvQvffWZ5EeM3K9JtfQMmS4i3f8T6VBDWcjrrPm0/edit?gid=1498096290#gid=1498096290",
  sheet = "Ketua" 
) 

data_srikandi <- readxl::read_excel("data/data_srikandi.xlsx") |>
  filter(Pengirim %in% data_anggota$Nama)

penyelesaian_disposisi <- data_srikandi |>
  filter(Jenis == "DISPOSISI SELESAI") |>
  select(-c(...1)) |>
  rename(Nama = "Pengirim") |>
  distinct()

penyelesaian_disposisi_ketua  <- data_srikandi |>
  filter(Jenis != "DISPOSISI SELESAI",
         Pengirim %in% data_ketua$Nama) |>
  mutate(row_id = row_number()) %>%
  # Proses setiap baris
  mutate(hasil = map(Tujuan, function(teks) {
    # Split berdasarkan UTAMA, SUDAH BACA, BELUM BACA
    parts <- str_split(teks, "(?=UTAMA|SUDAH BACA|BELUM BACA)")[[1]]
    parts <- parts[parts != ""]
    
    # Proses setiap bagian
    map_df(parts, function(part) {
      # Ekstrak keterangan
      keterangan <- str_extract(part, "^(UTAMA|SUDAH BACA|BELUM BACA)")
      sisa <- str_remove(part, "^(UTAMA|SUDAH BACA|BELUM BACA)")
      
      # Split nama dan jabatan
      if(str_detect(sisa, " - ")) {
        split_result <- str_split_fixed(sisa, " - ", 2)
        data.frame(
          Keterangan = keterangan,
          Penerima = str_trim(split_result[1]),
          `Jabatan.Penerima` = str_trim(split_result[2]),
          stringsAsFactors = FALSE
        )
      }
    })
  })) %>%
  unnest(hasil) %>%
  select(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, `Perihal Surat`, `Isi Ringkas`,
         Tanggal, Pengirim,
         Asal, Jenis, Keterangan, Penerima, `Jabatan.Penerima`) |>
  filter(Penerima %in% asn_perwakilan$`Nama Lengkap`)
  
penyelesaian_disposisi_ketua <- left_join(penyelesaian_disposisi_ketua, penyelesaian_disposisi |>
                                                select(`Nomor Naskah`, Nama, Jenis),
                                              by = c("Nomor Naskah", "Penerima" = "Nama")) |>
 # mutate(`TL Bawahan` = Jenis.y) |>
  group_by(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, `Perihal Surat`,
           `Isi Ringkas`, `Pengirim`) %>%
  summarize(
    `TL Bawahan` = if_else(
      any(Jenis.y == "DISPOSISI SELESAI", na.rm = TRUE), 
      "DISPOSISI SELESAI", 
      "DISPOSISI SELESAI"
    ),
    .groups = "drop"
  )

# penyelesaian_disposisi_ketua <- penyelesaian_disposisi_ketua |>
#   select("Tanggal Naskah", "Asal Naskah", "Nomor Naskah", "Perihal Surat",
#          "Isi Ringkas", "Pengirim","Penerima", "TL Bawahan")
# 
# hasil_evaluasi <- penyelesaian_disposisi_ketua %>%
#   group_by(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, `Perihal Surat`,
#            `Isi Ringkas`, `Pengirim`) %>%
#   summarize(
#     Status_Tugas = if_else(
#       any(`TL Bawahan` == "DISPOSISI SELESAI", na.rm = TRUE), 
#       "Selesai", 
#       "Belum Selesai"
#     ),
#     .groups = "drop"
#   )

# Menampilkan hasil
# print(hasil_evaluasi)

disposisi <- data_srikandi |>
  filter(Jenis != "DISPOSISI SELESAI") |>
  mutate(row_id = row_number()) %>%
  # Proses setiap baris
  mutate(hasil = map(Tujuan, function(teks) {
    # Split berdasarkan UTAMA, SUDAH BACA, BELUM BACA
    parts <- str_split(teks, "(?=UTAMA|SUDAH BACA|BELUM BACA)")[[1]]
    parts <- parts[parts != ""]
    
    # Proses setiap bagian
    map_df(parts, function(part) {
      # Ekstrak keterangan
      keterangan <- str_extract(part, "^(UTAMA|SUDAH BACA|BELUM BACA)")
      sisa <- str_remove(part, "^(UTAMA|SUDAH BACA|BELUM BACA)")
      
      # Split nama dan jabatan
      if(str_detect(sisa, " - ")) {
        split_result <- str_split_fixed(sisa, " - ", 2)
        data.frame(
          Keterangan = keterangan,
          Nama = str_trim(split_result[1]),
          Jabatan = str_trim(split_result[2]),
          stringsAsFactors = FALSE
        )
      }
    })
  })) %>%
  unnest(hasil) %>%
  select(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, `Perihal Surat`, `Isi Ringkas`,
         Tanggal, Pengirim,
         Asal, Jenis, Keterangan, Nama, Jabatan) |>
  filter(Nama %in% asn_perwakilan$`Nama Lengkap`)

final_srikandi <- left_join(disposisi, penyelesaian_disposisi,
                            by = c("Tanggal Naskah", "Asal Naskah", "Nomor Naskah", "Perihal Surat",
                                   "Isi Ringkas", "Nama"), suffix = c(" Pendispo", " Penerima Dispo")) |>
  select(-c(Tujuan))



final_srikandi <- left_join(final_srikandi, penyelesaian_disposisi_ketua,
                            by = c("Tanggal Naskah", "Asal Naskah", "Nomor Naskah", "Perihal Surat",
                                   "Isi Ringkas", "Nama" = "Pengirim")) |>
  mutate(`Jenis Penerima Dispo` = coalesce(`Jenis Penerima Dispo`, `TL Bawahan`))

final_srikandi <- final_srikandi |>
  select(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, `Perihal Surat`, 
         `Isi Ringkas`, Nama, Keterangan, `Jenis Penerima Dispo`) |>
  distinct() |>
  rename(`Status Baca` = Keterangan, `Status Tindaklanjut` =  `Jenis Penerima Dispo`) |>
  mutate(`Status Tindaklanjut` = case_when(
    Nama == "REZKY MURWANTO, S.Kom., MPH." ~ "DISPOSISI SELESAI",
    Nama == "ASHARI RAMADHAN, S.Stat" ~ "DISPOSISI SELESAI",
    is.na(`Status Tindaklanjut`) ~ "DISPOSISI BELUM SELESAI",
    `Status Tindaklanjut` == "DISPOSISI SELESAI" ~ "DISPOSISI SELESAI",
    TRUE ~ "DISPOSISI BELUM SELESAI"
    )
  ) |>
  mutate(`Status Baca` = case_when(
    Nama == "ASHARI RAMADHAN, S.Stat" ~ "SUDAH BACA",
    `Status Baca` == "SUDAH BACA" ~ "SUDAH BACA",
    `Status Baca` == "BELUM BACA" ~ "BELUM BACA",
    TRUE ~ "DISPOSISI BELUM SELESAI"
  )
  )

df_final_srikandi <- final_srikandi |>
group_by(Nama) %>%
  summarise(
    `Jumlah Surat Masuk` = n(),
    `Jumlah Sudah Baca` = sum(`Status Baca` == "SUDAH BACA"),
    `Jumlah Belum Baca` = sum(`Status Baca` == "BELUM BACA"),
    `Jumlah Sudah Tindaklanjut` = sum(`Status Tindaklanjut` == "DISPOSISI SELESAI"), # Asumsi status selesai
    `Jumlah Belum Tindaklanjut` = sum(`Status Tindaklanjut` == "DISPOSISI BELUM SELESAI"), # Asumsi status selesai
    .groups = 'drop'
  ) %>%
  mutate(
    `Persen Baca` = round((`Jumlah Sudah Baca` / `Jumlah Surat Masuk`) * 100, 2),
   # `Persen Belum Baca` = paste0(round((`Jumlah Belum Baca` / `Jumlah Surat Masuk`) * 100), "%"),
    `Persen Tindaklanjut` = round((`Jumlah Sudah Tindaklanjut` / `Jumlah Surat Masuk`) * 100, 2),
  #  `Persen Belum Tindaklanjut` = paste0(100 - round((`Jumlah Sudah Tindaklanjut` / `Jumlah Surat Masuk`) * 100), "%")
  ) |>
  full_join(
    asn_perwakilan |>
      filter(`Jenis Pegawai` != "P3K") |>
      select(`NIP Baru`, `Nama Lengkap`),
    by = c("Nama" = "Nama Lengkap")
  ) |>
  relocate(`NIP Baru`, .after = 1) |>
  rename(NIP = `NIP Baru`) |>
  mutate(
    `Skor Aktivitas Srikandi` = round((`Jumlah Surat Masuk` * 0.1) + (`Jumlah Sudah Baca` * 0.3) + (`Jumlah Sudah Tindaklanjut` * 0.6) - (`Jumlah Belum Baca` * 0.15) - (`Jumlah Belum Tindaklanjut` * 0.3), 2) 
  )

df_final_srikandi <- df_final_srikandi |>
  filter(Nama != c("REZKY MURWANTO, S.Kom., MPH.", "SUKADAMAI LAMPUGO, S.Sos")) |>
  select(Nama, NIP, `Jumlah Surat Masuk`, 
         `Jumlah Sudah Baca`, `Jumlah Belum Baca`, `Persen Baca`, 
         `Jumlah Sudah Tindaklanjut`, `Jumlah Belum Tindaklanjut`, `Persen Tindaklanjut`, 
         `Skor Aktivitas Srikandi`)

fst::write_fst(df_final_srikandi, "data/srikandi_pegawai.fst")

###batas
# Cek duplikasi di tabel disposisi
disposisi_duplicate <- disposisi %>%
  group_by(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, 
           `Perihal Surat`, `Isi Ringkas`, `Nama`) %>%
  summarise(jumlah = n(), .groups = 'drop') %>%
  filter(jumlah > 1)

print("Duplikasi di tabel disposisi:")
print(disposisi_duplicate)

# Cek duplikasi di tabel penyelesaian_disposisi
penyelesaian_duplicate <- penyelesaian_disposisi %>%
  group_by(`Tanggal Naskah`, `Asal Naskah`, `Nomor Naskah`, 
           `Perihal Surat`, `Isi Ringkas`, `Nama`) %>%
  summarise(jumlah = n(), .groups = 'drop') %>%
  filter(jumlah > 1)

print("Duplikasi di tabel penyelesaian_disposisi:")
print(penyelesaian_duplicate)

