library(tidyverse)
library(stringr)

# Contoh dataframe dengan kolom Tujuan yang berisi multiple records
df <- data.frame(
  Asal = c("REZKY MURWANTO, S.Kom., MPH. - Kemendukbangga/BKKBN"),
  Tujuan = c("UTAMASUDAH BACADra MARIA ERNAWATI, MM. - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Jawa TimurSUDAH BACADrs. PUTUT RIYATNO, M.Kes - Kepala Perwakilan BKKBN Provinsi JambiSUDAH BACAdr. VICTOR PALIMBONG, M.K.M - Kepala Perwakilan BKKBN Provinsi Maluku UtaraSUDAH BACAdr. JEANNY YOLA WINOKAN, MAP - Kepala Perwakilan Kemendukbangga/BKKBN Provinsi Sulawesi UtaraSUDAH BACAREZKY MURWANTO, S.Kom., MPH. - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sulawesi BaratSUDAH BACAShodiqin - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sulawesi SelatanSUDAH BACAAsmar - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sulawesi TenggaraSUDAH BACASARLES BRABAR, S.E., M.Si - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi PapuaSUDAH BACAMHD IRZAL, SE, ME - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi RiauSUDAH BACANURIZKY PERMANAJATI - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Kalimantan TimurBELUM BACADEDY AGUSTANTO, S.Kom, M.Pd.T. - Kemendukbangga/BKKBN, Sekretaris Perwakilan BKKBN Provinsi Sumatera BaratSUDAH BACAFATMAWATI - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sumatera UtaraSUDAH BACADr. dr. NI LUH GEDE SUKARDIASIH, M.For., M.A.R.S - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi BaliSUDAH BACAIr. Diano Tino Tandaju, S.T., M.Erg - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi GorontaloSUDAH BACAFaizal Fahmi - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Nusa Tenggara TimurSUDAH BACAZAMHARI, SH.,MH - Kepala Perwakilan BKKBN Provinsi BengkuluSUDAH BACADadi A Roswandi - Kepala Perwakilan BKKBN Provinsi Jawa BaratSUDAH BACAProf. Budi Setiyono, S.Sos., M.Pol.Admin., Ph.D - Sekretaris Kementerian / Sekretaris Utama (sesmen/sestama)SUDAH BACADr. Bonivasius Prasetya Ichtiarto, S.Si., M.Eng - Deputi Bidang Pengendalian PendudukSUDAH BACANOPIAN ANDUSTI, SE. MT - Deputi Bidang Keluarga Sejahtera dan Pemberdayaan KeluargaSUDAH BACADr. Drs. WAHIDIN, M.Kes - Deputi Bidang Bina Keluarga Berencana dan Kesehatan ReproduksiSUDAH BACADr. FAHARUDDIN, SST., M.Si - Kepala Pusat Data dan Teknologi InformasiSUDAH BACAAAN ARI WITOKO, S.E., M.Si - Kepala Pusat Pengembangan Sumber Daya Manusia Kependudukan, Pembangunan Keluarga, dan Keluarga BerencanaSUDAH BACAHERY WIYANTO - Plt. Kepala Perwakilan BKKBN Provinsi Jawa TengahSUDAH BACASoetriningsih, S.Sos., M.Si - Kepala Perwakilan BKKBN Provinsi LampungSUDAH BACAPhilmona Maria Yarollo, S.Sos, M.Si - Kepala Perwakilan BKKBN Provinsi Papua BaratSUDAH BACASAFRINA SALIM, SKM, M.Kes - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi AcehSUDAH BACADr. UKIK KUSUMA KURNIAWAN, SKM, MPS, MA - Deputi Bidang Kebijakan Strategi Pembangunan Keluarga, Pengendalian Penduduk dan Keluarga BerencanaSUDAH BACAMauliwaty Bulo - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi MalukuSUDAH BACADr. SUNARTO, S.K.M., M.Adm.KP. - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Kalimantan TengahSUDAH BACAARIOS SAPLIS - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sumatera SelatanSUDAH BACAIr RUSMAN EFENDI, MM - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi BantenSUDAH BACAMOHAMAD IQBAL APRIANSYAH, SH, M.P.H - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi DIYSUDAH BACALalu Makripuddin - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Nusa Tenggara BaratSUDAH BACADra. Maria Ernawati, MM. - Plt. Deputi Bidang Penggerakan dan Peran Serta MasyarakatSUDAH BACAROHINA, M.Si - Kepala Perwakilan BKKBN Provinsi Kepulauan RiauSUDAH BACANURYAMIN, S.TP., M.M. - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Kalimantan BaratSUDAH BACAFarah Adibah - Kepala Perwakilan BKKBN Provinsi Kalimantan SelatanSUDAH BACATenny Calvenny Soriton - Kemendukbangga/BKKBN, Kepala Perwakilan BKKBN Provinsi Sulawesi TengahSUDAH BACAFazar Supriadi Sentosa - Kepala Perwakilan BKKBN Provinsi Kepulauan Bangka Belitung"),
  Jenis = "DISPOSISI",
  stringsAsFactors = FALSE
)

## METODE 1: Memisahkan semua record dalam kolom Tujuan menjadi baris terpisah

# Fungsi untuk memproses teks dalam kolom Tujuan
proses_kolom_tujuan <- function(df, kolom_tujuan = "Tujuan") {
  
  df %>%
    mutate(
      # Tambahkan delimiter sebelum setiap SUDAH/BELUM BACA dan UTAMA
      row_id = row_number(),
      teks_processed = str_replace_all(.data[[kolom_tujuan]], 
                                       "(UTAMA|SUDAH BACA|BELUM BACA)", 
                                       "~\\1")
    ) %>%
    # Pisahkan menjadi baris berdasarkan delimiter
    separate_rows(teks_processed, sep = "~") %>%
    filter(teks_processed != "") %>%
    # Ekstrak keterangan (UTAMA/SUDAH/BELUM)
    mutate(
      keterangan = str_extract(teks_processed, "^(UTAMA|SUDAH BACA|BELUM BACA)"),
      sisa_teks = str_remove(teks_processed, "^(UTAMA|SUDAH BACA|BELUM BACA)")
    ) %>%
    # Pisahkan nama dan jabatan berdasarkan " - "
    separate(sisa_teks, into = c("nama", "jabatan"), sep = " - ", extra = "merge", fill = "right") %>%
    # Bersihkan data
    mutate(
      nama = str_trim(nama),
      jabatan = str_trim(jabatan),
      # Hapus "UTAMA" dari nama jika masih ada
      nama = str_remove(nama, "^UTAMA"),
      nama = str_trim(nama)
    ) %>%
    # Filter baris yang valid (punya jabatan)
    filter(!is.na(jabatan) & jabatan != "") %>%
    select(row_id, keterangan, nama, jabatan, everything(), -teks_processed)
}

# Terapkan fungsi
df_hasil <- proses_kolom_tujuan(df)

# Lihat hasil
print(df_hasil)
View(df_hasil)

## METODE 2: Dengan regex yang lebih presisi

proses_dengan_regex <- function(df, kolom_tujuan = "Tujuan") {
  
  # Pattern untuk mengekstrak: (KETERANGAN)(NAMA) - (JABATAN)
  pattern <- "(UTAMA|SUDAH BACA|BELUM BACA)([^-]+?)\\s*-\\s*([^~]+?)(?=UTAMA|SUDAH BACA|BELUM BACA|$)"
  
  df %>%
    mutate(row_id = row_number()) %>%
    # Terapkan regex ke setiap baris
    mutate(hasil = map(.data[[kolom_tujuan]], function(teks) {
      matches <- str_match_all(teks, pattern)
      if(length(matches[[1]]) > 0) {
        hasil_df <- as.data.frame(matches[[1]][, 2:4])
        colnames(hasil_df) <- c("keterangan", "nama", "jabatan")
        hasil_df %>%
          mutate(
            keterangan = str_trim(keterangan),
            nama = str_trim(nama),
            jabatan = str_trim(jabatan)
          )
      } else {
        NULL
      }
    })) %>%
    unnest(hasil, keep_empty = FALSE) %>%
    select(row_id, keterangan, nama, jabatan)
}

# Terapkan
df_hasil2 <- proses_dengan_regex(df)
print(df_hasil2)

## METODE 3: Jika ingin mempertahankan kolom asal lainnya

df_hasil_lengkap <- df %>%
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
          keterangan = keterangan,
          nama = str_trim(split_result[1]),
          jabatan = str_trim(split_result[2]),
          stringsAsFactors = FALSE
        )
      }
    })
  })) %>%
  unnest(hasil) %>%
  select(row_id, Asal, Jenis, keterangan, nama, jabatan)

# Lihat hasil
print(df_hasil_lengkap)

# Menghitung jumlah record
cat("Jumlah record sebelum diproses:", nrow(df), "baris\n")
cat("Jumlah record setelah diproses:", nrow(df_hasil_lengkap), "baris\n")

# Statistik berdasarkan keterangan
df_hasil_lengkap %>%
  count(keterangan) %>%
  print()

# Jika ingin menyimpan ke file CSV
# write.csv(df_hasil_lengkap, "data_tujuan_processed.csv", row.names = FALSE)