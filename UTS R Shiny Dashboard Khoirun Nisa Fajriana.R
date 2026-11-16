library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(corrplot)
library(broom)
library(scales)
library(car)      # Untuk uji VIF (Multikolinearitas)
library(lmtest)   # Untuk uji Breusch-Pagan (Heteroskedastisitas) & DW Test (Autokorelasi)

# ==============================================================================
# 1. PEMUATAN & PRA-PEMROSESAN DATA (DATA CLEANING)
# ==============================================================================
Data_UTS <- read_excel("D:/KULIAH ICO STAT/Semester 7/EDMV/UTS/Data Automobile.xlsx")
names(Data_UTS) <- gsub("-", "_", names(Data_UTS))

cat_vars <- c("make", "fuel_type", "aspiration", "num_of_doors", 
              "body_style", "drive_wheels", "engine_location", 
              "engine_type", "num_of_cylinders", "fuel_system")

num_vars <- c("price", "horsepower", "curb_weight", "engine_size", 
              "wheel_base", "length", "width", "height", 
              "city_mpg", "highway_mpg", "bore", "stroke", 
              "compression_ratio", "peak_rpm", "normalized_losses", "symboling")

auto_data <- Data_UTS %>%
  mutate(across(all_of(cat_vars), as.factor)) %>%
  mutate(across(all_of(num_vars), as.numeric))

# Dataset bersih tanpa NA pada variabel utama (195 observasi untuk regresi)
auto_clean <- auto_data %>%
  filter(!is.na(price), !is.na(horsepower), !is.na(bore), !is.na(stroke), !is.na(peak_rpm))

# Palet warna pastel lucu untuk grafik
cute_palette <- c("#FF7597", "#72EFDD", "#B892FF", "#FFD166", "#64DFDF", "#FF9F1C", "#80FFDB", "#F72585")

var_dictionary <- data.frame(
  Variabel = names(auto_data),
  Tipe = ifelse(names(auto_data) %in% num_vars, "Numerik", "Kategorik"),
  Missing_Values = colSums(is.na(auto_data)),
  Deskripsi = c(
    "Tingkat risiko asuransi (-3 = sangat aman, +3 = berisiko tinggi)",
    "Rata-rata pembayaran kerugian relatif per tahun (memiliki 41 NA)",
    "Merek / pabrikan mobil (22 merek)",
    "Jenis bahan bakar (gas / diesel)",
    "Sistem aspirasi mesin (std = standar, turbo)",
    "Jumlah pintu mobil (two / four, 2 NA)",
    "Tipe bodi mobil (sedan, hatchback, wagon, hardtop, convertible)",
    "Sistem penggerak roda (fwd, rwd, 4wd)",
    "Lokasi mesin (front / rear)",
    "Jarak sumbu roda dalam inci",
    "Panjang total mobil dalam inci",
    "Lebar total mobil dalam inci",
    "Tinggi total mobil dalam inci",
    "Berat kosong mobil tanpa penumpang/muatan (lbs)",
    "Tipe konfigurasi mesin (ohc, ohcf, ohcv, dohc, dll.)",
    "Jumlah silinder mesin (two, three, four, five, six, eight, twelve)",
    "Kapasitas volume mesin (cubic inches)",
    "Sistem injeksi bahan bakar (mpfi, 2bbl, idi, dll.)",
    "Diameter silinder mesin (inches)",
    "Panjang langkah piston (inches)",
    "Rasio kompresi mesin",
    "Tenaga kuda maksimum mesin (HP)",
    "Putaran mesin maksimum (RPM)",
    "Konsumsi bahan bakar di dalam kota (miles per gallon)",
    "Konsumsi bahan bakar di jalan tol (miles per gallon)",
    "Harga jual mobil dalam USD (Variabel Respons Utama, 4 NA)"
  )
)

# ==============================================================================
# 2. ANTARMUKA PENGGUNA (UI) - KHOIRUN NISA (UTS EVDM)
# ==============================================================================
cute_theme <- bs_theme(
  version = 5,
  bg = "#FFF9FB",           # Pink susu lembut
  fg = "#4A3B47",           # Cokelat keunguan lembut
  primary = "#FF6B9D",      # Strawberry pink
  secondary = "#A78BFA",    # Soft lavender
  info = "#4ECDC4",         # Mint toska
  success = "#52B788",
  warning = "#FFB703",
  base_font = font_google("Quicksand"),
  heading_font = font_google("Fredoka")
)

ui <- page_navbar(
  title = "🚗✨ UTS EVDM — Khoirun Nisa (Data Automobile)",
  theme = cute_theme,
  header = tags$head(
    tags$style(HTML("
      .card {
        border-radius: 18px !important;
        border: 2px solid #FFE0EC !important;
        box-shadow: 0 6px 16px rgba(255, 107, 157, 0.08) !important;
        background-color: #FFFFFF !important;
      }
      .card-header {
        background-color: #FFF0F6 !important;
        border-bottom: 2px dashed #FFD1E3 !important;
        font-weight: 700 !important;
        color: #D63384 !important;
        border-radius: 16px 16px 0 0 !important;
      }
      .interp-box {
        background-color: #F4FBF9;
        border-left: 5px solid #4ECDC4;
        padding: 14px 18px;
        border-radius: 12px;
        margin-top: 15px;
        color: #2D4A43;
        font-size: 14.5px;
      }
      .interp-box-pink {
        background-color: #FFF5F8;
        border-left: 5px solid #FF6B9D;
        padding: 14px 18px;
        border-radius: 12px;
        margin-top: 15px;
        color: #59253A;
        font-size: 14.5px;
      }
      .nav-pills .nav-link.active {
        background-color: #FF6B9D !important;
        color: white !important;
        border-radius: 12px !important;
      }
      .nav-pills .nav-link {
        color: #D63384 !important;
        font-weight: 600;
      }
    "))
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 1: PENDAHULUAN & DATASET
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🎀 1. Data & Latar Belakang",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("🌸 Latar Belakang & Tujuan Analisis"),
        card_body(
          h5("Permasalahan Nyata"),
          p("Harga jual kendaraan di pasar otomotif sangat dipengaruhi oleh spesifikasi teknis (seperti kapasitas mesin, tenaga kuda, dan dimensi fisik), tingkat efisiensi bahan bakar (MPG), serta segmen mereknya. Analisis ini disusun oleh Khoirun Nisa untuk mengidentifikasi faktor penentu harga secara objektif."),
          h5("Tujuan Utama"),
          tags$ul(
            tags$li("Mengeksplorasi distribusi, pemusatan data, dan pencilan (outlier) tiap variabel secara satu per satu."),
            tags$li("Menguji hubungan bivariat dan pola pengelompokan multivariat antar-variabel teknis secara terperinci."),
            tags$li("Membangun model Regresi Linear Berganda, memvisualisasikan estimasi koefisien, serta melakukan uji asumsi klasik.")
          )
        )
      ),
      card(
        card_header("🧁 Profil Dataset & Pembersihan Data"),
        card_body(
          p(strong("Penyusun:"), "Khoirun Nisa"),
          p(strong("Sumber Data:"), "UCI Machine Learning Repository — 1985 Ward's Automotive Yearbook (Data Automobile.xlsx)."),
          p(strong("Ukuran Data:"), "205 observasi mobil dengan 26 variabel (16 numerik & 10 kategorik)."),
          p(strong("Penanganan Missing Values (NA):"), 
            "Variabel ", code("normalized_losses"), " memiliki 41 nilai hilang sehingga tidak dipakai sebagai prediktor regresi. Baris dengan NA pada variabel utama (",
            code("price"), ", ", code("horsepower"), ", ", code("bore"), ", ", code("stroke"), ", ", code("peak_rpm"),
            ") disaring sehingga diperoleh ", strong("195 observasi bersih"), " untuk pemodelan regresi.")
        )
      )
    ),
    card(
      card_header("📋 Kamus Variabel & Tabel Data Lengkap"),
      navset_pill(
        nav_panel("Kamus 26 Variabel", DTOutput("tbl_dict")),
        nav_panel("Data Automobile (205 Baris)", DTOutput("tbl_raw"))
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: EKSPLORASI UNIVARIAT 
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🍭 2. Univariat",
    layout_sidebar(
      sidebar = sidebar(
        title = "🎛️ Kontrol Univariat",
        selectInput("uni_num", "Pilih Variabel Numerik:", choices = num_vars, selected = "price"),
        sliderInput("uni_bins", "Jumlah Batang (Bins):", min = 10, max = 40, value = 20),
        hr(),
        selectInput("uni_cat", "Pilih Variabel Kategorik:", choices = cat_vars, selected = "body_style"),
        helpText("💡 Setiap grafik dan tabel ditampilkan satu per satu di sub-tab terpisah agar terlihat sangat jelas!")
      ),
      navset_pill(
        nav_panel(
          title = "📊 1. Histogram & Densitas",
          card(
            card_header("Visualisasi Univariat 1 — Distribusi & Kurva Densitas"),
            card_body(
              plotlyOutput("plot_uni_hist", height = "460px"),
              div(class = "interp-box-pink", uiOutput("interp_uni_1"))
            )
          )
        ),
        nav_panel(
          title = "📦 2. Boxplot & Pencilan",
          card(
            card_header("Visualisasi Univariat 2 — Deteksi Pencilan / Outlier"),
            card_body(
              plotlyOutput("plot_uni_box", height = "460px"),
              div(class = "interp-box", uiOutput("interp_uni_2"))
            )
          )
        ),
        nav_panel(
          title = "🧸 3. Bar Chart Frekuensi",
          card(
            card_header("Visualisasi Univariat 3 — Frekuensi Kategori"),
            card_body(
              plotlyOutput("plot_uni_bar", height = "460px"),
              div(class = "interp-box-pink", uiOutput("interp_uni_3"))
            )
          )
        ),
        nav_panel(
          title = "🔢 4. Tabel Statistik Deskriptif",
          card(
            card_header("Ringkasan Ukuran Pemusatan & Penyebaran Data"),
            card_body(
              verbatimTextOutput("stat_uni_summary"),
              div(class = "interp-box", uiOutput("interp_uni_stat"))
            )
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: EKSPLORASI BIVARIAT 
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🍓 3. Bivariat",
    layout_sidebar(
      sidebar = sidebar(
        title = "🎛️ Kontrol Bivariat",
        h6("1️⃣ Numerik vs Numerik"),
        selectInput("bi_num_x", "Sumbu X (Numerik):", choices = num_vars, selected = "horsepower"),
        selectInput("bi_num_y", "Sumbu Y (Numerik):", choices = num_vars, selected = "price"),
        hr(),
        h6("2️⃣ Kategorik vs Numerik"),
        selectInput("bi_cat_x", "Kelompok (Kategorik):", choices = cat_vars, selected = "drive_wheels"),
        selectInput("bi_cat_y", "Nilai (Numerik):", choices = num_vars, selected = "price"),
        hr(),
        h6("3️⃣ Kategorik vs Kategorik"),
        selectInput("bi_cat2_x", "Kategori Utama:", choices = cat_vars, selected = "body_style"),
        selectInput("bi_cat2_fill", "Kategori Pembanding:", choices = cat_vars, selected = "fuel_type")
      ),
      navset_pill(
        nav_panel(
          title = "✨ 1. Scatter Plot",
          card(
            card_header("Visualisasi Bivariat 1 — Scatter Plot & Tren Linear"),
            card_body(
              plotlyOutput("plot_bi_scatter", height = "470px"),
              div(class = "interp-box-pink", uiOutput("interp_bi_1"))
            )
          )
        ),
        nav_panel(
          title = "🎀 2. Boxplot Kelompok",
          card(
            card_header("Visualisasi Bivariat 2 — Boxplot Perbandingan Kelompok"),
            card_body(
              plotlyOutput("plot_bi_box", height = "470px"),
              div(class = "interp-box", uiOutput("interp_bi_2"))
            )
          )
        ),
        nav_panel(
          title = "🍡 3. Proporsi Silang",
          card(
            card_header("Visualisasi Bivariat 3 — Proporsi Silang Antar Kategori"),
            card_body(
              plotlyOutput("plot_bi_bar", height = "470px"),
              div(class = "interp-box-pink", uiOutput("interp_bi_3"))
            )
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 4: EKSPLORASI MULTIVARIAT
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🌈 4. Multivariat",
    layout_sidebar(
      sidebar = sidebar(
        title = "🎛️ Kontrol Multivariat",
        selectInput("multi_x", "Sumbu X (Numerik):", choices = num_vars, selected = "curb_weight"),
        selectInput("multi_y", "Sumbu Y (Numerik):", choices = num_vars, selected = "price"),
        selectInput("multi_size", "Ukuran Gelembung (Numerik):", choices = num_vars, selected = "horsepower"),
        selectInput("multi_color", "Warna Kelompok (Kategorik):", choices = cat_vars, selected = "drive_wheels"),
        helpText("💡 Setiap visualisasi multivariat ditampilkan satu per satu secara fokus.")
      ),
      navset_pill(
        nav_panel(
          title = "🫧 1. Bubble Plot 4D",
          card(
            card_header("Visualisasi Multivariat 1 — Bubble Plot 4 Dimensi"),
            card_body(
              plotlyOutput("plot_multi_bubble", height = "480px"),
              div(class = "interp-box-pink", uiOutput("interp_multi_bubble"))
            )
          )
        ),
        nav_panel(
          title = "🎨 2. Heatmap Korelasi",
          card(
            card_header("Visualisasi Multivariat 2 — Matriks Korelasi Numerik"),
            card_body(
              plotOutput("plot_multi_corr", height = "480px"),
              div(class = "interp-box",
                  strong("📝 Interpretasi Matriks Korelasi: "),
                  "Harga mobil (price) memiliki korelasi positif sangat kuat dengan kapasitas mesin (engine_size, r = 0.87), berat kosong (curb_weight, r = 0.84), lebar mobil (width, r = 0.76), dan tenaga kuda (horsepower, r = 0.81). Sebaliknya, harga berkorelasi negatif kuat dengan efisiensi bahan bakar (city_mpg dan highway_mpg).")
            )
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 5: REGRESI LINEAR BERGANDA, VISUALISASI ESTIMASI & UJI ASUMSI KLASIK
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🔮 5. Regresi & Uji Asumsi Klasik",
    layout_sidebar(
      sidebar = sidebar(
        title = "🎛️ Spesifikasi Model",
        selectInput("reg_y", "Variabel Respons (Y):", choices = c("price", "city_mpg", "highway_mpg"), selected = "price"),
        checkboxGroupInput(
          "reg_x", "Pilih Variabel Prediktor (Min. 2):",
          choices = c("horsepower", "curb_weight", "engine_size", "width", "city_mpg", "peak_rpm"),
          selected = c("horsepower", "curb_weight", "engine_size", "width")
        ),
        helpText("💡 Centang minimal 2 prediktor untuk menguji model regresi, visualisasi estimasi, dan asumsi klasik.")
      ),
      navset_pill(
        # SUB-TAB 1: TABEL ESTIMASI MODEL
        nav_panel(
          title = "📑 1. Tabel Estimasi",
          card(
            card_header("Persamaan Regresi Linear Berganda & Uji Simultan/Parsial"),
            card_body(
              uiOutput("reg_equation"),
              verbatimTextOutput("reg_summary"),
              div(class = "interp-box-pink", uiOutput("reg_interpretation"))
            )
          )
        ),
        # SUB-TAB 2: VISUALISASI ESTIMASI KOEFISIEN (BARU)
        nav_panel(
          title = "📉 2. Visualisasi Estimasi",
          card(
            card_header("Visualisasi Estimasi Koefisien Regresi & Selang Kepercayaan 95%"),
            card_body(
              plotlyOutput("plot_reg_coef", height = "460px"),
              div(class = "interp-box", uiOutput("interp_reg_coef"))
            )
          )
        ),
        # SUB-TAB 3: AKTUAL VS PREDIKSI
        nav_panel(
          title = "🎯 3. Aktual vs Prediksi",
          card(
            card_header("Visualisasi Hasil Pemodelan — Nilai Aktual vs Prediksi"),
            card_body(
              plotlyOutput("plot_reg_actual", height = "470px"),
              div(class = "interp-box-pink", uiOutput("interp_reg_plot1"))
            )
          )
        ),
        # SUB-TAB 4: UJI NORMALITAS
        nav_panel(
          title = "📊 4. Uji Normalitas",
          card(
            card_header("Uji Asumsi Klasik: Normalitas Residual"),
            card_body(
              plotlyOutput("plot_asumsi_norm", height = "400px"),
              verbatimTextOutput("test_norm_res"),
              div(class = "interp-box", uiOutput("interp_asumsi_norm"))
            )
          )
        ),
        # SUB-TAB 5: UJI MULTIKOLINEARITAS
        nav_panel(
          title = "🔗 5. Uji Multikolinearitas",
          card(
            card_header("Uji Asumsi Klasik: Nilai VIF / Variance Inflation Factor"),
            card_body(
              verbatimTextOutput("test_vif_res"),
              div(class = "interp-box-pink", uiOutput("interp_asumsi_vif"))
            )
          )
        ),
        # SUB-TAB 6: UJI HETEROSKEDASTISITAS
        nav_panel(
          title = "🩺 6. Uji Heteroskedastisitas",
          card(
            card_header("Uji Asumsi Klasik: Breusch-Pagan & Plot Residual"),
            card_body(
              plotlyOutput("plot_asumsi_het", height = "400px"),
              verbatimTextOutput("test_het_res"),
              div(class = "interp-box", uiOutput("interp_asumsi_het"))
            )
          )
        ),
        # SUB-TAB 7: UJI AUTOKORELASI
        nav_panel(
          title = "📈 7. Uji Autokorelasi",
          card(
            card_header("Uji Asumsi Klasik: Durbin-Watson Test"),
            card_body(
              verbatimTextOutput("test_dw_res"),
              div(class = "interp-box-pink", uiOutput("interp_asumsi_dw"))
            )
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 6: KESIMPULAN & REKOMENDASI
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "🏆 6. Kesimpulan",
    card(
      card_header("🌟 Kesimpulan Utama & Rekomendasi Analisis (Khoirun Nisa)"),
      card_body(
        h4("1. Kesimpulan Utama"),
        tags$ol(
          tags$li(strong("Pola Univariat & Pencilan: "), "Harga mobil (price) berdistribusi menceng ke kanan dengan median $10.295 dan rata-rata $13.207. Pencilan atas didominasi oleh merek mewah berperforma tinggi."),
          tags$li(strong("Hubungan Bivariat & Multivariat: "), "Kapasitas mesin, berat kendaraan, dan tenaga kuda mendorong kenaikan harga secara signifikan, namun berbanding terbalik dengan keiritan bahan bakar (MPG)."),
          tags$li(strong("Pemodelan Regresi & Asumsi Klasik: "), "Model regresi linear berganda memiliki Adjusted R² > 0.80. Melalui visualisasi estimasi koefisien dan uji parsial, kapasitas mesin dan tenaga kuda merupakan pendorong utama kenaikan harga.")
        ),
        hr(),
        h4("2. Rekomendasi"),
        tags$ul(
          tags$li(strong("Bagi Calon Pembeli: "), "Segmen sedan atau hatchback berpenggerak fwd dengan kapasitas mesin menengah memberikan keseimbangan terbaik antara harga beli dan efisiensi BBM."),
          tags$li(strong("Bagi Pengembangan Model: "), "Disarankan melakukan transformasi logaritma pada variabel harga (log(price)) untuk meredam varians error (heteroskedastisitas) pada mobil kelas mewah.")
        )
      )
    )
  )
)

# ==============================================================================
# 3. LOGIKA SERVER (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  output$tbl_dict <- renderDT({
    datatable(var_dictionary, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$tbl_raw <- renderDT({
    datatable(auto_data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # --- UNIVARIAT ---
  output$plot_uni_hist <- renderPlotly({
    var <- input$uni_num
    df_sub <- auto_data %>% filter(!is.na(.data[[var]]))
    p <- ggplot(df_sub, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = input$uni_bins, fill = "#FF8FA3", color = "white", alpha = 0.85) +
      geom_density(color = "#7209B7", linewidth = 1.2) +
      labs(title = paste("Distribusi & Kurva Densitas:", var), x = var, y = "Densitas") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_uni_1 <- renderUI({
    var <- input$uni_num
    x <- na.omit(auto_data[[var]])
    m <- mean(x); med <- median(x)
    bentuk <- ifelse(m > med * 1.05, "menceng ke kanan (right-skewed)", ifelse(m < med * 0.95, "menceng ke kiri (left-skewed)", "relatif simetris"))
    HTML(sprintf("<strong>📝 Interpretasi Histogram:</strong> Variabel <b>%s</b> memiliki nilai rata-rata <b>%.2f</b> dan median <b>%.2f</b> (pola <b>%s</b>).", var, m, med, bentuk))
  })
  
  output$plot_uni_box <- renderPlotly({
    var <- input$uni_num
    df_sub <- auto_data %>% filter(!is.na(.data[[var]]))
    p <- ggplot(df_sub, aes(y = .data[[var]], x = "")) +
      geom_boxplot(fill = "#80FFDB", color = "#2B9348", alpha = 0.8, outlier.color = "#F72585", outlier.size = 3) +
      coord_flip() +
      labs(title = paste("Boxplot & Deteksi Pencilan:", var), x = "", y = var) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_uni_2 <- renderUI({
    var <- input$uni_num
    x <- na.omit(auto_data[[var]])
    q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
    n_out <- sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
    HTML(sprintf("<strong>📝 Interpretasi Boxplot:</strong> IQR variabel <b>%s</b> adalah <b>%.2f</b> (Q1=%.2f, Q3=%.2f) dengan <b>%d pencilan</b> terdeteksi.", var, iqr, q1, q3, n_out))
  })
  
  output$plot_uni_bar <- renderPlotly({
    var <- input$uni_cat
    df_sub <- auto_data %>% filter(!is.na(.data[[var]])) %>% count(.data[[var]], name = "Jumlah") %>% arrange(desc(Jumlah))
    p <- ggplot(df_sub, aes(x = reorder(.data[[var]], Jumlah), y = Jumlah, fill = .data[[var]])) +
      geom_col(alpha = 0.9) +
      scale_fill_manual(values = rep(cute_palette, length.out = nrow(df_sub))) +
      coord_flip() +
      labs(title = paste("Distribusi Frekuensi Kategori:", var), x = var, y = "Jumlah Mobil") +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$interp_uni_3 <- renderUI({
    var <- input$uni_cat
    df_sub <- auto_data %>% filter(!is.na(.data[[var]])) %>% count(.data[[var]], sort = TRUE)
    HTML(sprintf("<strong>📝 Interpretasi Kategorik:</strong> Kategori paling dominan pada <b>%s</b> adalah <b>%s</b> sebanyak <b>%d observasi</b>.", var, as.character(df_sub[[1]][1]), df_sub$n[1]))
  })
  
  output$stat_uni_summary <- renderPrint({
    var <- input$uni_num
    x <- na.omit(auto_data[[var]])
    cat("=== STATISTIK DESKRIPTIF:", toupper(var), "===\n")
    cat("n =", length(x), "| Mean =", round(mean(x), 2), "| Median =", round(median(x), 2), "\n")
    cat("SD =", round(sd(x), 2), "| Min =", round(min(x), 2), "| Max =", round(max(x), 2), "\n")
    cat("Q1 =", round(quantile(x, 0.25), 2), "| Q3 =", round(quantile(x, 0.75), 2), "| IQR =", round(IQR(x), 2), "\n")
  })
  
  output$interp_uni_stat <- renderUI({
    HTML(sprintf("<strong>📝 Interpretasi Statistik:</strong> Ringkasan ukuran pemusatan dan penyebaran di atas menunjukkan karakteristik sebaran data secara lengkap satu per satu."))
  })
  
  # --- BIVARIAT ---
  output$plot_bi_scatter <- renderPlotly({
    x_var <- input$bi_num_x; y_var <- input$bi_num_y
    df_sub <- auto_data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    p <- ggplot(df_sub, aes(x = .data[[x_var]], y = .data[[y_var]], text = make)) +
      geom_point(color = "#FF6B9D", size = 2.8, alpha = 0.75) +
      geom_smooth(method = "lm", color = "#7209B7", fill = "#E0AAFF", se = TRUE) +
      labs(title = paste("Hubungan Linear:", x_var, "vs", y_var), x = x_var, y = y_var) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_bi_1 <- renderUI({
    x_var <- input$bi_num_x; y_var <- input$bi_num_y
    df_sub <- auto_data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    r_val <- cor(df_sub[[x_var]], df_sub[[y_var]])
    HTML(sprintf("<strong>📝 Interpretasi Scatter Plot:</strong> Koefisien korelasi <b>r = %.3f</b> menunjukkan hubungan linear antara <b>%s</b> dan <b>%s</b>.", r_val, x_var, y_var))
  })
  
  output$plot_bi_box <- renderPlotly({
    x_var <- input$bi_cat_x; y_var <- input$bi_cat_y
    df_sub <- auto_data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    p <- ggplot(df_sub, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
      geom_boxplot(alpha = 0.85, outlier.color = "#F72585") +
      scale_fill_manual(values = rep(cute_palette, length.out = nlevels(droplevels(df_sub[[x_var]])))) +
      labs(title = paste("Perbandingan", y_var, "Berdasarkan", x_var), x = x_var, y = y_var) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$interp_bi_2 <- renderUI({
    HTML(sprintf("<strong>📝 Interpretasi Kelompok:</strong> Boxplot membandingkan sebaran <b>%s</b> di setiap kategori <b>%s</b>.", input$bi_cat_y, input$bi_cat_x))
  })
  
  output$plot_bi_bar <- renderPlotly({
    x_var <- input$bi_cat2_x; f_var <- input$bi_cat2_fill
    df_sub <- auto_data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[f_var]]))
    p <- ggplot(df_sub, aes(x = .data[[x_var]], fill = .data[[f_var]])) +
      geom_bar(position = "fill", alpha = 0.9) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values = rep(cute_palette, length.out = nlevels(droplevels(df_sub[[f_var]])))) +
      labs(title = paste("Proporsi", f_var, "pada Setiap Kategori", x_var), x = x_var, y = "Persentase (%)", fill = f_var) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_bi_3 <- renderUI({
    HTML(sprintf("<strong>📝 Interpretasi Proporsi:</strong> Grafik menunjukkan komposisi persentase <b>%s</b> di dalam kategori <b>%s</b>.", input$bi_cat2_fill, input$bi_cat2_x))
  })
  
  # --- MULTIVARIAT ---
  output$plot_multi_bubble <- renderPlotly({
    x_v <- input$multi_x; y_v <- input$multi_y; s_v <- input$multi_size; c_v <- input$multi_color
    df_sub <- auto_data %>% filter(!is.na(.data[[x_v]]), !is.na(.data[[y_v]]), !is.na(.data[[s_v]]), !is.na(.data[[c_v]]))
    p <- ggplot(df_sub, aes(x = .data[[x_v]], y = .data[[y_v]], size = .data[[s_v]], color = .data[[c_v]], text = make)) +
      geom_point(alpha = 0.75) +
      scale_color_manual(values = rep(cute_palette, length.out = nlevels(droplevels(df_sub[[c_v]])))) +
      labs(title = paste("Bubble Plot 4D:", y_v, "vs", x_v), x = x_v, y = y_v, size = s_v, color = c_v) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_multi_bubble <- renderUI({
    HTML(sprintf("<strong>📝 Interpretasi Multivariat:</strong> Grafik memetakan 4 variabel simultan (X, Y, Ukuran, dan Warna) untuk melihat pola kluster data secara terfokus."))
  })
  
  output$plot_multi_corr <- renderPlot({
    selected_num <- c("price", "horsepower", "curb_weight", "engine_size", "wheel_base", "width", "city_mpg", "highway_mpg")
    corr_mat <- cor(auto_clean[, selected_num], use = "complete.obs")
    col_pal <- colorRampPalette(c("#4ECDC4", "#FFFFFF", "#FF6B9D"))(200)
    corrplot(corr_mat, method = "color", type = "upper", col = col_pal,
             addCoef.col = "#4A3B47", tl.col = "#4A3B47", tl.srt = 45, number.cex = 0.85,
             title = "Matriks Korelasi Variabel Numerik", mar = c(0, 0, 2, 0))
  })
  
  # --- REGRESI BERGANDA, VISUALISASI ESTIMASI & UJI ASUMSI KLASIK ---
  reg_model <- reactive({
    req(length(input$reg_x) >= 2)
    formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
    lm(as.formula(formula_str), data = auto_clean)
  })
  
  output$reg_equation <- renderUI({
    mod <- reg_model()
    coefs <- coef(mod)
    terms <- paste0(round(coefs[-1], 3), "*(", names(coefs)[-1], ")", collapse = " + ")
    eq <- paste0(input$reg_y, " = ", round(coefs[1], 3), " + ", terms)
    div(class = "alert alert-light border", style = "border-color: #FF6B9D !important;",
        strong("✨ Persamaan Model Regresi: "), code(eq))
  })
  
  output$reg_summary <- renderPrint({
    summary(reg_model())
  })
  
  output$reg_interpretation <- renderUI({
    mod <- reg_model()
    s <- summary(mod)
    adj_r2 <- round(s$adj.r.squared * 100, 2)
    f_stat <- s$fstatistic
    p_val_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    HTML(sprintf("<strong>📝 Interpretasi Model:</strong> F-stat = %.2f (p-value = %.4e). <b>Adjusted R² = %.2f%%</b> variasi %s dapat dijelaskan oleh model.", f_stat[1], p_val_f, adj_r2, input$reg_y))
  })
  
  # VISUALISASI ESTIMASI KOEFISIEN (COEFFICIENT PLOT)
  output$plot_reg_coef <- renderPlotly({
    mod <- reg_model()
    coef_df <- broom::tidy(mod, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(Signifikansi = ifelse(p.value < 0.05, "Signifikan (p < 0.05)", "Tidak Signifikan (p >= 0.05)"))
    
    p <- ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate), color = Signifikansi)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#7F8C8D", linewidth = 1) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25, linewidth = 1.1) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Signifikan (p < 0.05)" = "#FF6B9D", "Tidak Signifikan (p >= 0.05)" = "#A78BFA")) +
      labs(title = paste("Visualisasi Estimasi Koefisien (β) & Selang Kepercayaan 95% untuk", input$reg_y),
           x = "Nilai Estimasi Koefisien (β)", y = "Variabel Prediktor") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_reg_coef <- renderUI({
    HTML("<strong>📝 Interpretasi Visualisasi Estimasi Koefisien:</strong> Titik menunjukkan nilai estimasi parameter (β), sedangkan garis horizontal menunjukkan selang kepercayaan 95%. Variabel berwarna <b>pink menyala</b> yang garis intervalnya <b>tidak memotong garis vertikal nol (0)</b> terbukti berpengaruh signifikan secara statistik terhadap variabel respons.")
  })
  
  output$plot_reg_actual <- renderPlotly({
    mod <- reg_model()
    df_res <- data.frame(Aktual = auto_clean[[input$reg_y]], Prediksi = fitted(mod), Merek = auto_clean$make)
    p <- ggplot(df_res, aes(x = Aktual, y = Prediksi, text = Merek)) +
      geom_point(color = "#4ECDC4", size = 3, alpha = 0.8) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#FF6B9D", linewidth = 1.2) +
      labs(title = "Aktual vs Prediksi Model", x = "Nilai Aktual", y = "Nilai Prediksi") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$interp_reg_plot1 <- renderUI({
    HTML("<strong>📝 Interpretasi Plot Aktual vs Prediksi:</strong> Titik yang mendekati garis diagonal menunjukkan tingkat keakuratan prediksi model yang tinggi.")
  })
  
  # UJI 1: NORMALITAS RESIDUAL
  output$plot_asumsi_norm <- renderPlotly({
    mod <- reg_model()
    res <- residuals(mod)
    p <- ggplot(data.frame(res), aes(x = res)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#A78BFA", color = "white", alpha = 0.85) +
      geom_density(color = "#FF6B9D", linewidth = 1.2) +
      labs(title = "Histogram & Kurva Densitas Residual Model", x = "Residual (Error)", y = "Densitas") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$test_norm_res <- renderPrint({
    mod <- reg_model()
    shapiro.test(residuals(mod))
  })
  
  output$interp_asumsi_norm <- renderUI({
    HTML("<strong>📝 Interpretasi Uji Normalitas:</strong> Histogram residual menunjukkan sebaran yang mendekati kurva lonceng (normal). Jika p-value Shapiro-Wilk &gt; 0.05, maka asumsi normalitas residual terpenuhi.")
  })
  
  # UJI 2: MULTIKOLINEARITAS (VIF)
  output$test_vif_res <- renderPrint({
    mod <- reg_model()
    if(length(coef(mod)) > 2) {
      vif(mod)
    } else {
      cat("Uji VIF memerlukan minimal 2 variabel prediktor dalam model.")
    }
  })
  
  output$interp_asumsi_vif <- renderUI({
    HTML("<strong>📝 Interpretasi Uji Multikolinearitas (VIF):</strong> Nilai VIF &lt; 10 (atau &lt; 5) menunjukkan bahwa tidak terdapat korelasi antarprediktor yang berlebihan, sehingga asumsi non-multikolinearitas terpenuhi.")
  })
  
  # UJI 3: HETEROSKEDASTISITAS
  output$plot_asumsi_het <- renderPlotly({
    mod <- reg_model()
    df_res <- data.frame(Fitted = fitted(mod), Residual = resid(mod), Merek = auto_clean$make)
    p <- ggplot(df_res, aes(x = Fitted, y = Residual, text = Merek)) +
      geom_point(color = "#FF7597", size = 3, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#4ECDC4", linewidth = 1.2) +
      labs(title = "Diagnostik Residual vs Fitted (Uji Heteroskedastisitas)", x = "Nilai Prediksi", y = "Residual") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$test_het_res <- renderPrint({
    mod <- reg_model()
    bptest(mod)
  })
  
  output$interp_asumsi_het <- renderUI({
    HTML("<strong>📝 Interpretasi Uji Heteroskedastisitas:</strong> Uji Breusch-Pagan memeriksa apakah varians error konstan (homoskedastis). Jika p-value &gt; 0.05, asumsi homoskedastisitas terpenuhi.")
  })
  
  # UJI 4: AUTOKORELASI (DW TEST)
  output$test_dw_res <- renderPrint({
    mod <- reg_model()
    dwtest(mod)
  })
  
  output$interp_asumsi_dw <- renderUI({
    HTML("<strong>📝 Interpretasi Uji Autokorelasi (Durbin-Watson):</strong> Uji Durbin-Watson mendeteksi korelasi antar-error berurutan. Nilai DW sekitar 2 atau p-value &gt; 0.05 mengindikasikan tidak adanya autokorelasi.")
  })
}

# ==============================================================================
# 4. MENJALANKAN APLIKASI SHINY
# ==============================================================================
shinyApp(ui = ui, server = server)
