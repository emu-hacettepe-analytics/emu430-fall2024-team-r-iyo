# R/prepare_data.R
# Reads the pivot-style HNS_2022 xlsx, flattens it, and writes RData + CSV.
# Run once (or after the source xlsx changes):
#   source("R/prepare_data.R")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(tibble)
})

raw <- read_excel(
  "data/HNS_2022.xlsx",
  sheet      = 1,
  skip       = 2,
  col_names  = c("port_authority", "facility", "company", "cargo_mt"),
  col_types  = c("text", "text", "text", "numeric")
)

clean <- raw |>
  filter(
    !is.na(company),
    !str_detect(company, "^Toplam"),
    !str_detect(replace_na(facility, ""), "^Toplam"),
    !str_detect(replace_na(port_authority, ""), "^Toplam"),
    !is.na(cargo_mt),
    cargo_mt > 0
  ) |>
  mutate(
    facility = if_else(facility == "(boş)", "Unspecified facility", facility),
    company  = str_squish(company),
    company_clean = company |>
      stri_trans_general("Any-Latin; Latin-ASCII") |>
      str_to_upper() |>
      str_replace_all("[^A-Z0-9]+", " ") |>
      str_squish(),
    port_authority_short = port_authority |>
      str_remove("\\s*B[öo]lge\\s*Liman\\s*Ba[şs]kanl[ıi][ğg][ıi].*") |>
      str_remove("\\s*Liman\\s*Ba[şs]kanl[ıi][ğg][ıi].*") |>
      str_squish()
  )

province_map <- tribble(
  ~port_authority_short, ~province,    ~iso2,
  "Bandırma",            "Balıkesir",  "TR-10",
  "Mersin",              "Mersin",     "TR-33",
  "İskenderun",          "Hatay",      "TR-31",
  "Aliağa",              "İzmir",      "TR-35",
  "Samsun",              "Samsun",     "TR-55",
  "Antalya",             "Antalya",    "TR-07",
  "Tekirdağ",            "Tekirdağ",   "TR-59",
  "Ceyhan",              "Adana",      "TR-01",
  "Trabzon",             "Trabzon",    "TR-61",
  "Tirebolu",            "Giresun",    "TR-28",
  "Ambarlı",             "İstanbul",   "TR-34",
  "İstanbul",            "İstanbul",   "TR-34",
  "Çanakkale",           "Çanakkale",  "TR-17",
  "Karadeniz Ereğli",    "Zonguldak",  "TR-67",
  "Gemlik",              "Bursa",      "TR-16",
  "Güllük",              "Muğla",      "TR-48",
  "İzmir",               "İzmir",      "TR-35"
)

hns2022 <- clean |>
  left_join(province_map, by = "port_authority_short")

unmatched <- hns2022 |> filter(is.na(province)) |> distinct(port_authority_short)
if (nrow(unmatched) > 0) {
  warning("Unmatched port authorities: ", paste(unmatched$port_authority_short, collapse = "; "))
}

dir.create("data", showWarnings = FALSE)
save(hns2022, file = "data/HNS_2022_clean.RData")
write.csv(hns2022, "data/HNS_2022_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")

message(sprintf("Done. %d clean leaf rows. Total cargo: %s metric tons.",
                nrow(hns2022),
                format(round(sum(hns2022$cargo_mt)), big.mark = ",")))
