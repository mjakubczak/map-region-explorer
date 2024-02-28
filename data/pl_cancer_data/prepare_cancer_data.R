# download ICD10en.csv from:
# https://dane.gov.pl/pl/dataset/27,rejestr-systemow-kodowania/resource/10566
icd10 <- readr::read_csv2("ICD10en.csv") |>
  dplyr::filter(`Subcategory code` == "|") |>
  dplyr::select(id = `Category code`, label = `Category`) |>
  dplyr::distinct(id, .keep_all = TRUE)

# download Zachorowania1999-2018_wojewodztwa.csv from:
# https://dane.gov.pl/pl/dataset/1792/resource/31979,zachorowania-na-nowotwory-w-polsce-w-latach-1999-2018
cancer_data <- readr::read_csv2("Zachorowania1999-2018_wojewodztwa.csv") |>
  dplyr::rename(
    year = "Rok (rok)",
    voivodeship = "Wojewodztwo (wojewodztwo)",
    sex = "Plec (plec)",
    icd10 = "ICD10 (icd10)",
    age = "Wiek (wiek)",
    count = "Liczba (liczba)"
  ) |>
  dplyr::mutate(sex = ifelse(sex == "K", "F", sex))

# download **ALL** voiv.* files from:
# https://github.com/mbojan/mapoland/tree/master/inst/shapes
# the mapoland package is not supported anymore
shapes <- sf::st_read("voiv.shp")

voivodeships <- shapes %>%
  split(., .$SP_ID_1) |>
  purrr::map_dfr(
    .f = function(x){
      g <- x$geometry[[1]][[1]]
      
      list(
        label = x$names_asci[1],
        x = g[, 1],
        y = g[, 2]
      )
    }, 
    .id = "id"
  )

readr::write_csv(voivodeships, "voivodeships.csv")
readr::write_csv(icd10, "icd10.csv")
readr::write_csv(cancer_data, "cancer_data.csv")
