set.seed(123)

region_data <- tibble::tribble(
  ~id, ~label, ~x, ~y,
  1, "region 1", 0, 0,
  1, "region 1", 0, 1,
  1, "region 1", 1, 1,
  1, "region 1", 1, 0,
  2, "region 2", 0, 0,
  2, "region 2", 0, 1,
  2, "region 2", -1, 0,
  3, "region 3", 0, 0,
  3, "region 3", 0, -1,
  3, "region 3", -1, -1,
  3, "region 3", -1, 0,
  4, "region 4", 0, 0,
  4, "region 4", 0, -1,
  4, "region 4", 1, 0
)

n_fractions <- 8
fraction_size <- 2

count_data <- expand.grid(
  region_id = unique(region_data$id),
  fraction = head(LETTERS, n_fractions),
  foo = seq_len(fraction_size)
)
count_data[["count"]] <- round(runif(n = nrow(count_data), min = 0, max = 1000))

original_path <- getwd()

tryCatch(
  expr = {
    setwd("tests/testthat/data")
    
    readr::write_csv(
      x = count_data,
      file = "test_count_data.csv"
    )
    readr::write_csv(
      x = region_data,
      file = "test_region_data.csv"
    )
  },
  finally = {
    setwd(original_path)
  }
)
