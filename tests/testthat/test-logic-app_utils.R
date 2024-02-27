box::use(
  testthat[...]
)
box::use(
  app/logic/app_utils[...]
)

test_that(
  desc = "app utils work fine",
  code = {
    x <- help_icon("test text")
    expect_is(x, "shiny.tag")
    expect_equal(x$name, "i")
    expect_equal(x$attribs$title, "test text")
    
    expect_true(check_filled_df(iris))
    expect_false(check_filled_df(data.frame()))
    expect_false(check_filled_df(data.frame(x = character(0))))
    expect_true(check_filled_df(data.frame(x = character(1))))
  }
)
