test_that("downloading state cancer profiles statistics works", {
  test_that("downloading data doesn't throw a warning", {
    expect_silent(cancer_statistics())
  })

  test_that("downloading county incidence data for all cancers works", {
    dat <- cancer_statistics(statistic = "incidence", by = "county", cancer = "all")

    expect_equivalent(names(dat), c("county",
                                    "fips",
                                    "incidence_rate",
                                    "incidence_rate_95_confint_lower",
                                    "incidence_rate_95_confint_upper",
                                    "average_annual_count",
                                    "recent_trend_description",
                                    "recent_trend",
                                    "recent_trend_95_confint_lower",
                                    "recent_trend_95_confint_upper",
                                    "statistic",
                                    "cancer",
                                    "sex",
                                    "age",
                                    "race"
    ))

    expect_equal(nrow(dat), 3141)

    expect_equal(dat$county[1], "US (SEER+NPCR)(1,10)")
    expect_equal(dat$fips[1], "00000")
    expect_equal(dat$incidence_rate[1], 448.4)
  })

  test_that("downloading county incidence data for a specific cancer works", {
    dat <- cancer_statistics(statistic = "incidence", by = "county", cancer="breast (female)")

    expect_equal(dat$cancer[1], "breast (female)")
    expect_equal(dat$incidence_rate[1], 123.3)
  })
})
