# drake plan
plan <- drake_plan(
  # simulate data
  d = simData(),
  # analyse data
  fitBinom = fitBinomialTests(d),
  fitGLMM  = fitBrms(d),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)