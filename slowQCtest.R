library(testthat)
library(jaspTools)

jaspBase::assignFunctionInPackage(
  fun     = function(ggplotObj, returnJSON = TRUE) {
    # see https://github.com/Rdatatable/data.table/issues/5375
    # we must set getOption("datatable.alloccol"), verbose = getOption("datatable.verbose")
    # options(datatable.alloccol = 1024L, datatable.verbose = FALSE) # default values
    # https://github.com/Rdatatable/data.table/blob/35544d34ce779599cb2ed8900da2ffbac0ffbf29/R/onLoad.R#L76C11-L94C9
    options(
      "datatable.verbose" = FALSE,        # datatable.<argument name>
      "datatable.optimize" = Inf,             # datatable.<argument name>
      "datatable.print.nrows" = 100L,         # datatable.<argument name>
      "datatable.print.topn" = 5L,            # datatable.<argument name>
      "datatable.print.class" = TRUE,         # for print.data.table
      "datatable.print.rownames" = TRUE,      # for print.data.table
      "datatable.print.colnames" = "'auto'",    # for print.data.table
      "datatable.print.keys" = TRUE,          # for print.data.table
      "datatable.print.trunc.cols" = FALSE,   # for print.data.table
      "datatable.show.indices" = FALSE,       # for print.data.table
      "datatable.allow.cartesian" = FALSE,    # datatable.<argument name>
      "datatable.join.many" = TRUE,           # mergelist, [.data.table #4383 #914
      "datatable.dfdispatchwarn" = TRUE,                   # not a function argument
      "datatable.warnredundantby" = TRUE,                  # not a function argument
      "datatable.alloccol" = 1024L,           # argument 'n' of alloc.col. Over-allocate 1024 spare column slots
      "datatable.auto.index" = TRUE,          # DT[col=="val"] to auto add index so 2nd time faster
      "datatable.use.index" = TRUE,           # global switch to address #1422
      "datatable.prettyprint.char"   = NULL     # FR #1091
    )

    # TODO: a lot of the ggplot2 stuff below assumes ggplot2 4.0.0 or higher!
    # TODO: this would really benefit from rigourous unit-tests!

    e <- try({

      converted <- jaspQualityControl:::log_time(jaspGraphs:::convertPlotObjectToPlotly(ggplotObj))

      # TODO: we should decode any column names in the data in plotlybuild$x... maybe we can do this through ggplot2 though
      if (returnJSON) {
        json <- jaspGraphs:::toJSON(list(data = converted$plotly$x$data, layout = converted$plotly$x$layout, hasRangeFrame = converted$hasRangeFrame))
        json
      } else {
        converted$plotly
      }
    })
    return(e)
  },
  name    = "convertGgplotToPlotly",
  package = "jaspGraphs"
)
print(jaspGraphs::convertGgplotToPlotly)
jaspBase::assignFunctionInPackage(
  fun = function(...) {
    tryCatch(
      suppressWarnings(return(jaspQualityControl:::log_time(jaspBase:::writeImageJaspResults(...)))),
      error	= function(e) { return(list(error = e$message)) }
    )
  },
  name    = "tryToWriteImageJaspResults",
  package = "jaspBase"
)
print(jaspBase:::tryToWriteImageJaspResults)
jaspBase::assignFunctionInPackage(
  fun = function(plot, width = 320, height = 320, obj = TRUE, relativePathpng = NULL, relativePathJson = NULL, ppi = 300, backgroundColor = "white",
                 location = jaspBase:::getImageLocation(), oldPlotInfo = list()) {
    # Set values from JASP'S Rcpp when available
    if (exists(".fromRCPP")) {
      location        <- jaspBase:::.fromRCPP(".requestTempFileNameNative", "png")
      backgroundColor <- jaspBase:::.fromRCPP(".imageBackground")
      ppi             <- jaspBase:::.fromRCPP(".ppi")
    }

    # TRUE if called from analysis, FALSE if called from editImage
    if (is.null(relativePathpng))
      relativePathpng <- location$relativePath

    image                           <- list()
    fullPathpng                     <- paste(location$root, relativePathpng, sep="/")
    plotEditingOptions              <- NULL
    root                            <- location$root
    oldwd                           <- getwd()
    setwd(root)
    on.exit(setwd(oldwd))

    if (length(oldPlotInfo) != 0L && !is.null(oldPlotInfo[["editOptions"]]) && ggplot2::is.ggplot(plot)) {

      # uncommenting this applies the edits previously done with plot editing to an older figure to the new figure.
      # see https://github.com/jasp-stats/INTERNAL-jasp/issues/1257 for discussion on what needs to be done before we can do this.

      # e <- try({
      #   # same construction as in editImage
      #   newPlot <- ggplot2:::plot_clone(plot)
      #
      #   newOpts       <- jaspBase::fromJSON(oldPlotInfo[["editOptions"]])
      #   oldOpts       <- jaspGraphs::plotEditingOptions(plot)
      #   newOpts$xAxis <- list(type = oldOpts$xAxis$type, settings = newOpts$xAxis$settings[names(newOpts$xAxis$settings) != "type"])
      #   newOpts$yAxis <- list(type = oldOpts$yAxis$type, settings = newOpts$yAxis$settings[names(newOpts$yAxis$settings) != "type"])
      #
      #   newPlot <- jaspGraphs::plotEditing(newPlot, newOpts)
      # })
      #
      # if (!inherits(e, "try-error"))
      #   plot <- newPlot

    }

    # IN CASE WE SWITCH TO SVG:
    # # convert width & height from pixels to inches. ppi = pixels per inch. 72 is a magic number inherited from the past.
    # # originally, this number was 96 but svglite scales this by (72/96 = 0.75). 0.75 * 96 = 72.
    # # for reference see https://cran.r-project.org/web/packages/svglite/vignettes/scaling.html
    # width  <- width / 72
    # height <- height / 72

    width  <- width  * (ppi / 96)
    height <- height * (ppi / 96)

    plot2draw <- jaspQualityControl:::log_time(jaspBase:::decodeplot(plot))

    jaspBase:::openGrDevice(file = relativePathpng, width = width, height = height, res = 72 * (ppi / 96), background = backgroundColor)#, dpi = ppi)
    on.exit(grDevices::dev.off(), add = TRUE)

    if (ggplot2::is.ggplot(plot2draw) || inherits(plot2draw, c("gtable", "gTree"))) {

      # inherited from ggplot2::ggsave
      jaspQualityControl:::log_time(grid::grid.draw(plot2draw))

    } else {

      isRecordedPlot <- inherits(plot2draw, "recordedplot")

      if (is.function(plot2draw) && !isRecordedPlot) {

        if (obj) grDevices::dev.control('enable') # enable plot recording
        eval(plot())
        if (obj) plot2draw <- grDevices::recordPlot() # save plot to R object

      } else if (isRecordedPlot) { # function was called from editImage to resize the plot

        .redrawPlot(plot2draw) #(see below)
      } else if (inherits(plot2draw, "qgraph")) {

        plot(plot2draw)
        # qgraph:::plot.qgraph(plot2draw)

      } else {
        jaspQualityControl:::log_time(plot(plot2draw))
      }

    }

    # Save path & plot object to output
    image[["png"]] <- relativePathpng

    if (obj) {
      image[["obj"]]         <- plot2draw
    }

    image[["editOptions"]] <- jaspQualityControl:::log_time(jaspGraphs::plotEditingOptions(plot, asJSON = TRUE))

    image[["interactive"]] <- ggplot2::is.ggplot(plot) || inherits(plot, "jaspMatrixPlot")
    if (image[["interactive"]] )
      tryCatch(
        {
          jsonOrTryError <- jaspQualityControl:::log_time(jaspGraphs::convertGgplotToPlotly(plot))

          if (exists(".fromRCPP")) {
            if (isTryError(jsonOrTryError)) {
              image[["interactiveConvertError"]] = gettextf("The following error occured while converting a ggplot to plotly: %s", .extractErrorMessage(jsonOrTryError))
            } else {

              if (!is.null(relativePathJson) && nzchar(relativePathJson)) {
                locationPlotly <- list(root = location$root, relativePath = relativePathJson)
              } else {
                locationPlotly <- jaspBase:::.fromRCPP(".requestTempFileNameNative", "json")
              }
              fullPathPlotly  <- paste(locationPlotly$root, locationPlotly$relativePath, sep="/")
              plotlyJsonFile  <- file(fullPathPlotly)
              on.exit(close(plotlyJsonFile), add = TRUE)
              jaspQualityControl:::log_time(writeLines(jsonOrTryError, plotlyJsonFile))

              if(file.exists(fullPathPlotly)) {
                image[["interactiveJsonData"]] <- locationPlotly$relativePath
              }
              else
              {
                image[["interactiveJsonData"]]      <- ""
                image[["interactiveConvertError"]]  <- gettext("No interactive plot generated...")
              }
            }
          }
        },
        error	= function(e) {
          image[["interactiveConvertError"]]  <- e
        })

    return(image)
  },
  name    = "writeImageJaspResults",
  package = "jaspBase"
)
print(jaspBase:::writeImageJaspResults)

jaspTools::setPkgOption("module.dirs", ".")
jaspTools::setPkgOption("reinstall.modules", FALSE)

datapath <- testthat::test_path("datasets/doeAnalysis/2level5facFull.csv")

context("[Quality Control] DoE Analysis")
.numDecimals <- 2

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$plotNorm <- TRUE
options$plotHist <- TRUE
options$plotFitted <- TRUE
options$plotRunOrder <- TRUE
options$fourInOneResidualPlot <- TRUE
options$plotPareto <- TRUE
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("D", "E"))
)
set.seed(123)
# profvis::profvis({
#   results <- runAnalysis("doeAnalysis", datapath, options)
# })

# debugonce(jaspQualityControl::doeAnalysis)
results <- jaspQualityControl:::log_time(runAnalysis("doeAnalysis", datapath, options))

test_that("27.1 Factorial design plots Matrix residual plot matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_fourInOneResidualPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-residual-plot27")
})

test_that("27.2 Factorial design plots Residuals versus Fitted Values plot matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_plotFitted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-fitted-values27")
})

test_that("27.3 Factorial design plots Histogram of Residuals plot matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_plotHist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-residuals27")
})

test_that("27.4 Factorial design plotsNormal Probability Plot of Residuals matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_plotNorm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "normal-probability-plot-of-residuals27")
})

test_that("27.5 Factorial design plotsPareto Chart of Standardized Effects plot matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_plotPareto"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "pareto-chart-of-standardized-effects27")
})

test_that("27.6 Factorial design plotsResiduals versus Run Order plot matches", {
  plotName <- results[["results"]][["Result"]][["collection"]][["Result_plotRunOrder"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-run-order27")
})

test_that("27.7 Factorial design plotsANOVA table results match", {
  table <- results[["results"]][["Result"]][["collection"]][["Result_tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(118.542221893211, 1778.13332839816, 15, 2.63173059967985, 0.0318832484215919,
                                      "Model", "", 906.296316752094, 5, "", "", "<unicode> Linear terms",
                                      162.13896103187, 162.13896103187, 1, 3.59961251217536, 0.0759912026029757,
                                      "<unicode> <unicode> A", 243.097271810399, 243.097271810399,
                                      1, 5.39695071261994, 0.0336761387549081, "<unicode> <unicode> B",
                                      0.437571792073413, 0.437571792073413, 1, 0.00971443808260777,
                                      0.922710410988336, "<unicode> <unicode> C", 64.4409045780405,
                                      64.4409045780405, 1, 1.43063878625335, 0.249085400175541, "<unicode> <unicode> D",
                                      436.181607539711, 436.181607539711, 1, 9.68357489210809, 0.00671125493403696,
                                      "<unicode> <unicode> E", "", 871.837011646071, 10, "", "", "<unicode> Interaction terms",
                                      67.4016021614681, 67.4016021614681, 1, 1.49636860219795, 0.238942285824101,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 47.6509408633181,
                                      47.6509408633181, 1, 1.05788838078723, 0.318996088964146, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      18.5769765311749, 18.5769765311749, 1, 0.412423496082852, 0.529836840772662,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 59.1507367552762,
                                      59.1507367552762, 1, 1.3131928980773, 0.268668089382453, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      134.516684996498, 134.516684996498, 1, 2.98637624990437, 0.103214709350105,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 84.5899429388892,
                                      84.5899429388892, 1, 1.87796329191461, 0.189487611022757, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      1.79916468907368, 1.79916468907368, 1, 0.0399428717504902, 0.844110325478541,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 51.6884900877184,
                                      51.6884900877184, 1, 1.14752515047035, 0.299961265674163, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      210.572179125343, 210.572179125343, 1, 4.67486806299832, 0.0461007806065773,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>E", 195.890293497311,
                                      195.890293497311, 1, 4.34891864977489, 0.0534069008645126, "<unicode> <unicode> D<unicode><unicode><unicode>E",
                                      45.0434485610464, 720.695176976742, 16, "", "", "Error", "",
                                      2498.82850537491, 31, "", "", "Total"))
})

test_that("27.8 Factorial design plotsCoded Coefficients table results match", {
  table <- results[["results"]][["Result"]][["collection"]][["Result_tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 101.699596592187, "", 9.7648722242665e-23, 1.18642646950104,
                                      "(Intercept)", 85.71925796207, "", "A", 2.25096480031251, 4.50192960062502,
                                      0.0759912026029757, 1.18642646950104, "A", 1.89726448134554,
                                      1, "B", -2.75622744781249, -5.51245489562499, 0.0336761387549081,
                                      1.18642646950104, "B", -2.32313381289583, 1, "C", 0.116936386562506,
                                      0.233872773125013, 0.922710410988331, 1.18642646950104, "C",
                                      0.0985618490218655, 1, "D", 1.41907655468751, 2.83815310937502,
                                      0.249085400175541, 1.18642646950104, "D", 1.19609313443952,
                                      1, "E", -3.69197443593749, -7.38394887187498, 0.00671125493403696,
                                      1.18642646950104, "E", -3.11184429110907, 1, "AB", 1.45130977656249,
                                      2.90261955312499, 0.2389422858241, 1.18642646950104, "A<unicode>B",
                                      1.22326146109405, 1, "AC", 1.22028353343749, 2.44056706687498,
                                      0.318996088964146, 1.18642646950104, "A<unicode>C", 1.02853700992586,
                                      1, "AD", -0.761925532187506, -1.52385106437501, 0.529836840772662,
                                      1.18642646950104, "A<unicode>D", -0.6422020679528, 1, "AE",
                                      -1.35958101031251, -2.71916202062502, 0.268668089382452, 1.18642646950104,
                                      "A<unicode>E", -1.14594628935099, 1, "BC", -2.05027959218751,
                                      -4.10055918437501, 0.103214709350105, 1.18642646950104, "B<unicode>C",
                                      -1.72811349450908, 1, "BD", 1.6258646059375, 3.25172921187499,
                                      0.189487611022757, 1.18642646950104, "B<unicode>D", 1.37038800779729,
                                      1, "BE", -0.237115787187507, -0.474231574375015, 0.844110325478542,
                                      1.18642646950104, "B<unicode>E", -0.199857128345449, 1, "CD",
                                      1.27093088531249, 2.54186177062499, 0.299961265674163, 1.18642646950104,
                                      "C<unicode>D", 1.07122600345135, 1, "CE", -2.5652252528125,
                                      -5.13045050562501, 0.0461007806065773, 1.18642646950104, "C<unicode>E",
                                      -2.16214432057583, 1, "DE", 2.4741810103125, 4.94836202062499,
                                      0.0534069008645126, 1.18642646950104, "D<unicode>E", 2.08540611147443,
                                      1))
})

test_that("27.9 Factorial design plotsRegression Equation in Coded Units table results match", {
  table <- results[["results"]][["Result"]][["collection"]][["Result_tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.7 + 2.25 A - 2.76 B + 0.12 C + 1.42 D - 3.69 E + 1.45 AB + 1.22 AC - 0.76 AD - 1.36 AE - 2.05 BC + 1.63 BD - 0.24 BE + 1.27 CD - 2.57 CE + 2.47 DE"
                                 ))
})

test_that("27.10 Factorial design plotsModel Summary table results match", {
  table <- results[["results"]][["Result"]][["collection"]][["Result_tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.441199385076272, 0, 0.711586779394205, 6.71144161570719))
})
