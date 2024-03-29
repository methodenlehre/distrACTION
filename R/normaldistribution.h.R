
# This file is automatically generated, you probably don't want to edit this

NormaldistributionOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "NormaldistributionOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            DistributionFunction = FALSE,
            QuantileFunction = FALSE,
            QuantileFunctionType = "central",
            DistributionFunctionType = "lower",
            x1 = NULL,
            p = 0.5,
            x2 = 1,
            dp1 = 0,
            dp2 = 1, ...) {

            super$initialize(
                package="distrACTION",
                name="Normaldistribution",
                requiresData=FALSE,
                ...)

            private$..DistributionFunction <- jmvcore::OptionBool$new(
                "DistributionFunction",
                DistributionFunction,
                default=FALSE)
            private$..QuantileFunction <- jmvcore::OptionBool$new(
                "QuantileFunction",
                QuantileFunction,
                default=FALSE)
            private$..QuantileFunctionType <- jmvcore::OptionList$new(
                "QuantileFunctionType",
                QuantileFunctionType,
                options=list(
                    "central",
                    "cumulative"),
                default="central")
            private$..DistributionFunctionType <- jmvcore::OptionList$new(
                "DistributionFunctionType",
                DistributionFunctionType,
                options=list(
                    "lower",
                    "higher",
                    "interval"),
                default="lower")
            private$..x1 <- jmvcore::OptionNumber$new(
                "x1",
                x1)
            private$..p <- jmvcore::OptionNumber$new(
                "p",
                p,
                default=0.5,
                min=0,
                max=1)
            private$..x2 <- jmvcore::OptionNumber$new(
                "x2",
                x2,
                default=1)
            private$..dp1 <- jmvcore::OptionNumber$new(
                "dp1",
                dp1,
                default=0)
            private$..dp2 <- jmvcore::OptionNumber$new(
                "dp2",
                dp2,
                default=1)

            self$.addOption(private$..DistributionFunction)
            self$.addOption(private$..QuantileFunction)
            self$.addOption(private$..QuantileFunctionType)
            self$.addOption(private$..DistributionFunctionType)
            self$.addOption(private$..x1)
            self$.addOption(private$..p)
            self$.addOption(private$..x2)
            self$.addOption(private$..dp1)
            self$.addOption(private$..dp2)
        }),
    active = list(
        DistributionFunction = function() private$..DistributionFunction$value,
        QuantileFunction = function() private$..QuantileFunction$value,
        QuantileFunctionType = function() private$..QuantileFunctionType$value,
        DistributionFunctionType = function() private$..DistributionFunctionType$value,
        x1 = function() private$..x1$value,
        p = function() private$..p$value,
        x2 = function() private$..x2$value,
        dp1 = function() private$..dp1$value,
        dp2 = function() private$..dp2$value),
    private = list(
        ..DistributionFunction = NA,
        ..QuantileFunction = NA,
        ..QuantileFunctionType = NA,
        ..DistributionFunctionType = NA,
        ..x1 = NA,
        ..p = NA,
        ..x2 = NA,
        ..dp1 = NA,
        ..dp2 = NA)
)

NormaldistributionResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "NormaldistributionResults",
    inherit = jmvcore::Group,
    active = list(
        Inputs = function() private$.items[["Inputs"]],
        Outputs = function() private$.items[["Outputs"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Normal Distribution")
            self$add(jmvcore::Table$new(
                options=options,
                name="Inputs",
                title="Input values",
                rows=2,
                columns=list(
                    list(
                        `name`="ParametersColumn", 
                        `title`="Parameters", 
                        `type`="text"),
                    list(
                        `name`="DistributionFunctionColumn", 
                        `title`="'Compute probability'", 
                        `type`="text", 
                        `visible`="(DistributionFunction)"),
                    list(
                        `name`="QuantileFunctionColumn", 
                        `title`="'Compute quantile(s)'", 
                        `type`="text", 
                        `visible`="(QuantileFunction)"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="Outputs",
                title="Results",
                visible="(DistributionFunction || QuantileFunction)",
                rows=1,
                clearWith=list(
                    "group",
                    "alt",
                    "varEq"),
                columns=list(
                    list(
                        `name`="DistributionResultColumn", 
                        `title`="Probability", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(DistributionFunction)"),
                    list(
                        `name`="QuantileResultColumn", 
                        `title`="x1", 
                        `type`="number", 
                        `visible`="(QuantileFunction && QuantileFunctionType==\"cumulative\")", 
                        `superTitle`="Quantile"),
                    list(
                        `name`="QuantileLowerResultColumn", 
                        `title`="x1", 
                        `type`="number", 
                        `visible`="(QuantileFunction && QuantileFunctionType==\"central\")", 
                        `superTitle`="Quantiles"),
                    list(
                        `name`="QuantileUpperResultColumn", 
                        `title`="x2", 
                        `type`="number", 
                        `visible`="(QuantileFunction && QuantileFunctionType==\"central\")", 
                        `superTitle`="Quantiles"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="",
                width=500,
                height=400,
                renderFun=".plot"))}))

NormaldistributionBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "NormaldistributionBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "distrACTION",
                name = "Normaldistribution",
                version = c(1,0,0),
                options = options,
                results = NormaldistributionResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'na')
        }))

#' Normal Distribution
#'
#' 
#' @param DistributionFunction .
#' @param QuantileFunction .
#' @param QuantileFunctionType .
#' @param DistributionFunctionType .
#' @param x1 .
#' @param p .
#' @param x2 .
#' @param dp1 .
#' @param dp2 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$Inputs} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$Outputs} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$Inputs$asDF}
#'
#' \code{as.data.frame(results$Inputs)}
#'
#' @export
Normaldistribution <- function(
    DistributionFunction = FALSE,
    QuantileFunction = FALSE,
    QuantileFunctionType = "central",
    DistributionFunctionType = "lower",
    x1,
    p = 0.5,
    x2 = 1,
    dp1 = 0,
    dp2 = 1) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("Normaldistribution requires jmvcore to be installed (restart may be required)")


    options <- NormaldistributionOptions$new(
        DistributionFunction = DistributionFunction,
        QuantileFunction = QuantileFunction,
        QuantileFunctionType = QuantileFunctionType,
        DistributionFunctionType = DistributionFunctionType,
        x1 = x1,
        p = p,
        x2 = x2,
        dp1 = dp1,
        dp2 = dp2)

    analysis <- NormaldistributionClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

