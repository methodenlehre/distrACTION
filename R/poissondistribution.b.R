PoissonDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "PoissonDistributionClass",
  inherit = PoissonDistributionBase,
  private = list(
    
    
    
    ########### 1) Main-Function ##########
    .run = function() {
      
      
      ###### 1.1) Preparation ######
      ##### 1.1.1) Extraction of Input-Values #####
      # Is the distribution function selected?
      DistributionFunction <- self$options$DistributionFunction ### == TRUE|FALSE      
      # Is the quantile function selected?
      QuantileFunction <- self$options$QuantileFunction ### == TRUE|FALSE       
      # Which type of the distribution function is selected?
      DistributionFunctionType <- self$options$DistributionFunctionType ### == lower|higher|interval|is
      # The specification of the x value is extracted
      XValue <- self$options$x1
      # The specification of the p value is extracted
      Quantile <- self$options$p
      # The specification of the second value is extracted
      XValue2 <- self$options$x2
      # The specification of the first distribution parameter (Rate) is extracted
      DP1 <- self$options$dp1
      #The quantiles are recalculated if the central interval quantile is selected
      
      
      ##### 1.1.2) Definition of variables #####
      # The lower end of the distribution
      LowerTail <- 0
      # The upper end of the distribution
      UpperTail <- UpperTail <- ceiling(qpois(0.99999, DP1))
      # The number of values in the curve
      N <-  ceiling(qpois(0.99999, DP1))
      # Define a variable for the columname of dataframes
      Columnames <- c("X", "Prob")
      
      
      ##### 1.1.3) Label setting ##### 
      # Label for the distribution parameters
      InputLabel1 <- "λ = "
      DistributionFunctionTypeLabel <- ""
      # Label for the selected type of distribution function
      if (DistributionFunctionType=="is"){
        DistributionFunctionTypeLabel <- "Mode: P(X = x1)"}
      if (DistributionFunctionType=="lower"){
        DistributionFunctionTypeLabel <- "Mode: P(X \u2264 x1)"}
      if (DistributionFunctionType=="interval"){
        DistributionFunctionTypeLabel <- paste("Mode: x2 = ", XValue2, sep = "")}
      if (DistributionFunctionType=="higher"){
        DistributionFunctionTypeLabel <- "Mode: P(X \u2265 x1)"}
      
      
      ##### 1.1.4) Inputs table ######
      # The input matrix is created
      InputSummary <- matrix(ncol = 3, nrow = 2)
      # The values are transferred to the input matrix
      InputSummary <- matrix(ncol = 3, nrow = 2)
      InputSummary[1,1] <- paste(InputLabel1, DP1, sep = "")
      InputSummary[1,2] <- paste("x1 = ", XValue, sep = "")
      InputSummary[2,1] <- paste("")
      InputSummary[2,2] <- DistributionFunctionTypeLabel
      InputSummary[1,3] <- paste("p = ", Quantile, sep = "")
      InputSummary[2,3] <- paste("")
      # The input matrix is written to the corresponding result cell
      Inputs <- self$results$Inputs
      Inputs$setRow(rowNo=1, values=list(
        ParametersColumn=InputSummary[1,1],
        DistributionFunctionColumn=InputSummary[1,2],
        QuantileFunctionColumn=InputSummary[1,3]))
      Inputs$setRow(rowNo=2, values=list(
        ParametersColumn=InputSummary[2,1],
        DistributionFunctionColumn=InputSummary[2,2],
        QuantileFunctionColumn=InputSummary[2,3]))
      
      ###### 1.2) Quantile & Distribution calculation ######
      ##### 1.2.1) Calculations #####
      # The sequence with the values for the distribution is created
      x <- seq(LowerTail, UpperTail, length=N+1)
      # Calculation of the distribution density
      Density <- dpois(x, DP1)
      if(DistributionFunction=="TRUE"){ 
        # Calculation of the probability P(X <= x1)
        DistributionResult <- ppois(XValue, DP1)
        if(DistributionFunctionType=="higher"){
          # P(X >= x1) = 1 - P(X < x1) = 1 - (P(X <= x1) - P(X = x1)) = 1 - P(X <= x1) + P(X = x1)
          DistributionResult <- 1 - ppois(XValue, DP1) + dpois(XValue, DP1)}
        if(DistributionFunctionType == "interval"){
          # The result is the difference between the two quantiles P(x1 <= X <= x2) = P(X <= x2) - P(X <= x1) + P(X = x1)
          DistributionResult <- ppois(XValue2, DP1) - ppois(XValue, DP1) + dpois(XValue, DP1)}}
      if(QuantileFunction=="TRUE"){
          # The x-value of the percentil is calculated
          QuantileResult <- qpois(Quantile, DP1)}
      
      
      ##### 1.2.2) Output Table #####
      # Outputlabels are created empty...
      OutputLabel11 <- ""
      OutputLabel12 <- ""
      OutputLabel21 <- ""
      OutputLabel22 <- ""
      # ... and filled by conditions. 
      if(DistributionFunction=="TRUE"){
        if(DistributionFunctionType=="is"){
          DistributionResult <- dpois(XValue, DP1)}
        OutputLabel11 <- DistributionResult}
      if(QuantileFunction=="TRUE"){
        OutputLabel12 <- QuantileResult}
      # The Output-Matrix is written to the according Result-Frame
      Outputs <- self$results$Outputs
      Outputs$setRow(rowNo=1, values=list(
        DistributionResultColumn=OutputLabel11,
        QuantileResultColumn=OutputLabel12,
        QuantileLowerResultColumn=OutputLabel12,
        QuantileUpperResultColumn=OutputLabel22))
      
      
      ###### 1.3) Plot preparation ######
      ##### 1.3.1) Data packing #####
      # The results are combined in a Dataframe
      Datas <- data.frame(x, Density)
      # Names of the colums
      colnames(Datas) <-  Columnames
      # For calculating the searched area, a new variable is created  
      MainCurveData <- as.data.frame(Datas)
      
      
      ##### 1.3.2) Remove values #####
      # Values which are not part of the searched area are removed
      if (DistributionFunction=="TRUE") {
        if (DistributionFunctionType=="is") {
          MainCurveData$Prob[MainCurveData$X != XValue] <- NA
          MainCurveData$X[MainCurveData$X != XValue] <- NA}          
        if (DistributionFunctionType=="lower") {
          MainCurveData$Prob[MainCurveData$X > XValue] <- NA
          MainCurveData$X[MainCurveData$X > XValue] <- NA}
        if (DistributionFunctionType=="higher") {
          MainCurveData$Prob[MainCurveData$X < XValue] <- NA
          MainCurveData$X[MainCurveData$X < XValue] <- NA}
        if (DistributionFunctionType=="interval") {
          MainCurveData$Prob[MainCurveData$X < XValue] <- NA
          MainCurveData$X[MainCurveData$X < XValue] <- NA        
          MainCurveData$Prob[MainCurveData$X > XValue2] <- NA
          MainCurveData$X[MainCurveData$X > XValue2] <- NA}}
      
      
      ##### 1.3.3) Calculations for the plot #####
      # The transcparency of the lower quantile segment is defined
      QuantileAlphaLow <- 1
      # The transcparency of the upper quantile segment is defined
      QuantileAlphaHigh <- 1
      # The text for the quantiles legend is defined
      QuantileLabel <- "Quantile"
      # The size of the legends text is defined
      Textsize <- 16
      # The lowest segment of the x-axis is defined
      LowerAxisSegment <- LowerTail
      # The highest segment of the x-axis is defined
      HigherAxisSegment <- UpperTail
      # The segments of the x-axis are defined
      AxisSegments <- seq(LowerAxisSegment, HigherAxisSegment, by = 1)  
      # A variable for the position of the upper quantile is defined
      HigherSegment <- NA
      # A variable for the position of the lower quantile is defined
      LowerSegment <- NA
      # A variable for the length of the upper quantile is defined
      HigherSegmentLength <- NA
      # A variable for the length of the lower quantile is defined
      LowerSegmentLength <- NA
      # The position of the upper quantile is calculated
      if(QuantileFunction=="TRUE"){
          HigherSegment <- QuantileResult
          HigherSegmentLength <- dpois(HigherSegment, DP1)
          # The length of the quantile segment is changed if it is too short
          if((HigherSegmentLength*18)<(max(Datas$Prob))){
            HigherSegmentLength <- ((max(Datas$Prob))/18)}
          # the position of the lower quantile is the same.
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength
        
        
        ##### 1.3.4) Improvements of the plot by conditions #####
        # A value to check if the quantiles are within the x-axis is calculated      
          HighLineCheck <- qpois(Quantile, DP1)
        # Changes are done if the quantile is outside the x-axis
          if(HighLineCheck>HigherAxisSegment){
            QuantileLabel <- "Quantile out of range"
            QuantAlphaLow <- 0
            QuantAlphaHigh <- 0
            Textsize <- 10
            HigherSegment <- HigherAxisSegment
            LowerSegment <- HigherAxisSegment}
          if(HighLineCheck<LowerAxisSegment){
            QuantileLabel <- "Quantile out of range"
            QuantAlphaLow <- 0
            QuantAlphaHigh <- 0
            Textsize <- 10
            HigherSegment <- HigherAxisSegment
            LowerSegment <- HigherAxisSegment}}
      
      
      ##### 1.3.5) Submit datas for plot #####
      # The variables that are needed for the plot function are combined to a new variable
      Dataset <- cbind(Datas, MainCurveData[,2], MainCurveData[,])
      # The last two rows of the dataset are cleared
      Dataset[,4:5] <- NA
      # The plotvalues are assigned to the dataset
      Dataset[1,4] <- HigherSegment
      Dataset[2,4] <- LowerSegment
      Dataset[3,4] <- HigherSegmentLength
      Dataset[4,4] <- LowerSegmentLength
      Dataset[5,4] <- QuantileAlphaLow
      Dataset[6,4] <- QuantileAlphaHigh
      Dataset[7,4] <- QuantileLabel
      Dataset[8,4] <- Textsize
      Dataset[1:(length(AxisSegments)),5] <- AxisSegments        
      # The new variable is transferred to the plot-object
      image <- self$results$plot
      image$setState(Dataset)
      
      
      
      ###### 1.4) Error Messages #####
      #Error if XValue\u2265XValue2
      if(((DistributionFunction=="TRUE") & (DistributionFunctionType=="interval"))&(XValue>=XValue2)){
        Inputs$setError("x2 must be greater than x1. ")
        Outputs$setVisible(visible=FALSE)}},
    
    
    
    ########### 2.) Plot-Function ##########
    .plot=function(image, ...) {
      
      
      ###### 2.1) Extraction of values ######
      ##### 2.1.1) Extraction of the plot datas #####
      # The main dataset for the plot is extracted
      Dataset <- image$state
      # The datas are recreated as a variable for the plot
      PlotData <- Dataset[,1:3]
      colnames(PlotData) <- c("X", "Prob", "CurveProb")
      # The x-axis point of the higher quantile segment
      HigherSegment <- as.numeric(Dataset[1,4])
      # The x-axis point of the lower quantile segment
      LowerSegment <- as.numeric(Dataset[2,4])
      # The length of the higher quantile segment
      HigherSegmentLength <- as.numeric(Dataset[3,4])
      # The length of the lower quantile segment
      LowerSegmentLength <- as.numeric(Dataset[4,4])
      # The transparency of the lower quntile segment
      QuantileAlphaLow <- as.numeric(Dataset[5,4])
      # The transparency of the lower higher segment
      QuantileAlphaHigh <- as.numeric(Dataset[6,4])
      # The text for the quantiles legend 
      QuantileLabel <- Dataset[7,4]
      # The textsize of the legend
      Textsize <- Dataset[8,4]
      # The x-axis labels
      AxisSegments <- as.numeric(Dataset[,5])
      AxisSegments <- na.omit(AxisSegments)
      
      
      ##### 2.1.2) Extraction of inputvalues #####
      # Is the distribution function selected?
      DistributionFunction <- self$options$DistributionFunction ### == TRUE|FALSE
      # Is the quantile function selected?
      QuantileFunction <- self$options$QuantileFunction ### == TRUE|FALSE
      
      
      ###### 2.2) Definition of parameters for the plot ######
      # Size of the points
      Pointsize <- 0.000001
      # Linetype
      TypeOfLine <- "dashed"
      # Linewidth
      Linewidth <- 1
      # Color to fill points
      Color <- c("#e0bc6b", "#7b9ee6", "#9f9f9f")
      
      
      ###### 2.3) Creation of the plot ######
      ##### 2.3.1) Settings of the plot #####
      Plot <- ggplot(PlotData, mapping = aes(x=PlotData$X, y=PlotData$Prob))+
        # # The whole distribution is plottet as background
        geom_col(PlotData, mapping = aes(x=PlotData$X, y=PlotData$Prob), fill="grey")+
        # X-axis-label
        ggplot2::xlab("")+
        # Y-axis-label
        ggplot2::ylab("")+
        # Add x-axis scale
        scale_x_continuous(breaks = AxisSegments)
      
      
      ##### 2.3.2) Area ##### 
      if (DistributionFunction=="TRUE") {
        Plot <- Plot+
          # The area of the searched interval is marked
          geom_col(PlotData, mapping = aes(x=PlotData$X, y=PlotData$CurveProb, fill=" P (Area)"))+
          # Set the colors of the legend
          scale_fill_manual(values = Color)}
      
      
      ##### 2.3.3) Quantile #####
      if (QuantileFunction=="TRUE") {
        Plot <- Plot+
          # The lines of the quantiles are added
          geom_segment(aes(x=LowerSegment, y=0, xend=LowerSegment, yend=LowerSegmentLength,linetype=QuantileLabel),colour = Color[2], size = Linewidth,  alpha = QuantileAlphaLow)+
          geom_segment(aes(x=HigherSegment, y=0, xend=HigherSegment, yend=HigherSegmentLength, linetype=QuantileLabel),colour = Color[2], size = Linewidth,  alpha = QuantileAlphaHigh)+
          # Set linetype
          scale_linetype_manual(values=TypeOfLine)}
      
      
      ##### 2.3.4) Final adjustments of the plot #####
      Plot <- Plot+
        # Theme of the Plot
        theme_classic()+
        # Set fontsize of the legend
        theme(legend.text = element_text(size = Textsize))+
        # Remove legend title
        theme(legend.title=element_blank())
      
      
      ###### 2.4) Print the plot ######
      # Print the plot
      print(Plot)
      # Show the plot
      TRUE}))
