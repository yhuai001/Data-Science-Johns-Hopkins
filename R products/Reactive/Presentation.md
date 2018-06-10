MPG prediction
========================================================
author: yhuai
date: 2018-05-25
autosize: true

Overview
========================================================

Course Project: Shiny Application and Reproducible Pitch
    
- am	 Transmission (0 = automatic, 1 = manual)
- wt	 Weight (1000 lbs)
- hp	 Gross horsepower

Dataset
========================================================


```r
str(mtcars)
```

```
'data.frame':	32 obs. of  11 variables:
 $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
 $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
 $ disp: num  160 160 108 258 360 ...
 $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
 $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
 $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
 $ qsec: num  16.5 17 18.6 19.4 17 ...
 $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
 $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
 $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
 $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```

Shiny UI
========================================================

<!--html_preserve--><div class="container-fluid">
<h2>MPG Prediction</h2>
<div class="row">
<div class="col-sm-4">
<form class="well">
<span class="help-block">This application predicts the Miles/(US) gallon of a car based on its characteristics.</span>
<div class="form-group shiny-input-container">
<label class="control-label" for="am">Transmission (0 = automatic, 1 = manual)</label>
<div>
<select id="am"><option value="*" selected>Please select</option>
<option value="0">0</option>
<option value="1">1</option></select>
<script type="application/json" data-for="am" data-nonempty="">{}</script>
</div>
</div>
<div class="form-group shiny-input-container">
<label class="control-label" for="hp">What is the Gross horsepower of the car?</label>
<input class="js-range-slider" id="hp" data-min="52" data-max="335" data-from="100" data-step="1" data-grid="true" data-grid-num="9.75862068965517" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="0.353356890459364" data-data-type="number"/>
</div>
<div class="form-group shiny-input-container">
<label class="control-label" for="wt">What is the Weight (1000 lbs) of the car?</label>
<input class="js-range-slider" id="wt" data-min="1.5" data-max="5.5" data-from="3" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="2.5" data-data-type="number"/>
</div>
<div>
<button type="submit" class="btn btn-primary">Submit</button>
</div>
</form>
</div>
<div class="col-sm-8">
<div id="plot1" class="shiny-plot-output" style="width: 100% ; height: 400px"></div>
<div id="plot2" class="shiny-plot-output" style="width: 100% ; height: 400px"></div>
<h3>Predicted the car MPG is:</h3>
<div id="result" class="shiny-text-output"></div>
</div>
</div>
</div><!--/html_preserve-->

Shiny Server
========================================================


```r
library(shiny)
library(ggplot2)

shinyServer(function(input, output){
    output$result <- renderText({
       
        model <- lm(mpg ~ hp , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                                    wt = input$wt))
        predmpg <- round(pred,2)
        predmpg
    })

    output$plot1 <- renderPlot({
       
        model <- lm(mpg ~ hp , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                              wt = input$wt
                                              ))
        
        plot <- ggplot(data=mtcars, aes(x = hp, y = mpg)) +
                    geom_point()+
                    geom_smooth(method = "lm")
        plot
    })
    
    output$plot2 <- renderPlot({
        
        model <- lm(mpg ~ wt , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                                    wt = input$wt
                                                    ))
        
        plot <- ggplot(data=mtcars, aes(x = wt, y = mpg)) +
            geom_point()+
            geom_smooth(method = "lm")
        plot
    })

})
```
