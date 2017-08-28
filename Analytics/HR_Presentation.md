<style>
body {
    overflow: scroll;
}
</style>


HR Presentation
========================================================
author:Alex Surin
date:08/26/2017
autosize: true

Project Overview
========================================================

- Analysis of 'Human Resources' Data from kaggle
  + Data Exploration
  + Machine Learning Approach
  + Findings and Discussion
  
### Why are our best and most experienced employees leaving prematurely?




![alt text](https://www.kaggle.com/static/images/site-logo.png)

Data Exploration: Given
========================================================
   

   
  
*Dependent Variable:
  + Whether the employee has left

*Independent Variables:
  + Satisfaction Level
  + Last evaluation
  + Number of projects
  + Average monthly hours
  + Time spent at the company
  + Whether they have had a work accident
  + Whether they have had a promotion in the last 5 years
  + Departments (column sales)
  + Salary

***

### Number of Records


```
[1] 14999
```

### Number of Variables


```
[1] 10
```

  
Data Exploration: Data Summary
========================================================
Quick Look and Summary of HR data

```
  satisfaction_level last_evaluation number_project average_montly_hours
1               0.38            0.53              2                  157
2               0.80            0.86              5                  262
3               0.11            0.88              7                  272
4               0.72            0.87              5                  223
5               0.37            0.52              2                  159
6               0.41            0.50              2                  153
  time_spend_company Work_accident left promotion_last_5years sales salary
1                  3             0    1                     0 sales    low
2                  6             0    1                     0 sales medium
3                  4             0    1                     0 sales medium
4                  5             0    1                     0 sales    low
5                  3             0    1                     0 sales    low
6                  3             0    1                     0 sales    low
```

```
 satisfaction_level last_evaluation  number_project  average_montly_hours
 Min.   :0.0900     Min.   :0.3600   Min.   :2.000   Min.   : 96.0       
 1st Qu.:0.4400     1st Qu.:0.5600   1st Qu.:3.000   1st Qu.:156.0       
 Median :0.6400     Median :0.7200   Median :4.000   Median :200.0       
 Mean   :0.6128     Mean   :0.7161   Mean   :3.803   Mean   :201.1       
 3rd Qu.:0.8200     3rd Qu.:0.8700   3rd Qu.:5.000   3rd Qu.:245.0       
 Max.   :1.0000     Max.   :1.0000   Max.   :7.000   Max.   :310.0       
                                                                         
 time_spend_company Work_accident         left       
 Min.   : 2.000     Min.   :0.0000   Min.   :0.0000  
 1st Qu.: 3.000     1st Qu.:0.0000   1st Qu.:0.0000  
 Median : 3.000     Median :0.0000   Median :0.0000  
 Mean   : 3.498     Mean   :0.1446   Mean   :0.2381  
 3rd Qu.: 4.000     3rd Qu.:0.0000   3rd Qu.:0.0000  
 Max.   :10.000     Max.   :1.0000   Max.   :1.0000  
                                                     
 promotion_last_5years         sales         salary    
 Min.   :0.00000       sales      :4140   high  :1237  
 1st Qu.:0.00000       technical  :2720   low   :7316  
 Median :0.00000       support    :2229   medium:6446  
 Mean   :0.02127       IT         :1227                
 3rd Qu.:0.00000       product_mng: 902                
 Max.   :1.00000       marketing  : 858                
                       (Other)    :2923                
```

We see that variables Sales and Salary are categorical, so they will need to be converted
to factors.



Data Exploration: Categorical Variables
========================================================

### Salary


```r
data<-read.csv("/Users/asurin/Documents/GITHUB_PUBLIC/HR_comma_sep.csv")
pie.sal<-as.character(data$salary)

l<-list(hchart(pie.sal, type = "pie"))

htmltools::tagList(l)
```

<!--html_preserve--><div id="htmlwidget-56441a22cc03b47cf865" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-56441a22cc03b47cf865">{"x":{"hc_opts":{"title":{"text":null},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":false},"plotOptions":{"series":{"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10},"xAxis":{"type":"category"},"series":[{"data":[{"name":"high","y":1237},{"name":"low","y":7316},{"name":"medium","y":6446}],"type":"pie"}]},"theme":{"chart":{"backgroundColor":"transparent"}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

***

### Sales





```
Error in file(con, "rb") : cannot open the connection
```
