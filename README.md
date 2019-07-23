# stockoption-report
Simple reports to show the financial exposures of a portfolio of stock options

This shows the profit and loss (P&L) and risk exposures for a basket of call and put options against a single 
underlying stock.  It can show these exposures for a matrix of scenerios indexed by future dates and future
stock prices.  

There's a version in OptionAnalysis.R which creates a text report and ggplot2 plots.  There's also a version 
in OptionAnalysisReport.Rmd which uses knitr to create a HTML report with tables and graphs.
