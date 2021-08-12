# ---------------------------
##
## Script name: bug_demo.R
##
## Purpose of script: monthly forecasts were sometimes 
##					  seemingly random, and upon investigation 
##					  we found that the bizdays() adjustment
##					  broke down (silently) if the ts-object
##					  was initialized with a wrong date format
##
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021
## Date Modified: 2021-08-12
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
##
## Notes: This script is made public as
##        a part of a publication for
##        documentation and transparency
##        purposes.
##
## ---------------------------

# ts() accepts two kinds of "start indicators"
ts1 <- ts(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), start = decimal_date(as.Date("2009-08-01")),  # decimal_date
		  frequency = 12)
ts1

ts2 <- ts(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), start = c(2009, 8, 1),  # integer vector
		  frequency = 12) 
ts2

# The bizdays(x, FinCenter=) function wants a time series as x. 
# However, it gets very confused when we used a ts that 
# has been initialized with decimal_date()
bizdays(ts1, FinCenter = "Zurich")
bizdays(ts2, FinCenter = "Zurich")

# The "bug" went initially unnoticed because 
# decimal_date() outputs a whole integer at the 1st of January 
# (date I likely used for testing this)
ts3 <- ts(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), start = decimal_date(as.Date("2010-01-01")),  # decimal_date
		  frequency = 12)
ts3

# The resulting difference in performance was significant.