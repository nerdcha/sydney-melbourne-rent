This repo has the data and code used to generate the graphs in my blog post "[The rent is too damn high, and also not high enough](http://www.jamiehall.cc/post/the-rent-is-too-damn-high-and-also-not-high-enough)", from September 2015.

# Graph sources and notes

## Sydney Data

All the Sydney data comes from the Housing section of the NSW Department of Family and Community Services: see their [Rent and Sales Report](http://www.housing.nsw.gov.au/About+Us/Reports+Plans+and+Papers/Rent+and+Sales+Reports/Latest+Issue/Issue+112.htm), Issue 112.

Their purchase price estimates are available one quarter behind the median rent series, so I extrapolated them into the June quarter using the RP Data [estimate](http://www.abc.net.au/news/2015-07-01/property-prices-reaccelerate-in-sydney-melbourne/6585944) for the Sydney metro area. 

## Melbourne Data

The Victorian Department of Human Services has a [Rental Report](http://www.dhs.vic.gov.au/about-the-department/documents-and-resources/research,-data-and-statistics/current-rental-report) similar to the NSW one, but it's less timely, so I only have data up to the March quarter. They only supply data on new rental bonds lodged in the quarter.

I couldn't find any kind of non-paywalled house price series for Melbourne.

## Median Rents Plot

This is a plot of the median Sydney rent for houses with any number of bedrooms in Inner Ring suburbs: that's west to Ashfield, south to Botany Bay, and north to Lane Cove. The Melbourne line is the closest equivalent I could find in the public data: median *new* rents for two-bedroom houses in the LGAs of Melbourne, Yarra, and Port Phillip, combined together via a simple average.

## Affordability Plot

The numerators of these percentage shares come from the rent data described above. In particular, it's the median rent for all houses in the Greater Sydney area, and the median new rent on two-bedroom houses in the Melbourne Metro zone.

The denominators are the NSW and VIC average ordinary-time earnings for full-time workers (known as AWOTE). The wage data is available twice-yearly, so I used linear interpolation to make it quarterly. It's only available statewide, not city by city, at that frequency, so that will make the estimated rent share too high. On the other hand, this is an average salary, not a median (which is more representative of a typical wage-earner's salary), which will make the estimated rent share too low.

## Rental Yields Plot

This is the sketchiest of the three graphs, because it's not a plot of the median rental yield over time, but rather it's the ratio of the median rent to the median house price. So there could be problems from Jensen's inequality, and most certainly there are compositional differences in the sets of housing included in the numerator and the denominator. If you'd like a more accurate calculation, see the Tulip--Fox paper.


