is.ts(rawbeer)
rawbeer
is.vector(rawbeer) # rawbeer is a vector
is.vector(beer) # beer is not
# no, it will still go into functions though,
tsdisplay(rawbeer) # note it cannot be identified as being seasonal, or having dates
help(ts)
# creat a time series object
newbeer.ts=ts(rawbeer,start=c(1991,1),end=c(1995,8),frequency=12)
is.ts(newbeer.ts)
newbeer.ts