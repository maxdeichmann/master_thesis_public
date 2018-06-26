
library(diverse)

data(pantheon)
ge <- geese
geese[1,1] <- 0
print(geese)
a <- diversity(geese, type='hh', category_row=TRUE)
print(a)