library(tidyverse)
weight =c(63:70)

Ratio = c(0.34,0.35,0.36,0.37)


round(weight*Ratio[1],1)

weight

table = data.frame()

for (i in 1:length(Ratio)){
  tmp =   data.frame( round(weight*Ratio[i],1))
  names(tmp)=Ratio[i]
  tmp %>% print()

}

table


iris %>% head(3) -> top3A
top3A

iris %>% slice(2:4) -> top3B
top3B

(iris %>% head(3) -> top3B)
top3B[1,1] = 5



sum(top3A != top3B)


setdiff(top3A,top3B) %>% typeof()
(setdiff(top3A,top3B) -> difAB)
difAB %>% unlist()

install.packages("compareDF")

library(compareDF)

compare_df(top3A,top3B)

old_df = data.frame(var1 = c("A", "B", "C"),
                    val1 = c(1, 2, 3))
new_df = data.frame(var1 = c("A", "B", "C"),
                    val1 = c(1, 2, 4))

old_df
new_df

ctable = compare_df(new_df, old_df, c("var1"))
ctable = compare_df(old_df, old_df, )



ctable
print(ctable$comparison_df)
ctable$html_output


install.packages("testthat")
library(testthat)

test_that("check",{
  expect_equal("abc","abc")
  expect_equal(new_df, old_df)

})

if (expect_equal("abc","abc"))){print("truee")}


expect_equal("abc","abc") %>% str()

expect_equal(new_df,new_df) %>% str()

expect_equal("abc","abcd") %>% str()

expect_equal(new_df,old_df) %>% str()

expect_equal("abc","abcd") %>%  typeof()

expect_equal(new_df,old_df) %>%  typeof()




(maru =  )
(batu = expect_equal("abc","abd") )

maru %>% str()
batu = str()


new_df
old_df


old_df = data.frame(var1 = c("A", "B", "C"),
                    val1 = c(1, 1, 1))
new_df = data.frame(var1 = c("A", "B", "C"),
                    val1 = c(1, 1, 2))



Examples
a <- 10
expect_equal(a, 10)

# Use expect_equal() when testing for numeric equality
sqrt(2) ^ 2 - 1
expect_equal(sqrt(2) ^ 2, 2)
# Neither of these forms take floating point representation errors into
# account
## Not run:
expect_true(sqrt(2) ^ 2 == 2)
expect_identical(sqrt(2) ^ 2, 2)

## End(Not run)

# You can pass on additional arguments to all.equal:
## Not run:
# Test the ABSOLUTE difference is within .002
expect_equal(10.01, 10, tolerance = .002, scale = 1)

## End(Not run)

# Test the RELATIVE difference is within .002
x <- 10
expect_equal(10.01, expected = x, tolerance = 0.002, scale = x)

# expect_equivalent ignores attributes
a <- b <- 1:3
names(b) <- letters[1:3]
expect_equivalent(a, b)



dat = c(1,2,3,4,5,6,30)
dat = c(1,2,3,4,5,6)
dat = c(2,5)

boxplot(dat)
