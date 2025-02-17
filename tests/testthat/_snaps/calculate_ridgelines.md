# calculate_ridgelines returns expected data structure

    Code
      str(utils::head(result))
    Output
      'data.frame':	6 obs. of  4 variables:
       $ x        : num  12.1 12.1 12.1 12.1 12.1 ...
       $ y        : num  47.5 47.5 47.5 47.5 47.5 ...
       $ elevation: num  630 631 628 623 620 623
       $ group    : int  1 1 1 1 1 1
    Code
      range(result$x)
    Output
      [1] 12.12689 12.42878
    Code
      range(result$y)
    Output
      [1] 47.52831 47.62777
    Code
      range(result$elevation)
    Output
      [1]  466 2259
    Code
      range(result$group)
    Output
      [1]  1 30
    Code
      nrow(result)
    Output
      [1] 7770
    Code
      length(unique(result$group))
    Output
      [1] 30

---

    Code
      length(unique(result_15_lines$group))
    Output
      [1] 15
    Code
      nrow(result_15_lines)
    Output
      [1] 3885

