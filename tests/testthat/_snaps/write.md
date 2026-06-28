# arrow prints something to stdout()

    Code
      write_file_md5(dataframe(a = 1), method = "feather")
    Output
      Table
      1 rows x 1 columns
      $a <double>

---

    Code
      write_file_md5(dataframe(a = 1), method = "parquet")
    Output
      # A data frame: 2 x 14
        file_name  r_col name  r_type type  type_length repetition_type converted_type
        <chr>      <int> <chr> <chr>  <chr>       <int> <chr>           <chr>         
      1 <temp>    NA sche~ <NA>   <NA>           NA <NA>            <NA>          
      2 <temp>     1 a     double DOUB~          NA REQUIRED        <NA>          
      # i 6 more variables: logical_type <I<list>>, num_children <int>, scale <int>,
      #   precision <int>, field_id <int>, children <list>

