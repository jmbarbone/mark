# arrow prints something to stdout()

    Code
      write_file_md5(quick_dfl(a = 1), method = "feather")
    Output
      <Feather file>
      [1 x 1] @ /tmp/<temp>PT/<temp>
      * 'a': double

---

    Code
      write_file_md5(quick_dfl(a = 1), method = "parquet")
    Output
      # A data frame: 2 x 12
        file_name        name  r_type type  type_length repetition_type converted_type
        <chr>            <chr> <chr>  <chr>       <int> <chr>           <chr>         
      1 /tmp/<temp>PT~ sche~ <NA>   <NA>           NA <NA>            <NA>          
      2 /tmp/<temp>PT~ a     double DOUB~          NA REQUIRED        <NA>          
      # i 5 more variables: logical_type <I<list>>, num_children <int>, scale <int>,
      #   precision <int>, field_id <int>

