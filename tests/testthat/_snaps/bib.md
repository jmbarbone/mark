# snapshots()

    Code
      print(bib_df)
    Output
                   key         field           author                 title
      1        article       article      Peter Adams The title of the work
      2           book          book  Peter Babington The title of the work
      3        booklet       booklet     Peter Caxton The title of the work
      4     conference    conference     Peter Draper The title of the work
      5         inbook        inbook      Peter Eston The title of the work
      6   incollection  incollection   Peter Farindon The title of the work
      7         manual        manual  Peter Gainsford The title of the work
      8  mastersthesis mastersthesis    Peter Harwood The title of the work
      9           misc          misc      Peter Isley The title of the work
      10     phdthesis     phdthesis     Peter Joslin The title of the work
      11   proceedings   proceedings             <NA> The title of the work
      12    techreport    techreport    Peter Lambert The title of the work
      13   unpublished   unpublished Peter Marcheford The title of the work
                         journal year number   pages month             note volume
      1  The name of the journal 1993      2 201-213     7 An optional note      4
      2                     <NA> 1993   <NA>    <NA>     7 An optional note      4
      3                     <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
      4                     <NA> 1993   <NA>     213     7 An optional note      4
      5                     <NA> 1993   <NA> 201-213     7 An optional note      4
      6                     <NA> 1993   <NA> 201-213     7 An optional note      4
      7                     <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
      8                     <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
      9                     <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
      10                    <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
      11                    <NA> 1993   <NA>    <NA>     7 An optional note      4
      12                    <NA> 1993      2    <NA>     7 An optional note   <NA>
      13                    <NA> 1993   <NA>    <NA>     7 An optional note   <NA>
                         publisher series                      address edition
      1                       <NA>   <NA>                         <NA>    <NA>
      2  The name of the publisher     10                  The address       3
      3                       <NA>   <NA> The address of the publisher    <NA>
      4              The publisher      5 The address of the publisher    <NA>
      5  The name of the publisher      5 The address of the publisher       3
      6  The name of the publisher      5 The address of the publisher       3
      7                       <NA>   <NA> The address of the publisher       3
      8                       <NA>   <NA> The address of the publisher    <NA>
      9                       <NA>   <NA>                         <NA>    <NA>
      10                      <NA>   <NA> The address of the publisher    <NA>
      11 The name of the publisher      5 The address of the publisher    <NA>
      12                      <NA>   <NA> The address of the publisher    <NA>
      13                      <NA>   <NA>                         <NA>    <NA>
               isbn         howpublished             booktitle         editor
      1        <NA>                 <NA>                  <NA>           <NA>
      2  3257227892                 <NA>                  <NA>           <NA>
      3        <NA> How it was published                  <NA>           <NA>
      4        <NA>                 <NA> The title of the book     The editor
      5        <NA>                 <NA>                  <NA>           <NA>
      6        <NA>                 <NA> The title of the book     The editor
      7        <NA>                 <NA>                  <NA>           <NA>
      8        <NA>                 <NA>                  <NA>           <NA>
      9        <NA> How it was published                  <NA>           <NA>
      10       <NA>                 <NA>                  <NA>           <NA>
      11       <NA>                 <NA>                  <NA> Peter Kidwelly
      12       <NA>                 <NA>                  <NA>           <NA>
      13       <NA>                 <NA>                  <NA>           <NA>
             organization chapter                   school
      1              <NA>    <NA>                     <NA>
      2              <NA>    <NA>                     <NA>
      3              <NA>    <NA>                     <NA>
      4  The organization    <NA>                     <NA>
      5              <NA>       8                     <NA>
      6              <NA>       8                     <NA>
      7  The organization    <NA>                     <NA>
      8              <NA>    <NA> The school of the thesis
      9              <NA>    <NA>                     <NA>
      10             <NA>    <NA> The school of the thesis
      11 The organization    <NA>                     <NA>
      12             <NA>    <NA>                     <NA>
      13             <NA>    <NA>                     <NA>
                            institution
      1                            <NA>
      2                            <NA>
      3                            <NA>
      4                            <NA>
      5                            <NA>
      6                            <NA>
      7                            <NA>
      8                            <NA>
      9                            <NA>
      10                           <NA>
      11                           <NA>
      12 The institution that published
      13                           <NA>

---

    Code
      print(bib_list)
    Output
      article ------------------------------------------------------------------------
        field      article
        author     Peter Adams
        title      The title of the work
        journal    The name of the journal
        year       1993
        number     2
        pages      201-213
        month      7
        note       An optional note
        volume     4
      
      book ---------------------------------------------------------------------------
        field        book
        author       Peter Babington
        title        The title of the work
        publisher    The name of the publisher
        year         1993
        volume       4
        series       10
        address      The address
        edition      3
        month        7
        note         An optional note
        isbn         3257227892
      
      booklet ------------------------------------------------------------------------
        field           booklet
        title           The title of the work
        author          Peter Caxton
        howpublished    How it was published
        address         The address of the publisher
        month           7
        year            1993
        note            An optional note
      
      conference ---------------------------------------------------------------------
        field           conference
        author          Peter Draper
        title           The title of the work
        booktitle       The title of the book
        year            1993
        editor          The editor
        volume          4
        series          5
        pages           213
        address         The address of the publisher
        month           7
        organization    The organization
        publisher       The publisher
        note            An optional note
      
      inbook -------------------------------------------------------------------------
        field        inbook
        author       Peter Eston
        title        The title of the work
        chapter      8
        pages        201-213
        publisher    The name of the publisher
        year         1993
        volume       4
        series       5
        address      The address of the publisher
        edition      3
        month        7
        note         An optional note
      
      incollection -------------------------------------------------------------------
        field        incollection
        author       Peter Farindon
        title        The title of the work
        booktitle    The title of the book
        publisher    The name of the publisher
        year         1993
        editor       The editor
        volume       4
        series       5
        chapter      8
        pages        201-213
        address      The address of the publisher
        edition      3
        month        7
        note         An optional note
      
      manual -------------------------------------------------------------------------
        field           manual
        title           The title of the work
        author          Peter Gainsford
        organization    The organization
        address         The address of the publisher
        edition         3
        month           7
        year            1993
        note            An optional note
      
      mastersthesis ------------------------------------------------------------------
        field      mastersthesis
        author     Peter Harwood
        title      The title of the work
        school     The school of the thesis
        year       1993
        address    The address of the publisher
        month      7
        note       An optional note
      
      misc ---------------------------------------------------------------------------
        field           misc
        author          Peter Isley
        title           The title of the work
        howpublished    How it was published
        month           7
        year            1993
        note            An optional note
      
      phdthesis ----------------------------------------------------------------------
        field      phdthesis
        author     Peter Joslin
        title      The title of the work
        school     The school of the thesis
        year       1993
        address    The address of the publisher
        month      7
        note       An optional note
      
      proceedings --------------------------------------------------------------------
        field           proceedings
        title           The title of the work
        year            1993
        editor          Peter Kidwelly
        volume          4
        series          5
        address         The address of the publisher
        month           7
        organization    The organization
        publisher       The name of the publisher
        note            An optional note
      
      techreport ---------------------------------------------------------------------
        field          techreport
        author         Peter Lambert
        title          The title of the work
        institution    The institution that published
        year           1993
        number         2
        address        The address of the publisher
        month          7
        note           An optional note
      
      unpublished --------------------------------------------------------------------
        field     unpublished
        author    Peter Marcheford
        title     The title of the work
        note      An optional note
        month     7
        year      1993

---

    Code
      print(bib_entry)
    Output
      key        article
      field      article
      author     Peter Adams
      title      The title of the work
      journal    The name of the journal
      year       1993
      number     2
      pages      201-213
      month      7
      note       An optional note
      volume     4

