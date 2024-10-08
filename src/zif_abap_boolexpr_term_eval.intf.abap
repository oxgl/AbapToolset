interface ZIF_ABAP_BOOLEXPR_TERM_EVAL
  public .


  methods EVALUATE
    importing
      !IV_TERM type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL .
endinterface.
