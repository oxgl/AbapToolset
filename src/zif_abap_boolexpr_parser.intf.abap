interface ZIF_ABAP_BOOLEXPR_PARSER
  public .


  methods EVALUATE
    importing
      !IV_EXPRESSION type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      ZCX_ABAP_BOOLEXPR_ERROR .
endinterface.
