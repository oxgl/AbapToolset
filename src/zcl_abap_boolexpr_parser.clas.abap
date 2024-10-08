***************************************************************************
* Project:    ABAP Toolset                                                *
* License:    MIT                                                         *
* Git:        https://github.com/oxgl/AbapToolset                         *
***************************************************************************
"! <p class="shorttext synchronized" lang="en">ABAP Simple boolean expression parser</p>
class ZCL_ABAP_BOOLEXPR_PARSER definition
  public
  final
  create public .

public section.

  interfaces ZIF_ABAP_BOOLEXPR_PARSER .

  "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  "!
  "! @parameter io_term_eval | <p class="shorttext synchronized" lang="en">ABAP Simple boolean expression parser term evaluator</p>
  methods CONSTRUCTOR
    importing
      !IO_TERM_EVAL type ref to ZIF_ABAP_BOOLEXPR_TERM_EVAL .
  "! <p class="shorttext synchronized" lang="en">Get parser instance</p>
  "!
  "! @parameter io_term_eval | <p class="shorttext synchronized" lang="en">ABAP Simple boolean expression parser term evaluator</p>
  "! @parameter ro_parser    | <p class="shorttext synchronized" lang="en">ABAP Simple boolean expression parser</p>
  class-methods GET_INSTANCE
    importing
      !IO_TERM_EVAL type ref to ZIF_ABAP_BOOLEXPR_TERM_EVAL
    returning
      value(RO_PARSER) type ref to ZIF_ABAP_BOOLEXPR_PARSER .
protected section.

  aliases EVALUATE
    for ZIF_ABAP_BOOLEXPR_PARSER~EVALUATE .

  data GO_TERM_EVAL type ref to ZIF_ABAP_BOOLEXPR_TERM_EVAL .
private section.

  types:
    tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY .

  constants GC_CHAR_ALTERNATIVES type STRING value '^!,|' ##NO_TEXT.
  constants GC_CHAR_NOT type c LENGTH 1 value '!' ##NO_TEXT.
  constants:
    GC_SEPA_AND TYPE c LENGTH 1 value '&' ##NO_TEXT.
  constants:
    GC_SEPA_OR TYPE c LENGTH 1 value '|' ##NO_TEXT.

  "! <p class="shorttext synchronized" lang="en">Split expression at given char (considering parentheses)</p>
  "!
  "! @parameter iv_expression | <p class="shorttext synchronized" lang="en">Expression</p>
  "! @parameter iv_separator  | <p class="shorttext synchronized" lang="en">Separator</p>
  "! @parameter ev_depth      | <p class="shorttext synchronized" lang="en">Depth =&gt; should be 0 if expression is syntactically correct</p>
  "! @parameter et_parts      | <p class="shorttext synchronized" lang="en">String table</p>
  class-methods SPLIT_EXPRESSION_AT
    importing
      !IV_EXPRESSION type CSEQUENCE
      !IV_SEPARATOR type C
    exporting
      !EV_DEPTH type I
      !ET_PARTS type TT_STRING .
  "! <p class="shorttext synchronized" lang="en">Trim needless parentheses</p>
  "!
  "! @parameter iv_expression | <p class="shorttext synchronized" lang="en">Expression</p>
  "! @parameter ev_expression | <p class="shorttext synchronized" lang="en">Expression after trimming</p>
  "! @parameter ev_found      | <p class="shorttext synchronized" lang="en">Unneded parentheses found</p>
  class-methods TRIM_UNNEEDED_PARENTHESES
    importing
      !IV_EXPRESSION type CSEQUENCE
    exporting
      value(EV_EXPRESSION) type STRING
      !EV_FOUND type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ABAP_BOOLEXPR_PARSER IMPLEMENTATION.


METHOD constructor.

  go_term_eval = io_term_eval.

ENDMETHOD.


METHOD get_instance.

  ro_parser = NEW zcl_abap_boolexpr_parser( io_term_eval ).

ENDMETHOD.


METHOD split_expression_at.

  CLEAR:
    ev_depth,
    et_parts.

  "************************************************************************
  " Speed up things little bit :)
  "************************************************************************
  IF iv_expression NS iv_separator.
    et_parts = VALUE #( ( iv_expression ) ).
    RETURN.
  ENDIF.


  "************************************************************************
  " Walk through the expression
  "************************************************************************
  DATA(lv_part) = ``.

  DO strlen( iv_expression ) TIMES.
    DATA(lv_index) = sy-index - 1.

    DATA(lv_char) = CONV string( iv_expression+lv_index(1) ).

    CASE lv_char.
      WHEN '('.
        ADD 1 TO ev_depth.
      WHEN ')'.
        SUBTRACT 1 FROM ev_depth.
        IF ev_depth LT 0.       " Invalid parentheses => closed without opening
          CLEAR et_parts.
          RETURN.
        ENDIF.
      WHEN iv_separator.
        IF ev_depth IS INITIAL.
          APPEND lv_part TO et_parts.
          CLEAR lv_part.
          CONTINUE. " Do not add separator to next part
        ENDIF.
      WHEN OTHERS.

    ENDCASE.

    lv_part = lv_part && lv_char.
  ENDDO.

  IF ev_depth NE 0.             " Invalid parentheses => opened without closing
    CLEAR et_parts.
    RETURN.
  ENDIF.

  IF lv_part IS NOT INITIAL.
    APPEND lv_part TO et_parts.
  ENDIF.

ENDMETHOD.


METHOD trim_unneeded_parentheses.

  CLEAR:
    ev_found.

  ev_expression = iv_expression.

  DO.
    CONDENSE ev_expression.

    " At least 2 chars needed
    DATA(lv_length) = strlen( ev_expression ).
    IF lv_length LT 2.
      EXIT.
    ENDIF.

    " Format (...)
    IF ev_expression NP '(*)'.
      EXIT.
    ENDIF.

    " Find the pair of first opening parenthesys
    DATA(lv_depth) = 0.
    DO lv_length TIMES.
      DATA(lv_offset) = sy-index - 1.
      DATA(lv_curr) = ev_expression+lv_offset(1).

      CASE lv_curr.
        WHEN '('.
          ADD 1 TO lv_depth.
        WHEN ')'.
          SUBTRACT 1 FROM lv_depth.
        WHEN OTHERS.
      ENDCASE.

      IF lv_depth EQ 0.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_offset EQ lv_length - 1.
      ev_found = abap_true.
      SUBTRACT 2 FROM lv_length.
      ev_expression = ev_expression+1(lv_length).
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDMETHOD.


METHOD zif_abap_boolexpr_parser~evaluate.

  " Supported special chars:
  " ( ) parenthesys
  " &   AND
  " | = OR (alternative char ,)
  " ! = NOT (alternative char ^)

  DATA(lv_expression) = CONV string( iv_expression ).

  CONDENSE lv_expression.

  IF lv_expression IS INITIAL.
    RAISE EXCEPTION TYPE zcx_abap_boolexpr_error
      EXPORTING
        textid = zcx_abap_boolexpr_error=>empty_expression.
  ENDIF.

  "************************************************************************
  " Translate alternative chars:
  " , is same as | (OR)
  " ^ is same as ! (NOT)
  "************************************************************************
  TRANSLATE lv_expression USING gc_char_alternatives.

  "************************************************************************
  " Trim unneeded parenthesys
  "************************************************************************
  trim_unneeded_parentheses(
    EXPORTING
      iv_expression = lv_expression
    IMPORTING
      ev_expression = lv_expression ).



  "************************************************************************
  " Special case when whole expression is in NOT
  " e.g.: !(expression&expression)
  "************************************************************************
  IF lv_expression(1) CA gc_char_not.
    DATA(lv_expression_wo_not) = lv_expression+1(*).

    trim_unneeded_parentheses(
      EXPORTING
        iv_expression = lv_expression_wo_not
      IMPORTING
        ev_expression = DATA(lv_expression_trimmed)
        ev_found      = DATA(lv_found) ).

    IF lv_found EQ abap_true.
      rv_result = zif_abap_boolexpr_parser~evaluate( lv_expression_trimmed ).
      rv_result = xsdbool( rv_result EQ abap_false ).
      RETURN.
    ENDIF.

  ENDIF.





  "************************************************************************
  " Handle OR parts
  "************************************************************************
  split_expression_at(
    EXPORTING
      iv_expression = lv_expression
      iv_separator  = gc_sepa_or
    IMPORTING
      ev_depth      = DATA(lv_depth)
      et_parts      = DATA(lt_parts) ).

  IF lv_depth NE 0.
    RAISE EXCEPTION TYPE zcx_abap_boolexpr_error
      EXPORTING
        textid = zcx_abap_boolexpr_error=>invalid_parentheses.
  ENDIF.

  IF lines( lt_parts ) GT 1.
    rv_result = abap_false.
    LOOP AT lt_parts INTO DATA(lv_part).
      rv_result = zif_abap_boolexpr_parser~evaluate( lv_part ).
      IF rv_result NE abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
    RETURN.
  ENDIF.


  "************************************************************************
  " Handle AND parts
  "************************************************************************
  split_expression_at(
    EXPORTING
      iv_expression = lv_expression
      iv_separator  = gc_sepa_and
    IMPORTING
      ev_depth      = lv_depth
      et_parts      = lt_parts ).

  IF lv_depth NE 0.
    RAISE EXCEPTION TYPE zcx_abap_boolexpr_error
      EXPORTING
        textid = zcx_abap_boolexpr_error=>invalid_parentheses.
  ENDIF.

  IF lines( lt_parts ) GT 1.
    rv_result = abap_true.
    LOOP AT lt_parts INTO lv_part.
      rv_result = zif_abap_boolexpr_parser~evaluate( lv_part ).

      IF rv_result NE abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    RETURN.
  ENDIF.


  "************************************************************************
  " No OR, AND parts anymore => handle NOT
  "************************************************************************
  IF lv_expression(1) CA gc_char_not.
    lv_expression = lv_expression+1(*).

    rv_result = zif_abap_boolexpr_parser~evaluate( lv_expression ).
    rv_result = xsdbool( rv_result EQ abap_false ).
    RETURN.
  ENDIF.



  "************************************************************************
  " No OR, AND and no parenthesys anymore => evaluate part
  "************************************************************************
  rv_result = go_term_eval->evaluate( lv_expression ).


ENDMETHOD.
ENDCLASS.
