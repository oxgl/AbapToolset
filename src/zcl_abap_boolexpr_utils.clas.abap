***************************************************************************
* Project:    ABAP Toolset                                                *
* License:    MIT                                                         *
* Git:        https://github.com/oxgl/AbapToolset                         *
***************************************************************************
class ZCL_ABAP_BOOLEXPR_UTILS definition
  public
  final
  create public .

public section.

  class-methods EVALUATE_BOOLEXPR_VALUE
    importing
      !IV_EXPRESSION type CLIKE
      !IV_VALUE type CLIKE
      !IV_CASE_INSENSITIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      ZCX_ABAP_BOOLEXPR_ERROR .
  class-methods EVALUATE_BOOLEXPR_TABLE
    importing
      !IV_EXPRESSION type CLIKE
      !IT_VALUES type INDEX TABLE
      !IV_CASE_INSENSITIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      ZCX_ABAP_BOOLEXPR_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAP_BOOLEXPR_UTILS IMPLEMENTATION.


METHOD evaluate_boolexpr_table.

  DATA(lro_term_eval) = NEW lcl_table_term_eval(
    it_values = it_values
    iv_ci     = iv_case_insensitive ).

  DATA(lro_parser) = zcl_abap_boolexpr_parser=>get_instance( lro_term_eval ).

  rv_result = lro_parser->evaluate( iv_expression ).

ENDMETHOD.


METHOD evaluate_boolexpr_value.

  DATA(lro_term_eval) = NEW lcl_value_term_eval(
    iv_value  = iv_value
    iv_ci     = iv_case_insensitive ).

  DATA(lro_parser) = zcl_abap_boolexpr_parser=>get_instance( lro_term_eval ).

  rv_result = lro_parser->evaluate( iv_expression ).

ENDMETHOD.
ENDCLASS.
