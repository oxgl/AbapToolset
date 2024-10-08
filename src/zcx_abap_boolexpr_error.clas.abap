***************************************************************************
* Project:    ABAP Toolset                                                *
* License:    MIT                                                         *
* Git:        https://github.com/oxgl/AbapToolset                         *
***************************************************************************
class ZCX_ABAP_BOOLEXPR_ERROR definition
  public
  inheriting from CX_DYNAMIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  constants:
    begin of EMPTY_EXPRESSION,
      msgid type symsgid value 'ZBASIS_ABAP',
      msgno type symsgno value '041',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of EMPTY_EXPRESSION .
  constants:
    begin of INVALID_PARENTHESES,
      msgid type symsgid value 'ZBASIS_ABAP',
      msgno type symsgno value '042',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_PARENTHESES .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAP_BOOLEXPR_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
