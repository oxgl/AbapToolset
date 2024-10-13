# AbapToolset
ABAP Toolset - utilities, helpers for ABAP

## Bool Expressions
Classes and interfaces
```
ZCL_ABAP_BOOLEXPR_PARSER
ZIF_ABAP_BOOLEXPR_PARSER
ZCL_ABAP_BOOLEXPR_UTILS
ZIF_ABAP_BOOLEXPR_TERM_EVAL

```
Simple boolean expression parser and evaluator. We often need to add complex rules into customizing, e.g.: record is relevant if:
* both status1 **and** status2 are active 
* **or** both status3 **and** status4 are active
* **or** status5 is inactive.

Thanks to Bool Expression tool we can put this complex rule directly to customizing as text:
```
(status1&status2)|(status3&status4)|!status5
```

Bool expression tool parses the input string and uses these special characters:
* & - operator AND
* | - operator OR (as an alternative char we can use ,)
* ! - operator NOT (alternative char is ^ - because table maintenance removes ! from the begginning of field)
* ( ) - parenthesys

Operator ! has the highest precedence, then & and finally |. 

Parser will split up the expression, but can't evaluate the value of the terms in expression (is status X true or false?). It will call an object with interface ZIF_ABAP_BOOLEXPR_TERM_EVAL, which has to return ABAP_TRUE or ABAP_FALSE for given term.
In most cases we can use methods of class ZCL_ABAP_BOOLEXPR_UTILS... 

Consider an example when we have to check whether object has active statuses: ( (s1 or s2) and s3 ) or (s4 but not s5). Our bool expression should look like: ((s1|s2)&s3)|(s4&!s5). We read active status list of object from database: s1,s3,s7. Put these these status list into table (of string):

```
DATA:
  lt_active_status TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

lt_active_status = VALUE #( ( `s1` ) ( `s3` ) ( `s7` ) ).

TRY.
    DATA(lv_result) = zcl_abap_boolexpr_utils=>evaluate_boolexpr_table(
      iv_expression = `((s1|s2)&s3)|(s4&!s5)`
      it_values     = lt_active_status ).
  CATCH zcx_abap_boolexpr_error. " Error during expression parsing
ENDTRY.

WRITE lv_result.
```
  
  
