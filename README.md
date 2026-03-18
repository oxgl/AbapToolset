# AbapToolset

ABAP Toolset: utilities and helper classes for ABAP.

## Boolean expressions

### Classes and interfaces

```
ZCL_ABAP_BOOLEXPR_PARSER
ZIF_ABAP_BOOLEXPR_PARSER
ZCL_ABAP_BOOLEXPR_UTILS
ZIF_ABAP_BOOLEXPR_TERM_EVAL
```

A simple boolean-expression parser and evaluator.

We often need to maintain complex rules in customizing, for example:

A record is relevant if:
- both `status1` **and** `status2` are active, **or**
- both `status3` **and** `status4` are active, **or**
- `status5` is inactive.

With the Boolean Expression tool, you can store this rule directly in customizing as plain text:

```
(status1&status2)|(status3&status4)|!status5
```

### Syntax

The parser supports the following special characters:

- `&` — logical **AND**
- `|` — logical **OR** (`,` can be used as an alternative)
- `!` — logical **NOT** (`^` can be used as an alternative because table maintenance may remove `!` at the beginning of a field)
- `(` `)` — parentheses

Operator precedence is: `!` (highest), then `&`, and finally `|`.

### Term evaluation

The parser can split the expression, but it cannot determine the truth value of individual terms (for example, whether status *X* is true or false). For this, it calls an object implementing `ZIF_ABAP_BOOLEXPR_TERM_EVAL`, which must return `ABAP_TRUE` or `ABAP_FALSE` for a given term.

In most cases, you can use the helper methods in `ZCL_ABAP_BOOLEXPR_UTILS`.

### Example

Suppose you want to check whether an object has active statuses matching:

- `((s1 OR s2) AND s3) OR (s4 AND NOT s5)`

The corresponding boolean expression is:

- `((s1|s2)&s3)|(s4&!s5)`

If the object’s active status list is `s1, s3, s7`, you can evaluate the expression like this:

```abap
DATA lt_active_status TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

lt_active_status = VALUE #( ( `s1` ) ( `s3` ) ( `s7` ) ).

TRY.
    DATA(lv_result) = zcl_abap_boolexpr_utils=>evaluate_boolexpr_table(
      iv_expression = `((s1|s2)&s3)|(s4&!s5)`
      it_values     = lt_active_status ).
  CATCH zcx_abap_boolexpr_error. " Error during expression parsing
ENDTRY.

WRITE lv_result.
```