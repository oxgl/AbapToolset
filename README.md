# AbapToolset

ABAP Toolset is a small collection of utility classes and helpers for ABAP. The main focus in this repository is a reusable boolean-expression engine that lets you store rule logic in customizing or configuration tables and evaluate it against values, tables, or custom term resolvers.

## Boolean expressions

### Classes and interfaces

```
ZCL_ABAP_BOOLEXPR_PARSER
ZIF_ABAP_BOOLEXPR_PARSER
ZCL_ABAP_BOOLEXPR_UTILS
ZIF_ABAP_BOOLEXPR_TERM_EVAL
ZCX_ABAP_BOOLEXPR_ERROR
```

The boolean-expression parser evaluates expressions made up of terms, parentheses, and the operators `&` for AND, `|` for OR, and `!` for NOT. It also accepts `,` as an OR alternative and `^` as a NOT alternative.

This is useful when a business rule needs to be maintained as text instead of hard-coded in ABAP. For example, a record can be considered relevant when either of these is true:

- `status1` and `status2` are active
- `status3` and `status4` are active
- `status5` is inactive

That rule can be written as:

```
(status1&status2)|(status3&status4)|!status5
```

### Syntax

Supported syntax:

- `&` — logical **AND**
- `|` — logical **OR**
- `,` — alternative OR separator
- `!` — logical **NOT**
- `^` — alternative NOT character
- `(` `)` — parentheses

Operator precedence is `!`, then `&`, then `|`.

The parser also trims unnecessary outer parentheses and handles a special case where an entire expression is negated, such as `!(a&b)`.

### Term evaluation

`ZCL_ABAP_BOOLEXPR_PARSER` only splits and combines expressions. It does not decide whether a single term is true or false. That responsibility is delegated to an implementation of `ZIF_ABAP_BOOLEXPR_TERM_EVAL`.

`ZCL_ABAP_BOOLEXPR_UTILS` provides ready-to-use helpers for the most common cases:

- `EVALUATE_BOOLEXPR_VALUE` — compare one expression term against a single value
- `EVALUATE_BOOLEXPR_TABLE` — evaluate an expression against a table of values

Both helpers support case-insensitive matching by default.

### Example

Suppose you want to evaluate this rule:

- `((s1 OR s2) AND s3) OR (s4 AND NOT s5)`

The boolean-expression equivalent is:

- `((s1|s2)&s3)|(s4&!s5)`

If the active values are `s1`, `s3`, and `s7`, you can evaluate it like this:

```abap
DATA lt_active_status TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

lt_active_status = VALUE #( ( `s1` ) ( `s3` ) ( `s7` ) ).

TRY.
    DATA(lv_result) = zcl_abap_boolexpr_utils=>evaluate_boolexpr_table(
      iv_expression = `((s1|s2)&s3)|(s4&!s5)`
      it_values     = lt_active_status ).
  CATCH zcx_abap_boolexpr_error.
ENDTRY.

WRITE lv_result.
```

### Other utilities

Besides boolean-expression helpers, the repository also includes general ABAP utilities such as:

- `ZCL_ABAP_CONVERSION` for date, time, and timestamp conversion into internal ABAP formats
- `ZCL_ABAP_PROCESS_UTILITIES` for waiting without an implicit commit

## License

MIT
