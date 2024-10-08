***************************************************************************
* Project:    ABAP Toolset                                                *
* License:    MIT                                                         *
* Git:        https://github.com/oxgl/AbapToolset                         *
***************************************************************************
class ZCL_ABAP_CONVERSION definition
  public
  final
  create public .


public section.

  types:
    TV_DATE_FORMAT type C LENGTH 1 .
  types:
    TV_TIMESTAMP_FORMAT type C LENGTH 1 .
  types:
    TV_TIME_FORMAT type C LENGTH 1 .

  constants GC_DATE_FORMAT_AUTO type TV_DATE_FORMAT value ' ' ##NO_TEXT.
  constants GC_DATE_FORMAT_EUR type TV_DATE_FORMAT value 'E' ##NO_TEXT.
  constants GC_DATE_FORMAT_INTERNAL type TV_DATE_FORMAT value 'D' ##NO_TEXT.
  constants GC_DATE_FORMAT_ISO type TV_DATE_FORMAT value 'I' ##NO_TEXT.
  constants GC_DATE_FORMAT_UNKNOWN type TV_DATE_FORMAT value ' ' ##NO_TEXT.
  constants GC_DATE_FORMAT_USA type TV_DATE_FORMAT value 'U' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_AUTO type TV_TIMESTAMP_FORMAT value ' ' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_ISO_LOCAL type TV_TIMESTAMP_FORMAT value 'L' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_ISO_OFFSET type TV_TIMESTAMP_FORMAT value 'O' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_ISO_UTC type TV_TIMESTAMP_FORMAT value 'Z' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_RAW type TV_TIMESTAMP_FORMAT value 'R' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_TEXT type TV_TIMESTAMP_FORMAT value 'T' ##NO_TEXT.
  constants GC_TIMESTAMP_FORMAT_UNKNOWN type TV_TIMESTAMP_FORMAT value ' ' ##NO_TEXT.
  constants GC_TIME_FORMAT_AUTO type TV_TIME_FORMAT value ' ' ##NO_TEXT.
  constants GC_TIME_FORMAT_EUR type TV_TIME_FORMAT value 'E' ##NO_TEXT.
  constants GC_TIME_FORMAT_INTERNAL type TV_TIME_FORMAT value 'T' ##NO_TEXT.
  constants GC_TIME_FORMAT_ISO type TV_TIME_FORMAT value 'I' ##NO_TEXT.
  constants GC_TIME_FORMAT_UNKNOWN type TV_TIME_FORMAT value ' ' ##NO_TEXT.

  class-methods DATE_TO_INTERNAL
    importing
      !IV_INPUT type CLIKE
      !IV_FORMAT type TV_DATE_FORMAT default ZCL_ABAP_CONVERSION=>GC_DATE_FORMAT_AUTO
      !IV_DATE_VALIDITY_CHECK type ABAP_BOOL default ABAP_TRUE
    exporting
      !EV_OUTPUT type D
      !EV_FORMAT type TV_DATE_FORMAT .
  class-methods TIMESTAMP_TO_INTERNAL
    importing
      !IV_INPUT type CLIKE
      !IV_FORMAT type TV_TIMESTAMP_FORMAT default ZCL_ABAP_CONVERSION=>GC_TIMESTAMP_FORMAT_AUTO
    exporting
      !EV_OUTPUT_SHORT type TIMESTAMP
      !EV_OUTPUT_LONG type TIMESTAMPL
      !EV_FORMAT type TV_TIMESTAMP_FORMAT .
  class-methods TIME_TO_INTERNAL
    importing
      !IV_INPUT type CLIKE
      !IV_FORMAT type TV_TIME_FORMAT default ZCL_ABAP_CONVERSION=>GC_TIME_FORMAT_AUTO
      !IV_TIME_VALIDITY_CHECK type ABAP_BOOL default ABAP_TRUE
    exporting
      !EV_OUTPUT type SYTIME
      !EV_FORMAT type TV_TIME_FORMAT .
protected section.
private section.

  class-methods CREATE_MATCHER
    importing
      !IV_PATTERN type CLIKE
      !IV_TEXT type CLIKE
      !IV_IGNORE_CASE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RO_MATCHER) type ref to CL_ABAP_MATCHER
    raising
      CX_SY_REGEX
      CX_SY_MATCHER .
ENDCLASS.



CLASS ZCL_ABAP_CONVERSION IMPLEMENTATION.


METHOD create_matcher.

  DATA(lo_regex) = cl_abap_regex=>create_pcre(
    pattern     = iv_pattern
    ignore_case = iv_ignore_case ).
  ro_matcher = lo_regex->create_matcher( text = iv_text ).

ENDMETHOD.


METHOD date_to_internal.

  TYPES:
    BEGIN OF ts_date_components,
      year(4)  TYPE n,
      month(2) TYPE n,
      day(2)   TYPE n,
    END OF ts_date_components.

  CONSTANTS:
    BEGIN OF lc_regex,
      eur TYPE string VALUE '^(0[1-9]|[1-2][0-9]|3[0-1]|[1-9])\.(0[1-9]|1[0-2]|[1-9])\.([0-9]{4})',
      usa TYPE string VALUE '^(0[1-9]|1[0-2]|[1-9])[/-](0[1-9]|[1-2][0-9]|3[0-1]|[1-9])[/-]([0-9]{4})',
      iso TYPE string VALUE '^([0-9]{4})[-/.](0[1-9]|1[0-2]|[1-9])[-/.](0[1-9]|[1-2][0-9]|3[0-1]|[1-9])',
      int TYPE string VALUE '^([0-9]{4})(0[1-9]|1[0-2])(0[1-9]|[1-2][0-9]|3[0-1])',
    END OF lc_regex.

  DATA:
    ls_datecomp TYPE ts_date_components.

  FIELD-SYMBOLS:
   <fss_submatch>   TYPE submatch_result.


  DEFINE lm_save_submatch.  " &text &submatch_table &submatch1 &submatch2 &submatch3
    LOOP AT &2 ASSIGNING <fss_submatch>.
      CASE sy-tabix.
        WHEN 1.
          &3 = &1+<fss_submatch>-offset(<fss_submatch>-length).
        WHEN 2.
          &4 = &1+<fss_submatch>-offset(<fss_submatch>-length).
        WHEN 3.
          &5 = &1+<fss_submatch>-offset(<fss_submatch>-length).
      ENDCASE.
    ENDLOOP.
  END-OF-DEFINITION.

  ev_format = gc_date_format_unknown.
  CLEAR ev_output.

  DATA(lv_input) = CONV string( iv_input ).
  CONDENSE lv_input NO-GAPS.

  TRY.

      DO 1 TIMES.
        IF iv_format = gc_date_format_auto OR iv_format = gc_date_format_eur.
          DATA(lo_matcher) = create_matcher( iv_pattern = lc_regex-eur iv_text = iv_input ).  " Try to parse as EUR
          IF lo_matcher->match( ).
            DATA(ls_match) = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_datecomp-day ls_datecomp-month ls_datecomp-year.

            ev_output = ls_datecomp.
            ev_format = gc_date_format_eur.
            EXIT.
          ENDIF.
        ENDIF.

        IF iv_format = gc_date_format_auto OR iv_format = gc_date_format_iso.
          lo_matcher = create_matcher( iv_pattern = lc_regex-iso iv_text = iv_input ).        " Try to parse as ISO
          IF lo_matcher->match( ).
            ls_match = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_datecomp-year ls_datecomp-month ls_datecomp-day.

            ev_output = ls_datecomp.
            ev_format = gc_date_format_iso.
            EXIT.
          ENDIF.
        ENDIF.

        IF iv_format = gc_date_format_auto OR iv_format = gc_date_format_usa.
          lo_matcher = create_matcher( iv_pattern = lc_regex-usa iv_text = iv_input ).         " Try to parse as USA
          IF lo_matcher->match( ).
            ls_match = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_datecomp-month ls_datecomp-day ls_datecomp-year.

            ev_output = ls_datecomp.
            ev_format = gc_date_format_usa.
            EXIT.
          ENDIF.
        ENDIF.

        IF iv_format = gc_date_format_auto OR iv_format = gc_date_format_internal.
          lo_matcher = create_matcher( iv_pattern = lc_regex-int iv_text = iv_input ).         " Try to parse as Internal
          IF lo_matcher->match( ).
            ls_match = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_datecomp-year ls_datecomp-month ls_datecomp-day.

            ev_output = ls_datecomp.
            ev_format = gc_date_format_internal.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.

    CATCH cx_sy_regex cx_sy_matcher.
      ASSERT 0 = 1.
  ENDTRY.


  IF iv_date_validity_check EQ abap_true AND ev_format NE gc_date_format_unknown.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = ev_output
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ev_output.
      ev_format = gc_date_format_unknown.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD timestamp_to_internal.

  TYPES:
    BEGIN OF ts_pattern_format,
      pattern TYPE string,
      format  TYPE tv_timestamp_format,
    END OF ts_pattern_format.

  CONSTANTS:
    BEGIN OF lc_timestamp_pattern,
      global TYPE string  VALUE '[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(.[0-9]*)?(Z)',
      offset TYPE string  VALUE '[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(.[0-9]*)?[+-][0-9]{2}:[0-9]{2}',
      local  TYPE string  VALUE '[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(.[0-9]*)?',
      text   TYPE string  VALUE '[0-9]{4}-[0-9]{2}-[0-9]{2}[[:space:]][0-9]{2}:[0-9]{2}:[0-9]{2}(.[0-9]*)?',
    END OF lc_timestamp_pattern.

  DATA:
     lt_pattern_format TYPE STANDARD TABLE OF ts_pattern_format WITH DEFAULT KEY.

  CLEAR:
    ev_output_short,
    ev_output_long.

  ev_format = gc_timestamp_format_unknown.

  DATA(lv_input) = CONV string( iv_input ).
  CONDENSE lv_input NO-GAPS.

  IF    iv_format EQ gc_timestamp_format_raw
     OR iv_format EQ gc_timestamp_format_auto.

    IF lv_input CO ' 01234567890.'.

      FIND ALL OCCURRENCES OF '.' IN lv_input MATCH COUNT DATA(lv_count).
      IF lv_count LE 1 AND strlen( lv_input ) GE 8.
        WHILE strlen( lv_input ) LT 14.
          lv_input = lv_input && '0'.
        ENDWHILE.
        ev_output_long  = lv_input.
        ev_output_short = trunc( ev_output_long ).
        ev_format       = gc_timestamp_format_raw.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

  lt_pattern_format = VALUE #(
                        ( pattern = lc_timestamp_pattern-global format = gc_timestamp_format_iso_utc )
                        ( pattern = lc_timestamp_pattern-offset format = gc_timestamp_format_iso_offset )
                        ( pattern = lc_timestamp_pattern-local  format = gc_timestamp_format_iso_local )
                        ( pattern = lc_timestamp_pattern-text   format = gc_timestamp_format_text )
  ).

  LOOP AT lt_pattern_format ASSIGNING FIELD-SYMBOL(<fss_pattern_format>).
    CHECK iv_format EQ <fss_pattern_format>-format
       OR iv_format EQ gc_timestamp_format_auto.

    CHECK <fss_pattern_format>-pattern IS INITIAL
       OR cl_abap_matcher=>matches(
            pattern = <fss_pattern_format>-pattern
            text    = iv_input ).

    TRY.
        cl_gdt_conversion=>date_time_inbound(
          EXPORTING
            im_value       = CONV string( iv_input )
          IMPORTING
            ex_value_short = ev_output_short
            ex_value_long  = ev_output_long ).

        ev_format = <fss_pattern_format>-format.
        RETURN.
      CATCH cx_gdt_conversion.
    ENDTRY.
  ENDLOOP.

  IF iv_format EQ gc_timestamp_format_text OR iv_format EQ gc_timestamp_format_auto.

    SPLIT iv_input AT space INTO TABLE DATA(lt_parts).

    IF lines( lt_parts ) GE 1.
      date_to_internal(
        EXPORTING
          iv_input  = lt_parts[ 1 ]
        IMPORTING
          ev_output = DATA(lv_date) ).

      IF lines( lt_parts ) GE 2.
        time_to_internal(
          EXPORTING
            iv_input  = lt_parts[ 2 ]
          IMPORTING
            ev_output = DATA(lv_time) ).
      ENDIF.

      IF lv_date IS NOT INITIAL.
        CONVERT DATE lv_date TIME lv_time
           INTO TIME STAMP ev_output_short TIME ZONE sy-zonlo.
        ev_output_long = ev_output_short.
        ev_format      = gc_timestamp_format_text.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.


ENDMETHOD.


METHOD time_to_internal.

  TYPES:
    BEGIN OF ts_time_components,
      hour(2)   TYPE n,
      minute(2) TYPE n,
      second(2) TYPE n,
    END OF ts_time_components.

  CONSTANTS:
    BEGIN OF lc_regex,
      eur TYPE string VALUE '^(0[1-9]|1[0-9]|2[0-3]|[1-9])\.([0-5][0-9]|[0-9])\.([0-5][0-9]|[0-9])',
      iso TYPE string VALUE '^(0[1-9]|1[0-9]|2[0-3]|[1-9])\:([0-5][0-9]|[0-9])\:([0-5][0-9]|[0-9])',
      int TYPE string VALUE '^(0[1-9]|1[0-9]|2[0-3])([0-5][0-9])([0-5][0-9])',
    END OF lc_regex.

  DATA:
    ls_timecomp TYPE ts_time_components.

  FIELD-SYMBOLS:
    <fss_submatch>   TYPE submatch_result.

  DEFINE lm_save_submatch.  " &text &submatch_table &submatch1 &submatch2 &submatch3
    LOOP AT &2 ASSIGNING <fss_submatch>.
      CASE sy-tabix.
        WHEN 1.
          &3 = &1+<fss_submatch>-offset(<fss_submatch>-length).
        WHEN 2.
          &4 = &1+<fss_submatch>-offset(<fss_submatch>-length).
        WHEN 3.
          &5 = &1+<fss_submatch>-offset(<fss_submatch>-length).
      ENDCASE.
    ENDLOOP.
  END-OF-DEFINITION.

  ev_format = gc_time_format_unknown.
  CLEAR ev_output.

  DATA(lv_input) = CONV string( iv_input ).
  CONDENSE lv_input NO-GAPS.

  TRY.
      DO 1 TIMES.
        IF iv_format = gc_time_format_auto OR iv_format = gc_time_format_eur.
          DATA(lo_matcher) = create_matcher( iv_pattern = lc_regex-eur iv_text = iv_input ).      " Try to parse as EUR
          IF lo_matcher->match( ).
            DATA(ls_match) = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_timecomp-hour ls_timecomp-minute ls_timecomp-second.

            ev_output = ls_timecomp.
            ev_format = gc_time_format_eur.
            EXIT.
          ENDIF.
        ENDIF.

        IF iv_format = gc_time_format_auto OR iv_format = gc_time_format_iso.
          lo_matcher = create_matcher( iv_pattern = lc_regex-iso iv_text = iv_input ).            " Try to parse as ISO
          IF lo_matcher->match( ).
            ls_match = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_timecomp-hour ls_timecomp-minute ls_timecomp-second.

            ev_output = ls_timecomp.
            ev_format = gc_time_format_iso.
            EXIT.
          ENDIF.
        ENDIF.

        IF iv_format = gc_time_format_auto OR iv_format = gc_time_format_internal.
          lo_matcher = create_matcher( iv_pattern = lc_regex-int iv_text = iv_input ).            " Try to parse as Internal
          IF lo_matcher->match( ).
            ls_match = lo_matcher->get_match( ).
            lm_save_submatch lv_input ls_match-submatches ls_timecomp-hour ls_timecomp-minute ls_timecomp-second.

            ev_output = ls_timecomp.
            ev_format = gc_time_format_internal.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    CATCH cx_sy_regex cx_sy_matcher.
      ASSERT 0 = 1.
  ENDTRY.

  IF iv_time_validity_check EQ abap_true AND ev_format NE gc_time_format_unknown.
    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = ev_output
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ev_output.
      ev_format = gc_time_format_unknown.
    ENDIF.

  ENDIF.

ENDMETHOD.
ENDCLASS.
