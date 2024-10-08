***************************************************************************
* Project:    ABAP Toolset                                                *
* License:    MIT                                                         *
* Git:        https://github.com/oxgl/AbapToolset                         *
***************************************************************************
class ZCL_ABAP_PROCESS_UTILITIES definition
  public
  final
  create public .

public section.

  class-methods WAIT
    importing
      !IV_WAIT_TIME type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAP_PROCESS_UTILITIES IMPLEMENTATION.


METHOD wait.

  DATA:
    lv_current_ts TYPE timestampl,
    lv_end_ts     TYPE timestampl.


  "************************************************************************
  " Parameter check
  "************************************************************************
  IF iv_wait_time LE 0.
    RETURN.
  ENDIF.


  "************************************************************************
  " Convert millis to seconds
  "************************************************************************
  DATA(lv_wait_seconds) = CONV timestampl( iv_wait_time ) / 1000.


  "************************************************************************
  " Calculate end time...
  "************************************************************************
  GET TIME STAMP FIELD lv_current_ts.
  TRY.
      lv_end_ts = cl_abap_tstmp=>add(
                    tstmp   = lv_current_ts
                    secs    = lv_wait_seconds ).
    CATCH cx_parameter_invalid_range.
    CATCH cx_parameter_invalid_type.
      RETURN.
  ENDTRY.


  "************************************************************************
  " Call this standard FM to wait without implicit commit...
  " FM can handle only seconds
  "************************************************************************
  IF lv_wait_seconds GT 1.
    CALL FUNCTION 'ENQUE_SLEEP'
      EXPORTING
        seconds = CONV i( lv_wait_seconds )
      EXCEPTIONS
        OTHERS  = 0.
  ENDIF.


  "************************************************************************
  " This is the dirty part: busy wait...
  "************************************************************************
  DO.
    GET TIME STAMP FIELD lv_current_ts.
    IF lv_current_ts GE lv_end_ts.
      EXIT.
    ENDIF.
  ENDDO.

ENDMETHOD.
ENDCLASS.
