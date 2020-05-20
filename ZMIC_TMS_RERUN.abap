*---------------------------------------------------------------------*
*  Author :  Flavio Rasseli
*  Date   :  05.03.2020
*  Ticket :  SR18491
*  Description: Rerun MIC/TMS6 interface for failed ARC Number transmission
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*                                                                      *
*   01) Change number :                                                *
*       Author        :                                                *
*       Date          :                                                *
*       Reference     :                                                *
*       Description   :                                                *
*----------------------------------------------------------------------*

REPORT zmic_tms_rerun.

*--------------------------------------------------------------------*
* Global Data
DATA: gt_ytms_status TYPE TABLE OF ytms_status,
      ls_ytms_status TYPE ytms_status,
      gd_zdate       TYPE sy-datum.

CONSTANTS:
  gc_http_success TYPE ytms_status-code VALUE 200,
  gc_http_error   TYPE ytms_status-code VALUE 404,
  gc_log_object   TYPE balhdr-object    VALUE 'YMIC_PXIIW_YMIC_NOTI',
  gc_log_sub_obj  TYPE balhdr-subobject VALUE 'SEND_DATA',

  "Plants Party IDs
  gc_party_pal1   TYPE rfcdest VALUE 'TMS-PAL1',  "Palermo
  gc_party_nap1   TYPE rfcdest VALUE 'TMS-NAP1',  "Naples
  gc_party_augt   TYPE rfcdest VALUE 'TMS-AUGT',  "Augusta Depot
  gc_party_aug1   TYPE rfcdest VALUE 'TMS-AUG1',  "Augusta Ref
  "RFC Destinations
  gc_dest_pal1    TYPE rfcdest VALUE 'TMS_PAL1',  "Palermo
  gc_dest_nap1    TYPE rfcdest VALUE 'TMS_NAP1',  "Naples
  gc_dest_augt    TYPE rfcdest VALUE 'TMS_AUGT',  "Augusta Depot
  gc_dest_aug1    TYPE rfcdest VALUE 'TMS_AUG1'.  "Augusta Ref


*--------------------------------------------------------------------*
* Selection Parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETERS:     p_daysb TYPE int1 OBLIGATORY DEFAULT 2.
SELECT-OPTIONS: s_trid  FOR ls_ytms_status-transaction_id.

SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.

  IF gt_ytms_status IS NOT INITIAL.
    PERFORM execute_failed_records.
  ENDIF.

*--------------------------------------------------------------------*
FORM select_data.

  IF p_daysb IS NOT INITIAL.

    gd_zdate = sy-datum.

    SUBTRACT p_daysb FROM gd_zdate.

    SELECT * FROM ytms_status
      INTO TABLE @gt_ytms_status
     WHERE zdate          GE @gd_zdate
       AND transaction_id IN @s_trid
       AND code           NE @gc_http_success.
    IF sy-subrc NE 0.
      MESSAGE text-m01 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF. "Do nothing from here
  ENDIF.

ENDFORM.

FORM execute_failed_records.

  DATA: lt_bal_t_msg TYPE bal_t_msg,
        lv_req_body  TYPE string,
        lv_http_rc   TYPE sy-subrc,
        lv_error     TYPE string.

  LOOP AT gt_ytms_status REFERENCE INTO DATA(lr_ytms_status).

    PERFORM get_msgs_for_tid
      USING lr_ytms_status->transaction_id
            lt_bal_t_msg.

    CLEAR lv_req_body.

    "Now all messages from the current LS_YTMS_STATUS-TRANSACTION_ID are in LT_BAL_T_MSG
    LOOP AT lt_bal_t_msg INTO DATA(ls_bal_msg).
      IF ls_bal_msg-msgv1 EQ 'HTTP Req body'.
        CONCATENATE lv_req_body ls_bal_msg-msgv2 INTO lv_req_body.
      ENDIF.
    ENDLOOP.

    IF lv_req_body IS NOT INITIAL.

      CLEAR: lv_http_rc, lv_error.

      PERFORM send_data_http USING lv_req_body
                                   lr_ytms_status->party_id
                                   lv_http_rc
                                   lv_error.

      IF lv_http_rc EQ gc_http_success.
        lr_ytms_status->code = gc_http_success.
        CLEAR lr_ytms_status->reason.
        MESSAGE | { TEXT-s01 } { lr_ytms_status->transaction_id }| type 'S'.
      ELSE. "IF lv_http_rc EQ gc_http_error.
        lr_ytms_status->code = lv_http_rc.
        lr_ytms_status->reason = lv_error.
        MESSAGE | { TEXT-e01 } { lr_ytms_status->transaction_id }. { lv_error }| type 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.
  ENDLOOP.

  MODIFY ytms_status FROM TABLE gt_ytms_status.

ENDFORM.


*--------------------------------------------------------------------*
* Read allmessages from the SLG1 log for a MIC/TMS6 Transaction ID
*--------------------------------------------------------------------*
FORM get_msgs_for_tid USING pv_tid TYPE ytms_status-transaction_id
                            pt_bal_t_msg TYPE bal_t_msg.

  DATA: ls_log_filter TYPE bal_s_lfil,
        ls_bal_s_obj  TYPE bal_s_obj,
        ls_bal_s_sub  TYPE bal_s_sub,
        ls_bal_s_extn TYPE bal_s_extn,
        lt_bal_t_msgh TYPE bal_t_msgh,
        ls_bal_s_msg  TYPE bal_s_msg,
        lt_field      TYPE bal_t_fld,
        lt_header     TYPE balhdr_t,
        lt_handle     TYPE bal_t_logh.

  "Search for the log in the database
  ls_bal_s_extn-sign = 'I'.
  ls_bal_s_extn-option = 'EQ'.
  ls_bal_s_extn-low = pv_tid.
  APPEND ls_bal_s_extn TO ls_log_filter-extnumber.

  SELECT log_handle
    INTO CORRESPONDING FIELDS OF TABLE @lt_header
    FROM balhdr
   WHERE object EQ @gc_log_object
     AND subobject EQ @gc_log_sub_obj
     AND extnumber IN @ls_log_filter-extnumber.
  IF sy-subrc <> 0.
    MESSAGE | { text-m02 } { pv_tid } |
      TYPE 'I' DISPLAY LIKE 'E'. " No log found for this record
  ENDIF.

  IF lt_header IS NOT INITIAL.

    CLEAR lt_handle.
    LOOP AT lt_header INTO DATA(ls_header).
      INSERT ls_header-log_handle INTO TABLE lt_handle.
    ENDLOOP.

    "Load the log from database into memory
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle         = lt_handle
        i_lock_handling        = 0
        i_do_not_load_messages = abap_false
      EXCEPTIONS
        OTHERS                 = 0.

    "Get the messages
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_handle
      IMPORTING
        e_t_msg_handle = lt_bal_t_msgh
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE | { text-m02 } { pv_tid } |
        TYPE 'I' DISPLAY LIKE 'E'. " No log found for this record
    ENDIF.

    LOOP AT lt_bal_t_msgh INTO DATA(ls_msgh).
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = ls_msgh
        IMPORTING
          e_s_msg        = ls_bal_s_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE | { text-m02 } { pv_tid } / { ls_msgh-msgnumber } |
          TYPE 'I' DISPLAY LIKE 'E'. " No log found for this record / msgnumber
      ENDIF.

      APPEND ls_bal_s_msg TO pt_bal_t_msg.

    ENDLOOP.

  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
* Send the data via HTTP_CLIENT
*--------------------------------------------------------------------*
FORM send_data_http USING iv_req_body TYPE string
                          iv_party_id TYPE ytms_status-party_id
                          ev_http_rc  TYPE sy-subrc
                          ev_error    TYPE string.

  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_dest_name   TYPE rfcdest.

*  Pick the RFC destination name from Party ID
*  and the response to back to the correct TMS6 terminal
  CLEAR: lv_dest_name.
  CASE iv_party_id.
    WHEN gc_party_pal1.
      lv_dest_name = gc_dest_pal1.
    WHEN gc_party_nap1.
      lv_dest_name = gc_dest_nap1.
    WHEN gc_party_augt.
      lv_dest_name = gc_dest_augt.
    WHEN gc_party_aug1.
      lv_dest_name = gc_dest_aug1.
    WHEN OTHERS.
      CLEAR lv_dest_name.
      MESSAGE | { text-m03 } { ls_ytms_status-party_id } | TYPE 'I' DISPLAY LIKE 'S'.
  ENDCASE.

  IF lv_dest_name IS NOT INITIAL.

    cl_http_client=>create_by_destination(
     EXPORTING
       destination              = lv_dest_name
     IMPORTING
       client                   = lo_http_client
     EXCEPTIONS
       argument_not_found       = 1
       destination_not_found    = 2
       destination_no_authority = 3
       plugin_not_active        = 4
       internal_error           = 5
       OTHERS                   = 6
    ).

    lo_http_client->request->set_method( 'POST' ).
    lo_http_client->request->set_content_type( 'text/xml' ).
    lo_http_client->request->set_cdata( iv_req_body ).

*     Sending the request
    lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).

*     Receiving the request
    lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2 ).

    IF sy-subrc NE 0.
      lo_http_client->get_last_error(
        IMPORTING
          message        = ev_error    " Error Message
      ).
    ENDIF.

    lo_http_client->response->get_status(
      IMPORTING
        code   = ev_http_rc ).

    lo_http_client->close( ).

  ENDIF.

ENDFORM.
