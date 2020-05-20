class ZCL_CJ74_REST_API definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF gty_cj74_76_output,
        date      TYPE sy-datum,
        posid     TYPE prps_r-posid,
        total     TYPE kaep_coac-wrgbtr,
        curr      TYPE kaep_coac-rwaer,
        committed TYPE kaep_coac-wrgbtr,
      END OF gty_cj74_76_output .
  types:
    gty_cj74_76_output_t TYPE STANDARD TABLE OF gty_cj74_76_output .
  types:
    BEGIN OF gty_cj76_output,
        year        TYPE c LENGTH 4,
        wbs_element TYPE c LENGTH 20,
        cost_elem   TYPE c LENGTH 10,
        object      TYPE c LENGTH 18,
        doc_date    TYPE c LENGTH 10,
        debit_date  TYPE c LENGTH 10,
        value_obj   TYPE c LENGTH 15,
      END OF gty_cj76_output .

  class-methods GET_LIST_FROM_MEMORY
    importing
      !IT_RSPARAMS type RSPARAMS_TT
      !IV_PROGNAME type SY-REPID
    exporting
      !ET_LIST_STRING_TABLE type LIST_STRING_TABLE .
  PROTECTED SECTION.
private section.

  methods CONV_CHAR_TO_AMMOUNT
    importing
      !IV_CHAR_VALUE type STRING
    returning
      value(ER_AMOUNT) type WRBTR .
  methods GET_CJ74_RESULT
    importing
      !IV_POSID type STRING
      !IV_DATE_FROM type DATUM
      !IV_DATE_TO type DATUM
    exporting
      !ET_RESULT type GTY_CJ74_76_OUTPUT_T
      !ES_TOTAL type GTY_CJ74_76_OUTPUT .
  methods CONVERT_DATA_TO_JSON
    importing
      !IT_DATA type DATA
    exporting
      !EV_JSON_XSTRING type XSTRING .
ENDCLASS.



CLASS ZCL_CJ74_REST_API IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CJ74_REST_API->CONVERT_DATA_TO_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        DATA
* | [<---] EV_JSON_XSTRING                TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_data_to_json.

    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE data = it_data
*    text = text
                           RESULT XML writer.

    ev_json_xstring = writer->get_output( ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CJ74_REST_API->CONV_CHAR_TO_AMMOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHAR_VALUE                  TYPE        STRING
* | [<-()] ER_AMOUNT                      TYPE        WRBTR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_char_to_ammount.

    DATA: lv_char TYPE c LENGTH 20.

    lv_char = iv_char_value.

    FIND FIRST OCCURRENCE OF '.' IN lv_char MATCH OFFSET DATA(lv_dot).
    FIND FIRST OCCURRENCE OF ',' IN lv_char MATCH OFFSET DATA(lv_com).

    IF lv_dot < lv_com.
      TRANSLATE lv_char USING '. '.
      TRANSLATE lv_char USING ',.'.
      CONDENSE lv_char NO-GAPS.
    ELSEIF lv_dot > lv_com.
      TRANSLATE lv_char USING ', '.
      CONDENSE lv_char NO-GAPS.
    ENDIF.

    er_amount = lv_char.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CJ74_REST_API->GET_CJ74_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_POSID                       TYPE        STRING
* | [--->] IV_DATE_FROM                   TYPE        DATUM
* | [--->] IV_DATE_TO                     TYPE        DATUM
* | [<---] ET_RESULT                      TYPE        GTY_CJ74_76_OUTPUT_T
* | [<---] ES_TOTAL                       TYPE        GTY_CJ74_76_OUTPUT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cj74_result.

    DATA: lt_rspar        TYPE STANDARD TABLE OF rsparams,
          lt_string_list  TYPE list_string_table,
          lt_string_list2 TYPE list_string_table,
          ls_rspar        TYPE rsparams,
          ls_result       TYPE gty_cj74_76_output,
          lt_cj76_output  TYPE STANDARD TABLE OF gty_cj76_output,
          ls_cj76_output  TYPE gty_cj76_output.

    CLEAR et_result.

    CLEAR: ls_rspar.
    ls_rspar-selname = 'CN_KOKRS'.
    ls_rspar-kind = 'P'.
    ls_rspar-low = '1004'.
    APPEND ls_rspar TO lt_rspar.

    CLEAR: ls_rspar.
    ls_rspar-selname = 'CN_PROFD'.
    ls_rspar-kind = 'P'.
    ls_rspar-low = '000000000001'.
    APPEND ls_rspar TO lt_rspar.

    CLEAR: ls_rspar.
    ls_rspar-selname = 'CN_PSPNR'.
    ls_rspar-kind = 'S'.
    ls_rspar-sign = 'I'.
    ls_rspar-option = 'BT'.
    ls_rspar-low = iv_posid.
    APPEND ls_rspar TO lt_rspar.

    CLEAR: ls_rspar.
    ls_rspar-selname = 'R_BUDAT'.
    ls_rspar-kind = 'S'.
    ls_rspar-sign = 'I'.
    ls_rspar-option = 'BT'.
    ls_rspar-low = iv_date_from.
    ls_rspar-high = iv_date_to.
    APPEND ls_rspar TO lt_rspar.

    CLEAR: ls_rspar.
    ls_rspar-selname = 'P_DISVAR'.
    ls_rspar-kind = 'P'.
    ls_rspar-low = '/LORELLA_AUG'.
    APPEND ls_rspar TO lt_rspar.

    zcl_cj74_rest_api=>get_list_from_memory(
      EXPORTING
        it_rsparams          = lt_rspar
        iv_progname          = 'ZRKPEP003'
      IMPORTING
        et_list_string_table = lt_string_list ).

    IF lt_string_list IS INITIAL.
      RETURN.
    ENDIF.

    es_total-posid = iv_posid.
    es_total-date = sy-datum.

    "Get CJ76 results
    READ TABLE lt_rspar WITH KEY selname = 'P_DISVAR' INTO ls_rspar.
    IF sy-subrc EQ 0.
      ls_rspar-low = '/PROG_LS'.
      MODIFY lt_rspar FROM ls_rspar INDEX sy-tabix.
      READ TABLE lt_rspar WITH KEY selname = 'R_BUDAT' INTO ls_rspar.
      IF sy-subrc EQ 0.
        ls_rspar-selname = 'R_OBDAT'.
        MODIFY lt_rspar FROM ls_rspar INDEX sy-tabix.
      ENDIF.

      zcl_cj74_rest_api=>get_list_from_memory(
        EXPORTING
          it_rsparams          = lt_rspar
          iv_progname          = 'ZRKPEP005'
        IMPORTING
          et_list_string_table = lt_string_list2 ).

      IF lt_string_list2 IS INITIAL.
        RETURN.
      ENDIF.

    ENDIF.

*    "Format CJ76 output
    LOOP AT lt_string_list2 INTO DATA(ls_list_line) FROM 8.
      IF ls_list_line(1) EQ '-'.
        EXIT.
      ENDIF.
      CLEAR ls_cj76_output.
      ls_cj76_output-year        = ls_list_line+1(4).
      ls_cj76_output-wbs_element = ls_list_line+6(20).
      ls_cj76_output-cost_elem   = ls_list_line+38(10).
      ls_cj76_output-object      = ls_list_line+49(18).
      CONDENSE ls_cj76_output-object NO-GAPS.
      ls_cj76_output-doc_date    = ls_list_line+27(10).
      ls_cj76_output-debit_date  = ls_list_line+68(10).
      ls_cj76_output-value_obj   = ls_list_line+79(15).
      APPEND ls_cj76_output TO lt_cj76_output.

      DATA(lv_str_value) = |{ ls_cj76_output-value_obj }|.
      data(lv_committed) = me->conv_char_to_ammount( lv_str_value ).
      ADD lv_committed TO es_total-committed.
    ENDLOOP.
    SORT lt_cj76_output.

    "Read CJ74 output
    LOOP AT lt_string_list INTO ls_list_line FROM 9. "line 9 is where the data begins in the output
      IF ls_list_line(1) EQ '-'.
        EXIT.
      ENDIF.
      CLEAR ls_result.
      ls_result-date = sy-datum.

      TRY.
          ls_result-posid = ls_list_line+13(18).
          ls_result-curr  = ls_list_line+127(3).
          lv_str_value = ls_list_line+111(15).

          ls_result-total = me->conv_char_to_ammount( lv_str_value ).
          ADD ls_result-total TO es_total-total.

          LOOP AT lt_cj76_output INTO ls_cj76_output.
            IF ls_cj76_output-wbs_element = ls_list_line+13(18).
              DATA(lv_object) = ls_list_line+32(15).
              CONDENSE lv_object NO-GAPS.
              IF lv_object = ls_cj76_output-object.
                lv_str_value = ls_cj76_output-value_obj.
                ls_result-committed = me->conv_char_to_ammount( lv_str_value ).
              ENDIF.
            ENDIF.
          ENDLOOP.

        CATCH cx_sy_range_out_of_bounds.
          CONTINUE.
      ENDTRY.

      APPEND ls_result TO et_result.

      IF es_total-curr IS INITIAL.
        es_total-curr = ls_result-curr.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CJ74_REST_API=>GET_LIST_FROM_MEMORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RSPARAMS                    TYPE        RSPARAMS_TT
* | [--->] IV_PROGNAME                    TYPE        SY-REPID
* | [<---] ET_LIST_STRING_TABLE           TYPE        LIST_STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_list_from_memory.

    DATA: lt_listobject TYPE TABLE OF abaplist.

    CALL FUNCTION 'LIST_FREE_MEMORY'.
    SUBMIT (iv_progname) WITH SELECTION-TABLE it_rsparams EXPORTING LIST TO MEMORY AND RETURN.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_listobject
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      CLEAR et_list_string_table.
    ENDIF.

    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = et_list_string_table
      TABLES
        listobject         = lt_listobject
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc NE 0.
      CLEAR et_list_string_table.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CJ74_REST_API->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA: lv_posid           TYPE string,
          lv_date_from       TYPE datum,
          lv_date_to         TYPE datum,
          lv_response_binary TYPE xstring,
*          lt_cj74            TYPE gty_cj74_76_output_t,
          ls_total           TYPE gty_cj74_76_output.


    lv_posid = server->request->get_form_field( name = 'POSID' ).

    IF lv_posid IS INITIAL.

      server->response->set_status( code = 400 "Bad Request
                                    reason = |{ text-m01 }| ).

      server->response->set_cdata(
        EXPORTING
          data               = | { text-e01 } |
      ).

    ELSE.

      TRY.
          lv_date_from = server->request->get_form_field( name = 'DATE_FROM' ).
          IF lv_date_from IS INITIAL.
            lv_date_from = sy-datum.
          ENDIF.
        CATCH cx_conversion_failed.
          lv_date_from = sy-datum.
      ENDTRY.

      TRY.
          lv_date_to = server->request->get_form_field( name = 'DATE_TO' ).
          IF lv_date_to IS INITIAL.
            lv_date_to = sy-datum.
          ENDIF.
        CATCH cx_conversion_failed.
          lv_date_to = sy-datum.
      ENDTRY.

      me->get_cj74_result(
        EXPORTING
          iv_posid     = lv_posid
          iv_date_from = lv_date_from
          iv_date_to   = lv_date_to
        IMPORTING
*          et_result = lt_cj74
          es_total  = ls_total ).

      me->convert_data_to_json(
        EXPORTING
*          it_data         = lt_cj74
          it_data         = ls_total
        IMPORTING
          ev_json_xstring = lv_response_binary
      ).

      "Content-Typ (application/json, text/xml o.a.)
      CALL METHOD server->response->set_header_field
        EXPORTING
          name  = if_http_header_fields=>content_type
          value = 'application/json' ##NO_TEXT.

      server->response->set_data(
        EXPORTING
          data               = lv_response_binary    " Binary data
      ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
