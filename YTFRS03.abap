*&---------------------------------------------------------------------*
*& Report  YTFRS03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ytfrs03.


DATA: go_proxy       TYPE REF TO ytmsco_emcsinput,
      gs_ecms_input  TYPE ytmsemcsinput,
      gs_ecms_output TYPE ytmsemcsinput_response,

      gt_xml_file    TYPE STANDARD TABLE OF string,

      gd_pos         TYPE i,
      gd_len         TYPE i,

      gd_date_issued TYPE string,
      gd_date        TYPE d,
      gd_time        TYPE t.


*--------------------------------------------------------------------*

PARAMETERS: p_file TYPE string LOWER CASE OBLIGATORY.


*--------------------------------------------------------------------*
START-OF-SELECTION.


  DEFINE get_xmld_data.

    FIND FIRST OCCURRENCE OF &1 IN ls_xml_file IGNORING CASE MATCH OFFSET gd_pos MATCH LENGTH gd_len.
    IF sy-subrc EQ 0.
      ADD gd_len TO gd_pos.
      &2 = ls_xml_file+gd_pos(&3).
    ENDIF.

  END-OF-DEFINITION.

  PERFORM read_file.

  READ TABLE gt_xml_file INTO DATA(ls_xml_file) INDEX 1.


  get_xmld_data '<messageID>' gs_ecms_input-message-envelope-message_id 6.


  get_xmld_data '<transactionID>' gs_ecms_input-message-envelope-transaction_id 9.
  get_xmld_data '<senderID>'      gs_ecms_input-message-envelope-sender_id      8.
  get_xmld_data '<receiverID>'    gs_ecms_input-message-envelope-receiver_id    8.
  get_xmld_data '<messageType>'   gs_ecms_input-message-envelope-message_type   9.
  get_xmld_data '<schemaID>'      gs_ecms_input-message-envelope-schema_id      4.
  get_xmld_data '<statusCode>'    gs_ecms_input-message-envelope-status_code    3.
  get_xmld_data '<dateIssued>'    gd_date_issued                               29.

  gd_date = |{ gd_date_issued(4) }{ gd_date_issued+5(2) }{ gd_date_issued+8(2) }|.
  gd_time = |{ gd_date_issued+11(2) }{ gd_date_issued+14(2) }{ gd_date_issued+17(2) }|.
  CONVERT DATE gd_date TIME gd_time INTO TIME STAMP gs_ecms_input-message-envelope-date_issued TIME ZONE sy-zonlo.

*  get_xmld_data '<dateIssued>'    gs_ecms_input-message-envelope-date_issued   29.


  FIND FIRST OCCURRENCE OF '<Body messageType="IE801">' IN ls_xml_file IGNORING CASE MATCH OFFSET gd_pos MATCH LENGTH gd_len.
  FIND FIRST OCCURRENCE OF '</Body>' IN ls_xml_file IGNORING CASE MATCH OFFSET DATA(gd_pos_end).

  ADD gd_len TO gd_pos.
  SUBTRACT gd_pos FROM gd_pos_end.

  gs_ecms_input-message-body-content = ls_xml_file+gd_pos(gd_pos_end).
  gs_ecms_input-message-body-message_type = 'IE801'.

  DATA: ld_xsddatetime_z TYPE xsddatetime_z.

  BREAK-POINT.

  "Problem 1 - XSD Date time to String conversion

  ld_xsddatetime_z = cl_clb2_tools=>timestamp_from_utc( iv_isostring = gd_date_issued ).

  "problem 2, int to string exception handling

  BREAK-POINT.

  TRY.
      DATA: ld_int TYPE i.
      ld_int = '1234E'.
    CATCH cx_sy_conversion_no_number INTO DATA(lx_error).
      ld_int = '1234'.
  ENDTRY.


*  CREATE OBJECT go_proxy "YTMSCO_EMCSINPUT
*    EXPORTING
*      logical_port_name = 'YTMS_AUG1'.
*
*  TRY .
*      go_proxy->emcsinput(
*        EXPORTING
*          input              = gs_ecms_input
*        IMPORTING
*          output             = gs_ecms_output
*      ).
*
*    CATCH cx_ai_system_fault INTO DATA(lx_error).    "
*      BREAK-POINT.
*
*      MESSAGE lx_error->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
*  ENDTRY.

  MESSAGE 'Success!!!' TYPE 'S'.

*--------------------------------------------------------------------*

FORM read_file.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = p_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = gt_xml_file
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
