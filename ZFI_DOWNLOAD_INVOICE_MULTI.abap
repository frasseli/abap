*---------------------------------------------------------------------*
* Author:         Flavio Rasseli - IBM
* Date:           12-Apr-2019
* Description:    Download FI document Attachments
* Change Request: SR11687
* Tcode:          ZFI_DOWNLOAD_INVOICE
*--------------------------------------------------------------------*
* Program to create a list (ALV) of the selected documents that have
* attachments and allow to download them as PDF, either one by one
* or mass download all at the same time, saving them in the local
* folder indicated by the user.
*---------------------------------------------------------------------*
REPORT zfi_download_invoice_multi.

*--------------------------------------------------------------------*
*Data types
TYPES: gty_t_bkpf TYPE TABLE OF bkpf.

*--------------------------------------------------------------------*
* Global Data
DATA: gs_bkpf TYPE bkpf.

CONSTANTS gc_func_download TYPE salv_de_function VALUE 'DOWNL'.

CLASS lcl_invoice_list DEFINITION DEFERRED.

DATA: go_invoice_list TYPE REF TO lcl_invoice_list.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETER      p_bukrs TYPE bkpf-bukrs    OBLIGATORY.
SELECT-OPTIONS s_belnr FOR  gs_bkpf-belnr.
PARAMETER      p_gjahr TYPE bkpf-gjahr    OBLIGATORY.


SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_path TYPE rlgrap-filename,
            cb_log AS CHECKBOX.

*--------------------------------------------------------------------*
INITIALIZATION.

  DATA: ld_my_documents_path TYPE string.

  CALL METHOD cl_gui_frontend_services=>registry_get_value
    EXPORTING
      root                 = cl_gui_frontend_services=>hkey_current_user
      key                  = |{ text-k01 }|
      value                = |{ text-k02 }|
    IMPORTING
      reg_value            = ld_my_documents_path
    EXCEPTIONS
      get_regvalue_failed  = 1
      cntl_error           = 2
      error_no_gui         = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF sy-subrc EQ 0.
    p_path = |{ ld_my_documents_path }\\|.
  ENDIF.

*--------------------------------------------------------------------*
* Main class of the program
CLASS lcl_invoice_list DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_salv_csqt_content_manager.

    METHODS:

      main,

      display_data IMPORTING io_container TYPE REF TO cl_gui_custom_container,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column,

      on_action FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

  PRIVATE SECTION.

    DATA: it_documents    TYPE gty_t_bkpf,
          it_document_ids TYPE cl_alink_connection=>toav0_tab,
          it_messages     TYPE TABLE OF messages.

    METHODS:

      log_document_error IMPORTING iv_belnr TYPE bkpf-belnr
                                   iv_subrc TYPE sy-subrc,

      display_log_messages.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_invoice_list IMPLEMENTATION.

  METHOD main.

    DATA: lt_document_ids TYPE cl_alink_connection=>toav0_tab,
          ld_object_id    TYPE toav0-object_id.

    SELECT bukrs, belnr, gjahr
      INTO CORRESPONDING FIELDS OF TABLE @it_documents
      FROM bkpf
     WHERE bukrs EQ @p_bukrs
       AND belnr IN @s_belnr
       AND gjahr EQ @p_gjahr.
    IF sy-subrc NE 0.
      MESSAGE text-i01 TYPE 'I'.
    ENDIF.

    LOOP AT it_documents INTO DATA(ls_documents).

      CLEAR lt_document_ids.

      CONCATENATE ls_documents-bukrs ls_documents-belnr ls_documents-gjahr INTO ld_object_id.

      cl_alink_connection=>find(
        EXPORTING
          sap_object         = 'BKPF'          " SAP ArchiveLink: Object type of business object
          object_id          = ld_object_id    " SAP ArchiveLink: Object ID (Object Identifier)
          mandt              = sy-mandt        " Client ID of Current User
        IMPORTING
          connections        = lt_document_ids
        EXCEPTIONS
          not_found          = 1
          error_authorithy   = 2
          error_parameter    = 3
          OTHERS             = 4
      ).

      IF sy-subrc <> 0.
        log_document_error( EXPORTING iv_belnr = ls_documents-belnr iv_subrc = sy-subrc ).
        CONTINUE.
      ENDIF.

      APPEND LINES OF lt_document_ids TO it_document_ids.
    ENDLOOP.

    IF it_document_ids IS INITIAL.
      MESSAGE text-i02 TYPE 'I'.
      RETURN.
    ENDIF.

    IF it_messages IS NOT INITIAL AND cb_log EQ abap_true.
      display_log_messages( ).
    ENDIF.

    "This function creates the container and call the local implementation of
    "method fill_container_content, which in turn create the alv and displays the data
    CALL FUNCTION 'SALV_CSQT_CREATE_CONTAINER'
      EXPORTING
        r_content_manager = go_invoice_list
        title             = text-t01.

  ENDMETHOD.

*--------------------------------------------------------------------*
* Creates and display main ALV
  METHOD if_salv_csqt_content_manager~fill_container_content.
*    importing
*      value(r_container) type ref to cl_gui_custom_container.
    display_data( io_container = r_container ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Logs an error message in the messages table
  METHOD log_document_error.

    DATA: ls_messages TYPE messages.

    ls_messages-msg_txt = |{ text-m01 } { iv_belnr }. { text-m02 } { iv_subrc }.|.
    APPEND ls_messages TO it_messages.

  ENDMETHOD.

*--------------------------------------------------------------------*
* This method creates the main ALV list and adds the function to
* mass download the documents
  METHOD display_data.

    DATA: lo_salv_table TYPE REF TO cl_salv_table.

    TRY.

        cl_salv_table=>factory(
          EXPORTING
            r_container    = io_container
          IMPORTING
            r_salv_table   = lo_salv_table
          CHANGING
            t_table        = it_document_ids ).

        lo_salv_table->get_functions( )->set_all( abap_true ).

        lo_salv_table->get_event( ).

        SET HANDLER me->on_double_click FOR lo_salv_table->get_event( ).
        SET HANDLER me->on_action FOR lo_salv_table->get_event( ).

        lo_salv_table->get_functions( )->add_function(
          EXPORTING
            name               = gc_func_download    " ALV Function
            icon               = '@49@' "ICON_EXPORT
            text               = |{ text-f01 }|
            tooltip            = |{ text-f02 }|
            position           = if_salv_c_function_position=>left_of_salv_functions   " Positioning Function
        ).

        lo_salv_table->display( ).

      CATCH cx_root.    "
        MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

*--------------------------------------------------------------------*
* This method download only the clicked document to the user's folder
  METHOD on_double_click.

    TYPES: BEGIN OF lty_binary,
             binary_field(1000) TYPE c,
           END OF lty_binary.

    DATA: lt_toauri   TYPE TABLE OF toauri,
          ld_url      TYPE string,
          ld_xcontent TYPE xstring,
          http_client TYPE REF TO if_http_client,
          lt_hex_tab1 TYPE TABLE OF lty_binary.

    READ TABLE it_document_ids INTO DATA(ls_document_ids) INDEX row.

    CALL FUNCTION 'ARCHIVOBJECT_GET_URI'
      EXPORTING
        objecttype               = 'BKPF'
        object_id                = ls_document_ids-object_id
*       LOCATION                 = 'F'
*       HTTP_URL_ONLY            = ' '
      TABLES
        uri_table                = lt_toauri
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        error_http               = 4
        error_dp                 = 5
        OTHERS                   = 6.

    IF sy-subrc <> 0.
      MESSAGE text-i03 TYPE 'I'.
    ENDIF.

    READ TABLE lt_toauri INTO DATA(ls_toauri) INDEX 1.
    IF sy-subrc EQ 0.

      ld_url = ls_toauri-uri.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = ld_url
        IMPORTING
          client             = http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      IF sy-subrc = 0.

        http_client->send(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5
        ).
        IF sy-subrc <> 0.
          DATA(ld_error) = |{ text-m03 } { text-m02 } { sy-subrc }.|.
          MESSAGE ld_error TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4
        ).
        IF sy-subrc <> 0.
          ld_error = |{ text-m03 } { text-m02 } { sy-subrc }.|.
          MESSAGE ld_error TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        ld_xcontent = http_client->response->get_data( ).

        http_client->close( ).

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = ld_xcontent
          TABLES
            binary_tab = lt_hex_tab1.

        "Add a forward slash at the end of the path is is not there yet
        DATA(ld_path_len) = strlen( p_path ) - 1.
        IF p_path+ld_path_len(1) NE '\'.
          p_path = |{ p_path }\\|.
        ENDIF.

        CONCATENATE p_path ls_document_ids-object_id '.pdf' INTO DATA(ld_filepath) ##no_text.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = ld_filepath
            filetype                = 'BIN'
          TABLES
            data_tab                = lt_hex_tab1
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.

        IF sy-subrc <> 0.
          MESSAGE text-i03 TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
* This method is for mass download all the documents to the user's
* folder, by simulating a double click in each of the table's lines
  METHOD on_action.

    IF e_salv_function EQ gc_func_download.
      LOOP AT it_document_ids REFERENCE INTO DATA(lr_dummy).
        me->on_double_click(
          EXPORTING
            row    = sy-tabix
            column = '1' ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
* This method display the log messages for documents without attachment
  METHOD display_log_messages.

    DATA: lo_salv_table_messages TYPE REF TO cl_salv_table.

    TRY .

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_salv_table_messages " Basis Class Simple ALV Tables
          CHANGING
            t_table        = it_messages
        ).

        lo_salv_table_messages->get_functions( )->set_all( abap_true ).

        lo_salv_table_messages->get_columns( )->get_column( columnname = 'LOG_NO' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_LINENO' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'LANGUAGE' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_TYPE' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_ID' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_NO' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_V1' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_V2' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_V3' )->set_visible( abap_false ).
        lo_salv_table_messages->get_columns( )->get_column( columnname = 'MSG_V4' )->set_visible( abap_false ).

        lo_salv_table_messages->display( ).

      CATCH cx_root.    "
        MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: ld_result   TYPE abap_bool,
        ld_path_str TYPE string.

  ld_path_str = p_path.
  cl_gui_frontend_services=>directory_exist(
    EXPORTING
      directory            = ld_path_str    " Directory name
    RECEIVING
      result               = ld_result    " Result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5
  ).
  IF sy-subrc <> 0 OR ld_result EQ abap_false.
    MESSAGE text-e03 TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF s_belnr IS INITIAL.
    MESSAGE text-e04 TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CREATE OBJECT go_invoice_list.
  go_invoice_list->main( ).
