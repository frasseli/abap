REPORT yrecover_tms_mic_xml_from_log.



*--------------------------------------------------------------------*
* Global data
DATA: gt_log_file TYPE STANDARD TABLE OF string,
      gt_xml_file TYPE STANDARD TABLE OF string.

*--------------------------------------------------------------------*
PARAMETERS: p_file TYPE string LOWER CASE OBLIGATORY.

PARAMETERS: p_remcd AS CHECKBOX.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_log_file.

  IF p_remcd IS NOT INITIAL.

    PERFORM get_cdata_out.

  ELSE.

  PERFORM read_log_file.

  PERFORM classify_log_entries.

  PERFORM show_xml_file.

  PERFORM download_log_file.

  ENDIF.

*--------------------------------------------------------------------*
FORM read_log_file.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = p_file    " Name of file
      filetype                = 'ASC'    " File Type (ASCII, Binary)
    CHANGING
      data_tab                = gt_log_file    " Transfer table for file contents
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

*--------------------------------------------------------------------*
FORM classify_log_entries.

  LOOP AT gt_log_file INTO DATA(ls_log).

    CASE ls_log(13).
      WHEN 'HTTP Req body'.
        APPEND ls_log+14 TO gt_xml_file.
      WHEN 'Req HTTP_HEAD'.
        WRITE: /001 ls_log.
      WHEN 'Resp HTTP_HEA'.
        WRITE: /001 ls_log.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.


FORM show_xml_file.

  ULINE.
  WRITE: /001 'XML body of the request'.
  LOOP AT gt_xml_file INTO DATA(ls_xml).
    WRITE: /001 ls_xml.
  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------------*
FORM download_log_file.

  DATA: ls_file        TYPE string,
        lt_to_download TYPE STANDARD TABLE OF string.

  LOOP AT gt_xml_file INTO DATA(ls_xml).
    CONCATENATE ls_file ls_xml INTO ls_file.
  ENDLOOP.

  APPEND ls_file TO lt_to_download.

  DATA(lv_filename) = p_file.
  CONCATENATE lv_filename '_LOG.txt' INTO lv_filename.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
*      bin_filesize              =     " File length for binary files
      filename                  = lv_filename    " Name of file
    CHANGING
      data_tab                  = lt_to_download    " Transfer table
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.


FORM get_cdata_out.

  DATA: ls_file        TYPE string,
        lt_to_download TYPE STANDARD TABLE OF string.

  LOOP AT gt_log_file INTO DATA(ls_xml).
    CONCATENATE ls_file ls_xml INTO ls_file.
  ENDLOOP.


  IF ls_file CS '&lt;![CDATA['. " or ls_file CS '<![CDATA['.
    REPLACE '&lt;![CDATA[' IN ls_file WITH space.
    REPLACE ']]&gt;' IN ls_file WITH space.
  ENDIF.

  IF ls_file CS '<![CDATA['.
    REPLACE '<![CDATA[' IN ls_file WITH space.
    REPLACE ']]>' IN ls_file WITH space.
  ENDIF.

  APPEND ls_file TO lt_to_download.

  DATA(lv_filename) = p_file.
  CONCATENATE lv_filename '_NO_CDATA.xml' INTO lv_filename.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
*      bin_filesize              =     " File length for binary files
      filename                  = lv_filename    " Name of file
    CHANGING
      data_tab                  = lt_to_download    " Transfer table
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
