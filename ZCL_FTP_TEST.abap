class ZCL_FTP_TEST definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_SERVER type STRING
      !IV_USERNAME type VVXINFO
      !IV_PASSWORD type VVXINFO
    exceptions
      FTP_CONN_FAILED .
  methods GET_FILE
    importing
      !IV_FILENAME type RLGRAP-FILENAME
      !IV_PATH type RLGRAP-FILENAME
    exporting
      value(ET_DATA) type TABLE .
  methods CLOSE_FTP .
  PROTECTED SECTION.
private section.

  data M_HANDLE type I .
  data M_SERVER type RLGRAP-FILENAME .
  constants CO_ENCRYPTION_KEY type I value 26101957 ##NO_TEXT.
  data M_USERNAME type VVXINFO.
  data M_PASSWORD type VVXINFO.
  constants CO_FTP_DESTINATION type RFCDEST value 'SAPFTPA' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_FTP_TEST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FTP_TEST->CLOSE_FTP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_ftp.

    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = me->m_handle.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = co_ftp_destination
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc NE 0.
      "Dunno
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FTP_TEST->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SERVER                      TYPE        STRING
* | [--->] IV_USERNAME                    TYPE        VVXINFO
* | [--->] IV_PASSWORD                    TYPE        VVXINFO
* | [EXC!] FTP_CONN_FAILED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    "Initialize instance variables
    me->m_server   = iv_server.
    me->m_username = iv_username.
    me->m_password = iv_password.

    "Encrypt the password
    DATA: lv_password TYPE c LENGTH 255.
    lv_password = iv_password.

    DATA(lv_password_len) = strlen( iv_password ).

    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        source      = lv_password "Actual password
        sourcelen   = lv_password_len
        key         = co_encryption_key
      IMPORTING
        destination = lv_password. "Encyrpted Passwor

    "Connect to remote server
    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        user            = me->m_username
        password        = me->m_password
        host            = me->m_server
        rfc_destination = co_ftp_destination
      IMPORTING
        handle          = me->m_handle
      EXCEPTIONS
        not_connected   = 1
        OTHERS          = 2.

    IF sy-subrc NE 0.
      RAISE ftp_conn_failed.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FTP_TEST->GET_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        RLGRAP-FILENAME
* | [--->] IV_PATH                        TYPE        RLGRAP-FILENAME
* | [<---] ET_DATA                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_file.

    DATA: lt_cmdout TYPE STANDARD TABLE OF string.

    "Do nothing if there is no active handle
    IF me->m_handle IS INITIAL.
      RETURN.
    ENDIF.

    "Build a CD command to get to the file path
    DATA: lv_command TYPE c LENGTH 255.

    lv_command = |cd { iv_path } |.

    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = me->m_handle
        command       = lv_command
        compress      = 'N' ##NO_TEXT
      TABLES
        data          = lt_cmdout
*       data          = et_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = me->m_handle
        fname         = iv_filename
      TABLES
        text          = et_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
ENDCLASS.
