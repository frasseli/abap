class ZCL_ZSD_KPI_DPC_EXT definition
  public
  inheriting from ZCL_ZSD_KPI_DPC
  create public .

public section.
protected section.

  methods OPENSALESORDERSS_GET_ENTITYSET
    redefinition .
  methods INVOICESSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSD_KPI_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSD_KPI_DPC_EXT->INVOICESSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSD_KPI_MPC=>TT_INVOICES
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD invoicesset_get_entityset.

    zcl_sales_kpi_helper=>get_invoices(
      EXPORTING
        iv_company_code = '0524'
        iv_start_date   = '20190101'
        iv_end_date     = '20190115'
      IMPORTING
        et_invoices     = et_entityset ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSD_KPI_DPC_EXT->OPENSALESORDERSS_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSD_KPI_MPC=>TT_OPENSALESORDERS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD opensalesorderss_get_entityset.

*  READ TABLE IT_FILTER_SELECT_OPTIONS WITH KEY PROPERTY = 'VKORG' INTO DATA(ls_vkorg_key).
*  IF sy-subrc eq 0.
*    ls_vkorg_key-select_options
*  ENDIF.
*

    DATA: ls_vbcom TYPE vbcom.

    ls_vbcom-vkorg     = '0524'.
    ls_vbcom-parvw     = 'AG'.
    ls_vbcom-audat     = '20190101'.
    ls_vbcom-audat_bis = sy-datum.
    ls_vbcom-zuart     = 'A'.
    ls_vbcom-trvog     = '0'.
    ls_vbcom-vboff     = abap_true.
    ls_vbcom-stat_dazu = abap_true.
    ls_vbcom-name_dazu = abap_true.
    ls_vbcom-mandt     = sy-mandt.
*  ls_vbcom-vboff     = p_open.
    "Always send all orders flag, because the filter will be done by the program later
    ls_vbcom-vball     = abap_true.
    ls_vbcom-vberf     = abap_false.

    zcl_sales_kpi_helper=>get_open_orders(
      EXPORTING
        iv_vbcom       = ls_vbcom
        iv_open        = abap_false
      IMPORTING
        et_open_orders = et_entityset    " View: Order Items for Material
    ).

    DELETE et_entityset FROM 11.

**TRY.
*CALL METHOD SUPER->OPENSALESORDERSS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

  ENDMETHOD.
ENDCLASS.
