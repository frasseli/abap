*---------------------------------------------------------------------*
*  Author :  Flavio Rasseli
*  Date   :  28.03.2019
*  Description: Open Orders KPI report
*  Purpose:  This ABAP produces a listing of Sales documents
*  using the same selection and list structure as in VA05 transaction
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*                                                                      *
*   01) Change number : CH01                                           *
*       Author        : FRASSEL                                        *
*       Date          : 20-Jun-2019                                    *
*       Reference     : SR15786                                        *
*       Description   : Add Billing info in the output list:           *
*                       - Invoice number - VBRK-VBELN                  *
*                       - Created By     - VBRK-ERNAM                  *
*                       - Created On     - VBRK-ERDAT                  *
*----------------------------------------------------------------------*
*   02) Change number : CH02                                           *
*       Author        : FRASSEL                                        *
*       Date          : 25-Jun-2019                                    *
*       Reference     : SR15786                                        *
*       Description   : The report was not considering Credit/Debit    *
*                       Memos invoices in order to display the billing *
*                       info added in CH01. Add this logic in the      *
*                       program.                                       *
*----------------------------------------------------------------------*
*   03) Change number : CH03                                           *
*       Author        : TPERAVALI                                      *
*       Date          : 25-July-2019                                   *
*       Reference     : SR15786                                        *
*       Description   : The report was changed to handle below issues  *
*                       1)Dont display Pricing status for billed docs  *
*                       2)Pass ZF2 doc type in the billing doc field   *
*                                                                      *
*----------------------------------------------------------------------*
*   04) Change number : CH04                                           *
*       Author        : SKESANI                                        *
*       Date          : 13-AUG-2019                                    *
*       Reference     : SR17949                                        *
*       Transport     : AGDK902040                                     *
*       Description   : The report was changed to activate             *
*                       Select layout & save layout option             *
*                       (Modified Table display to OO ALV)             *
*----------------------------------------------------------------------*
REPORT zsd_order_kpi_report.

*--------------------------------------------------------------------*
* Global data
TABLES: vbcom.

"Structure to calculate order total tax value
TYPES: BEGIN OF ty_order_tax,
         vbeln TYPE vbap-vbeln,
         mwsbp TYPE vbap-mwsbp,
       END OF ty_order_tax.

DATA: gt_vbmtv         TYPE TABLE OF vbmtv,
      w_vbmtv          LIKE LINE OF gt_vbmtv,
      gt_order_tax     TYPE TABLE OF ty_order_tax,
      go_grid          TYPE REF TO cl_gui_alv_grid,
      gt_field_catalog TYPE lvc_t_fcat,
      gv_ok_code       TYPE sy-ucomm.


*--------------------------------------------------------------------*
* Selection criteria

SELECTION-SCREEN BEGIN OF BLOCK b0.

SELECTION-SCREEN SKIP.

PARAMETERS: p_vkorg TYPE vbcom-vkorg OBLIGATORY MEMORY ID vko.

SELECT-OPTIONS: s_kunde FOR vbcom-kunde MATCHCODE OBJECT debi,
                s_matnr FOR vbcom-matnr,
                s_auart FOR vbcom-auart,
                s_vbeln FOR vbcom-vbeln MATCHCODE OBJECT psfck.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: a_audat FOR vbcom-audat NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK  b1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS p_open RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 03(29) text-t03 FOR FIELD p_open.
PARAMETERS p_myor AS CHECKBOX.
SELECTION-SCREEN COMMENT 48(30) text-t04 FOR FIELD p_myor.

SELECTION-SCREEN END OF LINE.

PARAMETERS p_all  RADIOBUTTON GROUP rb1.

SELECTION-SCREEN END OF BLOCK  b2.
SELECTION-SCREEN END OF BLOCK  b0.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.

  PERFORM display_alv.


*--------------------------------------------------------------------*
FORM get_data.

  DATA: ls_order_tax TYPE ty_order_tax.

  vbcom-vkorg     = p_vkorg.
  vbcom-parvw     = 'AG'.
  vbcom-audat     = a_audat-low.
  vbcom-audat_bis = a_audat-high.
  vbcom-zuart     = 'A'.
  vbcom-trvog     = '0'.
  vbcom-vboff     = abap_true.
  vbcom-stat_dazu = abap_true.
  vbcom-name_dazu = abap_true.
  vbcom-mandt     = sy-mandt.
*  vbcom-vboff     = p_open.
  "Always send all orders flag, because the filter will be done by the program later
  vbcom-vball     = abap_true.
  vbcom-vberf     = p_myor.

  CALL FUNCTION 'RV_SALES_DOCUMENT_VIEW_3'
    EXPORTING
      vbcom      = vbcom
      bstkd_flag = abap_true
    TABLES
      lvbmtv     = gt_vbmtv.

  IF s_matnr IS NOT INITIAL.
    DELETE gt_vbmtv WHERE matnr NOT IN s_matnr.
  ENDIF.

  IF s_kunde IS NOT INITIAL.
    DELETE gt_vbmtv WHERE kunnr NOT IN s_kunde.
  ENDIF.

  IF s_vbeln IS NOT INITIAL.
    DELETE gt_vbmtv WHERE vbeln NOT IN s_vbeln.
  ENDIF.

  IF s_auart IS NOT INITIAL.
    DELETE gt_vbmtv WHERE auart NOT IN s_auart.
  ENDIF.

  IF gt_vbmtv IS NOT INITIAL.

    "Filter only Open Orders that do not have invoices done
*    IF p_open EQ abap_true. "CH01
    SELECT vbelv,
           posnv,
           vbfa~vbeln,
           posnn,
           vbrk~fkart, "CH01
           vbrk~ernam, "CH01
           vbrk~erdat  "CH01
      FROM vbfa
           INNER JOIN vbrk ON vbrk~vbeln = vbfa~vbeln "CH01
      INTO TABLE @DATA(lt_invoices)
       FOR ALL ENTRIES IN @gt_vbmtv
     WHERE vbelv   = @gt_vbmtv-vbeln
       AND posnv   = @gt_vbmtv-posnr
       AND vbtyp_n IN ('M', 'O', 'P'). "CH02
    IF sy-subrc EQ 0.
      SORT lt_invoices BY vbelv posnv.
    ENDIF.

    "Select the item status in order to check
    "the billing status for Orders
    SELECT vbeln,
           posnr,
           fksta,
           fksaa
      FROM vbup
      INTO TABLE @DATA(lt_item_status)
       FOR ALL ENTRIES IN @gt_vbmtv
     WHERE vbeln = @gt_vbmtv-vbeln
       AND posnr = @gt_vbmtv-posnr.
    IF sy-subrc EQ 0.
      SORT lt_item_status BY vbeln posnr.
    ENDIF.
*    ENDIF. "CH01

    "Select Contracts for Orders
    SELECT vbeln AS order,
           posnn AS order_item,
           vbelv AS contract,
           posnv AS contract_item
      FROM vbfa
      INTO TABLE @DATA(lt_contracts)
       FOR ALL ENTRIES IN @gt_vbmtv
     WHERE vbeln   = @gt_vbmtv-vbeln
       AND posnn   = @gt_vbmtv-posnr
       AND vbtyp_n = 'C'
       AND vbtyp_v = 'G'.
    IF sy-subrc EQ 0.
      SORT lt_contracts BY order order_item.
    ENDIF.

    "Select Deliveries for Orders
    SELECT vbelv AS order,
           posnv AS order_item,
           vbeln AS delivery,
           posnn AS delivery_item
      FROM vbfa
      INTO TABLE @DATA(lt_deliveries)
       FOR ALL ENTRIES IN @gt_vbmtv
     WHERE vbelv   = @gt_vbmtv-vbeln
       AND posnv   = @gt_vbmtv-posnr
       AND vbtyp_n = 'J'.
    IF sy-subrc EQ 0.
      SORT lt_deliveries BY order order_item.

      "Select Deliveries date and PGI date for Orders
      SELECT vbeln,
             bldat,
             wadat_ist
        FROM likp
        INTO TABLE @DATA(lt_delivery_dates)
         FOR ALL ENTRIES IN @lt_deliveries
       WHERE vbeln = @lt_deliveries-delivery.
      IF sy-subrc EQ 0.
        SORT lt_delivery_dates BY vbeln.
      ENDIF.

      "Select the item status in order to check
      "the billing status for deliveries
      SELECT vbeln,
             posnr,
             fksta,
             fksaa
        FROM vbup
        APPENDING TABLE @lt_item_status
         FOR ALL ENTRIES IN @lt_deliveries
       WHERE vbeln = @lt_deliveries-delivery
         AND posnr = @lt_deliveries-delivery_item.
      IF sy-subrc EQ 0.
        SORT lt_item_status BY vbeln posnr.
      ENDIF.
    ENDIF.

    "Get the pricing condition record and status
    SELECT vbak~vbeln,
           vbak~knumv,
           oicq7~forstatus
      INTO TABLE @DATA(lt_pricing_status)
      FROM vbak INNER JOIN oicq7 ON oicq7~knumv = vbak~knumv
       FOR ALL ENTRIES IN @gt_vbmtv
     WHERE vbak~vbeln = @gt_vbmtv-vbeln.
    IF sy-subrc EQ 0.
      SORT lt_pricing_status BY vbeln.
    ENDIF.

    "Get tax amount for orders
    SELECT vbeln, posnr, mwsbp
      FROM vbap
      INTO TABLE @DATA(lt_vbap_tax)
       FOR ALL ENTRIES IN @gt_vbmtv
      WHERE vbeln = @gt_vbmtv-vbeln
        AND posnr = @gt_vbmtv-posnr.
    IF sy-subrc EQ 0.
      SORT lt_vbap_tax BY vbeln posnr.
    ENDIF.

    "Get Order type descriptions
    SELECT auart, bezei
      INTO TABLE @DATA(lt_tvakt)
      FROM tvakt
     WHERE spras = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_tvakt BY auart.
    ENDIF.

    "Get Order reasons descriptions
    SELECT augru, bezei
      INTO TABLE @DATA(lt_tvaut)
      FROM tvaut
     WHERE spras = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_tvaut BY augru.
    ENDIF.

    "Get Order reasons for rejection descriptions
    SELECT abgru, bezei
      INTO TABLE @DATA(lt_tvagt)
      FROM tvagt
     WHERE spras = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_tvagt BY abgru.
    ENDIF.

    "Get text values for domain Formula Status
    SELECT domvalue_l,
           ddtext
      INTO TABLE @DATA(lt_forstatus_text)
      FROM dd07t
     WHERE domname = 'OIC_FSTAT'
       AND ddlanguage = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_forstatus_text BY domvalue_l.
    ENDIF.

*--------------------------------------------------------------------*
*   SR15786 - FRASSEL - 20/06/2019 - CH01 - Start
    IF lt_invoices IS NOT INITIAL.
      SELECT DISTINCT vbeln, fkart, ernam, erdat
        INTO TABLE @DATA(lt_vbrk)
        FROM vbrk
         FOR ALL ENTRIES IN @lt_invoices
       WHERE vbeln = @lt_invoices-vbeln.
      IF sy-subrc EQ 0.
        SORT lt_vbrk BY vbeln.
      ENDIF.
    ENDIF.
*   SR15786 - FRASSEL - 20/06/2019 - CH01 - End
*--------------------------------------------------------------------*

  ENDIF. "gt_vbmtv IS NOT INITIAL

  LOOP AT gt_vbmtv REFERENCE INTO DATA(lr_vbmtv).

    DATA(lv_vbmtv_index) = sy-tabix.

    IF p_open EQ abap_true.
      "Check if the order has an invoice
      READ TABLE lt_invoices WITH KEY vbelv = lr_vbmtv->vbeln
                                      posnv = lr_vbmtv->posnr BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        "If it has, it should not be caneclled, so check the item status for the delivery as well
        READ TABLE lt_deliveries WITH KEY order = lr_vbmtv->vbeln
                                          order_item = lr_vbmtv->posnr
                                          REFERENCE INTO DATA(lr_delivery) BINARY SEARCH.
        IF sy-subrc EQ 0.
          "Get the item status for Delivery
          READ TABLE lt_item_status WITH KEY vbeln = lr_delivery->delivery
                                             posnr = lr_delivery->delivery_item
                                             REFERENCE INTO DATA(lr_item_status) BINARY SEARCH.
*          IF sy-subrc EQ 0 AND lr_item_status->fksta NE 'A'. "Not yet processed
          IF sy-subrc EQ 0.
            IF lr_item_status->fksta EQ 'C'. "Completely processed
              DELETE gt_vbmtv INDEX lv_vbmtv_index.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        "If there is no delivery for this order, check the item status for the order itself
        READ TABLE lt_item_status WITH KEY vbeln = lr_vbmtv->vbeln
                                           posnr = lr_vbmtv->posnr
                                           REFERENCE INTO lr_item_status BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF lr_item_status->fksaa EQ 'C'. "Completely processed
            DELETE gt_vbmtv INDEX lv_vbmtv_index.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "CH01 - Start
    READ TABLE lt_invoices WITH KEY vbelv = lr_vbmtv->vbeln
                                    posnv = lr_vbmtv->posnr BINARY SEARCH REFERENCE INTO DATA(lr_invoice).
    IF sy-subrc EQ 0.
      "Read billing info
      READ TABLE lt_vbrk WITH KEY vbeln = lr_invoice->vbeln
*                                  fkart = 'ZF2'      "CH03   "comment CH04
                                  BINARY SEARCH REFERENCE INTO DATA(lr_vbrk).
      IF sy-subrc EQ 0.
        lr_vbmtv->zzinvoiceno = lr_vbrk->vbeln.
        lr_vbmtv->zzinvusnam  = lr_vbrk->ernam.
        lr_vbmtv->zzinvdate   = lr_vbrk->erdat.
      ENDIF.
      "Read the ZDIF invoice
      READ TABLE lt_invoices WITH KEY vbelv = lr_vbmtv->vbeln
                                      posnv = lr_vbmtv->posnr
                                      fkart = 'ZDIF' REFERENCE INTO DATA(lr_invoice_zdif).
      IF sy-subrc EQ 0.
        lr_vbmtv->zzdifinv = lr_invoice_zdif->vbeln.
        lr_vbmtv->zzdifinvusnam = lr_invoice_zdif->ernam.
        lr_vbmtv->zzdifinvdate = lr_invoice_zdif->erdat.
      ENDIF.
    ENDIF.
    "CH01 - End

    "The net value for the Order should only be present in the first line for the same Order,
    "the subsequent line items for the Order only display the item net value
    ADD 1 TO lv_vbmtv_index.
    READ TABLE gt_vbmtv INDEX lv_vbmtv_index REFERENCE INTO DATA(lr_vbmtv_next).
    IF sy-subrc EQ 0.
      IF lr_vbmtv->vbeln EQ lr_vbmtv_next->vbeln.
        CLEAR lr_vbmtv_next->netwr_ak.
      ENDIF.
    ENDIF.

    lr_vbmtv->zzordernet = lr_vbmtv->netwr_ak.

    "Read the contract number
    READ TABLE lt_contracts WITH KEY order = lr_vbmtv->vbeln
                                     order_item = lr_vbmtv->posnr
                                     REFERENCE INTO DATA(lr_contract) BINARY SEARCH.
    IF sy-subrc EQ 0.
      lr_vbmtv->zzcontract = lr_contract->contract.
    ENDIF.

    "Read delivery number and delivery dates
    READ TABLE lt_deliveries WITH KEY order = lr_vbmtv->vbeln
                                      order_item = lr_vbmtv->posnr
                                      REFERENCE INTO lr_delivery BINARY SEARCH.
    IF sy-subrc EQ 0.
      lr_vbmtv->zzdelivnum = lr_delivery->delivery.
      READ TABLE lt_delivery_dates WITH KEY vbeln = lr_delivery->delivery
                                            REFERENCE INTO DATA(lr_delivery_dates) BINARY SEARCH.
      IF sy-subrc EQ 0.
        lr_vbmtv->zzdelivdate = lr_delivery_dates->bldat.
        lr_vbmtv->zzactpgidt  = lr_delivery_dates->wadat_ist.
      ENDIF.
    ENDIF.

    "Read descriptions
    READ TABLE lt_tvakt WITH KEY auart = lr_vbmtv->auart REFERENCE INTO DATA(lr_tvakt) BINARY SEARCH.
    IF sy-subrc EQ 0.
      lr_vbmtv->zzordertype = lr_tvakt->bezei.
    ENDIF.

    READ TABLE lt_tvaut WITH KEY augru = lr_vbmtv->augru REFERENCE INTO DATA(lr_tvaut) BINARY SEARCH.
    IF sy-subrc EQ 0.
      lr_vbmtv->zzorderreas = lr_tvaut->bezei.
    ENDIF.

    READ TABLE lt_tvagt WITH KEY abgru = lr_vbmtv->abgru REFERENCE INTO DATA(lr_tvagt) BINARY SEARCH.
    IF sy-subrc EQ 0.
      lr_vbmtv->zzreasrej = lr_tvagt->bezei.
    ENDIF.

    "CH03 - Start
    "Check if the order has an invoice
    READ TABLE lt_invoices WITH KEY vbelv = lr_vbmtv->vbeln
                                    posnv = lr_vbmtv->posnr BINARY SEARCH TRANSPORTING NO FIELDS.

    IF sy-subrc EQ 0.

      "CH03 - End
      "Read pricing status
      READ TABLE lt_pricing_status WITH KEY vbeln = lr_vbmtv->vbeln
                                            REFERENCE INTO DATA(lr_pricing_status) BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_forstatus_text WITH KEY domvalue_l = lr_pricing_status->forstatus
                                              REFERENCE INTO DATA(lr_forstatus_text).
        IF sy-subrc EQ 0.
          lr_vbmtv->zzpricestat = lr_forstatus_text->ddtext.
        ENDIF.
      ENDIF.
    ENDIF.   "CH03
    "Read tax amount
    READ TABLE lt_vbap_tax WITH KEY vbeln = lr_vbmtv->vbeln
                                    posnr = lr_vbmtv->posnr REFERENCE INTO DATA(lr_vbap_tax).
    IF sy-subrc EQ 0.
      MOVE lr_vbap_tax->mwsbp TO lr_vbmtv->zzitemtax.
    ENDIF.

    CLEAR ls_order_tax.
    MOVE lr_vbmtv->vbeln TO ls_order_tax-vbeln.
    MOVE lr_vbmtv->zzitemtax TO ls_order_tax-mwsbp.
    COLLECT ls_order_tax INTO gt_order_tax.

  ENDLOOP.

  "Get the Order total tax into the first item in the output table
  LOOP AT gt_order_tax INTO ls_order_tax.
    READ TABLE gt_vbmtv WITH KEY vbeln = ls_order_tax-vbeln REFERENCE INTO lr_vbmtv BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE ls_order_tax-mwsbp TO lr_vbmtv->zzordertax.
    ENDIF.
  ENDLOOP.


ENDFORM.

*--------------------------------------------------------------------*
FORM display_alv.

* Begin of comment CH04-

**  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
**    EXPORTING
**      i_structure_name       = 'VBMTV'
**    CHANGING
**      ct_fieldcat            = gt_field_catalog
**    EXCEPTIONS
**      inconsistent_interface = 1
**      program_error          = 2
**      OTHERS                 = 3.
**  IF sy-subrc NE 0.
**    MESSAGE e000(zz) WITH text-e02.
**  ENDIF.
**
**  "Format the list output Field catalog
**  PERFORM format_field_catalog CHANGING gt_field_catalog.
**
**  "Display the ALV Grid
**  CREATE OBJECT go_grid
**    EXPORTING
**      i_parent = cl_gui_container=>screen0.
**
**  go_grid->set_table_for_first_display(
**    CHANGING
**      it_outtab       = gt_vbmtv
**      it_fieldcatalog = gt_field_catalog ).
**
**  CALL SCREEN 100.

* End of comment CH04-

  TYPES: BEGIN OF ty_vbmtv1,
           zzcontract    TYPE zzcontract,
           vbeln         TYPE vbeln,
           vkorg         TYPE vkorg,
           vtweg         TYPE vtweg,
           spart         TYPE spart,
           auart         TYPE auart,
           zzordertype   TYPE zzordertype,
           erdat         TYPE erdat,
           ernam         TYPE ernam,
           kunnr         TYPE kunag,
           name1         TYPE name1_gp,
           cmgst_bez     TYPE yybezei,
           zzorderreas   TYPE zzorderreas,
           zzordernet    TYPE zzordernet,
           zzordertax    TYPE zzordertax,
           posnr         TYPE posnr,
           matnr         TYPE matnr,
           arktx         TYPE arktx,
           werks         TYPE werks,
           kwmeng        TYPE kwmeng,
           bmeng         TYPE bmeng,
           wldat         TYPE wldat,
           statu         TYPE stats,
           zzreasrej     TYPE zzreasrej,
           zzbilblock    TYPE zzbilblock,
           zzdelblock    TYPE zzdelblock,
           meins         TYPE meins,
           netpr         TYPE netpr,
           kpein         TYPE kpein,
           prsdt         TYPE prsdt,
           netwr         TYPE netwr,
           zzitemtax     TYPE zzitemtax,
           vrkme         TYPE vrkme,
           waerk         TYPE waerk,
           shkzg         TYPE shkzg,
           zzdelivnum    TYPE zzdelivnum,
           zzdelivdate   TYPE zzdelivdate,
           zzactpgidt    TYPE zzactpgidt,
           zzpricestat   TYPE zzpricestat,
           zzinvoiceno   TYPE zzinvoiceno,
           zzinvusnam    TYPE ernam,
           zzinvdate     TYPE erdat,
           zzdifinv      TYPE zzdifinv,
           zzdifinvusnam TYPE ernam,
           zzdifinvdate  TYPE erdat,
         END OF ty_vbmtv1.

  DATA: gt_vbmtv1 TYPE TABLE OF ty_vbmtv1,
        w_vbmtv1  LIKE LINE OF gt_vbmtv1.

* Begin of Insert CH04+
  DATA: lo_alv       TYPE REF TO cl_salv_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_layout    TYPE REF TO cl_salv_layout,
        lf_variant   TYPE slis_vari,
        ls_key       TYPE salv_s_layout_key.


  LOOP AT gt_vbmtv INTO w_vbmtv.

    MOVE-CORRESPONDING w_vbmtv TO w_vbmtv1.
    APPEND w_vbmtv1 TO gt_vbmtv1.
    CLEAR: w_vbmtv, w_vbmtv1.

  ENDLOOP.

  CALL METHOD cl_salv_table=>factory
*  EXPORTING
*    LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE
*    R_CONTAINER    =
*    CONTAINER_NAME =
    IMPORTING
      r_salv_table = lo_alv
    CHANGING
      t_table      = gt_vbmtv1.


* setting pf status
  lo_alv->set_screen_status(

    pfstatus      =  'ZSTANDARD'
    report        =  sy-repid
    set_functions = lo_alv->c_functions_all ).

*   Default Functions
  lo_functions = lo_alv->get_functions( ).
*    lo_functions->set_default( abap_true ).
  lo_functions->set_all( abap_true ).

* Setting up the Layout
  lo_layout = lo_alv->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_initial_layout( lf_variant ).

  lo_alv->display( ).
* End of insert CH04+
ENDFORM.

* Begin of Comment CH04-
***----------------------------------------------------------------------*
**MODULE status_0100 OUTPUT.
**
**  SET TITLEBAR  'KPI_REPORT'.
**  SET PF-STATUS 'ALV_0100'.
**
**ENDMODULE.
**
***----------------------------------------------------------------------*
**MODULE user_command_0100 INPUT.
**
**  CASE gv_ok_code.
**    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
**      CALL METHOD go_grid->free.
**      CALL METHOD cl_gui_cfw=>flush.
**      SET SCREEN 0.
**      LEAVE SCREEN.
**    WHEN OTHERS.
***     do nothing
**  ENDCASE.
**  CLEAR gv_ok_code.
**
**ENDMODULE.
* End of comment CH04-
*--------------------------------------------------------------------*
FORM format_field_catalog CHANGING pt_field_catalog TYPE lvc_t_fcat.

  DATA: lr_field_catalog TYPE REF TO lvc_s_fcat.

  LOOP AT pt_field_catalog REFERENCE INTO lr_field_catalog.

    CLEAR lr_field_catalog->no_out.

    CASE lr_field_catalog->fieldname.
      WHEN 'ZZCONTRACT'.
        lr_field_catalog->col_pos = 1.
        lr_field_catalog->col_opt = abap_true.
        lr_field_catalog->key = abap_true.
      WHEN 'VBELN'.
        lr_field_catalog->col_pos = 2.
        lr_field_catalog->col_opt = abap_true.
        lr_field_catalog->key = abap_true.
      WHEN 'VKORG'.
        lr_field_catalog->col_pos = 3.
      WHEN 'VTWEG'.
        lr_field_catalog->col_pos = 4.
      WHEN 'SPART'.
        lr_field_catalog->col_pos = 5.
      WHEN 'AUART'.
        lr_field_catalog->col_pos = 6.
      WHEN 'ZZORDERTYPE'.
        lr_field_catalog->col_pos = 7.
      WHEN 'ERDAT'.
        lr_field_catalog->col_pos = 8.
      WHEN 'ERNAM'.
        lr_field_catalog->col_pos = 9.
      WHEN 'KUNNR'.
        lr_field_catalog->col_pos = 10.
      WHEN 'NAME1'.
        lr_field_catalog->col_pos = 11.
      WHEN 'CMGST_BEZ'.
        lr_field_catalog->col_pos = 12.
      WHEN 'ZZORDERREAS'.
        lr_field_catalog->col_pos = 13.
      WHEN 'ZZORDERNET'.
        lr_field_catalog->col_pos = 14.
      WHEN 'ZZORDERTAX'.
        lr_field_catalog->col_pos = 15 .
      WHEN 'POSNR'.
        lr_field_catalog->col_pos = 16.
      WHEN 'MATNR'.
        lr_field_catalog->col_pos = 17.
      WHEN 'ARKTX'.
        lr_field_catalog->col_pos = 18.
      WHEN 'WERKS'.
        lr_field_catalog->col_pos = 19.
      WHEN 'KWMENG'.
        lr_field_catalog->col_pos = 20.
      WHEN 'BMENG'.
        lr_field_catalog->col_pos = 21.
      WHEN 'WLDAT'.
        lr_field_catalog->col_pos = 22.
      WHEN 'STATU'.
        lr_field_catalog->col_pos = 23.
      WHEN 'ZZREASREJ'.
        lr_field_catalog->col_pos = 24.
      WHEN 'ZZBILBLOCK'.
        lr_field_catalog->col_pos = 25.
      WHEN 'ZZDELBLOCK'.
        lr_field_catalog->col_pos = 26.
      WHEN 'MEINS'.
        lr_field_catalog->col_pos = 27.
      WHEN 'NETPR'.
        lr_field_catalog->col_pos = 28.
      WHEN 'KPEIN'.
        lr_field_catalog->col_pos = 29.
      WHEN 'PRSDT'.
        lr_field_catalog->col_pos = 30.
      WHEN 'NETWR'.
        lr_field_catalog->col_pos = 31.
      WHEN 'ZZITEMTAX'.
        lr_field_catalog->col_pos = 32.
      WHEN 'VRKME'.
        lr_field_catalog->col_pos = 33.
      WHEN 'WAERK'.
        lr_field_catalog->col_pos = 34.
      WHEN 'SHKZG'.
        lr_field_catalog->col_pos = 35.
      WHEN 'ZZDELIVNUM'.
        lr_field_catalog->col_pos = 36.
      WHEN 'ZZDELIVDATE'.
        lr_field_catalog->col_pos = 37.
      WHEN 'ZZACTPGIDT'.
        lr_field_catalog->col_pos = 38.
      WHEN 'ZZPRICESTAT'.
        lr_field_catalog->col_pos = 39.
        lr_field_catalog->col_opt = abap_true.

        "CH01 - Start
      WHEN 'ZZINVOICENO'.
        lr_field_catalog->col_pos = 40.
      WHEN 'ZZINVUSNAM'.
        lr_field_catalog->col_pos = 41.
      WHEN 'ZZINVDATE'.
        lr_field_catalog->col_pos = 42.
      WHEN 'ZZDIFINV'.
        lr_field_catalog->col_pos = 43.
      WHEN 'ZZDIFINVUSNAM'.
        lr_field_catalog->col_pos = 44.
      WHEN 'ZZDIFINVDATE'.
        lr_field_catalog->col_pos = 45.
        "CH01 - End

      WHEN OTHERS.
        lr_field_catalog->no_out = abap_true.
        lr_field_catalog->col_pos = sy-tabix + 100.
    ENDCASE.

  ENDLOOP.

ENDFORM.
