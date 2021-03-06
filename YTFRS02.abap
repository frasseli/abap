*---------------------------------------------------------------------*
*  Author :  Flavio Rasseli
*  Date   :  06.03.2019
*  Description: Billing KPI report
*  Purpose:  This ABAP produces a listing of invoice documents
*            extracted from VBRK/VBRP
*---------------------------------------------------------------------*
REPORT ytfrs02.

*---------------------------------------------------------------------*
* Data types
*---------------------------------------------------------------------*
TYPES: BEGIN OF ty_selection,
         vbeln  TYPE vbrp-vbeln,
         fkdat  TYPE vbrk-fkdat,
         kunag  TYPE vbrk-kunag,
         fkart  TYPE vbrk-fkart,
         matnr  TYPE vbrp-matnr,
         matkl  TYPE vbrp-matkl,
         klagr  TYPE klah-klagr,
         class  TYPE klah-class,
         werks  TYPE vbrp-werks,
         bukrs  TYPE vbrk-bukrs,
         oland1 TYPE vbrp-aland,
         wkreg  TYPE vbrp-wkreg,
         dland1 TYPE vbrk-land1,
         dregio TYPE vbrk-regio,
         aland  TYPE vbrp-aland,
       END OF ty_selection.

*---------------------------------------------------------------------*
* Global data
*---------------------------------------------------------------------*
DATA: gs_selection          TYPE ty_selection,
      gt_report             TYPE TABLE OF zds_invoice_kpi,
      gr_negative_inv_types TYPE RANGE OF vbrk-fkart,
      go_grid               TYPE REF TO cl_gui_alv_grid,
      gt_field_catalog      TYPE lvc_t_fcat,
      gv_ok_code            TYPE sy-ucomm.


*---------------------------------------------------------------------*
* Selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS: s_vbeln  FOR gs_selection-vbeln,
                s_fkdat  FOR gs_selection-fkdat,
                s_kunag  FOR gs_selection-kunag,
                s_fkart  FOR gs_selection-fkart,
                s_matnr  FOR gs_selection-matnr,
                s_matkl  FOR gs_selection-matkl,
                s_klagr  FOR gs_selection-klagr,
                s_class  FOR gs_selection-class MATCHCODE OBJECT clas,
                s_werks  FOR gs_selection-werks,
                s_bukrs  FOR gs_selection-bukrs,
                s_oland1 FOR gs_selection-aland,
                s_oregio FOR gs_selection-wkreg,
                s_dland1 FOR gs_selection-dland1,
                s_dregio FOR gs_selection-dregio.

SELECTION-SCREEN END OF BLOCK blk1.

PARAMETERS: p_filen TYPE string LOWER CASE.

*---------------------------------------------------------------------*
* Program start
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM validate_selection.

  PERFORM get_invoice_return_types.

  PERFORM get_data.

*  PERFORM display_alv.

  PERFORM get_json.

*---------------------------------------------------------------------*
FORM get_data.

  SELECT vbrk~vbeln,
         vbrk~fkart,
         vbrk~waerk,
         vbrk~vkorg,
         vbrk~vtweg,
         vbrk~knumv,
         vbrk~fkdat,
         vbrk~inco1,
         vbrk~rfbsk,
         vbrk~zterm,
         vbrk~land1,
         vbrk~regio,
         vbrk~bukrs,
         vbrk~netwr,
         vbrk~mwsbk,
         vbrk~kunag,
         vbrk~spart,
         vbrk~vsbed,
         vbrp~posnr,
         vbrp~fkimg,
         vbrp~vrkme,
         vbrp~fklmg,
         vbrp~meins,
         vbrp~gsber,
         vbrp~netwr AS item_value,
         vbrp~mwsbp,
         vbrp~vgbel,
         vbrp~vgtyp,
         vbrp~aubel,
         vbrp~matnr,
         vbrp~arktx,
         vbrp~matkl,
         vbrp~werks,
         vbrp~aland,
         likp~kunnr,
         klah~class,
         kna1_sold~name1 AS sold_to_name,
         kna1_ship~name1 AS ship_to_name,
         tvfkt~vtext,
         vbfa~vbelv,
         likp~wadat_ist
    INTO TABLE @DATA(lt_vbrp)
    FROM vbrk INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
               LEFT JOIN likp ON vbrp~vgbel = likp~vbeln
              INNER JOIN kssk ON vbrp~matnr = kssk~objek
              INNER JOIN klah ON kssk~clint = klah~clint
               LEFT JOIN kna1 AS kna1_sold ON kna1_sold~kunnr = vbrk~kunag
               LEFT JOIN kna1 AS kna1_ship ON kna1_ship~kunnr = likp~kunnr
               LEFT JOIN vbfa ON vbfa~vbeln = vbrp~aubel
                             AND vbfa~posnn = vbrp~aupos
                             AND vbfa~vbtyp_v = 'G'
               LEFT JOIN tvfkt ON tvfkt~spras = @sy-langu
                              AND tvfkt~fkart = vbrk~fkart
   WHERE vbrk~kunag IN @s_kunag
     AND vbrk~vbeln IN @s_vbeln
     AND vbrk~fkdat IN @s_fkdat
     AND vbrk~fkart IN @s_fkart
     AND vbrk~bukrs IN @s_bukrs
     AND vbrk~land1 IN @s_dland1
     AND vbrk~regio IN @s_dregio
     AND vbrp~matnr IN @s_matnr
     AND vbrp~matkl IN @s_matkl
     AND vbrp~werks IN @s_werks
     AND vbrp~aland IN @s_oland1
     AND vbrp~wkreg IN @s_oregio
     AND kssk~mafid = 'O'
     AND kssk~klart = 'Z01'
     AND klah~klagr IN @s_klagr
     AND klah~class IN @s_class.

  IF sy-subrc NE 0.
    MESSAGE i000(zz) WITH text-e04.
  ENDIF.

  IF lt_vbrp IS NOT INITIAL.
    "Get payment terms descriptions
    SELECT zterm, text1
      INTO TABLE @DATA(lt_paymt_term_desc)
      FROM t052u
     WHERE spras = @sy-langu.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-e03.
    ELSE.
      SORT lt_paymt_term_desc BY zterm ASCENDING.
    ENDIF.

    "Get Shipping conditions descriptions
    SELECT vsbed, vtext
      INTO TABLE @DATA(lt_ship_cond_desc)
      FROM tvsbt
     WHERE spras = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_ship_cond_desc BY vsbed ASCENDING.
    ENDIF.

    "Select FI document (open items) to calculate due date
    SELECT DISTINCT
           vbeln,
           zfbdt,
           zbd1t,
           zbd2t,
           zbd3t,
           shkzg,
           rebzg,
           belnr
      INTO TABLE @DATA(lt_bsid_bsad)
      FROM bsid
       FOR ALL ENTRIES IN @lt_vbrp
     WHERE vbeln EQ @lt_vbrp-vbeln.
    IF sy-subrc EQ 0.
      SORT lt_bsid_bsad BY vbeln ASCENDING.
    ENDIF.

    "Select FI document (cleared items) to calculate due date
    SELECT DISTINCT
           vbeln,
           zfbdt,
           zbd1t,
           zbd2t,
           zbd3t,
           shkzg,
           rebzg,
           belnr
      APPENDING TABLE @lt_bsid_bsad
      FROM bsad
       FOR ALL ENTRIES IN @lt_vbrp
     WHERE vbeln EQ @lt_vbrp-vbeln.
    IF sy-subrc EQ 0.
      SORT lt_bsid_bsad BY vbeln ASCENDING.
    ENDIF.

  ENDIF.

  "Translate the data into the report structure
  LOOP AT lt_vbrp REFERENCE INTO DATA(lr_vbrp).

    DATA(lv_index_lt_vbrp) = sy-tabix.

    "The net value for the invoice should only be present in the first line for the same invoice,
    "the subsequent line items for the invoice only display the item net value
    ADD 1 TO lv_index_lt_vbrp.
    READ TABLE lt_vbrp INDEX lv_index_lt_vbrp REFERENCE INTO DATA(lr_vbrp_next).
    IF sy-subrc EQ 0.
      IF lr_vbrp->vbeln EQ lr_vbrp_next->vbeln.
        CLEAR lr_vbrp_next->netwr.
        CLEAR lr_vbrp_next->mwsbk.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO gt_report REFERENCE INTO DATA(lr_report).
    MOVE-CORRESPONDING lr_vbrp->* TO lr_report->*.

    "Invert the value of Cancel and Return invoices
    IF lr_vbrp->fkart IN gr_negative_inv_types.
      lr_vbrp->netwr = lr_vbrp->netwr * -1.
      lr_vbrp->item_value = lr_vbrp->item_value * -1.
      lr_report->mwsbk = lr_vbrp->mwsbk * -1.
      lr_report->mwsbp = lr_vbrp->mwsbp * -1.
    ENDIF.

    MOVE lr_vbrp->netwr TO lr_report->invoice_value.
    MOVE lr_vbrp->item_value TO lr_report->lineitem_value.

    "Read payment terms description
    READ TABLE lt_paymt_term_desc WITH KEY zterm = lr_vbrp->zterm REFERENCE INTO DATA(lr_paymt_term_desc) BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE lr_paymt_term_desc->text1 TO lr_report->text1.
    ENDIF.

    "Read Shipping conditions description
    READ TABLE lt_ship_cond_desc WITH KEY vsbed = lr_vbrp->vsbed REFERENCE INTO DATA(lr_ship_cond_desc) BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE lr_ship_cond_desc->vtext TO lr_report->vtext_shipcond.
    ENDIF.

    "Read invoice due date from FI document
    READ TABLE lt_bsid_bsad WITH KEY vbeln = lr_vbrp->vbeln REFERENCE INTO DATA(lr_bsid_bsad) BINARY SEARCH.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = lr_bsid_bsad->zfbdt
          i_zbd1t = lr_bsid_bsad->zbd1t
          i_zbd2t = lr_bsid_bsad->zbd2t
          i_zbd3t = lr_bsid_bsad->zbd3t
          i_shkzg = lr_bsid_bsad->shkzg
          i_rebzg = lr_bsid_bsad->rebzg
        IMPORTING
          e_faedt = lr_report->yyvalut.

      MOVE lr_bsid_bsad->belnr TO lr_report->belnr.

    ENDIF.

    "If Reference document is not Delivery, clear the field in the output
    IF lr_vbrp->vgtyp NE 'J'.
      CLEAR lr_report->vgbel.
    ENDIF.

  ENDLOOP.

ENDFORM.


*---------------------------------------------------------------------*
FORM display_alv.

  DATA: ls_field_catalog    TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZDS_INVOICE_KPI'
    CHANGING
      ct_fieldcat            = gt_field_catalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-e02.
  ENDIF.

  LOOP AT gt_field_catalog REFERENCE INTO DATA(lr_field_catalog).

    CASE lr_field_catalog->fieldname.
      WHEN 'VBELN'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c01.

      WHEN 'BELNR'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c40.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c41.

      WHEN 'FKDAT'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c02.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c03.

      WHEN 'FKART'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c04.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c05.

      WHEN 'LAND1'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c06.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c37.

      WHEN 'ALAND'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c38.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c39.

      WHEN 'REGIO'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c07.

      WHEN 'TEXT1'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c08.

      WHEN 'KNUMV'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c09.

      WHEN 'INVOICE_VALUE'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c10.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c11.

      WHEN 'LINEITEM_VALUE'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c12.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c13.

      WHEN 'VTEXT'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c04.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c05.

      WHEN 'ARKTX'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c16.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c17.

      WHEN 'FKIMG'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c18.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c19.

      WHEN 'VRKME'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c20.

      WHEN 'FKLMG'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c21.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c22.

      WHEN 'MEINS'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c23.

      WHEN 'VGBEL'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c24.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c25.

      WHEN 'WADAT_IST'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c26.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c27.

      WHEN 'AUBEL'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c28.

      WHEN 'VBELV'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s =
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c29.

      WHEN 'VTEXT_SHIPCOND'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c35.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c36.

      WHEN 'MWSBP'.
        lr_field_catalog->reptext = lr_field_catalog->scrtext_s = text-c14.
        lr_field_catalog->scrtext_m = lr_field_catalog->scrtext_l = text-c15.

      WHEN 'ZTERM' OR 'VSBED'.
        lr_field_catalog->no_out = abap_true.

      WHEN OTHERS.
        "Do nothing
    ENDCASE.

    lr_field_catalog->col_opt = abap_true.
  ENDLOOP.

  "Display the ALV Grid
  CREATE OBJECT go_grid
    EXPORTING
      i_parent = cl_gui_container=>screen0.

  go_grid->set_table_for_first_display(
    CHANGING
      it_outtab       = gt_report
      it_fieldcatalog = gt_field_catalog ).

  CALL SCREEN 100.

ENDFORM.

*---------------------------------------------------------------------*
FORM validate_selection.

  IF s_vbeln  IS INITIAL AND
     s_fkdat  IS INITIAL AND
     s_kunag  IS INITIAL AND
     s_fkart  IS INITIAL AND
     s_matnr  IS INITIAL AND
     s_matkl  IS INITIAL AND
     s_klagr  IS INITIAL AND
     s_class  IS INITIAL AND
     s_werks  IS INITIAL AND
     s_bukrs  IS INITIAL AND
     s_oland1 IS INITIAL AND
     s_oregio IS INITIAL AND
     s_dland1 IS INITIAL AND
     s_dregio IS INITIAL.
    MESSAGE i000(zz) WITH text-e01.
    STOP.
  ENDIF.

ENDFORM.

FORM get_invoice_return_types.

  gr_negative_inv_types = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZS1' )
                                                            ( low = 'ZS2' )
                                                            ( low = 'ZS5' )
                                                            ( low = 'ZS6' )
                                                            ( low = 'ZCR1' )
                                                            ( low = 'ZCR2' )
                                                            ( low = 'ZCR3' )
                                                            ( low = 'ZRE1' ) ).

ENDFORM.

*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET TITLEBAR  'KPI_REPORT'.
  SET PF-STATUS 'ALV_0100'.

ENDMODULE.

*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gv_ok_code.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      CALL METHOD go_grid->free.
      CALL METHOD cl_gui_cfw=>flush.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR gv_ok_code.

ENDMODULE.


FORM get_json.

  DATA: l_serializer TYPE REF TO cl_trex_json_serializer,
        json_c       TYPE string,
        json_t       TYPE TABLE OF string.

  CREATE OBJECT l_serializer
    EXPORTING
      data = gt_report.      " your data here!

  l_serializer->serialize( ).
  json_c = l_serializer->get_data( ).

  APPEND json_c TO json_t.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                  = p_filen    " Name of file
    CHANGING
      data_tab                  = json_t    " Transfer table
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
    MESSAGE 'Error' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
