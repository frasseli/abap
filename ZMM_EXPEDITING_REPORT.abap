*---------------------------------------------------------------------*
*  Author :  Flavio Rasseli
*  Date   :  28.10.2019
*  Ticket :  SR18491
*  Description: Material-Vendor Expediting transaction report
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*                                                                      *
*   01) Change number : CH01                                           *
*       Author        : Pennisi Davide                                 *
*       Date          : 24.04.2020                                     *
*       Reference     :                                                *
*       Description   : Add column POH and call transaction ME23N      *
*                       Remove constant ekbe-vgabe  VALUE '1'         *
*                      Add default value to field ekpo-pstyp DEFAULT '0' *
*----------------------------------------------------------------------*

REPORT zmm_expediting_report.

*--------------------------------------------------------------------*
* Global data
TABLES: mara, ekko, ekpo, eket, eban, marc, ekbe.

CLASS lcl_handler DEFINITION DEFERRED.

TYPES: BEGIN OF ty_exped_l1_pr,
         banfn TYPE eban-banfn,
         bnfpo TYPE eban-bnfpo.
        INCLUDE STRUCTURE zmms_exped_l1.
TYPES END OF ty_exped_l1_pr.

DATA: gt_exped_l1      TYPE TABLE OF zmms_exped_l1,
      gt_exped_l1_pr   TYPE TABLE OF ty_exped_l1_pr,
      gt_exped_l2_pr   TYPE TABLE OF zmms_exped_l2_pr,
      gt_exped_l2_po   TYPE TABLE OF zmms_exped_l2_po,
      gt_exped_flat    TYPE TABLE OF zmms_exped_flat,
      go_handler_class TYPE REF TO lcl_handler,
* Start of  Insert CH01
      gd_goods_receipt TYPE ekbe-vgabe.
* End of  Insert CH01
CONSTANTS: gc_service_po_itm_cat TYPE ekpo-pstyp   VALUE '9',
* Start of Comment CH01
*           gc_goods_receipt      TYPE ekbe-vgabe   VALUE '1',
*  End of Comment CH01
           gc_expd_del_dt_event  TYPE txpdat-event VALUE '00002'.

*--------------------------------------------------------------------*
CLASS lcl_handler DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column.

ENDCLASS.

*--------------------------------------------------------------------*
* Selection criteria
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-t01.
SELECTION-SCREEN SKIP.

PARAMETERS: rb_rep1 RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_mtart FOR mara-mtart,
                s_werks FOR marc-werks DEFAULT '0L07',
                s_dismm FOR marc-dismm DEFAULT 'VB'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

PARAMETERS: rb_rep2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr,
                s_wrk_po FOR ekpo-werks DEFAULT '0L07',
                s_bsart FOR ekko-bsart,
                s_knttp FOR ekpo-knttp,
* Start of  Comment CH01
*                s_pstyp FOR ekpo-pstyp,
* Start of  Comment CH01
* Start of  Insert CH01
                s_pstyp FOR ekpo-pstyp DEFAULT '9' OPTION NE,
* Start of  Insert CH01
                s_elikz FOR ekpo-elikz,
* Start of Insert CH01
                s_vgabe FOR ekbe-vgabe DEFAULT '1',
* End of Insert CH01
                s_banfn FOR eban-banfn,
                s_ebeln FOR ekko-ebeln,
                s_eindt FOR eket-eindt,
                s_bedat FOR ekko-bedat.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b0.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF rb_rep1 EQ abap_true.
    PERFORM get_data_by_material.
  ENDIF.

  IF rb_rep2 EQ abap_true.
    PERFORM get_data_by_pur_doc.
  ENDIF.


*--------------------------------------------------------------------*
FORM get_data_by_material.

  "-------------------------------------------------------------------
  "Local types
  TYPES: BEGIN OF ty_po_item,
           ebeln TYPE ekpo-ebeln,
           ebelp TYPE ekpo-ebelp,
         END OF ty_po_item.

  TYPES: BEGIN OF ty_po_pr_quant,
           matnr    TYPE eban-matnr,
           menge    TYPE ekpo-menge,
           menge_pr TYPE eban-menge,
         END OF ty_po_pr_quant.

  "-------------------------------------------------------------------
  "Local data
  DATA: ls_exped_l1    TYPE zmms_exped_l1,
        lt_po_list     TYPE TABLE OF ty_po_item,
        lt_txpdat      TYPE TABLE OF txpdat,
        lt_po_pr_quant TYPE TABLE OF ty_po_pr_quant,
        ls_po_pr_quant TYPE ty_po_pr_quant.

  "-------------------------------------------------------------------
  "Select from Material Master first, then get PO and PR data
  SELECT mara~matnr,
         ekpo~txz01 AS maktx,
         marc~ausdt,
         mard~werks,
         marc~ekgrp,
         marc~dismm,
         marc~dispo,
         "marc~plifz,
         ekpo~plifz,
         marc~webaz,
         marc~perkz,
         marc~disls,
         marc~eisbe,
         marc~bstmi,
         marc~bstma,
         marc~bstrf,
         marc~prctr,
         mard~labst,
         marc~minbe,
         marc~mabst,
         eban~banfn,
         eban~bnfpo,
         eban~afnam,
         eban~lfdat,
         eban~menge AS menge_pr,
         eban~flief,
         eban~konnr,
         eban~ktpnr,
         ekko~ebeln,
         ekko~bsart,
         ekpo~knttp,
         ekpo~ebelp,
         ekko~bedat,
         ekko~lifnr,
         lfa1~name1,
         ekpo~menge,
         ekpo~meins,
         ekpo~netpr,
         eket~eindt,
         eban~statu, " NE 'B'
         eban~frgkz, " EQ '1'
         eban~loekz, " EQ @space.
         ekbe~budat AS gr_date,
         @sy-datum  AS zzwarndt, "placeholder
         1          AS zzdeltime, "placeholder
         0          AS zzwarnnum
    FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
              INNER JOIN eket ON eket~ebeln = ekko~ebeln
                             AND eket~ebelp = ekpo~ebelp
               LEFT JOIN ekbe ON ekbe~ebeln = ekko~ebeln
                             AND ekbe~ebelp = ekpo~ebelp
                             AND ekbe~vgabe = @gd_goods_receipt
              INNER JOIN eban ON eban~ebeln = ekko~ebeln
                             AND eban~ebelp = ekpo~ebelp
              INNER JOIN mara ON mara~matnr = ekpo~matnr
              INNER JOIN makt ON makt~matnr = mara~matnr
                             AND makt~spras = @sy-langu
              INNER JOIN marc ON marc~matnr = ekpo~matnr
                             AND marc~werks = ekpo~werks
              INNER JOIN mard ON mard~matnr = ekpo~matnr
                             AND mard~werks = ekpo~werks
                             AND mard~lgort = ekpo~lgort
              INNER JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
    INTO TABLE @DATA(lt_report)
   WHERE
* Start of Comment CH01
*         ekko~ebeln IN @s_ebeln
*     AND mara~matnr IN @s_matnr
*     AND ekko~bsart IN @s_bsart
*     AND ekpo~knttp IN @s_knttp
*     AND eban~banfn IN @s_banfn
*     AND ekko~lifnr IN @s_lifnr
*     AND eket~eindt IN @s_eindt
*     AND ekko~bedat IN @s_bedat
*     AND ekpo~elikz IN @s_elikz
*     AND ekpo~pstyp IN @s_pstyp
*     AND marc~werks IN @s_werks
*     AND marc~dismm IN @s_dismm
*     AND eban~loekz EQ @space
*     AND ekpo~loekz EQ @space
*     AND mara~lvorm EQ @space
*     AND ekpo~elikz EQ @space.
* End of Comment CH01

* Start of Insert CH01
    mara~matnr IN @s_matnr AND
    mara~mtart IN @s_mtart AND
    marc~dismm IN @s_dismm AND
    marc~werks IN @s_werks.
* End of Insert CH01



  IF sy-subrc EQ 0.

    "-------------------------------------------------------------------
    "Prepare data to calculate PO/PR specific fields
    LOOP AT lt_report REFERENCE INTO DATA(lr_report).
      MOVE-CORRESPONDING lr_report->* TO ls_po_pr_quant.

      "Calcolare data delivery di PO meno data di lancio report
      DATA(lv_num_days) = lr_report->eindt - sy-datum.
*      ADD lv_num_days TO lr_report->zzwarndt.
      ADD lv_num_days TO lr_report->zzwarnnum.

      lr_report->zzwarnnum = lr_report->zzwarnnum * -1. " Remaining days to delivery

      ">>> Color the date cell RED in case the date is less than today


      "Calcolare DATA CONSEGNA PIANIFICATA eket-eindt - DATA po BEDAT
      IF lr_report->gr_date IS NOT INITIAL.
        lv_num_days = lr_report->eindt - lr_report->gr_date. "  eket-eindt - ekbe-budat
        lr_report->zzdeltime = lv_num_days.
      ENDIF.

      COLLECT ls_po_pr_quant INTO lt_po_pr_quant.
    ENDLOOP.
    SORT lt_po_pr_quant.

    "-------------------------------------------------------------------
    "Prepare for EXPD data
    MOVE-CORRESPONDING lt_report TO lt_po_list.
    SORT lt_po_list.
    DELETE ADJACENT DUPLICATES FROM lt_po_list COMPARING ebeln.

    IF lt_po_list IS NOT INITIAL.
      "Select Expd data based on PO Num and Item
      SELECT expobj,
             ident,
             nmrid,
             event,
             xbaseline,
             xforecast,
             xactual,
             ebeln
        FROM txpdat INTO TABLE @DATA(gt_expd_data)
         FOR ALL ENTRIES IN @lt_po_list
       WHERE ebeln EQ @lt_po_list-ebeln
         AND event EQ @gc_expd_del_dt_event.
      IF sy-subrc EQ 0.
        SORT gt_expd_data BY ebeln.
      ENDIF.
    ENDIF.

    "Move the data into separate level tables
    LOOP AT lt_report REFERENCE INTO lr_report.
      CLEAR ls_exped_l1.

      "Level 1 table
      MOVE-CORRESPONDING lr_report->* TO ls_exped_l1.

*      READ TABLE gt_expd_data WITH KEY ebeln = lr_report->ebeln REFERENCE INTO DATA(lr_expd_data).
*      IF sy-subrc EQ 0.
*        ls_exped_l1-xbaseline = lr_expd_data->xbaseline.
*        ls_exped_l1-xforecast = lr_expd_data->xforecast.
*        ls_exped_l1-xactual   = lr_expd_data->xactual.
*      ENDIF.

      READ TABLE lt_po_pr_quant WITH KEY matnr = ls_exped_l1-matnr
                                INTO ls_po_pr_quant BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_exped_l1-zzpending_quant_po = ls_po_pr_quant-menge.
        ls_exped_l1-zzpending_quant_pr = ls_po_pr_quant-menge_pr.
      ENDIF.

      COLLECT ls_exped_l1 INTO gt_exped_l1.
    ENDLOOP.

    "Level 2 table for PR
    MOVE-CORRESPONDING lt_report TO gt_exped_l2_pr.
    DELETE ADJACENT DUPLICATES FROM gt_exped_l2_pr.
    "Level 2 table for PO
    DELETE ADJACENT DUPLICATES FROM gt_exped_l2_po.
    MOVE-CORRESPONDING lt_report TO gt_exped_l2_po.


    PERFORM display_alv_by_material.

  ELSE.
    MESSAGE text-m01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.


*--------------------------------------------------------------------*
FORM display_alv_by_material.

*--------------------------------------------------------------------*
  DATA: lo_salv_table      TYPE REF TO cl_salv_table,
        lr_alv_column_list TYPE REF TO cl_salv_column_list,
        lr_events          TYPE REF TO cl_salv_events_table.



  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_salv_table    " Basis Class Simple ALV Tables
        CHANGING
          t_table        = gt_exped_l1
      ).
    CATCH cx_salv_msg INTO DATA(lx_root).    "
      MESSAGE lx_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.



  lo_salv_table->get_functions( )->set_all( abap_true ).

  "Configure conlumns
  lo_salv_table->get_columns( )->set_key_fixation( abap_true ).
  lo_salv_table->get_columns( )->set_optimize( abap_true ).

  LOOP AT lo_salv_table->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).

    CASE lr_column->columnname.
      WHEN 'MATNR' OR 'LABST' OR 'ZZPENDING_QUANT_PR'OR 'ZZPENDING_QUANT_PO'.  "'MAKTX'.
        lr_alv_column_list ?= lr_column->r_column.
        lr_alv_column_list->set_key( ).
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  CREATE OBJECT go_handler_class.
  SET HANDLER go_handler_class->on_double_click FOR lo_salv_table->get_event( ).

  lo_salv_table->display( ).


ENDFORM.


*--------------------------------------------------------------------*
FORM get_data_by_pur_doc.

*--------------------------------------------------------------------*
  DATA: lo_salv_table      TYPE REF TO cl_salv_table,
        lr_alv_column_list TYPE REF TO cl_salv_column_list,
        lr_events          TYPE REF TO cl_salv_events_table,
        grt_columns        TYPE REF TO cl_salv_columns,
        lv_tabix           TYPE sy-tabix,
* Start of  Insert CH01
        lv_d1              TYPE sy-datum,
        lv_d2              TYPE sy-datum,
        lv_diffdate        TYPE i.
* End of  Insert CH01
*--------------------------------------------------------------------*
  SELECT ekko~bsart,
         ekko~bedat,
         ekko~ebeln,
         ekpo~ebelp,
         ekpo~werks,
         ekpo~knttp,
         ekpo~pstyp,
         eket~eindt,
         ekpo~netwr,
         ekpo~menge,
         ekpo~meins,
         eket~menge AS menge_sch,
         ekbe~menge AS menge_del,
         ekko~lifnr,
         lfa1~name1,
         ekpo~matnr,
         ekpo~txz01 AS maktx,
         ekpo~wepos,
         ekpo~elikz,
         ekbe~vgabe
         FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
              INNER JOIN eket ON eket~ebeln = ekko~ebeln
                             AND eket~ebelp = ekpo~ebelp
              INNER JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
              LEFT  JOIN ekbe ON ekbe~ebeln = ekko~ebeln
                             AND ekbe~ebelp = ekpo~ebelp
    INTO TABLE @gt_exped_flat
   WHERE ekko~ebeln IN @s_ebeln
     AND ekko~bsart IN @s_bsart
     AND ekpo~knttp IN @s_knttp
     AND ekko~lifnr IN @s_lifnr
     AND eket~eindt IN @s_eindt
     AND ekpo~werks IN @s_wrk_po
     AND ekko~bedat IN @s_bedat
     AND ekpo~elikz IN @s_elikz
     AND ekpo~pstyp IN @s_pstyp
     AND ekpo~matnr NE @space
     AND ekpo~loekz EQ @space
* Start of Insert CH01
     AND ekbe~vgabe IN @s_vgabe.
* End of Insert CH01
* Start of Comment CH01
*        AND ekbe~vgabe EQ @gc_goods_receipt.
*  End of Comment CH01

*     AND ekpo~elikz EQ @space.

  IF sy-subrc EQ 0.

* Start of  Insert CH01
    SELECT ebeln, ebelp FROM ekbe APPENDING TABLE @DATA(ls_ekbe)
      FOR ALL ENTRIES IN  @gt_exped_flat
              WHERE ebeln EQ @gt_exped_flat-ebeln AND
             ebelp EQ @gt_exped_flat-ebelp.

    LOOP AT gt_exped_flat INTO DATA(ls_exped_flat).
      lv_tabix  = sy-tabix.
      READ TABLE ls_ekbe WITH  KEY ebeln = ls_exped_flat-ebeln ebelp = ls_exped_flat-ebelp TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        ls_exped_flat-poh = text-t10.
*        IF ls_exped_flat-wepos = space AND ls_exped_flat-eindt < sy-datum.
*          ls_exped_flat-alert = text-t11.
*        ENDIF.
*      ENDIF.
*   Good recepit and  Check Delivery  flag
*      IF ls_exped_flat-wepos = 'X' AND ls_exped_flat-elikz = '' AND ls_exped_flat-eindt < sy-datum.
*        ls_exped_flat-alert = text-t11. " Icon Red
*      ELSEIF ls_exped_flat-wepos = '' AND ls_exped_flat-vgabe NE 1.
*        DELETE gt_exped_flat INDEX lv_tabix.
*      ELSEIF ls_exped_flat-wepos = 'X' AND ls_exped_flat-elikz = 'X'.
*        ls_exped_flat-alert = text-t12. " Icon Green
*      ELSEIF
*         ls_exped_flat-wepos = 'X' AND ls_exped_flat-elikz = ' ' AND ls_exped_flat-eindt > sy-datum.
*        ls_exped_flat-alert = text-t13. " Icon Yellow
*      ENDIF.
*      MODIFY gt_exped_flat FROM ls_exped_flat INDEX lv_tabix.
      IF sy-subrc = 0.
        ls_exped_flat-poh = text-t10.
      ENDIF.
* Alert Icon color
      IF ( ls_exped_flat-wepos = 'X' AND ls_exped_flat-elikz = '' ).
        lv_d1 = ls_exped_flat-eindt.
        lv_d2 = sy-datum.
        lv_diffdate = lv_d1 - lv_d2.

        IF ( lv_diffdate  BETWEEN 0 AND 15 ) OR  ( sy-datum GT ls_exped_flat-eindt ).
          ls_exped_flat-alert = text-t11.
        ELSEIF lv_diffdate BETWEEN 15 AND 60.
          ls_exped_flat-alert = text-t13.
        ELSEIF
           lv_diffdate GT 60.
          ls_exped_flat-alert = text-t12.
        ENDIF.
      ENDIF.
      MODIFY gt_exped_flat FROM ls_exped_flat INDEX lv_tabix.

    ENDLOOP.

* End of  Insert CH01

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_salv_table    " Basis Class Simple ALV Tables
          CHANGING
            t_table        = gt_exped_flat
        ).
      CATCH cx_salv_msg INTO DATA(lx_root).    "
        MESSAGE lx_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.


    lo_salv_table->get_functions( )->set_all( abap_true ).

    "Configure conlumns
    lo_salv_table->get_columns( )->set_key_fixation( abap_true ).
    lo_salv_table->get_columns( )->set_optimize( abap_true ).

    LOOP AT lo_salv_table->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).
      CASE lr_column->columnname.
        WHEN  'EBELN' OR  'MATNR' .        "'EBELP'. " 'BSART'
          lr_alv_column_list ?= lr_column->r_column.
          lr_alv_column_list->set_key( ).
        WHEN 'MENGE_DEL'.
          lr_column->r_column->set_short_text( text-c01 ).
          lr_column->r_column->set_medium_text( text-c02 ).
          lr_column->r_column->set_long_text( text-c02 ).
* Start of  Insert CH01
        WHEN 'POH'.
          lr_column->r_column->set_short_text( text-c03 ).
          lr_column->r_column->set_medium_text( text-c03 ).
          lr_column->r_column->set_long_text( text-c03 ).

        WHEN 'ALERT'.
          lr_column->r_column->set_short_text( text-c04 ).
          lr_column->r_column->set_medium_text( text-c04 ).
          lr_column->r_column->set_long_text( text-c04 ).
        WHEN 'WEPOS'.
          lr_column->r_column->set_visible( abap_false ).
        WHEN 'VGABE'.
          lr_column->r_column->set_visible( abap_false ).
* End of  Insert CH01
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    CREATE OBJECT go_handler_class.
    SET HANDLER go_handler_class->on_double_click FOR lo_salv_table->get_event( ).

* Start of  Insert CH01
    grt_columns = lo_salv_table->get_columns( ).
    grt_columns->set_optimize( abap_true ).
    grt_columns->set_column_position( columnname = 'POH'
                                     position   = 1 ).
    grt_columns->set_column_position( columnname = 'ALERT'
                                     position   = 2 ).
    grt_columns->set_column_position( columnname = 'ELIKZ'
                                    position   = 2 ).
* End of  Insert CH01

    lo_salv_table->display( ).

  ELSE.
    MESSAGE text-m01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
CLASS lcl_handler IMPLEMENTATION.

  METHOD on_double_click.
    INCLUDE: <icon>.
    DATA: lo_salv_l2         TYPE REF TO cl_salv_table,
          lr_alv_column_list TYPE REF TO cl_salv_column_list.

    DATA: lo_columns             TYPE REF TO cl_salv_columns.
    DATA: lo_column              TYPE REF TO cl_salv_column_table,
          lo_functional_settings TYPE REF TO cl_salv_functional_settings,
          lo_tooltips            TYPE REF TO cl_salv_tooltips,
          lv_value               TYPE lvc_value,
          lo_functions           TYPE REF TO cl_salv_functions_list.




    IF column EQ 'ZZPENDING_QUANT_PR' OR
       column EQ 'ZZPENDING_QUANT_PO'.

      TRY.
          IF column EQ 'ZZPENDING_QUANT_PR'.
            cl_salv_table=>factory(
              IMPORTING
                r_salv_table = lo_salv_l2
              CHANGING
                t_table      = gt_exped_l2_pr ).

          ELSE.
            cl_salv_table=>factory(
              IMPORTING
                r_salv_table = lo_salv_l2
              CHANGING
                t_table      = gt_exped_l2_po ).
          ENDIF.

          lo_salv_l2->get_functions( )->set_all( 'X' ).

          IF lo_salv_l2 IS BOUND.
*          lo_salv_l2->set_screen_popup(
*            start_column = 25
*            end_column  = 225
*            start_line  = 6
*            end_line    = 36 ).


            CREATE OBJECT go_handler_class.
            SET HANDLER go_handler_class->on_double_click FOR lo_salv_l2->get_event( ).


            READ TABLE gt_exped_l1 INDEX row REFERENCE INTO DATA(lr_report).
            IF row GT 0.
              DATA(lv_low_value) = CONV salv_de_selopt_low( lr_report->matnr ).
              lo_salv_l2->get_filters( )->add_filter(
                EXPORTING
                  columnname         = 'MATNR'
                  sign               = 'I'
                  option             = 'EQ'
                  low                = lv_low_value
              ).


              DATA: lt_s_color TYPE lvc_t_scol,
                    ls_s_color TYPE lvc_s_scol.

              LOOP AT lo_salv_l2->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).
                CASE lr_column->columnname.
                  WHEN  'EBELN' OR  'MATNR' OR 'BANFN' .        "'EBELP'. " 'BSART'
                    lr_alv_column_list ?= lr_column->r_column.
                    lr_alv_column_list->set_key( ).
                ENDCASE.
              ENDLOOP.


              LOOP AT lo_salv_l2->get_columns( )->get( ) REFERENCE INTO DATA(lr_column1).
                CASE lr_column1->columnname.

                  WHEN 'PLIFZ'.

                    lr_column1->r_column->set_long_text( text-t03 ).
                    lr_column1->r_column->set_medium_text( text-t04 ).
                    lr_column1->r_column->set_short_text( text-t05 ).


*                  WHEN 'ZZDELTIME'.
*                    lr_column1->r_column->set_long_text( 'Lead time GR' ).
*                    lr_column1->r_column->set_medium_text( 'Lead time GR' ).
*                    lr_column1->r_column->set_short_text( 'LT GR' ).
**                  WHEN  'ZICON'.
**                    lr_column1->r_column->set_icon( if_salv_c_bool_sap=>true ).

                  WHEN 'ZZWARNNUM'.
                    lr_column1->r_column->set_long_text( text-t06 ).
                    lr_column1->r_column->set_medium_text( text-t07 ).
                    lr_column1->r_column->set_short_text( text-t08 ).

                  WHEN 'ZZDELTIME' OR 'ZZWARNDT'.

                    lr_column1->r_column->set_visible( abap_false ).   "Hide column

                ENDCASE.
              ENDLOOP.

              LOOP AT gt_exped_l2_po INTO DATA(lt_gt_exped_l2_po).


                IF lt_gt_exped_l2_po-eindt LE sy-datum.

                  lt_gt_exped_l2_po-zicon = icon_red_light.

                ELSE.
                  lt_gt_exped_l2_po-zicon = icon_green_light.

                ENDIF.
                MODIFY  gt_exped_l2_po   FROM lt_gt_exped_l2_po.
              ENDLOOP.




              lo_functions = lo_salv_l2->get_functions( ).
              lo_functions->set_all( abap_true ).
              lo_columns = lo_salv_l2->get_columns( ).
              "lo_columns->set_optimize( abap_true ).
              TRY.
                  lo_column ?= lo_columns->get_column( 'ZICON' ).
                  lo_column->set_icon( if_salv_c_bool_sap=>true ).
                  lo_column->set_long_text( text-t09 ).
                  lo_column->set_alignment( if_salv_c_alignment=>centered ).
                  lo_column->set_output_length( 5 ).
                CATCH cx_salv_not_found.                "#EC NO_HANDLER
              ENDTRY.


              lo_functional_settings = lo_salv_l2->get_functional_settings( ).
              lo_tooltips = lo_functional_settings->get_tooltips( ).

              TRY.
                  lv_value = icon_green_light.
                  lo_tooltips->add_tooltip(
                    type    = cl_salv_tooltip=>c_type_icon
                    value   = lv_value
                    tooltip = 'Delivery is in time' ).      "#EC NOTEXT
                CATCH cx_salv_existing.                 "#EC NO_HANDLER
              ENDTRY.



              lo_salv_l2->display( ).
            ENDIF.
          ENDIF.
        CATCH cx_salv_msg.
        CATCH cx_salv_existing.
        CATCH cx_salv_not_found.
        CATCH cx_salv_data_error.
      ENDTRY.
    ENDIF.

*--------------------------------------------------------------------*
    IF column EQ 'MATNR'.
      IF rb_rep1 EQ 'X' .
        READ TABLE gt_exped_l1 INDEX row REFERENCE INTO lr_report.
        SET PARAMETER ID 'MAT' FIELD lr_report->matnr.
        SET PARAMETER ID 'WRK' FIELD lr_report->werks.
        SET PARAMETER ID 'MXX' FIELD 'D'. "FIELD 'B'.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*      SUBMIT rexpdsel WITH profile = '000001' WITH matnr = lr_report->matnr AND RETURN.
      ELSE.
        READ TABLE  gt_exped_flat INDEX row REFERENCE INTO DATA(lt_exped_flat).
        SET PARAMETER ID 'MAT' FIELD lt_exped_flat->matnr.
        SET PARAMETER ID 'WRK' FIELD lt_exped_flat->werks.
        SET PARAMETER ID 'MXX' FIELD 'D'. "FIELD 'B'.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

    IF column EQ 'EBELN'.
      IF rb_rep2 EQ 'X' .
        READ TABLE gt_exped_flat INDEX row REFERENCE INTO DATA(lr_exped_flat).
        IF sy-subrc EQ 0.
          SET PARAMETER ID 'BES' FIELD lr_exped_flat->ebeln.
          SET PARAMETER ID 'BSP' FIELD lr_exped_flat->ebelp.
          CALL TRANSACTION 'ME23N'.
        ENDIF.
      ELSE.
        READ TABLE  gt_exped_l2_po INDEX row REFERENCE INTO DATA(lt_exped_flat_eban).
        IF sy-subrc EQ 0.
          SET PARAMETER ID 'BES' FIELD lt_exped_flat_eban->ebeln.
          SET PARAMETER ID 'BSP' FIELD lt_exped_flat_eban->ebelp.
          CALL TRANSACTION 'ME23N'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF column EQ 'LABST'.
      READ TABLE gt_exped_l1 INDEX row REFERENCE INTO lr_report.
      SET PARAMETER ID 'MAT' FIELD lr_report->matnr.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

    ENDIF.


    IF column EQ 'BANFN'.
      READ TABLE gt_exped_l2_pr INDEX row REFERENCE INTO DATA(lr_report1).

      SET PARAMETER ID 'BAN' FIELD lr_report1->banfn.
      CALL TRANSACTION 'ME53N' .
    ENDIF.


* Start of  Insert CH01
    IF column EQ 'POH'.
      READ TABLE gt_exped_flat INDEX row REFERENCE INTO DATA(ls_exped_flat).
      IF sy-subrc EQ 0.
        IF ls_exped_flat->poh IS NOT INITIAL.
          SET PARAMETER ID 'BES' FIELD ls_exped_flat->ebeln.
          SET PARAMETER ID 'BSP' FIELD ls_exped_flat->ebelp.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
*  End of Insert : CH01
*    INDEX row.
  ENDMETHOD.


ENDCLASS.
