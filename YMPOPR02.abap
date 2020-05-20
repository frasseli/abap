*---------------------------------------------------------------------*
*                                                                     *
*  Author :  Flavio Rasseli                                           *
*  Date   :  16.09.2019                                               *
*  Description: SR20541 - OUTPUT/TABELLARE YMPOPR01                   *
*  Purpose: Requisition vs PO value report.                           *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*  Modification Log:                                                  *
*                                                                     *
*   01) Change number : CH01                                          *
*       Author        : Pennisi Davide                                *
*       Date          : 12.03.2020                                    *
*       Reference     : SR34097 Problemi YMPOPR02                                      *
*       Description   : Correct fields: Delta value and old_value     *
*                                                                     *
*---------------------------------------------------------------------*

REPORT ympopr02.

*--------------------------------------------------------------------*
* Global data
CLASS lcl_events_handler DEFINITION DEFERRED.
TABLES: ekko, ekpo, eban.

DATA: gr_events TYPE REF TO lcl_events_handler,
      gr_report TYPE REF TO data.

CONSTANTS: gc_pr_objectclas TYPE cdhdr-objectclas VALUE 'BANF'.

*--------------------------------------------------------------------*
* Local class definition for event handling
CLASS lcl_events_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.

*--------------------------------------------------------------------*
* Selection criterea by Purchase order
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bukrs FOR ekko-bukrs MEMORY ID buk,
                s_ekorg FOR ekko-ekorg OBLIGATORY MEMORY ID eko,
                s_ekgrp FOR ekko-ekgrp.

SELECT-OPTIONS: s_werks  FOR ekpo-werks,
                s_ebeln  FOR ekko-ebeln,
                s_matkl  FOR ekpo-matkl,
                s_pstyp  FOR ekpo-pstyp,
                s_ernam  FOR ekko-ernam,
                s_bedat  FOR ekko-bedat OBLIGATORY DEFAULT sy-datum,
                s_bsart  FOR ekko-bsart.

SELECTION-SCREEN END OF BLOCK a1.

* Selection criterea for Requisitions.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.

SELECT-OPTIONS: sr_ernam FOR eban-ernam,
                sr_erdat FOR eban-erdat.

SELECTION-SCREEN END OF BLOCK b1.

* Allowed varience
SELECTION-SCREEN BEGIN OF BLOCK d1 WITH FRAME TITLE text-004.
SELECT-OPTIONS s_dmbtr FOR ekpo-netwr DEFAULT '50' OPTION GE NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS s_perct FOR ekpo-menge DEFAULT '10' "Percentage
                                      OPTION GE NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK d1.


*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.


*--------------------------------------------------------------------*
FORM get_data.

  TYPES: BEGIN OF ty_ch_doc_obj_id,
           banfn TYPE cdpos-objectid,
         END OF ty_ch_doc_obj_id.

  DATA: lt_ch_doc_obj_id TYPE TABLE OF ty_ch_doc_obj_id.

  "Select data for the report
  SELECT eban~banfn,  "Requisition
         eban~bnfpo,  "Item
         eban~ernam,  "Creator
         eban~statu,  "Status
         eban~menge,  "Quantity
         eban~preis,
         eban~rlwrt AS eban_netwr,
         eban~waers,  "Currency
         eban~ebeln,  "Order
         eban~ebelp,  "Item
         ekko~ernam AS ernam_po,  "Creator (User ID)
         ekpo~statu AS statu_po,  "Status
         ekpo~netpr,  "PO net price
         ekpo~netwr,  "PO Value. Doc Cur
         ekko~waers AS ekpo_wa,  "PO Currency
         ekpo~netwr AS delta, "Delta Loc Cur
         eban~rlwrt AS delta_perc,  "Delta %
*         eban~peinh, "Value Unit
         ekko~lifnr,  "Vendor Id
         lfa1~name1,  "Vendor Name
         eban~frgdt,  "Data PR (data del primo rilascio)
         ekpo~netwr AS old_pr_value,  "Old Value Price
         ekko~bedat   "Data PO (data creazione del PO)
    FROM ekko INNER JOIN ekpo  ON ekpo~ebeln = ekko~ebeln
              INNER JOIN eban  ON eban~ebeln = ekpo~ebeln AND eban~ebelp = ekpo~ebelp
              INNER JOIN lfa1  ON lfa1~lifnr = ekko~lifnr
    INTO TABLE @DATA(gt_report)
   WHERE ekko~ebeln IN @s_ebeln
     AND ekko~bedat IN @s_bedat
     AND ekko~ekorg IN @s_ekorg
     AND ekko~ekgrp IN @s_ekgrp
     AND ekko~bsart IN @s_bsart
     AND ekko~ernam IN @s_ernam
     AND ekpo~werks IN @s_werks
     AND ekpo~matkl IN @s_matkl
     AND ekpo~pstyp IN @s_pstyp
     AND ekko~bukrs IN @s_bukrs
     AND eban~ernam IN @s_ernam
     AND eban~erdat IN @sr_erdat
   ORDER BY eban~banfn, eban~bnfpo.

*--------------------------------------------------------------------*
  IF sy-subrc EQ 0.

    "Get chages to value in PR
    MOVE-CORRESPONDING gt_report TO lt_ch_doc_obj_id.
    SORT lt_ch_doc_obj_id.
    DELETE ADJACENT DUPLICATES FROM lt_ch_doc_obj_id.
    IF lt_ch_doc_obj_id IS NOT INITIAL.
      SELECT objectclas,
             objectid,
             changenr,
             tabname,
             tabkey,
             fname,
             chngind,
             value_new,
             value_old
        INTO TABLE @DATA(lt_cdpos)
        FROM cdpos
         FOR ALL ENTRIES IN @lt_ch_doc_obj_id
       WHERE objectclas = @gc_pr_objectclas
         AND objectid   = @lt_ch_doc_obj_id-banfn
         AND fname      = 'RLWRT'
       ORDER BY PRIMARY KEY.

* Start of Comment CH01
*      IF sy-subrc EQ 0.
*        SELECT objectclas,
*               objectid,
*               changenr,
*               udate
*          INTO TABLE @DATA(lt_cdhdr)
*          FROM cdhdr
*           FOR ALL ENTRIES IN @lt_cdpos
*         WHERE objectclas = @lt_cdpos-objectclas
*           AND objectid   = @lt_cdpos-objectid
*           AND changenr   = @lt_cdpos-changenr
*         ORDER BY PRIMARY KEY.

* Start of  Insert CH01
      IF sy-subrc NE 0.
* End of  Insert CH01
        CLEAR:
*  Start of Comment CH01
*          lt_cdhdr,
*  End of Comment CH01
          lt_cdpos.
*      ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*


  LOOP AT gt_report REFERENCE INTO DATA(lr_report).

    CLEAR lr_report->old_pr_value. "Clears the arbitrary selected value

    lr_report->eban_netwr = lr_report->menge * lr_report->preis.
    lr_report->delta = lr_report->netwr - lr_report->eban_netwr.
    IF lr_report->delta NE 0 AND lr_report->netwr NE 0 AND lr_report->eban_netwr NE 0.
      lr_report->delta_perc = round( val = 100 / lr_report->eban_netwr * lr_report->netwr dec = 2 ).
      lr_report->delta_perc = lr_report->delta_perc - 100. "Adjust percentage
    ENDIF.

*  Start of Insert  CH01

    IF lr_report->delta EQ 0.
      lr_report->delta_perc = 0.
    ENDIF.
*  End of Insert : CH01


* Start of Comment CH01

*READ TABLE lt_cdhdr WITH KEY objectclas = gc_pr_objectclas
*                                 objectid   = lr_report->banfn
*                                 TRANSPORTING NO FIELDS BINARY SEARCH.
*
*    IF sy-subrc EQ 0.
*      DATA(ld_loop_from) = sy-tabix.
*      LOOP AT lt_cdhdr FROM ld_loop_from REFERENCE INTO DATA(lr_cdhdr).
*        IF lr_cdhdr->objectid NE lr_report->banfn.
*          EXIT.
*        ENDIF.
*        IF lr_cdhdr->udate GT lr_report->frgdt.
*  End of Comment CH01



    "Look for changes of PR price after release
    READ TABLE lt_cdpos WITH KEY objectclas = gc_pr_objectclas
                                 objectid   = lr_report->banfn
*                              Start of Comment CH01
*                                changenr   = lr_cdhdr->changenr
*                              End of Comment CH01
*                              Start of Insert  CH01
                                 tabkey     = |{ sy-mandt }{ lr_report->banfn }{ lr_report->bnfpo }|
*                              End of Insert : CH01
                                 REFERENCE INTO DATA(lr_cdpos).


    IF sy-subrc EQ 0.
* Start of Insert  CH01
      lr_report->old_pr_value = lr_cdpos->value_new.
* End of Insert : CH01
    ENDIF.
  ENDLOOP.

  IF s_dmbtr IS NOT INITIAL.
    DELETE gt_report WHERE delta NOT IN s_dmbtr.
  ENDIF.

  IF s_perct IS NOT INITIAL.
    DELETE gt_report WHERE delta_perc NOT IN s_perct.
  ENDIF.

*  ASSIGN gt_report TO <lf_report>.
  CREATE DATA gr_report LIKE gt_report.
  GET REFERENCE OF gt_report INTO gr_report.

  PERFORM display_data USING gt_report.

ENDFORM.


*--------------------------------------------------------------------*
FORM display_data USING pt_report_data TYPE ANY TABLE.

  DATA: lo_slav_table      TYPE REF TO cl_salv_table,
        lr_alv_column_list TYPE REF TO cl_salv_column_list,
        lr_events          TYPE REF TO cl_salv_events_table.


  DATA: lr_header TYPE REF TO cl_salv_form_header_info,
        ld_text   TYPE string,
        ld_date   TYPE c LENGTH 10,
        ld_time   TYPE c LENGTH 10.

  DEFINE change_col_desc.
    lr_column->r_column->set_short_text( &1 ).
    lr_column->r_column->set_medium_text( &2 ).
*      Start of Insert  CH01
    lr_column->r_column->set_long_text( &2 ).
*      End of Insert : CH01
    lr_column->r_column->set_fixed_header_text( abap_false ).
  END-OF-DEFINITION.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_slav_table    " Basis Class Simple ALV Tables
        CHANGING
          t_table        = pt_report_data
      ).
    CATCH cx_salv_msg INTO DATA(lx_root).    "
      MESSAGE lx_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.


  lo_slav_table->get_functions( )->set_all( abap_true ).
  lo_slav_table->get_columns( )->set_optimize( abap_true ).

  lo_slav_table->get_columns( )->set_key_fixation( abap_true ).

  LOOP AT lo_slav_table->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).

    CASE lr_column->columnname.
      WHEN 'EBELN' OR 'EBELP' OR 'BANFN' OR 'BNFPO'.
        lr_alv_column_list ?= lr_column->r_column.
        lr_alv_column_list->set_key( ).
      WHEN 'EBAN_NETWR'.
        change_col_desc 'Net.Vl.PR'(h01) 'Net value PR'(h02).
      WHEN 'DELTA'.
        change_col_desc 'Amt.Delta'(h03) 'Amount Delta POxPR'(h04).
      WHEN 'DELTA_PERC'.
        change_col_desc '% Delta'(h05) '% Delta POxPR'(h06).
      WHEN 'FRGDT'.
        change_col_desc 'Rel.Dt. PR'(h07) 'Release Date PR'(h08).
        lr_column->r_column->set_output_length( 45 ).
      WHEN 'BEDAT'.
        change_col_desc 'Doc.Dt. PO'(h09) 'Document Date PO'(h10).
      WHEN 'OLD_PR_VALUE'.
        change_col_desc 'Old Val.PR'(h11) 'Old Val.Price PR'(h12).
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  "Add Top of List
  WRITE sy-datlo TO ld_date DD/MM/YYYY.
  WRITE sy-timlo TO ld_time ENVIRONMENT TIME FORMAT.

  ld_text = |Report created on: { ld_date } at { ld_time } |.

  CREATE OBJECT lr_header
    EXPORTING
      text    = ld_text
      tooltip = ld_text.

  "Add events
  CREATE OBJECT gr_events.
  lr_events = lo_slav_table->get_event( ).
  SET HANDLER gr_events->on_double_click FOR lr_events.

  lo_slav_table->set_top_of_list( lr_header ).

  lo_slav_table->display( ).

ENDFORM.


*--------------------------------------------------------------------*
CLASS lcl_events_handler IMPLEMENTATION.

  METHOD on_double_click.

    FIELD-SYMBOLS: <lf_table> TYPE ANY TABLE.

    CASE column.
      WHEN 'EBELN'.
        ASSIGN gr_report->* TO <lf_table>.
        LOOP AT <lf_table> ASSIGNING FIELD-SYMBOL(<lf_record>).
          IF sy-tabix EQ row.
            ASSIGN COMPONENT 'EBELN' OF STRUCTURE <lf_record> TO FIELD-SYMBOL(<lf_field>).
            IF sy-subrc EQ 0.
              SET PARAMETER ID 'BES' FIELD <lf_field>.
              CALL TRANSACTION 'ME23N'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN 'BANFN'.
        ASSIGN gr_report->* TO <lf_table>.
        LOOP AT <lf_table> ASSIGNING <lf_record>.
          IF sy-tabix EQ row.
            ASSIGN COMPONENT 'BANFN' OF STRUCTURE <lf_record> TO <lf_field>.
            IF sy-subrc EQ 0.
              SET PARAMETER ID 'BAN' FIELD <lf_field>.
              CALL TRANSACTION 'ME53N'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
