*---------------------------------------------------------------------*
*  Author :  Flavio Rasseli
*  Date   :  03.02.2020
*  Ticket :  SR26490
*  Description: Custom Materials List report
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*                                                                      *
*   01) Change number : CH01                                           *
*       Author        : Pennisi Davide                                 *
*       Date          : 12/05/2020                                     *
*       Reference     : SR36531                                        *
*       Description   : Add functional location and BOM                *
*----------------------------------------------------------------------*

REPORT zmaterial_list.

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
* Global data
TABLES: t134, t023, mara, mard.

DATA: gt_report  TYPE TABLE OF zmms_material_list,
      go_handler TYPE REF TO lcl_handler.

*START insert CH01

TYPES:  BEGIN OF ty_equi_fl,
          matnr TYPE mara-matnr,
          equnr TYPE eqst-equnr,
          tplnr TYPE tpst-tplnr,
          stlty TYPE stpo-stlty,
        END OF   ty_equi_fl.
DATA gt_equi_fl TYPE TABLE OF  ty_equi_fl.

TYPES:  BEGIN OF ty_stpo,
          stlty TYPE stpo-stlty,
          stlnr TYPE stpo-stlnr,
          idnrk TYPE stpo-idnrk,
        END OF ty_stpo.

DATA gt_stpo TYPE TABLE OF ty_stpo.

TYPES:  BEGIN OF ty_eqst,
          stlnr TYPE eqst-stlnr,
          equnr TYPE eqst-equnr,
        END OF ty_eqst.

DATA gt_eqst TYPE TABLE OF ty_eqst.

TYPES:  BEGIN OF ty_tpst,
          stlnr TYPE tpst-stlnr,
          tplnr TYPE tpst-tplnr,
        END OF ty_tpst.

DATA gt_tpst TYPE TABLE OF ty_tpst.

TYPES:  BEGIN OF ty_equi,
          equnr TYPE eqst-equnr,
        END OF   ty_equi.

TYPES:  BEGIN OF ty_tplnr,
          tplnr TYPE tpst-tplnr,
        END OF ty_tplnr.
*END insert CH01


*--------------------------------------------------------------------*
* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_matnr FOR mara-matnr,
  s_werks FOR mard-werks.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
    s_mtart FOR t134-mtart,
    s_matkl FOR t023-matkl,
    s_ernam FOR mara-ernam.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF s_matnr IS INITIAL AND
     s_werks IS INITIAL AND
     s_mtart IS INITIAL AND
     s_matkl IS INITIAL AND
     s_ernam IS INITIAL.
    MESSAGE text-m01 TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM get_material_data.
    PERFORM display_alv.
  ENDIF.

*--------------------------------------------------------------------*
FORM get_material_data.

  "Local data
  DATA: ls_output TYPE zmms_material_list,
        lt_lines  TYPE TABLE OF tline,
        lv_tdname TYPE thead-tdname.


  "Read Material Master
  SELECT mara~matnr,
         ernam,
         laeda,
         mtart,
         matkl,
         meins,
         begru,
         maktx,
         spart,
         mfrpn
    FROM mara INNER JOIN makt ON makt~matnr EQ mara~matnr
                             AND makt~spras EQ @sy-langu
    INTO TABLE @DATA(lt_mara)
   WHERE mara~matnr IN @s_matnr
     AND mtart IN @s_mtart
     AND matkl IN @s_matkl
     AND ernam IN @s_ernam.

  IF sy-subrc EQ 0.

    SORT lt_mara BY matnr.

    "Select Material/Plant data
    SELECT matnr,
           werks,
           ekgrp,
           maabc,
           dismm,
           minbe,
           mmsta,
           mabst,
           plifz
      FROM marc
      INTO TABLE @DATA(lt_marc)
       FOR ALL ENTRIES IN @lt_mara
     WHERE matnr EQ @lt_mara-matnr
       AND werks IN @s_werks
     ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
      CLEAR lt_marc.
    ENDIF.

    "Select storage location data
    SELECT matnr,
           werks,
           lgort,
           labst
      FROM mard
      INTO TABLE @DATA(lt_mard)
       FOR ALL ENTRIES IN @lt_mara
     WHERE matnr EQ @lt_mara-matnr
       AND werks IN @s_werks
     ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
      CLEAR lt_mard.
    ENDIF.

    "Select stock and valuation data
    SELECT w~matnr,
           p~werks,
           w~bwtar,
           w~bklas,
           w~vprsv,
           w~stprs,
           w~verpr,
           w~stprs AS preis,
           w~peinh,
           w~pstat,
           t~waers
      FROM mbew AS w INNER JOIN t001w AS p  ON p~bwkey = w~bwkey
                     INNER JOIN t001k AS tk ON tk~bwkey = p~bwkey
                     INNER JOIN t001  AS t  ON t~bukrs = tk~bukrs
      INTO TABLE @DATA(lt_mbew)
       FOR ALL ENTRIES IN @lt_mara
     WHERE w~matnr EQ @lt_mara-matnr
       AND p~werks IN @s_werks.
    IF sy-subrc EQ 0.
      SORT lt_mbew BY matnr werks.
    ENDIF.

*start insert CH01
    SELECT stlty, stlnr, idnrk FROM stpo
      INTO TABLE @gt_stpo
      FOR ALL ENTRIES IN @lt_mara
           WHERE idnrk  EQ @lt_mara-matnr.

    SELECT stlnr,equnr FROM eqst
      INTO TABLE @gt_eqst
       FOR ALL ENTRIES IN @gt_stpo
           WHERE stlnr EQ @gt_stpo-stlnr.

    SELECT stlnr, tplnr FROM tpst
      INTO TABLE @gt_tpst
   FOR ALL ENTRIES IN @gt_stpo
       WHERE stlnr EQ @gt_stpo-stlnr.
*End insert CH01

  ENDIF.

  "Build output table
  LOOP AT lt_mara REFERENCE INTO DATA(lr_mara).
    CLEAR ls_output.
    MOVE-CORRESPONDING lr_mara->* TO ls_output.

    CLEAR lt_lines.
    lv_tdname = ls_output-matnr.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'BEST'
        language                = sy-langu
        name                    = lv_tdname
        object                  = 'MATERIAL'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 0.
      PERFORM tline_to_string USING lt_lines ls_output-long_text.
      IF strlen( ls_output-long_text ) > 255.
        ls_output-long_text = | { ls_output-long_text(250) }...|.
      ENDIF.
    ELSE.
      CLEAR ls_output-long_text.
    ENDIF.

    LOOP AT lt_marc REFERENCE INTO DATA(lr_marc) WHERE matnr = lr_mara->matnr.
      MOVE-CORRESPONDING lr_marc->* TO ls_output.

      READ TABLE lt_mbew WITH KEY matnr = lr_marc->matnr
                                  werks = lr_marc->werks REFERENCE INTO DATA(lr_mbew) BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lr_mbew->* TO ls_output.
      ENDIF.

      LOOP AT lt_mard REFERENCE INTO DATA(lr_mard) WHERE matnr = lr_mara->matnr
                                                     AND werks = lr_marc->werks.
        ls_output-lgort = lr_mard->lgort.
        ls_output-labst = lr_mard->labst.

        PERFORM retrive_func_loc_equip USING lr_mard->matnr ls_output .

        APPEND ls_output TO gt_report.

      ENDLOOP.
      IF sy-subrc NE 0. "Not found Storage Location info for material
* Start insert CH01
        PERFORM retrive_func_loc_equip USING lr_mara->matnr ls_output .
* End insert CH01
        APPEND ls_output TO gt_report.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0. "Not found Plant info for material

      IF ls_output-werks NOT IN s_werks.
        CONTINUE.
      ENDIF.
* Start insert CH01
      PERFORM retrive_func_loc_equip USING lr_mara->matnr ls_output .
* End insert CH01
      APPEND ls_output TO gt_report.
    ENDIF.

  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------------*
FORM display_alv.

  DATA: lo_salv_table TYPE REF TO cl_salv_table.
  TRY .

      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_salv_table
        CHANGING
          t_table        = gt_report
      ).


*start insert CH01

      LOOP AT lo_salv_table->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).
        CASE lr_column->columnname.

          WHEN 'ICON_EQ'.
            lr_column->r_column->set_long_text( text-i01 ).
          WHEN 'ICON_FL'.
            lr_column->r_column->set_long_text( text-i04 ).

          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
*end insert CH01

      lo_salv_table->get_functions( )->set_all( abap_true ).
      lo_salv_table->get_columns( )->set_optimize( abap_true ).



      CREATE OBJECT go_handler.
      SET HANDLER go_handler->on_double_click FOR lo_salv_table->get_event( ).


      lo_salv_table->display( ).

    CATCH cx_salv_msg.    "

  ENDTRY.

ENDFORM.


*--------------------------------------------------------------------*
CLASS lcl_handler IMPLEMENTATION.

  METHOD on_double_click.
*Start insert CH01
    DATA: lt_out_equipment TYPE TABLE OF  ty_equi,
          ls_out_equipment TYPE ty_equi,
          lt_out_slocation TYPE TABLE OF ty_tplnr,
          ls_out_slocation TYPE ty_tplnr.

*END insert CH01

    IF column EQ 'MATNR'.

      READ TABLE gt_report INDEX row REFERENCE INTO DATA(lr_report).
      SET PARAMETER ID 'MAT' FIELD lr_report->matnr.
      SET PARAMETER ID 'WRK' FIELD lr_report->werks.
      IF lr_report->werks IS INITIAL.
        SET PARAMETER ID 'MXX' FIELD 'B'.
      ELSE.
        SET PARAMETER ID 'MXX' FIELD 'D'.
      ENDIF.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    ENDIF.


*START insert CH01
    IF column EQ 'EQUNR'.

      READ TABLE gt_report INDEX row REFERENCE INTO DATA(lr_equip).
      IF sy-subrc = 0.
        IF lr_equip->icon_eq NE space.
          LOOP AT  gt_equi_fl INTO DATA(ls_qui_eq) WHERE matnr =  lr_equip->matnr AND stlty = 'E'.
            ls_out_equipment-equnr = ls_qui_eq-equnr.
            APPEND ls_out_equipment TO lt_out_equipment.
          ENDLOOP.

          PERFORM out_equipment_slocation USING lt_out_equipment.
        ENDIF.
      ENDIF.
    ENDIF.

    IF column EQ 'TPLNR'.

      READ TABLE gt_report INDEX row REFERENCE INTO DATA(lr_tplnr).
      IF sy-subrc = 0.
        IF lr_tplnr->icon_fl NE space.
          LOOP AT  gt_equi_fl INTO DATA(ls_qui_fl) WHERE matnr = lr_tplnr->matnr AND stlty = 'T'.
            ls_out_slocation-tplnr = ls_qui_fl-tplnr.
            APPEND ls_out_slocation  TO lt_out_slocation .
          ENDLOOP.
          PERFORM out_equipment_slocation USING lt_out_slocation .
        ENDIF.
      ENDIF.
    ENDIF.
*End insert CH01
  ENDMETHOD.

ENDCLASS.


FORM tline_to_string USING pt_tline  TYPE tlinet
                           pt_string TYPE string.

  LOOP AT pt_tline REFERENCE INTO DATA(lr_tline).
    CONCATENATE pt_string lr_tline->tdline INTO pt_string.
  ENDLOOP.

ENDFORM.

*Start insert CH01
FORM retrive_func_loc_equip USING lr_mard-matnr ls_output TYPE zmms_material_list .


  DATA: ls_equi_fl TYPE ty_equi_fl.
  DATA: lv_tpst_count TYPE i,lv_eqst_count TYPE i.


  LOOP AT gt_stpo REFERENCE INTO DATA(lr_stpo) WHERE idnrk = lr_mard-matnr.
    IF sy-subrc EQ 0.
      IF lr_stpo->stlty = 'E'.
        READ TABLE gt_eqst REFERENCE INTO DATA(lr_eqst) WITH KEY  stlnr = lr_stpo->stlnr .
        IF sy-subrc = 0.
          lv_eqst_count = lv_eqst_count + 1.
          ls_equi_fl-matnr = lr_mard-matnr .
          ls_equi_fl-equnr = lr_eqst->equnr.
          ls_equi_fl-stlty = 'E'.
          APPEND ls_equi_fl TO gt_equi_fl.
        ENDIF.

      ELSEIF lr_stpo->stlty = 'T'.
        READ TABLE gt_tpst REFERENCE INTO DATA(lr_tpst) WITH KEY stlnr = lr_stpo->stlnr .
        IF sy-subrc = 0.
          lv_tpst_count = lv_tpst_count + 1.
          ls_equi_fl-matnr = lr_mard-matnr .
          ls_equi_fl-tplnr = lr_tpst->tplnr.
          ls_equi_fl-stlty = 'T'.
          APPEND ls_equi_fl TO gt_equi_fl.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_eqst_count > 1.
    ls_output-icon_eq = text-i03.
    ls_output-equnr = lr_eqst->equnr.
  ELSEIF lv_eqst_count = 1.
    ls_output-equnr = lr_eqst->equnr.
  ENDIF.

  IF lv_tpst_count > 1.
    ls_output-icon_fl = text-i03.
    ls_output-tplnr = lr_tpst->tplnr.
  ELSEIF lv_tpst_count = 1.
    ls_output-tplnr = lr_tpst->tplnr.
  ENDIF.

  CLEAR: lv_tpst_count,lv_tpst_count.

ENDFORM.

FORM out_equipment_slocation USING  lt_out_popup.

  DATA: lo_salv_table TYPE REF TO cl_salv_table.
  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_salv_table
        CHANGING
          t_table        = lt_out_popup
      ).
    CATCH cx_salv_msg.
  ENDTRY.


  lr_functions = lo_salv_table->get_functions( ).
  lr_functions->set_all( 'X' ).

  IF lo_salv_table IS BOUND.
    lo_salv_table->set_screen_popup(
    start_column =  60         "i_start_column
    end_column  =   100       " i_end_column
    start_line  =   10             "i_start_line
    end_line    =   20 ).          "i_end_line ).
    lo_salv_table->display( ).
  ENDIF.

ENDFORM.


*End insert CH01
