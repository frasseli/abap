class ZCL_SALES_KPI_HELPER definition
  public
  final
  create public .

public section.

  class-methods GET_OPEN_ORDERS
    importing
      !IV_VBCOM type VBCOM
      !IV_OPEN type ABAP_BOOL
    exporting
      !ET_OPEN_ORDERS type ZCL_ZSD_KPI_MPC=>TT_OPENSALESORDERS .
  class-methods GET_INVOICES
    importing
      !IV_COMPANY_CODE type VBRK-BUKRS
      !IV_START_DATE type VBRK-FKDAT
      !IV_END_DATE type VBRK-FKDAT
    exporting
      !ET_INVOICES type ZCL_ZSD_KPI_MPC=>TT_INVOICES .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SALES_KPI_HELPER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SALES_KPI_HELPER=>GET_INVOICES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMPANY_CODE                TYPE        VBRK-BUKRS
* | [--->] IV_START_DATE                  TYPE        VBRK-FKDAT
* | [--->] IV_END_DATE                    TYPE        VBRK-FKDAT
* | [<---] ET_INVOICES                    TYPE        ZCL_ZSD_KPI_MPC=>TT_INVOICES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_invoices.

    DATA gr_negative_inv_types TYPE RANGE OF vbrk-fkart.

    gr_negative_inv_types = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZS1' )
                                                              ( low = 'ZS2' )
                                                              ( low = 'ZS5' )
                                                              ( low = 'ZS6' )
                                                              ( low = 'ZCR1' )
                                                              ( low = 'ZCR2' )
                                                              ( low = 'ZCR3' )
                                                              ( low = 'ZRE1' ) ).


    "Select the data from invoice tables
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
     WHERE vbrk~bukrs EQ @iv_company_code
*       AND vbrk~kunag IN @s_kunag
*       AND vbrk~vbeln IN @s_vbeln

*       AND vbrk~fkart IN @s_fkart
       AND vbrk~fkdat BETWEEN @iv_start_date AND @iv_end_date
*       AND vbrk~land1 IN @s_dland1
*       AND vbrk~regio IN @s_dregio
*       AND vbrp~matnr IN @s_matnr
*       AND vbrp~matkl IN @s_matkl
*       AND vbrp~werks IN @s_werks
*       AND vbrp~aland IN @s_oland1
*       AND vbrp~wkreg IN @s_oregio
       AND kssk~mafid = 'O'
       AND kssk~klart = 'Z01'.
*       AND klah~klagr IN @s_klagr
*       AND klah~class IN @s_class.

    IF sy-subrc NE 0.
      RETURN. "No Data
    ENDIF.

    IF lt_vbrp IS NOT INITIAL.
      "Get payment terms descriptions
      SELECT zterm, text1
        INTO TABLE @DATA(lt_paymt_term_desc)
        FROM t052u
       WHERE spras = @sy-langu.
      IF sy-subrc EQ 0.
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

      APPEND INITIAL LINE TO et_invoices REFERENCE INTO DATA(lr_invoices).
      MOVE-CORRESPONDING lr_vbrp->* TO lr_invoices->*.

      "Invert the value of Cancel and Return invoices
      IF lr_vbrp->fkart IN gr_negative_inv_types.
        lr_vbrp->netwr = lr_vbrp->netwr * -1.
        lr_vbrp->item_value = lr_vbrp->item_value * -1.
        lr_invoices->mwsbk = lr_vbrp->mwsbk * -1.
        lr_invoices->mwsbp = lr_vbrp->mwsbp * -1.
      ENDIF.

      MOVE lr_vbrp->netwr TO lr_invoices->invoice_value.
      MOVE lr_vbrp->item_value TO lr_invoices->lineitem_value.

      "Read payment terms description
      READ TABLE lt_paymt_term_desc WITH KEY zterm = lr_vbrp->zterm REFERENCE INTO DATA(lr_paymt_term_desc) BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lr_paymt_term_desc->text1 TO lr_invoices->text1.
      ENDIF.

      "Read Shipping conditions description
      READ TABLE lt_ship_cond_desc WITH KEY vsbed = lr_vbrp->vsbed REFERENCE INTO DATA(lr_ship_cond_desc) BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lr_ship_cond_desc->vtext TO lr_invoices->vtext_shipcond.
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
            e_faedt = lr_invoices->yyvalut.

        MOVE lr_bsid_bsad->belnr TO lr_invoices->belnr.

      ENDIF.

      "If Reference document is not Delivery, clear the field in the output
      IF lr_vbrp->vgtyp NE 'J'.
        CLEAR lr_invoices->vgbel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SALES_KPI_HELPER=>GET_OPEN_ORDERS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VBCOM                       TYPE        VBCOM
* | [--->] IV_OPEN                        TYPE        ABAP_BOOL
* | [<---] ET_OPEN_ORDERS                 TYPE        ZCL_ZSD_KPI_MPC=>TT_OPENSALESORDERS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_open_orders.

    "Structure to calculate order total tax value
    TYPES: BEGIN OF ty_order_tax,
             vbeln TYPE vbap-vbeln,
             mwsbp TYPE vbap-mwsbp,
           END OF ty_order_tax.

    DATA: ls_order_tax TYPE ty_order_tax,
          lt_order_tax TYPE TABLE OF ty_order_tax.

    CHECK iv_vbcom IS NOT INITIAL.

    CALL FUNCTION 'RV_SALES_DOCUMENT_VIEW_3'
      EXPORTING
        vbcom      = iv_vbcom
        bstkd_flag = abap_true
      TABLES
        lvbmtv     = et_open_orders.

    IF et_open_orders IS NOT INITIAL.

      "Filter only Open Orders that do not have invoices done
      IF iv_open EQ abap_true.
        SELECT vbelv,
               posnv,
               vbeln,
               posnn
          FROM vbfa
          INTO TABLE @DATA(lt_invoices)
           FOR ALL ENTRIES IN @et_open_orders
         WHERE vbelv   = @et_open_orders-vbeln
           AND posnv   = @et_open_orders-posnr
           AND vbtyp_n = 'M'.
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
           FOR ALL ENTRIES IN @et_open_orders
         WHERE vbeln = @et_open_orders-vbeln
           AND posnr = @et_open_orders-posnr.
        IF sy-subrc EQ 0.
          SORT lt_item_status BY vbeln posnr.
        ENDIF.
      ENDIF.

      "Select Contracts for Orders
      SELECT vbeln AS order,
             posnn AS order_item,
             vbelv AS contract,
             posnv AS contract_item
        FROM vbfa
        INTO TABLE @DATA(lt_contracts)
         FOR ALL ENTRIES IN @et_open_orders
       WHERE vbeln   = @et_open_orders-vbeln
         AND posnn   = @et_open_orders-posnr
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
         FOR ALL ENTRIES IN @et_open_orders
       WHERE vbelv   = @et_open_orders-vbeln
         AND posnv   = @et_open_orders-posnr
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
         FOR ALL ENTRIES IN @et_open_orders
       WHERE vbak~vbeln = @et_open_orders-vbeln.
      IF sy-subrc EQ 0.
        SORT lt_pricing_status BY vbeln.
      ENDIF.

      "Get tax amount for orders
      SELECT vbeln, posnr, mwsbp
        FROM vbap
        INTO TABLE @DATA(lt_vbap_tax)
         FOR ALL ENTRIES IN @et_open_orders
        WHERE vbeln = @et_open_orders-vbeln
          AND posnr = @et_open_orders-posnr.
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

    ENDIF. "et_open_orders IS NOT INITIAL

    LOOP AT et_open_orders REFERENCE INTO DATA(lr_vbmtv).

      DATA(lv_vbmtv_index) = sy-tabix.

      IF iv_open EQ abap_true.
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
                DELETE et_open_orders INDEX lv_vbmtv_index.
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
              DELETE et_open_orders INDEX lv_vbmtv_index.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      "The net value for the Order should only be present in the first line for the same Order,
      "the subsequent line items for the Order only display the item net value
      ADD 1 TO lv_vbmtv_index.
      READ TABLE et_open_orders INDEX lv_vbmtv_index REFERENCE INTO DATA(lr_vbmtv_next).
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

      "Read tax amount
      READ TABLE lt_vbap_tax WITH KEY vbeln = lr_vbmtv->vbeln
                                      posnr = lr_vbmtv->posnr REFERENCE INTO DATA(lr_vbap_tax).
      IF sy-subrc EQ 0.
        MOVE lr_vbap_tax->mwsbp TO lr_vbmtv->zzitemtax.
      ENDIF.

      CLEAR ls_order_tax.
      MOVE lr_vbmtv->vbeln TO ls_order_tax-vbeln.
      MOVE lr_vbmtv->zzitemtax TO ls_order_tax-mwsbp.
      COLLECT ls_order_tax INTO lt_order_tax.

      "This value keep causing conversion errors!!!
      CLEAR lr_vbmtv->kursk.

    ENDLOOP.

    "Get the Order total tax into the first item in the output table
    LOOP AT lt_order_tax INTO ls_order_tax.
      READ TABLE et_open_orders WITH KEY vbeln = ls_order_tax-vbeln REFERENCE INTO lr_vbmtv BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE ls_order_tax-mwsbp TO lr_vbmtv->zzordertax.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
