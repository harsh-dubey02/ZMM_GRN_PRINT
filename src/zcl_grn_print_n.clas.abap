CLASS zcl_grn_print_n DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    METHODS get_pdf_64
      IMPORTING
        VALUE(io_materialdocno)   TYPE i_materialdocumentheader_2-materialdocument
        VALUE(io_materialdocdat)  TYPE i_materialdocumentheader_2-postingdate
        VALUE(io_materialdocyear) TYPE i_materialdocumentheader_2-MaterialDocumentYear
      RETURNING
        VALUE(pdf_64)             TYPE string.

  PRIVATE SECTION.
    METHODS build_xml_single_po
      IMPORTING
        VALUE(io_materialdocno)   TYPE i_materialdocumentheader_2-materialdocument
        VALUE(io_materialdocdat)  TYPE i_materialdocumentheader_2-postingdate
        VALUE(io_materialdocyear) TYPE i_materialdocumentheader_2-MaterialDocumentYear
      RETURNING
        VALUE(rv_xml)             TYPE string.

    METHODS build_xml_multi_po
      IMPORTING
        VALUE(io_materialdocno)   TYPE i_materialdocumentheader_2-materialdocument
        VALUE(io_materialdocdat)  TYPE i_materialdocumentheader_2-postingdate
        VALUE(io_materialdocyear) TYPE i_materialdocumentheader_2-MaterialDocumentYear
      RETURNING
        VALUE(rv_xml)             TYPE string.
ENDCLASS.



CLASS ZCL_GRN_PRINT_N IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA: lv_doc  TYPE i_materialdocumentheader_2-materialdocument,
          ld_doc  TYPE i_materialdocumentheader_2-postingdate,
          lv_year TYPE i_materialdocumentheader_2-materialdocumentyear,
          lv_pdf  TYPE string.

    out->write( |Enter Material Document Number: | ).
*   in->read( IMPORTING data = lv_doc ).

    out->write( |Enter Posting Date (YYYY-MM-DD): | ).
*   in->read( IMPORTING data = ld_doc ).

    IF lv_doc IS INITIAL OR ld_doc IS INITIAL.
      out->write( |Material Document or Posting Date missing. Exiting...| ).
      RETURN.
    ENDIF.

    lv_pdf = get_pdf_64(
                io_materialdocno   = lv_doc
                io_materialdocdat  = ld_doc
                io_materialdocyear = lv_year ).

    IF lv_pdf IS NOT INITIAL.
      out->write( |PDF Generated Successfully for Material Document { lv_doc }| ).
    ELSE.
      out->write( |Failed to generate PDF.| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_pdf_64.

    DATA:
      lt_po_items TYPE STANDARD TABLE OF i_materialdocumentitem_2-purchaseorderitem,
      lv_first_po TYPE i_materialdocumentitem_2-purchaseorderitem,
      lv_same_po  TYPE abap_bool VALUE abap_true,
      lv_xml      TYPE string,
      lv_template TYPE string.

    "--------------------------------------------------
    " Fetch PO items
    "--------------------------------------------------
    SELECT purchaseorderitem
      FROM i_materialdocumentitem_2
      WHERE materialdocument = @io_materialdocno
      INTO TABLE @lt_po_items.

    "--------------------------------------------------
    " Compare PO items
    "--------------------------------------------------
    IF lt_po_items IS NOT INITIAL.
      READ TABLE lt_po_items INTO lv_first_po INDEX 1.

      LOOP AT lt_po_items INTO DATA(lv_po_item).
        IF lv_po_item <> lv_first_po.
          lv_same_po = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "--------------------------------------------------
    " Decide template + XML
    "--------------------------------------------------
    IF lv_same_po = abap_true.
      lv_template = 'ZRM_GRN/ZRM_GRN'.
      lv_xml = build_xml_single_po(
                  io_materialdocno   = io_materialdocno
                  io_materialdocdat  = io_materialdocdat
                  io_materialdocyear = io_materialdocyear ).
    ELSE.
      lv_template = 'ZGRN_PRINT/ZGRN_PRINT'.
      lv_xml = build_xml_multi_po(
                  io_materialdocno   = io_materialdocno
                  io_materialdocdat  = io_materialdocdat
                  io_materialdocyear = io_materialdocyear ).
    ENDIF.

    IF lv_xml IS INITIAL.
      RETURN.
    ENDIF.

    "--------------------------------------------------
    " Generate PDF (Base64)
    "--------------------------------------------------
    CALL METHOD zadobe_ads_class=>getpdf
      EXPORTING
        template = lv_template
        xmldata  = lv_xml
      RECEIVING
        result   = DATA(lv_result).

    IF lv_result IS NOT INITIAL.
      pdf_64 = lv_result.
    ENDIF.

  ENDMETHOD.


  METHOD build_xml_single_po.
    DATA: lv_batchbysupplier TYPE i_batch-batchbysupplier,
          lv_mfg_month       TYPE string,
          lv_exp_month       TYPE string,
          lv_total_qty       TYPE p LENGTH 13 DECIMALS 2,
          lv_item            TYPE string,
          lv_mat_desc        TYPE string,
          lv_po_no           TYPE i_materialdocumentitem_2-purchaseorder,
          lv_storage_loc     TYPE i_materialdocumentheader_2-StorageLocation,
          lv_grn_date_text   TYPE string,
          lv_po_date         TYPE string,
          lv_chl_dt_ext      TYPE string,
          lv_inv_dt_ext      TYPE string,
          lv_lr_dt_ext       TYPE string,
          lv_ge_dt_ext       TYPE string,
          lv_BOE_dt_ext      TYPE string,
          lv_po_dt_ext       TYPE string,
          lv_pr_no           TYPE i_purchaseorderitemapi01-PurchaseRequisition,
          lv_pr_date         TYPE I_PurchaseRequisitionAPI01-LastChangeDateTime,
          lv_manf_nm         TYPE zi_manuf_dtls_n-ManfNm,
          lv_manf_addr       TYPE zi_manuf_dtls_n-ManfAddr,
          lv_manf_stat       TYPE zi_manuf_dtls_n-ManfStat,
          lv_chl_no          TYPE i_materialdocumentitemtp-yy1_aa3_mmi,
          lv_chl_dt          TYPE i_materialdocumentitemtp-yy1_aa4_mmi,
          lv_ge_no           TYPE i_materialdocumentitemtp-yy1_aa5_mmi,
          lv_ge_dt           TYPE i_materialdocumentitemtp-yy1_aa6_mmi,
          lv_lr_no           TYPE i_materialdocumentitemtp-yy1_aa7_mmi,
          lv_lr_dt           TYPE i_materialdocumentitemtp-yy1_aa8_mmi,
          lv_inv_no          TYPE i_materialdocumentitemtp-yy1_aa9_mmi,
          lv_inv_dt          TYPE i_materialdocumentitemtp-yy1_aa10_mmi,
          lv_rt_dt           TYPE i_materialdocumentitemtp-yy1_aa15_mmi,
          lv_nos             TYPE i_materialdocumentitemtp-yy1_aa1_mmi,
          lv_pkg             TYPE i_materialdocumentitemtp-yy1_aa2_mmi,
          lv_boe             TYPE I_MaterialDocumentItemTP-yy1_aa11_mmi,
          LV_BOE_dt          TYPE I_MaterialDocumentItemTP-yy1_aa12_mmi,
          LV_trans           TYPE I_MaterialDocumentItemTP-yy1_aa13_mmi,
          LV_veh_no          TYPE I_MaterialDocumentItemTP-yy1_aa14_mmi,
          lv_sup_bt          TYPE I_MaterialDocumentItemTP-YY1_SupplierBatch1_mmi,
          lv_bt              TYPE i_materialdocumentitem_2-Batch,
          lv_batch_code      TYPE i_materialdocumentitem_2-Batch,
          lv_hsn_cd          TYPE i_productplantintltrd-ConsumptionTaxCtrlCode,
          lv_itm_nm          TYPE I_ProductText-ProductName,
          lv_insp_lt         TYPE I_InspectionLot-InspectionLot,
          lv_manf_stat_text  TYPE string.

    DATA: lv_postdate      TYPE string,
          lv_year          TYPE string,
          lv_month         TYPE string,
          lv_day           TYPE string,
          lv_month_name    TYPE string,
          lv_po_month_name TYPE string.

    DATA : lv_supplier_btch  TYPE string.

    " Convert GRN Posting Date (YYYYMMDD → DD Month YYYY)
    lv_postdate = io_materialdocdat.
    IF lv_postdate IS NOT INITIAL.
      lv_year  = lv_postdate+0(4).
      lv_month = lv_postdate+4(2).
      lv_day   = lv_postdate+6(2).

      CASE lv_month.
        WHEN '01'. lv_month_name = '01'.
        WHEN '02'. lv_month_name = '02'.
        WHEN '03'. lv_month_name = '03'.
        WHEN '04'. lv_month_name = '04'.
        WHEN '05'. lv_month_name = '05'.
        WHEN '06'. lv_month_name = '06'.
        WHEN '07'. lv_month_name = '07'.
        WHEN '08'. lv_month_name = '08'.
        WHEN '09'. lv_month_name = '09'.
        WHEN '10'. lv_month_name = '10'.
        WHEN '11'. lv_month_name = '11'.
        WHEN '12'. lv_month_name = '12'.
        WHEN OTHERS. lv_month_name = ''.
      ENDCASE.

      lv_grn_date_text = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
    ELSE.
      lv_grn_date_text = 'N/A'.
    ENDIF.

    " Step 1: Fetch Material Document Items
    SELECT DISTINCT
        a~MaterialDocument,
        a~MaterialDocumentItem,
        a~Material,
        a~Plant,
        a~Batch,
        a~EntryUnit,
        a~QuantityInEntryUnit,
        a~ManufactureDate,
        a~ShelfLifeExpirationDate,
        a~PurchaseOrder,
        a~PurchaseOrderITEM,
        b~yy1_aa2_mmi,
        b~yy1_aa1_mmi,
        b~yy1_aa15_mmi,
        b~YY1_SupplierBatch1_mmi,
        c~InspectionLot
   FROM i_materialdocumentitem_2 AS a
   LEFT OUTER JOIN i_materialdocumentitemtp AS b
     ON a~MaterialDocument     = b~MaterialDocument
    AND a~MaterialDocumentItem = b~MaterialDocumentItem
   LEFT OUTER JOIN I_InspectionLot AS c
     ON  c~MaterialDocument     = a~MaterialDocument
     AND c~MaterialDocumentItem = a~MaterialDocumentItem
     AND c~Batch                = a~Batch       "IMPORTANT: avoids wrong inspection lot
  WHERE a~MaterialDocument     = @io_materialdocno
    AND a~PostingDate          = @io_materialdocdat
    AND a~MaterialDocumentYear = @io_materialdocyear
  INTO TABLE @DATA(lt_matdoc).


*    SELECT DISTINCT
*           A~MaterialDocument,
*           A~MaterialDocumentItem,
*           A~Material,
*           A~Plant,
*           A~Batch,
*           A~EntryUnit,
*           A~QuantityInEntryUnit,
*           A~ManufactureDate,
*           A~ShelfLifeExpirationDate,
*           B~yy1_aa4_mmi,
*           A~PurchaseOrder,
*           B~yy1_aa2_mmi,
*             B~yy1_aa1_mmi
*      FROM i_materialdocumentitem_2 AS A
*      LEFT OUTER JOIN  i_materialdocumentitemtp AS B
*      ON A~MaterialDocument = B~MaterialDocument
*      WHERE A~MaterialDocument     = @io_materialdocno
*        AND A~PostingDate          = @io_materialdocdat
*        AND A~MaterialDocumentYear = @io_materialdocyear
*     INTO TABLE @DATA(lt_matdoc).

    READ TABLE lt_matdoc INTO DATA(ls_matdoc) INDEX 1.


*    IF ls_matdoc-yy1_aa4_mmi IS NOT INITIAL.
*      DATA(lv_chdt) = CONV string( ls_matdoc-yy1_aa4_mmi ).
*      lv_chl_dt = |{ lv_chdt+6(2) }/{ lv_chdt+4(2) }/{ lv_chdt+4(4) }|.
*    ENDIF.




    IF lt_matdoc IS NOT INITIAL.
      SELECT
      materialdocument,
      yy1_aa3_mmi,
      yy1_aa5_mmi,
      yy1_aa9_mmi,
      yy1_aa10_mmi,
      yy1_aa8_mmi,
      yy1_aa7_mmi,
      yy1_aa6_mmi,
      yy1_aa2_mmi,
      yy1_aa1_mmi,
      yy1_aa14_mmi,
      yy1_aa13_mmi,
      yy1_aa12_mmi,
      yy1_aa4_mmi,
      YY1_SupplierBatch1_mmi,
      yy1_aa11_mmi
FROM i_materialdocumentitemtp
      FOR ALL ENTRIES IN  @lt_matdoc
      WHERE materialdocument = @lt_matdoc-MaterialDocument
      AND yy1_aa3_mmi IS NOT INITIAL   AND yy1_aa5_mmi IS NOT INITIAL
      INTO TABLE @DATA(lt_matdoc_item).

      READ TABLE lt_matdoc_item INTO DATA(ls_matdoc_item) INDEX 1.
      IF sy-subrc = 0.

        " Convert YYYYMMDD → DD-MM-YY

*        IF ls_matdoc_item-yy1_aa10_mmi IS NOT INITIAL.
*          DATA(lv_idt) = CONV string( ls_matdoc_item-yy1_aa10_mmi ).
*          lv_inv_dt = |{ lv_idt+6(2) }/{ lv_idt+4(2) }/{ lv_idt+2(4) }|.
*        ENDIF.

        lv_chl_dt = ls_matdoc_item-yy1_aa4_mmi.
        IF lv_chl_dt IS NOT INITIAL.
          lv_year  = lv_chl_dt+0(4).
          lv_month = lv_chl_dt+4(2).
          lv_day   = lv_chl_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_chl_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_chl_dt_ext = 'N/A'.
        ENDIF.


        lv_inv_dt = ls_matdoc_item-yy1_aa10_mmi.
        IF lv_inv_dt IS NOT INITIAL.
          lv_year  = lv_inv_dt+0(4).
          lv_month = lv_inv_dt+4(2).
          lv_day   = lv_inv_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_inv_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_inv_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa8_mmi IS NOT INITIAL.
*          DATA(lv_lrdt) = CONV string( ls_matdoc_item-yy1_aa8_mmi ).
*          lv_lr_dt = |{ lv_lrdt+6(2) }/{ lv_lrdt+4(2) }/{ lv_lrdt+2(4) }|.
*        ENDIF.

        lv_lr_dt = ls_matdoc_item-yy1_aa8_mmi.
        IF lv_chl_dt IS NOT INITIAL.
          lv_year  = lv_lr_dt+0(4).
          lv_month = lv_lr_dt+4(2).
          lv_day   = lv_lr_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_lr_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_lr_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa6_mmi IS NOT INITIAL.
*          DATA(lv_gedt) = CONV string( ls_matdoc_item-yy1_aa6_mmi ).
*          lv_ge_dt = |{ lv_gedt+6(2) }/{ lv_gedt+4(2) }/{ lv_gedt+2(4) }|.
*        ENDIF.

        lv_ge_dt = ls_matdoc_item-yy1_aa6_mmi.
        IF lv_ge_dt IS NOT INITIAL.
          lv_year  = lv_ge_dt+0(4).
          lv_month = lv_ge_dt+4(2).
          lv_day   = lv_ge_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_ge_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_ge_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa12_mmi IS NOT INITIAL.
*          DATA(LV_BOEdt) = CONV string( ls_matdoc_item-yy1_aa12_mmi ).
*          LV_BOE_dt = |{ lv_boedt+6(2) }/{ lv_boedt+4(2) }/{ lv_boedt+2(4) }|.
*        ENDIF.

        LV_BOE_dt = ls_matdoc_item-yy1_aa12_mmi.
        IF LV_BOE_dt IS NOT INITIAL.
          lv_year  = LV_BOE_dt+0(4).
          lv_month = LV_BOE_dt+4(2).
          lv_day   = LV_BOE_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_BOE_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_BOE_dt_ext = 'N/A'.
        ENDIF.

      ENDIF.


*    IF sy-subrc = 0.
*        l= ls_matdoc_item.
*      ELSE.
*        lv_mat_desc = 'Description not found'.
*      ENDIF.

      IF lv_mat_desc IS INITIAL.
        lv_mat_desc = 'Description not found'.
      ENDIF.

    ENDIF.

    SORT lt_matdoc BY MaterialDocumentItem ASCENDING.

    " Step 2: Fetch Storage Location

    SELECT SINGLE
       a~StorageLocation,
       b~StorageLocationName
  FROM I_MaterialDocumentItem_2 AS a
  LEFT OUTER JOIN I_StorageLocation AS b
    ON  a~StorageLocation = b~StorageLocation
   AND a~Plant            = b~Plant
 WHERE a~MaterialDocument = @io_materialdocno
 INTO @DATA(ls_sloc).

    " Step 3: Get first Purchase Order number
    READ TABLE lt_matdoc INDEX 1 INTO DATA(ls_first_item).
    IF sy-subrc = 0.
      lv_po_no = ls_first_item-PurchaseOrder.
    ENDIF.

    " Step 4: Get PO Creation Date from I_PURCHASEORDERAPI01
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE CreationDate
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @DATA(lv_po_creationdate).

*     lv_po_date = ls_matdoc_item-yy1_aa6_mmi.
      IF lv_po_creationdate IS NOT INITIAL.
        lv_year  = lv_po_creationdate+0(4).
        lv_month = lv_po_creationdate+4(2).
        lv_day   = lv_po_creationdate+6(2).

        CASE lv_month.
          WHEN '01'. lv_month_name = '01'.
          WHEN '02'. lv_month_name = '02'.
          WHEN '03'. lv_month_name = '03'.
          WHEN '04'. lv_month_name = '04'.
          WHEN '05'. lv_month_name = '05'.
          WHEN '06'. lv_month_name = '06'.
          WHEN '07'. lv_month_name = '07'.
          WHEN '08'. lv_month_name = '08'.
          WHEN '09'. lv_month_name = '09'.
          WHEN '10'. lv_month_name = '10'.
          WHEN '11'. lv_month_name = '11'.
          WHEN '12'. lv_month_name = '12'.
          WHEN OTHERS. lv_month_name = ''.
        ENDCASE.

        lv_po_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
      ELSE.
        lv_po_dt_ext = 'N/A'.
      ENDIF.
    ELSE.
      lv_po_date = 'N/A'.
    ENDIF.

    " Step 5: Get PR Number and Manufacturer Info
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE
             a~PurchaseOrder,
             a~PurchaseOrderItem,
             a~PurchaseRequisition,
             b~manfnm,
             b~manfno,
             b~manfstat
        FROM i_purchaseorderitemapi01 AS a
        INNER JOIN zc_mat_linkage AS b
          ON a~Material = b~MaterialNo
        WHERE a~PurchaseOrder = @lv_po_no
        INTO @DATA(ls_poitem).

      IF sy-subrc = 0 AND ls_poitem IS NOT INITIAL.
        lv_pr_no = ls_poitem-PurchaseRequisition.
      ELSE.
        lv_pr_no = 'N/A'.
      ENDIF.
    ELSE.
      lv_pr_no = 'N/A'.
    ENDIF.

    IF lv_po_no IS NOT INITIAL.
      SELECT
      a~PurchaseOrder,
      a~PurchaseOrderItem,
      a~PurchaseRequisition,
      b~LastChangeDateTime
*      a~YY1_ManufacturerNoA1_PDI
      FROM i_purchaseorderitemapi01 AS a
      LEFT OUTER JOIN I_PurchaseRequisitionAPI01 AS b ON a~PurchaseRequisition = b~PurchaseRequisition
      WHERE a~PurchaseOrder = @lv_po_no
      INTO TABLE @DATA(lt_poitem).


      IF sy-subrc = 0 AND lt_poitem IS NOT INITIAL.
        READ TABLE lt_poitem INDEX 1 INTO DATA(ls_po_item).

        IF sy-subrc = 0 AND ls_po_item-PurchaseRequisition IS NOT INITIAL.
          lv_pr_no     = ls_po_item-PurchaseRequisition.
          Lv_pr_date  =  ls_po_item-LastChangeDateTime.
        ELSE.
          lv_pr_no = 'N/A'.
        ENDIF.

      ELSE.
        lv_pr_no = 'N/A'.
      ENDIF.

    ELSE.
      lv_pr_no = 'N/A'.
    ENDIF.
    DATA(lv_pr_raw) = CONV string( ls_po_item-LastChangeDateTime ).

    IF lv_pr_raw IS NOT INITIAL.

      "Extract only YYYYMMDD (first 8 chars)
      lv_pr_date = lv_pr_raw+0(8).
    ELSE.
      lv_pr_date = 'N/A'.
    ENDIF.

    IF ls_po_item-LastChangeDateTime IS NOT INITIAL.
      DATA(LV_prdt) = CONV string( ls_po_item-LastChangeDateTime ).
      lv_pr_raw = |{ lv_prdt+6(2) }-{ lv_prdt+4(2) }-{ lv_prdt+2(2) }|.
    ENDIF.




*
*   IF lt_poitem IS NOT INITIAL.
*
*
**  SELECT
**    ManfNm,
**    ManfNo,
**    ManfAddr,
**    ManfStat
**    FROM zi_manuf_dtls_n
**    FOR ALL ENTRIES IN @lt_poitem
**    WHERE ManfNo = @lt_poitem-
**    INTO TABLE @DATA(it_zc).
**
**  READ TABLE it_zc INDEX 1 INTO DATA(ls_zc).
**
**  IF sy-subrc = 0.
**
**    lv_manf_nm   = ls_zc-ManfNm.
**    lv_manf_stat = ls_zc-ManfStat.
**    lv_manf_addr = ls_zc-ManfAddr.
**
**    "----------------------------
**    "  A → Approved
**    "  B → Blocked
**    "----------------------------
*
*
*  ENDIF.
*

    READ TABLE lt_matdoc INTO DATA(wa_mat_item) INDEX 1.
    SELECT SINGLE *
      FROM I_PurchaseOrderItemAPI01
      WHERE purchaseorder = @wa_mat_item-PurchaseOrder
      INTO @DATA(wa_po_item_pant).

    SELECT SINGLE *
         FROM zc_manuf_dtls_n
        WHERE ManfNo = @wa_po_item_pant-YY1_ManufacturerNo1_PDI
         INTO @DATA(wa_po_manuf).

    DATA: lt_manf TYPE RANGE OF zi_manuf_dtls_n-ManfNo.
    LOOP AT lt_poitem INTO DATA(ls_po).
*  IF ls_po-YY1_ManufacturerNoA1_PDI IS NOT INITIAL.
*
*    APPEND VALUE #(
*      sign   = 'I'
*      option = 'EQ'
*      low    = CONV zi_manuf_dtls_n-ManfNo( ls_po-YY1_ManufacturerNoA1_PDI )
*    ) TO lt_manf.
*
*  ENDIF.
    ENDLOOP.

*DELETE ADJACENT DUPLICATES FROM lt_manf COMPARING low.
*SELECT
*    ManfNm,
*    ManfNo,
*    ManfAddr,
*    ManfStat
*  FROM zi_manuf_dtls_n
*  WHERE ManfNo IN @lt_manf
*  INTO TABLE @DATA(it_zc).
*
* READ TABLE it_zc INDEX 1 INTO DATA(ls_zc).
*
*  IF sy-subrc = 0.
*
*    lv_manf_nm   = ls_zc-ManfNm.
*    lv_manf_stat = ls_zc-ManfStat.
*    lv_manf_addr = ls_zc-ManfAddr.
*
*  ENDIF.



*ENDIF.

    DATA: lv_po_type     TYPE i_purchaseorderapi01-purchaseordertype,
          lv_po_category TYPE string.

    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE PurchaseOrderType
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @lv_po_type.

      IF sy-subrc = 0 AND lv_po_type IS NOT INITIAL.
        CASE lv_po_type.
          WHEN 'ZDOM'.
            lv_po_category = 'DOMESTIC'.
          WHEN 'ZIMP'.
            lv_po_category = 'IMPORT'.
          WHEN OTHERS.
            lv_po_category = 'OTHER'.
        ENDCASE.
      ELSE.
        lv_po_category = 'NOT FOUND'.
      ENDIF.
    ELSE.
      lv_po_category = 'N/A'.
    ENDIF.


    DATA: lv_supplier      TYPE i_purchaseorderapi01-supplier,
          lv_vendor_name   TYPE i_supplier-organizationbpname1,
          ls_supplier_addr TYPE i_supplier.


    TYPES: BEGIN OF ty_header,
             vendor        TYPE string,
             vendoraddress TYPE string,
           END OF ty_header.

    DATA: gs_header TYPE ty_header.


    " 1. Get Supplier from PO
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE Supplier
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @lv_supplier.
    ENDIF.

    " 2. Get Supplier Name
    IF sy-subrc = 0 AND lv_supplier IS NOT INITIAL.
      SELECT SINGLE OrganizationBPName1
        FROM i_supplier
        WHERE Supplier = @lv_supplier
        INTO @lv_vendor_name.

      IF sy-subrc = 0.
        gs_header-vendor = lv_vendor_name.
      ELSE.
        gs_header-vendor = 'N/A'.
      ENDIF.
    ELSE.
      gs_header-vendor = 'N/A'.
    ENDIF.

    " 3. Get Supplier Address (Street, PostalCode, City)
    IF lv_supplier IS NOT INITIAL.
      SELECT SINGLE
             StreetName,
             CityName,
             PostalCode
        FROM i_supplier
        WHERE Supplier = @lv_supplier
        INTO CORRESPONDING FIELDS OF @ls_supplier_addr.

      IF sy-subrc = 0.
        gs_header-vendoraddress =
          |{ ls_supplier_addr-streetname }, { ls_supplier_addr-postalcode } { ls_supplier_addr-cityname }|.
        CONDENSE gs_header-vendoraddress.
      ELSE.
        gs_header-vendoraddress = 'N/A'.
      ENDIF.
    ELSE.
      gs_header-vendoraddress = 'N/A'.
    ENDIF.

*    DATA(lv_clean) = |{ ls_matdoc_item-yy1_aa9_mmi DECIMALS = 0 }|.
*    DATA(ls_clean) = |{ ls_matdoc_item-yy1_aa7_mmi DECIMALS = 0 }|.

    " Step 6: Build header XML
    DATA(lv_header) =
     |<form1>| &&
     |  <Subform1>| &&
     |    <MFG_NAME>{ wa_po_manuf-ManfNm }</MFG_NAME>| &&
     |    <ADDRESS>{ wa_po_manuf-ManfAddr }</ADDRESS>| &&
     |    <STATUS>{ wa_po_manuf-ManfStat }</STATUS>| &&
     |  </Subform1>| &&
     |  <Subform2>| &&
     |    <SUPPLIER>{ lv_vendor_name }</SUPPLIER>| &&
     |    <ADDR>{ gs_header-vendoraddress }</ADDR>| &&
     |    <GRN_NO>{ io_materialdocno }</GRN_NO>| &&
     |    <GRN_DATE>{ lv_grn_date_text }</GRN_DATE>| &&
     |    <TYPE>{ lv_po_category }</TYPE>| &&
     |  </Subform2>| &&
     |  <Subform3>| &&
     |    <INVOICE>{ ls_matdoc_item-yy1_aa9_mmi }</INVOICE>| &&
     |    <GE_NO>{ ls_matdoc_item-yy1_aa5_mmi }</GE_NO>| &&
     |    <TRANSPORTER>{ ls_matdoc_item-yy1_aa13_mmi }</TRANSPORTER>| &&
     |    <PO_NO>{ lv_po_no }</PO_NO>| &&
     |    <GE_DT>{ lv_ge_dt_ext }</GE_DT>| &&
     |    <LR_NO>{ ls_matdoc_item-yy1_aa7_mmi }</LR_NO>| &&
     |    <PO_DT>{ lv_po_dt_ext }</PO_DT>| &&
     |    <LR_DT>{ lv_lr_dt_ext }</LR_DT>| &&
     |    <STORAGE_LOC>{ ls_sloc-StorageLocation },{ ls_sloc-StorageLocationName }</STORAGE_LOC>| &&
     |    <VEHICLE_NO>{ ls_matdoc_item-yy1_aa14_mmi }</VEHICLE_NO>| &&
     |    <CHL_NO>{ ls_matdoc_item-yy1_aa3_mmi }</CHL_NO>| &&
     |    <CHL_DT>{ lv_chl_dt_ext }</CHL_DT>| &&
     |    <INVOICE_DT>{ lv_inv_dt_ext }</INVOICE_DT>| &&
     |    <BOE_NO>{ ls_matdoc_item-yy1_aa11_mmi }</BOE_NO>| &&
     |    <BOE_DT>{ lv_boe_dt_ext }</BOE_DT>| &&
*     |    <ITEM_CODE></ITEM_CODE>| &&
*     |    <ITEM_NAME>{ lv_itm_nm }</ITEM_NAME>| &&
*     |    <HSN_CD>{ lv_hsn_cd }</HSN_CD>| &&
     |  </Subform3>| &&
      |  <Subform4>| &&
     |    <Table1>| &&
     |      <HeaderRow/>|.


    DATA:lv_items TYPE string.
    DATA(lv_item_row) = ``.

    " Step 5: Loop through all items
    LOOP AT lt_matdoc INTO DATA(ls_item).

      CLEAR: lv_batchbysupplier, lv_mfg_month, lv_exp_month,
             lv_mat_desc,lv_sup_bt,lv_batch_code.
*             lv_rt_dt.

      "-----------------------------------------------------
      " Supplier Batch (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-Batch IS NOT INITIAL.
        SELECT SINGLE BatchBySupplier
          FROM I_Batch
          WHERE Batch    = @ls_item-Batch
            AND Material = @ls_item-Material
            AND Plant    = @ls_item-Plant
          INTO @lv_batchbysupplier.
      ENDIF.

*
      IF lv_batchbysupplier IS INITIAL.
        lv_supplier_btch = ls_item-YY1_SupplierBatch1_MMI.
      ELSE.
        lv_supplier_btch = lv_batchbysupplier.
      ENDIF.

      "-----------------------------------------------------
      " Material Description (Existing Logic)
      "-----------------------------------------------------
      SELECT
      a~ProductName,
      b~product,
      b~ConsumptionTaxCtrlCode,
      d~StandardPricePrevYear
        FROM I_ProductText AS a
        INNER JOIN i_productplantintltrd AS b
        ON a~Product = b~Product
         INNER JOIN I_ProductValuationAccounting_2 AS d
         ON b~product = d~Product
        WHERE a~Product  = @ls_item-Material
          AND Language = @sy-langu
        INTO TABLE @DATA(ls_mat_desc).

      READ TABLE ls_mat_desc INDEX 1 INTO DATA(ln_mat_desc).
      IF sy-subrc = 0.
        lv_hsn_cd = ln_mat_desc-ConsumptionTaxCtrlCode.
        lv_itm_nm  = ln_mat_desc-ProductName.
      ENDIF.




      "-----------------------------------------------------
      " NEW LOGIC FOR BATCH CODE (From I_MATERIALDOCUMENTITEM_2)
      "-----------------------------------------------------
*      SELECT SINGLE  mdi~Batch,insp~InspectionLot
*  FROM I_InspectionLot AS insp
* WHERE insp~MaterialDocument     = @mdi-MaterialDocument
*   AND insp~MaterialDocumentItem = @mdi-MaterialDocumentItem
*   AND insp~Batch                = @mdi-Batch
*  INTO @lv_insp_lt.
*
*      SELECT SINGLE InspectionLot
*      FROM I_InspectionLot
*      INTO @lv_insp_lt.
*
*
*      SELECT distinct
*        mdi~Batch,
*        insp~InspectionLot
*   FROM I_MaterialDocumentItem_2 AS mdi
*   LEFT JOIN I_InspectionLot AS insp
*     ON insp~MaterialDocument     = mdi~MaterialDocument
*    AND insp~MaterialDocumentItem = mdi~MaterialDocumentItem
*   WHERE mdi~MaterialDocument     = insp~MaterialDocument
*     AND mdi~MaterialDocumentItem = insp~MaterialDocumentItem
*    INTO TABLE @DATA(ls_join).
*
*   READ TABLE ls_join INDEX 1 INTO DATA(ln_join).
*   IF sy-subrc = 0.
*      lv_batch_code = ln_join-Batch.
*      lv_insp_lt  = ln_join-InspectionLot.
*    ENDIF.
*        IF lv_batch_code IS INITIAL.
*          lv_batch_code = ''.
*        ENDIF.
*

      """



      "-----------------------------------------------------
      " Manufacture Month (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-ManufactureDate IS NOT INITIAL.
        DATA(lv_mfg_year)   = ls_item-ManufactureDate(4).
        DATA(lv_mfg_monthn) = ls_item-ManufactureDate+4(2).

        lv_mfg_month = SWITCH #( lv_mfg_monthn
          WHEN '01' THEN |Jan { lv_mfg_year }|
          WHEN '02' THEN |Feb { lv_mfg_year }|
          WHEN '03' THEN |Mar { lv_mfg_year }|
          WHEN '04' THEN |Apr { lv_mfg_year }|
          WHEN '05' THEN |May { lv_mfg_year }|
          WHEN '06' THEN |Jun { lv_mfg_year }|
          WHEN '07' THEN |Jul { lv_mfg_year }|
          WHEN '08' THEN |Aug { lv_mfg_year }|
          WHEN '09' THEN |Sep { lv_mfg_year }|
          WHEN '10' THEN |Oct { lv_mfg_year }|
          WHEN '11' THEN |Nov { lv_mfg_year }|
          WHEN '12' THEN |Dec { lv_mfg_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_mfg_month = 'N/A'.
      ENDIF.


      "-----------------------------------------------------
      " Expiry Month (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-ShelfLifeExpirationDate IS NOT INITIAL.
        DATA(lv_exp_year)   = ls_item-ShelfLifeExpirationDate(4).
        DATA(lv_exp_monthn) = ls_item-ShelfLifeExpirationDate+4(2).

        lv_exp_month = SWITCH #( lv_exp_monthn
          WHEN '01' THEN |Jan { lv_exp_year }|
          WHEN '02' THEN |Feb { lv_exp_year }|
          WHEN '03' THEN |Mar { lv_exp_year }|
          WHEN '04' THEN |Apr { lv_exp_year }|
          WHEN '05' THEN |May { lv_exp_year }|
          WHEN '06' THEN |Jun { lv_exp_year }|
          WHEN '07' THEN |Jul { lv_exp_year }|
          WHEN '08' THEN |Aug { lv_exp_year }|
          WHEN '09' THEN |Sep { lv_exp_year }|
          WHEN '10' THEN |Oct { lv_exp_year }|
          WHEN '11' THEN |Nov { lv_exp_year }|
          WHEN '12' THEN |Dec { lv_exp_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_exp_month = 'N/A'.
      ENDIF.


      "-----------------------------------------------------
      " Retest Date (Existing Logic)
      "-----------------------------------------------------
*      READ TABLE lt_matdoc_item INTO DATA(ls_retest)
*           WITH KEY materialdocument = ls_item-MaterialDocument.

*      select SINGLE *
*      from I_MaterialDocumentItemTP
*      WHERE materialdocumentITEM = @ls_item-MaterialDocumentItem
*      into  @data(ls_matdoc_rt).

      IF ls_item-yy1_aa15_mmi IS NOT INITIAL.

        DATA(lv_rt_year)   = ls_item-yy1_aa15_mmi(4).
        DATA(lv_rt_monthn) = ls_item-yy1_aa15_mmi+4(2).

        lv_rt_dt = SWITCH #( lv_rt_monthn
          WHEN '01' THEN |Jan { lv_rt_year }|
          WHEN '02' THEN |Feb { lv_rt_year }|
          WHEN '03' THEN |Mar { lv_rt_year }|
          WHEN '04' THEN |Apr { lv_rt_year }|
          WHEN '05' THEN |May { lv_rt_year }|
          WHEN '06' THEN |Jun { lv_rt_year }|
          WHEN '07' THEN |Jul { lv_rt_year }|
          WHEN '08' THEN |Aug { lv_rt_year }|
          WHEN '09' THEN |Sep { lv_rt_year }|
          WHEN '10' THEN |Oct { lv_rt_year }|
          WHEN '11' THEN |Nov { lv_rt_year }|
          WHEN '12' THEN |Dec { lv_rt_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_rt_dt = 'N/A'.
      ENDIF.





      "-----------------------------------------------------
      " Formatting
      "-----------------------------------------------------
      ls_item-Material = |{ ls_item-Material ALPHA = OUT }|.
      lv_item          = |{ ls_item-MaterialDocumentItem ALPHA = OUT }|.
      ls_matdoc_item-yy1_aa1_mmi = |{ ls_matdoc_item-yy1_aa1_mmi ALPHA = OUT }|.
      ls_matdoc_item-yy1_aa2_mmi = |{ ls_matdoc_item-yy1_aa2_mmi ALPHA = OUT }|.

      SHIFT ls_item-yy1_aa1_mmi LEFT DELETING LEADING '0'.
      SHIFT ls_item-yy1_aa2_mmi LEFT DELETING LEADING '0'.

      "-----------------------------------------------------
      " XML Row (BATCH_CODE added)
      "-----------------------------------------------------
      lv_item_row =
  |      <Row1>| &&
  |        <SRNO>{ lv_item }</SRNO>| &&
  |        <INSPECTION_NO>{ ls_item-InspectionLot }</INSPECTION_NO>| &&
  |        <UNIT>{ ls_item-EntryUnit }</UNIT>| &&
  |        <BATCH_CODE>{ ls_item-Batch }</BATCH_CODE>| &&
  |        <SUPPLIER_NO>{ lv_supplier_btch }</SUPPLIER_NO>| &&
  |        <MFG_MONTH>{ lv_mfg_month }</MFG_MONTH>| &&
  |        <RETEST_MONTH>{ lv_rt_dt }</RETEST_MONTH>| &&
  |        <EXPIRY_MONTH>{ lv_exp_month }</EXPIRY_MONTH>| &&
  |        <NOS>{ ls_item-yy1_aa1_mmi }</NOS>| &&
  |        <PKG>{ ls_item-yy1_aa2_mmi }</PKG>| &&
  |        <QUANTITY>{ ls_item-QuantityInEntryUnit }</QUANTITY>| &&
  |        <REJECTED></REJECTED>| &&
  |      </Row1>|.

*      lv_total_qty += ls_item-QuantityInEntryUnit.

      lv_items = lv_items && lv_item_row.
      CLEAR lv_item_row.

    ENDLOOP.




    " Step 6: Footer XML
    DATA(lv_footer) =
     |    </Table1>| &&
     |   <ITEM_CODE>{ ls_item-Material }</ITEM_CODE>| &&
     |   <ITEM_NAME>{ lv_itm_nm }</ITEM_NAME>| &&
     |   <HSN_CD>{ lv_hsn_cd }</HSN_CD>| &&
     |  </Subform4>| &&
     |  <Subform8>| &&
     |    <WAREHOUSE_DES></WAREHOUSE_DES>| &&
     |    <GRN_PREPARED></GRN_PREPARED>| &&
     |    <APPROVED_QC></APPROVED_QC>| &&
     |    <MATERIAL_REC></MATERIAL_REC>| &&
     |    <SAMPLED_BY></SAMPLED_BY>| &&
     |    <SAMPLED_QTY></SAMPLED_QTY>| &&
     |    <CHECKED_BY_QC></CHECKED_BY_QC>| &&
     |    <REASON_REJ></REASON_REJ>| &&
     |  </Subform8>| &&
     |</form1>|.

    rv_xml = |{ lv_header } { lv_items } { lv_footer }|.

  ENDMETHOD.


  METHOD build_xml_multi_po.
    " TODO: Build XML for multi PO form (ZGRN_PRINT)

    DATA: lv_batchbysupplier TYPE i_batch-batchbysupplier,
          lv_mfg_month       TYPE string,
          lv_exp_month       TYPE string,
          lv_total_qty       TYPE p LENGTH 13 DECIMALS 2,
          lv_item            TYPE string,
          lv_mat_desc        TYPE string,
          lv_po_no           TYPE i_materialdocumentitem_2-purchaseorder,
          lv_storage_loc     TYPE i_materialdocumentheader_2-StorageLocation,
          lv_grn_date_text   TYPE string,
          lv_po_date         TYPE string,
          lv_chl_dt_ext      TYPE string,
          lv_inv_dt_ext      TYPE string,
          lv_lr_dt_ext       TYPE string,
          lv_ge_dt_ext       TYPE string,
          lv_BOE_dt_ext      TYPE string,
          lv_po_dt_ext       TYPE string,
          lv_pr_no           TYPE i_purchaseorderitemapi01-PurchaseRequisition,
          lv_pr_date         TYPE I_PurchaseRequisitionAPI01-LastChangeDateTime,
          lv_manf_nm         TYPE zi_manuf_dtls_n-ManfNm,
          lv_manf_addr       TYPE zi_manuf_dtls_n-ManfAddr,
          lv_manf_stat       TYPE zi_manuf_dtls_n-ManfStat,
          lv_chl_no          TYPE i_materialdocumentitemtp-yy1_aa3_mmi,
          lv_chl_dt          TYPE i_materialdocumentitemtp-yy1_aa4_mmi,
          lv_ge_no           TYPE i_materialdocumentitemtp-yy1_aa5_mmi,
          lv_ge_dt           TYPE i_materialdocumentitemtp-yy1_aa6_mmi,
          lv_lr_no           TYPE i_materialdocumentitemtp-yy1_aa7_mmi,
          lv_lr_dt           TYPE i_materialdocumentitemtp-yy1_aa8_mmi,
          lv_inv_no          TYPE i_materialdocumentitemtp-yy1_aa9_mmi,
          lv_inv_dt          TYPE i_materialdocumentitemtp-yy1_aa10_mmi,
          lv_rt_dt           TYPE i_materialdocumentitemtp-yy1_aa15_mmi,
          lv_nos             TYPE i_materialdocumentitemtp-yy1_aa1_mmi,
          lv_pkg             TYPE i_materialdocumentitemtp-yy1_aa2_mmi,
          lv_boe             TYPE I_MaterialDocumentItemTP-yy1_aa11_mmi,
          LV_BOE_dt          TYPE I_MaterialDocumentItemTP-yy1_aa12_mmi,
          LV_trans           TYPE I_MaterialDocumentItemTP-yy1_aa13_mmi,
          LV_veh_no          TYPE I_MaterialDocumentItemTP-yy1_aa14_mmi,
          lv_sup_bt          TYPE I_MaterialDocumentItemTP-YY1_SupplierBatch1_mmi,
          lv_bt              TYPE i_materialdocumentitem_2-Batch,
          lv_batch_code      TYPE i_materialdocumentitem_2-Batch,
          lv_hsn_cd          TYPE i_productplantintltrd-ConsumptionTaxCtrlCode,
          lv_itm_nm          TYPE I_ProductText-ProductName,
          lv_insp_lt         TYPE I_InspectionLot-InspectionLot,
          lv_manf_stat_text  TYPE string.

    DATA: lv_postdate      TYPE string,
          lv_year          TYPE string,
          lv_month         TYPE string,
          lv_day           TYPE string,
          lv_month_name    TYPE string,
          lv_po_month_name TYPE string.

    DATA : lv_supplier_btch  TYPE string.

    " Convert GRN Posting Date (YYYYMMDD → DD Month YYYY)
    lv_postdate = io_materialdocdat.
    IF lv_postdate IS NOT INITIAL.
      lv_year  = lv_postdate+0(4).
      lv_month = lv_postdate+4(2).
      lv_day   = lv_postdate+6(2).

      CASE lv_month.
        WHEN '01'. lv_month_name = '01'.
        WHEN '02'. lv_month_name = '02'.
        WHEN '03'. lv_month_name = '03'.
        WHEN '04'. lv_month_name = '04'.
        WHEN '05'. lv_month_name = '05'.
        WHEN '06'. lv_month_name = '06'.
        WHEN '07'. lv_month_name = '07'.
        WHEN '08'. lv_month_name = '08'.
        WHEN '09'. lv_month_name = '09'.
        WHEN '10'. lv_month_name = '10'.
        WHEN '11'. lv_month_name = '11'.
        WHEN '12'. lv_month_name = '12'.
        WHEN OTHERS. lv_month_name = ''.
      ENDCASE.

      lv_grn_date_text = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
    ELSE.
      lv_grn_date_text = 'N/A'.
    ENDIF.

    " Step 1: Fetch Material Document Items
    SELECT DISTINCT
        a~MaterialDocument,
        a~MaterialDocumentItem,
        a~Material,
        a~Plant,
        a~Batch,
        a~EntryUnit,
        a~QuantityInEntryUnit,
        a~ManufactureDate,
        a~ShelfLifeExpirationDate,
        a~PurchaseOrder,
        a~PurchaseOrderITEM,
        b~yy1_aa2_mmi,
        b~yy1_aa1_mmi,
        b~yy1_aa15_mmi,
        b~YY1_SupplierBatch1_mmi,
        c~InspectionLot
   FROM i_materialdocumentitem_2 AS a
   LEFT OUTER JOIN i_materialdocumentitemtp AS b
     ON a~MaterialDocument     = b~MaterialDocument
    AND a~MaterialDocumentItem = b~MaterialDocumentItem
   LEFT OUTER JOIN I_InspectionLot AS c
     ON  c~MaterialDocument     = a~MaterialDocument
     AND c~MaterialDocumentItem = a~MaterialDocumentItem
     AND c~Batch                = a~Batch       "IMPORTANT: avoids wrong inspection lot
  WHERE a~MaterialDocument     = @io_materialdocno
    AND a~PostingDate          = @io_materialdocdat
    AND a~MaterialDocumentYear = @io_materialdocyear
  INTO TABLE @DATA(lt_matdoc).


*    SELECT DISTINCT
*           A~MaterialDocument,
*           A~MaterialDocumentItem,
*           A~Material,
*           A~Plant,
*           A~Batch,
*           A~EntryUnit,
*           A~QuantityInEntryUnit,
*           A~ManufactureDate,
*           A~ShelfLifeExpirationDate,
*           B~yy1_aa4_mmi,
*           A~PurchaseOrder,
*           B~yy1_aa2_mmi,
*             B~yy1_aa1_mmi
*      FROM i_materialdocumentitem_2 AS A
*      LEFT OUTER JOIN  i_materialdocumentitemtp AS B
*      ON A~MaterialDocument = B~MaterialDocument
*      WHERE A~MaterialDocument     = @io_materialdocno
*        AND A~PostingDate          = @io_materialdocdat
*        AND A~MaterialDocumentYear = @io_materialdocyear
*     INTO TABLE @DATA(lt_matdoc).

    READ TABLE lt_matdoc INTO DATA(ls_matdoc) INDEX 1.


*    IF ls_matdoc-yy1_aa4_mmi IS NOT INITIAL.
*      DATA(lv_chdt) = CONV string( ls_matdoc-yy1_aa4_mmi ).
*      lv_chl_dt = |{ lv_chdt+6(2) }/{ lv_chdt+4(2) }/{ lv_chdt+4(4) }|.
*    ENDIF.




    IF lt_matdoc IS NOT INITIAL.
      SELECT
      materialdocument,
      yy1_aa3_mmi,
      yy1_aa5_mmi,
      yy1_aa9_mmi,
      yy1_aa10_mmi,
      yy1_aa8_mmi,
      yy1_aa7_mmi,
      yy1_aa6_mmi,
      yy1_aa2_mmi,
      yy1_aa1_mmi,
      yy1_aa14_mmi,
      yy1_aa13_mmi,
      yy1_aa12_mmi,
      yy1_aa4_mmi,
      YY1_SupplierBatch1_mmi,
      yy1_aa11_mmi
FROM i_materialdocumentitemtp
      FOR ALL ENTRIES IN  @lt_matdoc
      WHERE materialdocument = @lt_matdoc-MaterialDocument
      AND yy1_aa3_mmi IS NOT INITIAL   AND yy1_aa5_mmi IS NOT INITIAL
      INTO TABLE @DATA(lt_matdoc_item).

      READ TABLE lt_matdoc_item INTO DATA(ls_matdoc_item) INDEX 1.
      IF sy-subrc = 0.

        " Convert YYYYMMDD → DD-MM-YY

*        IF ls_matdoc_item-yy1_aa10_mmi IS NOT INITIAL.
*          DATA(lv_idt) = CONV string( ls_matdoc_item-yy1_aa10_mmi ).
*          lv_inv_dt = |{ lv_idt+6(2) }/{ lv_idt+4(2) }/{ lv_idt+2(4) }|.
*        ENDIF.

        lv_chl_dt = ls_matdoc_item-yy1_aa4_mmi.
        IF lv_chl_dt IS NOT INITIAL.
          lv_year  = lv_chl_dt+0(4).
          lv_month = lv_chl_dt+4(2).
          lv_day   = lv_chl_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_chl_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_chl_dt_ext = 'N/A'.
        ENDIF.


        lv_inv_dt = ls_matdoc_item-yy1_aa10_mmi.
        IF lv_inv_dt IS NOT INITIAL.
          lv_year  = lv_inv_dt+0(4).
          lv_month = lv_inv_dt+4(2).
          lv_day   = lv_inv_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_inv_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_inv_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa8_mmi IS NOT INITIAL.
*          DATA(lv_lrdt) = CONV string( ls_matdoc_item-yy1_aa8_mmi ).
*          lv_lr_dt = |{ lv_lrdt+6(2) }/{ lv_lrdt+4(2) }/{ lv_lrdt+2(4) }|.
*        ENDIF.

        lv_lr_dt = ls_matdoc_item-yy1_aa8_mmi.
        IF lv_chl_dt IS NOT INITIAL.
          lv_year  = lv_lr_dt+0(4).
          lv_month = lv_lr_dt+4(2).
          lv_day   = lv_lr_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_lr_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_lr_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa6_mmi IS NOT INITIAL.
*          DATA(lv_gedt) = CONV string( ls_matdoc_item-yy1_aa6_mmi ).
*          lv_ge_dt = |{ lv_gedt+6(2) }/{ lv_gedt+4(2) }/{ lv_gedt+2(4) }|.
*        ENDIF.

        lv_ge_dt = ls_matdoc_item-yy1_aa6_mmi.
        IF lv_ge_dt IS NOT INITIAL.
          lv_year  = lv_ge_dt+0(4).
          lv_month = lv_ge_dt+4(2).
          lv_day   = lv_ge_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_ge_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_ge_dt_ext = 'N/A'.
        ENDIF.

*        IF ls_matdoc_item-yy1_aa12_mmi IS NOT INITIAL.
*          DATA(LV_BOEdt) = CONV string( ls_matdoc_item-yy1_aa12_mmi ).
*          LV_BOE_dt = |{ lv_boedt+6(2) }/{ lv_boedt+4(2) }/{ lv_boedt+2(4) }|.
*        ENDIF.

        LV_BOE_dt = ls_matdoc_item-yy1_aa12_mmi.
        IF LV_BOE_dt IS NOT INITIAL.
          lv_year  = LV_BOE_dt+0(4).
          lv_month = LV_BOE_dt+4(2).
          lv_day   = LV_BOE_dt+6(2).

          CASE lv_month.
            WHEN '01'. lv_month_name = '01'.
            WHEN '02'. lv_month_name = '02'.
            WHEN '03'. lv_month_name = '03'.
            WHEN '04'. lv_month_name = '04'.
            WHEN '05'. lv_month_name = '05'.
            WHEN '06'. lv_month_name = '06'.
            WHEN '07'. lv_month_name = '07'.
            WHEN '08'. lv_month_name = '08'.
            WHEN '09'. lv_month_name = '09'.
            WHEN '10'. lv_month_name = '10'.
            WHEN '11'. lv_month_name = '11'.
            WHEN '12'. lv_month_name = '12'.
            WHEN OTHERS. lv_month_name = ''.
          ENDCASE.

          lv_BOE_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
        ELSE.
          lv_BOE_dt_ext = 'N/A'.
        ENDIF.

      ENDIF.


*    IF sy-subrc = 0.
*        l= ls_matdoc_item.
*      ELSE.
*        lv_mat_desc = 'Description not found'.
*      ENDIF.

      IF lv_mat_desc IS INITIAL.
        lv_mat_desc = 'Description not found'.
      ENDIF.

    ENDIF.

    SORT lt_matdoc BY MaterialDocumentItem ASCENDING.

    " Step 2: Fetch Storage Location

    SELECT SINGLE
       a~StorageLocation,
       b~StorageLocationName
  FROM I_MaterialDocumentItem_2 AS a
  LEFT OUTER JOIN I_StorageLocation AS b
    ON  a~StorageLocation = b~StorageLocation
   AND a~Plant            = b~Plant
 WHERE a~MaterialDocument = @io_materialdocno
 INTO @DATA(ls_sloc).

    " Step 3: Get first Purchase Order number
    READ TABLE lt_matdoc INDEX 1 INTO DATA(ls_first_item).
    IF sy-subrc = 0.
      lv_po_no = ls_first_item-PurchaseOrder.
    ENDIF.

    " Step 4: Get PO Creation Date from I_PURCHASEORDERAPI01
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE CreationDate
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @DATA(lv_po_creationdate).

*     lv_po_date = ls_matdoc_item-yy1_aa6_mmi.
      IF lv_po_creationdate IS NOT INITIAL.
        lv_year  = lv_po_creationdate+0(4).
        lv_month = lv_po_creationdate+4(2).
        lv_day   = lv_po_creationdate+6(2).

        CASE lv_month.
          WHEN '01'. lv_month_name = '01'.
          WHEN '02'. lv_month_name = '02'.
          WHEN '03'. lv_month_name = '03'.
          WHEN '04'. lv_month_name = '04'.
          WHEN '05'. lv_month_name = '05'.
          WHEN '06'. lv_month_name = '06'.
          WHEN '07'. lv_month_name = '07'.
          WHEN '08'. lv_month_name = '08'.
          WHEN '09'. lv_month_name = '09'.
          WHEN '10'. lv_month_name = '10'.
          WHEN '11'. lv_month_name = '11'.
          WHEN '12'. lv_month_name = '12'.
          WHEN OTHERS. lv_month_name = ''.
        ENDCASE.

        lv_po_dt_ext = |{ lv_day }/{ lv_month_name }/{ lv_year }|.
      ELSE.
        lv_po_dt_ext = 'N/A'.
      ENDIF.
    ELSE.
      lv_po_date = 'N/A'.
    ENDIF.

    " Step 5: Get PR Number and Manufacturer Info
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE
             a~PurchaseOrder,
             a~PurchaseOrderItem,
             a~PurchaseRequisition,
             b~manfnm,
             b~manfno,
             b~manfstat
        FROM i_purchaseorderitemapi01 AS a
        INNER JOIN zc_mat_linkage AS b
          ON a~Material = b~MaterialNo
        WHERE a~PurchaseOrder = @lv_po_no
        INTO @DATA(ls_poitem).

      IF sy-subrc = 0 AND ls_poitem IS NOT INITIAL.
        lv_pr_no = ls_poitem-PurchaseRequisition.
      ELSE.
        lv_pr_no = 'N/A'.
      ENDIF.
    ELSE.
      lv_pr_no = 'N/A'.
    ENDIF.

    IF lv_po_no IS NOT INITIAL.
      SELECT
      a~PurchaseOrder,
      a~PurchaseOrderItem,
      a~PurchaseRequisition,
      b~LastChangeDateTime
*      a~YY1_ManufacturerNoA1_PDI
      FROM i_purchaseorderitemapi01 AS a
      LEFT OUTER JOIN I_PurchaseRequisitionAPI01 AS b ON a~PurchaseRequisition = b~PurchaseRequisition
      WHERE a~PurchaseOrder = @lv_po_no
      INTO TABLE @DATA(lt_poitem).


      IF sy-subrc = 0 AND lt_poitem IS NOT INITIAL.
        READ TABLE lt_poitem INDEX 1 INTO DATA(ls_po_item).

        IF sy-subrc = 0 AND ls_po_item-PurchaseRequisition IS NOT INITIAL.
          lv_pr_no     = ls_po_item-PurchaseRequisition.
          Lv_pr_date  =  ls_po_item-LastChangeDateTime.
        ELSE.
          lv_pr_no = 'N/A'.
        ENDIF.

      ELSE.
        lv_pr_no = 'N/A'.
      ENDIF.

    ELSE.
      lv_pr_no = 'N/A'.
    ENDIF.
    DATA(lv_pr_raw) = CONV string( ls_po_item-LastChangeDateTime ).

    IF lv_pr_raw IS NOT INITIAL.

      "Extract only YYYYMMDD (first 8 chars)
      lv_pr_date = lv_pr_raw+0(8).
    ELSE.
      lv_pr_date = 'N/A'.
    ENDIF.

    IF ls_po_item-LastChangeDateTime IS NOT INITIAL.
      DATA(LV_prdt) = CONV string( ls_po_item-LastChangeDateTime ).
      lv_pr_raw = |{ lv_prdt+6(2) }-{ lv_prdt+4(2) }-{ lv_prdt+2(2) }|.
    ENDIF.




*
*   IF lt_poitem IS NOT INITIAL.
*
*
**  SELECT
**    ManfNm,
**    ManfNo,
**    ManfAddr,
**    ManfStat
**    FROM zi_manuf_dtls_n
**    FOR ALL ENTRIES IN @lt_poitem
**    WHERE ManfNo = @lt_poitem-
**    INTO TABLE @DATA(it_zc).
**
**  READ TABLE it_zc INDEX 1 INTO DATA(ls_zc).
**
**  IF sy-subrc = 0.
**
**    lv_manf_nm   = ls_zc-ManfNm.
**    lv_manf_stat = ls_zc-ManfStat.
**    lv_manf_addr = ls_zc-ManfAddr.
**
**    "----------------------------
**    "  A → Approved
**    "  B → Blocked
**    "----------------------------
*
*
*  ENDIF.
*

    READ TABLE lt_matdoc INTO DATA(wa_mat_item) INDEX 1.
    SELECT SINGLE *
      FROM I_PurchaseOrderItemAPI01
      WHERE purchaseorder = @wa_mat_item-PurchaseOrder
      INTO @DATA(wa_po_item_pant).

    SELECT SINGLE *
         FROM zc_manuf_dtls_n
        WHERE ManfNo = @wa_po_item_pant-YY1_ManufacturerNo1_PDI
         INTO @DATA(wa_po_manuf).

    DATA: lt_manf TYPE RANGE OF zi_manuf_dtls_n-ManfNo.
    LOOP AT lt_poitem INTO DATA(ls_po).
*  IF ls_po-YY1_ManufacturerNoA1_PDI IS NOT INITIAL.
*
*    APPEND VALUE #(
*      sign   = 'I'
*      option = 'EQ'
*      low    = CONV zi_manuf_dtls_n-ManfNo( ls_po-YY1_ManufacturerNoA1_PDI )
*    ) TO lt_manf.
*
*  ENDIF.
    ENDLOOP.

*DELETE ADJACENT DUPLICATES FROM lt_manf COMPARING low.
*SELECT
*    ManfNm,
*    ManfNo,
*    ManfAddr,
*    ManfStat
*  FROM zi_manuf_dtls_n
*  WHERE ManfNo IN @lt_manf
*  INTO TABLE @DATA(it_zc).
*
* READ TABLE it_zc INDEX 1 INTO DATA(ls_zc).
*
*  IF sy-subrc = 0.
*
*    lv_manf_nm   = ls_zc-ManfNm.
*    lv_manf_stat = ls_zc-ManfStat.
*    lv_manf_addr = ls_zc-ManfAddr.
*
*  ENDIF.



*ENDIF.

    DATA: lv_po_type     TYPE i_purchaseorderapi01-purchaseordertype,
          lv_po_category TYPE string.

    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE PurchaseOrderType
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @lv_po_type.

      IF sy-subrc = 0 AND lv_po_type IS NOT INITIAL.
        CASE lv_po_type.
          WHEN 'ZDOM'.
            lv_po_category = 'DOMESTIC'.
          WHEN 'ZIMP'.
            lv_po_category = 'IMPORT'.
          WHEN OTHERS.
            lv_po_category = 'OTHER'.
        ENDCASE.
      ELSE.
        lv_po_category = 'NOT FOUND'.
      ENDIF.
    ELSE.
      lv_po_category = 'N/A'.
    ENDIF.


    DATA: lv_supplier      TYPE i_purchaseorderapi01-supplier,
          lv_vendor_name   TYPE i_supplier-organizationbpname1,
          ls_supplier_addr TYPE i_supplier.


    TYPES: BEGIN OF ty_header,
             vendor        TYPE string,
             vendoraddress TYPE string,
           END OF ty_header.

    DATA: gs_header TYPE ty_header.


    " 1. Get Supplier from PO
    IF lv_po_no IS NOT INITIAL.
      SELECT SINGLE Supplier
        FROM i_purchaseorderapi01
        WHERE PurchaseOrder = @lv_po_no
        INTO @lv_supplier.
    ENDIF.

    " 2. Get Supplier Name
    IF sy-subrc = 0 AND lv_supplier IS NOT INITIAL.
      SELECT SINGLE OrganizationBPName1
        FROM i_supplier
        WHERE Supplier = @lv_supplier
        INTO @lv_vendor_name.

      IF sy-subrc = 0.
        gs_header-vendor = lv_vendor_name.
      ELSE.
        gs_header-vendor = 'N/A'.
      ENDIF.
    ELSE.
      gs_header-vendor = 'N/A'.
    ENDIF.

    " 3. Get Supplier Address (Street, PostalCode, City)
    IF lv_supplier IS NOT INITIAL.
      SELECT SINGLE
             StreetName,
             CityName,
             PostalCode
        FROM i_supplier
        WHERE Supplier = @lv_supplier
        INTO CORRESPONDING FIELDS OF @ls_supplier_addr.

      IF sy-subrc = 0.
        gs_header-vendoraddress =
          |{ ls_supplier_addr-streetname }, { ls_supplier_addr-postalcode } { ls_supplier_addr-cityname }|.
        CONDENSE gs_header-vendoraddress.
      ELSE.
        gs_header-vendoraddress = 'N/A'.
      ENDIF.
    ELSE.
      gs_header-vendoraddress = 'N/A'.
    ENDIF.

*    DATA(lv_clean) = |{ ls_matdoc_item-yy1_aa9_mmi DECIMALS = 0 }|.
*    DATA(ls_clean) = |{ ls_matdoc_item-yy1_aa7_mmi DECIMALS = 0 }|.

    " Step 6: Build header XML
    DATA(lv_header) =
       |<form1>| &&
       |   <Subform1>| &&
       |      <MFG_NAME>{ wa_po_manuf-ManfNm  }</MFG_NAME>| &&
       |      <ADDRESS>{ wa_po_manuf-ManfAddr }</ADDRESS>| &&
       |       <STATUS>{ wa_po_manuf-ManfStat }</STATUS>| &&
       |   </Subform1>| &&
       |   <Subform2>| &&
       |      <SUPPLIER>{ lv_vendor_name }</SUPPLIER>| &&
       |      <ADDR>{ gs_header-vendoraddress }</ADDR>| &&
       |      <GRN_NO>{ io_materialdocno }</GRN_NO>| &&
       |      <GRN_DATE>{ lv_grn_date_text }</GRN_DATE>| &&
       |       <TYPE>{ lv_po_category }</TYPE>| &&
       |       </Subform2>| &&
       |       <Subform3>| &&
       |      <PO_NO>{ lv_po_no }</PO_NO>| &&
       |      <STORAGE_LOC>{ ls_sloc-StorageLocation },{ ls_sloc-StorageLocationName }</STORAGE_LOC>| &&
       |      <PO_DT>{ lv_po_dt_ext }</PO_DT>| &&
       |      <PR_NO>{ lv_pr_no }</PR_NO>| &&   " <-- Added PR Number
       |      <PR_DT>{ lv_pr_raw }</PR_DT>| &&                " Optional: can populate later
       |      <GE_NO>{ ls_matdoc_item-yy1_aa5_mmi }</GE_NO>| &&
       |      <GE_DT>{ lv_ge_dt_ext }</GE_DT>| &&
       |      <CHL_NO>{ ls_matdoc_item-yy1_aa3_mmi }</CHL_NO>| &&
       |      <CHL_DT>{ lv_chl_dt_ext }</CHL_DT>| &&
       |      <LR_NO>{ ls_matdoc_item-yy1_aa7_mmi }</LR_NO>| &&
       |      <LR_DT>{ lv_lr_dt_ext }</LR_DT>| &&
       |      <INVOICE>{ ls_matdoc_item-yy1_aa9_mmi }</INVOICE>| &&
       |      <INVOICE_DT>{ lv_inv_dt_ext }</INVOICE_DT>| &&
       |      <BOE_NO>{ ls_matdoc_item-yy1_aa11_mmi }</BOE_NO>| &&
       |      <BOE_DT>{ lv_BOE_dt_ext }</BOE_DT>| &&
       |      <TRANSPORTER>{ ls_matdoc_item-yy1_aa13_mmi  }</TRANSPORTER>| &&
       |      <VEHICLE_NO>{ ls_matdoc_item-yy1_aa14_mmi  }</VEHICLE_NO>| &&
       |   </Subform3>|.

    DATA(lv_items) = ``.

    " Step 5: Loop through all items
    LOOP AT lt_matdoc INTO DATA(ls_item).

      CLEAR: lv_batchbysupplier, lv_mfg_month, lv_exp_month,
             lv_mat_desc,lv_sup_bt,lv_batch_code.
*             lv_rt_dt.

      "-----------------------------------------------------
      " Supplier Batch (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-Batch IS NOT INITIAL.
        SELECT SINGLE BatchBySupplier
          FROM I_Batch
          WHERE Batch    = @ls_item-Batch
            AND Material = @ls_item-Material
            AND Plant    = @ls_item-Plant
          INTO @lv_batchbysupplier.
      ENDIF.

*
      IF lv_batchbysupplier IS INITIAL.
        lv_supplier_btch = ls_item-YY1_SupplierBatch1_MMI.
      ELSE.
        lv_supplier_btch = lv_batchbysupplier.
      ENDIF.

      "-----------------------------------------------------
      " Material Description (Existing Logic)
      "-----------------------------------------------------
      SELECT
      a~ProductName,
      b~product,
      b~ConsumptionTaxCtrlCode,
      d~StandardPricePrevYear
        FROM I_ProductText AS a
        INNER JOIN i_productplantintltrd AS b
        ON a~Product = b~Product
         INNER JOIN I_ProductValuationAccounting_2 AS d
         ON b~product = d~Product
        WHERE a~Product  = @ls_item-Material
          AND Language = @sy-langu
        INTO TABLE @DATA(ls_mat_desc).

      READ TABLE ls_mat_desc INDEX 1 INTO DATA(ln_mat_desc).
      IF sy-subrc = 0.
        lv_hsn_cd = ln_mat_desc-ConsumptionTaxCtrlCode.
        lv_itm_nm  = ln_mat_desc-ProductName.
      ENDIF.




      "-----------------------------------------------------
      " NEW LOGIC FOR BATCH CODE (From I_MATERIALDOCUMENTITEM_2)
      "-----------------------------------------------------
*      SELECT SINGLE  mdi~Batch,insp~InspectionLot
*  FROM I_InspectionLot AS insp
* WHERE insp~MaterialDocument     = @mdi-MaterialDocument
*   AND insp~MaterialDocumentItem = @mdi-MaterialDocumentItem
*   AND insp~Batch                = @mdi-Batch
*  INTO @lv_insp_lt.
*
*      SELECT SINGLE InspectionLot
*      FROM I_InspectionLot
*      INTO @lv_insp_lt.
*
*
*      SELECT distinct
*        mdi~Batch,
*        insp~InspectionLot
*   FROM I_MaterialDocumentItem_2 AS mdi
*   LEFT JOIN I_InspectionLot AS insp
*     ON insp~MaterialDocument     = mdi~MaterialDocument
*    AND insp~MaterialDocumentItem = mdi~MaterialDocumentItem
*   WHERE mdi~MaterialDocument     = insp~MaterialDocument
*     AND mdi~MaterialDocumentItem = insp~MaterialDocumentItem
*    INTO TABLE @DATA(ls_join).
*
*   READ TABLE ls_join INDEX 1 INTO DATA(ln_join).
*   IF sy-subrc = 0.
*      lv_batch_code = ln_join-Batch.
*      lv_insp_lt  = ln_join-InspectionLot.
*    ENDIF.
*        IF lv_batch_code IS INITIAL.
*          lv_batch_code = ''.
*        ENDIF.
*

      """



      "-----------------------------------------------------
      " Manufacture Month (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-ManufactureDate IS NOT INITIAL.
        DATA(lv_mfg_year)   = ls_item-ManufactureDate(4).
        DATA(lv_mfg_monthn) = ls_item-ManufactureDate+4(2).

        lv_mfg_month = SWITCH #( lv_mfg_monthn
          WHEN '01' THEN |Jan { lv_mfg_year }|
          WHEN '02' THEN |Feb { lv_mfg_year }|
          WHEN '03' THEN |Mar { lv_mfg_year }|
          WHEN '04' THEN |Apr { lv_mfg_year }|
          WHEN '05' THEN |May { lv_mfg_year }|
          WHEN '06' THEN |Jun { lv_mfg_year }|
          WHEN '07' THEN |Jul { lv_mfg_year }|
          WHEN '08' THEN |Aug { lv_mfg_year }|
          WHEN '09' THEN |Sep { lv_mfg_year }|
          WHEN '10' THEN |Oct { lv_mfg_year }|
          WHEN '11' THEN |Nov { lv_mfg_year }|
          WHEN '12' THEN |Dec { lv_mfg_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_mfg_month = 'N/A'.
      ENDIF.


      "-----------------------------------------------------
      " Expiry Month (Existing Logic)
      "-----------------------------------------------------
      IF ls_item-ShelfLifeExpirationDate IS NOT INITIAL.
        DATA(lv_exp_year)   = ls_item-ShelfLifeExpirationDate(4).
        DATA(lv_exp_monthn) = ls_item-ShelfLifeExpirationDate+4(2).

        lv_exp_month = SWITCH #( lv_exp_monthn
          WHEN '01' THEN |Jan { lv_exp_year }|
          WHEN '02' THEN |Feb { lv_exp_year }|
          WHEN '03' THEN |Mar { lv_exp_year }|
          WHEN '04' THEN |Apr { lv_exp_year }|
          WHEN '05' THEN |May { lv_exp_year }|
          WHEN '06' THEN |Jun { lv_exp_year }|
          WHEN '07' THEN |Jul { lv_exp_year }|
          WHEN '08' THEN |Aug { lv_exp_year }|
          WHEN '09' THEN |Sep { lv_exp_year }|
          WHEN '10' THEN |Oct { lv_exp_year }|
          WHEN '11' THEN |Nov { lv_exp_year }|
          WHEN '12' THEN |Dec { lv_exp_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_exp_month = 'N/A'.
      ENDIF.


      "-----------------------------------------------------
      " Retest Date (Existing Logic)
      "-----------------------------------------------------
*      READ TABLE lt_matdoc_item INTO DATA(ls_retest)
*           WITH KEY materialdocument = ls_item-MaterialDocument.

*      select SINGLE *
*      from I_MaterialDocumentItemTP
*      WHERE materialdocumentITEM = @ls_item-MaterialDocumentItem
*      into  @data(ls_matdoc_rt).

      IF ls_item-yy1_aa15_mmi IS NOT INITIAL.

        DATA(lv_rt_year)   = ls_item-yy1_aa15_mmi(4).
        DATA(lv_rt_monthn) = ls_item-yy1_aa15_mmi+4(2).

        lv_rt_dt = SWITCH #( lv_rt_monthn
          WHEN '01' THEN |Jan { lv_rt_year }|
          WHEN '02' THEN |Feb { lv_rt_year }|
          WHEN '03' THEN |Mar { lv_rt_year }|
          WHEN '04' THEN |Apr { lv_rt_year }|
          WHEN '05' THEN |May { lv_rt_year }|
          WHEN '06' THEN |Jun { lv_rt_year }|
          WHEN '07' THEN |Jul { lv_rt_year }|
          WHEN '08' THEN |Aug { lv_rt_year }|
          WHEN '09' THEN |Sep { lv_rt_year }|
          WHEN '10' THEN |Oct { lv_rt_year }|
          WHEN '11' THEN |Nov { lv_rt_year }|
          WHEN '12' THEN |Dec { lv_rt_year }|
          ELSE 'N/A' ).
      ELSE.
        lv_rt_dt = 'N/A'.
      ENDIF.





      "-----------------------------------------------------
      " Formatting
      "-----------------------------------------------------
      ls_item-Material = |{ ls_item-Material ALPHA = OUT }|.
      lv_item          = |{ ls_item-MaterialDocumentItem ALPHA = OUT }|.
      ls_matdoc_item-yy1_aa1_mmi = |{ ls_matdoc_item-yy1_aa1_mmi ALPHA = OUT }|.
      ls_matdoc_item-yy1_aa2_mmi = |{ ls_matdoc_item-yy1_aa2_mmi ALPHA = OUT }|.

      SHIFT ls_item-yy1_aa1_mmi LEFT DELETING LEADING '0'.
      SHIFT ls_item-yy1_aa2_mmi LEFT DELETING LEADING '0'.

      "-----------------------------------------------------
      " XML Row (BATCH_CODE added)
      "-----------------------------------------------------
      lv_items &&=
       |   <Subform4>| &&
       |<Table1>| &&
       |<HeaderRow/>| &&
        |<Row1>| &&
        |   <SRNO>{ lv_item }</SRNO>| &&
        |   <SUPPLIER_NO>{ lv_supplier_btch }</SUPPLIER_NO>| &&
        |   <INSPECTION_NO>{ lS_ITEM-InspectionLot }</INSPECTION_NO>| &&
        |   <BATCH_CODE>{  ls_item-Batch }</BATCH_CODE>| &&   " <-- ADDED HERE
        |   <UNIT>{ ls_item-EntryUnit }</UNIT>| &&
        |   <PKG>{ ls_item-yy1_aa2_mmi }</PKG>| &&
        |   <NOS>{ ls_ITEM-yy1_aa1_mmi }</NOS>| &&
        |   <QUANTITY>{ ls_item-QuantityInEntryUnit }</QUANTITY>| &&
        |   <MFG_MONTH>{ lv_mfg_month }</MFG_MONTH>| &&
        |   <EXPIRY_MONTH>{ lv_exp_month }</EXPIRY_MONTH>| &&
        |   <RETEST_MONTH>{ lv_rt_dt }</RETEST_MONTH>| &&
        |</Row1>| &&
        |</Table1>| &&
       |      <Subform7>| &&
       |      <Subform7>| &&
       |         <TOTAL_QUANTITY>{ lv_total_qty }</TOTAL_QUANTITY>| &&
       |      </Subform7>| &&
       |      </Subform7>| &&
       |   <ITEM_CODE>{ ls_item-Material }</ITEM_CODE>| &&
       |   <ITEM_NAME>{ lv_itm_nm }</ITEM_NAME>| &&
       |   <HSN_CD>{ lv_hsn_cd }</HSN_CD>| &&
       |   </Subform4>|.

      lv_total_qty += ls_item-QuantityInEntryUnit.

    ENDLOOP.




    " Step 6: Footer XML
    DATA(lv_footer) =
       |      <Subform8>| &&
       |         <WAREHOUSE_DES></WAREHOUSE_DES>| &&
       |         <GRN_PREPARED></GRN_PREPARED>| &&
       |         <APPROVED_QC></APPROVED_QC>| &&
       |         <MATERIAL_REC></MATERIAL_REC>| &&
       |         <SAMPLED_BY></SAMPLED_BY>| &&
       |         <SAMPLED_QTY></SAMPLED_QTY>| &&
       |         <CHECKED_BY_QC></CHECKED_BY_QC>| &&
       |         <REASON_REJ></REASON_REJ>| &&
       |      </Subform8>| &&
       |</form1>|.

    rv_xml = |{ lv_header } { lv_items } { lv_footer }|.

  ENDMETHOD.
ENDCLASS.
