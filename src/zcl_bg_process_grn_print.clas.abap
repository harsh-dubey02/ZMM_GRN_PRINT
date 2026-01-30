CLASS zcl_bg_process_grn_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_bgmc_operation .
    INTERFACES if_bgmc_op_single_tx_uncontr .
    INTERFACES if_serializable_object .

    METHODS constructor
      IMPORTING
        iv_bill            TYPE I_MaterialDocumentHeader_2-MaterialDocument
        iv_m_ind           TYPE abap_boolean
        iv_postigdt        TYPE I_MaterialDocumentHeader_2-PostingDate
        iv_materialdocyear TYPE I_MaterialDocumentHeader_2-MaterialDocumentYear.

  PROTECTED SECTION.
    DATA: im_bill            TYPE I_MaterialDocumentHeader_2-MaterialDocument,
          im_postigdt        TYPE I_MaterialDocumentHeader_2-PostingDate,
          im_year            TYPE I_MaterialDocumentHeader_2-MaterialDocumentYear,
          im_ind             TYPE abap_boolean,
          im_materialdocyear TYPE I_MaterialDocumentHeader_2-MaterialDocumentYear.

    METHODS modify
      RAISING cx_bgmc_operation.

ENDCLASS.



CLASS ZCL_BG_PROCESS_GRN_PRINT IMPLEMENTATION.


  METHOD constructor.
    im_bill     = iv_bill.
    im_ind      = iv_m_ind.
    im_year     = iv_materialdocyear.
    im_postigdt = iv_postigdt.
    im_materialdocyear = iv_materialdocyear.
  ENDMETHOD.


  METHOD if_bgmc_op_single_tx_uncontr~execute.
    modify( ).
  ENDMETHOD.


  METHOD modify.

    DATA: wa_data TYPE ztb_grn_print,
          lo_pfd  TYPE REF TO zcl_grn_print_n.

    CREATE OBJECT lo_pfd.

    lo_pfd->get_pdf_64(
      EXPORTING io_materialdocno = im_bill
     io_materialdocdat  = im_postigdt
     io_materialdocyear = im_year
*     io_storagelocation = im_storageloc
      RECEIVING pdf_64           = DATA(pdf_64)
    ).

    wa_data-materialdocument     = im_bill.
    wa_data-base64_3             = pdf_64.
    wa_data-m_ind                = im_ind.
    wa_data-materialdocumentyear = im_year.
    wa_data-postingdate          = im_postigdt.

    MODIFY ztb_grn_print FROM @wa_data.

  ENDMETHOD.
ENDCLASS.
