CLASS lsc_zi_grn_print DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zi_grn_print IMPLEMENTATION.

  METHOD save_modified.
    DATA lo_pfd TYPE REF TO zcl_grn_print_n.

    CREATE OBJECT lo_pfd.

    IF update-zi_comm_doc IS NOT INITIAL.

      LOOP AT update-zi_comm_doc INTO DATA(ls_data).

        DATA(new) = NEW zcl_bg_process_grn_print( iv_bill            = ls_data-MaterialDocument
                                                  iv_m_ind           = ls_data-m_ind
                                                  iv_postigdt        = ls_data-PostingDate
                                                  iv_MaterialdocYear = ls_data-MaterialDocumentYear ).

        DATA background_process TYPE REF TO if_bgmc_process_single_op.

        TRY.

            background_process = cl_bgmc_process_factory=>get_default( )->create( ).

            background_process->set_operation_tx_uncontrolled( new ).

            background_process->save_for_execution( ).

          CATCH cx_bgmc INTO DATA(exception).
            "handle exception
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_ZI_GRN_PRINT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_comm_doc RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_comm_doc RESULT result.

    METHODS zprint FOR MODIFY
      IMPORTING keys FOR ACTION zi_comm_doc~zprint RESULT result.

ENDCLASS.

CLASS lhc_ZI_GRN_PRINT IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD zprint.
    DATA ls_pdf TYPE REF TO zcl_grn_print_n.

    CREATE OBJECT ls_pdf.

    READ ENTITIES OF zi_grn_print  IN LOCAL MODE
           ENTITY zi_comm_doc
          ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(lt_result).

    LOOP AT lt_result INTO DATA(lw_result).

      DATA : update_lines TYPE TABLE FOR UPDATE zi_grn_print,
             update_line  TYPE STRUCTURE FOR UPDATE zi_grn_print.

      update_line-%tky                   = lw_result-%tky.
      update_line-base64_3                = 'A'.

      IF update_line-base64_3 IS NOT INITIAL.

        APPEND update_line TO update_lines.

        MODIFY ENTITIES OF zi_grn_print IN LOCAL MODE
         ENTITY zi_comm_doc
           UPDATE
           FIELDS ( base64_3 )
           WITH update_lines
         REPORTED reported
         FAILED failed
         MAPPED mapped.

        READ ENTITIES OF zi_grn_print IN LOCAL MODE  ENTITY zi_comm_doc
            ALL FIELDS WITH CORRESPONDING #( lt_result ) RESULT DATA(lt_final).

        result =  VALUE #( FOR  lw_final IN  lt_final ( %tky = lw_final-%tky
         %param = lw_final  )  ).

        APPEND VALUE #( %tky = keys[ 1 ]-%tky
                        %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-success
                        text = 'PDF Generated!, Please Wait for 30 Sec' )
                         ) TO reported-zi_comm_doc.

      ELSE.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
