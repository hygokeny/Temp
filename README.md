CLASS zcl_batch_input DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: ty_bdc_msg TYPE bdcmsgcoll.

    METHODS:
      constructor,

      add_dynpro
        IMPORTING iv_program TYPE syrepid
                  iv_dynpro  TYPE sydynnr,

      add_field
        IMPORTING iv_name  TYPE bdc_fnam
                  iv_value TYPE bdc_fval,

      execute
        IMPORTING iv_tcode TYPE tcode
        EXPORTING et_msgs  TYPE TABLE OF bapiret2,

      " Novo método: monta BDC a partir de tabela
      build_from_table
        IMPORTING it_steps TYPE STANDARD TABLE
        RETURNING VALUE(ro_self) TYPE REF TO zcl_batch_input.

  PRIVATE SECTION.
    DATA mt_bdc TYPE TABLE OF bdcdata.

ENDCLASS.



CLASS zcl_batch_input IMPLEMENTATION.

  METHOD constructor.
    CLEAR mt_bdc.
  ENDMETHOD.


  METHOD add_dynpro.
    DATA ls_bdc TYPE bdcdata.

    CLEAR ls_bdc.
    ls_bdc-program  = iv_program.
    ls_bdc-dynpro   = iv_dynpro.
    ls_bdc-dynbegin = abap_true.
    APPEND ls_bdc TO mt_bdc.
  ENDMETHOD.


  METHOD add_field.
    DATA ls_bdc TYPE bdcdata.

    CLEAR ls_bdc.
    ls_bdc-fnam = iv_name.
    ls_bdc-fval = iv_value.
    APPEND ls_bdc TO mt_bdc.
  ENDMETHOD.


  METHOD execute.
    DATA lt_msgs_raw TYPE TABLE OF bdcmsgcoll.
    DATA ls_msg_raw  TYPE bdcmsgcoll.
    DATA ls_bapi_ret TYPE bapiret2.

    CALL TRANSACTION iv_tcode
      USING    mt_bdc
      MODE     'N'
      UPDATE   'S'
      MESSAGES INTO lt_msgs_raw.

    LOOP AT lt_msgs_raw INTO ls_msg_raw.
      CLEAR ls_bapi_ret.
      ls_bapi_ret-type       = ls_msg_raw-msgtyp.
      ls_bapi_ret-id         = ls_msg_raw-msgid.
      ls_bapi_ret-number     = ls_msg_raw-msgno.
      ls_bapi_ret-message_v1 = ls_msg_raw-msgv1.
      ls_bapi_ret-message_v2 = ls_msg_raw-msgv2.
      ls_bapi_ret-message_v3 = ls_msg_raw-msgv3.
      ls_bapi_ret-message_v4 = ls_msg_raw-msgv4.

      MESSAGE ID ls_msg_raw-msgid TYPE ls_msg_raw-msgtyp
              NUMBER ls_msg_raw-msgno
              WITH ls_msg_raw-msgv1 ls_msg_raw-msgv2 ls_msg_raw-msgv3 ls_msg_raw-msgv4
              INTO ls_bapi_ret-message.

      APPEND ls_bapi_ret TO et_msgs.
    ENDLOOP.
  ENDMETHOD.



  "===========================================================
  " Novo método: recebe tabela com passos e monta o BDC sozinho
  " Campos esperados:
  " PROGRAM | DYNPRO | NAME | VALUE | IS_DYNPRO (X para tela)
  "===========================================================
  METHOD build_from_table.

    DATA ls_step TYPE LINE OF it_steps.

    LOOP AT it_steps INTO ls_step.

      IF ls_step-is_dynpro = abap_true.
        me->add_dynpro(
          iv_program = ls_step-program
          iv_dynpro  = ls_step-dynpro ).
      ENDIF.

      IF ls_step-name IS NOT INITIAL.
        me->add_field(
          iv_name  = ls_step-name
          iv_value = ls_step-value ).
      ENDIF.

    ENDLOOP.

    ro_self = me.

  ENDMETHOD.

ENDCLASS.


**************************************************************


TYPES: BEGIN OF ty_bdc_step,
         program  TYPE syrepid,
         dynpro   TYPE sydynnr,
         is_dynpro TYPE abap_bool,
         name     TYPE bdc_fnam,
         value    TYPE bdc_fval,
       END OF ty_bdc_step.

DATA lt_steps TYPE STANDARD TABLE OF ty_bdc_step.
DATA ls_step TYPE ty_bdc_step.


"----------------------"
" Tela 1 ZINT_VENX 1000"
"----------------------"
ls_step-program  = 'ZINT_VENX'.
ls_step-dynpro   = '1000'.
ls_step-is_dynpro = abap_true.
CLEAR ls_step-name. CLEAR ls_step-value.
APPEND ls_step TO lt_steps.

ls_step-name  = 'BDC_CURSOR'. ls_step-value = 'P_ROLE'.  ls_step-is_dynpro = abap_false. APPEND ls_step TO lt_steps.
ls_step-name  = 'BDC_OKCODE'. ls_step-value = '=ONLI'.   APPEND ls_step TO lt_steps.
ls_step-name  = 'P_ROLE'.     ls_step-value = 'abd'.     APPEND ls_step TO lt_steps.


"----------------------"
" Tela 2 SAPMSSY0 0120"
"----------------------"
ls_step-program  = 'SAPMSSY0'.
ls_step-dynpro   = '0120'.
ls_step-is_dynpro = abap_true.
CLEAR ls_step-name. CLEAR ls_step-value.
APPEND ls_step TO lt_steps.

ls_step-name  = 'BDC_OKCODE'. ls_step-value = '=DBAC'. ls_step-is_dynpro = abap_false. APPEND ls_step TO lt_steps.


"----------------------"
" Tela 3 SAPLSUPRN 0300"
"----------------------"
ls_step-program  = 'SAPLSUPRN'.
ls_step-dynpro   = '0300'.
ls_step-is_dynpro = abap_true.
CLEAR ls_step-name. CLEAR ls_step-value.
APPEND ls_step TO lt_steps.

ls_step-name  = 'BDC_OKCODE'. ls_step-value = '=GEN1'. ls_step-is_dynpro = abap_false. APPEND ls_step TO lt_steps.


"----------------------"
" Tela 4 SAPLSPO1 0500"
"----------------------"
ls_step-program  = 'SAPLSPO1'.
ls_step-dynpro   = '0500'.
ls_step-is_dynpro = abap_true.
CLEAR ls_step-name. CLEAR ls_step-value.
APPEND ls_step TO lt_steps.

ls_step-name  = 'BDC_OKCODE'. ls_step-value = '=OPT1'. APPEND ls_step TO lt_steps.
ls_step-name  = 'BDC_SUBSCR'. ls_step-value = 'SAPLSPO1                                0501SUBSCREEN'. APPEND ls_step TO lt_steps.



********************************************





REPORT zbdc_venx_auto.

DATA lo_bdc  TYPE REF TO zcl_batch_input.
DATA lt_msgs TYPE TABLE OF bapiret2.
DATA ls_msg  TYPE bapiret2.

DATA lt_steps TYPE STANDARD TABLE OF ty_bdc_step.

" Aqui você insere o bloco de criação da tabela lt_steps
" (o que eu montei acima com seus dados)

CREATE OBJECT lo_bdc.

lo_bdc->build_from_table( lt_steps )->execute(
  EXPORTING iv_tcode = 'ZINT_VENX'
  IMPORTING et_msgs  = lt_msgs ).

LOOP AT lt_msgs INTO ls_msg.
  WRITE: / ls_msg-type, ls_msg-id, ls_msg-number, ls_msg-message.
ENDLOOP.


