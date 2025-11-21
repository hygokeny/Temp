REPORT zbdc_venx.

DATA lo_bdc  TYPE REF TO zcl_batch_input.
DATA lt_msgs TYPE TABLE OF bapiret2.
DATA ls_msg  TYPE bapiret2.

CREATE OBJECT lo_bdc.

"==============================================================
" Tela 1 - Tcode ZINT_VENX Dynpro 1000
"==============================================================
lo_bdc->add_dynpro(
  iv_program = 'ZINT_VENX'
  iv_dynpro  = '1000' ).

lo_bdc->add_field(
  iv_name  = 'BDC_CURSOR'
  iv_value = 'P_ROLE' ).

lo_bdc->add_field(
  iv_name  = 'BDC_OKCODE'
  iv_value = '=ONLI' ).

lo_bdc->add_field(
  iv_name  = 'P_ROLE'
  iv_value = 'abd' ).

"==============================================================
" Tela 2 - SAPMSSY0 Dynpro 0120
"==============================================================
lo_bdc->add_dynpro(
  iv_program = 'SAPMSSY0'
  iv_dynpro  = '0120' ).

lo_bdc->add_field(
  iv_name  = 'BDC_OKCODE'
  iv_value = '=DBAC' ).

"==============================================================
" Tela 3 - SAPLSUPRN Dynpro 0300
"==============================================================
lo_bdc->add_dynpro(
  iv_program = 'SAPLSUPRN'
  iv_dynpro  = '0300' ).

lo_bdc->add_field(
  iv_name  = 'BDC_OKCODE'
  iv_value = '=GEN1' ).

"==============================================================
" Tela 4 - SAPLSPO1 Dynpro 0500
"==============================================================
lo_bdc->add_dynpro(
  iv_program = 'SAPLSPO1'
  iv_dynpro  = '0500' ).

lo_bdc->add_field(
  iv_name  = 'BDC_OKCODE'
  iv_value = '=OPT1' ).

lo_bdc->add_field(
  iv_name  = 'BDC_SUBSCR'
  iv_value = 'SAPLSPO1                                0501SUBSCREEN' ).

"==============================================================
" Executa o Batch Input
"==============================================================
lo_bdc->execute(
  EXPORTING
    iv_tcode = 'ZINT_VENX'
  IMPORTING
    et_msgs  = lt_msgs ).

"==============================================================
" Log
"==============================================================
LOOP AT lt_msgs INTO ls_msg.
  WRITE: / ls_msg-type, ls_msg-id, ls_msg-number, ls_msg-message.
ENDLOOP.
