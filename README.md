REPORT zbdc_venx_auto.

DATA lt_steps TYPE STANDARD TABLE OF zcl_bdc_utils=>ty_bdc_step.
DATA lo_bdc   TYPE REF TO zcl_batch_input.
DATA lt_msgs  TYPE TABLE OF bapiret2.
DATA ls_msg   TYPE bapiret2.

" 1) Obter passos
lt_steps = zcl_bdc_utils=>get_steps( ).

" 2) Criar objeto BDC
CREATE OBJECT lo_bdc.

" 3) Montar BDCDATA internamente
lo_bdc->build_from_table( lt_steps ).

" 4) Executar
lo_bdc->execute(
  EXPORTING iv_tcode = 'ZINT_VENX'
  IMPORTING et_msgs  = lt_msgs ).

" 5) Log
LOOP AT lt_msgs INTO ls_msg.
  WRITE: / ls_msg-type, ls_msg-id, ls_msg-number, ls_msg-message.
ENDLOOP.
