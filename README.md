  CLEAR mt_bdc.

  LOOP AT it_steps INTO DATA(ls_step).

    IF ls_step-is_dynpro = abap_true.
      add_dynpro(
        iv_program = ls_step-program
        iv_dynpro  = ls_step-dynpro
      ).

    ELSEIF ls_step-name IS NOT INITIAL.
      add_field(
        iv_name  = ls_step-name
        iv_value = ls_step-value
      ).
    ENDIF.

  ENDLOOP.

  methods BUILD_FROM_STEPS
  importing
    it_steps type TT_BDC_STEP.


    " 1) Obter passos do SHDB
DATA(lt_steps) = get_steps( ).

" 2) Montar BDCDATA
build_from_steps( lt_steps ).

" 3) Executar
DATA lt_msgs TYPE bapiret2_t.
execute(
  EXPORTING iv_tcode = 'ZINT_VENX'
  IMPORTING et_msgs  = lt_msgs
).

" 4) Log
LOOP AT lt_msgs INTO DATA(ls_msg).
  WRITE:/ ls_msg-type, ls_msg-id, ls_msg-number, ls_msg-message.
ENDLOOP.
