    TYPES: BEGIN OF ty_bdc_step,
             program   TYPE syrepid,
             dynpro    TYPE sydynnr,
             is_dynpro TYPE abap_bool,
             name      TYPE bdc_fnam,
             value     TYPE bdc_fval,
           END OF ty_bdc_step.

    CLASS-METHODS get_steps
      RETURNING VALUE(rt_steps) TYPE STANDARD TABLE OF ty_bdc_step.


      METHOD get_steps.

    DATA ls_step TYPE ty_bdc_step.

    "----------------------"
    " Tela 1 ZINT_VENX 1000
    "----------------------
    ls_step-program   = 'ZINT_VENX'.
    ls_step-dynpro    = '1000'.
    ls_step-is_dynpro = abap_true.
    CLEAR ls_step-name. CLEAR ls_step-value.
    APPEND ls_step TO rt_steps.

    ls_step-is_dynpro = abap_false.

    ls_step-name  = 'BDC_CURSOR'.
    ls_step-value = 'P_ROLE'.
    APPEND ls_step TO rt_steps.

    ls_step-name  = 'BDC_OKCODE'.
    ls_step-value = '=ONLI'.
    APPEND ls_step TO rt_steps.

    ls_step-name  = 'P_ROLE'.
    ls_step-value = 'abd'.
    APPEND ls_step TO rt_steps.


    "----------------------"
    " Tela 2 SAPMSSY0 0120
    "----------------------
    ls_step-program   = 'SAPMSSY0'.
    ls_step-dynpro    = '0120'.
    ls_step-is_dynpro = abap_true.
    CLEAR ls_step-name. CLEAR ls_step-value.
    APPEND ls_step TO rt_steps.

    ls_step-is_dynpro = abap_false.

    ls_step-name  = 'BDC_OKCODE'.
    ls_step-value = '=DBAC'.
    APPEND ls_step TO rt_steps.


    "----------------------"
    " Tela 3 SAPLSUPRN 0300
    "----------------------
    ls_step-program   = 'SAPLSUPRN'.
    ls_step-dynpro    = '0300'.
    ls_step-is_dynpro = abap_true.
    CLEAR ls_step-name. CLEAR ls_step-value.
    APPEND ls_step TO rt_steps.

    ls_step-is_dynpro = abap_false.

    ls_step-name  = 'BDC_OKCODE'.
    ls_step-value = '=GEN1'.
    APPEND ls_step TO rt_steps.


    "----------------------"
    " Tela 4 SAPLSPO1 0500
    "----------------------
    ls_step-program   = 'SAPLSPO1'.
    ls_step-dynpro    = '0500'.
    ls_step-is_dynpro = abap_true.
    CLEAR ls_step-name. CLEAR ls_step-value.
    APPEND ls_step TO rt_steps.

    ls_step-is_dynpro = abap_false.

    ls_step-name  = 'BDC_OKCODE'.
    ls_step-value = '=OPT1'.
    APPEND ls_step TO rt_steps.

    ls_step-name  = 'BDC_SUBSCR'.
    ls_step-value = 'SAPLSPO1                                0501SUBSCREEN'.
    APPEND ls_step TO rt_steps.

  ENDMETHOD.
