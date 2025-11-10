class ZCL_USER_SETTINGS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(I_USER) type BAPIBNAME-BAPIBNAME
      value(I_DEPARTAMENT) type BAPIADDR3-DEPARTMENT
      value(I_FUNCTION) type BAPIADDR3-FUNCTION
      value(I_COMPANY) type BAPIADDR3-NAME
      value(I_EMAIL) type BAPIADDR3-E_MAIL
      value(I_BEGDA) type BAPILOGOND-GLTGV
      value(I_ENDDA) type BAPILOGOND-GLTGB .
  methods PREPARE_LOG .
  methods GET_RETURN
    returning
      value(R_RETURN) type BAPIRET2_T .
protected section.
private section.

  data I_USER type BAPIBNAME-BAPIBNAME .
  data I_DEPARTAMENT type BAPIADDR3-DEPARTMENT .
  data I_FUNCTION type BAPIADDR3-FUNCTION .
  data I_COMPANY type BAPIADDR3-NAME .
  data I_EMAIL type BAPIADDR3-E_MAIL .
  data I_BEGDA type BAPILOGOND-GLTGV .
  data I_ENDDA type BAPILOGOND-GLTGB .
  data:
    GT_RETURN type TABLE of BAPIRET2 .
  data MV_LOG_NUMBER type BALOGNR .

  methods UPDATE_USER
    importing
      value(I_USER) type BAPIBNAME-BAPIBNAME
      value(I_DEPARTAMENT) type BAPIADDR3-DEPARTMENT
      value(I_FUNCTION) type BAPIADDR3-FUNCTION
      value(I_COMPANY) type BAPIADDR3-NAME
      value(I_EMAIL) type BAPIADDR3-E_MAIL
      value(I_BEGDA) type BAPILOGOND-GLTGV
      value(I_ENDDA) type BAPILOGOND-GLTGB
    exporting
      value(E_SUCCESS) type ABAP_BOOL
      value(E_MESSAGE) type STRING
      value(ET_RETURN) type BAPIRET2_T .
  methods CREATE_USER
    importing
      value(I_USER) type BAPIBNAME-BAPIBNAME
      value(I_DEPARTAMENT) type BAPIADDR3-DEPARTMENT
      value(I_FUNCTION) type BAPIADDR3-FUNCTION
      value(I_COMPANY) type BAPIADDR3-NAME
      value(I_EMAIL) type BAPIADDR3-E_MAIL
      value(I_BEGDA) type BAPILOGOND-GLTGV
      value(I_ENDDA) type BAPILOGOND-GLTGB
    exporting
      value(E_SUCCESS) type ABAP_BOOL
      value(E_MESSAGE) type STRING
      value(ET_RETURN) type BAPIRET2_T .
  methods CHECK_VALID_USER
    returning
      value(IS_VALID) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_USER_SETTINGS IMPLEMENTATION.


  method UPDATE_USER.

    data lv_taskname type string VALUE 'UPDATE_TASK'.
    data(ls_address) = VALUE bapiaddr3(
        department       = I_DEPARTAMENT "'Departamento'
        function         = i_function "'Funcao'
        e_mail           = i_email "'eliezer.rodrigues@grupomoura.com.br'
    ).

    data(ls_addressx) = VALUE bapiaddr3x(
        department       = abap_true
        function         = abap_true
        title            = abap_true
        name             = abap_true
        e_mail           = abap_true
    ).


    data(ls_logondata) = VALUE bapilogond(
        gltgv           = i_begda "'19900101'
        gltgb           = i_endda "'20300101'
    ).

    data(ls_logondatax) = VALUE bapilogonx(
        gltgv           = abap_true
        gltgb           = abap_true
    ).

    data(ls_company) = VALUE bapiuscomp(
        company          = i_company
    ).

    data(ls_companyx) = VALUE bapiuscomx(
        company = abap_true
    ).

    CALL FUNCTION 'BAPI_USER_CHANGE'
*    STARTING NEW TASK lv_taskname
*    DESTINATION 'NONE'
      EXPORTING
        username                 = i_user
       LOGONDATA                = ls_logondata
       LOGONDATAX               = ls_logondatax
       ADDRESS                  = ls_address
       ADDRESSX                 = ls_addressx
       COMPANY                  = ls_company
       COMPANYX                 = ls_companyx
      tables
        return                   = gt_RETURN.

  endmethod.


  METHOD create_user.
    data(lv_max_tries) = 30.

    DATA lv_EXCEPTION TYPE char50.
    DATA lv_taskname TYPE char20.
    lv_taskname = |USER_CREATE_{ sy-uzeit } { sy-datum }|.

    DATA(ls_address) = VALUE bapiaddr3(
        department       = i_departament "'Departamento'
        function         = i_function "'Funcao'
        e_mail           = i_email "'eliezer.rodrigues@grupomoura.com.br'
    ).


    DATA(ls_addressx) = VALUE bapiaddr3x(
        department       = abap_true
        function         = abap_true
        title            = abap_true
        name             = abap_true
        e_mail           = abap_true
    ).


    DATA(ls_logondata) = VALUE bapilogond(
        gltgv           = i_begda                           "'19900101'
        gltgb           = i_endda                           "'20300101'
    ).

    DATA(ls_logondatax) = VALUE bapilogonx(
        gltgv           = abap_true
        gltgb           = abap_true
    ).

    DATA(ls_company) = VALUE bapiuscomp(
        company          = i_company
    ).

    DATA(ls_companyx) = VALUE bapiuscomx(
        company = abap_true
    ).

    DATA(ls_pass) = VALUE bapipwd(
        bapipwd = 'SenhaMoura!@#01'
    ).

    CALL FUNCTION 'BAPI_USER_CREATE'
*      STARTING NEW TASK lv_taskname
*      DESTINATION IN GROUP DEFAULT
*      CALLING on_end_of_task ON END OF TASK
      EXPORTING
        username              = i_user
        logondata             = ls_logondata
        password              = ls_pass
*       defaults              =
        address               = ls_address
        company               = ls_company
*       lock_locally          = space
*       generate_pwd          = space
*       description           =
*       sapuser_uuid          =
*      IMPORTING
*       generated_password    =
      TABLES
        return                = gt_return.

ENDMETHOD.


  method CHECK_VALID_USER.

    select COUNT(*)
      from usr01
      where bname =  me->i_user.
      IF sy-subrc is INITIAL.
        is_valid = abap_true.
      ENDIF.
  endmethod.


  METHOD constructor.

    me->i_user         = i_user       .
    me->i_departament  = i_departament.
    me->i_function     = i_function   .
    me->i_company      = i_company    .
    me->i_email        = i_email      .
    me->i_begda        = i_begda      .
    me->i_endda        = i_endda      .


    "Rotina para Editar
    IF check_valid_user( ) = abap_true.

      update_user(
        EXPORTING
          i_user        = i_user
          i_departament = i_departament
          i_function    = i_function
          i_company     = i_company
          i_email       = i_email
          i_begda       = i_begda
          i_endda       = i_endda
        IMPORTING
          e_success     = DATA(e_success)
          e_message     = DATA(e_message)
          et_return     = DATA(e_return) ).



      "Rotina para Criar
    ELSE.

      create_user(
        EXPORTING
          i_user        = i_user
          i_departament = i_departament
          i_function    = i_function
          i_company     = i_company
          i_email       = i_email
          i_begda       = i_begda
          i_endda       = i_endda
        IMPORTING
          e_success     = e_success
          e_message     = e_message
          et_return     = e_return ).


    ENDIF.
    prepare_log( ).
  ENDMETHOD.


  method GET_RETURN.
    r_return = gt_return.
  endmethod.


METHOD PREPARE_LOG.
  DATA: lv_exception   TYPE char50,
        lo_log_writer  TYPE REF TO /aif/cl_appl_log_writer,
        lv_has_error   TYPE abap_bool VALUE abap_false,
        lv_has_warning TYPE abap_bool VALUE abap_false,
        lv_has_success TYPE abap_bool VALUE abap_false.

  " Limpar retorno anterior
*  CLEAR gt_return.

  " Receber resultados da BAPI
*  RECEIVE RESULTS FROM FUNCTION 'BAPI_USER_CREATE'
*    TABLES
*      return                = gt_return
*    EXCEPTIONS
*      communication_failure = 1 MESSAGE lv_exception
*      system_failure        = 2 MESSAGE lv_exception
*      resource_failure      = 3.

  " Se houver exceção de comunicação, adicionar ao gt_return
*  IF sy-subrc <> 0.
*    APPEND VALUE #(
*      type       = 'E'
*      id         = '00'
*      number     = '398'
*      message    = |Exceção de comunicação: { lv_exception }|
*      message_v1 = lv_exception
*    ) TO gt_return.
*  ENDIF.

  " ========================================
  " CRIAR APPLICATION LOG
  " ========================================
  TRY.
      " Criar instância do log writer
      lo_log_writer = /aif/cl_appl_log_writer=>get_instance(
        iv_obj        = 'ZAPP_LOG'                                    " Objeto na SLG0
        iv_subobj     = 'ZUSER_LOG'                                   " Subobjeto
        iv_extnumber  = |{ me->i_user }_{ sy-datum }{ sy-uzeit }|     " Número externo
      ).

      " ========================================
      " CABEÇALHO DO LOG
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = '========== CRIAÇÃO DE USUÁRIO =========='
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = |Usuário: { me->i_user }|
        iv_msgv2      = |Data: { sy-datum DATE = USER }|
        iv_msgv3      = |Hora: { sy-uzeit TIME = USER }|
        iv_msgv4      = |Executado por: { sy-uname }|
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = |Email: { me->i_email }|
        iv_msgv2      = |Departamento: { me->i_departament }|
        iv_msgv3      = |Função: { me->i_function }|
        iv_msgv4      = |Empresa: { me->i_company }|
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = |Válido de: { me->i_begda DATE = USER }|
        iv_msgv2      = |Válido até: { me->i_endda DATE = USER }|
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = |Programa: { sy-cprog }|
        iv_msgv2      = |Transação: { sy-tcode }|
        iv_max_errors = 9999
      ).

      " ========================================
      " PROCESSAR MENSAGENS DO GT_RETURN
      " ========================================
      IF gt_return IS NOT INITIAL.

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'I'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = '========== MENSAGENS DA BAPI =========='
          iv_max_errors = 9999
        ).

        " Adicionar cada mensagem do BAPIRET2
        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          " ========================================
          " ADICIONAR MENSAGEM PRINCIPAL DA BAPI
          " ========================================
          lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
            iv_msgty      = <return>-type          " Usar campo TYPE da estrutura
            iv_msgid      = <return>-id            " Usar campo ID da estrutura
            iv_msgno      = <return>-number        " Usar campo NUMBER da estrutura
            iv_msgv1      = <return>-message_v1    " Usar campo MESSAGE_V1 da estrutura
            iv_msgv2      = <return>-message_v2    " Usar campo MESSAGE_V2 da estrutura
            iv_msgv3      = <return>-message_v3    " Usar campo MESSAGE_V3 da estrutura
            iv_msgv4      = <return>-message_v4    " Usar campo MESSAGE_V4 da estrutura
            iv_max_errors = 9999
          ).

          " ========================================
          " ADICIONAR DETALHES EXTRAS (PARAMETER, FIELD, ROW, SYSTEM)
          " ========================================
          IF <return>-parameter IS NOT INITIAL OR
             <return>-field IS NOT INITIAL OR
             <return>-row IS NOT INITIAL OR
             <return>-system IS NOT INITIAL.

            lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
              iv_msgty      = 'I'
              iv_msgid      = '00'
              iv_msgno      = '001'
              iv_msgv1      = |  Parâmetro: { <return>-parameter }|    " Campo PARAMETER da estrutura
              iv_msgv2      = |  Campo: { <return>-field }|             " Campo FIELD da estrutura
              iv_msgv3      = |  Linha: { <return>-row }|               " Campo ROW da estrutura
              iv_msgv4      = |  Sistema: { <return>-system }|          " Campo SYSTEM da estrutura
              iv_max_errors = 9999
            ).
          ENDIF.

          " ========================================
          " ADICIONAR LOG_NO e LOG_MSG_NO SE EXISTIREM
          " ========================================
          IF <return>-log_no IS NOT INITIAL OR <return>-log_msg_no IS NOT INITIAL.
            lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
              iv_msgty      = 'I'
              iv_msgid      = '00'
              iv_msgno      = '001'
              iv_msgv1      = |  Log Number: { <return>-log_no }|       " Campo LOG_NO da estrutura
              iv_msgv2      = |  Log Msg No: { <return>-log_msg_no }|   " Campo LOG_MSG_NO da estrutura
              iv_max_errors = 9999
            ).
          ENDIF.

          " ========================================
          " ADICIONAR MENSAGEM COMPLETA SE EXISTIR
          " ========================================
          IF <return>-message IS NOT INITIAL.
            lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
              iv_msgty      = 'I'
              iv_msgid      = '00'
              iv_msgno      = '001'
              iv_msgv1      = |  Mensagem: { <return>-message }|        " Campo MESSAGE da estrutura
              iv_max_errors = 9999
            ).
          ENDIF.

          " ========================================
          " CLASSIFICAR TIPO DE MENSAGEM
          " ========================================
          CASE <return>-type.
            WHEN 'E' OR 'A'.  " Error ou Abort
              lv_has_error = abap_true.
            WHEN 'W'.         " Warning
              lv_has_warning = abap_true.
            WHEN 'S'.         " Success
              lv_has_success = abap_true.
          ENDCASE.

        ENDLOOP.

        " ========================================
        " ESTATÍSTICAS DAS MENSAGENS
        " ========================================
        DATA(lv_total_msgs) = lines( gt_return ).
        DATA(lv_count_errors) = REDUCE i( INIT x = 0
                                           FOR wa IN gt_return
                                           WHERE ( type = 'E' OR type = 'A' )
                                           NEXT x = x + 1 ).
        DATA(lv_count_warnings) = REDUCE i( INIT x = 0
                                             FOR wa IN gt_return
                                             WHERE ( type = 'W' )
                                             NEXT x = x + 1 ).
        DATA(lv_count_success) = REDUCE i( INIT x = 0
                                            FOR wa IN gt_return
                                            WHERE ( type = 'S' )
                                            NEXT x = x + 1 ).
        DATA(lv_count_info) = REDUCE i( INIT x = 0
                                         FOR wa IN gt_return
                                         WHERE ( type = 'I' )
                                         NEXT x = x + 1 ).

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'I'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = '========== ESTATÍSTICAS =========='
          iv_max_errors = 9999
        ).

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'I'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = |Total de mensagens: { lv_total_msgs }|
          iv_msgv2      = |Erros: { lv_count_errors }|
          iv_msgv3      = |Avisos: { lv_count_warnings }|
          iv_msgv4      = |Sucessos: { lv_count_success }|
          iv_max_errors = 9999
        ).

        IF lv_count_info > 0.
          lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
            iv_msgty      = 'I'
            iv_msgid      = '00'
            iv_msgno      = '001'
            iv_msgv1      = |Informações: { lv_count_info }|
            iv_max_errors = 9999
          ).
        ENDIF.

      ELSE.
        " Nenhuma mensagem retornada pela BAPI
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'W'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = 'ATENÇÃO: Nenhuma mensagem retornada pela BAPI'
          iv_msgv2      = 'Verifique se a BAPI foi executada corretamente'
          iv_max_errors = 9999
        ).
      ENDIF.

      " ========================================
      " RESULTADO FINAL DA OPERAÇÃO
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = '=========================================='
        iv_max_errors = 9999
      ).

      IF lv_has_error = abap_true.
        " ERRO - Operação falhou
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'E'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = 'RESULTADO: ERRO'
          iv_msgv2      = |Usuário { me->i_user } NÃO foi criado|
          iv_msgv3      = |Total de erros: { lv_count_errors }|
          iv_max_errors = 9999
        ).
      ELSEIF lv_has_warning = abap_true.
        " WARNING - Operação com avisos
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'W'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = 'RESULTADO: AVISO'
          iv_msgv2      = |Usuário { me->i_user } criado com avisos|
          iv_msgv3      = |Total de avisos: { lv_count_warnings }|
          iv_max_errors = 9999
        ).
      ELSEIF lv_has_success = abap_true.
        " SUCCESS - Operação bem-sucedida
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'S'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = 'RESULTADO: SUCESSO'
          iv_msgv2      = |Usuário { me->i_user } criado com sucesso|
          iv_max_errors = 9999
        ).
      ELSE.
        " INDETERMINADO - Sem mensagens claras
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty      = 'W'
          iv_msgid      = '00'
          iv_msgno      = '001'
          iv_msgv1      = 'RESULTADO: INDETERMINADO'
          iv_msgv2      = |Usuário { me->i_user } - status desconhecido|
          iv_msgv3      = 'Verifique as mensagens acima'
          iv_max_errors = 9999
        ).
      ENDIF.

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty      = 'I'
        iv_msgid      = '00'
        iv_msgno      = '001'
        iv_msgv1      = '=========================================='
        iv_max_errors = 9999
      ).

      " ========================================
      " SALVAR LOG NO BANCO DE DADOS
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~save_log( ).

      " Adicionar mensagem informativa ao gt_return
      APPEND VALUE #(
        type       = 'I'
        id         = '00'
        number     = '001'
        message    = 'Application Log gravado com sucesso na SLG1'
        message_v1 = 'ZAPP_LOG'
        message_v2 = 'ZUSER_CRUD'
        parameter  = 'LOG_SUCCESS'
      ) TO gt_return.

    CATCH /aif/cx_error_handling_general INTO DATA(lx_aif_error).
      " Exceção específica do framework AIF
      APPEND VALUE #(
        type       = 'E'
        id         = '00'
        number     = '001'
        message    = |Erro AIF ao criar log: { lx_aif_error->get_text( ) }|
        message_v1 = lx_aif_error->get_text( )
      ) TO gt_return.

    CATCH cx_root INTO DATA(lx_error).
      " Exceção genérica
      APPEND VALUE #(
        type       = 'E'
        id         = '00'
        number     = '001'
        message    = |Erro geral ao gravar log: { lx_error->get_text( ) }|
        message_v1 = lx_error->get_text( )
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
