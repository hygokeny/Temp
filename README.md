METHOD save_log.
  DATA: lt_log_handle     TYPE bal_t_logh,
        lt_new_lognumbers TYPE bal_t_lgnm,
        lv_subrc          TYPE sy-subrc.

  APPEND mv_log_handle TO lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
      i_save_all       = abap_true
    IMPORTING
      e_new_lognumbers = lt_new_lognumbers
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  lv_subrc = sy-subrc.

  IF lv_subrc = 0 AND lt_new_lognumbers IS NOT INITIAL.
    rv_log_number = lt_new_lognumbers[ 1 ]-lognumber.
    
    " COMMIT é essencial para persistir no banco
    COMMIT WORK AND WAIT.
    
  ELSE.
    " Erro ao salvar - retornar vazio
    CLEAR rv_log_number.
  ENDIF.

ENDMETHOD.

----------------------

METHOD on_end_of_task.
  DATA: lv_exception   TYPE char50,
        lo_log         TYPE REF TO zcl_application_log,
        lv_has_error   TYPE abap_bool VALUE abap_false,
        lv_has_warning TYPE abap_bool VALUE abap_false,
        lv_has_success TYPE abap_bool VALUE abap_false,
        lv_log_created TYPE abap_bool VALUE abap_false.

  " Limpar retorno anterior
  CLEAR gt_return.

  " Receber resultados da BAPI
  RECEIVE RESULTS FROM FUNCTION 'BAPI_USER_CREATE'
    TABLES
      return                = gt_return
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_exception
      system_failure        = 2 MESSAGE lv_exception
      resource_failure      = 3.

  " Se houver exceção de comunicação
  IF sy-subrc <> 0.
    APPEND VALUE #(
      type       = 'E'
      id         = '00'
      number     = '398'
      message    = |Exceção: { lv_exception }|
      message_v1 = lv_exception
    ) TO gt_return.
  ENDIF.

  " ========================================
  " CRIAR APPLICATION LOG
  " ========================================
  TRY.
      " Verificar se objeto existe na SLG0
      DATA: ls_obj TYPE bal_s_obj.
      
      CALL FUNCTION 'BAL_OBJECT_READ'
        EXPORTING
          i_object         = 'ZAPP_LOG'
        IMPORTING
          e_s_obj          = ls_obj
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        " Objeto não existe na SLG0
        APPEND VALUE #(
          type    = 'E'
          id      = '00'
          number  = '001'
          message = 'Objeto ZAPP_LOG não existe na SLG0. Execute SLG0 e crie o objeto.'
        ) TO gt_return.
        RETURN.
      ENDIF.

      " Criar instância do log
      lo_log = NEW zcl_application_log(
        iv_object     = 'ZAPP_LOG'
        iv_subobject  = 'ZUSER_CRUD'
        iv_extnumber  = |{ me->i_user }_{ sy-datum }{ sy-uzeit }|
      ).

      " Verificar se log foi criado
      DATA(lv_handle) = lo_log->get_log_handle( ).
      
      IF lv_handle IS INITIAL.
        APPEND VALUE #(
          type    = 'E'
          id      = '00'
          number  = '001'
          message = 'Erro ao criar handle do log'
        ) TO gt_return.
        RETURN.
      ELSE.
        lv_log_created = abap_true.
      ENDIF.

      " Adicionar cabeçalho
      lo_log->add_message_text(
        iv_text  = |========== CRIAÇÃO DE USUÁRIO ==========|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Usuário: { me->i_user }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Data/Hora: { sy-datum } { sy-uzeit }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Executado por: { sy-uname }|
        iv_msgty = 'I'
      ).

      " Adicionar dados do usuário
      lo_log->add_message_text(
        iv_text  = |Email: { me->i_email }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Departamento: { me->i_departament }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Função: { me->i_function }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Empresa: { me->i_company }|
        iv_msgty = 'I'
      ).

      " Processar mensagens da BAPI
      IF gt_return IS NOT INITIAL.

        lo_log->add_message_text(
          iv_text  = |========== MENSAGENS DA BAPI ==========|
          iv_msgty = 'I'
        ).

        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          " Adicionar mensagem
          lo_log->add_message_from_bapiret2( <return> ).

          " Contar tipos
          CASE <return>-type.
            WHEN 'E' OR 'A'.
              lv_has_error = abap_true.
            WHEN 'W'.
              lv_has_warning = abap_true.
            WHEN 'S'.
              lv_has_success = abap_true.
          ENDCASE.

        ENDLOOP.

        " Estatísticas
        DATA(lv_total) = lines( gt_return ).
        DATA(lv_errors) = REDUCE i( INIT x = 0
                                     FOR wa IN gt_return
                                     WHERE ( type = 'E' OR type = 'A' )
                                     NEXT x = x + 1 ).

        lo_log->add_message_text(
          iv_text  = |Total mensagens: { lv_total } | Erros: { lv_errors }|
          iv_msgty = 'I'
        ).

      ENDIF.

      " Resultado final
      IF lv_has_error = abap_true.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: ERRO - Usuário não criado|
          iv_msgty = 'E'
        ).
      ELSE.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: SUCESSO|
          iv_msgty = 'S'
        ).
      ENDIF.

      " SALVAR LOG
      mv_log_number = lo_log->save_log( ).

      IF mv_log_number IS NOT INITIAL.
        " Log salvo com sucesso
        APPEND VALUE #(
          type       = 'S'
          id         = '00'
          number     = '001'
          message    = |Log gravado: { mv_log_number }|
          message_v1 = CONV #( mv_log_number )
          parameter  = 'LOG_NUMBER'
        ) TO gt_return.
      ELSE.
        " Falha ao salvar
        APPEND VALUE #(
          type    = 'E'
          id      = '00'
          number  = '001'
          message = 'Erro: Log não foi salvo no banco'
        ) TO gt_return.
      ENDIF.

    CATCH cx_root INTO DATA(lx_error).
      APPEND VALUE #(
        type    = 'E'
        id      = '00'
        number  = '001'
        message = |Exceção ao criar log: { lx_error->get_text( ) }|
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.

----------


" Na definição da classe zcl_application_log
METHODS get_log_handle
  RETURNING
    VALUE(rv_handle) TYPE balloghndl.

" Na implementação
METHOD get_log_handle.
  rv_handle = mv_log_handle.
ENDMETHOD.
