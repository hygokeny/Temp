METHOD on_end_of_task.
  DATA: lv_exception  TYPE char50,
        lo_log        TYPE REF TO zcl_application_log,
        lv_has_error  TYPE abap_bool VALUE abap_false,
        lv_has_warning TYPE abap_bool VALUE abap_false,
        lv_operation  TYPE string.

  " Receber resultados da BAPI
  RECEIVE RESULTS FROM FUNCTION 'BAPI_USER_CREATE'
    TABLES
      return                = gt_return
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_exception
      system_failure        = 2 MESSAGE lv_exception
      resource_failure      = 3.

  " Criar log de aplicação
  TRY.
      " Criar instância do log
      lo_log = NEW zcl_application_log(
        iv_object     = 'ZAPP_LOG'
        iv_subobject  = 'ZUSER_CRUD'
        iv_extnumber  = |USER_{ me->i_user }_{ sy-datum }{ sy-uzeit }|
      ).

      " Determinar tipo de operação
      lv_operation = |Criação de usuário { me->i_user }|.

      " Adicionar mensagem inicial
      lo_log->add_message_text(
        iv_text  = |Início: { lv_operation }|
        iv_msgty = 'I'
      ).

      " Adicionar detalhes da requisição
      lo_log->add_message_text(
        iv_text  = |Usuário: { me->i_user }|
        iv_msgty = 'I'
      ).
      
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
      
      lo_log->add_message_text(
        iv_text  = |Válido de { me->i_begda } até { me->i_endda }|
        iv_msgty = 'I'
      ).

      " Processar e logar todas as mensagens do gt_return
      IF gt_return IS NOT INITIAL.
        
        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).
          
          " Adicionar cada mensagem BAPIRET2 ao log
          lo_log->add_message_from_bapiret2( <return> ).
          
          " Verificar se há erros ou warnings
          CASE <return>-type.
            WHEN 'E' OR 'A'.  " Error ou Abort
              lv_has_error = abap_true.
            WHEN 'W'.         " Warning
              lv_has_warning = abap_true.
          ENDCASE.
          
        ENDLOOP.

        " Adicionar resumo
        DATA(lv_total_msgs) = lines( gt_return ).
        DATA(lv_errors) = REDUCE i( INIT x = 0 
                                     FOR wa IN gt_return 
                                     WHERE ( type = 'E' OR type = 'A' )
                                     NEXT x = x + 1 ).
        DATA(lv_warnings) = REDUCE i( INIT x = 0 
                                       FOR wa IN gt_return 
                                       WHERE ( type = 'W' )
                                       NEXT x = x + 1 ).
        DATA(lv_success) = REDUCE i( INIT x = 0 
                                      FOR wa IN gt_return 
                                      WHERE ( type = 'S' )
                                      NEXT x = x + 1 ).

        lo_log->add_message_text(
          iv_text  = |Resumo: { lv_total_msgs } msgs ({ lv_errors } erros, { lv_warnings } avisos, { lv_success } sucesso)|
          iv_msgty = 'I'
        ).

      ELSE.
        lo_log->add_message_text(
          iv_text  = 'Nenhuma mensagem retornada pela BAPI'
          iv_msgty = 'W'
        ).
      ENDIF.

      " Adicionar mensagem final baseada no resultado
      IF lv_has_error = abap_true.
        lo_log->add_message_text(
          iv_text  = |ERRO: Falha ao criar usuário { me->i_user }|
          iv_msgty = 'E'
        ).
      ELSEIF lv_has_warning = abap_true.
        lo_log->add_message_text(
          iv_text  = |AVISO: Usuário { me->i_user } criado com avisos|
          iv_msgty = 'W'
        ).
      ELSE.
        lo_log->add_message_text(
          iv_text  = |SUCESSO: Usuário { me->i_user } criado com sucesso|
          iv_msgty = 'S'
        ).
      ENDIF.

      " Salvar log no banco de dados
      mv_log_number = lo_log->save_log( ).

      " Adicionar número do log ao retorno (opcional)
      APPEND VALUE #(
        type       = 'I'
        id         = '00'
        number     = '001'
        message    = |Log de aplicação gravado: { mv_log_number }|
        message_v1 = mv_log_number
      ) TO gt_return.

    CATCH cx_root INTO DATA(lx_error).
      " Se houver erro ao criar log, adicionar ao retorno
      APPEND VALUE #(
        type    = 'E'
        id      = '00'
        number  = '001'
        message = |Erro ao gravar log: { lx_error->get_text( ) }|
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
