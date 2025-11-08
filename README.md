METHOD on_end_of_task.
  DATA: lv_exception   TYPE char50,
        lo_log         TYPE REF TO zcl_application_log,
        lv_has_error   TYPE abap_bool VALUE abap_false,
        lv_has_warning TYPE abap_bool VALUE abap_false,
        lv_has_success TYPE abap_bool VALUE abap_false.

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

  " Se houver exceção de comunicação, adicionar ao gt_return
  IF sy-subrc <> 0.
    APPEND VALUE #(
      type       = 'E'
      id         = '00'
      number     = '398'
      message    = |Exceção de comunicação: { lv_exception }|
      message_v1 = lv_exception
    ) TO gt_return.
  ENDIF.

  " ========================================
  " CRIAR APPLICATION LOG (SLG0)
  " ========================================
  TRY.
      " Criar instância do log
      lo_log = NEW zcl_application_log(
        iv_object     = 'ZAPP_LOG'
        iv_subobject  = 'ZUSER_CRUD'
        iv_extnumber  = |{ me->i_user }_{ sy-datum }{ sy-uzeit }|
      ).

      " ========================================
      " CABEÇALHO DO LOG
      " ========================================
      lo_log->add_message_text(
        iv_text  = |========== CRIAÇÃO DE USUÁRIO ==========|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Data/Hora: { sy-datum DATE = USER } { sy-uzeit TIME = USER }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Executado por: { sy-uname }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Programa: { sy-cprog }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |Transação: { sy-tcode }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |========================================|
        iv_msgty = 'I'
      ).

      " ========================================
      " DADOS DA REQUISIÇÃO
      " ========================================
      lo_log->add_message_text(
        iv_text  = |DADOS DO USUÁRIO:|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Usuário: { me->i_user }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Email: { me->i_email }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Departamento: { me->i_departament }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Função: { me->i_function }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Empresa: { me->i_company }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Válido de: { me->i_begda DATE = USER }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |  Válido até: { me->i_endda DATE = USER }|
        iv_msgty = 'I'
      ).

      lo_log->add_message_text(
        iv_text  = |========================================|
        iv_msgty = 'I'
      ).

      " ========================================
      " PROCESSAR TODAS AS MENSAGENS DO GT_RETURN
      " ========================================
      IF gt_return IS NOT INITIAL.

        lo_log->add_message_text(
          iv_text  = |MENSAGENS DA BAPI:|
          iv_msgty = 'I'
        ).

        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          " Adicionar mensagem completa da BAPIRET2
          lo_log->add_message_from_bapiret2( <return> ).

          " Adicionar detalhes extras se existirem
          IF <return>-parameter IS NOT INITIAL.
            lo_log->add_message_text(
              iv_text  = |  Parâmetro: { <return>-parameter }|
              iv_msgty = 'I'
            ).
          ENDIF.

          IF <return>-field IS NOT INITIAL.
            lo_log->add_message_text(
              iv_text  = |  Campo: { <return>-field }|
              iv_msgty = 'I'
            ).
          ENDIF.

          IF <return>-row IS NOT INITIAL.
            lo_log->add_message_text(
              iv_text  = |  Linha: { <return>-row }|
              iv_msgty = 'I'
            ).
          ENDIF.

          IF <return>-system IS NOT INITIAL.
            lo_log->add_message_text(
              iv_text  = |  Sistema: { <return>-system }|
              iv_msgty = 'I'
            ).
          ENDIF.

          IF <return>-log_no IS NOT INITIAL.
            lo_log->add_message_text(
              iv_text  = |  Log No: { <return>-log_no } / { <return>-log_msg_no }|
              iv_msgty = 'I'
            ).
          ENDIF.

          " Verificar tipo de mensagem
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
        " ESTATÍSTICAS
        " ========================================
        DATA(lv_total_msgs) = lines( gt_return ).
        DATA(lv_errors)     = REDUCE i( INIT x = 0
                                        FOR wa IN gt_return
                                        WHERE ( type = 'E' OR type = 'A' )
                                        NEXT x = x + 1 ).
        DATA(lv_warnings)   = REDUCE i( INIT x = 0
                                        FOR wa IN gt_return
                                        WHERE ( type = 'W' )
                                        NEXT x = x + 1 ).
        DATA(lv_success)    = REDUCE i( INIT x = 0
                                        FOR wa IN gt_return
                                        WHERE ( type = 'S' )
                                        NEXT x = x + 1 ).
        DATA(lv_info)       = REDUCE i( INIT x = 0
                                        FOR wa IN gt_return
                                        WHERE ( type = 'I' )
                                        NEXT x = x + 1 ).

        lo_log->add_message_text(
          iv_text  = |========================================|
          iv_msgty = 'I'
        ).

        lo_log->add_message_text(
          iv_text  = |RESUMO DAS MENSAGENS:|
          iv_msgty = 'I'
        ).

        lo_log->add_message_text(
          iv_text  = |  Total de mensagens: { lv_total_msgs }|
          iv_msgty = 'I'
        ).

        lo_log->add_message_text(
          iv_text  = |  Erros: { lv_errors }|
          iv_msgty = COND #( WHEN lv_errors > 0 THEN 'E' ELSE 'I' )
        ).

        lo_log->add_message_text(
          iv_text  = |  Avisos: { lv_warnings }|
          iv_msgty = COND #( WHEN lv_warnings > 0 THEN 'W' ELSE 'I' )
        ).

        lo_log->add_message_text(
          iv_text  = |  Sucessos: { lv_success }|
          iv_msgty = 'I'
        ).

        lo_log->add_message_text(
          iv_text  = |  Informações: { lv_info }|
          iv_msgty = 'I'
        ).

      ELSE.
        " Nenhuma mensagem retornada
        lo_log->add_message_text(
          iv_text  = |ATENÇÃO: Nenhuma mensagem retornada pela BAPI|
          iv_msgty = 'W'
        ).
      ENDIF.

      " ========================================
      " RESULTADO FINAL
      " ========================================
      lo_log->add_message_text(
        iv_text  = |========================================|
        iv_msgty = 'I'
      ).

      IF lv_has_error = abap_true.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: FALHA - Usuário { me->i_user } NÃO foi criado|
          iv_msgty = 'E'
        ).
      ELSEIF lv_has_warning = abap_true.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: PARCIAL - Usuário { me->i_user } criado com avisos|
          iv_msgty = 'W'
        ).
      ELSEIF lv_has_success = abap_true.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: SUCESSO - Usuário { me->i_user } criado com sucesso|
          iv_msgty = 'S'
        ).
      ELSE.
        lo_log->add_message_text(
          iv_text  = |RESULTADO: INDETERMINADO - Verificar mensagens|
          iv_msgty = 'W'
        ).
      ENDIF.

      lo_log->add_message_text(
        iv_text  = |========================================|
        iv_msgty = 'I'
      ).

      " ========================================
      " SALVAR LOG NO BANCO DE DADOS
      " ========================================
      mv_log_number = lo_log->save_log( ).

      " Adicionar número do log ao gt_return para referência
      IF mv_log_number IS NOT INITIAL.
        APPEND VALUE #(
          type       = 'I'
          id         = '00'
          number     = '001'
          message    = |Application Log gravado com número: { mv_log_number }|
          message_v1 = mv_log_number
          parameter  = 'LOG_NUMBER'
        ) TO gt_return.
      ENDIF.

    CATCH cx_root INTO DATA(lx_error).
      " Se houver erro ao criar log, adicionar ao gt_return
      APPEND VALUE #(
        type       = 'E'
        id         = '00'
        number     = '001'
        message    = |ERRO ao gravar Application Log: { lx_error->get_text( ) }|
        message_v1 = lx_error->get_text( )
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
