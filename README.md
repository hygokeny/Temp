METHOD on_end_of_task.
  DATA: lv_exception   TYPE char50,
        lo_log         TYPE REF TO cl_bal_log,
        lo_logger      TYPE REF TO cl_bal_logger,
        lv_has_error   TYPE abap_bool VALUE abap_false.

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
  " CRIAR APPLICATION LOG usando CL_BAL_LOGGER
  " ========================================
  TRY.
      " Criar cabeçalho do log
      lo_log = NEW cl_bal_log(
        i_object    = 'ZAPP_LOG'      " Seu objeto na SLG0
        i_subobject = 'ZUSER_CRUD'    " Seu subobjeto na SLG0
      ).

      " Definir número externo
      lo_log->set_external_number( |{ me->i_user }_{ sy-datum }{ sy-uzeit }| ).

      " Criar instância de logger
      lo_logger = NEW cl_bal_logger( i_log = lo_log ).

      " ========================================
      " CABEÇALHO DO LOG
      " ========================================
      lo_logger->add_msg(
        i_msgid = '00'
        i_msgno = '001'
        i_msgty = 'I'
        i_msgv1 = '====== CRIAÇÃO DE USUÁRIO ======'
      ).

      lo_logger->add_msg(
        i_msgid = '00'
        i_msgno = '001'
        i_msgty = 'I'
        i_msgv1 = |Usuário: { me->i_user }|
        i_msgv2 = |Data: { sy-datum }|
        i_msgv3 = |Hora: { sy-uzeit }|
        i_msgv4 = |Por: { sy-uname }|
      ).

      lo_logger->add_msg(
        i_msgid = '00'
        i_msgno = '001'
        i_msgty = 'I'
        i_msgv1 = |Email: { me->i_email }|
        i_msgv2 = |Departamento: { me->i_departament }|
        i_msgv3 = |Função: { me->i_function }|
        i_msgv4 = |Empresa: { me->i_company }|
      ).

      lo_logger->add_msg(
        i_msgid = '00'
        i_msgno = '001'
        i_msgty = 'I'
        i_msgv1 = |Válido de: { me->i_begda }|
        i_msgv2 = |Válido até: { me->i_endda }|
      ).

      " ========================================
      " PROCESSAR MENSAGENS DO GT_RETURN
      " ========================================
      IF gt_return IS NOT INITIAL.

        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'I'
          i_msgv1 = '====== MENSAGENS DA BAPI ======'
        ).

        " Adicionar cada mensagem do BAPIRET2
        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          lo_logger->add_msg(
            i_msgid = <return>-id
            i_msgno = <return>-number
            i_msgty = <return>-type
            i_msgv1 = <return>-message_v1
            i_msgv2 = <return>-message_v2
            i_msgv3 = <return>-message_v3
            i_msgv4 = <return>-message_v4
          ).

          " Adicionar detalhes extras se existirem
          IF <return>-parameter IS NOT INITIAL OR
             <return>-field IS NOT INITIAL OR
             <return>-row IS NOT INITIAL OR
             <return>-system IS NOT INITIAL.

            lo_logger->add_msg(
              i_msgid = '00'
              i_msgno = '001'
              i_msgty = 'I'
              i_msgv1 = |Parâmetro: { <return>-parameter }|
              i_msgv2 = |Campo: { <return>-field }|
              i_msgv3 = |Linha: { <return>-row }|
              i_msgv4 = |Sistema: { <return>-system }|
            ).
          ENDIF.

          IF <return>-log_no IS NOT INITIAL.
            lo_logger->add_msg(
              i_msgid = '00'
              i_msgno = '001'
              i_msgty = 'I'
              i_msgv1 = |Log No: { <return>-log_no }|
              i_msgv2 = |Log Msg No: { <return>-log_msg_no }|
            ).
          ENDIF.

          " Verificar se tem erro
          IF <return>-type CA 'AEX'.
            lv_has_error = abap_true.
          ENDIF.

        ENDLOOP.

        " ========================================
        " ESTATÍSTICAS
        " ========================================
        DATA(lv_total) = lines( gt_return ).
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

        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'I'
          i_msgv1 = '====== RESUMO ======'
        ).

        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'I'
          i_msgv1 = |Total de mensagens: { lv_total }|
          i_msgv2 = |Erros: { lv_errors }|
          i_msgv3 = |Avisos: { lv_warnings }|
          i_msgv4 = |Sucessos: { lv_success }|
        ).

      ELSE.
        " Nenhuma mensagem retornada
        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'W'
          i_msgv1 = 'Nenhuma mensagem retornada pela BAPI'
        ).
      ENDIF.

      " ========================================
      " RESULTADO FINAL
      " ========================================
      lo_logger->add_msg(
        i_msgid = '00'
        i_msgno = '001'
        i_msgty = 'I'
        i_msgv1 = '================================'
      ).

      IF lv_has_error = abap_true.
        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'E'
          i_msgv1 = 'RESULTADO: FALHA'
          i_msgv2 = |Usuário { me->i_user } NÃO foi criado|
        ).
      ELSE.
        lo_logger->add_msg(
          i_msgid = '00'
          i_msgno = '001'
          i_msgty = 'S'
          i_msgv1 = 'RESULTADO: SUCESSO'
          i_msgv2 = |Usuário { me->i_user } criado com sucesso|
        ).
      ENDIF.

      " ========================================
      " SALVAR LOG NO BANCO
      " ========================================
      lo_logger->save( ).

      " Adicionar mensagem de sucesso ao gt_return
      APPEND VALUE #(
        type    = 'S'
        id      = '00'
        number  = '001'
        message = 'Application Log gravado com sucesso na SLG1'
      ) TO gt_return.

    CATCH cx_root INTO DATA(lx_error).
      " Se houver erro ao criar log
      APPEND VALUE #(
        type       = 'E'
        id         = '00'
        number     = '001'
        message    = |Erro ao gravar log: { lx_error->get_text( ) }|
        message_v1 = lx_error->get_text( )
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
