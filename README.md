METHOD on_end_of_task.
  DATA: lv_exception   TYPE char50,
        lo_log         TYPE REF TO cl_bal_log,
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

  " Se houver exceção de comunicação
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
  " CRIAR APPLICATION LOG usando CL_BAL_LOG
  " ========================================
  TRY.
      " Criar instância do log
      lo_log = NEW cl_bal_log( ).
      
      " Criar log
      lo_log->create( ).

      " ========================================
      " CABEÇALHO DO LOG
      " ========================================
      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = '========== CRIAÇÃO DE USUÁRIO =========='
        iv_msgv2 = ''
        iv_msgv3 = ''
        iv_msgv4 = ''
      ).

      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = |Data/Hora: { sy-datum } { sy-uzeit }|
        iv_msgv2 = |Usuário: { sy-uname }|
        iv_msgv3 = |Programa: { sy-cprog }|
        iv_msgv4 = |Transação: { sy-tcode }|
      ).

      " ========================================
      " DADOS DO USUÁRIO
      " ========================================
      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = |Usuário: { me->i_user }|
        iv_msgv2 = |Email: { me->i_email }|
        iv_msgv3 = ''
        iv_msgv4 = ''
      ).

      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = |Departamento: { me->i_departament }|
        iv_msgv2 = |Função: { me->i_function }|
        iv_msgv3 = |Empresa: { me->i_company }|
        iv_msgv4 = ''
      ).

      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = |Válido de: { me->i_begda }|
        iv_msgv2 = |Válido até: { me->i_endda }|
        iv_msgv3 = ''
        iv_msgv4 = ''
      ).

      " ========================================
      " PROCESSAR MENSAGENS DO GT_RETURN
      " ========================================
      IF gt_return IS NOT INITIAL.

        lo_log->add_message(
          iv_msgty = 'I'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = '========== MENSAGENS DA BAPI =========='
          iv_msgv2 = ''
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).

        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          " Adicionar cada mensagem do BAPIRET2 ao log
          lo_log->add_message(
            iv_msgty = <return>-type
            iv_msgid = <return>-id
            iv_msgno = <return>-number
            iv_msgv1 = <return>-message_v1
            iv_msgv2 = <return>-message_v2
            iv_msgv3 = <return>-message_v3
            iv_msgv4 = <return>-message_v4
          ).

          " Adicionar detalhes extras se existirem
          IF <return>-parameter IS NOT INITIAL OR
             <return>-field IS NOT INITIAL OR
             <return>-row IS NOT INITIAL.
            
            lo_log->add_message(
              iv_msgty = 'I'
              iv_msgid = '00'
              iv_msgno = '001'
              iv_msgv1 = |Parâmetro: { <return>-parameter }|
              iv_msgv2 = |Campo: { <return>-field }|
              iv_msgv3 = |Linha: { <return>-row }|
              iv_msgv4 = |Sistema: { <return>-system }|
            ).
          ENDIF.

          " Verificar tipo de mensagem
          CASE <return>-type.
            WHEN 'E' OR 'A'.
              lv_has_error = abap_true.
            WHEN 'W'.
              lv_has_warning = abap_true.
            WHEN 'S'.
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

        lo_log->add_message(
          iv_msgty = 'I'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = '========== RESUMO =========='
          iv_msgv2 = ''
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).

        lo_log->add_message(
          iv_msgty = 'I'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = |Total mensagens: { lv_total_msgs }|
          iv_msgv2 = |Erros: { lv_errors }|
          iv_msgv3 = |Avisos: { lv_warnings }|
          iv_msgv4 = |Sucessos: { lv_success }|
        ).

      ELSE.
        " Nenhuma mensagem retornada
        lo_log->add_message(
          iv_msgty = 'W'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = 'ATENÇÃO: Nenhuma mensagem retornada pela'
          iv_msgv2 = 'BAPI'
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).
      ENDIF.

      " ========================================
      " RESULTADO FINAL
      " ========================================
      lo_log->add_message(
        iv_msgty = 'I'
        iv_msgid = '00'
        iv_msgno = '001'
        iv_msgv1 = '========================================'
        iv_msgv2 = ''
        iv_msgv3 = ''
        iv_msgv4 = ''
      ).

      IF lv_has_error = abap_true.
        lo_log->add_message(
          iv_msgty = 'E'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = |RESULTADO: FALHA|
          iv_msgv2 = |Usuário { me->i_user } NÃO foi criado|
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).
      ELSEIF lv_has_warning = abap_true.
        lo_log->add_message(
          iv_msgty = 'W'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = |RESULTADO: PARCIAL|
          iv_msgv2 = |Usuário { me->i_user } criado com avisos|
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).
      ELSEIF lv_has_success = abap_true.
        lo_log->add_message(
          iv_msgty = 'S'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = |RESULTADO: SUCESSO|
          iv_msgv2 = |Usuário { me->i_user } criado com sucesso|
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).
      ELSE.
        lo_log->add_message(
          iv_msgty = 'W'
          iv_msgid = '00'
          iv_msgno = '001'
          iv_msgv1 = |RESULTADO: INDETERMINADO|
          iv_msgv2 = 'Verificar mensagens'
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).
      ENDIF.

      " ========================================
      " SALVAR LOG NO BANCO
      " ========================================
      lo_log->save( ).

      " Adicionar mensagem ao gt_return informando que log foi gravado
      APPEND VALUE #(
        type       = 'I'
        id         = '00'
        number     = '001'
        message    = 'Application Log gravado com sucesso'
        parameter  = 'LOG_SAVED'
      ) TO gt_return.

    CATCH cx_root INTO DATA(lx_error).
      " Se houver erro ao criar log, adicionar ao gt_return
      APPEND VALUE #(
        type       = 'E'
        id         = '00'
        number     = '001'
        message    = |ERRO ao gravar log: { lx_error->get_text( ) }|
        message_v1 = lx_error->get_text( )
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
