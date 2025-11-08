METHOD on_end_of_task.
  DATA: lv_exception   TYPE char50,
        lo_log_writer  TYPE REF TO /aif/cl_appl_log_writer,
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
  " CRIAR APPLICATION LOG
  " ========================================
  TRY.
      " Criar instância do log writer
      lo_log_writer = /aif/cl_appl_log_writer=>get_instance(
        iv_obj        = 'ZAPP_LOG'                           " Seu objeto na SLG0
        iv_subobj     = 'ZUSER_CRUD'                         " Seu subobjeto
        iv_extnumber  = |{ me->i_user }_{ sy-datum }{ sy-uzeit }|  " Número externo
      ).

      " ========================================
      " CABEÇALHO DO LOG
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty     = 'I'
        iv_msgid     = '00'
        iv_msgno     = '001'
        iv_msgv1     = '====== CRIAÇÃO DE USUÁRIO ======'
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty     = 'I'
        iv_msgid     = '00'
        iv_msgno     = '001'
        iv_msgv1     = |Usuário: { me->i_user }|
        iv_msgv2     = |Data: { sy-datum }|
        iv_msgv3     = |Hora: { sy-uzeit }|
        iv_msgv4     = |Por: { sy-uname }|
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty     = 'I'
        iv_msgid     = '00'
        iv_msgno     = '001'
        iv_msgv1     = |Email: { me->i_email }|
        iv_msgv2     = |Departamento: { me->i_departament }|
        iv_msgv3     = |Função: { me->i_function }|
        iv_msgv4     = |Empresa: { me->i_company }|
        iv_max_errors = 9999
      ).

      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty     = 'I'
        iv_msgid     = '00'
        iv_msgno     = '001'
        iv_msgv1     = |Válido de: { me->i_begda }|
        iv_msgv2     = |Válido até: { me->i_endda }|
        iv_max_errors = 9999
      ).

      " ========================================
      " PROCESSAR MENSAGENS DO GT_RETURN
      " ========================================
      IF gt_return IS NOT INITIAL.

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'I'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = '====== MENSAGENS DA BAPI ======'
          iv_max_errors = 9999
        ).

        " Adicionar cada mensagem do BAPIRET2
        LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<return>).

          lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
            iv_msgty     = <return>-type
            iv_msgid     = <return>-id
            iv_msgno     = <return>-number
            iv_msgv1     = <return>-message_v1
            iv_msgv2     = <return>-message_v2
            iv_msgv3     = <return>-message_v3
            iv_msgv4     = <return>-message_v4
            iv_max_errors = 9999
          ).

          " Adicionar detalhes extras se existirem
          IF <return>-parameter IS NOT INITIAL OR
             <return>-field IS NOT INITIAL OR
             <return>-row IS NOT INITIAL OR
             <return>-system IS NOT INITIAL.

            lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
              iv_msgty     = 'I'
              iv_msgid     = '00'
              iv_msgno     = '001'
              iv_msgv1     = |Parâmetro: { <return>-parameter }|
              iv_msgv2     = |Campo: { <return>-field }|
              iv_msgv3     = |Linha: { <return>-row }|
              iv_msgv4     = |Sistema: { <return>-system }|
              iv_max_errors = 9999
            ).
          ENDIF.

          IF <return>-log_no IS NOT INITIAL.
            lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
              iv_msgty     = 'I'
              iv_msgid     = '00'
              iv_msgno     = '001'
              iv_msgv1     = |Log No: { <return>-log_no }|
              iv_msgv2     = |Log Msg No: { <return>-log_msg_no }|
              iv_max_errors = 9999
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

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'I'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = '====== RESUMO ======'
          iv_max_errors = 9999
        ).

        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'I'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = |Total: { lv_total }|
          iv_msgv2     = |Erros: { lv_errors }|
          iv_msgv3     = |Avisos: { lv_warnings }|
          iv_msgv4     = |Sucessos: { lv_success }|
          iv_max_errors = 9999
        ).

      ELSE.
        " Nenhuma mensagem retornada
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'W'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = 'Nenhuma mensagem retornada pela BAPI'
          iv_max_errors = 9999
        ).
      ENDIF.

      " ========================================
      " RESULTADO FINAL
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
        iv_msgty     = 'I'
        iv_msgid     = '00'
        iv_msgno     = '001'
        iv_msgv1     = '================================'
        iv_max_errors = 9999
      ).

      IF lv_has_error = abap_true.
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'E'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = 'RESULTADO: FALHA'
          iv_msgv2     = |Usuário { me->i_user } NÃO criado|
          iv_max_errors = 9999
        ).
      ELSE.
        lo_log_writer->/aif/if_appl_log_writer~add_msg_to_log(
          iv_msgty     = 'S'
          iv_msgid     = '00'
          iv_msgno     = '001'
          iv_msgv1     = 'RESULTADO: SUCESSO'
          iv_msgv2     = |Usuário { me->i_user } criado|
          iv_max_errors = 9999
        ).
      ENDIF.

      " ========================================
      " SALVAR LOG NO BANCO
      " ========================================
      lo_log_writer->/aif/if_appl_log_writer~save_log( ).

      " Adicionar mensagem de sucesso ao gt_return
      APPEND VALUE #(
        type    = 'S'
        id      = '00'
        number  = '001'
        message = 'Application Log gravado com sucesso'
      ) TO gt_return.

    CATCH /aif/cx_error_handling_general INTO DATA(lx_aif_error).
      " Erro específico do AIF
      APPEND VALUE #(
        type    = 'E'
        id      = '00'
        number  = '001'
        message = |Erro AIF: { lx_aif_error->get_text( ) }|
      ) TO gt_return.

    CATCH cx_root INTO DATA(lx_error).
      " Erro genérico
      APPEND VALUE #(
        type    = 'E'
        id      = '00'
        number  = '001'
        message = |Erro ao gravar log: { lx_error->get_text( ) }|
      ) TO gt_return.
  ENDTRY.

ENDMETHOD.
