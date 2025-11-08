CLASS zcl_application_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_object     TYPE balobj_d DEFAULT 'ZAPP_LOG'
        iv_subobject  TYPE balsubobj DEFAULT 'ZUSER_CRUD'
        iv_extnumber  TYPE balnrext OPTIONAL.

    METHODS add_message_from_bapiret2
      IMPORTING
        is_return TYPE bapiret2.

    METHODS add_message_text
      IMPORTING
        iv_text  TYPE string
        iv_msgty TYPE sy-msgty DEFAULT 'I'.

    METHODS save_log
      RETURNING
        VALUE(rv_log_number) TYPE balognr.

  PRIVATE SECTION.
    DATA: mv_log_handle TYPE balloghndl.

    METHODS create_log_header
      IMPORTING
        iv_object    TYPE balobj_d
        iv_subobject TYPE balsubobj
        iv_extnumber TYPE balnrext OPTIONAL.

ENDCLASS.

CLASS zcl_application_log IMPLEMENTATION.

  METHOD constructor.
    create_log_header(
      iv_object    = iv_object
      iv_subobject = iv_subobject
      iv_extnumber = iv_extnumber
    ).
  ENDMETHOD.

  METHOD create_log_header.
    DATA: ls_log_header TYPE bal_s_log.

    ls_log_header-object    = iv_object.
    ls_log_header-subobject = iv_subobject.
    ls_log_header-extnumber = iv_extnumber.
    ls_log_header-aluser    = sy-uname.
    ls_log_header-alprog    = sy-cprog.
    ls_log_header-altcode   = sy-tcode.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log_header
      IMPORTING
        e_log_handle = mv_log_handle
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.

  METHOD add_message_from_bapiret2.
    DATA: ls_msg TYPE bal_s_msg.

    ls_msg-msgty = is_return-type.
    ls_msg-msgid = is_return-id.
    ls_msg-msgno = is_return-number.
    ls_msg-msgv1 = is_return-message_v1.
    ls_msg-msgv2 = is_return-message_v2.
    ls_msg-msgv3 = is_return-message_v3.
    ls_msg-msgv4 = is_return-message_v4.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.

  METHOD add_message_text.
    DATA: ls_msg TYPE bal_s_msg.

    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = '00'.
    ls_msg-msgno = '001'.
    
    " Dividir texto em partes se for maior que 50 caracteres
    IF strlen( iv_text ) > 50.
      ls_msg-msgv1 = iv_text+0(50).
      IF strlen( iv_text ) > 100.
        ls_msg-msgv2 = iv_text+50(50).
        IF strlen( iv_text ) > 150.
          ls_msg-msgv3 = iv_text+100(50).
          IF strlen( iv_text ) > 200.
            ls_msg-msgv4 = iv_text+150(50).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      ls_msg-msgv1 = iv_text.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.

  METHOD save_log.
    DATA: lt_log_handle     TYPE bal_t_logh,
          lt_new_lognumbers TYPE bal_t_lgnm.

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

    IF sy-subrc = 0 AND lt_new_lognumbers IS NOT INITIAL.
      rv_log_number = lt_new_lognumbers[ 1 ]-lognumber.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.
