class CL_BAL_LOG definition
  public
  final
  create public .

public section.

  methods CREATE
    importing
      !IV_SBSDC type OIO_RT_SBSDC_ID optional .
  methods ADD_MESSAGE
    importing
      !IV_MSGTY type SYST_MSGTY
      !IV_MSGID type SYST_MSGID
      !IV_MSGNO type SYST_MSGNO
      !IV_MSGV1 type SYST_MSGV
      !IV_MSGV2 type SYST_MSGV
      !IV_MSGV3 type SYST_MSGV
      !IV_MSGV4 type SYST_MSGV .
  methods REFRESH .
  methods SAVE .
  methods READ
    importing
      !IV_LOG_HANDLE type BALLOGHNDL
    exporting
      !ET_MSG type BAL_T_MSG .
  methods MERGE_LOG .
protected section.
private section.

  data GV_LOG_HANDLE type BALLOGHNDL .
  data GV_MSGSV_MAX type N .
  data GV_MSGTY_MAX type BAPI_MTYPE .
ENDCLASS.



CLASS CL_BAL_LOG IMPLEMENTATION.


  METHOD add_message.

*   local data
    DATA:
      lv_msgsv       TYPE n,
      ls_msg         TYPE bal_s_msg.

*------------------------------------------------------------
* Application log
*------------------------------------------------------------

*   Message parameters
    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgno = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.

*   Interpret message type
    CASE iv_msgty.
      WHEN 'X'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_x.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_very_high.
      WHEN 'A'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_a.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_very_high.
      WHEN 'E'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_e.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_high.
      WHEN 'W'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_w.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_medium.
      WHEN 'I'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_i.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_low.
      WHEN 'S'.
        lv_msgsv          = if_oio_oil_constants=>gc_msgsv_s.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_low.
      WHEN OTHERS.
        ls_msg-probclass = if_oio_oil_constants=>gc_probclass_low.
    ENDCASE.

*   Maximum severity
    IF lv_msgsv > gv_msgsv_max.
      gv_msgsv_max = lv_msgsv.
      gv_msgty_max = iv_msgty.
    ENDIF.

*   Add message to log file
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = gv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA:
      ls_prof               TYPE bal_s_prof,
*      ls_msg_handle         type balmsghndl,
      ls_log                TYPE bal_s_log.
*      ls_log_profile        type bal_s_prof.

*----------------------------------------------------------------
*  Create log instance
*----------------------------------------------------------------

*   define header data for application log
    ls_log-object           = if_oio_oil_constants=>gc_returns_subs.
    ls_log-subobject        = space.
    ls_log-aldate           = sy-datum.
    ls_log-altime           = sy-uzeit.
    ls_log-aluser           = sy-uname.
    ls_log-altcode          = sy-tcode.
    ls_log-alprog           = sy-repid.
    ls_log-almode         =  'D'.     "Dialog
    ls_log-aldate_del       = sy-datum.
    ls_log-aldate_del       = ls_log-aldate_del + 31.
    ls_log-aldate_del+6(2)  = sy-datum+6(2).   "One month's time
    ls_log-del_before       = if_oio_oil_constants=>gc_true.

*   create a log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = gv_log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*----------------------------------------------------------------
*   profile for popup
*----------------------------------------------------------------
*   Get standard profile for popup
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_prof.

  ENDMETHOD.


  METHOD merge_log.

    DATA: lv_log_handle  TYPE balloghndl,
          ls_msg        TYPE bal_s_msg,
          lt_msg        TYPE bal_t_msg.
*
*    ls_logh_from = i_logh.
    CALL METHOD read
      EXPORTING
        iv_log_handle = lv_log_handle
      IMPORTING
        et_msg        = lt_msg.

    LOOP AT lt_msg INTO ls_msg.
      CALL METHOD me->add_message
        EXPORTING
          iv_msgty = ls_msg-msgty
          iv_msgid = ls_msg-msgid
          iv_msgno = ls_msg-msgno
          iv_msgv1 = ls_msg-msgv1
          iv_msgv2 = ls_msg-msgv2
          iv_msgv3 = ls_msg-msgv3
          iv_msgv4 = ls_msg-msgv4.
    ENDLOOP.

  ENDMETHOD.


  METHOD read.

    DATA:
        lt_log_handle   TYPE bal_t_logh,
        lv_msg_handle   TYPE balmsghndl,
        lt_msg_handle   TYPE bal_t_msgh,
        ls_msg          TYPE bal_s_msg,
        lt_msg          TYPE bal_t_msg.

*   get messages in log
    APPEND iv_log_handle TO lt_log_handle.
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_log_handle
      IMPORTING
        e_t_msg_handle = lt_msg_handle
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.

*   If messages found append them to global application log
    IF sy-subrc = 0.
      LOOP AT lt_msg_handle INTO lv_msg_handle.
        CALL FUNCTION 'BAL_LOG_MSG_READ'
          EXPORTING
            i_s_msg_handle = lv_msg_handle
          IMPORTING
            e_s_msg        = ls_msg
          EXCEPTIONS
            msg_not_found  = 1
            OTHERS         = 2.

        APPEND ls_msg TO lt_msg.

      ENDLOOP.
    ENDIF.
    et_msg[] = lt_msg[].

  ENDMETHOD.


  METHOD refresh.

    CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
      EXPORTING
        i_log_handle  = gv_log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR:
      gv_msgsv_max,
      gv_msgty_max.

  ENDMETHOD.


  METHOD save.

    DATA: lt_log_handle TYPE bal_t_logh.

    IF gv_msgsv_max IS INITIAL.
      RETURN.
    ENDIF.

* save message log
    APPEND gv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all       = 'X'
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
