REPORT z_regenera_perfil.

PARAMETERS p_role TYPE agr_name OBLIGATORY.

"-------------------------------------------------------------
" Tabelas e Work Areas
"-------------------------------------------------------------
DATA: lt_agr1251    TYPE TABLE OF agr_1251,
      ls_agr1251    TYPE agr_1251,

      lt_agr1252    TYPE TABLE OF agr_1252,
      ls_agr1252    TYPE agr_1252,

      lt_usobx      TYPE TABLE OF usobx,
      ls_usobx      TYPE usobx,

      lt_usobt      TYPE TABLE OF usobt,
      ls_usobt      TYPE usobt,

      lt_profs_old  TYPE TABLE OF uspro,
      lt_profs_new  TYPE TABLE OF uspro,

      lt_i_prof     TYPE TABLE OF pt1016,
      ls_i_prof     TYPE pt1016,

      lt_return     TYPE bapirettab,
      ls_ret        TYPE bapiret2.

DATA lv_profile_text TYPE char80.
DATA lv_pstate       TYPE xuaktpas.
DATA lv_dummy        TYPE char1.

"-------------------------------------------------------------
" 1. Carregar AGR_1251 (objetos + autorização + valores)
"-------------------------------------------------------------
SELECT * FROM agr_1251
  INTO TABLE lt_agr1251
  WHERE agr_name = p_role
    AND deleted = space.

IF sy-subrc <> 0.
  WRITE: / 'Role não encontrada na AGR_1251:', p_role.
  EXIT.
ENDIF.

"-------------------------------------------------------------
" 2. Carregar AGR_1252 (org levels)
"-------------------------------------------------------------
SELECT * FROM agr_1252
  INTO TABLE lt_agr1252
  WHERE agr_name = p_role.

"-------------------------------------------------------------
" 3. Gerar AUTORIZAÇÕES (PRGN_AUTH_ACTIVITY_GROUP)
"-------------------------------------------------------------
CALL FUNCTION 'PRGN_AUTH_ACTIVITY_GROUP'
  EXPORTING
    activity_group  = p_role
    action_generate = 'X'
    message_output  = space
  EXCEPTIONS
    not_authorized  = 1
    OTHERS          = 2.

IF sy-subrc <> 0.
  WRITE: / 'Erro em PRGN_AUTH_ACTIVITY_GROUP.'.
  EXIT.
ENDIF.

"-------------------------------------------------------------
" 4. Montar USOBX (objetos)
"-------------------------------------------------------------
CLEAR lt_usobx.

LOOP AT lt_agr1251 INTO ls_agr1251.

  CLEAR ls_usobx.

  ls_usobx-name    = p_role.
  ls_usobx-type    = 'A'.         "Tipo antigo de AUTH
  ls_usobx-object  = ls_agr1251-object.
  ls_usobx-okflag  = 'Y'.

  APPEND ls_usobx TO lt_usobx.

ENDLOOP.

"-------------------------------------------------------------
" 5. Montar USOBT (valores)
"-------------------------------------------------------------
CLEAR lt_usobt.

" Valores vindos da AGR_1251
LOOP AT lt_agr1251 INTO ls_agr1251.

  IF ls_agr1251-field IS NOT INITIAL.

    CLEAR ls_usobt.

    ls_usobt-name   = p_role.
    ls_usobt-type   = 'A'. "Mesmo tipo dos AUTH antigos
    ls_usobt-object = ls_agr1251-object.
    ls_usobt-field  = ls_agr1251-field.
    ls_usobt-low    = ls_agr1251-low.
    ls_usobt-high   = ls_agr1251-high.

    APPEND ls_usobt TO lt_usobt.

  ENDIF.

ENDLOOP.

" Valores da AGR_1252 (org levels)
LOOP AT lt_agr1252 INTO ls_agr1252.

  CLEAR ls_usobt.

  ls_usobt-name   = p_role.
  ls_usobt-type   = 'A'.
  ls_usobt-field  = ls_agr1252-varbl.
  ls_usobt-low    = ls_agr1252-low.
  ls_usobt-high   = ls_agr1252-high.

  APPEND ls_usobt TO lt_usobt.

ENDLOOP.

"-------------------------------------------------------------
" 6. Sanitizar tabelas (SUPRN_CORRECT_INPUT_TABLES)
"-------------------------------------------------------------
CALL FUNCTION 'SUPRN_CORRECT_INPUT_TABLES'
  TABLES
    values = lt_usobt
    auths  = lt_usobx.

"-------------------------------------------------------------
" 7. GERAR PERFIL TÉCNICO (SUSR_INTERFACE_PROF)
"-------------------------------------------------------------
lv_profile_text = |Perfil gerado automaticamente para role { p_role }|.

CALL FUNCTION 'SUSR_INTERFACE_PROF'
  EXPORTING
    profile                 = p_role    "perfil = role
    ptext                   = lv_profile_text
    action                  = '01'
    no_check_in_create_mode = 'X'
    no_check_in_update_mode = 'X'
    dialog                  = space
  IMPORTING
    pstate                  = lv_pstate
    return                  = lt_return
  TABLES
    values                  = lt_usobt
    auths                   = lt_usobx
    prof_in                 = lt_profs_old
    prof_out                = lt_profs_new
  EXCEPTIONS
    OTHERS                  = 1.

IF sy-subrc <> 0.
  WRITE: / 'Erro em SUSR_INTERFACE_PROF.'.
  EXIT.
ENDIF.

"-------------------------------------------------------------
" 8. Atualizar AGR_1016
"-------------------------------------------------------------
CLEAR lt_i_prof.

LOOP AT lt_profs_new ASSIGNING FIELD-SYMBOL(<prof>).

  CLEAR ls_i_prof.
  ls_i_prof-profile   = <prof>-profn.
  ls_i_prof-generated = 'X'.
  ls_i_prof-pstate    = <prof>-pstate.

  APPEND ls_i_prof TO lt_i_prof.

ENDLOOP.

CALL FUNCTION 'PRGN_1016_SAVE_PROFILE_NAME'
  EXPORTING
    activity_group = p_role
  TABLES
    i_prof         = lt_i_prof.

"-------------------------------------------------------------
" 9. Persistir tudo no banco
"-------------------------------------------------------------
CALL FUNCTION 'PRGN_UPDATE_DATABASE'.

"-------------------------------------------------------------
" 10. Atualizar timestamp de geração
"-------------------------------------------------------------
CALL FUNCTION 'PRGN_SET_GENERATE_TIMESTAMP'
  EXPORTING
    activity_group = p_role.

"-------------------------------------------------------------
" 11. Done
"-------------------------------------------------------------
WRITE: / 'Perfil regenerado com sucesso para a role:', p_role.
