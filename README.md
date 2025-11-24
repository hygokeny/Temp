REPORT z_regenera_perfil.

PARAMETERS: p_role TYPE agr_name OBLIGATORY.

DATA: lt_auth   TYPE TABLE OF agr_1251,
      lt_aufld  TYPE TABLE OF agr_1252,
      lt_prof   TYPE TABLE OF agr_1016,
      lt_prof_old TYPE TABLE OF agr_1016,
      lt_prof_new TYPE TABLE OF agr_1016,
      lt_b_usaut TYPE STANDARD TABLE OF usobx,
      lt_b_usval TYPE STANDARD TABLE OF usobt,
      lv_profile_text TYPE text80,
      lv_actvt TYPE c VALUE '01',
      lv_dummy TYPE c,
      ls_prof TYPE agr_1016.

FIELD-SYMBOLS: <fs_prof> LIKE LINE OF lt_prof_new.

*---------------------------------------------------------------------*
* 1. Carrega AUTORIZAÇÕES atuais da role
*---------------------------------------------------------------------*
SELECT * FROM agr_1251 INTO TABLE lt_auth
  WHERE agr_name = p_role.

SELECT * FROM agr_1252 INTO TABLE lt_aufld
  WHERE agr_name = p_role.

*---------------------------------------------------------------------*
* 2. Geração das AUTORIZAÇÕES (PRGN_AUTH_ACTIVITY_GROUP)
*---------------------------------------------------------------------*
CALL FUNCTION 'PRGN_AUTH_ACTIVITY_GROUP'
  EXPORTING
    activity_group  = p_role
    action_generate = 'X'
    message_output  = space
  EXCEPTIONS
    not_authorized  = 1
    OTHERS          = 2.

IF sy-subrc <> 0.
  MESSAGE 'Erro na geração de autorizações (PRGN_AUTH_ACTIVITY_GROUP)' TYPE 'E'.
ENDIF.

*---------------------------------------------------------------------*
* 3. Monta tabelas b_usaut / b_usval
*---------------------------------------------------------------------*
CLEAR lt_b_usaut.
CLEAR lt_b_usval.

" Autorizacoes (AGR_1251 -> USOBX)
LOOP AT lt_auth WHERE deleted = space.
  DATA(ls_autx) = VALUE usobx(
      action = '01'
      objct  = lt_auth-object
      auth   = lt_auth-auth
      atext  = lt_auth-atext ).
  APPEND ls_autx TO lt_b_usaut.
ENDLOOP.

" Valores (AGR_1252 -> USOBT)
LOOP AT lt_aufld WHERE deleted = space.
  DATA(ls_val) = VALUE usobt(
      objct  = lt_aufld-object
      auth   = lt_aufld-auth
      sfield = lt_aufld-field
      von    = lt_aufld-low
      bis    = lt_aufld-high ).
  APPEND ls_val TO lt_b_usval.
ENDLOOP.

*---------------------------------------------------------------------*
* 4. Corrige entradas via SUPRN_CORRECT_INPUT_TABLES
*---------------------------------------------------------------------*
CALL FUNCTION 'SUPRN_CORRECT_INPUT_TABLES'
  TABLES
    values = lt_b_usval
    auths  = lt_b_usaut.

*---------------------------------------------------------------------*
* 5. Gera PERFIL TÉCNICO via SUSR_INTERFACE_PROF
*---------------------------------------------------------------------*
DATA(lv_stellen_profile) = ''.  " perfil atual (opcional)
DATA(lv_proftext) = |Perfil gerado por Z_REG_PERFIL { p_role }|.

CALL FUNCTION 'SUSR_INTERFACE_PROF'
  EXPORTING
    profile                 = lv_stellen_profile
    ptext                   = lv_proftext
    action                  = '01'
    no_check_in_create_mode = 'X'
    no_check_in_update_mode = 'X'
    dialog                  = space
  TABLES
    values                  = lt_b_usval
    auths                   = lt_b_usaut
    prof_in                 = lt_prof_old
    prof_out                = lt_prof_new
  EXCEPTIONS
    OTHERS                  = 1.

IF sy-subrc <> 0.
  MESSAGE 'Erro na SUSR_INTERFACE_PROF ao gerar perfil técnico' TYPE 'E'.
ENDIF.

*---------------------------------------------------------------------*
* 6. Atualiza AGR_1016 com novo perfil
*---------------------------------------------------------------------*
IF lt_prof_new IS NOT INITIAL.
  lt_prof = lt_prof_new.
ELSE.
  CLEAR ls_prof.
  ls_prof-profile = lv_stellen_profile.
  ls_prof-generated = 'X'.
  ls_prof-pstate = 'A'.
  APPEND ls_prof TO lt_prof.
ENDIF.

CALL FUNCTION 'PRGN_1016_SAVE_PROFILE_NAME'
  EXPORTING
    activity_group = p_role
  TABLES
    i_prof         = lt_prof.

*---------------------------------------------------------------------*
* 7. PRGN_UPDATE_DATABASE
*---------------------------------------------------------------------*
CALL FUNCTION 'PRGN_UPDATE_DATABASE'.

*---------------------------------------------------------------------*
* 8. Timestamp de geração
*---------------------------------------------------------------------*
CALL FUNCTION 'PRGN_SET_GENERATE_TIMESTAMP'
  EXPORTING
    activity_group = p_role.

WRITE: / 'Perfil regenerado com sucesso para role:', p_role.

