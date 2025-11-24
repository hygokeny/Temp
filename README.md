*&---------------------------------------------------------------------*
*& Report Z_REGENERATE_ROLE_PROFILE
*&---------------------------------------------------------------------*
*& Programa para regenerar perfis de roles SAP
*& Baseado no programa standard SAPLSUPRN
*&---------------------------------------------------------------------*
REPORT z_regenerate_role_profile.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_role,
         agr_name TYPE agr_name,
         status   TYPE char20,
         message  TYPE char100,
       END OF ty_role.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_roles      TYPE TABLE OF ty_role,
      gs_role       TYPE ty_role,
      gv_gen        TYPE char01,
      gv_dialog     TYPE char01,
      gv_ret        TYPE sysubrc.

* Variables from SAPLSUPRN
DATA: stellen_profile TYPE xuprofname,
      profile_text    TYPE xutext,
      profil_vergeben TYPE char01,
      global_act_objid TYPE agr_name,
      state_save      TYPE char01.

* Authorization data tables
DATA: b_usval TYPE TABLE OF usval WITH HEADER LINE,
      b_usaut TYPE TABLE OF usaut WITH HEADER LINE.

DATA: lt_1016_old  TYPE TABLE OF pt1016,
      lt_1016_new  TYPE TABLE OF pt1016,
      lt_profs_old TYPE TABLE OF uspro,
      lt_profs_new TYPE TABLE OF uspro.

DATA: i_auth   TYPE TABLE OF agr_1250 WITH HEADER LINE,
      i_au_fld TYPE TABLE OF agr_1251 WITH HEADER LINE,
      i_prof   TYPE TABLE OF pt1016 WITH HEADER LINE.

DATA: actvt(2)  TYPE c,
      var       TYPE xuvariable,
      ret       TYPE sysubrc,
      lv_exit_flag TYPE char01.

* Constants from SAPLSUPRN
CONSTANTS: x                VALUE 'X',
           space            VALUE ' ',
           state_altered    VALUE 'A',
           state_saved      VALUE 'S',
           state_gen        VALUE 'G',
           leerstring(3)    VALUE ''' ''',
           zwei_hochkomma(2) VALUE '''''',
           ein_hochkomma(1) VALUE ''''.

* Field symbols
FIELD-SYMBOLS: <prof> TYPE uspro,
               <1016> TYPE pt1016.

* Structure for role status
DATA: BEGIN OF s_stat,
        objid TYPE agr_name,
      END OF s_stat.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_role FOR global_act_objid OBLIGATORY.
PARAMETERS: p_dialog TYPE char01 AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Text Elements
*----------------------------------------------------------------------*
* TEXT-001: 'Regeneração de Perfil de Role'

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Textos da selection screen

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON s_role.
  " Validar se as roles existem
  DATA: lv_role_check TYPE agr_name.
  
  LOOP AT s_role.
    SELECT SINGLE agr_name FROM agr_define
      INTO lv_role_check
      WHERE agr_name IN s_role.
    IF sy-subrc NE 0.
      MESSAGE e001(00) WITH 'Role não encontrada:' s_role-low.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Set dialog mode
  IF p_dialog = x.
    gv_dialog = x.
  ELSE.
    gv_dialog = space.
  ENDIF.

  " Process each role in selection
  DATA: lv_role_count TYPE i,
        lv_success    TYPE i,
        lv_error      TYPE i.

  CLEAR: lv_role_count, lv_success, lv_error.

  " Get roles from selection
  SELECT agr_name FROM agr_define
    INTO TABLE @DATA(lt_roles)
    WHERE agr_name IN @s_role
    ORDER BY agr_name.

  DESCRIBE TABLE lt_roles LINES lv_role_count.

  IF lv_role_count = 0.
    MESSAGE s001(00) WITH 'Nenhuma role encontrada' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Process each role
  LOOP AT lt_roles INTO DATA(ls_role_name).
    
    global_act_objid = ls_role_name-agr_name.
    s_stat-objid = ls_role_name-agr_name.

    " Execute profile regeneration
    PERFORM regenerate_role_profile USING ls_role_name-agr_name
                                          gv_dialog
                                    CHANGING gv_gen
                                             gv_ret.

    " Collect results
    CLEAR gs_role.
    gs_role-agr_name = ls_role_name-agr_name.

    IF gv_ret = 0 AND gv_gen = x.
      gs_role-status = 'SUCCESS'.
      gs_role-message = 'Perfil regenerado com sucesso'.
      ADD 1 TO lv_success.
    ELSE.
      gs_role-status = 'ERROR'.
      CASE gv_gen.
        WHEN 'A'.
          gs_role-message = 'Erro: Sem autorização'.
        WHEN space.
          gs_role-message = 'Erro: Ação cancelada'.
        WHEN OTHERS.
          gs_role-message = 'Erro ao regenerar perfil'.
      ENDCASE.
      ADD 1 TO lv_error.
    ENDIF.

    APPEND gs_role TO gt_roles.

  ENDLOOP.

*----------------------------------------------------------------------*
* End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  " Display results
  IF gt_roles IS NOT INITIAL.
    
    WRITE: / '========================================================='.
    WRITE: / 'Resultado da Regeneração de Perfis'.
    WRITE: / '========================================================='.
    WRITE: / 'Total de Roles processadas:', lv_role_count.
    WRITE: / 'Sucesso:', lv_success.
    WRITE: / 'Erro:', lv_error.
    WRITE: / '========================================================='.
    SKIP 1.

    LOOP AT gt_roles INTO gs_role.
      WRITE: / 'Role:', gs_role-agr_name COLOR COL_KEY.
      
      IF gs_role-status = 'SUCCESS'.
        WRITE: / 'Status:', gs_role-status COLOR COL_POSITIVE.
      ELSE.
        WRITE: / 'Status:', gs_role-status COLOR COL_NEGATIVE.
      ENDIF.
      
      WRITE: / 'Mensagem:', gs_role-message.
      WRITE: / '---------------------------------------------------------'.
    ENDLOOP.

    SKIP 1.
    WRITE: / '========================================================='.

    " Display summary message
    IF lv_error = 0.
      MESSAGE s018(5@) WITH 'Todas as roles regeneradas com sucesso'.
    ELSE.
      MESSAGE s001(00) WITH lv_error 'role(s) com erro' DISPLAY LIKE 'W'.
    ENDIF.

  ELSE.
    WRITE: / 'Nenhuma role processada.'.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form REGENERATE_ROLE_PROFILE
*&---------------------------------------------------------------------*
FORM regenerate_role_profile USING    iv_role   TYPE agr_name
                                      iv_dialog TYPE char01
                             CHANGING cv_gen    TYPE char01
                                      cv_ret    TYPE sysubrc.

  DATA: lv_exit_flag TYPE char01,
        lv_ret       TYPE sysubrc.

  " Set global role ID
  global_act_objid = iv_role.
  s_stat-objid = iv_role.

  " Show warning for regenerate
  PERFORM show_warning_for_regenerate USING    global_act_objid
                                      CHANGING lv_exit_flag.
  IF lv_exit_flag EQ x.
    MESSAGE s001(5@).              "Aktion abgebrochen ...
    cv_gen = space.
    cv_ret = 4.
    RETURN.
  ENDIF.

  " Check authorization for profile generation
  PERFORM aut_chks_4_role IN PROGRAM saplprgn_auth
                          USING    global_act_objid 'G'
                          CHANGING ret.
  IF ret NE 0.
    IF iv_dialog EQ x.
      MESSAGE e425(s#) WITH s_stat-objid.
    ENDIF.
    cv_gen = 'A'.
    cv_ret = ret.
    RETURN.
  ENDIF.

  " Set generation flag
  cv_gen = x.

  " Load role data
  PERFORM load_role_data USING iv_role
                         CHANGING lv_ret.
  IF lv_ret NE 0.
    cv_ret = lv_ret.
    cv_gen = space.
    RETURN.
  ENDIF.

  " Prepare authorization data
  PERFORM prepare_authorization_data USING iv_dialog
                                     CHANGING lv_ret.
  IF lv_ret NE 0.
    cv_ret = lv_ret.
    cv_gen = space.
    RETURN.
  ENDIF.

  " Generate profile
  PERFORM generate_profile USING iv_dialog
                           CHANGING cv_gen lv_ret.

  cv_ret = lv_ret.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_ROLE_DATA
*&---------------------------------------------------------------------*
FORM load_role_data USING    iv_role TYPE agr_name
                    CHANGING cv_ret  TYPE sysubrc.

  DATA: lv_ret TYPE sysubrc.

  " Get profile name
  CLEAR: stellen_profile, profile_text, profil_vergeben.

  CALL FUNCTION 'PRGN_PROFILE_NAME_GET'
    EXPORTING
      act_objid            = iv_role
    IMPORTING
      act_profile_name     = stellen_profile
      act_prof_text        = profile_text
      profile_from_tprprof = profil_vergeben
    EXCEPTIONS
      OTHERS               = 1.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    cv_ret = sy-subrc.
    RETURN.
  ENDIF.

  " Load profiles from AGR_1016
  REFRESH i_prof.
  SELECT * FROM agr_1016
    INTO CORRESPONDING FIELDS OF TABLE i_prof
    WHERE agr_name = iv_role.

  " The authorization data must contain the profile torso throughout.
  IF profil_vergeben EQ x.
    LOOP AT i_prof ASSIGNING <1016>
                   WHERE profile(10) NE stellen_profile.
      <1016>-profile(10) = stellen_profile.
      state_save = state_altered.
    ENDLOOP.
  ENDIF.

  " Load authorizations from AGR_1250
  REFRESH i_auth.
  SELECT * FROM agr_1250
    INTO CORRESPONDING FIELDS OF TABLE i_auth
    WHERE agr_name = iv_role
      AND deleted  = space.

  " Load authorization fields from AGR_1251
  REFRESH i_au_fld.
  SELECT * FROM agr_1251
    INTO CORRESPONDING FIELDS OF TABLE i_au_fld
    WHERE agr_name = iv_role
      AND deleted  = space.

  cv_ret = 0.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_AUTHORIZATION_DATA
*&---------------------------------------------------------------------*
FORM prepare_authorization_data USING    iv_dialog TYPE char01
                                CHANGING cv_ret    TYPE sysubrc.

  DATA: lv_var TYPE xuvariable,
        lv_ret TYPE sysubrc.

  " Copy of the authorization data of the role
  REFRESH: b_usaut, b_usval.

  " Build b_usaut table (authorization objects)
  LOOP AT i_auth WHERE deleted = space.
    b_usaut-action = '01'.
    b_usaut-objct  = i_auth-object.
    b_usaut-auth   = i_auth-auth.
    b_usaut-atext  = i_auth-atext.
    APPEND b_usaut.
  ENDLOOP.

  IF sy-subrc NE 0.
    actvt = '06'.
    cv_ret = 0.
    RETURN.
  ELSE.
    actvt = '01'.
  ENDIF.

  " Build b_usval table (field values)
  LOOP AT i_au_fld WHERE deleted = space.
    b_usval-objct  = i_au_fld-object.
    b_usval-auth   = i_au_fld-auth.
    b_usval-sfield = i_au_fld-field.
    b_usval-von    = i_au_fld-low.
    b_usval-bis    = i_au_fld-high.

    " Check if it's an organizational level field
    PERFORM is_org IN PROGRAM saplsuprn
                   USING i_au_fld-field var ret.
    IF ret EQ 0 AND b_usval-von EQ var.
      " Removing the technical identifier of org fields; Otherwise it
      " would be copied to the generated authorization.
      CLEAR: b_usval-von, b_usval-bis.
      APPEND b_usval.
      CONTINUE.
    ENDIF.

    " Handle special characters in VON field
    IF b_usval-von EQ ein_hochkomma  OR
       b_usval-von EQ zwei_hochkomma.
      CLEAR b_usval-von.
    ENDIF.

    " Handle special characters in BIS field
    IF b_usval-bis EQ leerstring     OR
       b_usval-bis EQ ein_hochkomma  OR
       b_usval-bis EQ zwei_hochkomma.
      CLEAR b_usval-bis.
    ENDIF.

    APPEND b_usval.
  ENDLOOP.

  " Insertion of the string ' ' in VON fields containing space.
  CALL FUNCTION 'SUPRN_CORRECT_INPUT_TABLES'
    TABLES
      values = b_usval
      auths  = b_usaut.

  cv_ret = 0.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GENERATE_PROFILE
*&---------------------------------------------------------------------*
FORM generate_profile USING    iv_dialog TYPE char01
                      CHANGING cv_gen    TYPE char01
                               cv_ret    TYPE sysubrc.

  DATA: l_ssm_cust TYPE ssm_cust.

  " Clear tables for profile generation
  CLEAR: lt_profs_old, lt_profs_new, lt_1016_old, lt_1016_new.

  " Profile generation
  CALL FUNCTION 'SUSR_INTERFACE_PROF'
    EXPORTING
      profile                    = stellen_profile
      ptext                      = profile_text
      action                     = actvt
      no_check_in_create_mode    = 'X'
      no_check_in_update_mode    = 'X'
      dialog                     = iv_dialog
    TABLES
      values                     = b_usval
      auths                      = b_usaut
      prof_in                    = lt_profs_old
      prof_out                   = lt_profs_new
    EXCEPTIONS
      not_authorized_for_auth    = 1
      not_authorized_for_profile = 2
      OTHERS                     = 3.

  IF sy-subrc EQ 0.
    " Process old profiles
    LOOP AT lt_profs_old ASSIGNING <prof> WHERE pstate = 'A'.
      APPEND INITIAL LINE TO lt_1016_old ASSIGNING <1016>.
      <1016>-profile   = <prof>-profn.
      <1016>-generated = 'X'.
      <1016>-pstate    = <prof>-pstate.
    ENDLOOP.

    " Process new profiles
    LOOP AT lt_profs_new ASSIGNING <prof> WHERE pstate = 'A'.
      APPEND INITIAL LINE TO lt_1016_new ASSIGNING <1016>.
      <1016>-profile   = <prof>-profn.
      <1016>-generated = 'X'.
      <1016>-pstate    = <prof>-pstate.
    ENDLOOP.

    " Copy of generated profiles into the global table
    REFRESH i_prof.
    IF NOT lt_1016_new IS INITIAL.
      i_prof[] = lt_1016_new.
    ELSE.
      " Even though the profile data was deleted one entry in AGR_1016 must survive.
      CLEAR i_prof.
      i_prof-profile   = stellen_profile.
      i_prof-generated = x.
      i_prof-pstate    = 'A'.
      APPEND i_prof.
    ENDIF.

    " Updating table AGR_1016
    PERFORM db_save_1016 IN PROGRAM saplsuprn USING x.

    IF iv_dialog EQ x.
      " Success message
      MESSAGE s018(5@).
    ENDIF.

    " Database update
    CALL FUNCTION 'PRGN_UPDATE_DATABASE'.

    " Update of the time stamp for the profile generation
    IF lt_1016_old EQ lt_1016_new.
      CALL FUNCTION 'PRGN_SET_GENERATE_TIMESTAMP'
        EXPORTING
          activity_group = s_stat-objid.
    ELSE.
      CALL FUNCTION 'PRGN_SET_GENERATE_TIMESTAMP'
        EXPORTING
          activity_group = s_stat-objid
          user_comp      = 'U'.
      " Adjustment of the profile assignments to users
      PERFORM check_and_update_users IN PROGRAM saplsuprn
                                     TABLES lt_1016_old lt_1016_new
                                     USING  global_act_objid.
    ENDIF.

    state_save = state_gen.
    cv_gen = x.
    cv_ret = 0.

    " Exits for SAP components and customers
    CLEAR l_ssm_cust.
    SELECT SINGLE * FROM ssm_cust INTO l_ssm_cust
           WHERE id = 'SAP_AFTER_PROF_GEN'.
    IF sy-subrc = 0 AND l_ssm_cust-path <> space.
      CALL FUNCTION l_ssm_cust-path
        EXPORTING
          activity_group = global_act_objid
          generated      = cv_gen.
    ENDIF.

    CLEAR l_ssm_cust.
    SELECT SINGLE * FROM ssm_cust INTO l_ssm_cust
           WHERE id = 'Z_AFTER_PROF_GEN'.
    IF sy-subrc = 0 AND l_ssm_cust-path <> space.
      CALL FUNCTION l_ssm_cust-path
        EXPORTING
          activity_group = global_act_objid
          generated      = cv_gen.
    ENDIF.

  ELSE.
    MESSAGE s336(s#) WITH s_stat-objid DISPLAY LIKE 'E'.
    CASE sy-subrc.
      WHEN 1 OR 2.
        " No authorization for maintaining the profile or the authorizations
        cv_gen = 'A'.
      WHEN OTHERS.
        cv_gen = space.
    ENDCASE.
    cv_ret = sy-subrc.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHOW_WARNING_FOR_REGENERATE
*&---------------------------------------------------------------------*
FORM show_warning_for_regenerate USING    iv_agr_name   TYPE agr_name
                                 CHANGING cv_exit_flag  TYPE char01.

  " Esta rotina pode ser chamada do programa standard
  " ou implementada aqui se necessário

  " Por enquanto, não exibir warning em modo batch
  cv_exit_flag = space.

ENDFORM.
