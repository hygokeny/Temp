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
      gv_role       TYPE agr_name,
      gv_gen        TYPE char01,
      gv_dialog     TYPE char01,
      gv_ret        TYPE sysubrc.

* Variables from SAPLSUPRN
DATA: stellen_profile TYPE xuprofname,
      profile_text    TYPE xutext,
      profil_vergeben TYPE char01,
      global_act_objid TYPE agr_name.

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

DATA: actvt(2) TYPE c,
      var      TYPE xuvariable,
      ret      TYPE sysubrc.

* Constants
CONSTANTS: x            VALUE 'X',
           space        VALUE ' ',
           state_altered VALUE 'A'.

* Field symbols
FIELD-SYMBOLS: <prof> TYPE uspro,
               <1016> TYPE pt1016.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_role   TYPE agr_name OBLIGATORY,
            p_dialog TYPE char01 AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Textos da selection screen

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  gv_role = p_role.
  
  " Set dialog mode
  IF p_dialog = x.
    gv_dialog = x.
  ELSE.
    gv_dialog = space.
  ENDIF.

  " Execute profile regeneration
  PERFORM regenerate_role_profile USING gv_role
                                        gv_dialog
                                  CHANGING gv_gen
                                           gv_ret.

  " Display results
  IF gv_ret = 0.
    gs_role-agr_name = gv_role.
    gs_role-status = 'SUCCESS'.
    gs_role-message = 'Perfil regenerado com sucesso'.
    APPEND gs_role TO gt_roles.
    
    WRITE: / 'Role:', gv_role.
    WRITE: / 'Status: Sucesso'.
    WRITE: / 'Perfil regenerado com sucesso'.
  ELSE.
    gs_role-agr_name = gv_role.
    gs_role-status = 'ERROR'.
    gs_role-message = 'Erro ao regenerar perfil'.
    APPEND gs_role TO gt_roles.
    
    WRITE: / 'Role:', gv_role.
    WRITE: / 'Status: Erro'.
    WRITE: / 'Erro ao regenerar perfil'.
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

  " Check if system is productive
  CALL FUNCTION 'PRGN_CHECK_SYSTEM_PRODUCTIVE'
    EXCEPTIONS
      client_is_productive = 1
      OTHERS               = 2.

  IF sy-subrc = 1.
    IF iv_dialog = x.
      MESSAGE i000(prgn) WITH 'Sistema produtivo - cuidado!'.
    ENDIF.
  ENDIF.

  " Check authorization
  PERFORM check_authorization USING iv_role
                               CHANGING lv_ret.
  IF lv_ret NE 0.
    cv_ret = lv_ret.
    cv_gen = 'A'.
    RETURN.
  ENDIF.

  " Load role data
  PERFORM load_role_data USING iv_role
                         CHANGING lv_ret.
  IF lv_ret NE 0.
    cv_ret = lv_ret.
    cv_gen = space.
    RETURN.
  ENDIF.

  " Generate profile using standard form
  PERFORM act_generate_profile IN PROGRAM saplsuprn
                                USING iv_dialog
                                CHANGING cv_gen.

  IF cv_gen = x OR cv_gen = space.
    cv_ret = 0.
  ELSE.
    cv_ret = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORIZATION
*&---------------------------------------------------------------------*
FORM check_authorization USING    iv_role TYPE agr_name
                         CHANGING cv_ret  TYPE sysubrc.

  DATA: lv_ret TYPE sysubrc.

  " Check authorization for profile generation
  PERFORM aut_chks_4_role IN PROGRAM saplprgn_auth
                          USING    iv_role 'G'
                          CHANGING lv_ret.
  
  IF lv_ret NE 0.
    MESSAGE e425(s#) WITH iv_role.
    cv_ret = lv_ret.
  ELSE.
    cv_ret = 0.
  ENDIF.

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

  " Load authorization data
  PERFORM load_auth_data USING iv_role
                         CHANGING lv_ret.
  
  cv_ret = lv_ret.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_AUTH_DATA
*&---------------------------------------------------------------------*
FORM load_auth_data USING    iv_role TYPE agr_name
                    CHANGING cv_ret  TYPE sysubrc.

  " Clear authorization tables
  REFRESH: b_usaut, b_usval, i_auth, i_au_fld.

  " Read authorizations from AGR_1250
  SELECT * FROM agr_1250
    INTO CORRESPONDING FIELDS OF TABLE i_auth
    WHERE agr_name = iv_role
      AND deleted  = space.

  IF sy-subrc NE 0.
    actvt = '06'.  " No authorizations found
    cv_ret = 0.
    RETURN.
  ENDIF.

  " Read authorization fields from AGR_1251
  SELECT * FROM agr_1251
    INTO CORRESPONDING FIELDS OF TABLE i_au_fld
    WHERE agr_name = iv_role
      AND deleted  = space.

  IF sy-subrc = 0.
    actvt = '01'.  " Update mode
    
    " Prepare authorization data for SUSR_INTERFACE_PROF
    PERFORM prepare_auth_tables CHANGING cv_ret.
  ELSE.
    actvt = '06'.  " Create mode
    cv_ret = 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_AUTH_TABLES
*&---------------------------------------------------------------------*
FORM prepare_auth_tables CHANGING cv_ret TYPE sysubrc.

  DATA: lv_var TYPE xuvariable,
        lv_ret TYPE sysubrc.

  " Build b_usaut table (authorization objects)
  LOOP AT i_auth WHERE deleted = space.
    b_usaut-action = '01'.
    b_usaut-objct  = i_auth-object.
    b_usaut-auth   = i_auth-auth.
    b_usaut-atext  = i_auth-atext.
    APPEND b_usaut.
  ENDLOOP.

  " Build b_usval table (field values)
  LOOP AT i_au_fld WHERE deleted = space.
    b_usval-objct  = i_au_fld-object.
    b_usval-auth   = i_au_fld-auth.
    b_usval-sfield = i_au_fld-field.
    b_usval-von    = i_au_fld-low.
    b_usval-bis    = i_au_fld-high.
    
    " Handle organizational level variables
    PERFORM is_org IN PROGRAM saplsuprn 
                   USING i_au_fld-field lv_var lv_ret.
    IF lv_ret EQ 0 AND b_usval-von EQ lv_var.
      CLEAR: b_usval-von, b_usval-bis.
      APPEND b_usval.
      CONTINUE.
    ENDIF.
    
    " Handle special characters
    IF b_usval-von EQ '''' OR b_usval-von EQ ''''''.
      CLEAR b_usval-von.
    ENDIF.
    IF b_usval-bis EQ '''' OR b_usval-bis EQ ''''''.
      CLEAR b_usval-bis.
    ENDIF.
    
    APPEND b_usval.
  ENDLOOP.

  " Correct input tables
  CALL FUNCTION 'SUPRN_CORRECT_INPUT_TABLES'
    TABLES
      values = b_usval
      auths  = b_usaut.

  cv_ret = 0.

ENDFORM.
