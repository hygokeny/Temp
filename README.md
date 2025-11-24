DATA: dialog TYPE c VALUE ' ',
      gen    TYPE c.

"--------------------------------------------
" 1. Informar role (global suposta pelo SUPRN)
"--------------------------------------------
global_act_objid = p_role.

"--------------------------------------------
" 2. Ler role como a PFCG interna
"--------------------------------------------
PERFORM stelle_read IN PROGRAM SAPLSUPRN.

"--------------------------------------------
" 3. Gerar perfil como a PFCG
"--------------------------------------------
PERFORM act_generate_profile IN PROGRAM SAPLSUPRN USING dialog gen.

IF gen = 'X'.
  WRITE: / 'Perfil gerado para role:', p_role.
ELSE.
  WRITE: / 'Falha ao gerar o perfil da role:', p_role, ' GEN=', gen.
ENDIF.
