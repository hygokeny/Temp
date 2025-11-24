REPORT z_generate_pfcg_profile NO STANDARD PAGE HEADING.

PARAMETERS p_role TYPE agr_name OBLIGATORY.

"--------------------------------------------------------------
" IMPORTANTE: Todas variáveis globais da SAPLSUPRN foram
" declaradas lá dentro. Então aqui nós só vamos preencher
" os globais do function-pool via IMPORT/EXPORT MEMORY.
"--------------------------------------------------------------

DATA: gen    TYPE c,
      dialog TYPE c VALUE ' ',
      ret    TYPE sysubrc.

"--------------------------------------------------------------
" 1. Preencher global_act_objid
"--------------------------------------------------------------
EXPORT global_act_objid = p_role            TO MEMORY ID 'PFCG_OBJID'.

"--------------------------------------------------------------
" 2. Carregar dados da ROLE como a PFCG faz
"--------------------------------------------------------------
PERFORM agr_read IN PROGRAM SAPLSUPRN USING p_role ret.
IF ret NE 0.
  WRITE: / 'Erro lendo role:', p_role.
  EXIT.
ENDIF.

" agr_read preenche automaticamente:
" i_auth, i_au_fld, i_prof, s_stat, estado, textos, torso, etc.

"--------------------------------------------------------------
" 3. Chamar o FORM ORIGINAL de geração de perfil
"--------------------------------------------------------------
PERFORM act_generate_profile IN PROGRAM SAPLSUPRN USING dialog gen.

IF gen = 'X'.
  WRITE: / 'Perfil gerado com sucesso para role:', p_role.
ELSE.
  WRITE: / 'Falha ao gerar perfil para role:', p_role, 'GEN=', gen.
ENDIF.

"--------------------------------------------------------------
" 4. Commit final
"--------------------------------------------------------------
COMMIT WORK.
