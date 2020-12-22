*&---------------------------------------------------------------------*
*& Include ZCYKTOP                                  - Module Pool      ZCYK_ALG
*&---------------------------------------------------------------------*
PROGRAM ZCYK_ALG.

TYPES: BEGIN OF ts_production_global,
        selected TYPE char1,
        nonterminal TYPE string,
        generates TYPE string,
       END OF ts_production_global,

       BEGIN OF ts_grammar,
         io_nonterminals TYPE string,
         io_terminals TYPE string,
         io_start_symbol TYPE string,
       END OF ts_grammar.

DATA:
      gt_productions TYPE TABLE OF ts_production_global,
      gs_wa_productions TYPE ts_production_global,
      gs_grammar TYPE ts_grammar,
      io_word_to_check TYPE string.
