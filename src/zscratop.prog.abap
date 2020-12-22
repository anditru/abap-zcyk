*&---------------------------------------------------------------------*
*& Include ZSCRATOP                                 - Module Pool      ZSCREENA
*&---------------------------------------------------------------------*
PROGRAM ZSCREENA.

TYPES: BEGIN OF ts_productions,
        selected TYPE char1,
        nonterminal TYPE string,
        generates TYPE string,
       END OF ts_productions,

       BEGIN OF ts_grammar,
         io_nonterminals TYPE string,
         io_terminals TYPE string,
         io_start_symbol TYPE string,
       END OF ts_grammar.

DATA:
      gt_productions TYPE TABLE OF ts_productions,
      gs_wa_productions TYPE ts_productions,
      gs_grammar TYPE ts_grammar,
      io_word_to_check TYPE string.
