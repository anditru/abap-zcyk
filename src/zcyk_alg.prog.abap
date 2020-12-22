*&---------------------------------------------------------------------*
*& Module Pool      ZCYK_ALG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE zcyktop                                 .    " Global Data

* INCLUDE ZCYKO01                                 .  " PBO-Modules
* INCLUDE ZCYKI01                                 .  " PAI-Modules
* INCLUDE ZCYKF01                                 .  " FORM-Routines

*&SPWIZARD: DECLARATION OF TABLECONTROL 'T_PRODUCTIONS' ITSELF
CONTROLS: t_productions TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'T_PRODUCTIONS'
DATA:     g_t_productions_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

MODULE screen_0100_set_title OUTPUT.
  SET TITLEBAR 'CYK_ALGORITHM'.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'T_PRODUCTIONS'. DO NOT CHANGE THIS LIN
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE t_productions_change_tc_attr OUTPUT.
  IF gt_productions IS INITIAL.
* Fill gt_productions with empty lines because the table control wizard is stupid or maybe I am stupid
    DO 6 TIMES.
      APPEND gs_wa_productions TO gt_productions.
    ENDDO.
   ENDIF.
   DESCRIBE TABLE gt_productions LINES t_productions-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'T_PRODUCTIONS'. DO NOT CHANGE THIS LIN
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE t_productions_get_lines OUTPUT.
  g_t_productions_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'T_PRODUCTIONS'. DO NOT CHANGE THIS LINE
*&SPWIZARD: MODIFY TABLE
MODULE t_productions_modify INPUT.
  MODIFY gt_productions
    FROM gs_wa_productions
    INDEX t_productions-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'T_PRODUCTIONS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE t_productions_mark INPUT.
  DATA: g_t_productions_wa2 LIKE LINE OF gt_productions.
  IF t_productions-line_sel_mode = 1
  AND gs_wa_productions-selected = 'X'.
    LOOP AT gt_productions INTO g_t_productions_wa2
      WHERE selected = 'X'.
      g_t_productions_wa2-selected = ''.
      MODIFY gt_productions
        FROM g_t_productions_wa2
        TRANSPORTING selected.
    ENDLOOP.
  ENDIF.
  MODIFY gt_productions
    FROM gs_wa_productions
    INDEX t_productions-current_line
    TRANSPORTING selected.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'T_PRODUCTIONS'. DO NOT CHANGE THIS LINE
*&SPWIZARD: PROCESS USER COMMAND
MODULE t_productions_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'T_PRODUCTIONS'
                              'GT_PRODUCTIONS'
                              'SELECTED'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Module  SCREEN_0100_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_0100_user_command INPUT.
  TYPES: BEGIN OF ts_symbol_list,
          symbol TYPE c LENGTH 50,
         END OF ts_symbol_list,

         BEGIN OF ts_matrix,
           line TYPE i,
           column TYPE i,
           values TYPE STANDARD TABLE OF ts_symbol_list WITH NON-UNIQUE DEFAULT KEY,
         END OF ts_matrix,

         BEGIN OF ts_production,
           generates TYPE string,
           nonterminal TYPE string,
         END OF ts_production.

  DATA:
        lt_wa TYPE TABLE OF ts_symbol_list,
        lt_combinations TYPE TABLE OF ts_symbol_list.


  CASE sy-ucomm.
  WHEN 'CHECK_WORD'.
    "Get terminals
    DATA lt_terminals TYPE TABLE OF ts_symbol_list.
    SPLIT gs_grammar-io_terminals AT ' ' INTO TABLE lt_terminals.

    "Get nonterminals
    DATA lt_nonterminals TYPE TABLE OF ts_symbol_list.
    SPLIT gs_grammar-io_nonterminals AT ' ' INTO TABLE lt_nonterminals.

    "Check validty to start symbol
    READ TABLE lt_nonterminals WITH KEY symbol = gs_grammar-io_start_symbol TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.

    "Get word to check
    DATA(lv_n) = strlen( io_word_to_check ). "Uffbasse: lv_n = Length of the word
    DATA lt_word TYPE TABLE OF ts_symbol_list.
    DO lv_n TIMES.
      DATA(ls_symbol) = VALUE ts_symbol_list( symbol = substring( val = io_word_to_check off = sy-index - 1 len = 1 ) ).
      APPEND ls_symbol TO lt_word.
    ENDDO.

    "Get productions and remove shit
    DATA:
          lt_productions TYPE HASHED TABLE OF ts_production
            WITH UNIQUE KEY primary_key COMPONENTS generates nonterminal
            WITH NON-UNIQUE SORTED KEY secondary_key COMPONENTS generates,
          ls_production TYPE ts_production.

    LOOP AT gt_productions INTO DATA(ls_production_global).
      IF ls_production_global IS NOT INITIAL.
        MOVE-CORRESPONDING ls_production_global TO ls_production.
        INSERT ls_production INTO TABLE lt_productions.
      ENDIF.
    ENDLOOP.

    "Initialize pyramid
    DATA:
          lt_pyramid TYPE TABLE OF ts_matrix,
          ls_pyramid_line TYPE ts_matrix.
    DATA(lv_line) = 0.
    DATA(lv_column) = 0.
    DO lv_n TIMES.
      DO lv_n TIMES.
        ls_pyramid_line = VALUE #( line = lv_line column = lv_column ).
        APPEND ls_pyramid_line TO lt_pyramid.
        lv_column = lv_column + 1.
      ENDDO.
      lv_column = 0.
      lv_line = lv_line + 1.
    ENDDO.

    CLEAR ls_symbol.

    "Fill bottom line of pyramid
    DATA(lv_i) = 0.
    WHILE lv_i LE lv_n - 1.
      "Get next symbol in word to check
      READ TABLE lt_word INDEX lv_i + 1 INTO ls_symbol.
      "Get all nonterminals generating the respective symbol
      LOOP AT lt_productions ASSIGNING FIELD-SYMBOL(<fs_production>) USING KEY secondary_key WHERE generates = ls_symbol-symbol.
        DATA(ls_nonterminal) = VALUE ts_symbol_list( symbol = <fs_production>-nonterminal ).
        APPEND ls_nonterminal TO lt_wa.
      ENDLOOP.
      "Write the nonterminals to the pyramid
      READ TABLE lt_pyramid WITH KEY line = lv_i column = lv_i ASSIGNING FIELD-SYMBOL(<fs_pyramid_line>).
      APPEND LINES OF lt_wa TO <fs_pyramid_line>-values.
      CLEAR lt_wa.
      lv_i = lv_i + 1.
    ENDWHILE.

    "Do the rest
    DATA(lv_k) = 2.
    WHILE lv_k LE lv_n.
      lv_i = 0.
      WHILE lv_i LE lv_n - lv_k.
        CLEAR lt_wa.
        DATA(lv_m) = lv_i + 1.
        WHILE lv_m LE lv_i + lv_k - 1.
          "Get D[i,m] (lt_d1) and D[m, i+k] (lt_d2)
          READ TABLE lt_pyramid WITH KEY line = lv_i column = lv_m - 1 INTO DATA(ls_d1).
          READ TABLE lt_pyramid WITH KEY line = lv_m column = lv_i + lv_k - 1 INTO DATA(ls_d2).
          "Compute all combinations of nonterminals in D[i,m] and D[m, i+k]
          LOOP AT ls_d1-values ASSIGNING FIELD-SYMBOL(<fs_d1>).
            LOOP AT ls_d2-values ASSIGNING FIELD-SYMBOL(<fs_d2>).
              CONCATENATE <fs_d1> <fs_d2> INTO DATA(lv_combination).
              "Get all nonterminals generating lv_combination
              LOOP AT lt_productions ASSIGNING <fs_production> USING KEY secondary_key WHERE generates = lv_combination.
                ls_nonterminal = VALUE ts_symbol_list( symbol = <fs_production>-nonterminal ).
                APPEND ls_nonterminal TO lt_wa.
                CLEAR ls_nonterminal.
              ENDLOOP.
            ENDLOOP.
          ENDLOOP.
          "Write the nonterminals to the pyramid
          READ TABLE lt_pyramid WITH KEY line = lv_i column = lv_i + lv_k - 1 ASSIGNING <fs_pyramid_line>.
          APPEND LINES OF lt_wa TO <fs_pyramid_line>-values.
          lv_m = lv_m + 1.
        ENDWHILE.
        lv_i = lv_i + 1.
      ENDWHILE.
      lv_k = lv_k + 1.
    ENDWHILE.

    "View result
    READ TABLE lt_pyramid WITH KEY line = 0 column = lv_n - 1 ASSIGNING <fs_pyramid_line>.
    READ TABLE <fs_pyramid_line>-values WITH KEY symbol = gs_grammar-io_start_symbol TRANSPORTING NO FIELDS.
    DATA lv_message TYPE string.
    IF sy-subrc = 0.
      CONCATENATE io_word_to_check ' is in the language.' INTO lv_message.
    ELSE.
      CONCATENATE io_word_to_check ' is not in the language.' INTO lv_message.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = lv_message
        display_cancel_button = abap_false.

    "Clear everything
    CLEAR:
      lt_word,
      lt_terminals,
      lt_productions,
      lt_pyramid,
      lt_wa.

  ENDCASE.
ENDMODULE.
