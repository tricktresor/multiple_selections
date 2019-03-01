REPORT ZTRCKTRSR_MULTI_SEL_SALV.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_option,
             mark  TYPE boolean_flg,
             icon  type icon_d,
             key   TYPE c LENGTH 10,
             text  TYPE c LENGTH 100,
             _col_ TYPE lvc_t_scol,
           END OF ty_option,
           ty_options TYPE STANDARD TABLE OF ty_option with DEFAULT KEY.
    METHODS set IMPORTING options TYPE ty_options.
    METHODS get RETURNING VALUE(options) TYPE ty_options.
    METHODS display.
  PROTECTED SECTION.
    DATA mt_options TYPE ty_options.
    DATA mo_salv TYPE REF TO cl_salv_table.
    METHODS on_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column sender.
    METHODS set_colors.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD set.
    mt_options = options.
    set_colors( ).
  ENDMETHOD.

  METHOD get.
    options = mt_options.
  ENDMETHOD.

  METHOD display.

    DATA(docker) = NEW cl_gui_docking_container( extension = 200 side = cl_gui_docking_container=>dock_at_left ).

    DATA o_column TYPE REF TO cl_salv_column_table.

    cl_salv_table=>factory( EXPORTING r_container = docker
                            IMPORTING r_salv_table = mo_salv
                            CHANGING t_table = mt_options ).

    DATA(layout)  = mo_salv->get_display_settings( ).
    layout->set_list_header( 'Select option' ).



    DATA(columns) = mo_salv->get_columns( ).
    columns->set_color_column( '_COL_' ).
    columns->set_headers_visible( abap_false ).

    o_column ?= columns->get_column( 'MARK' ).
*    o_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
    o_column->set_technical( abap_true ).

    o_column ?= columns->get_column( 'ICON' ).
    o_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    o_column->set_icon( abap_true ).

    o_column ?= columns->get_column( 'KEY' ).
    o_column->set_technical( abap_true ).

    o_column ?= columns->get_column( 'TEXT' ).

    mo_salv->display( ).

    DATA(handler) = mo_salv->get_event( ).
    SET HANDLER on_click FOR handler.


  ENDMETHOD.
  METHOD on_click.

    READ TABLE mt_options ASSIGNING FIELD-SYMBOL(<option>) INDEX row.
    IF sy-subrc = 0.
      IF <option>-mark = abap_true.
        <option>-mark = abap_false.
         <option>-icon = icon_led_red.
        CLEAR <option>-_col_.
        APPEND INITIAL LINE TO <option>-_col_ ASSIGNING FIELD-SYMBOL(<col>).
*        <col>-color-col = col_background.
      ELSE.
        <option>-mark  = abap_true.
         <option>-icon = icon_led_green.
        CLEAR <option>-_col_.
        APPEND INITIAL LINE TO <option>-_col_ ASSIGNING <col>.
*        <col>-color-col = col_positive.
      ENDIF.
    ENDIF.

    mo_salv->refresh( ).
    DATA(selections) = mo_salv->get_selections( ).
    selections->set_selected_cells( VALUE #( ) ).


  ENDMETHOD.

  METHOD set_colors.

    LOOP AT mt_options ASSIGNING FIELD-SYMBOL(<option>).
      CLEAR <option>-_col_.
      APPEND INITIAL LINE TO <option>-_col_ ASSIGNING FIELD-SYMBOL(<col>).
      IF <option>-mark = abap_false.
*        <col>-color-col = col_background.
        <option>-icon = icon_led_red.
      ELSE.
*        <col>-color-col = col_positive.
        <option>-icon = icon_led_green.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
PARAMETERS p_dummy.

INITIALIZATION.

  DATA(main) = NEW lcl_main( ).
  main->set( VALUE #( ( text = `One`   key = '1' )
                      ( text = `Two`   key = '2' )
                      ( text = `Three` key = '3'  )
                      ( text = `Four`  key = '4'  )
                     ) ).
  main->display( ).

AT SELECTION-SCREEN.
  cl_demo_output=>display_data( main->get( ) ).
