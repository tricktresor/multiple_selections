REPORT ZTRCKTRSR_MULTI_SEL_CTMENU.


CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS display.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_option,
             value   TYPE char10,
             text    TYPE string,
             checked TYPE boolean_flg,
           END OF ty_option.
    DATA: mytoolbar    TYPE REF TO cl_gui_toolbar,
          menupos_x    TYPE i,
          menupos_y    TYPE i,
          options      TYPE STANDARD TABLE OF ty_option,
          menu_dynamic TYPE REF TO cl_ctmenu.
    METHODS build_menu.
    METHODS on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode sender.
    METHODS on_dropdown_clicked  FOR EVENT dropdown_clicked OF cl_gui_toolbar
      IMPORTING fcode posx posy sender.
ENDCLASS.                    "lcl_my_event_handler DEFINITION



CLASS lcl_main IMPLEMENTATION.

  METHOD build_menu.

    IF menu_dynamic IS INITIAL.
      "Create menu
      CREATE OBJECT menu_dynamic.
    ELSE.
      "Clear all entries before rebuild
      menu_dynamic->clear( ).
    ENDIF.

    LOOP AT options ASSIGNING FIELD-SYMBOL(<option>).
      "add menu entry with current status
      menu_dynamic->add_function( fcode   = CONV #( <option>-value )
                                  checked = <option>-checked
                                  text    = CONV #( <option>-text ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    "Create docker on Top of the screen
    DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_top extension = 30 ).

    "create toolbar object
    mytoolbar = NEW #( parent = docker ).

    "register events
    mytoolbar->set_registered_events( VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected )
                                               ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked ) ) ).

    "Set handler
    SET HANDLER on_function_selected FOR mytoolbar.
    SET HANDLER on_dropdown_clicked  FOR mytoolbar.

    "set initial values
    options = VALUE #( ( value = 'ONE'   text = 'Option One' )
                       ( value = 'TWO'   text = 'Option Two' )
                       ( value = 'THREE' text = 'Option Three' )
                       ( value = 'FOUR'  text = 'Option Four' ) ).
    "Build menu
    build_menu( ).

    "Add button for selecting options
    mytoolbar->add_button( EXPORTING
                             icon             = 'ICON_TOOL'
                             fcode            = 'CHOOSE'
                             butn_type        = '1'
                             text             = 'Select options'
                             quickinfo        = 'Select some options...'
                           EXCEPTIONS
                             cntb_error_fcode = 1 ).

  ENDMETHOD.

  METHOD on_function_selected.

    "switch option entry
    LOOP AT options ASSIGNING FIELD-SYMBOL(<option>).
      IF <option>-value = fcode.
        IF <option>-checked = abap_true.
          <option>-checked = abap_false.
        ELSE.
          <option>-checked = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "rebuild menu
    build_menu( ).

    "raise event dropdown clicked again
    sender->dispatch( cargo = 'mytoolbar' eventid = cl_gui_toolbar=>m_id_dropdown_clicked is_shellevent = abap_false ).

    "Set coordinates of menu
    sender->track_context_menu(
         context_menu = menu_dynamic
         posx         = menupos_x
         posy         = menupos_y ).

  ENDMETHOD.                    "lcl_my_event_handler

  METHOD on_dropdown_clicked.

    IF fcode = 'CHOOSE'.
      "call of dropdown: remember current position for displaying menu
      menupos_x = posx.
      menupos_y = posy.
    ENDIF.

    "Set coordinates
    mytoolbar->track_context_menu(
        context_menu = menu_dynamic
        posx         = posx
        posy         = posy ).

  ENDMETHOD.                    "lcl_my_event_handler

ENDCLASS.                    "lcl_my_event_handler IMPLEMENTATION


INITIALIZATION.

  new lcl_main( )->display( ).


  PARAMETERS p_test.