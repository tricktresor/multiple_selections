REPORT ztrcktrsr_multi_sel_menu.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS display.
    METHODS set_icons IMPORTING on  TYPE string
                                off TYPE string.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_option,
             value   TYPE char10,
             text    TYPE string,
             checked TYPE abap_bool,
           END OF ty_option.
    DATA: mytoolbar TYPE REF TO cl_gui_toolbar,
          options   TYPE STANDARD TABLE OF ty_option.
    DATA m_icon_on  TYPE string VALUE icon_led_green.
    DATA m_icon_off TYPE string VALUE icon_led_red.
    METHODS build_menu.
    METHODS on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode sender.

ENDCLASS.                    "lcl_my_event_handler DEFINITION


CLASS lcl_main IMPLEMENTATION.

  METHOD build_menu.

    DATA lv_icon TYPE icon_text.

    IF mytoolbar IS INITIAL.
      "Create docker on Top of the screen
      DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_left extension = 130 ).


      "create toolbar object
      mytoolbar = NEW #( parent       = docker
                         display_mode = cl_gui_toolbar=>m_mode_vertical ).

      "register events
      mytoolbar->set_registered_events( VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected ) ) ).

      "Set handler
      SET HANDLER on_function_selected FOR mytoolbar.
    ELSE.
      mytoolbar->delete_all_buttons( ).
    ENDIF.

    LOOP AT options ASSIGNING FIELD-SYMBOL(<option>).
      IF <option>-checked = abap_false.
        lv_icon = m_icon_off.
      ELSE.
        lv_icon = m_icon_on.
      ENDIF.
      "add menu entry with current status
      mytoolbar->add_button( fcode      = CONV #( <option>-value )
                             icon       = lv_icon
                             is_checked = <option>-checked
                             butn_type  = cntb_btype_button
                             text       = CONV #( <option>-text ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD display.


    "set initial values
    options = VALUE #( ( value = 'ONE'   text = 'Option One' )
                       ( value = 'TWO'   text = 'Option Two' )
                       ( value = 'THREE' text = 'Option Three' )
                       ( value = 'FOUR'  text = 'Option Four' ) ).
    "Build menu
    build_menu( ).


  ENDMETHOD.

  METHOD set_icons.

    m_icon_on  = on.
    m_icon_off = off.
    build_menu( ).

  ENDMETHOD.

  METHOD on_function_selected.

    ASSIGN options[ value = fcode ] TO FIELD-SYMBOL(<option>).
    IF sy-subrc = 0.
      IF <option>-checked = abap_true.
        <option>-checked = abap_false.
      ELSE.
        <option>-checked = abap_true.
      ENDIF.
    ENDIF.

    build_menu( ).

  ENDMETHOD.                    "lcl_my_event_handler



ENDCLASS.                    "lcl_my_event_handler IMPLEMENTATION


INITIALIZATION.

  DATA(main) = NEW lcl_main( ).
  main->display( ).


  PARAMETERS p_on  TYPE icon_d DEFAULT icon_okay   MATCHCODE OBJECT h_icon.
  PARAMETERS p_off TYPE icon_d DEFAULT icon_cancel MATCHCODE OBJECT h_icon.

AT SELECTION-SCREEN.
  main->set_icons(
          EXPORTING on  = conv #( p_on )
                    off = conv #( p_off ) ).
