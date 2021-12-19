*&---------------------------------------------------------------------*
*& Report zhttped
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhttped.

TYPES: BEGIN OF ty_header_field,
         key   TYPE string,
         value TYPE string,
       END OF ty_header_field,
       ty_header_fields TYPE STANDARD TABLE OF ty_header_field WITH EMPTY KEY,
       BEGIN OF ty_item,
         name TYPE string,
         BEGIN OF request,
           method        TYPE string,
           header_fields TYPE ty_header_fields,
           body          TYPE string,
           url           TYPE string,
         END OF request,
       END OF ty_item,
       ty_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY,
       BEGIN OF ty_postman,
         items TYPE ty_items,
       END OF ty_postman.

CLASS lcl_dialogbox DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        title         TYPE csequence
      RETURNING
        VALUE(result) TYPE REF TO lcl_dialogbox.
    METHODS get_container
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_container.
  PRIVATE SECTION.
    METHODS on_close
      FOR EVENT close OF cl_gui_dialogbox_container IMPORTING
      sender.
    DATA: dialogbox TYPE REF TO cl_gui_dialogbox_container.
ENDCLASS.
CLASS lcl_dialogbox IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT result.
    CREATE OBJECT result->dialogbox
      EXPORTING
        width   = 540
        height  = 100
        top     = 150
        left    = 150
        repid   = sy-repid
        dynnr   = sy-dynnr
        caption = title.
    SET HANDLER result->on_close FOR result->dialogbox.
  ENDMETHOD.
  METHOD on_close.
    CALL METHOD sender->free.
  ENDMETHOD.
  METHOD get_container.
    result = dialogbox.
  ENDMETHOD.
ENDCLASS.
CLASS lcx_app DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        text     TYPE string OPTIONAL
        previous LIKE previous OPTIONAL
        textid   LIKE textid OPTIONAL.
    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.
  PRIVATE SECTION.
    DATA: text TYPE string.
ENDCLASS.

CLASS lcx_app IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        textid   = textid
        previous = previous ).
    me->text = text.
  ENDMETHOD.
  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.
  METHOD get_text.
    result = text.
    DATA(lx) = previous.
    WHILE lx IS BOUND.
      result = |{ result } — { lx->get_text( ) }|.
      lx = lx->previous.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_root DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS lcl_root IMPLEMENTATION.
ENDCLASS.


CLASS lcl_request DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        request TYPE REF TO if_http_entity.
    DATA request TYPE REF TO if_http_entity READ-ONLY.
ENDCLASS.
CLASS lcl_request IMPLEMENTATION.
  METHOD constructor.
    me->request = request.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_header_fields DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS lcl_header_fields IMPLEMENTATION.
ENDCLASS.


CLASS lcl_body DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS lcl_body IMPLEMENTATION.
ENDCLASS.


CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS set_screen_information
      IMPORTING
        ref_sscrfields TYPE REF TO sscrfields
        container      TYPE REF TO cl_gui_container.

    METHODS at_selection_screen_output.

    METHODS at_selection_screen
      RAISING
        lcx_app.

  PRIVATE SECTION.

    METHODS load_file
      IMPORTING
        file_path     TYPE file_table-filename
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS write_bin_file
      IMPORTING
        i_filename     TYPE csequence
        i_file_xstring TYPE xstring.

    DATA: ref_sscrfields TYPE REF TO sscrfields.

ENDCLASS.


CLASS lcl_http_tree DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        container          TYPE REF TO cl_gui_container
        postman_collection TYPE ty_postman
      RETURNING
        VALUE(result)      TYPE REF TO lcl_http_tree.

    METHODS show.

  PRIVATE SECTION.

    METHODS add_request
      IMPORTING
        request         TYPE REF TO if_http_entity
        description     TYPE string
        parent_node_key TYPE string
      RETURNING
        VALUE(result)   TYPE string.

    METHODS get_new_node_key
      RETURNING
        VALUE(result) TYPE string.

    METHODS create_request.

    METHODS node_get_user_object
      IMPORTING
        node_key      TYPE string
      RETURNING
        VALUE(result) TYPE REF TO object.

    METHODS copy_request
      IMPORTING
        request TYPE REF TO if_http_entity
        to      TYPE REF TO lcl_body.
    METHODS postman_item_to_request
      IMPORTING
        postman_item  TYPE ty_item
      RETURNING
        VALUE(result) TYPE REF TO if_http_entity.

    METHODS handle_node_context_menu_req
                  FOR EVENT node_context_menu_request OF cl_tree_model
      IMPORTING node_key menu.

    METHODS handle_node_context_menu_sel
                  FOR EVENT node_context_menu_select OF cl_tree_model
      IMPORTING node_key fcode.

    METHODS on_drag
                  FOR EVENT drag OF cl_column_tree_model
      IMPORTING node_key item_name drag_drop_object.

    METHODS on_drop
                  FOR EVENT drop OF cl_tree_model
      IMPORTING node_key drag_drop_object.

    METHODS on_drop_get_flavor
                  FOR EVENT drop_get_flavor OF cl_tree_model
      IMPORTING node_key drag_drop_object flavors.

    DATA:
      postman_collection         TYPE ty_postman,
      go_tree2                   TYPE REF TO cl_column_tree_model,
*          go_splitter_container      TYPE REF TO cl_gui_splitter_container,
*          go_container_left          TYPE REF TO cl_gui_container,
      l_node_key                 TYPE string,
      l_node_key_2               TYPE string,
      lt_column                  TYPE treemcitab,
      ls_column                  TYPE treemcitem,
      ref_sscrfields             TYPE REF TO sscrfields,
      container                  TYPE REF TO cl_gui_container,
      next_node_key              TYPE i VALUE 1,
      dnd_request_to_body_src    TYPE REF TO cl_dragdrop,
      dnd_request_to_body_src_id TYPE i,
      dnd_request_to_body_tgt_id TYPE i,
      dnd_request_to_body_tgt    TYPE REF TO cl_dragdrop.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.

    ref_sscrfields->functxt_01 = 'Load file'.
    ref_sscrfields->functxt_02 = 'New file'.

    DATA(exclude_functions) = VALUE ui_functions( ( 'ONLI' ) ( 'PRIN' ) ( 'SPOS' ) ).
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = exclude_functions.

    LOOP AT SCREEN INTO DATA(screen_line).
      screen_line-active = '0'.
      MODIFY SCREEN FROM screen_line.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_screen_information.

    me->ref_sscrfields = ref_sscrfields.

  ENDMETHOD.


  METHOD at_selection_screen.

    DATA: lt_filetable TYPE filetable,
          l_rc         TYPE i,
          l_action     TYPE i,
          file_name    TYPE string,
          file_path    TYPE string,
          full_path    TYPE string,
          tree         TYPE REF TO lcl_http_tree.
    FIELD-SYMBOLS:
      <ls_file> TYPE file_table.

    CASE ref_sscrfields->ucomm.

      WHEN 'FC01'.
        " Load existing file
        cl_gui_frontend_services=>file_open_dialog(
          EXPORTING
            window_title            = 'Open existing Postman file'
            default_filename        = 'Fiori.postman_collection.json'
            file_filter             = '.json'
            initial_directory       = 'C:\Users\sandra.rossi\Accenture\LPP - Local - Documents\General\OData'
          CHANGING
            file_table              = lt_filetable
            rc                      = l_rc
            user_action             = l_action
          EXCEPTIONS
            file_open_dialog_failed = 1
            cntl_error              = 2
            error_no_gui            = 3
            not_supported_by_gui    = 4
            OTHERS                  = 5 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_app EXPORTING text = 'save dialog'.
        ENDIF.
        IF l_action = cl_gui_frontend_services=>action_ok.
          READ TABLE lt_filetable INDEX 1 ASSIGNING <ls_file>.
          IF sy-subrc = 0.
            DATA(result) = load_file( file_path = <ls_file>-filename ).
            DATA(postman_collection) = VALUE ty_postman( ).
            TRY.
                CALL TRANSFORMATION zpostman_collection SOURCE XML result RESULT root = postman_collection.
              CATCH cx_root INTO DATA(lx).
                RAISE EXCEPTION TYPE lcx_app EXPORTING text = 'transfo' previous = lx.
            ENDTRY.
            tree = lcl_http_tree=>create(
                container          = lcl_dialogbox=>create( title = <ls_file>-filename )->get_container( )
                postman_collection = postman_collection ).
            tree->show( ).
          ENDIF.
        ENDIF.

      WHEN 'FC02'.
        " Create new file
        cl_gui_frontend_services=>file_save_dialog(
          EXPORTING
            window_title            = 'Open existing Postman file'
            default_file_name       = 'XXXXX.postman_collection.json'
            file_filter             = '.json'
            initial_directory       = 'C:\Users\sandra.rossi\Accenture\LPP - Local - Documents\General\OData'
            prompt_on_overwrite     = abap_true
          CHANGING
            filename                  = file_name
            path                      = file_path
            fullpath                  = full_path
            user_action               = l_action
          EXCEPTIONS
            cntl_error                = 1
            error_no_gui              = 2
            not_supported_by_gui      = 3
            invalid_default_file_name = 4
            OTHERS                    = 5 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_app EXPORTING text = 'save dialog'.
        ENDIF.
        IF l_action = cl_gui_frontend_services=>action_ok.
          TRY.
              CLEAR postman_collection.
              CALL TRANSFORMATION zpostman_collection SOURCE root = postman_collection RESULT XML DATA(xstring).
            CATCH cx_root INTO lx.
              RAISE EXCEPTION TYPE lcx_app EXPORTING text = 'transfo' previous = lx.
          ENDTRY.
          write_bin_file( i_filename = full_path i_file_xstring = xstring ).
          tree = lcl_http_tree=>create(
              container          = lcl_dialogbox=>create( title = full_path )->get_container( )
              postman_collection = postman_collection ).
          tree->show( ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD load_file.

    DATA: l_filename TYPE string,
          l_length   TYPE i,
          solix_tab  TYPE solix_tab.

    l_filename = file_path.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = l_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = l_length
      CHANGING
        data_tab   = solix_tab
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      "
    ENDIF.

    result = cl_bcs_convert=>solix_to_xstring(
        it_solix   = solix_tab
        iv_size    = l_length ).

  ENDMETHOD.

  METHOD write_bin_file.

    DATA lt_xstring TYPE TABLE OF x255.
    DATA l_length TYPE i.
    DATA l_filename TYPE string.

    l_filename = i_filename.

    CALL METHOD cl_swf_utl_convert_xstring=>xstring_to_table
      EXPORTING
        i_stream = i_file_xstring
      IMPORTING
        e_table  = lt_xstring
      EXCEPTIONS
        OTHERS   = 3.

    l_length = xstrlen( i_file_xstring ).

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = l_length
        filename     = l_filename
        filetype     = 'BIN'
      CHANGING
        data_tab     = lt_xstring
      EXCEPTIONS
        OTHERS       = 3.

  ENDMETHOD.


ENDCLASS.


CLASS lcl_http_tree IMPLEMENTATION.


  METHOD create.

    result = NEW lcl_http_tree( ).
    result->container = container.
    result->postman_collection = postman_collection.

  ENDMETHOD.


  METHOD add_request.

    IF request IS INSTANCE OF if_http_request.
      DATA(version) = CAST if_http_request( request )->get_method( ).
      DATA(url) = request->get_header_field( name = if_http_header_fields_sap=>path ).
    ENDIF.

    DATA(request_node_key) = get_new_node_key( ).
    result = request_node_key.
    go_tree2->add_node(
          node_key          = request_node_key
          relationship      = cl_tree_model=>relat_last_child
          relative_node_key = parent_node_key
          isfolder          = abap_true
          item_table        = VALUE #(
                              ( item_name = 'C1'
                                class     = cl_column_tree_model=>item_class_text
                                text      = |{ description } ({ request->get_content_type( ) })| ) )
          drag_drop_id      = dnd_request_to_body_src_id
          user_object       = NEW lcl_request( request ) ).

    IF version IS NOT INITIAL.
      go_tree2->add_node(
            node_key          = get_new_node_key( )
            relationship      = cl_tree_model=>relat_last_child
            relative_node_key = request_node_key
            isfolder          = abap_false
            item_table        = VALUE #(
                                ( item_name = 'C1'
                                  class     = cl_column_tree_model=>item_class_text
                                  text      = |{ version } { url }| ) )
            user_object       = NEW lcl_header_fields( ) ).
    ENDIF.

    go_tree2->add_node(
          node_key          = get_new_node_key( )
          relationship      = cl_tree_model=>relat_last_child
          relative_node_key = request_node_key
          isfolder          = abap_false
          item_table        = VALUE #(
                              ( item_name = 'C1'
                                class     = cl_column_tree_model=>item_class_text
                                text      = |header fields| ) )
          user_object       = NEW lcl_header_fields( ) ).

    DATA(body_node_key) = get_new_node_key( ).
    go_tree2->add_node(
          node_key          = body_node_key
          relationship      = cl_tree_model=>relat_last_child
          relative_node_key = request_node_key
          isfolder          = abap_false
          item_table        = VALUE #(
                              ( item_name = 'C1'
                                class     = cl_column_tree_model=>item_class_text
                                text      = |body| ) )
          drag_drop_id      = dnd_request_to_body_tgt_id
          user_object       = NEW lcl_body( ) ).

    IF request->get_content_type( ) = |application/http|.
      DATA(embedded_request) = NEW cl_http_request( ).
      embedded_request->from_xstring( request->get_data( ) ).
      add_request( request = embedded_request description = 'HTTP' parent_node_key = body_node_key ).
    ELSEIF request->num_multiparts( ) > 0.
      DO request->num_multiparts( ) TIMES.
        add_request( request = request->get_multipart( index = sy-index + 0 ) description = |Part { sy-index }| parent_node_key = body_node_key ).
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD get_new_node_key.

    result = |{ next_node_key }|.
    next_node_key = next_node_key + 1.

  ENDMETHOD.

  METHOD handle_node_context_menu_req.

    DATA(user_object) = node_get_user_object( node_key ).

    CASE TYPE OF user_object.

      WHEN TYPE lcl_root.

        menu->add_function( text = |Create request| fcode = 'CREATE_REQUEST' ). "#EC NOTEXT
*    menu->add_function( text = || fcode = 'Entry1' ). "#EC NOTEXT

    ENDCASE.

  ENDMETHOD.

  METHOD handle_node_context_menu_sel.

    DATA(user_object) = node_get_user_object( node_key ).

    CASE fcode.
      WHEN 'CREATE_REQUEST'.
        CASE node_key.
          WHEN 'root'.
            create_request( ).

        ENDCASE.
    ENDCASE.

  ENDMETHOD.


  METHOD create_request.

    cl_http_client=>create_internal(
      IMPORTING
        client            = DATA(client)
      EXCEPTIONS
        plugin_not_active = 1
        internal_error    = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DATA(request) = CAST cl_http_request( client->request ).

    DATA(node_key) = add_request( request = request description = 'request' parent_node_key = 'root' ).

    go_tree2->ensure_visible(
      EXPORTING
        node_key             = node_key
      EXCEPTIONS
        control_not_existing = 1
        control_dead         = 2
        cntl_system_error    = 3
        failed               = 4
        node_not_found       = 5
        OTHERS               = 6
    ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    go_tree2->update_view(
*      EXCEPTIONS
*        control_not_existing = 1
*        control_dead         = 2
*        cntl_system_error    = 3
*        failed               = 4
*        others               = 5
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD node_get_user_object.

    go_tree2->node_get_user_object( EXPORTING node_key = node_key IMPORTING user_object = result EXCEPTIONS node_not_found = 1 OTHERS = 2 ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD on_drag.

    drag_drop_object->object = node_get_user_object( node_key ).

  ENDMETHOD.

  METHOD on_drop.

    copy_request( request = CAST #( drag_drop_object->object ) to = CAST #( node_get_user_object( node_key ) ) ).

  ENDMETHOD.

  METHOD on_drop_get_flavor.

*    flavors = VALUE #( ( 'request_to_body' ) ).

  ENDMETHOD.


  METHOD copy_request.

    cl_http_client=>create_internal(
      IMPORTING
        client            = DATA(client)
      EXCEPTIONS
        plugin_not_active = 1
        internal_error    = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DATA(new_request) = CAST cl_http_request( client->request ).
    new_request->from_xstring( request->to_xstring( ) ).
*    DATA(node_key) = add_request( request = request description = 'request' parent_node_key = parent_node_key ).
*
*    go_tree2->ensure_visible(
*      EXPORTING
*        node_key             = node_key
*      EXCEPTIONS
*        control_not_existing = 1
*        control_dead         = 2
*        cntl_system_error    = 3
*        failed               = 4
*        node_not_found       = 5
*        OTHERS               = 6
*    ).
*    IF sy-subrc <> 0.
**       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    go_tree2->update_view(
**      EXCEPTIONS
**        control_not_existing = 1
**        control_dead         = 2
**        cntl_system_error    = 3
**        failed               = 4
**        others               = 5
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

  ENDMETHOD.


  METHOD show.

    DATA: ls_hierarchy_header TYPE treemhhdr,
          multipart           TYPE REF TO if_http_entity.

    me->container = container.

    ls_hierarchy_header-heading = 'Hierarchy Header'.       "#EC NOTEXT
    ls_hierarchy_header-width = 30.         " width: 30 characters

    go_tree2 = NEW #(
        node_selection_mode   = cl_gui_simple_tree=>node_sel_mode_single
        hierarchy_column_name = 'C1'
        hierarchy_header      = ls_hierarchy_header ).

    go_tree2->create_tree_control( container ).

    dnd_request_to_body_src = NEW cl_dragdrop( ).
    dnd_request_to_body_src->add(
      EXPORTING
        flavor          = 'request_to_body'
        dragsrc         = abap_true
        droptarget      = abap_false
        effect          = cl_dragdrop=>move + cl_dragdrop=>copy
      EXCEPTIONS
        already_defined = 1
        obj_invalid     = 2
        OTHERS          = 3 ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    dnd_request_to_body_src->get_handle(
      IMPORTING
        handle      = dnd_request_to_body_src_id
      EXCEPTIONS
        obj_invalid = 1
        OTHERS      = 2 ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    dnd_request_to_body_tgt = NEW cl_dragdrop( ).
    dnd_request_to_body_tgt->add(
      EXPORTING
        flavor          = 'request_to_body'
        dragsrc         = abap_false
        droptarget      = abap_true
        effect          = cl_dragdrop=>move + cl_dragdrop=>copy
      EXCEPTIONS
        already_defined = 1
        obj_invalid     = 2
        OTHERS          = 3 ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    dnd_request_to_body_tgt->get_handle(
      IMPORTING
        handle      = dnd_request_to_body_tgt_id
      EXCEPTIONS
        obj_invalid = 1
        OTHERS      = 2 ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    go_tree2->add_node(
          node_key  = 'root'
          isfolder  = abap_true
          item_table  = VALUE #(
                      ( item_name = 'C1'
                        class     = cl_column_tree_model=>item_class_text
                        text      = |ROOT| ) )
          user_object = NEW lcl_root( ) ).


    LOOP AT postman_collection-items ASSIGNING FIELD-SYMBOL(<item>).
      DATA(request) = postman_item_to_request( postman_item = <item> ).
      add_request( request = request description = <item>-name parent_node_key = 'root' ).
    ENDLOOP.


    go_tree2->expand_root_nodes(
      EXPORTING
        expand_subtree      = abap_true
      EXCEPTIONS
        illegal_level_count = 1
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    go_tree2->set_registered_events(
      EXPORTING
        events = VALUE #(
                  ( eventid = cl_item_tree_model=>eventid_node_context_menu_req ) )
      EXCEPTIONS
        illegal_event_combination = 3
        unknown_event = 4 ).

    SET HANDLER handle_node_context_menu_req FOR go_tree2.
    SET HANDLER handle_node_context_menu_sel FOR go_tree2.
    SET HANDLER on_drag FOR go_tree2.
    SET HANDLER on_drop FOR go_tree2.
    SET HANDLER on_drop_get_flavor FOR go_tree2.

  ENDMETHOD.


  METHOD postman_item_to_request.

    cl_http_client=>create_internal(
      IMPORTING
        client            = DATA(client)
      EXCEPTIONS
        plugin_not_active = 1
        internal_error    = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(request) = client->request.

    request->set_version( if_http_request=>co_protocol_version_1_0 ).
    request->set_method( method = postman_item-request-method ).
    cl_http_utility=>set_request_uri( request = request uri = postman_item-request-url ).
    LOOP AT postman_item-request-header_fields ASSIGNING FIELD-SYMBOL(<header_field>).
      request->set_header_field( name = <header_field>-key value = <header_field>-value ).
    ENDLOOP.
    request->set_cdata( postman_item-request-body ).

    " Reload is required to calculate multipart information!
    DATA(xstring) = request->to_xstring( ).
    request->from_xstring( xstring ).

    " debug helper
    IF 0 = 1.
      DATA(string) = cl_abap_codepage=>convert_from( xstring ).
    ENDIF.

    result = request.

  ENDMETHOD.

ENDCLASS.

TABLES sscrfields.
PARAMETERS dummy.
SELECTION-SCREEN FUNCTION KEY: 1, 2.

LOAD-OF-PROGRAM.
  DATA(go_app) = NEW lcl_app( ).
  go_app->set_screen_information(
        ref_sscrfields = REF #( sscrfields )
        container      = cl_gui_container=>screen0 ).

AT SELECTION-SCREEN OUTPUT.
  TRY.
      go_app->at_selection_screen_output( ).
    CATCH lcx_app INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

AT SELECTION-SCREEN.
  TRY.
      go_app->at_selection_screen( ).
    CATCH lcx_app INTO DATA(lx).
      MESSAGE lx TYPE 'E'.
  ENDTRY.
