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
        width   = 640
        height  = 400
        top     = 150
        left    = 350
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

CLASS zcx_httped DEFINITION INHERITING FROM cx_static_check.
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

CLASS zcx_httped IMPLEMENTATION.

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

INTERFACE lif_tree_node.
  DATA: request      TYPE REF TO if_http_entity READ-ONLY,
        node_key     TYPE string READ-ONLY,
        tree_request TYPE REF TO lif_tree_node READ-ONLY.
  METHODS info
    IMPORTING
      request       TYPE REF TO if_http_entity
      node_key      TYPE string
      tree_request  TYPE REF TO lif_tree_node OPTIONAL
    RETURNING
      VALUE(result) TYPE REF TO lif_tree_node.
ENDINTERFACE.

CLASS lcl_tree_node DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_tree_node.
ENDCLASS.
CLASS lcl_tree_node IMPLEMENTATION.
  METHOD lif_tree_node~info.
    lif_tree_node~request = request.
    lif_tree_node~node_key = node_key.
    lif_tree_node~tree_request = tree_request.
    result = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_root DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_root IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_root( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_request DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_request IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_request( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_method DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_method IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_method( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_header DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_header IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_header( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_body DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_body IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_body( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tree_part DEFINITION INHERITING FROM lcl_tree_node CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_node.
ENDCLASS.
CLASS lcl_tree_part IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_tree_part( ).
  ENDMETHOD.
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
        zcx_httped.

  PRIVATE SECTION.

    TYPES:
      ty_files TYPE STANDARD TABLE OF file_table WITH EMPTY KEY.

    METHODS load_file
      IMPORTING
        file_path     TYPE file_table-filename
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_httped.

    METHODS write_bin_file
      IMPORTING
        i_filename     TYPE csequence
        i_file_xstring TYPE xstring
      RAISING
        zcx_httped.

    METHODS file_open_dialog
      RETURNING
        VALUE(result) TYPE ty_files
      RAISING
        zcx_httped.

    METHODS file_save_dialog
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_httped.

    DATA: ref_sscrfields TYPE REF TO sscrfields.

ENDCLASS.


CLASS lzcl_httped_tree DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        container          TYPE REF TO cl_gui_container
        postman_collection TYPE ty_postman
      RETURNING
        VALUE(result)      TYPE REF TO lzcl_httped_tree
      RAISING
        zcx_httped.

    METHODS show
      RAISING
        zcx_httped.

  PRIVATE SECTION.

    METHODS add_request
      IMPORTING
        request          TYPE REF TO if_http_entity
        description      TYPE string
        parent           TYPE REF TO lif_tree_node OPTIONAL " ROOT or Body
        previous_sibling TYPE REF TO lif_tree_node OPTIONAL
        multipart        TYPE REF TO if_http_entity OPTIONAL
      RETURNING
        VALUE(result)    TYPE REF TO lif_tree_node
      RAISING
        zcx_httped.

    METHODS get_new_node_key
      RETURNING
        VALUE(result) TYPE string.

    METHODS create_request
      RAISING
        zcx_httped.

    METHODS node_get_user_object
      IMPORTING
        node_key      TYPE string
      RETURNING
        VALUE(result) TYPE REF TO object
      RAISING
        zcx_httped.

    METHODS copy_request
      IMPORTING
        request TYPE REF TO lif_tree_node
        to      TYPE REF TO lif_tree_node
      RAISING
        zcx_httped.

    METHODS postman_item_to_request
      IMPORTING
        postman_item  TYPE ty_item
      RETURNING
        VALUE(result) TYPE REF TO if_http_entity
      RAISING
        zcx_httped.

    METHODS add_node
      IMPORTING
        i_parent           TYPE REF TO lif_tree_node OPTIONAL
        i_previous_sibling TYPE REF TO lif_tree_node OPTIONAL
        i_tree_request     TYPE REF TO lif_tree_node
        i_icon             TYPE csequence
        i_drag_drop_id     TYPE i OPTIONAL
        i_text             TYPE string
      RAISING
        zcx_httped.

    METHODS ensure_visible
      IMPORTING
        node_key TYPE string
      RAISING
        zcx_httped.

    METHODS update_view
      RAISING
        zcx_httped.

    METHODS set_focus
      IMPORTING
        container TYPE REF TO cl_gui_container
      RAISING
        zcx_httped.

    METHODS create_http_client
      RETURNING
        VALUE(result) TYPE REF TO if_http_client
      RAISING
        zcx_httped.

    METHODS clone_http_request
      IMPORTING
        entity        TYPE REF TO if_http_entity
      RETURNING
        VALUE(result) TYPE REF TO if_http_request
      RAISING
        zcx_httped.

    METHODS get_entity_xstring
      IMPORTING
        entity        TYPE REF TO if_http_entity
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS expand_node
      IMPORTING
        node_key TYPE string
      RAISING
        zcx_httped.

    METHODS delete_request_or_part
      IMPORTING
        i_tree_node TYPE REF TO lif_tree_node
      RAISING
        zcx_httped.

    METHODS delete_node
      IMPORTING
        tree_node TYPE REF TO lif_tree_node
      RAISING
        zcx_httped.

    METHODS set_registered_events
      IMPORTING
        events TYPE cntl_simple_events
      RAISING
        zcx_httped.

    CLASS-METHODS initialize_dnd
      RAISING
        zcx_httped.

    CLASS-METHODS create_dragdrop
      IMPORTING
        flavor        TYPE c
        dragsrc       TYPE abap_bool
        droptarget    TYPE abap_bool
        effect        TYPE i
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_httped.

    METHODS handle_node_context_menu_req
                  FOR EVENT node_context_menu_request OF cl_tree_model
      IMPORTING node_key menu.

    METHODS handle_node_context_menu_sel
                  FOR EVENT node_context_menu_select OF cl_tree_model
      IMPORTING node_key fcode.

    METHODS on_node_double_click
                  FOR EVENT node_double_click OF cl_tree_model
      IMPORTING node_key.

    METHODS on_drag
                  FOR EVENT drag OF cl_column_tree_model
      IMPORTING node_key item_name drag_drop_object.

    METHODS on_drop
                  FOR EVENT drop OF cl_tree_model
      IMPORTING node_key drag_drop_object.

    METHODS on_drop_complete
                  FOR EVENT drop_complete OF cl_column_tree_model
      IMPORTING node_key item_name drag_drop_object.

    DATA:
      postman_collection TYPE ty_postman,
      go_tree2           TYPE REF TO cl_column_tree_model,
      container          TYPE REF TO cl_gui_container,
      next_node_key      TYPE i VALUE 1,
      tree_root          TYPE REF TO lcl_tree_node.
    CLASS-DATA:
      BEGIN OF dnd_id,
        BEGIN OF part_to_multipart,
          source        TYPE i,
          target        TYPE i,
          source_target TYPE i,
        END OF part_to_multipart,
      END OF dnd_id.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.

    ref_sscrfields->functxt_01 = 'Open file(s)'.
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

    DATA: tree TYPE REF TO lzcl_httped_tree.

    CASE ref_sscrfields->ucomm.

      WHEN 'FC01'.

        DATA(files) = file_open_dialog( ).

        LOOP AT files ASSIGNING FIELD-SYMBOL(<file>).

          DATA(result) = load_file( file_path = <file>-filename ).

          DATA(postman_collection) = VALUE ty_postman( ).
          TRY.
              CALL TRANSFORMATION zhttped_postman_collection SOURCE XML result RESULT root = postman_collection.
            CATCH cx_root INTO DATA(lx).
              RAISE EXCEPTION TYPE zcx_httped EXPORTING text = 'transfo' previous = lx.
          ENDTRY.

          tree = lzcl_httped_tree=>create(
              container          = lcl_dialogbox=>create( title = <file>-filename )->get_container( )
              postman_collection = postman_collection ).
          tree->show( ).
        ENDLOOP.

      WHEN 'FC02'.

        DATA(full_path) = file_save_dialog( ).

        TRY.
            CLEAR postman_collection.
            CALL TRANSFORMATION zhttped_postman_collection SOURCE root = postman_collection RESULT XML DATA(xstring).
          CATCH cx_root INTO lx.
            RAISE EXCEPTION NEW zcx_httped( text = 'transfo' previous = lx ).
        ENDTRY.

        write_bin_file( i_filename = full_path i_file_xstring = xstring ).

        tree = lzcl_httped_tree=>create(
            container          = lcl_dialogbox=>create( title = full_path )->get_container( )
            postman_collection = postman_collection ).
        tree->show( ).

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
      RAISE EXCEPTION NEW zcx_httped( text = 'cl_gui_frontend_services=>gui_upload' ).
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
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'cl_swf_utl_convert_xstring=>xstring_to_table' ).
    ENDIF.

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
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'cl_gui_frontend_services=>gui_download' ).
    ENDIF.

  ENDMETHOD.



  METHOD file_open_dialog.

    DATA l_rc TYPE i.
    DATA l_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Open existing Postman file'
        default_filename        = 'Fiori.postman_collection.json'
        file_filter             = '.json'
        initial_directory       = 'C:\Users\sandra.rossi\Accenture\LPP - Local - Documents\General\OData'
        multiselection          = abap_true
      CHANGING
        file_table              = result
        rc                      = l_rc
        user_action             = l_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_httped EXPORTING text = 'save dialog'.
    ENDIF.
    IF l_action <> cl_gui_frontend_services=>action_ok.
      RAISE EXCEPTION TYPE zcx_httped EXPORTING text = 'Dialog cancelled by the user'.
    ENDIF.

  ENDMETHOD.


  METHOD file_save_dialog.

    DATA l_action TYPE i.
    DATA file_name TYPE string.
    DATA file_path TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title            = 'Create new Postman file'
        default_file_name       = 'XXXXX.postman_collection.json'
        file_filter             = '.json'
        initial_directory       = 'C:\Users\sandra.rossi\Accenture\LPP - Local - Documents\General\OData'
        prompt_on_overwrite     = abap_true
      CHANGING
        filename                  = file_name
        path                      = file_path
        fullpath                  = result
        user_action               = l_action
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_httped EXPORTING text = 'save dialog'.
    ENDIF.
    IF l_action <> cl_gui_frontend_services=>action_ok.
      RAISE EXCEPTION TYPE zcx_httped EXPORTING text = 'Dialog cancelled by the user'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lzcl_httped_tree IMPLEMENTATION.


  METHOD create.

    result = NEW lzcl_httped_tree( ).
    result->container = container.
    result->postman_collection = postman_collection.

    initialize_dnd( ).

  ENDMETHOD.


  METHOD add_request.
    DATA: tree_request_or_part TYPE REF TO lif_tree_node.

    IF request IS INSTANCE OF if_http_request.
      DATA(request2) = CAST if_http_request( request ).
      DATA(method) = request2->get_method( ).
      DATA(url) = request2->get_header_field( name = if_http_header_fields_sap=>path ).
    ENDIF.

    IF multipart IS NOT BOUND.
      tree_request_or_part = lcl_tree_request=>create( )->info( request = request node_key = get_new_node_key( ) ).
    ELSE.
      tree_request_or_part = lcl_tree_part=>create( )->info( request = request node_key = get_new_node_key( ) ).
    ENDIF.
    add_node( i_parent           = parent
              i_previous_sibling = previous_sibling
              i_tree_request     = tree_request_or_part
              i_icon             = icon_htm
              i_drag_drop_id     = dnd_id-part_to_multipart-source_target
              i_text             = |{ description } ({ request->get_content_type( ) })| ).

    IF method IS NOT INITIAL.
      DATA(tree_method) = lcl_tree_method=>create( )->info( request = request node_key = get_new_node_key( ) ).
      add_node( i_parent       = tree_request_or_part
                i_tree_request = tree_method
                i_icon         = icon_oo_method
                i_text         = |{ method } { url }| ).
    ENDIF.

    DATA(tree_header) = lcl_tree_header=>create( )->info( request = request node_key = get_new_node_key( ) ).
    add_node( i_parent           = COND #( WHEN tree_method IS NOT BOUND THEN tree_request_or_part )
              i_previous_sibling = COND #( WHEN tree_method IS BOUND THEN tree_method )
              i_tree_request     = tree_header
              i_icon             = icon_header
              i_text             = |header| ).

    request->get_data_length( IMPORTING data_length = DATA(length) ).

    IF length > 0.

      DATA(body_node_key) = get_new_node_key( ).
      DATA(tree_body) = lcl_tree_body=>create( )->info( tree_request = tree_request_or_part request = request node_key = body_node_key ).
      add_node( i_previous_sibling = tree_header
                i_tree_request     = tree_body
                i_icon             = icon_package_standard
                i_drag_drop_id     = dnd_id-part_to_multipart-target
                i_text             = |body| ).

      IF request->get_content_type( ) = |application/http|.
        DATA(embedded_request) = NEW zcl_httped_request( ).
        embedded_request->from_xstring( request->get_data( ) ).
        add_request( request = embedded_request description = 'HTTP' parent = tree_body ).
      ELSEIF request->num_multiparts( ) > 0.
        DATA previous_sibling_2 TYPE REF TO lif_tree_node.
        CLEAR previous_sibling_2.
        DO request->num_multiparts( ) TIMES.
          previous_sibling_2 = add_request( request          = request->get_multipart( index = sy-index + 0 )
                                            description      = |Part|
                                            parent           = COND #( WHEN previous_sibling_2 IS NOT BOUND THEN tree_body )
                                            previous_sibling = previous_sibling_2
                                            multipart        = request ).
        ENDDO.
      ENDIF.
    ENDIF.

    result = tree_request_or_part.

  ENDMETHOD.


  METHOD get_new_node_key.

    result = |{ next_node_key }|.
    next_node_key = next_node_key + 1.

  ENDMETHOD.

  METHOD handle_node_context_menu_req.

    TRY.

        DATA(user_object) = node_get_user_object( node_key ).

        CASE TYPE OF user_object.

          WHEN TYPE lcl_tree_root.
            menu->add_function( text = |Create request| fcode = 'CREATE_REQUEST' ). "#EC NOTEXT
          WHEN TYPE lcl_tree_part.
            menu->add_function( text = |Delete part| fcode = 'DELETE_REQ_OR_PART' ). "#EC NOTEXT

        ENDCASE.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_node_context_menu_sel.

    TRY.

        DATA(tree_node) = CAST lif_tree_node( node_get_user_object( node_key ) ).

        CASE fcode.

          WHEN 'CREATE_REQUEST'.

            CASE TYPE OF tree_node.
              WHEN TYPE lcl_tree_root.
                create_request( ).
            ENDCASE.

          WHEN 'DELETE_REQ_OR_PART'.

            CASE TYPE OF tree_node.
              WHEN TYPE lcl_tree_request.
                delete_request_or_part( tree_node ).
              WHEN TYPE lcl_tree_part.
                delete_request_or_part( tree_node ).
            ENDCASE.

        ENDCASE.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD create_request.

    DATA(client) = create_http_client( ).
    DATA(request) = CAST zcl_httped_request( client->request ).

    DATA(tree_request) = add_request( request = request description = 'request' parent = tree_root ).

    ensure_visible( tree_request->node_key ).

    update_view( ).

  ENDMETHOD.


  METHOD node_get_user_object.

    go_tree2->node_get_user_object( EXPORTING node_key = node_key IMPORTING user_object = result EXCEPTIONS node_not_found = 1 OTHERS = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->node_get_user_object' ).
    ENDIF.

  ENDMETHOD.

  METHOD on_drag.

    TRY.

        drag_drop_object->object = node_get_user_object( node_key ).

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD on_drop.

    TRY.

        CASE drag_drop_object->flavor.

          WHEN 'http_part_to_request'.

            copy_request( request = CAST #( drag_drop_object->object ) to = CAST #( node_get_user_object( node_key ) ) ).
            update_view( ).

        ENDCASE.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.



  METHOD on_drop_complete.

    TRY.

        IF drag_drop_object->effect = cl_dragdrop=>move.
          delete_node( CAST #( drag_drop_object->object ) ).
          update_view( ).
        ENDIF.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD copy_request.
    DATA: tree_request  TYPE REF TO lif_tree_node,
          new_request   TYPE REF TO if_http_request,
          entity        TYPE REF TO if_http_entity,
          header_fields TYPE tihttpnvp.

    CASE TYPE OF to.
      WHEN TYPE lcl_tree_root.
        new_request = clone_http_request( request->request ).
        tree_request = add_request( request = new_request description = 'new request' parent = tree_root ).
      WHEN TYPE lcl_tree_request.
        new_request = clone_http_request( request->request ).
        tree_request = add_request( request = new_request description = 'new request' previous_sibling = to ).
      WHEN TYPE lcl_tree_body.
        entity = to->request->add_multipart( ).
        header_fields = VALUE tihttpnvp( ).
        request->request->get_header_fields( CHANGING fields = header_fields ).
        entity->set_header_fields( header_fields ).
        entity->set_data( request->request->get_data( ) ).
        tree_request = add_request( request     = entity
                                    description = 'new request'
                                    parent      = to
                                    multipart   = to->request ).
      WHEN TYPE lcl_tree_part.
        entity = to->request->add_multipart( ).
        header_fields = VALUE tihttpnvp( ).
        request->request->get_header_fields( CHANGING fields = header_fields ).
        entity->set_header_fields( header_fields ).
        entity->set_data( request->request->get_data( ) ).
        tree_request = add_request( request          = entity
                                    description      = 'new request'
                                    previous_sibling = to
                                    multipart        = to->request ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_httped( text = 'programming error: target node not supported for copy/move' ).
*      WHEN TYPE lcl_tree_method.
*      WHEN TYPE lcl_tree_header.
    ENDCASE.

    expand_node( tree_request->node_key ).
    ensure_visible( tree_request->node_key ).
    update_view( ).

  ENDMETHOD.



  METHOD clone_http_request.
    DATA: xstring TYPE xstring.

    DATA(client) = create_http_client( ).
    result  = CAST #( client->request ).
    CASE TYPE OF entity.
      WHEN TYPE zcl_httped_request.
        xstring = entity->to_xstring( ).
      WHEN OTHERS.
        xstring = get_entity_xstring( entity ).
    ENDCASE.
    result->from_xstring( xstring ).
    xstring = result->to_xstring( ).

  ENDMETHOD.



  METHOD show.

    DATA: ls_hierarchy_header TYPE treemhhdr,
          previous_sibling    TYPE REF TO lif_tree_node.

    me->container = container.

    ls_hierarchy_header-heading = 'Hierarchy Header'.       "#EC NOTEXT
    ls_hierarchy_header-width = 30.         " width: 30 characters

    go_tree2 = NEW #(
        node_selection_mode   = cl_gui_simple_tree=>node_sel_mode_single
        hierarchy_column_name = 'C1'
        hierarchy_header      = ls_hierarchy_header ).

    go_tree2->create_tree_control( container ).

    initialize_dnd( ).

    tree_root ?= lcl_tree_root=>create( )->info( request = VALUE #( ) node_key = 'root' ).

    go_tree2->add_node(
          node_key     = tree_root->lif_tree_node~node_key
          isfolder     = abap_true
          item_table   = VALUE #(
                          ( item_name = 'C1'
                            class     = cl_column_tree_model=>item_class_text
                            text      = |ROOT| ) )
          drag_drop_id = dnd_id-part_to_multipart-target
          user_object  = tree_root ).

    CLEAR previous_sibling.
    LOOP AT postman_collection-items ASSIGNING FIELD-SYMBOL(<item>).
      DATA(request) = postman_item_to_request( postman_item = <item> ).
      previous_sibling = add_request( request     = request
                                      description = <item>-name
                                      parent      = COND #( WHEN previous_sibling IS NOT BOUND THEN tree_root )
                                      previous_sibling = previous_sibling ).
    ENDLOOP.

    expand_node( tree_root->lif_tree_node~node_key ).

    set_registered_events( VALUE #(
          ( eventid = cl_item_tree_model=>eventid_node_context_menu_req )
          ( eventid = cl_item_tree_model=>eventid_node_double_click ) ) ).

    SET HANDLER handle_node_context_menu_req FOR go_tree2.
    SET HANDLER handle_node_context_menu_sel FOR go_tree2.
    SET HANDLER on_drag FOR go_tree2.
    SET HANDLER on_drop FOR go_tree2.
    SET HANDLER on_drop_complete FOR go_tree2.
    SET HANDLER on_node_double_click FOR go_tree2.

    ensure_visible( tree_root->lif_tree_node~node_key ).

    set_focus( container ).

  ENDMETHOD.


  METHOD postman_item_to_request.

    DATA: client TYPE REF TO if_http_client.


    client = create_http_client( ).

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


  METHOD initialize_dnd.

    CHECK dnd_id IS INITIAL.

    dnd_id-part_to_multipart-source = create_dragdrop(
            flavor          = 'http_part_to_request'
            dragsrc         = abap_true
            droptarget      = abap_false
            effect          = cl_dragdrop=>move + cl_dragdrop=>copy ).

    dnd_id-part_to_multipart-target = create_dragdrop(
            flavor          = 'http_part_to_request'
            dragsrc         = abap_false
            droptarget      = abap_true
            effect          = cl_dragdrop=>move + cl_dragdrop=>copy ).

    dnd_id-part_to_multipart-source_target = create_dragdrop(
            flavor          = 'http_part_to_request'
            dragsrc         = abap_true
            droptarget      = abap_true
            effect          = cl_dragdrop=>move + cl_dragdrop=>copy ).

  ENDMETHOD.


  METHOD add_node.

    go_tree2->add_node(
      EXPORTING
          node_key          = i_tree_request->node_key
          relationship      = COND #( WHEN i_parent IS BOUND THEN cl_tree_model=>relat_first_child
                                                             ELSE cl_tree_model=>relat_next_sibling )
          relative_node_key = COND #( WHEN i_parent IS BOUND THEN i_parent->node_key
                                                             ELSE i_previous_sibling->node_key )
          isfolder          = abap_true
          image             = CONV #( i_icon )
          expanded_image    = CONV #( i_icon )
          drag_drop_id      = i_drag_drop_id
          user_object       = i_tree_request
          item_table        = VALUE #(
                              ( item_name = 'C1'
                                class     = cl_column_tree_model=>item_class_text
                                text      = i_text ) )
      EXCEPTIONS
        node_key_exists         = 1
        node_key_empty          = 2
        illegal_relationship    = 3
        relative_node_not_found = 4
        error_in_item_table     = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->add_node' ).
    ENDIF.

  ENDMETHOD.


  METHOD ensure_visible.

    go_tree2->ensure_visible(
      EXPORTING
        node_key             = node_key
      EXCEPTIONS
        control_not_existing = 1
        control_dead         = 2
        cntl_system_error    = 3
        failed               = 4
        node_not_found       = 5
        OTHERS               = 6 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->ensure_visible' ).
    ENDIF.

  ENDMETHOD.


  METHOD update_view.

    go_tree2->update_view(
      EXCEPTIONS
        control_not_existing = 1
        control_dead         = 2
        cntl_system_error    = 3
        failed               = 4
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->update_view' ).
    ENDIF.

  ENDMETHOD.


  METHOD set_focus.

    cl_gui_control=>set_focus(
      EXPORTING
        control           = container
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'cl_gui_control=>set_focus' ).
    ENDIF.

  ENDMETHOD.


  METHOD create_http_client.

    result = zcl_httped_client=>create_internal( ).

  ENDMETHOD.


  METHOD create_dragdrop.

    DATA(dragdrop) = NEW cl_dragdrop( ).
    dragdrop->add(
      EXPORTING
        flavor          = flavor
        dragsrc         = dragsrc
        droptarget      = droptarget
        effect          = effect
      EXCEPTIONS
        already_defined = 1
        obj_invalid     = 2
        OTHERS          = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'drag drop ID -> add' ).
    ENDIF.

    dragdrop->get_handle(
      IMPORTING
        handle      = result
      EXCEPTIONS
        obj_invalid = 1
        OTHERS      = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'drag drop ID -> get_handle' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_entity_xstring.

    DATA(header_fields) = VALUE tihttpnvp( ).
    entity->get_header_fields( CHANGING fields = header_fields ).
    DATA(header) = cl_abap_codepage=>convert_to(
        concat_lines_of( sep = |\r\n| table =
            VALUE string_table(
            FOR <header_field> IN header_fields
            ( |{ <header_field>-name }: { <header_field>-value }| ) ) ) ).

    DATA(between) = cl_abap_codepage=>convert_to( |\r\n\r\n| ).

    DATA(body) = entity->get_data( ).

    CONCATENATE header between body INTO result IN BYTE MODE.

  ENDMETHOD.


  METHOD expand_node.

    go_tree2->expand_node(
      EXPORTING
        node_key            = node_key
        expand_subtree      = abap_true
      EXCEPTIONS
        node_not_found      = 1
        OTHERS              = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->expand_node' ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_request_or_part.

    delete_node( tree_node = i_tree_node ).
    update_view( ).

  ENDMETHOD.


  METHOD delete_node.

    go_tree2->delete_node(
      EXPORTING
        node_key       = tree_node->node_key
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->delete_node' ).
    ENDIF.

  ENDMETHOD.


  METHOD on_node_double_click.

    TRY.

        DATA(tree_node) = CAST lif_tree_node( node_get_user_object( node_key ) ).
        DATA(dialogbox) = lcl_dialogbox=>create( title = 'HTTP contents' ).
        DATA(go_textedit) = NEW cl_gui_textedit( parent = dialogbox->get_container( ) ).
        go_textedit->set_textstream(
          EXPORTING
            text = cl_abap_codepage=>convert_from( tree_node->request->to_xstring( ) )
          EXCEPTIONS
            error_cntl_call_method = 1
            not_supported_by_gui   = 2
            OTHERS                 = 3 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zcx_httped( text = 'textedit->set_textstream' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.



  METHOD set_registered_events.

    go_tree2->set_registered_events(
      EXPORTING
        events                    = events
      EXCEPTIONS
        illegal_event_combination = 1
        unknown_event             = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_httped( text = 'tree->set_registered_events' ).
    ENDIF.

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
    CATCH zcx_httped INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

AT SELECTION-SCREEN.
  TRY.
      go_app->at_selection_screen( ).
    CATCH zcx_httped INTO DATA(lx).
      MESSAGE lx TYPE 'E'.
  ENDTRY.
