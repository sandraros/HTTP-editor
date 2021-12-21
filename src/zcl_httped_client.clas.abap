CLASS zcl_httped_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    interfaces ZIF_HTTPED_CLIENT.

    METHODS constructor.

    CLASS-METHODS create_internal
      RETURNING
        VALUE(result) TYPE REF TO zcl_httped_client.

    CLASS-METHODS create_by_destination
      IMPORTING
        destination   TYPE c
      RETURNING
        VALUE(result) TYPE REF TO zcl_httped_client.

    METHODS get_http_client
      RETURNING
        VALUE(result) TYPE REF TO if_http_client
      RAISING
        zcx_httped.

    DATA: rfc_destination TYPE string READ-ONLY.

  aliases REQUEST
    for IF_HTTP_CLIENT~REQUEST .
*  aliases RESPONSE
*    for IF_HTTP_CLIENT~RESPONSE .
  aliases CREATE_ABS_URL
    for IF_HTTP_CLIENT~CREATE_ABS_URL .
  aliases CREATE_REL_URL
    for IF_HTTP_CLIENT~CREATE_REL_URL .
  aliases ESCAPE_URL
    for IF_HTTP_CLIENT~ESCAPE_URL .

ENDCLASS.



CLASS zcl_httped_client IMPLEMENTATION.


  METHOD constructor.
    request = NEW zcl_httped_request( ).
  ENDMETHOD.


  METHOD create_by_destination.
    result = NEW #( ).
    result->rfc_destination = destination.
  ENDMETHOD.


  METHOD create_internal.
    result = NEW #( ).
  ENDMETHOD.


  METHOD get_http_client.
    DATA: dref TYPE REF TO data.
    FIELD-SYMBOLS <rfc_destination> TYPE c.

    IF rfc_destination IS NOT INITIAL.
      DATA(len) = strlen( rfc_destination ).
      CREATE DATA dref TYPE c LENGTH len.
      ASSIGN dref->* TO <rfc_destination>.
      <rfc_destination> = rfc_destination.
      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = <rfc_destination>
        IMPORTING
          client                   = result
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_httped( text = 'cl_http_client=>create_by_destination' ).
      ENDIF.
    ELSE.
      cl_http_client=>create_internal(
        IMPORTING
          client            = result
        EXCEPTIONS
          plugin_not_active = 1
          internal_error    = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_httped( text = 'cl_http_client=>create_internal' ).
      ENDIF.
    ENDIF.
    cast zcl_httped_request( request )->get_http_entity( result->request ).
  ENDMETHOD.

  METHOD if_http_client~append_field_url.

  ENDMETHOD.

  METHOD if_http_client~authenticate.

  ENDMETHOD.

  METHOD if_http_client~close.

  ENDMETHOD.

  METHOD if_http_client~create_abs_url.

  ENDMETHOD.

  METHOD if_http_client~create_rel_url.

  ENDMETHOD.

  METHOD if_http_client~escape_html.

  ENDMETHOD.

  METHOD if_http_client~escape_url.

  ENDMETHOD.

  METHOD if_http_client~get_last_error.

  ENDMETHOD.

  METHOD if_http_client~listen.

  ENDMETHOD.

  METHOD if_http_client~receive.

  ENDMETHOD.

  METHOD if_http_client~refresh_cookie.

  ENDMETHOD.

  METHOD if_http_client~refresh_request.

  ENDMETHOD.

  METHOD if_http_client~refresh_response.

  ENDMETHOD.

  METHOD if_http_client~send.

  ENDMETHOD.

  METHOD if_http_client~send_sap_assertion_ticket.

  ENDMETHOD.

  METHOD if_http_client~send_sap_logon_ticket.

  ENDMETHOD.

  METHOD if_http_client~set_compression.

  ENDMETHOD.

  METHOD if_http_client~unescape_url.

  ENDMETHOD.


ENDCLASS.
