CLASS zcl_httped_request DEFINITION INHERITING FROM zcl_httped_entity
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_httped_request.

    METHODS constructor.

    METHODS from_xstring
      IMPORTING
        value TYPE xstring.

    METHODS get_http_entity
      IMPORTING
        request TYPE REF TO if_http_request
      RAISING
        zcx_httped.

    METHODS if_http_entity~to_xstring REDEFINITION.

    ALIASES:
        get_url           FOR zif_httped_request~get_url,
        get_version       FOR if_http_request~get_version,
        set_data          FOR if_http_entity~set_data,
        set_header_field  FOR if_http_entity~set_header_field,
        set_header_fields FOR if_http_entity~set_header_fields,
        set_method        FOR if_http_request~set_method,
        set_url           FOR zif_httped_request~set_url,
        set_version       FOR if_http_request~set_version.

  PRIVATE SECTION.

    DATA: method LIKE if_http_request=>co_request_method_get.

ENDCLASS.



CLASS zcl_httped_request IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    method = if_http_request=>co_request_method_get.
  ENDMETHOD.


  METHOD from_xstring.

    DATA(request) = NEW cl_http_request( ).
    request->from_xstring( value ).
    set_version( request->get_version( ) ).
    set_method( request->if_http_request~get_method( ) ).
    set_url( request->get_header_field( name = if_http_header_fields_sap=>path ) ).
    DATA(local_header_fields) = VALUE tihttpnvp( ).
    request->get_header_fields( CHANGING fields = local_header_fields ).
    set_header_fields( local_header_fields ).
    set_data( request->get_data( ) ).

  ENDMETHOD.


  METHOD get_http_entity.

    request->set_version( get_version( ) ).
    request->set_method( if_http_request~get_method( ) ).
    DATA(local_header_fields) = VALUE tihttpnvp( ).
    if_http_entity~get_header_fields( CHANGING fields = local_header_fields ).
    request->set_header_fields( local_header_fields ).

  ENDMETHOD.


  METHOD if_http_request~copy.

  ENDMETHOD.

  METHOD if_http_request~get_authorization.

  ENDMETHOD.

  METHOD if_http_request~get_form_data.

  ENDMETHOD.

  METHOD if_http_request~get_method.

  ENDMETHOD.

  METHOD if_http_request~get_raw_message.

  ENDMETHOD.

  METHOD if_http_request~get_uri_parameter.

  ENDMETHOD.

  METHOD zif_httped_request~get_url.

  ENDMETHOD.

  METHOD if_http_request~get_user_agent.

  ENDMETHOD.

  METHOD if_http_request~set_authorization.

  ENDMETHOD.




  METHOD if_http_request~set_method.
    me->method = method.
  ENDMETHOD.


  METHOD if_http_request~set_version.
    me->version = version.
  ENDMETHOD.


  METHOD if_http_entity~to_xstring.

    data(data1) = cl_abap_codepage=>convert_to( |{ if_http_request~get_method( ) } { get_url( )
                    } { SWITCH string( get_version( )
                        WHEN if_http_entity=>co_protocol_version_1_0 THEN |HTTP/1.0|
                        WHEN if_http_entity=>co_protocol_version_1_0 THEN |HTTP/1.1| )
                    }\r\n{
                        concat_lines_of( sep = |\r\n| table = VALUE string_table(
                        FOR <header_field> IN header_fields
                        ( |{ <header_field>-name }: { <header_field>-value }| ) ) )
                    }\r\n\r\n| ).
    data(data2) = if_http_entity~get_data( ).
    CONCATENATE data1 data2 INTO data IN BYTE MODE.

  ENDMETHOD.


  METHOD set_url.
    set_header_field( name = if_http_header_fields_sap=>path value = value ).
  ENDMETHOD.


ENDCLASS.
