CLASS zcl_httped_entity DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_httped_entity.

    METHODS constructor.

    ALIASES: multipart FOR zif_httped_entity~multipart.

  PROTECTED SECTION.

    DATA: version       LIKE if_http_request=>co_protocol_version_1_1,
          header_fields TYPE HASHED TABLE OF ihttpnvp WITH UNIQUE KEY name,
          form_fields   TYPE tihttpnvp,
          data          TYPE xstring,
          cdata         TYPE string.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_httped_entity IMPLEMENTATION.

  METHOD if_http_entity~add_cookie_field.

  ENDMETHOD.

  METHOD if_http_entity~add_multipart.
    entity = zif_httped_entity~multipart->add( ).
  ENDMETHOD.

  METHOD if_http_entity~append_cdata.

  ENDMETHOD.

  METHOD if_http_entity~append_cdata2.

  ENDMETHOD.

  METHOD if_http_entity~append_data.

  ENDMETHOD.

  METHOD constructor.
    zif_httped_entity~multipart = NEW #( ).
    version = if_http_request=>co_protocol_version_1_1.
  ENDMETHOD.

  METHOD if_http_entity~delete_cookie.

  ENDMETHOD.

  METHOD if_http_entity~delete_cookie_secure.

  ENDMETHOD.

  METHOD if_http_entity~delete_form_field.

  ENDMETHOD.

  METHOD if_http_entity~delete_form_field_secure.

  ENDMETHOD.

  METHOD if_http_entity~delete_header_field.

  ENDMETHOD.

  METHOD if_http_entity~delete_header_field_secure.

  ENDMETHOD.

  METHOD if_http_entity~from_xstring.

  ENDMETHOD.

  METHOD if_http_entity~get_cdata.

  ENDMETHOD.

  METHOD if_http_entity~get_content_type.

  ENDMETHOD.

  METHOD if_http_entity~get_cookie.

  ENDMETHOD.

  METHOD if_http_entity~get_cookies.

  ENDMETHOD.

  METHOD if_http_entity~get_cookie_field.

  ENDMETHOD.

  METHOD if_http_entity~get_data.
    data = me->data.
  ENDMETHOD.

  METHOD if_http_entity~get_data_length.

  ENDMETHOD.

  METHOD if_http_entity~get_form_field.

  ENDMETHOD.

  METHOD if_http_entity~get_form_fields.

  ENDMETHOD.

  METHOD if_http_entity~get_form_fields_cs.

  ENDMETHOD.

  METHOD if_http_entity~get_form_field_cs.

  ENDMETHOD.

  METHOD if_http_entity~get_header_field.

  ENDMETHOD.

  METHOD if_http_entity~get_header_fields.

  ENDMETHOD.

  METHOD if_http_entity~get_last_error.

  ENDMETHOD.

  METHOD if_http_entity~get_multipart.

  ENDMETHOD.

  METHOD if_http_entity~get_serialized_message_length.

  ENDMETHOD.

  METHOD if_http_entity~get_version.
    version = me->version.
  ENDMETHOD.

  METHOD if_http_entity~num_multiparts.
    num = multipart->num_multiparts( ).
  ENDMETHOD.

  METHOD if_http_entity~set_cdata.
    CLEAR me->data.
    me->cdata = data.
  ENDMETHOD.

  METHOD if_http_entity~set_compression.

  ENDMETHOD.

  METHOD if_http_entity~set_content_type.

  ENDMETHOD.

  METHOD if_http_entity~set_cookie.

  ENDMETHOD.


  METHOD if_http_entity~set_data.
    CLEAR cdata.
    me->data = data.
  ENDMETHOD.


  METHOD if_http_entity~set_formfield_encoding.

  ENDMETHOD.


  METHOD if_http_entity~set_form_field.

    ASSIGN form_fields[ name = name ] TO FIELD-SYMBOL(<form_field>).
    IF sy-subrc <> 0.
      INSERT VALUE #( name = name ) INTO TABLE form_fields ASSIGNING <form_field>.
    ENDIF.
    <form_field>-value = value.

  ENDMETHOD.


  METHOD if_http_entity~set_form_fields.
    me->form_fields = fields.
  ENDMETHOD.


  METHOD if_http_entity~set_header_field.

    DATA(lower_name) = to_lower( name ).
    ASSIGN header_fields[ name = lower_name ] TO FIELD-SYMBOL(<header_field>).
    IF sy-subrc <> 0.
      INSERT VALUE #( name = lower_name ) INTO TABLE header_fields ASSIGNING <header_field>.
    ENDIF.
    <header_field>-value = value.

  ENDMETHOD.


  METHOD if_http_entity~set_header_fields.
    CLEAR me->header_fields.
    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
      if_http_entity~set_header_field( name = <field>-name value = <field>-value ).
    ENDLOOP.
  ENDMETHOD.


  METHOD if_http_entity~suppress_content_type.

  ENDMETHOD.

  METHOD if_http_entity~to_xstring.

  ENDMETHOD.

ENDCLASS.
