INTERFACE zif_httped_request
  PUBLIC .

  INTERFACES if_http_request.
  INTERFACES zif_httped_entity.

  METHODS get_url
    RETURNING
      VALUE(result) TYPE string.

  METHODS set_url
    IMPORTING
      value TYPE string.

ENDINTERFACE.
