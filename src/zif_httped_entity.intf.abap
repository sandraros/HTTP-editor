INTERFACE zif_httped_entity
  PUBLIC .

  INTERFACES if_http_entity.

  DATA: multipart TYPE REF TO zcl_httped_multipart READ-ONLY.

ENDINTERFACE.
