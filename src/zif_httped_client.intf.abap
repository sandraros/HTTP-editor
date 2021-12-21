INTERFACE zif_httped_client
  PUBLIC .

  INTERFACES if_http_client.

  DATA: request TYPE REF TO zcl_httped_request READ-ONLY.
*          response        TYPE REF TO zcl_httped_request READ-ONLY.

ENDINTERFACE.
