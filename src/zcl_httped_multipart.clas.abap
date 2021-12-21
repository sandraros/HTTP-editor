CLASS zcl_httped_multipart DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        position TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zif_httped_entity.

    METHODS delete
      IMPORTING
        position TYPE i DEFAULT 0.

    METHODS get
      IMPORTING
        position TYPE i DEFAULT 0.

    methods num_multiparts
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_httped_multipart IMPLEMENTATION.


  METHOD add.

    result = NEW zcl_httped_entity( ).

  ENDMETHOD.


  METHOD delete.

  ENDMETHOD.


  METHOD get.

  ENDMETHOD.


  METHOD num_multiparts.

  ENDMETHOD.


ENDCLASS.
