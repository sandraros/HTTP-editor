CLASS zcx_httped DEFINITION INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        text     TYPE string OPTIONAL
        previous LIKE previous OPTIONAL
        textid   LIKE textid OPTIONAL.

    METHODS get_text REDEFINITION.

    METHODS get_longtext REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: text TYPE string.

ENDCLASS.



CLASS zcx_httped IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
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

