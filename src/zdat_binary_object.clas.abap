CLASS zdat_binary_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      t_solisti1_tab TYPE STANDARD TABLE OF solisti1 WITH DEFAULT KEY .

    CLASS-METHODS create_by_xstring
      IMPORTING
        !data_xstring        TYPE xstring
      RETURNING
        VALUE(binary_object) TYPE REF TO zdat_binary_object .
    CLASS-METHODS create_by_string
      IMPORTING
        !data_string         TYPE string
      RETURNING
        VALUE(binary_object) TYPE REF TO zdat_binary_object
      RAISING
        zcx_return3 .
    CLASS-METHODS create_by_generic_binary_tab
      IMPORTING
        !binary_tab          TYPE STANDARD TABLE
        !binary_length       TYPE i
      RETURNING
        VALUE(binary_object) TYPE REF TO zdat_binary_object
      RAISING
        zcx_return3 .
    CLASS-METHODS create_by_solix_tab
      IMPORTING
        !solix_tab           TYPE solix_tab
        !binary_length       TYPE i
      RETURNING
        VALUE(binary_object) TYPE REF TO zdat_binary_object
      RAISING
        zcx_return3 .
    CLASS-METHODS create_by_solisti1_tab
      IMPORTING
        !solisti1_tab        TYPE t_solisti1_tab
        !text_length         TYPE i
      RETURNING
        VALUE(binary_object) TYPE REF TO zdat_binary_object
      RAISING
        zcx_return3 .
    METHODS get_xstring
      RETURNING
        VALUE(data_xstring) TYPE xstring .
    METHODS get_string
      RETURNING
        VALUE(data_string) TYPE string
      RAISING
        zcx_return3 .
    METHODS get_rspolpbi_tab
      RETURNING
        VALUE(binary_tab) TYPE rmps_rspolpbi .
    METHODS get_binary_length
      RETURNING
        VALUE(size) TYPE i .
    METHODS get_binary_length_text
      RETURNING
        VALUE(size_text) TYPE char12 .
    METHODS get_text_length
      RETURNING
        VALUE(text_length) TYPE i
      RAISING
        zcx_return3 .
    METHODS get_solix_tab
      RETURNING
        VALUE(solix_tab) TYPE solix_tab .
    METHODS get_solisti1_tab
      RETURNING
        VALUE(solisti1_tab) TYPE t_solisti1_tab
      RAISING
        zcx_return3 .
    METHODS get_tbl1024_tab
      RETURNING
        VALUE(tbl1024_tab) TYPE tabl1024_t
      RAISING
        zcx_return3 .
    METHODS get_generic_binary_tab
      CHANGING
        !raw_line_tab TYPE STANDARD TABLE
      RAISING
        zcx_return3 .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA new_data_xstring TYPE xstring .

    DATA content_xstring_data_object TYPE REF TO data.

    METHODS _set_data_object.

ENDCLASS.



CLASS ZDAT_BINARY_OBJECT IMPLEMENTATION.


  METHOD create_by_generic_binary_tab.

    binary_object = NEW #( ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = binary_length
      IMPORTING
        buffer       = binary_object->new_data_xstring
      TABLES
        binary_tab   = binary_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_return3
        USING MESSAGE.
    ENDIF.

    binary_object->content_xstring_data_object = REF #( binary_object->new_data_xstring ).

  ENDMETHOD.


  METHOD create_by_solisti1_tab.

    "--------------------------------------
    "Convert to SOLI_TAB
    "--------------------------------------
    DATA(solisti1_tab_line_count) = lines( solisti1_tab ).

    DATA soli_tab TYPE soli_tab.
    DATA text_size TYPE i.

    LOOP AT solisti1_tab
      ASSIGNING FIELD-SYMBOL(<solist>).

      IF sy-tabix < solisti1_tab_line_count.
        text_size = text_size + 255.
      ELSE.
        text_size = text_size + strlen( <solist>-line ).
      ENDIF.

      APPEND INITIAL LINE TO soli_tab
        ASSIGNING FIELD-SYMBOL(<soli>).

      <soli>-line = <solist>-line.

    ENDLOOP.

    "--------------------------------------
    "Convert to XSTRING
    "--------------------------------------
    binary_object = NEW #( ).

    TRY.

        binary_object->new_data_xstring =
         cl_bcs_convert=>txt_to_xstring(
           it_soli     = soli_tab
           iv_size     = text_size ).

      CATCH cx_bcs INTO DATA(bcs_exc).

        DATA(return_exc) = NEW zcx_return3( ).
        return_exc->add_exception_object( bcs_exc ).
        RAISE EXCEPTION return_exc.

    ENDTRY.

    binary_object->_set_data_object( ).

  ENDMETHOD.


  METHOD create_by_solix_tab.

    binary_object = create_by_generic_binary_tab(
      binary_tab    = solix_tab
      binary_length = binary_length ).

  ENDMETHOD.


  METHOD create_by_string.

    binary_object = NEW #( ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = data_string
      IMPORTING
        buffer = binary_object->new_data_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_return3
        USING MESSAGE.

    ENDIF.

    binary_object->_set_data_object( ).

  ENDMETHOD.


  METHOD create_by_xstring.

    binary_object = NEW #( ).

    binary_object->new_data_xstring = data_xstring.

    binary_object->_set_data_object( ).

  ENDMETHOD.


  METHOD get_binary_length.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    size = xstrlen( <content_xstring> ).

  ENDMETHOD.


  METHOD get_binary_length_text.

    size_text = shift_left( me->get_binary_length( ) ).

  ENDMETHOD.


  METHOD get_generic_binary_tab.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    TRY.

        CALL METHOD cl_bcs_convert=>xstring_to_xtab
          EXPORTING
            iv_xstring = <content_xstring>
          IMPORTING
            et_xtab    = raw_line_tab.

      CATCH cx_bcs INTO DATA(bcs_exc).

        DATA(return_exc) = NEW zcx_return3( ).

        return_exc->add_exception_object( bcs_exc ).

        RAISE EXCEPTION return_exc.

    ENDTRY.

  ENDMETHOD.


  METHOD get_rspolpbi_tab.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = <content_xstring>
      TABLES
        binary_tab = binary_tab.

  ENDMETHOD.


  METHOD get_solisti1_tab.

    DATA(string) = me->get_string( ).

    TRY.

        cl_bcs_convert=>string_to_tab(
          EXPORTING
            iv_string = string
          IMPORTING
            et_ctab   = solisti1_tab ).

      CATCH cx_bcs INTO DATA(bcs_exc).

        DATA(return3_exc) = NEW zcx_return3( ).

        return3_exc->add_exception_object( bcs_exc ).

        RAISE EXCEPTION return3_exc.

    ENDTRY.

  ENDMETHOD.


  METHOD get_solix_tab.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    solix_tab =
      cl_bcs_convert=>xstring_to_solix(
        iv_xstring = <content_xstring> ).

  ENDMETHOD.


  METHOD get_string.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    data_string = cl_bcs_convert=>xstring_to_string(
      iv_xstr   = <content_xstring>
      iv_cp     =  1100  ).      "SAP character set identification

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_return3
        USING MESSAGE.

    ENDIF.

  ENDMETHOD.


  METHOD get_tbl1024_tab.

    get_generic_binary_tab(
      CHANGING raw_line_tab = tbl1024_tab ).

  ENDMETHOD.


  METHOD get_text_length.

    DATA(data_string) = me->get_string( ).

    text_length = strlen( data_string ).

  ENDMETHOD.


  METHOD get_xstring.

    ASSIGN me->content_xstring_data_object->* TO  FIELD-SYMBOL(<content_xstring>).

    data_xstring = <content_xstring>.

  ENDMETHOD.


  METHOD _set_data_object.

    me->content_xstring_data_object = REF #( me->new_data_xstring ).

  ENDMETHOD.
ENDCLASS.
