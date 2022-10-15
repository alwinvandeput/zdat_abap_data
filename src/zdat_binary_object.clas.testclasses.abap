*"* use this source file for your ABAP unit test classes

CLASS unit_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_string FOR TESTING.

    METHODS test_xstring FOR TESTING.

    METHODS test_solix_tab FOR TESTING.

    METHODS test_solisti1_tab FOR TESTING.

    METHODS test_others FOR TESTING.


    METHODS _get_data_string
      RETURNING VALUE(data_string) TYPE string.

    METHODS _get_data_xstring
      RETURNING VALUE(data_xstring) TYPE xstring
      RAISING   zcx_return3.

ENDCLASS.       "unit_Test


CLASS unit_test IMPLEMENTATION.

  METHOD _get_data_string.

    data_string =
      |AAA. Dit is een test.\n{ cl_abap_char_utilities=>cr_lf }. | &&
      |This is the next line.| &&
      |A new line { cl_abap_char_utilities=>newline }| &&
      |Horizontal tab { cl_abap_char_utilities=>horizontal_tab }|.

    data_string = data_string && data_string && data_string.

  ENDMETHOD.

  METHOD _get_data_xstring.

    DATA(data_string) = me->_get_data_string( ).

    DATA(binary_object) = zdat_binary_object=>create_by_string( data_string ).
    data_xstring  = binary_object->get_xstring( ).
    DATA(exp_binary_length) = binary_object->get_binary_length( ).

    cl_abap_unit_assert=>assert_not_initial( data_xstring ).

  ENDMETHOD.

  METHOD test_string.

    TRY.

        "--------------------------------------------------------------------
        "String
        "--------------------------------------------------------------------
        "CREATE_BY_STRING
        DATA(exp_data_string) = me->_get_data_string( ).

        exp_data_string = exp_data_string && exp_data_string && exp_data_string.

        DATA(exp_text_length) = strlen( exp_data_string ).

        DATA(xstring_binary_object) = zdat_binary_object=>create_by_string( exp_data_string ).

        DATA(act_data_string) = xstring_binary_object->get_string( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_string
            exp = exp_data_string ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
            msg    = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_xstring.

    TRY.

        "--------------------------------------------------------------------
        "Initial data
        "--------------------------------------------------------------------
        DATA(exp_data_xstring)  = me->_get_data_xstring( ).
        DATA(exp_binary_length) = xstrlen( exp_data_xstring ).

        "--------------------------------------------------------------------
        "XString
        "--------------------------------------------------------------------
        "CREATE_BY_XSTRING
        DATA(xstring_binary_object) = zdat_binary_object=>create_by_xstring( exp_data_xstring ).

        DATA(act_data_xstring) = xstring_binary_object->get_xstring( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_xstring
            exp = exp_data_xstring ).

        CLEAR xstring_binary_object.

        "--------------------------------------------------------------------
        "XString data object
        "With a data object the data is not copied to, but a reference to the data is copied.
        "Performance wise it uses less memory in case of very large binary data.
        "--------------------------------------------------------------------
        "Copy the xstring
        DATA(exp_test_xstring) = exp_data_xstring.

        DATA exp_test_xstring_data_object TYPE REF TO data.
        exp_test_xstring_data_object = REF data( exp_test_xstring ).
        xstring_binary_object = zdat_binary_object=>create_by_xstring_data_object( exp_test_xstring_data_object ).

        DATA(act_test_xstring_data_object) = xstring_binary_object->get_xstring_data_object( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_test_xstring_data_object
            exp = exp_test_xstring_data_object ).

        "Check that the data is not copied in memory.
        ASSIGN act_test_xstring_data_object->* TO FIELD-SYMBOL(<test_xstring>).
        DATA(exp_test_string) = |Abc|.
        DATA(test_binary_object) = zdat_binary_object=>create_by_string( exp_test_string ).

        <test_xstring> = exp_test_string.

        cl_abap_unit_assert=>assert_equals(
            act = REF data( exp_test_xstring )
            exp = exp_test_xstring_data_object ).

        cl_abap_unit_assert=>assert_equals(
            act = act_test_xstring_data_object
            exp = exp_test_xstring_data_object ).

        DATA(act_test_string) = test_binary_object->get_string( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_test_string
            exp = exp_test_string ).

        CLEAR xstring_binary_object.

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
            msg    = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_solix_tab.

    TRY.

        "CREATE_BY_SOLIX_TAB
        DATA(exp_data_xstring) = me->_get_data_xstring( ).
        DATA(xstring_binary_object) = zdat_binary_object=>create_by_xstring( exp_data_xstring ).
        DATA(exp_binary_length) = xstrlen( exp_data_xstring ).

        DATA(exp_solix_tab) = xstring_binary_object->get_solix_tab( ).
        cl_abap_unit_assert=>assert_not_initial( exp_solix_tab ).

        DATA(solix_tab_binary_object) =
          zdat_binary_object=>create_by_solix_tab(
            solix_tab     = exp_solix_tab[]
            binary_length = exp_binary_length ).

        DATA(act_data_xstring) = solix_tab_binary_object->get_xstring( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_xstring
            exp = exp_data_xstring ).

        "GET_SOLIX_TAB
        DATA(act_solix_tab) = solix_tab_binary_object->get_solix_tab( ).
        DATA(act_binary_length) = solix_tab_binary_object->get_binary_length( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_solix_tab
            exp = exp_solix_tab ).

        cl_abap_unit_assert=>assert_equals(
            act = act_binary_length
            exp = exp_binary_length ).

        CLEAR solix_tab_binary_object.

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
            msg    = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_solisti1_tab.

    TRY.

        DATA(exp_text_string) = me->_get_data_string( ).
        DATA(exp_text_length) = strlen( exp_text_string ).
        DATA(binary_object) = zdat_binary_object=>create_by_string( exp_text_string ).

        DATA(exp_solisti1_tab) = binary_object->get_solisti1_tab( ).
        cl_abap_unit_assert=>assert_not_initial( exp_solisti1_tab ).

        "CREATE_BY_SOLISTI1_TAB
        binary_object =
          zdat_binary_object=>create_by_solisti1_tab(
            solisti1_tab = exp_solisti1_tab
            text_length  = exp_text_length ).

        data(act_text_string)  = binary_object->get_string( ).
        DATA(act_text_length)   = binary_object->get_text_length( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_text_string
            exp = exp_text_string ).

        cl_abap_unit_assert=>assert_equals(
            act = act_text_length
            exp = exp_text_length ).

        "GET_SOLISTI1_TAB
        DATA(act_solisti1_tab) = binary_object->get_solisti1_tab( ).
        act_text_length      = binary_object->get_text_length( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_solisti1_tab
            exp = exp_solisti1_tab ).

        cl_abap_unit_assert=>assert_equals(
            act = act_text_length
            exp = exp_text_length ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
            msg    = return3_exc->get_text( ) ).

    ENDTRY.


  ENDMETHOD.

  METHOD test_others.

    TRY.

        "--------------------------------------------------------------------
        "GET_RSPOLPBI_TAB
        "--------------------------------------------------------------------
        data(exp_data_xstring) = me->_get_data_xstring( ).

        DATA(rspolpbi_tab_binary_object) = zdat_binary_object=>create_by_xstring( exp_data_xstring ).

        DATA(act_rspolpbi_tab) = rspolpbi_tab_binary_object->get_rspolpbi_tab( ).
        cl_abap_unit_assert=>assert_not_initial( act_rspolpbi_tab ).

        "--------------------------------------------------------------------
        "GET_TBL1024_TAB
        "--------------------------------------------------------------------
        DATA(act_tbl1024_tab) = rspolpbi_tab_binary_object->get_tbl1024_tab( ).
        cl_abap_unit_assert=>assert_not_initial( act_tbl1024_tab ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
            msg    = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
