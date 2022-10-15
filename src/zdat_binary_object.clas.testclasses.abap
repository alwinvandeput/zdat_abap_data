*"* use this source file for your ABAP unit test classes

CLASS unit_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: create_by_binary_tab FOR TESTING.

ENDCLASS.       "unit_Test


CLASS unit_test IMPLEMENTATION.

  METHOD create_by_binary_tab.

    TRY.

        "--------------------------------------------------------------------
        "String
        "--------------------------------------------------------------------
        "CREATE_BY_STRING
        DATA(exp_data_string) =
          |AAA. Dit is een test.\n{ cl_abap_char_utilities=>cr_lf }. | &&
          |This is the next line.| &&
          |A new line { cl_abap_char_utilities=>newline }| &&
          |Horizontal tab { cl_abap_char_utilities=>horizontal_tab }|.

        exp_data_string = exp_data_string && exp_data_string && exp_data_string.

        DATA(exp_text_length) = strlen( exp_data_string ).

        DATA(xstring_binary_object) = zdat_binary_object=>create_by_string( exp_data_string ).

        DATA(act_data_string) = xstring_binary_object->get_string( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_string
            exp = exp_data_string ).

        "--------------------------------------------------------------------
        "Initial data
        "--------------------------------------------------------------------
        DATA(exp_data_xstring)  = xstring_binary_object->get_xstring( ).
        DATA(exp_binary_length) = xstring_binary_object->get_binary_length( ).

        cl_abap_unit_assert=>assert_not_initial( exp_data_xstring ).

        "--------------------------------------------------------------------
        "XString
        "--------------------------------------------------------------------
        "CREATE_BY_XSTRING
        xstring_binary_object = zdat_binary_object=>create_by_xstring( exp_data_xstring ).

        DATA(act_data_xstring) = xstring_binary_object->get_xstring( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_xstring
            exp = exp_data_xstring ).

        CLEAR xstring_binary_object.

        "--------------------------------------------------------------------
        "SOLIX tab
        "--------------------------------------------------------------------
        "CREATE_BY_SOLIX_TAB
        xstring_binary_object = zdat_binary_object=>create_by_xstring( exp_data_xstring ).
        DATA(exp_solix_tab) = xstring_binary_object->get_solix_tab( ).
        cl_abap_unit_assert=>assert_not_initial( exp_solix_tab ).

        DATA(solix_tab_binary_object) =
          zdat_binary_object=>create_by_solix_tab(
            solix_tab     = exp_solix_tab[]
            binary_length = exp_binary_length ).

        act_data_xstring = solix_tab_binary_object->get_xstring( ).

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

        "--------------------------------------------------------------------
        "SOLISTI1 (text)
        "--------------------------------------------------------------------
        DATA(solisti1_binary_object) = zdat_binary_object=>create_by_xstring( exp_data_xstring ).

        DATA(exp_solisti1_tab) = solisti1_binary_object->get_solisti1_tab( ).
        cl_abap_unit_assert=>assert_not_initial( exp_solisti1_tab ).

        "CREATE_BY_SOLISTI1_TAB
        solisti1_binary_object =
          zdat_binary_object=>create_by_solisti1_tab(
            solisti1_tab = exp_solisti1_tab
            text_length  = exp_text_length ).

        act_data_xstring  = solisti1_binary_object->get_xstring( ).
        act_binary_length = solisti1_binary_object->get_binary_length( ).
        DATA(act_text_length)   = solisti1_binary_object->get_text_length( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_data_xstring
            exp = exp_data_xstring ).

        cl_abap_unit_assert=>assert_equals(
            act = act_binary_length
            exp = exp_binary_length ).

        "GET_SOLISTI1_TAB
        DATA(act_solisti1_tab) = solisti1_binary_object->get_solisti1_tab( ).
        act_binary_length      = solisti1_binary_object->get_binary_length( ).

        cl_abap_unit_assert=>assert_equals(
            act = act_solisti1_tab
            exp = exp_solisti1_tab ).

        cl_abap_unit_assert=>assert_equals(
            act = act_binary_length
            exp = exp_binary_length ).

        "--------------------------------------------------------------------
        "GET_RSPOLPBI_TAB
        "--------------------------------------------------------------------
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
