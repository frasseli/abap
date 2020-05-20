class ZCL_ZSD_KPI_MPC_EXT definition
  public
  inheriting from ZCL_ZSD_KPI_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSD_KPI_MPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZSD_KPI_MPC_EXT->DEFINE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] /IWBEP/CX_MGW_MED_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD define.

    super->define( ).

    "model->set_no_conversion( iv_no_conversion = abap_true ).

*    DATA(lr_entity) = model->get_entity_type( iv_entity_name = 'OpenSalesOrders' ).
*    lr_entity->get_property( iv_property_name = 'Kursk' )->disable_conversion( ).

  ENDMETHOD.
ENDCLASS.
