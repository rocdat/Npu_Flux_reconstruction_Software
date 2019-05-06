elemental function bc_integer_to_string(ibc) result(return_value)
  !
  integer, intent(in) :: ibc
  !
  character(len=28) :: return_value
  !
continue
  !
  if ( ibc == bc_generic_inflow ) then
    return_value = "generic_inflow"
  else if ( ibc == bc_sub_inflow ) then
    return_value = "sub_inflow"
  else if ( ibc == bc_sup_inflow ) then
    return_value = "sup_inflow"
  else if ( ibc == bc_mdot_inflow ) then
    return_value = "mdot_inflow"
  else if ( ibc == bc_generic_outflow ) then
    return_value = "generic_outflow"
  else if ( ibc == bc_sub_outflow ) then
    return_value = "sub_outflow"
  else if ( ibc == bc_sup_outflow ) then
    return_value = "sup_outflow"
  else if ( ibc == bc_mdot_outflow ) then
    return_value = "mdot_outflow"
  else if ( ibc == bc_generic_freeflow ) then
    return_value = "generic_freeflow"
  else if ( ibc == bc_characteristic ) then
    return_value = "characteristic"
  else if ( ibc == bc_freestream ) then
    return_value = "freestream"
  else if ( ibc == bc_fixed ) then
    return_value = "fixed"
  else if ( ibc == bc_custom_profile ) then
    return_value = "custom_profile"
  else if ( ibc == bc_turbulence_generation ) then
    return_value = "turbulence_generation"
  else if ( ibc == bc_slip_wall ) then
    return_value = "slip_wall"
  else if ( ibc == bc_euler_wall ) then
    return_value = "euler_wall"
  else if ( ibc == bc_adiabatic_wall ) then
    return_value = "adiabatic_wall"
  else if ( ibc == bc_isothermal_wall ) then
    return_value = "isothermal_wall"
  else if ( ibc == not_a_bc ) then
    return_value = "not_a_bc"
  else if ( ibc == bc_symmetry ) then
    return_value = "symmetry"
  else if ( any( ibc == bc_periodic_list ) ) then
    return_value = "periodic"
  else if ( ibc == bc_mms_dirichlet ) then
    return_value = "MMS_dirichlet"
  else if ( ibc == bc_cpu_bnd ) then
    return_value = "cpu_bnd"
  else if ( ibc == bc_unknown ) then
    return_value = "unknown"
  else
    return_value = "BC_INTEGER_TO_STRING ERROR!!"
  end if
  !
  return_value = uppercase(return_value)
  !
end function bc_integer_to_string
