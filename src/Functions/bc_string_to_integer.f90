pure function bc_string_to_integer(bc_text) result(return_value)
  !
  !.. Formal Arguments ..
  character(len=*), intent(in) :: bc_text
  !
  !.. Function Return Value ..
  integer :: return_value
  !
  !.. Local Scalars ..
  integer :: n,nb,ne
  character(len=len_trim(bc_text)) :: text
  !
  !.. Local Parameters ..
  character(len=*), parameter :: cbnd   = "BOUNDARY"
  character(len=*), parameter :: csuper = "SUPERSONIC"
  character(len=*), parameter :: csonic = "SONIC"
  !
continue
  !
  ! Copy the input BC string into text so we can manipulate it
  !
  text = uppercase( trim(adjustl(bc_text)) )
  !
  nb = 1
  ne = len_trim(text)
  !
  ! Remove all spaces from the character string
  !
  do
    n = index(text(nb:ne)," ")
    if (n == 0) exit
    text = text(nb:n-1) // text(n+1:ne)
    ne = len_trim(text)
  end do
  !
  ! Remove all underscores from the character string
  !
  do
    n = index(text(nb:ne),"_")
    if (n == 0) exit
    text = text(nb:n-1) // text(n+1:ne)
    ne = len_trim(text)
  end do
  !
  ! Remove the substring "BOUNDARY" if it exists
  !
  n = index(text(nb:ne),cbnd)
  if (n > 0) then
    text = text(nb:n-1) // text(n+len_trim(cbnd):ne)
    ne = len_trim(text)
  end if
  !
  ! Remove the substring "BC" from the beginning of the string
  !
  if (text(1:2) == "BC") then
    text = text(3:len_trim(text))
    ne = len_trim(text)
  end if
  !
  ! Change the substring "SUPERSONIC" TO "SUP" if it exists
  !
  n = index(text(nb:ne),csuper)
  if (n > 0) then
    text = text(nb:n+2) // text(n+len_trim(csuper):ne)
    ne = len_trim(text)
  end if
  !
  ! Remove the substring "SONIC" if it exists
  !
  n = index(text(nb:ne),csonic)
  if (n > 0) then
    text = text(nb:n-1) // text(n+len_trim(csonic):ne)
    ne = len_trim(text)
  end if
  !
  ! Use the character string given in the PhysicalName
  ! section to set the value of the boundary condition\
  !
  ! For periodic BC. First check the ending number.
  ne = len_trim(adjustl(text))
  n = char2integer(text(ne:ne))
  if ( n /= -1 ) then
    !
    ! The ending number is an integer. This is a periodic BC
    ! Remove the ending dash and integer.
    text = text(1:ne-2)
    !
  end if
  !
  select case (trim(adjustl(text)))
    !
    case ("MESHINTERIOR","INTERIOR")
      return_value = not_a_bc
      !
    case ("INFLOW","INLET")
      return_value = bc_generic_inflow
    case ("SUBINFLOW","INFLOWSUB")
      return_value = bc_sub_inflow
    case ("SUPINFLOW","INFLOWSUP")
      return_value = bc_sup_inflow
      !
    case ("OUTFLOW","EXIT")
      return_value = bc_generic_outflow
    case ("SUBOUTFLOW","OUTFLOWSUB")
      return_value = bc_sub_outflow
    case ("SUPOUTFLOW","OUTFLOWSUP","EXTRAP","EXTRAPOLATE")
      return_value = bc_sup_outflow
      !
    case ("FREEFLOW","FARFIELD")
      return_value = bc_generic_freeflow
    case ("CHARACTERISTIC","CHAR","CHARACTER")
      return_value = bc_characteristic
    case ("FREESTREAM","REFERENCE")
      return_value = bc_freestream
    case ("FIXED")
      return_value = bc_fixed
      !
    case ("CUSTOMPROFILE")
      return_value = bc_custom_profile
    case ("TURBULENCEGENERATION","INFLOWTURBULENCE")
      return_value = bc_turbulence_generation
      !
    case ("WALL","WALLVISCOUS")
      return_value = bc_default_wall
    case ("SWALL","SLIP","SLIPWALL","WALLINVISCID")
      return_value = bc_slip_wall
    case ("EWALL","EULER","EULERWALL")
      return_value = bc_euler_wall
    case ("AWALL","ADIABATIC","WALLVISCOUSHEATFLUX","ADIABATICWALL")
      return_value = bc_adiabatic_wall
    case ("IWALL","ISOTHERMAL","WALLVISCOUSISOTHERMAL","ISOTHERMALWALL")
      return_value = bc_isothermal_wall
      !
    case ("SYMM","SYMMETRY","SYMMETRYPLANE")
      return_value = bc_symmetry
    case ("PERIODIC")
      !
      ! Periodic BC must be in pair. So check the ending number of periodic BC
      ! to see if it is odd or even.
      nb = ceiling(real(n)/2)
      ne = merge(nb,-nb,modulo(n,2)==1)
      ! So the periodic BC
      return_value = bc_periodic_list(ne)
    case ("MMS_DIRICHLET","MMSDIRICHLET")
      return_value = bc_mms_dirichlet
      !
    case("NULL")
      return_value = bc_unsupported
    case("USERDEFINED")
      return_value = bc_unsupported
    case("GENERAL")
      return_value = bc_unsupported
    case("DIRICHLET")
      return_value = bc_unsupported
    case("NEUMANN")
      return_value = bc_unsupported
    case("TUNNELINFLOW","TUNNELOUTFLOW")
      return_value = bc_unsupported
    case("DEGENERATELINE","DEGENERATEPOINT")
      return_value = bc_unsupported
    case("SYMMETRYPOLAR","AXISYMMETRICWEDGE")
      return_value = bc_unsupported
    case ("FAMILYSPECIFIED")
      return_value = bc_unsupported
      !
    case default
      return_value = bc_unknown
      !
  end select
  !
end function bc_string_to_integer
!
!###############################################################################
!
pure function char2integer(in_char) result(out_int)
  !
  implicit none
  !
  character, intent(in) :: in_char
  logical :: is_int
  integer :: out_int
  integer :: err
  !
continue
  !
  read(in_char,*,iostat=err) out_int
  is_int = err == 0
  !
  ! If in_char is not an integer, return -1 as a flag
  out_int = merge(out_int,-1,is_int)
  !
end function char2integer

