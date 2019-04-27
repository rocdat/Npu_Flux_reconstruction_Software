pure function calc_tauw(vwflx,unrm) result(tau_w)
  !
  ! Use Statements
  ! use flowvar, only : tauw_fp
  ! use module_kind_types, only : wp,lk,zero,one,true,fals
  use geovar, only : nr
  use eqn_idx, only : nq
  use ovar, only : pv_in
  !
  ! Formal Arguments
  real(wp), dimension(1:nq), intent(in) :: vwflx
  real(wp), dimension(1:nr), intent(in) :: unrm
  !
  ! Function Result ..
  real(wp) :: tau_w
  ! Local Scalars
  integer :: i
  logical(lk) :: is_tauw_against_flow
  !
  ! Local Array
  real(wp), dimension(1:nr) :: tau_n_vec
  real(wp), dimension(1:nr) :: tau_w_vec
  !
  ! Local Parameter
  character(len=*), parameter :: pname = "calc_tauw"
  !
continue
  !
  do i = 1,nr
    tau_n_vec(i) = vwflx(nmb-1+i) * unrm(i)
  end do
  do i = 1,nr
    tau_w_vec(i) = vwflx(nmb-1+i) - tau_n_vec(i)
  end do
  !
  ! Check the direction of the tau_w vector
  ! Use the reference solution to determine
  is_tauw_against_flow = merge( fals, true, &
    dot(pv_in(nmb:nme), tau_w_vec) > zero )
  !
  tau_w = norm2(tau_w_vec) * merge(one,-one,is_tauw_against_flow)
  !
  ! Let us stop here. The first grid point away from the wall depends on the
  ! cell geometry. If we want to get y1, we need to find out the opposite
  ! face of the wall face in this wall cell. And that depends on the cell
  ! geometry. Probably only working on Quad and Hex. Let the code calculate
  ! tau_w and output the time averaged tau_w. y+ could be calculated based on
  ! that. Actually, tau_w is the true physical quantities while y+ is an
  ! virtuall concept. y+ is an estimate about the grid size.
  !
  ! utau = sqrt(tau_w / rho)
  ! ! Obtain nu
  ! nu = viscosity_cv()
  ! ! How to obtain y1? We need to find out the opposite face of this wall face.
  ! yplus_fp(k,i) = y1 * utau / nu
  !
end function calc_tauw
