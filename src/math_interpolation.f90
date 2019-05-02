module math_interpolation
  !
  ! Currently, this is a simple linear interpolation. But I put it in this
  ! separate module file to be possible to extend to like a cubic spline
  ! interpolator.
  !
  use module_kind_types
  !
  implicit none
  !
  private
  !
  type, public :: interp_data_t
    !
    real(wp), allocatable :: x_arr(:)
    real(wp), allocatable :: f_arr(:)
    !
  contains
    !
    procedure, pass(this) :: init
    procedure, pass(this) :: eval
    procedure, pass(this) :: destroy
    !
  end type interp_data_t
  !
  ! type(interp_data_t), public, save ::
  !
contains
!
!###############################################################################
!
subroutine init(this,in_x,in_f)
  !
  !.. Passed Argument from Invoking Object ..
  class(interp_data_t), intent(inout) :: this
  real(wp), intent(in) :: in_x(:)
  real(wp), intent(in) :: in_f(:)
  !
  ! Local Scalar
  integer :: ierr
  integer :: nx
  !
  ! Local Array
  ! real(wp), allocatable :: in_x_diff(:)
  !
  ! Local Parameter
  character(len=*), parameter :: pname = "interp_data_t%init"
  !
continue
  !
  ! First check if the input coordinate array is monotonic
  nx = size(in_x)
  ! F2003 Auto Reallocation
  if ( any( ( in_x(2:nx) - in_x(1:nx-1) ) <= zero ) ) then
    !
    ! The slope calculation does not alow coincident coordinates.
    ! So it is <= zero.
    call stop_gfr(stop_mpi,pname,__LINE__,__FILE__, &
      "The input data coordinates for the interpolator are not monotonic!")
    !
  end if
  !
  ! Store the input data arrays
  ! F2003 Auto Reallocation
  this%x_arr = in_x
  this%f_arr = in_f
  !
  ! Calculate the interpolation coefficients
  !
end subroutine init
!
!###############################################################################
!
pure function eval(this,in_x) result(out_f)
  !
  ! Passed Argument from Invoking Object
  class(interp_data_t), intent(in) :: this
  real(wp), intent(in) :: in_x
  !
  ! Function Result
  real(wp) :: out_f
  !
  ! Local Scalar
  integer :: l1,l2
  real(wp) :: slope
  !
  ! Local Parameter
  character(len=*), parameter :: pname = "interp_data_t%eval"
  !
continue
  !
  ! Find the location of in_x in x_arr. We already have the assumption that
  ! x_arr is monotonic.
  l1 = minloc(abs( this%x_arr - in_x ), dim=1)
  !
  if ( this%x_arr(l1) < in_x ) then
    !
    ! in_x \in [x_arr(l1),x_arr(l2)]
    l2 = l1 + 1
    !
  else if ( this%x_arr(l1) > in_x ) then
    !
    ! in_x is smaller than x_arr(l1)
    l2 = l1
    l1 = l1 - 1
    !
  else
    !
    out_f = this%f_arr(l1); return
    !
  end if
  !
  slope = ( this%f_arr(l2) - this%f_arr(l1) ) &
        / ( this%x_arr(l2) - this%x_arr(l1) )
  out_f = this%f_arr(l1) + slope * ( in_x - this%x_arr(l1) )
  !
end function eval
!
!###############################################################################
!
subroutine destroy(this)
  !
  ! Passed Argument from Invoking Object
  class(interp_data_t), intent(inout) :: this
  !
  ! Local Scalar
  integer :: ierr
  !
  ! Local Parameter
  character(len=*), parameter :: pname = "interp_data_t%destropy"
  !
continue
  !
  if ( allocated(this%x_arr) ) then
    deallocate(this%x_arr,stat=ierr,errmsg=error_message)
    call alloc_error(pname,"interp_data_t%x_arr",2,__LINE__,__FILE__,ierr, &
      error_message)
  end if
  !
  if ( allocated(this%f_arr) ) then
    deallocate(this%f_arr,stat=ierr,errmsg=error_message)
    call alloc_error(pname,"interp_data_t%f_arr",2,__LINE__,__FILE__,ierr, &
      error_message)
  end if
  !
end subroutine destroy
!
end module math_interpolation
