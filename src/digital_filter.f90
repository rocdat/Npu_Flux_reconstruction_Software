module digital_filter
  !
  use module_kind_types
  !
  implicit none
  !
  public :: init_digital_filter
  public :: update_dgft_data
  public :: allocate_arrays
  public :: gen_rand
  public :: set_filter_coef
  public :: apply_filter
  !
  ! private
  ! Scalars
  integer, save :: ncy,ncz
  integer, save :: ncy_intl,ncz_intl
  integer, save :: ncy_halo,ncz_halo
  integer, save :: ncy_all,ncz_all
  integer, save :: npy_all,npz_all
  real(wp), save :: int_ts
  logical(lk), save :: use_tgbc_dgft = fals
  ! Arrays
  real(wp), allocatable, save, public :: rand_data(:,:,:)
  real(wp), allocatable, save :: filt_coef(:,:)
  real(wp), allocatable, save, public :: filtered_data(:,:,:)
  real(wp), allocatable, save, public :: filtered_data_old(:,:,:)
  real(wp), allocatable, save, public :: rey_sts(:,:)
  !
contains
  !
  subroutine init_digital_filter(in_ncy,in_ncz, &
      in_intl_y,in_intl_z,in_bg_dy,in_bg_dz, &
      in_intl_x,in_inlet_u)
    !
    implicit none
    !
    integer, intent(in) :: in_ncy
    integer, intent(in) :: in_ncz
    real(wp), intent(in) :: in_intl_y
    real(wp), intent(in) :: in_intl_z
    real(wp), intent(in) :: in_bg_dy
    real(wp), intent(in) :: in_bg_dz
    real(wp), intent(in) :: in_intl_x
    real(wp), intent(in) :: in_inlet_u
    !
    ! Local Scalars
    integer :: ncy_halo_intl_ratio,ncz_halo_intl_ratio
    real(wp) :: u_scale,u
    !
  continue
    !
    ncy = in_ncy
    ncz = in_ncz
    ! npy = ncy + 1
    ! npz = ncz + 1
    !
    ncy_intl = in_intl_y / in_bg_dy
    ncz_intl = in_intl_z / in_bg_dz
    !
    ! This ratio could be adjusted. But 2 should be enough.
    ncy_halo_intl_ratio = 2
    ncz_halo_intl_ratio = 2
    !
    ncy_halo = ncy_halo_intl_ratio * ncy_intl
    ncz_halo = ncz_halo_intl_ratio * ncz_intl
    ncy_all = ncy + 2*ncy_halo
    ncz_all = ncz + 2*ncz_halo
    !
    npy_all = ncy_all + 1
    npz_all = ncz_all + 1
    !
    ! int_ts: Integral time scale
    u_scale = 0.6
    u = u_scale * in_inlet_u
    int_ts = in_intl_x / u
    !
    ! Allocate arrays
    call allocate_arrays
    !
    ! Set up the filter coefficients
    call set_filter_coef
    !
    ! Load the Reynolds stress profile
    call load_Reynolds_stress
    !
    ! Set up the flag saying the turbulence generation via the digital filter
    ! enabled.
    use_tgbc_dgft = true
    !
  end subroutine init_digital_filter
  !
  subroutine update_dgft_data(dt)
    !
    implicit none
    !
    ! Formal Arguments
    real(wp), intent(in) :: dt
    !
  continue
    !
    call gen_rand
    !
    call apply_filter(dt)
    !
  end subroutine update_dgft_data
  !
  subroutine allocate_arrays
    !
    implicit none
    !
    !
  continue
    !
    allocate( rand_data( -ncy_halo:ncy+ncy_halo, -ncz_halo:ncz+ncz_halo, 1:3 ) )
    allocate( filt_coef( -ncy_halo:ncy_halo, -ncz_halo:ncz_halo ) )
    allocate( filtered_data( 0:ncy, 0:ncz, 1:3 ) )
    allocate( filtered_data_old( 0:ncy, 0:ncz, 1:3 ), source=zero )
    !
  end subroutine allocate_arrays
  !
  subroutine gen_rand
    !
    use mersenne_twister, only : int32,ieee64
    use mersenne_twister, only : mtprng_state
    use mersenne_twister, only : mtprng_init,mtprng_rand_real3
    !
    implicit none
    !
    integer(int32) :: seed
    type(mtprng_state) :: state
    real(ieee64) :: rnd1,rnd2
    !
    integer :: i,j,k
    !
  continue
    !
    call system_clock(seed)
    call mtprng_init(seed,state)
    !
    do k = 1, 3
      do j = lbound(rand_data,dim=2), ubound(rand_data,dim=2)
        do i = lbound(rand_data,dim=1), ubound(rand_data,dim=1)
          !
          rnd1 = mtprng_rand_real3(state)
          rnd2 = mtprng_rand_real3(state)
          rand_data(i,j,k) = sqrt(-two*log(rnd1)) * cos(two*PI*rnd2)
          !
        end do
      end do
    end do
    !
  end subroutine gen_rand
  !
  subroutine set_filter_coef
    !
    implicit none
    !
    integer :: k
    real(wp) :: fc_norm2
    !
    real(wp), allocatable :: filt_coef_y(:,:)
    real(wp), allocatable :: filt_coef_z(:,:)
    !
    !
  continue
    !
    allocate(filt_coef_y(-ncy_halo:ncy_halo,1))
    allocate(filt_coef_z(1,-ncz_halo:ncz_halo))
    !
    ! Y direction
    filt_coef_y(:,1) = exp( -PI * [(abs(k),k=-ncy_halo,ncy_halo)] / ncy_intl )
    ! F2008 norm2
    fc_norm2 = norm2(filt_coef_y(:,1))
    !
    filt_coef_y(:,1) = filt_coef_y(:,1) / fc_norm2
    !
    ! Z direction
    filt_coef_z(1,:) = exp( -PI * [(abs(k),k=-ncz_halo,ncz_halo)] / ncz_intl )
    ! F2008 norm2
    fc_norm2 = norm2(filt_coef_z(1,:))
    !
    filt_coef_z(1,:) = filt_coef_z(1,:) / fc_norm2
    !
    ! Tensor product to form Y-Z
    filt_coef = matmul(filt_coef_y,filt_coef_z)
    !
    ! Deallocate the memory
    deallocate(filt_coef_y)
    deallocate(filt_coef_z)
    !
  end subroutine set_filter_coef
  !
  subroutine apply_filter(dt)
    !
    implicit none
    !
    ! Formal Arguments
    real(wp), intent(in) :: dt
    !
    integer :: i,j,k
    real(wp) :: s
    !
  continue
    !
    ! Spatial
    ! Only working for 3D
    do k = 1,3
      do j = 0, ncz
        do i = 0, ncy
          !
          filtered_data(i,j,k) = sum( filt_coef &
            * rand_data( i-ncy_halo:i+ncy_halo, j-ncz_halo:j+ncz_halo, k ) )
          !
        end do
      end do
    end do
    !
    ! Temporal
    s = exp(-half*PI*dt/int_ts)
    filtered_data = filtered_data_old*s + filtered_data*sqrt(one-s**2)
    filtered_data_old = filtered_data
    !
  end subroutine apply_filter
  !
  subroutine load_Reynolds_stress
    !
    use io, only : read_in_matrix
    use ovar, only : tgbc_dgft_input
    !
    implicit none
    !
    real(wp), allocatable :: tmat(:,:)
    integer :: mat_shape(1:2)
    !
  continue
    !
    call read_in_matrix(tgbc_dgft_input%ReySts_fname,tmat, &
      tgbc_dgft_input%ReySts_fdelim,mat_shape)
    !
    call reallocate( tmat, mat_shape(1),mat_shape(2) )
    !
    allocate(rey_sts(1:mat_shape(2),1:mat_shape(1)), &
      stat=ierr,errmsg=error_message)
    call alloc_error(pname,"rey_sts",1,__LINE__,__FILE__,ierr, &
      error_message)
    !
    rey_sts(:,:) = transpose(tmat)
    !
    deallocate(tmat,stat=ierr,errmsg=error_message)
    call alloc_error(pname,"tmat",2,__LINE__,__FILE__,ierr,error_message)
    !
  end subroutine load_Reynolds_stress
  ! !
  ! subroutine apply_lund_transform
  !   !
  !   implicit none
  !   !
  !   !
  ! continue
  !   !
  !   !
  ! end subroutine apply_lund_transform
  !
  ! subroutine calc_thermal_vars
  !   !
  !   implicit none
  !   !
  !   !
  ! continue
  !   !
  !   !
  ! end subroutine calc_thermal_vars
  !
  !
  !
end module digital_filter
