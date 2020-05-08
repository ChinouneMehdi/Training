program main
  use hdf5, only : h5open_f, h5close_f, h5gopen_f, h5gclose_f, h5fopen_f, &
    h5fclose_f, h5dopen_f, h5dclose_f, h5dread_f, h5f_acc_rdonly_f, h5t_ieee_f32le, &
    h5t_ieee_f64le, hid_t, hsize_t, h5dget_space_f, h5sget_simple_extent_dims_f, &
    h5screate_simple_f, h5sclose_f, h5dcreate_f, h5dwrite_f, h5gcreate_f, h5fcreate_f, &
    h5f_acc_trunc_f
  implicit none
  !
  integer, parameter :: sp = selected_real_kind( 6, 37 )
  integer, parameter :: dp = selected_real_kind( 15, 307 )
  !
  real(sp), allocatable :: highestreturn(:), lowestmode(:)
  real(dp), allocatable :: hr_lat(:), hr_lon(:), lm_lat(:), lm_lon(:)
  !
  character(len=*), parameter :: filename = "GEDI02_A_2019219212118_O03696_T01120_02_001_01.h5"
  character(len=*), parameter :: filename2 = "data/gedi.h5"
  integer(hid_t) :: file_id, grp_id, dset_id, dspace_id
  integer(hid_t) :: file_id2, grp_id2, dset_id2, dspace_id2
  integer(hsize_t) :: dset_dim(1), maxdims(1), mt
  character(:), allocatable :: grp_name
  character(4) :: beam_nb
  character(132) :: directory
  integer :: error
  !
  integer :: i, dir_len
  integer, parameter :: beam(0:7) = [ 0, 1, 2, 3, 5, 6, 8, 11 ]
  !
  ! Get Current Working Directory
  call get_environment_variable( "PWD", directory, dir_len )
  ! Download Required Data
  call execute_command_line( "wget &
    &https://e4ftl01.cr.usgs.gov/GEDI/GEDI02_A.001/2019.08.07/"//filename//" -nv" )
  ! Open Fortran Interface
  call h5open_f(error)
  ! Open File
  call h5fopen_f( filename, h5f_acc_rdonly_f, file_id, error)
  ! Create Output File
  call h5fcreate_f( filename2, h5f_acc_trunc_f, file_id2, error)
  !
  do i = 0, 7
    write( beam_nb, '(b4.4)' ) beam(i)
    grp_name = "BEAM"//beam_nb
    ! Open Group
    call h5gopen_f(file_id, grp_name, grp_id, error )
    ! Create Output Group
    call h5gcreate_f(file_id2, grp_name, grp_id2, error )
    ! highest return elevation
    ! Get Data
    call h5dopen_f(grp_id, "elev_highestreturn", dset_id, error )
    ! Get dimensions
    call h5dget_space_f(dset_id, dspace_id, error )
    call h5sget_simple_extent_dims_f( dspace_id, dset_dim, maxdims, error )
    mt = product(dset_dim)
    allocate( highestreturn(mt), lowestmode(mt), hr_lat(mt), hr_lon(mt), &
      lm_lat(mt), lm_lon(mt) )
    ! Continue
    call h5dread_f(dset_id, h5t_ieee_f32le, highestreturn, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "elev_highestreturn", h5t_ieee_f32le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f32le, highestreturn, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    CALL h5sclose_f(dspace_id2, error)
    ! Latitude
    ! Get Data
    call h5dopen_f(grp_id, "lat_highestreturn", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, hr_lat, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "lat_highestreturn", h5t_ieee_f64le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f64le, hr_lat, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    CALL h5sclose_f(dspace_id2, error)
    ! Longitude
    ! Get Data
    call h5dopen_f(grp_id, "lon_highestreturn", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, hr_lon, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "lon_highestreturn", h5t_ieee_f64le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f64le, hr_lon, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    call h5sclose_f(dspace_id2, error)
    ! Lowest mode elevation
    ! Get Data
    call h5dopen_f(grp_id, "elev_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f32le, lowestmode, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "elev_lowestmode", h5t_ieee_f32le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f32le, lowestmode, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    call h5sclose_f(dspace_id2, error)
    ! Latitude
    ! Get Data
    call h5dopen_f(grp_id, "lat_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, lm_lat, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "lat_lowestmode", h5t_ieee_f64le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f64le, lm_lat, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    call h5sclose_f(dspace_id2, error)
    ! Longitude
    ! Get Data
    call h5dopen_f(grp_id, "lon_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, lm_lon, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Write Data
    call h5screate_simple_f( 1, dset_dim, dspace_id2, error )
    call h5dcreate_f(grp_id2, "lon_lowestmode", h5t_ieee_f64le, dspace_id2, &
       dset_id2, error)
    call h5dwrite_f(dset_id2, h5t_ieee_f64le, lm_lon, dset_dim, error)
    call h5dclose_f(dset_id2, error)
    call h5sclose_f(dspace_id2, error)
    ! Close Groups
    call h5gclose_f(grp_id, error)
    call h5gclose_f(grp_id2, error)
    !
    deallocate( highestreturn, lowestmode, hr_lat, hr_lon, lm_lat, lm_lon )
    !
  end do
  ! Close Files
  call h5fclose_f(file_id, error)
  call h5fclose_f(file_id2, error)
  ! Close Fortran Interface
  call h5close_f(error)
  !
end program main
