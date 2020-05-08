program main
  use hdf5, only : h5open_f, h5close_f, h5gopen_f, h5gclose_f, h5fopen_f, &
    h5fclose_f, h5dopen_f, h5dclose_f, h5dread_f, h5f_acc_rdonly_f, h5t_ieee_f32le, &
    h5t_ieee_f64le, hid_t, hsize_t, h5dget_space_f, h5sget_simple_extent_dims_f
  implicit none
  !
  integer, parameter :: sp = selected_real_kind( 6, 37 )
  integer, parameter :: dp = selected_real_kind( 15, 307 )
  !
  real(sp), allocatable :: highestreturn(:), lowestmode(:)
  real(dp), allocatable :: hr_lat(:), hr_lon(:), lm_lat(:), lm_lon(:)
  !
  character(len=*), parameter :: filename = "GEDI02_A_2019219212118_O03696_T01120_02_001_01.h5"
  integer(hid_t) :: file_id, grp_id, dset_id, dspace_id
  integer(hsize_t) :: dset_dim(1), maxdims(1), mt, j
  character(:), allocatable :: grp_name
  character(4) :: beam_nb
  character(132) :: directory
  integer :: error
  !
  integer :: hr_unit, lm_unit, i, dir_len
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
  !
  do i = 0, 7
    write( beam_nb, '(b4.4)' ) beam(i)
    ! Open Group
    grp_name = "BEAM"//beam_nb
    call h5gopen_f(file_id, grp_name, grp_id, error )
    ! highest return elvation
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
    ! latitude
    call h5dopen_f(grp_id, "lat_highestreturn", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, hr_lat, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! longitude
    call h5dopen_f(grp_id, "lon_highestreturn", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, hr_lon, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! lowest mode elevation
    call h5dopen_f(grp_id, "elev_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f32le, lowestmode, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! latitude
    call h5dopen_f(grp_id, "lat_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, lm_lat, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! longitude
    call h5dopen_f(grp_id, "lon_lowestmode", dset_id, error )
    call h5dread_f(dset_id, h5t_ieee_f64le, lm_lon, dset_dim, error)
    call h5dclose_f(dset_id, error)
    ! Close Group
    call h5gclose_f(grp_id, error)
    ! Open Output Files
    open( newunit = hr_unit, file = directory(1:dir_len)//"/data/highestreturn"//beam_nb//".dat", &
      action = "write", status = "replace", form = "unformatted" )
    open( newunit = lm_unit, file = directory(1:dir_len)//"/data/lowestmode"//beam_nb//".dat", &
      action = "write", status = "replace", form = "unformatted" )
    ! Write Extracted Data
    do j = 1, mt
      !if( hr_lat(j)>=35._dp .and. hr_lat(j)<=36._dp .and. hr_lon(j)>=5._dp &
      !  .and. hr_lon(j)<=6._dp ) &
        write( hr_unit ) hr_lon(j), hr_lat(j), highestreturn(j)
      !if( lm_lat(j)>=35._dp .and. lm_lat(j)<=36._dp .and. lm_lon(j)>=5._dp &
      !  .and. lm_lon(j)<=6._dp ) &
        write( lm_unit ) lm_lon(j), lm_lat(j), lowestmode(j)
    end do
    ! Close Output Files
    close( hr_unit )
    close( lm_unit )
    !
    deallocate( highestreturn, lowestmode, hr_lat, hr_lon, lm_lat, lm_lon )
    !
  end do
  ! Close File
  call h5fclose_f(file_id, error)
  ! Close Fortran Interface
  call h5close_f(error)
  !
end program main
