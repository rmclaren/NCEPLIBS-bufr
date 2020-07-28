 
module bufr_c_interface_mod
  use iso_c_binding
  implicit none

  private
  public :: open_c
  public :: close_c
  public :: openbf_c
  public :: ireadmg_c
  public :: ireadsb_c
  public :: ufbint_c
  public :: ufbrep_c
  public :: dxdump_c

contains

  function c_to_f_string(c_str) result(str)
    character(kind=c_char,len=1), intent(in) :: c_str(*)
    character(len=:), allocatable :: str
    integer nchars

    nchars = 1
    do while (c_str(nchars) .ne. c_null_char)
       nchars = nchars + 1
    end do

    nchars = nchars - 1
    allocate(character(len=nchars) :: str)
    str = transfer(c_str(1:nchars), str)

  end function c_to_f_string


  ! function f_to_c_string(f_str) result(c_str)
  !   implicit none

  !   character(len=*), target, intent(in) :: f_str(*)

  !   character(len=len(f_str)), pointer :: f_str_ptr(:)
  !   type(c_ptr) :: c_str
  !   integer :: str_len

  !   f_str_ptr => f_str(1)
  !   str_len = len_trim(f_str_ptr)
  !   f_str_ptr(str_len+1:str_len+1) = c_null_char
  !   c_str = c_loc(f_str_ptr)
  ! end function


  subroutine open_c(unit, filepath) bind(C, name='open_f')
    integer(c_int), value, intent(in) :: unit
    character(kind=c_char, len=1) :: filepath

    open(unit, file=c_to_f_string(filepath))
  end subroutine open_c


  subroutine close_c(unit) bind(C, name='close_f')
    integer(c_int), value, intent(in) :: unit

    close(unit=unit)
  end subroutine close_c


  subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f') 
    integer(c_int), value, intent(in) :: bufr_unit
    character(kind=c_char, len=1), intent(in) :: cio
    integer(c_int), value, intent(in) :: table_file_id

    call openbf(bufr_unit, c_to_f_string(cio), table_file_id)

  end subroutine openbf_c


  subroutine closbf_c(bufr_unit) bind(C, name='closbf_f') 
    integer(c_int), value, intent(in) :: bufr_unit

    call closbf(bufr_unit)

  end subroutine closbf_c


  function ireadmg_c(bufr_unit, subset, iddate) result(ires) bind(C, name='ireadmg_f')
    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(out) :: subset
    integer(c_int), intent(out) :: iddate
    
    integer(c_int) :: ires
    integer :: subset_len
    integer :: ireadmg

    character(len=25), pointer :: subset_ptr
    character(len=25), target :: f_subset
    ires = ireadmg(bufr_unit, f_subset, iddate)

    ! !convert fortran string to c string
    ! subset_ptr => f_subset
    ! subset_len = len_trim(subset_ptr)
    ! subset_ptr(subset_len+1:subset_len+1) = c_null_char
    ! subset = c_loc(subset_ptr)
    
  end function ireadmg_c


  function ireadsb_c(bufr_unit) result(ires) bind(C, name='ireadsb_f')
    integer(c_int), value, intent(in) :: bufr_unit

    integer(c_int) :: ires
    integer :: ireadsb
    
    ires = ireadsb(bufr_unit)

  end function ireadsb_c


  subroutine ufbint_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbint_f')
    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(inout) ::  c_data
    integer(c_int), value, intent(in) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic

    real, pointer :: f_data
    call c_f_pointer(c_data, f_data)
    
    call ufbint(bufr_unit, f_data, dim_1, dim_2, iret, c_to_f_string(table_b_mnemonic))

  end subroutine ufbint_c


  subroutine ufbrep_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrep_f')
    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(inout) :: c_data
    integer(c_int), value, intent(in) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic

    real, pointer :: f_data
    call c_f_pointer(c_data, f_data)

    call ufbrep(bufr_unit, f_data, dim_1, dim_2, iret, c_to_f_string(table_b_mnemonic))

  end subroutine ufbrep_c


  subroutine dxdump_c(bufr_unit, table_unit) bind(C, name='dxdump_f')
    integer(c_int), value, intent(in) :: bufr_unit
    integer(c_int), value, intent(in) :: table_unit

    call dxdump(bufr_unit, table_unit)
    
  end subroutine dxdump_c

end module bufr_c_interface_mod