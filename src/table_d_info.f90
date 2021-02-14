!> @file
!> @authors Ronald Mclaren
!> @date 2021-02-13

!> @brief This subroutine is an interface which allows a client application
!>   to get the annotated table d information for a bufr file in the
!>   following comma seperated field format:
!>
!>   Type, Mnemonic, Sequence
!>
!>   Where the type is either R (for root) or B (for branch) and the
!>   sequence information contains the replication information.
!>
!>   Based on dxdump (written by J. Ator).
!>
!> @param[in] lunit    - integer: Fortran logical unit number for fortran
!>                       file
!> @param[out] tbldat  - character:(:): annotated table d data
!>

module table_d_info
  use moda_tababd
  use moda_nmikrp
  implicit none

  common /reptab/ idnr(5,2), typs(5,2), reps(5,2), lens(5)

  integer :: idnr
  integer :: lens
  character(len=3) :: typs
  character(len=1) :: reps

  private
  public::get_ann_tbl_d

contains

  subroutine get_ann_tbl_d(lunit, tbldat)

    integer, intent(in) :: lunit
    character(len=:), allocatable, intent(out) :: tbldat(:)
    character(len=20) :: cmstr
    character(len=8) :: wrk1, wrk2

    integer :: num_rows, num_cols, num_line_cols;
    integer :: il, im, n, na, nc, nch, nseq
    integer :: line_pos, cursor_pos, icms
    integer :: lun
    logical :: isroot


    call size_of_tbl_d(lunit, num_rows, num_cols)
    allocate(character(len=num_cols) :: tbldat(num_rows))
    tbldat = ' '

    ! Read the data into the data structure

    call status(lunit, lun, il, im)

    line_pos = 0
    do n=1,ntbd(lun)
      if(.not.skip_tbld_section(tabd(n,lun)(1:6))) then
        line_pos = line_pos+1
        cursor_pos = 1

  !     Check if sequence name is a table a mnemonic and add the node
  !     type field ("R" for root and "B" for branch)

        isroot = .false.
        do na=1,ntba(lun)
          if(taba(na,lun)(4:11)==tabd(n, lun)(7:14)) then
            isroot = .true.
          end if
        end do

        if (isroot.eqv..true.) then
          tbldat(line_pos)(cursor_pos:cursor_pos + 3) = 'R, '
        else
          tbldat(line_pos)(cursor_pos:cursor_pos + 3) = 'B, '
        end if

        cursor_pos = cursor_pos + 3

  !     Add the sequence name

        call strsuc(tabd(n,lun)( 7:14), wrk2,nch)
        tbldat(line_pos)(cursor_pos:nch + cursor_pos) = wrk2
        cursor_pos = cursor_pos+nch

        tbldat(line_pos)(cursor_pos:cursor_pos+2) = ', '
        cursor_pos = cursor_pos + 2

        call nemtbd(lun, n, nseq,nem(1,1), irp(1,1), krp(1,1))
        if(nseq > 0) then
          do nc=1,nseq
            cmstr=' '
            icms=0

            call strsuc(nem(nc,1),wrk2,nch)
            if(irp(nc,1) /= 0) then

  !           Add the opening replication tag.

              icms=icms+1
              cmstr(icms:icms)=reps(irp(nc,1),1)
            end if

  !         Add mnemonic

            cmstr(icms+1:icms+nch)=wrk2(1:nch)
            icms=icms+nch

            if(irp(nc,1) /= 0) then

  !           Add the closing replication tag.

              icms=icms+1
              cmstr(icms:icms)=reps(irp(nc,1),2)
            end if

            if(krp(nc,1) /= 0) then

  !           Add the fixed replication count.

              wrk1=' '
              call strsuc(wrk1,wrk2,nch)
              cmstr(icms+1:icms+nch)=wrk2(1:nch)
              icms=icms+nch+2
            end if

            tbldat(line_pos)(cursor_pos:cursor_pos + icms)=cmstr(1:icms)
            cursor_pos = cursor_pos + icms + 1

          end do
        end if
      end if
    end do
  end subroutine get_ann_tbl_d

  subroutine size_of_tbl_d(lunit, num_rows, num_cols)
    integer, intent(in) :: lunit
    integer, intent(out) :: num_rows, num_cols

    integer num_line_cols
    integer :: il, im, n, na, nc, nch, nseq
    integer :: lun
    character(len=20) :: cmstr
    character(len=8) :: wrk1, wrk2

    call status(lunit, lun, il, im)

    num_cols = 0
    num_rows = 0
    do n=1,ntbd(lun)
      num_line_cols = 3

      if(.not.skip_tbld_section(tabd(n,lun)(1:6))) then
        num_rows = num_rows + 1

        call strsuc(tabd(n,lun)( 7:14), wrk2,nch)
        num_line_cols = num_line_cols + nch + 2

        call nemtbd(lun, n, nseq,nem(1,1), irp(1,1), krp(1,1))
        if(nseq > 0) then
          do nc=1,nseq
            call strsuc(nem(nc,1),wrk2,nch)
            if(irp(nc,1) /= 0) then ! opening rep tag
              num_line_cols = num_line_cols + 1
            end if

            num_line_cols = num_line_cols + nch ! the mnemonic

            if(irp(nc,1) /= 0) then ! closing rep
              num_line_cols = num_line_cols + 1
            end if

            if(krp(nc,1) /= 0) then ! the rep count
              wrk1=' '
              call strsuc(wrk1,wrk2,nch)
              num_line_cols = num_line_cols + nch
            end if

            num_line_cols = num_line_cols + 1 ! count space

          end do
        end if
      end if

      if (num_line_cols > num_cols) then
        num_cols = num_line_cols
      end if
    end do
  end subroutine

  logical function skip_tbld_section(str) result(skip)
    character(len=6), intent(in) :: str

    skip = ((str == '360001').or.(str == '360002').or.(str == '360003').or.(str == '360004'))

  end function skip_tbld_section

end module table_d_info
