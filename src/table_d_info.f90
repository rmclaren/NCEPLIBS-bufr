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
!>   This subroutine is based on dxdump (written by J. Ator).
!>
!> @param[in] lunit    - integer: Fortran logical unit number for fortran
!>                       file
!> @param[out] tbldat  - character*(*): annotated table d data
!>

subroutine table_d_info(lunit, tbldat)
  use moda_tababd
  use moda_nmikrp
  implicit none

  common /reptab/ typs(5,2), reps(5,2)

  integer, intent(in) :: lunit
  character(len=1000), dimension(100), intent(out) :: tbldat

  character(len=6) :: adn
  character(len=20) :: cmstr
  character(len=8) :: wrk1, wrk2
  character(len=3) :: typs
  character(len=1) :: reps
  integer :: il, im, n, na, nc, nch, nseq
  integer :: line_pos, cursor_pos, icms
  integer(kind=8) :: lun
  logical tdskip
  logical isroot


  tdskip(adn) = ((adn == '360001').or.(adn == '360002').or.(adn == '360003').or.(adn == '360004'))
  line_pos = 0

  call status(lunit, lun, il, im)

  do n=1,ntbd(lun)
    if(.not.tdskip(tabd(n,lun)(1:6))) then
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

      PRINT *, tbldat(line_pos)

    end if
  end do
end subroutine table_d_info
