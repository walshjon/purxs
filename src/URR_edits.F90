!>@brief Utilities for editing out data
module URR_edits

  use URR_constants,      only: ISOTROPIC,&
       EQUIPROBABLE_32_BINS,&
       TABULAR
  use URR_error, only: exit_status,&
                       log_message,&
                       INFO
  use URR_openmc_wrapper, only: Material,&
       materials,&
       e_grid,&
       Nuclide,&
       nuclides,&
       to_str,&
       n_materials,&
       Reaction,&
       prn,&
       fatal_error,&
       message
  use URR_isotope,        only: Isotope,&
                                isotopes
  use URR_resonance,      only: wigner_level_spacing

  implicit none
  private

contains


!> Write abscissa-ordinate pairs to a text file
  subroutine write_coords(unit_num, filename, x, y)

    integer :: unit_num ! unit number for output file
    integer :: i        ! coordinate pair index
    character(*) :: filename ! name of output file
    real(8) :: x(:) ! vector of abscissae
    real(8) :: y(:) ! vector of ordinates

    open(unit = unit_num, file = trim(adjustl(filename)))

    if (size(x) == size(y)) then
      do i = 1, size(x)
        write(unit_num, '(ES24.16, ES24.16)') x(i), y(i)
      end do
    else
      message = 'Mismatched vector lengths in write_coords'
      call fatal_error()
    end if

    close(unit_num)

  end subroutine write_coords


!> Generate realizations of level spacings by sampling the Wigner's Surmise
!! distribution
  subroutine wigner_dist_samples(n)

    integer :: n ! number of samples
    integer :: i ! iteration index
    real(8) :: D_mean    ! average level spacing in eV
    real(8) :: D_vals(n) ! sampled level spacings

    D_mean = 20.0_8

    do i = 1, n
      D_vals(i) = wigner_level_spacing(D_mean, prn())
    end do

    call write_coords(99, 'wigner-dist-samples.dat',&
         dble([(i, i = 1, n)]), D_vals(:) / D_mean)

  end subroutine wigner_dist_samples


end module URR_edits
