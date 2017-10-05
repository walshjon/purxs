!>@brief Psi-Chi Doppler integral functions
module URR_psi_chi

  use URR_constants, only:&
       ZERO,&
       HALF,&
       ONE,&
       SQRT_PI,&
       MIT_W,&
       QUICK_W
  use URR_error, only:&
       EXIT_FAILURE,&
       exit_status
  use URR_faddeeva, only:&
       faddeeva_w,&
       quickw
  use URR_settings, only:&
       faddeeva_method

  implicit none
  private
  public ::&
       psi,&
       chi

contains


!> Compute Doppler integral function, psi
  function psi(T, theta, x) result(psi_val)

    real(8), intent(in) :: T     ! temperature [K]
    real(8), intent(in) :: theta ! psi argument
    real(8), intent(in) :: x     ! psi argument

    real(8) :: psi_val ! calculated value of psi
    real(8) :: relerr  ! relative error of the Faddeeva evaluation
    complex(8) :: w_val ! complex return value of the Faddeeva evaluation

    if (T > ZERO) then
      select case (faddeeva_method)
      case (MIT_W)
        ! S.G. Johnson's Faddeeva evaluation
        relerr = 1.0e-6
        w_val = faddeeva_w(cmplx(theta * x * HALF, theta * HALF, 8), relerr)
        psi_val = SQRT_PI * HALF * theta&
             * real(real(w_val, 8), 8)

      case (QUICK_W)
        ! QUICKW Faddeeva evaluation from Argonne (also used in NJOY)
        psi_val = SQRT_PI * HALF * theta&
             * real(real(quickw(cmplx(theta * x * HALF, theta * HALF, 8)),8),8)

      case default
        call exit_status(EXIT_FAILURE, 'Unrecognized W function evaluation method')
        return

      end select

    else
      psi_val = ONE / (ONE + x*x)

    end if

  end function psi


!> Compute Doppler integral function, chi
  function chi(T, theta, x) result(chi_val)

    real(8), intent(in) :: T     ! temperature [K]
    real(8), intent(in) :: theta ! psi argument
    real(8), intent(in) :: x     ! psi argument

    real(8) :: chi_val ! calculated value of chi
    real(8) :: relerr  ! relative error of the Faddeeva evaluation
    complex(8) :: w_val ! complex return value of the Faddeeva evaluation

    if (T > ZERO) then
      ! evaluate the W (Faddeeva) function
      select case (faddeeva_method)

      case (MIT_W)
        ! S.G. Johnson's Faddeeva evaluation
        relerr = 1.0e-6
        w_val = faddeeva_w(cmplx(theta * x * HALF, theta * HALF, 8), relerr)
        chi_val = SQRT_PI * HALF * theta&
             * real(aimag(w_val), 8)

      case (QUICK_W)
        ! QUICKW Faddeeva evaluation from Argonne (also used in NJOY)
        chi_val = SQRT_PI * HALF * theta&
             * real(aimag(quickw(cmplx(theta * x * HALF, theta * HALF, 8))), 8)

      case default
        call exit_status(EXIT_FAILURE, 'Unrecognized W function evaluation method')
        return

      end select

    else
      chi_val = x / (ONE + x*x)

    end if

  end function chi


end module URR_psi_chi
