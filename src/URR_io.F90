!>@brief Functions for reading/writing PURXS data
module URR_io

  use URR_constants, only:&
       ZERO,&
       ONE,&
       SLBW,&
       MLBW,&
       REICH_MOORE
  use URR_error,     only: exit_status,&
                           log_message,&
                           EXIT_FAILURE,&
                           INFO
  use URR_isotope,   only: isotopes
  use URR_resonance, only: BreitWignerResonance
  use URR_settings,  only:&
       endf_filenames,&
       formalism,&
       path_avg_xs,&
       path_endf_files,&
       path_prob_tables

  implicit none
  private
  public :: read_avg_xs,&
            read_prob_tables,&
            write_MF2

contains


!> Reads in pre-computed average URR cross sections needed for LSSF=1 treatment
  subroutine read_avg_xs(i_iso)

    integer, intent(in) :: i_iso ! isotope index

    logical :: file_exists ! does avg URR xs file exist?
    character(7)   :: readable ! is avg URR xs file readable?
    character(7)   :: zaid_str ! ZA number as a string
    character(255) :: rec      ! file record
    integer :: E_depend      ! parameter energy dependence flag
    integer :: avg_unit = 12 ! input unit
    integer :: ir            ! record index
    integer :: ZAI           ! isotope ZA number
    real(8) :: tol ! tolerance used in generating avg xs values

    if (isotopes(i_iso) % metastable) then
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
      zaid_str = trim(adjustl(zaid_str)) // 'm'
    else
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
    end if

    ! check that file exists and is readable
    inquire(file = trim(path_avg_xs)//trim(adjustl(zaid_str))//&
         &'-avg-urr-xs.dat', exist = file_exists, read = readable)
    if (.not. file_exists) then
      call exit_status(EXIT_FAILURE, 'Averaged URR cross sections file '&
           //trim(adjustl(zaid_str))//'-avg-urr-xs.dat does not exist.')
    else if (readable(1:3) == 'NO') then
      call exit_status(EXIT_FAILURE, 'Averaged URR cross sections file '&
           //trim(adjustl(zaid_str))//'-avg-urr-xs.dat&
           & is not readable.  Change file permissions with chmod command.')
    end if

    ! open file with average xs values
    call log_message(INFO, 'Loading average URR cross sections for ZA '//zaid_str)
    open(unit=avg_unit,&
         file=trim(path_avg_xs)//trim(adjustl(zaid_str))//'-avg-urr-xs.dat')

10  format(A255)
    ! ENDF-6 filepath
    read(avg_unit, 10) rec

    ! resonance formalism
    read(avg_unit, 10) rec

    ! number of contributing s-wave resonances
    read(avg_unit, 10) rec

    ! number of contributing p-wave resonances
    read(avg_unit, 10) rec

    ! number of contributing d-wave resonances
    read(avg_unit, 10) rec

    ! number of contributing f-wave resonances
    read(avg_unit, 10) rec

    ! model competitive resonance structure
    read(avg_unit, 10) rec

    ! parameter energy dependence
    read(avg_unit, 10) rec

    ! Faddeeva function evaluation
    read(avg_unit, 10) rec

    ! target tolerance (1sigma/mean) for averaged partial cross sections
    read(avg_unit, 10) rec

    ! number of energies
    read(avg_unit, 10) rec

    read(rec(10:80), '(i71)') isotopes(i_iso) % nE_tabs

    isotopes(i_iso) % num_avg_xs_grid = isotopes(i_iso) % nE_tabs

    ! column labels
    read(avg_unit, 10) rec

    ! allocate average xs grids
    allocate(isotopes(i_iso) % E_avg_xs(isotopes(i_iso) % num_avg_xs_grid))
    allocate(isotopes(i_iso) % avg_xs(isotopes(i_iso) % num_avg_xs_grid))

20  format(6ES24.16)
    ! read in average xs values

    do ir = 1, isotopes(i_iso) % num_avg_xs_grid
      read(avg_unit, 20)&
           isotopes(i_iso) % E_avg_xs(ir),&
           isotopes(i_iso) % avg_xs(ir) % t,&
           isotopes(i_iso) % avg_xs(ir) % n,&
           isotopes(i_iso) % avg_xs(ir) % g,&
           isotopes(i_iso) % avg_xs(ir) % f,&
           isotopes(i_iso) % avg_xs(ir) % x
    end do

    ! read max 1sigma/mean values
    read(avg_unit, *) rec

    close(avg_unit)

  end subroutine read_avg_xs


!> Reads in pre-generated URR probability tables
  subroutine read_prob_tables(i_iso)

    integer, intent(in) :: i_iso ! isotope index

    logical :: file_exists ! does probability table file exist?
    integer :: i_E           ! energy index
    integer :: i_T           ! temperature index
    integer :: i_b           ! probability band index
    integer :: tab_unit = 99 ! tables output file unit
    character(7) :: readable ! is probability table file readable?
    character(7) :: zaid_str ! ZA number as a string
    character(255) :: rec ! file record

    if (isotopes(i_iso) % metastable) then
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
      zaid_str = trim(adjustl(zaid_str)) // 'm'
    else
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
    end if

    ! check that file exists and is readable
    inquire(file = trim(path_prob_tables)//trim(adjustl(zaid_str))//&
         &'-prob-tables.dat', exist = file_exists, read = readable)
    if (.not. file_exists) then
      call exit_status(EXIT_FAILURE, 'Probability table file '//trim(adjustl(zaid_str))//&
           &'-prob-tables.dat does not exist.')
    else if (readable(1:3) == 'NO') then
      call exit_status(EXIT_FAILURE, 'Probability table file '//trim(adjustl(zaid_str))//&
           &'-prob-tables.dat is not readable.  Change file permissions with &
           &chmod command.')
    end if

    ! open probability table file
    call log_message(INFO, 'Loading probability tables for ZA '//zaid_str)
    open(unit = tab_unit, file =&
         trim(path_prob_tables)//trim(adjustl(zaid_str))//'-prob-tables.dat')

10  format(A255)
    ! ENDF-6 filepath
    read(tab_unit, 10) rec

    ! resonance formalism
    read(tab_unit, 10) rec

    ! number of contributing s-wave resonances
    read(tab_unit, 10) rec

    ! number of contributing p-wave resonances
    read(tab_unit, 10) rec

    ! number of contributing d-wave resonances
    read(tab_unit, 10) rec

    ! number of contributing f-wave resonances
    read(tab_unit, 10) rec

    ! model competitive resonance structure
    read(tab_unit, 10) rec

    ! parameter energy dependence
    read(tab_unit, 10) rec

    ! Faddeeva function evaluation
    read(tab_unit, 10) rec

    ! target tolerance (1sigma/mean) for averaged partial cross sections
    read(tab_unit, 10) rec

    ! number of energies
    read(tab_unit, 10) rec
    read(rec(10:), '(i246)') isotopes(i_iso) % nE_tabs

    ! number of temperatures
    read(tab_unit, 10) rec
    read(rec(14:), '(i242)') isotopes(i_iso) % nT_tabs

    ! number of probability-xs bands
    read(tab_unit, 10) rec
    read(rec(7:), '(i249)') isotopes(i_iso) % n_bands

    ! allocate probability tables
    allocate(isotopes(i_iso) % E_tabs(isotopes(i_iso) % nE_tabs))
    allocate(isotopes(i_iso) % T_tabs(isotopes(i_iso) % nT_tabs))
    allocate(isotopes(i_iso) % prob_tables(isotopes(i_iso) % nE_tabs, isotopes(i_iso) % nT_tabs))
    do i_E = 1, isotopes(i_iso) % nE_tabs
      do i_T = 1, isotopes(i_iso) % nT_tabs
        allocate(isotopes(i_iso) % prob_tables(i_E, i_T) % t(isotopes(i_iso) % n_bands))
        allocate(isotopes(i_iso) % prob_tables(i_E, i_T) % n(isotopes(i_iso) % n_bands))
        allocate(isotopes(i_iso) % prob_tables(i_E, i_T) % g(isotopes(i_iso) % n_bands))
        allocate(isotopes(i_iso) % prob_tables(i_E, i_T) % f(isotopes(i_iso) % n_bands))
        allocate(isotopes(i_iso) % prob_tables(i_E, i_T) % x(isotopes(i_iso) % n_bands))
      end do
    end do

    ! read probability tables
    do i_E = 1, isotopes(i_iso) % nE_tabs
      do i_T = 1, isotopes(i_iso) % nT_tabs
        ! read energy        
        read(tab_unit, 10) rec
        read(rec(25:48), '(es24.16)') isotopes(i_iso) % E_tabs(i_E)

        ! read temperature
        read(tab_unit, 10) rec
        read(rec(25:48), '(es24.16)') isotopes(i_iso) % T_tabs(i_T)

        ! read column labels
        read(tab_unit, 10) rec

        ! read cross sections
        do i_b = 1, isotopes(i_iso) % n_bands
          read(tab_unit, 10) rec
          read(rec(49:192), '(6es24.16)')&
               isotopes(i_iso) % prob_tables(i_E, i_T) % t(i_b) % cnt_mean,&
               isotopes(i_iso) % prob_tables(i_E, i_T) % t(i_b) % xs_mean,&
               isotopes(i_iso) % prob_tables(i_E, i_T) % n(i_b) % xs_mean,&
               isotopes(i_iso) % prob_tables(i_E, i_T) % g(i_b) % xs_mean,&
               isotopes(i_iso) % prob_tables(i_E, i_T) % f(i_b) % xs_mean,&
               isotopes(i_iso) % prob_tables(i_E, i_T) % x(i_b) % xs_mean
        end do

        ! read number of batches
        read(tab_unit, 10) rec

        ! read mean cross sections
        read(tab_unit, 10) rec

        ! read 1sigma values
        read(tab_unit, 10) rec

        ! read 1sigma/mean values
        read(tab_unit, 10) rec

      end do
    end do

    close(tab_unit)

  end subroutine read_prob_tables
  

!> Writes out resonance parameters and other data for a realization of
!! unresolved resonances
  subroutine write_MF2(i_iso)

    integer, intent(in) :: i_iso ! isotope index

    integer :: res_unit = 12 ! resonance realization file unit
    integer :: sum_unit = 13 ! resonance realization summary file unit
    integer :: MAT           ! isotope MAT number
    integer :: MF            ! File number
    integer :: MT            ! Section number
    integer :: NS            ! sequence (line) number
    integer :: i_l           ! l loop index
    integer :: i_J           ! J loop index
    integer :: i_res_l       ! resonance index for a given l, any J
    integer :: i_res_lJ      ! resonance index for a given l, J
    integer, allocatable :: NRS(:) ! num resonances for each l-wave
    integer :: num_sorted_resonances ! number of l-wave resonances that have been sorted by increasing energyg
    type(BreitWignerResonance), allocatable :: l_wave_bw_resonances(:) ! all Breit-Wigner resonances for a given l, sorted by energy
    type(BreitWignerResonance), allocatable :: resonance_parameter_sums(:) ! for each l-wave
    character(7)  :: zaid_str ! ZA number as a string
    character(80) :: rec      ! ENDF-6 file record

    if (isotopes(i_iso) % metastable) then
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
      zaid_str = trim(adjustl(zaid_str)) // 'm'
    else
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
    end if

    open(unit = res_unit, file = trim(adjustl(zaid_str))//'-urr-realization.dat')

10  format(A80)
20  format(i4,i2,i3,i5)

    MAT = isotopes(i_iso) % MAT
    MF  = 2
    MT  = 151
    NS  = 1

    ! HEAD
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(dble(isotopes(i_iso) % ZAI)) ! actually ZA of nuclide (ZA in ENDF-6 format specs)
    write(rec(12:22), '(A11)') endf6_floating_point(isotopes(i_iso) % AWR)
    write(rec(23:33), '(I11)') 0
    write(rec(34:44), '(I11)') 0
    write(rec(45:55), '(I11)') 1 ! NIS = 1 --> 1 isotope present in this ENDF-6 file
    write(rec(56:66), '(I11)') 0
    write(rec(67:80), 20) MAT, MF, MT, NS
    NS = NS + 1
    write(res_unit, 10) rec

    ! CONT
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(dble(isotopes(i_iso) % ZAI)) ! ZA of isotope
    write(rec(12:22), '(A11)') endf6_floating_point(ONE)
    write(rec(23:33), '(I11)')   0
    write(rec(34:44), '(I11)')   0 ! LFW = 0 since URR is being replaced with a realization
    write(rec(45:55), '(I11)')   isotopes(i_iso) % NER ! same NER, URR is just being replaced by a realization
    write(rec(56:66), '(I11)')   0
    write(rec(67:80), 20) MAT, MF, MT, NS
    NS = NS + 1
    write(res_unit, 10) rec

    ! CONT
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(isotopes(i_iso) % EL(isotopes(i_iso) % i_urr))
    write(rec(12:22), '(A11)') endf6_floating_point(isotopes(i_iso) % EH(isotopes(i_iso) % i_urr))
    write(rec(23:33), '(I11)') 1 ! isotopes(i_iso) % LRU(isotopes(i_iso) % i_urr): 2 --> 1 because the URR parameters are converted to a resolved realization
    write(rec(34:44), '(I11)') formalism ! isotopes(i_iso) % LRF(isotopes(i_iso) % i_urr): new meaning of LRF in the RRR --> indicates resonance formalism
    write(rec(45:55), '(I11)') isotopes(i_iso) % NRO(isotopes(i_iso) % i_urr)
    write(rec(56:66), '(I11)') isotopes(i_iso) % NAPS(isotopes(i_iso) % i_urr)
    write(rec(67:80), 20) MAT, MF, MT, NS
    NS = NS + 1
    write(res_unit, 10) rec

!TODO: handle NRO / =1
    ! CONT (handles SLBW, LRF = 1; MLBW, LRF = 2; and RM, LRF = 3)
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(isotopes(i_iso) % SPI(isotopes(i_iso) % i_urr))
    write(rec(12:22), '(A11)') endf6_floating_point(isotopes(i_iso) % AP(isotopes(i_iso) % i_urr))
    write(rec(23:33), '(I11)')   0 ! not used/0 in SLBW/MLBW, LAD in RM w/ 0 --> don't use parameters to compute ang. dist.
    write(rec(34:44), '(I11)')   0
    write(rec(45:55), '(I11)')   isotopes(i_iso) % NLS(isotopes(i_iso) % i_urr)
    write(rec(56:66), '(I11)')   20 ! not used/0 in SLBW/MLBW, NLSC in RM --> # l values needed to converge elastic ang. dist.
    write(rec(67:80), 20) MAT, MF, MT, NS
    NS = NS + 1
    write(res_unit, 10) rec

    allocate(NRS(isotopes(i_iso) % NLS(isotopes(i_iso) % i_urr)))
    allocate(resonance_parameter_sums(isotopes(i_iso) % NLS(isotopes(i_iso) % i_urr)))
    do i_l = 1, isotopes(i_iso) % NLS(isotopes(i_iso) % i_urr)
      ! LIST
      write(rec, 10) ''
      write(rec(1:11),  '(A11)') endf6_floating_point(isotopes(i_iso) % AWR) ! actually atomic weight ratio of isotope (AWRI)
      if (formalism == SLBW .or. formalism == MLBW) write(rec(12:22), '(A11)') endf6_floating_point(ZERO) ! SLBW/MLBW: QX = 0.0 because no competitive width is given
      if (formalism == REICH_MOORE) write(rec(12:22), '(A11)') endf6_floating_point(isotopes(i_iso) % AP(isotopes(i_iso) % i_urr)) ! RM: l-dependent scat radius, if zero, use APL = AP
      write(rec(23:33),   '(I11)') i_l - 1
      write(rec(34:44),   '(I11)') 0 ! SLBW/MLBW: LRX = 0 --> no competitive width given, RM: not used/0
      NRS(i_l) = 0
      do i_J = 1, isotopes(i_iso) % NJS(i_l)
        NRS(i_l) = NRS(i_l) + size(isotopes(i_iso) % urr_resonances(i_l, 1) % J(i_J) % res(:))
      end do
      write(rec(45:55),   '(I11)') 6 * NRS(i_l)
      write(rec(56:66),   '(I11)') NRS(i_l)
      write(rec(67:80), 20) MAT, MF, MT, NS
      NS = NS + 1
      write(res_unit, 10) rec
      allocate(l_wave_bw_resonances(NRS(i_l)))
      num_sorted_resonances = size(isotopes(i_iso) % urr_resonances(i_l, 1) % J(1) % res(:))
      l_wave_bw_resonances(1:num_sorted_resonances) =&
           isotopes(i_iso) % urr_resonances(i_l, 1) % J(1) % res(:)
      do i_J = 2, isotopes(i_iso) % NJS(i_l)
        i_res_l = 1
        do i_res_lJ = 1, size(isotopes(i_iso) % urr_resonances(i_l, 1) % J(i_J) % res(:))
          do
            if (isotopes(i_iso) % urr_resonances(i_l, 1) % J(i_J) % res(i_res_lJ) % E_lam&
                 < l_wave_bw_resonances(i_res_l) % E_lam) exit
            i_res_l = i_res_l + 1
            if (i_res_l > num_sorted_resonances) exit
          end do
          if (i_res_l == NRS(i_l)) then
            l_wave_bw_resonances(i_res_l) = isotopes(i_iso) % urr_resonances(i_l, 1) % J(i_J) % res(i_res_lJ)
          else
            l_wave_bw_resonances(i_res_l+1:NRS(i_l)) = l_wave_bw_resonances(i_res_l:NRS(i_l)-1)
            l_wave_bw_resonances(i_res_l) = isotopes(i_iso) % urr_resonances(i_l, 1) % J(i_J) % res(i_res_lJ)
          end if
          num_sorted_resonances = num_sorted_resonances + 1
        end do
      end do
      resonance_parameter_sums(i_l) % E_lam = ZERO
      resonance_parameter_sums(i_l) % AJ = ZERO
      resonance_parameter_sums(i_l) % GT = ZERO
      resonance_parameter_sums(i_l) % GN = ZERO
      resonance_parameter_sums(i_l) % GG = ZERO
      resonance_parameter_sums(i_l) % GF = ZERO
      do i_res_l = 1, NRS(i_l)
        write(rec, 10) ''
        if (formalism == SLBW .or. formalism == MLBW) then
          write(rec(1:66), '(6A11)')&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % E_lam),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % AJ),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GT),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GN),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GG),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GF)
        else if (formalism == REICH_MOORE) then
!TODO: convert SLBW/MLBW fission widths to Reich-Moore, setting both to ZERO reduces RM to an R-function
          write(rec(1:66), '(6A11)')&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % E_lam),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % AJ),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GN),&
               endf6_floating_point(l_wave_bw_resonances(i_res_l) % GG),&
               endf6_floating_point(ZERO),& ! first partial fission width, GFA
               endf6_floating_point(ZERO)  ! second partial fission width, GFB
        end if
        write(rec(67:80), 20) MAT, MF, MT, NS
        NS = NS + 1
        write(res_unit, 10) rec
        if (i_res_l > 1) resonance_parameter_sums(i_l) % E_lam = resonance_parameter_sums(i_l) % E_lam&
             + (l_wave_bw_resonances(i_res_l) % E_lam - l_wave_bw_resonances(i_res_l-1) % E_lam)
        resonance_parameter_sums(i_l) % AJ = resonance_parameter_sums(i_l) % AJ + l_wave_bw_resonances(i_res_l) % AJ
        resonance_parameter_sums(i_l) % GT = resonance_parameter_sums(i_l) % GT + l_wave_bw_resonances(i_res_l) % GT
        resonance_parameter_sums(i_l) % GN = resonance_parameter_sums(i_l) % GN + l_wave_bw_resonances(i_res_l) % GN
        resonance_parameter_sums(i_l) % GG = resonance_parameter_sums(i_l) % GG + l_wave_bw_resonances(i_res_l) % GG
        resonance_parameter_sums(i_l) % GF = resonance_parameter_sums(i_l) % GF + l_wave_bw_resonances(i_res_l) % GF
      end do
      deallocate(l_wave_bw_resonances)
    end do

    ! SEND
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(ZERO)
    write(rec(12:22), '(A11)') endf6_floating_point(ZERO)
    write(rec(23:33), '(I11)')   0
    write(rec(34:44), '(I11)')   0
    write(rec(45:55), '(I11)')   0
    write(rec(56:66), '(I11)')   0
    MT = 0
    NS = 99999
    write(rec(67:80), 20) MAT, MF, MT, NS
    write(res_unit, 10) rec

    ! FEND
    write(rec, 10) ''
    write(rec(1:11),  '(A11)') endf6_floating_point(ZERO)
    write(rec(12:22), '(A11)') endf6_floating_point(ZERO)
    write(rec(23:33), '(I11)')   0
    write(rec(34:44), '(I11)')   0
    write(rec(45:55), '(I11)')   0
    write(rec(56:66), '(I11)')   0
    MF = 0
    MT = 0
    NS = 0
    write(rec(67:80), 20) MAT, MF, MT, NS
    write(res_unit, 10) rec

    close(res_unit)

    ! write summary of realization
    open(unit = sum_unit, file = trim(adjustl(zaid_str))//'-urr-realization-summary.txt')
    write(sum_unit, '("Unresolved Resonance Region Realization Summary")')
    write(sum_unit, '("ENDF-6 File:")', advance='no')
    write(sum_unit, *) trim(adjustl(path_endf_files))//trim(adjustl(endf_filenames(i_iso)))
    do i_l = 1, isotopes(i_iso) % NLS(isotopes(i_iso) % i_urr)
      write(sum_unit, '(A2,I1)') 'l=', i_l - 1
      write(sum_unit, '(A5,I11)')      'NRS: ', NRS(i_l)
      write(sum_unit, '(A5,ES11.5E1)') 'D:   ', resonance_parameter_sums(i_l) % E_lam / (NRS(i_l) - 1)
      write(sum_unit, '(A5,ES11.5E1)') 'AJ:  ', resonance_parameter_sums(i_l) % AJ / NRS(i_l)
      write(sum_unit, '(A5,ES11.5E1)') 'GT:  ', resonance_parameter_sums(i_l) % GT / NRS(i_l)
      write(sum_unit, '(A5,ES11.5E1)') 'GN:  ', resonance_parameter_sums(i_l) % GN / NRS(i_l)
      write(sum_unit, '(A5,ES11.5E1)') 'GG:  ', resonance_parameter_sums(i_l) % GG / NRS(i_l)
      write(sum_unit, '(A5,ES11.5E1)') 'GF:  ', resonance_parameter_sums(i_l) % GF / NRS(i_l)
    end do

    deallocate(NRS)
    deallocate(resonance_parameter_sums)

    close(sum_unit)

    call insert_MF2(i_iso)

  end subroutine write_MF2


!> Insert a realization of URR resonances into File 2 of an ENDF-6 file
  subroutine insert_MF2(i_iso)

    integer, intent(in) :: i_iso ! isotope index

    character(7) :: zaid_str ! ZA number as a string
    integer :: mf2_unit = 12 ! inserted URR realization unit
    integer :: old_unit = 13 ! original ENDF-6 file unit
    integer :: new_unit = 14 ! new ENDF-6 file unit
    character(80) :: rec     ! ENDF-6 file record (line)
    character(81) :: new_rec ! modified record
    real(8)       :: field_1 ! field 1/6 of record
    real(8)       :: field_2 ! field 2/6 of record
    character(11) :: field_3 ! field 3/6 of record
    integer       :: field_5 ! field 5/6 of record
    integer       :: field_6 ! field 6/6 of record
    integer :: MAT    ! isotope MAT number
    integer :: MF     ! File number
    integer :: MT     ! Section number
    integer :: NS     ! MT record (line) number
    integer :: new_NS ! MT record (line) number w/ realization inserted
    integer :: NWD    ! number of descriptive text records in MF=1
    integer :: NC     ! number of records in MF=2, MT=151 w/ realization inserted
    integer :: i_rec  ! record count

    if (isotopes(i_iso) % metastable) then
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
      zaid_str = trim(adjustl(zaid_str)) // 'm'
    else
      write(zaid_str, '(I7)') isotopes(i_iso) % ZAI
    end if

    open(unit = mf2_unit, file = trim(adjustl(zaid_str))//'-urr-realization.dat')
    open(unit = old_unit, file = path_endf_files//trim(adjustl(endf_filenames(i_iso))))
    open(unit = new_unit, file = 'purxs-urr-LRU1-'//trim(adjustl(endf_filenames(i_iso))))

10  format(A80)
20  format(i4,i2,i3,i5)

    new_NS = 0
    do
      read(old_unit, 10) rec
      read(rec(67:80), 20) MAT, MF, MT, NS
      if (MF == 2 .and. MT == 151 .and. NS > 2) then
        read(rec(23:33), '(A11)') field_3
        if (field_3 == '          2') then
          read(rec(1:22), '(2E11.0)') field_1, field_2
          read(rec(45:66), '(2I11)') field_5, field_6
          if ((field_1 /= ZERO .and. field_2 /= ZERO)&
               .and. (.not. (field_5 == 6 * field_6 .and. field_6 /= 0))) then
            read(mf2_unit, 10) rec
            read(mf2_unit, 10) rec
            new_NS = NS
            do
              read(mf2_unit, 10) rec
              read(rec(67:80), 20) MAT, MF, MT, NS
              if (NS == 99999) then
                write(new_unit, 10) rec
                read(mf2_unit, 10) rec
                write(new_unit, 10) rec
                NC = new_NS - 1
                new_NS = 0
                do
                  read(old_unit, 10) rec
                  read(rec(67:80), 20) MAT, MF, MT, NS
                  if (NS == 99999) then
                    read(old_unit, 10) rec
                    exit
                  end if
                end do
                exit
              else
                write(new_unit, '(A75,I5)') rec(1:75), new_NS
                new_NS = new_NS + 1
              end if
            end do
          else
            write(new_unit, '(A75,I5)') rec(1:75), max(NS, new_NS)
          end if
        else
          write(new_unit, '(A75,I5)') rec(1:75), max(NS, new_NS)
        end if
      else
        write(new_unit, '(A75,I5)') rec(1:75), max(NS, new_NS)
      end if
      if (MAT == -1) exit
    end do

    close(mf2_unit)
    close(old_unit)
    close(new_unit)

    open(unit = new_unit, file = 'purxs-urr-LRU1-'//trim(adjustl(endf_filenames(i_iso))))
    i_rec = 0
    do
      i_rec = i_rec + 1
      read(new_unit, 10) rec
      read(rec(67:80), 20) MAT, MF, MT, NS
      if (MF == 1 .and. MT == 451 .and. NS == 4) read(rec(45:55), '(I11)') NWD
      if (MF == 1 .and. MT == 451 .and. NS > 4 + NWD) read(rec(23:44), '(2I11)') MF, MT
      if (MF == 2 .and. MT == 151) exit
    end do
    close(new_unit)

    write(new_rec, '(A44,I11,A25,A1)') rec(1:44), NC, rec(56:80), new_line('')
    open(unit = new_unit, file = 'purxs-urr-LRU1-'//trim(adjustl(endf_filenames(i_iso))),&
         access="direct", recl=81, action="write")
    write(unit = new_unit, rec = i_rec) new_rec
    close(new_unit)

  end subroutine insert_MF2


!> Convert a floating-point value to the ENDF-6 11-character field format
  function endf6_floating_point(val) result(field)

    real(8), intent(in) :: val
    character(11) :: field

    character(1) :: val_sign_char
    character(len=:), allocatable :: significand_char
    integer :: len_significand_char
    integer :: expo
    character(len=:), allocatable :: expo_char
    character(1) :: expo_sign_char
    character(255) :: fmt_char

    if (val < ZERO) then
      val_sign_char = '-'
    else
      val_sign_char = ' '
    end if

    if (val == ZERO) then
      expo = 0
    else
      expo = floor(log10(abs(val)))
    end if

    if (expo < 0) then
      expo_sign_char = '-'
    else
      expo_sign_char = '+'
    end if

    if (abs(expo) < 10) then
      allocate(character(1) :: expo_char)
      write(expo_char, '(i1)') abs(expo)
    else if (abs(expo) < 100) then
      allocate(character(2) :: expo_char)
      write(expo_char, '(i2)') abs(expo)
    else
      allocate(character(3) :: expo_char)
      write(expo_char, '(i3)') abs(expo)
    end if

    len_significand_char = 9 - len(expo_char) ! 11 - 2 signs - # exponent chars
    allocate(character(len_significand_char) :: significand_char)
    write(fmt_char, '("(F",I0,".",I0,")")') len_significand_char, len_significand_char - 2
    write(significand_char, fmt_char) abs(val) / 10.0_8**dble(expo)

    field = val_sign_char//significand_char//expo_sign_char//expo_char

    deallocate(significand_char)
    deallocate(expo_char)

  end function endf6_floating_point


end module URR_io
