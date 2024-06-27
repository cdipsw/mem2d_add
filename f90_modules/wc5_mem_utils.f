c-- WC5_MEM_UTILS --------------------------------------------------------------
c   Contains routines for generating and working with 2D spectra in wavecdf
c   datasets. MEM routines ported from bor's original MEM code.
c   The standard wc5_dataset component wave%dirspec holds a 2D spectrum,
c   with the direction dimension defined in wave%dirs.
c-------------------------------------------------------------------------------

      module wc5_mem_utils

      use unit_conversions
      use wavecdf5_data

      implicit none

      save

      contains


c-- WC5M_MEM_WRITE -------------------------------------------------------------
c  Writes a 2D spectrum to the specified unit
c-------------------------------------------------------------------------------
        subroutine wc5m_mem_write(wset, idx, ounit)
          integer             idx, i, j, ounit
          type(wc5_dataset)   wset

          do i = 1, wset%wave%dir_count
            do j = 1, wset%wave%freq_count
              write(ounit,'(f10.7,1x,$)') wset%wave%dirspec(i,j,idx)
            end do
            write(ounit,'(a)') ''
          end do
        end subroutine


c-- WC5M_CALC_HS ----------------------------------------------------------------
c  Calculates the Hs for a single 2D spectrum
c-------------------------------------------------------------------------------
        real function wc5m_calc_hs(wset, idx)
          integer        i, idx, j
          real           etot
          type(wc5_dataset)   wset

          etot = 0.0
          do i = 1, wset%wave%freq_count
            do j = 1, wset%wave%dir_count
              etot = etot + wset%wave%dirspec(j,i,idx) * wset%wave%bw(i) * 360.0 / wset%wave%dir_count
            end do
          end do
          wc5m_calc_hs = 4 * etot**0.5
        end function


c-- WC5M_ADD_2D_CDIP -----------------------------------------------------------
c  Adds standard CDIP-layout 2D spectra (5-degree bins, 5.0 to 360.0) to wset.
c-------------------------------------------------------------------------------
        subroutine wc5m_add_2d_cdip(wset)
          real               dirs(72)
          integer            dir_count, i
          type(wc5_dataset)  wset

          dir_count = 72
          do i = 1, dir_count
            dirs(i) = 0.0 + (i * 5.0)
          end do
          call wc5m_add_2d(wset, dirs, dir_count)
        end subroutine


c-- WC5M_ADD_2D ----------------------------------------------------------------
c  Adds 2D spectra to a wc5_dataset.
c-------------------------------------------------------------------------------
        subroutine wc5m_add_2d(wset, dirs, dir_count)
          real               coeff_max, dirs(*), max_coeff
          integer            dir_count
          type(wc5_dataset)  wset

c-- Intialize 2D structures within the wc5_dataset

          wset%wave%dir_count = dir_count
          allocate(wset%wave%dirs(dir_count), wset%wave%dirspec(dir_count,wset%wave%freq_count,wset%wave%time_count))
          wset%wave%dirs = dirs(1:dir_count)
          wset%wave%dirspec = WC5_real_fill
          wset%is_2d_model = .true.
        
c-- Normalize coefficients for pressure sensor data

          coeff_max = MAX(MAXVAL(wset%wave%a1),MAXVAL(wset%wave%b1),MAXVAL(wset%wave%a2),MAXVAL(wset%wave%b2))
          if (coeff_max .gt. 1.0) call wc5m_normalize_sp(wset)

c-- Calculate and add 2D spectra

          call wc5m_mem_est(wset)
        end subroutine


c-- WC5M_NORMALIZE_SP ----------------------------------------------------------
c  Subroutine that normalizes the coefficients in a wc5_dataset. Direction
c  is set to WC5_real_fill for any band in which the coeffs can't be normalized
c  by energy.
c-------------------------------------------------------------------------------
      subroutine wc5m_normalize_sp(wset)
        integer              i, j
        real                 max_coeff, min_coeff
        type(wc5_dataset)    wset

        do i = 1, wset%wave%freq_count
          do j = 1, wset%wave%time_count
            if (wset%wave%mdir(i,j) .ne. WC5_real_fill) then
              wset%wave%a1(i,j) = wset%wave%a1(i,j) / wset%wave%a0(i,j)
              wset%wave%a2(i,j) = wset%wave%a2(i,j) / wset%wave%a0(i,j)
              wset%wave%b1(i,j) = wset%wave%b1(i,j) / wset%wave%a0(i,j)
              wset%wave%b2(i,j) = wset%wave%b2(i,j) / wset%wave%a0(i,j)
  
              max_coeff = MAX(wset%wave%a1(i,j), wset%wave%a2(i,j), wset%wave%b1(i,j), wset%wave%b2(i,j))
              min_coeff = MIN(wset%wave%a1(i,j), wset%wave%a2(i,j), wset%wave%b1(i,j), wset%wave%b2(i,j))
              if (max_coeff .gt. 1.0 .or. min_coeff .lt. -1.0) wset%wave%mdir(i,j) = WC5_real_fill
            end if
          end do
        end do

      end subroutine



c-- WC5M_UNNORMALIZE_SP --------------------------------------------------------
c  Subroutine that unnormalizes the coefficients in a wset. 
c-------------------------------------------------------------------------------
      subroutine wc5m_unnormalize_sp(wset)
        integer               i, j
        type(wc5_dataset)     wset

        do i = 1, wset%wave%freq_count
          do j = 1, wset%wave%time_count
            if (wset%wave%mdir(i,j) .ne. WC5_real_fill) then
              wset%wave%a1(i,j) = wset%wave%a1(i,j) * wset%wave%a0(i,j)
              wset%wave%a2(i,j) = wset%wave%a2(i,j) * wset%wave%a0(i,j)
              wset%wave%b1(i,j) = wset%wave%b1(i,j) * wset%wave%a0(i,j)
              wset%wave%b2(i,j) = wset%wave%b2(i,j) * wset%wave%a0(i,j)
            end if
          end do
        end do

      end subroutine


c-- WC5M_MAKE_NOISY_SP ---------------------------------------------------------
c  Subroutine that adds random noise to a modeled spectrum to make it look
c  more like actual wave data
c-------------------------------------------------------------------------------
      subroutine wc5m_make_noisy_sp(wset)
        integer                i, j
        real                   rando(4)
        type(wc5_dataset)      wset

          do i = 1, wset%wave%freq_count
            do j = 1, wset%wave%time_count
              call RANDOM_NUMBER(rando)
              wset%wave%a1(i,j) = wset%wave%a1(i,j)*(0.9 + (rando(1)/5))
              wset%wave%b1(i,j) = wset%wave%b1(i,j)*(0.9 + (rando(2)/5))
              wset%wave%a2(i,j) = wset%wave%a2(i,j)*(0.9 + (rando(3)/5))
              wset%wave%b2(i,j) = wset%wave%b2(i,j)*(0.9 + (rando(4)/5))
            end do
          end do

      end subroutine


c-- WC5M_REDISTRIBUTE_SP_01Hz --------------------------------------------------
c  Subroutine that redistributes spectra into even 0.01 Hz width bands.
c  Currently it is set to create 56 bands from 0.03 to 0.58 Hz. The redist
c  work is done by the general WC5M_REDISTIBUTE_SP routine.
c-------------------------------------------------------------------------------
      subroutine wc5m_redistribute_sp_01hz(orig_wset, redist_wset)
        integer               i
        integer, parameter::  rbands = 56
        type(wc5_dataset)     orig_wset, redist_wset

c--   Intialize redist_wset with 0.01 width bins starting at 0.03 Hz

        redist_wset%wave%time_count = orig_wset%wave%time_count
        redist_wset%wave%freq_count = rbands
        call wc5_allocate_set(redist_wset)

        do i = 3, 2 + rbands
          redist_wset%wave%freqs(i-2) = i/100.0
          redist_wset%wave%bw(i-2) = 0.01
        end do

        call wc5m_redistribute_sp(orig_wset, redist_wset)

      end subroutine


c-- WC5M_REDISTRIBUTE_SP_2DATAWELL ---------------------------------------------
c  Subroutine that redistributes a spectrum into the datawell 64-band format.
c  The redist work is done by the general WC5M_REDISTIBUTE_SP routine.
c-------------------------------------------------------------------------------
      subroutine wc5m_redistribute_sp_2datawell(orig_wset, redist_wset)
        integer               i
        integer, parameter::  rbands = 64
        type(wc5_dataset)     orig_wset, redist_wset

c--   Intialize redist_wset with the datawell spectral layout

        redist_wset%wave%time_count = orig_wset%wave%time_count
        redist_wset%wave%freq_count = rbands
        call wc5_allocate_set(redist_wset)

        do i = 1, rbands
          if (i .lt. 16) then
            redist_wset%wave%freqs(i) = 0.020 + i * 0.005
            redist_wset%wave%bw(i) = 0.0050
          else if (i .eq. 16) then
            redist_wset%wave%freqs(i) = 0.10125
            redist_wset%wave%bw(i) = 0.0075
          else
            redist_wset%wave%freqs(i) = 0.10 + (i - 16) * 0.01
            redist_wset%wave%bw(i) = 0.0100
          end if
        end do

        call wc5m_redistribute_sp(orig_wset, redist_wset)

      end subroutine


c-- WC5M_REDISTRIBUTE_SP -------------------------------------------------------
c  Subroutine that redistributes spectra into a new spectral layout.
c  The frequencies and bandwidths must be set in redist_wset before calling.
c
c  NOTE: This code assumes that the frequencies given in the wave%freqs are
c  center frquencies.
c-------------------------------------------------------------------------------
      subroutine wc5m_redistribute_sp(orig_wset, redist_wset)

      integer   b, i, j, rbands, rtimes, tidx
      logical   bin_complete
      real      bot_orig, bot_redist, cos_avg, sin_avg, top_orig, top_redist
      logical,allocatable::   missing_dir(:)
      real, allocatable::     cos_sum(:), curr_energy(:), sin_sum(:)
      type(wc5_dataset)       orig_wset, redist_wset

        rtimes = redist_wset%wave%time_count
        ALLOCATE(cos_sum(rtimes), curr_energy(rtimes), missing_dir(rtimes), sin_sum(rtimes))

        do i = 1, redist_wset%wave%freq_count          !* loop over new bins
          cos_sum = 0
          sin_sum = 0
          missing_dir = .false.
          top_redist = redist_wset%wave%freqs(i) + redist_wset%wave%bw(i)/2
          bot_redist = redist_wset%wave%freqs(i) - redist_wset%wave%bw(i)/2

          do b = 1, orig_wset%wave%freq_count          !* loop over original bands
            top_orig = orig_wset%wave%freqs(b) + orig_wset%wave%bw(b)/2
            bot_orig = orig_wset%wave%freqs(b) - orig_wset%wave%bw(b)/2

c--   If the full band falls into the current bin, add the entire contents

            if (top_orig .le. top_redist .and. bot_orig .ge. bot_redist) then
              do tidx = 1, orig_wset%wave%time_count
                curr_energy(tidx) = orig_wset%wave%a0(b,tidx) * orig_wset%wave%bw(b)
              end do
              call wc5m_band_calcs(curr_energy, orig_wset, redist_wset, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the bottom of the band falls in the bin, add in the appropriate portion

            else if (bot_orig .lt. top_redist .and.
     *               bot_orig .ge. bot_redist) then
              curr_energy = orig_wset%wave%a0(b,:) * (top_redist-bot_orig)
              call wc5m_band_calcs(curr_energy, orig_wset, redist_wset, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the top of the band falls in the bin, add in the appropriate portion

            else if (top_orig .gt. bot_redist .and.
     *               top_orig .le. top_redist) then
              curr_energy = orig_wset%wave%a0(b,:) * (top_orig-bot_redist)
              call wc5m_band_calcs(curr_energy, orig_wset, redist_wset, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the middle of the band falls in the bin, add in the appropriate portion

            else if (top_orig .ge. top_redist .and.
     *               bot_orig .le. bot_redist) then
              curr_energy = orig_wset%wave%a0(b,:) * (top_redist-bot_redist)
              call wc5m_band_calcs(curr_energy, orig_wset, redist_wset, sin_sum, cos_sum, 
     *               missing_dir, i, b)

            end if
          end do

c--   Calculate direction and calc ener_dens once bin is complete

          do j = 1, redist_wset%wave%time_count
            if (redist_wset%wave%a0(i,j) .gt. 0) then
              redist_wset%wave%a0(i,j) = redist_wset%wave%a0(i,j) / redist_wset%wave%bw(i)
              if (.not. missing_dir(j)) then
                sin_avg = sin_sum(j)/redist_wset%wave%a0(i,j)
                cos_avg = cos_sum(j)/redist_wset%wave%a0(i,j)
                redist_wset%wave%mdir(i,j) = to_degrees(ATAN2(sin_avg,cos_avg))
                if (redist_wset%wave%mdir(i,j) .lt. 0) redist_wset%wave%mdir(i,j) = redist_wset%wave%mdir(i,j) + 360.
                redist_wset%wave%a1(i,j) = redist_wset%wave%a1(i,j) / redist_wset%wave%bw(i)
                redist_wset%wave%b1(i,j) = redist_wset%wave%b1(i,j) / redist_wset%wave%bw(i)
                redist_wset%wave%a2(i,j) = redist_wset%wave%a2(i,j) / redist_wset%wave%bw(i)
                redist_wset%wave%b2(i,j) = redist_wset%wave%b2(i,j) / redist_wset%wave%bw(i)
              end if
            end if
          end do
        end do

        DEALLOCATE(cos_sum, curr_energy, sin_sum)

c--   Normalize once energy redistributed

        call wc5m_normalize_sp(redist_wset)

      end subroutine


c-- BAND_CALCS -----------------------------------------------------------------
c  Helper for REDISTRIBUTE_SP; adds components of original spectral layout
c  into the redistributed layout, weighting by energy
c-------------------------------------------------------------------------------
        subroutine wc5m_band_calcs(curr_energy, orig_wset, redist_wset, sin_sum, cos_sum, missing_dir, i, b)
          integer  b, i, tidx         !* b = curr band in original, i = curr in new, tidx = time index
          logical  missing_dir(*)
          real     cos_sum(*), curr_energy(*), sin_sum(*)
          type(wc5_dataset)  orig_wset, redist_wset

          do tidx = 1, redist_wset%wave%time_count
            if (curr_energy(tidx) .gt. 0.0) then
              redist_wset%wave%a0(i,tidx) = redist_wset%wave%a0(i,tidx) + curr_energy(tidx)
              if (orig_wset%wave%mdir(b,tidx) .eq. WC5_real_fill) then
                missing_dir(tidx) = .true.
              else
                redist_wset%wave%a1(i,tidx) = redist_wset%wave%a1(i,tidx) + curr_energy(tidx)*orig_wset%wave%a1(b,tidx)
                redist_wset%wave%b1(i,tidx) = redist_wset%wave%b1(i,tidx) + curr_energy(tidx)*orig_wset%wave%b1(b,tidx)
                redist_wset%wave%a2(i,tidx) = redist_wset%wave%a2(i,tidx) + curr_energy(tidx)*orig_wset%wave%a2(b,tidx)
                redist_wset%wave%b2(i,tidx) = redist_wset%wave%b2(i,tidx) + curr_energy(tidx)*orig_wset%wave%b2(b,tidx)
                sin_sum(tidx) = sin_sum(tidx) + curr_energy(tidx)*sin(to_radians(orig_wset%wave%mdir(b,tidx)))
                cos_sum(tidx) = cos_sum(tidx) + curr_energy(tidx)*cos(to_radians(orig_wset%wave%mdir(b,tidx)))
              end if
            end if
          end do

        end subroutine


c-- WC5M_MEM_EST ---------------------------------------------------------------
c
c  Subroutine to make MEM estimate of the directional spectrum
c  This version reads in normalized four. coeff., so no division by a0
c
c  Input:  Energy and 4 normalized directional fourier coeff.
c          from a buoy or slope array.
c
c  Output: Directional spectrum wset%wave%dirspec in directional bins defined
c            by wset%wave%dirs in the "compass heading" coordinate frame. This
c            is the heading from which the wave energy is arriving, e.g. 270 is
c            from west, 180 is from south etc.
c
c    Default CDIP layout: 72 directional bands. dirs(1) = 5 deg, dirs(72) = 360 deg
c
c  The ds estimate is set to 0 for any band in which direction is not
c  resolved (dir = WC5_real_fill) or where the coeffs could not be normalized.
c-------------------------------------------------------------------------------
      subroutine wc5m_mem_est(wset)
        real               a0_tot, chk, dir, dir_radius, dir_width, d(360)
        integer            dir_radius_int, didx, fidx, i, min_dir, max_dir, tidx
        type(wc5_dataset)  wset

        dir_width = wset%wave%dirs(2) - wset%wave%dirs(1)
        dir_radius = (dir_width - 1.0) / 2.0
        dir_radius_int = FLOOR(dir_radius)

c-- Loop over times, freqs; calculate MEM values with one-degree resolution

        do tidx = 1, wset%wave%time_count
          do fidx = 1, wset%wave%freq_count
            call wc5m_mem_1deg(wset%wave%a1(fidx,tidx), wset%wave%a2(fidx,tidx), wset%wave%b1(fidx,tidx), 
     *             wset%wave%b2(fidx,tidx), d, chk)

c-- Merge into directional bins as defined in wset%wave%mdirs; maintain units of m^2/Hz-deg

            do didx = 1, wset%wave%dir_count
              dir = wset%wave%dirs(didx)
              a0_tot = 0.0

              if (wset%wave%mdir(fidx,tidx) .ne. WC5_real_fill) then
                min_dir = NINT(dir)- dir_radius_int
                max_dir = NINT(dir)+ dir_radius_int
                if (min_dir .lt. 1) then
                  do i = min_dir, 0
                    a0_tot = a0_tot + wset%wave%a0(fidx,tidx) * d(i+360)
                  end do
                  min_dir = 1
                end if
                if (max_dir .gt. 360) then
                  do i = 361, max_dir
                    a0_tot = a0_tot + wset%wave%a0(fidx,tidx) * d(i-360)
                  end do
                  max_dir = 360
                end if

                do i = min_dir, max_dir, 1
                  a0_tot = a0_tot + wset%wave%a0(fidx,tidx) * d(i)
                end do

                if (dir_radius - dir_radius_int .gt. 0.01) then
                  min_dir = NINT(dir) - dir_radius_int - 1
                  if (min_dir .lt. 1) min_dir = min_dir + 360
                  a0_tot = a0_tot + (dir_radius - dir_radius_int) * wset%wave%a0(fidx,tidx) * d(min_dir)
                  max_dir = NINT(dir) + dir_radius_int + 1
                  if (max_dir .gt. 360) max_dir = max_dir - 360
                  a0_tot = a0_tot + (dir_radius - dir_radius_int) * wset%wave%a0(fidx,tidx) * d(max_dir)
                end if
              end if
              wset%wave%dirspec(didx,fidx,tidx) = a0_tot / dir_width              
            end do
c           write(6,*) wset%wave%freqs(fidx), MAXVAL(d), MAXVAL(wset%wave%dirspec(:,fidx,tidx))
          end do
        end do
      end subroutine


c-- WC5M_MEM -------------------------------------------------------------------
c  Maximum entropy method (MEM) for the estimation of a directional
c  distribution from pitch-and-roll data. By default, covers the
c  full 360 degrees with 0.1-degree resolution. (Call MEMD directly for
c  settings other than the default.)
c
c  (Lygre and Krogstad, JPO v16 1986: NOTE - there is a typo in the
c  paper...BOTH exponetials in eq. 13 should have negative signs.
c  This is fixed in Krogstad, 1991- THH has the book)
c
c  variables:
c    a1,b1,b1,b2 = fourier coef. of dir. dist.
c      (normalized ala Long [1980]) - TRIG COORDINATES
c
c    s = MEM estimate of directional spectrum, 1 deg.
c      resolution - COMPASS COORDINATES
c
c    chk = check factor: MEM likes to make narrow spikes for
c      directional spectra if it can (it fits the a1,a2,
c      b1 abd b2's exactly).  If it does this, then the
c      .1 deg stepsize for making the 1 deg resolution
c      estimate is too coarse.  The check factor should
c      be close to 1.
c-------------------------------------------------------------------------------
        subroutine wc5m_mem_1deg(a1,a2,b1,b2,s,chk)
          real    a1, a2, b1, b2, chk, s(360)
          call wc5_memd(a1,a2,b1,b2,1,360,1.0,s,chk)
        end subroutine


c-- WC5M_MEMD ------------------------------------------------------------------
c  Does the calcs for MEM above. Call directly if do not need full 360 degrees
c  or 0.1-degree resolution. Only returns values for integer degrees; res
c  must be <= 1.0, and the start direction, begin, must be > 0.
c
c  Modified 6/2011, added normalization to handle narrow peaks in hindcast data
c    When running the full directional span (1 to 360), normalized by tot
c    instead of 360.
c-------------------------------------------------------------------------------
        subroutine wc5_memd(a1,a2,b1,b2,begin,ndeg,res,s,chk)
          integer begin, dir, i, n, ndeg, ndir
          real    a, a1, a2, b1, b2, chk, d1, d2, d3, d4, s(360), offset, res, rn, tot
          complex c1, c2, p1, p2, e1, e2, x, y

          real,parameter:: dr = 0.0174533

          do i = begin, begin+ndeg-1
            if (i .gt. 360) then
              dir = i - 360
            else
              dir = i
            end if
            s(dir)=0.
          end do

c-- switch to Lygre & Krogstad notation

          d1 = a1
          d2 = b1
          d3 = a2
          d4 = b2

          c1=(1.,0)*d1+(0,1.)*d2
          c2=(1.,0)*d3+(0,1.)*d4

          p1=(c1-c2*conjg(c1))/(1.-MIN(0.99999,cabs(c1)**2))
          p2=c2-c1*p1

          x=1.-p1*conjg(c1)-p2*conjg(c2)

c-- sum over 'ndeg' in steps, get distribution with 'res' degree resolution

          tot=0
          offset = 0.5 * (1.0 - res)

          do n = NINT(1000.0*(begin-offset)), NINT(1000.0*(begin+ndeg-1+offset)), NINT(1000.0*res)
            rn = REAL(n) / 1000.0
            a=rn*dr
            e1=(1.,0)*cos(a)-(0,1.)*sin(a)
            e2=(1.,0)*cos(2*a)-(0,1.)*sin(2*a)
            y=cabs((1.,0)-p1*e1-p2*e2)**2

c-- put in proper 1 deg directional band

            ndir=NINT(rn)
            if (ndir .gt. 360) ndir=ndir-360

c-- switch from trig to compass coordinates

c           ndir=270-ndir
c           if (ndir .gt. 360) ndir=ndir-360
c           if (ndir .lt. 1) ndir=ndir+360

c-- normalize by 360/(step size) if not running full 360 degrees

            if (ndeg .ne. 360) then
              s(ndir)=s(ndir)+cabs(x/y)/(360./res)
            else
              s(ndir)=s(ndir)+cabs(x/y)
            end if
            tot=tot+cabs(x/y)
          end do

c--  normalize spectrum for full 360 degree run

          if (ndeg .eq. 360 .and. tot .gt. 0.0) then
            do i = 1, 360
              s(i) = s(i)/tot
            end do
            chk = 1
          else

c-- tot should = 360.  If directional peak is extremely narrow then
c-- 1 deg resolution may be insufficient and tot .ne. 360

            chk=tot/(360./res)
          end if

          return
        end subroutine


c-- WC5M_MEM_TO_SP -------------------------------------------------------------
c  Transforms wave%dirspec 2D spectra into wave%a0/a1/a2/b1/b2 values.
c  Used to fill wc5_datasets where only the 2D spectra are known.
c-------------------------------------------------------------------------------
      subroutine wc5m_mem_to_sp(wset, ecode)
        integer::  ecode, i, j, k
        real       band_e
        type(wc5_dataset)   wset

c--   Calculate freq spectrum at prediction site by multiplying the
c--   MEM spectrum energies (m^2) by the transfer coefficients.  

        do k = 1, wset%wave%time_count
          do j = 1, wset%wave%freq_count
            wset%wave%a0(j,k) = 0.
            wset%wave%a1(j,k) = 0.
            wset%wave%b1(j,k) = 0.
            wset%wave%a2(j,k) = 0.
            wset%wave%b2(j,k) = 0.

            do i = 1, wset%wave%dir_count
              if (wset%wave%dirspec(i,j,k) .gt. 0) then
                band_e = wset%wave%dirspec(i,j,k) * 360.0/wset%wave%dir_count

c--   Predicted directional moments (use directional coeff array variables for 
c--   this, but these are not normalized by energy, so true E*cos(theta) etc...)

                wset%wave%a0(j,k) = wset%wave%a0(j,k) + band_e
                wset%wave%a1(j,k) = wset%wave%a1(j,k) + band_e*COS(to_radians(1.0*wset%wave%dirs(i)))
                wset%wave%b1(j,k) = wset%wave%b1(j,k) + band_e*SIN(to_radians(1.0*wset%wave%dirs(i)))
                wset%wave%a2(j,k) = wset%wave%a2(j,k) + band_e*COS(to_radians(2.0*wset%wave%dirs(i)))
                wset%wave%b2(j,k) = wset%wave%b2(j,k) + band_e*SIN(to_radians(2.0*wset%wave%dirs(i)))
              end if
            end do	!* End direction loop

            if (wset%wave%a0(j,k) .gt. 0.0) then
              wset%wave%a1(j,k) = wset%wave%a1(j,k) / wset%wave%a0(j,k)
              wset%wave%b1(j,k) = wset%wave%b1(j,k) / wset%wave%a0(j,k)
              wset%wave%a2(j,k) = wset%wave%a2(j,k) / wset%wave%a0(j,k)
              wset%wave%b2(j,k) = wset%wave%b2(j,k) / wset%wave%a0(j,k)
              wset%wave%mdir(j,k) = to_degrees(ATAN2(wset%wave%b1(j,k),wset%wave%a1(j,k)))
              if (wset%wave%mdir(j,k) .lt. 0) wset%wave%mdir(j,k) = wset%wave%mdir(j,k) + 360
            else
              wset%wave%mdir(j,k) =  WC5_real_fill
              wset%wave%a1(j,k) = WC5_real_fill
              wset%wave%b1(j,k) = WC5_real_fill
              wset%wave%a2(j,k) = WC5_real_fill
              wset%wave%b2(j,k) = WC5_real_fill
            end if
          end do		!* End frequency loop
        end do		!* End time loop
      end subroutine

      end module
