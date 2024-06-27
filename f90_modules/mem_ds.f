c-- MEM_DS ---------------------------------------------------------------------
c
c   Contains routines for generating and working with directional spectra.
c   Based on the 'mem_data' type, which contains the two-dimensional (freq, dir)
c   spectra. Currently the directional bands are hardwired at 5 degrees,
c   creating 72 bands from 5 to 360.
c
c   07/2013 - changed mem%dir from integer to real
c-------------------------------------------------------------------------------

      module mem_ds

      use dates
      use file_ops
      use lookup_utils
      use spectral
      use unit_conversions

      save

      integer,parameter::   MEM_dir_bands = 72

      type mem_data
        integer dir_bands, freq_bands
        real    dir(MEM_dir_bands)
        real    freq(SP_max_coeffs), band_width(SP_max_coeffs)
        real    ds(MEM_dir_bands,SP_max_coeffs)
      end type

      contains


c-- MEM_WRITE ------------------------------------------------------------------
c  Dumps the mem_data to the specified out unit
c-------------------------------------------------------------------------------
        subroutine mem_write(mem, ounit)
          integer        i, j, ounit
          real           etot
          type(mem_data) mem

          etot = 0.0
          do i = 1, mem%dir_bands
            do j = 1, mem%freq_bands
              write(ounit,'(f10.7,1x,$)') mem%ds(i,j)
            end do
            write(ounit,'(a)') ''
          end do
        end subroutine


c-- MEM_CALC_HS ----------------------------------------------------------------
c  Calculates the Hs from the mem_data object given
c-------------------------------------------------------------------------------
        real function mem_calc_hs(mem)
          integer        i, j
          real           etot
          type(mem_data) mem

          etot = 0.0
          do i = 1, mem%freq_bands
            do j = 1, mem%dir_bands
              etot = etot + mem%ds(j,i)*mem%band_width(i)*(360/mem%dir_bands)
            end do
          end do
          mem_calc_hs = 4 * etot**0.5
        end function


c-- INIT_DS --------------------------------------------------------------------
c  Initializes a directional spectra with the specified freq/dir layout and
c  default ds value 
c-------------------------------------------------------------------------------
        type(mem_data) function init_ds(freq_count,freqs,bws,dir_count,dirs,val)
          integer dir_count, freq_count
          real    dirs(*), freqs(*), bws(*) 

          init_ds%dir_bands = dir_count
          init_ds%dir(1:dir_count) = dirs(1:dir_count)

          init_ds%freq_bands = freq_count
          init_ds%freq(1:freq_count) = freqs(1:freq_count)
          init_ds%band_width(1:freq_count) = bws(1:freq_count)

          do i = 1, dir_count
            do j = 1, freq_count
              init_ds%ds(i,j) = val
            end do
          end do
        end function


c-- LOAD_SPFILE ----------------------------------------------------------------
c  Loads a spectral file for use with other routines in the module.
c-------------------------------------------------------------------------------
        subroutine load_spfile(sp_name, sp_data, sp_hdr, err_code)

          integer:: err_code, err_unit=6
          character*100 sp_name, sp_path
          type(sp_data_block) sp_data
          type(sp_hdr_block) sp_hdr

c-- Lookup path of spectral file

          sp_path = get_file_path(sp_name(1:19), err_code)
          if (err_code .ne. 0) then
            write(err_unit,'(a)') 'ERROR: sp file not found'
            return
          end if

c-- Load data, header

          call read_sp_file(sp_path, sp_name, sp_data, sp_hdr, err_code,
     *           err_unit)
          if (err_code .eq. -1) err_code = 0
          if (err_code .ne. 0) then
            write(err_unit,'(a)') 'ERROR: reading sp file'
            return
          end if

          return
        end subroutine


c-- SP_TO_MEM ------------------------------------------------------------------
c  Creates the mem_data object corresponding to the given sp_data_block
c-------------------------------------------------------------------------------
        subroutine sp_to_mem(sp_data, sp_hdr, mem_out, err_code)

          integer:: err_code, err_unit=6
          type(sp_data_block) sp_data
          type(sp_hdr_block) sp_hdr
          type(mem_data) mem_out

c-- Normalize coefficients for pressure sensor data

          if (sp_hdr%sensor_type(1:8) .eq. 'Pressure') then
            call array_mem_est(sp_data, mem_out)
          else
            call mem_est(sp_data, mem_out)
          end if

          return
        end subroutine


c-- NORMALIZE_SP ---------------------------------------------------------------
c  Subroutine that normalizes the coefficients in a sp_data_block. Direction
c  is set to -1 for any band in which the coeffs can't be normalized
c  by energy.
c-------------------------------------------------------------------------------
      subroutine normalize_sp(sp_data)
        type(sp_data_block) sp_data

        do i= 1, sp_data%bands
          if (sp_data%dir(i) .ne. -1) then
            sp_data%a1(i) = sp_data%a1(i) / sp_data%ener_dens(i)
            sp_data%a2(i) = sp_data%a2(i) / sp_data%ener_dens(i)
            sp_data%b1(i) = sp_data%b1(i) / sp_data%ener_dens(i)
            sp_data%b2(i) = sp_data%b2(i) / sp_data%ener_dens(i)

            max_coeff = MAX(sp_data%a1(i), sp_data%a2(i), sp_data%b1(i),
     *        sp_data%b2(i))
            min_coeff = MIN(sp_data%a1(i), sp_data%a2(i), sp_data%b1(i),
     *        sp_data%b2(i))
            if (max_coeff .gt. 1.0 .or. min_coeff .lt. -1.0) sp_data%dir(i) = -1
          end if
        end do

      end subroutine



c-- UNNORMALIZE_SP -------------------------------------------------------------
c  Subroutine that unnormalizes the coefficients in a sp_data_block. 
c-------------------------------------------------------------------------------
      subroutine unnormalize_sp(sp_data)
        type(sp_data_block) sp_data

        do i= 1, sp_data%bands
          if (sp_data%dir(i) .ne. -1) then
            sp_data%a1(i) = sp_data%a1(i) * sp_data%ener_dens(i)
            sp_data%a2(i) = sp_data%a2(i) * sp_data%ener_dens(i)
            sp_data%b1(i) = sp_data%b1(i) * sp_data%ener_dens(i)
            sp_data%b2(i) = sp_data%b2(i) * sp_data%ener_dens(i)
          end if
        end do

      end subroutine


c-- MAKE_NOISY_SP --------------------------------------------------------------
c  Subroutine that adds random noise to a modeled spectrum to make it look
c  more like actual wave data
c-------------------------------------------------------------------------------
      subroutine make_noisy_sp(sp_data)
        real rando(4)
        type(sp_data_block) sp_data

          do i=1, sp_data%bands
            call random_number(rando)
            sp_data%a1(i) = sp_data%a1(i)*(0.9 + (rando(1)/5))
            sp_data%b1(i) = sp_data%b1(i)*(0.9 + (rando(2)/5))
            sp_data%a2(i) = sp_data%a2(i)*(0.9 + (rando(3)/5))
            sp_data%b2(i) = sp_data%b2(i)*(0.9 + (rando(4)/5))
          end do

      end subroutine


c-- COMBINE_SPS ----------------------------------------------------------------
c  Subroutine that combines an array of spectra; used to average
c  data that are very close in time. NOTE: the spectra must have the same
c  layout; use redistribute_sp() if necessary.
c-------------------------------------------------------------------------------
        subroutine combine_sps(sp_array, count, combo)

        integer count
        real  cos_combo, sin_combo
        type(sp_data_block) sp_array(count), combo

          if (count .le. 1) then		!* No combining needed
            combo = sp_array(1)
            return
          end if

          if (sp_array(1)%bands .ne. sp_array(2)%bands) then
            write(6,'(a)') 'ERROR combine_sps(): spectra must have same layout'
            return
          end if
          combo%bands = sp_array(1)%bands

          do i = 1, combo%bands

            combo%freq(i) = sp_array(1)%freq(i)
            combo%band_width(i) = sp_array(1)%band_width(i)
            combo%ener_dens(i) = 0
            combo%a1(i) = 0
            combo%a2(i) = 0
            combo%b1(i) = 0
            combo%b2(i) = 0
            combo%check(i) = -1         !* Check factors not recalculated
            sin_combo = 0
            cos_combo = 0

c--  Directional coeffs must be unnormalized, merged

            do j = 1, count
              combo%ener_dens(i) = combo%ener_dens(i) + sp_array(j)%ener_dens(i)
              combo%a1(i) = combo%a1(i) +
     *                       sp_array(j)%ener_dens(i)*sp_array(j)%a1(i)
              combo%a2(i) = combo%a2(i) +
     *                       sp_array(j)%ener_dens(i)*sp_array(j)%a2(i)
              combo%b1(i) = combo%b1(i) +
     *                       sp_array(j)%ener_dens(i)*sp_array(j)%b1(i)
              combo%b2(i) = combo%b2(i) +
     *                       sp_array(j)%ener_dens(i)*sp_array(j)%b2(i)
              sin_combo = sin_combo + sp_array(j)%ener_dens(i) *
     *                      SIN(to_radians(sp_array(j)%dir(i)))
              cos_combo = cos_combo + sp_array(j)%ener_dens(i) *
     *                      COS(to_radians(sp_array(j)%dir(i)))
            end do

c--  Now do energy and renormalize directional coeff.

            if (combo%ener_dens(i) .gt. 0.0) then
              combo%ener_dens(i) = combo%ener_dens(i) / count
              combo%a1(i) = combo%a1(i) / count / combo%ener_dens(i)
              combo%a2(i) = combo%a2(i) / count / combo%ener_dens(i)
              combo%b1(i) = combo%b1(i) / count / combo%ener_dens(i)
              combo%b2(i) = combo%b2(i) / count / combo%ener_dens(i)

c--  Last recalculate direction

              combo%dir(i) = to_degrees(ATAN2(sin_combo,cos_combo))
              if (combo%dir(i) .lt. 0) combo%dir(i) = combo%dir(i)+360.
            else
              combo%dir(i) = -1
            end if

          end do
        end subroutine


c-- REDISTRIBUTE_SP_01Hz -------------------------------------------------------
c  Subroutine that redistributes a spectrum into even 0.01 Hz width bands.
c  Currently it is set to create 56 bands from 0.03 to 0.58 Hz. The redist
c  work is done by the general REDISTIBUTE_SP routine.
c-------------------------------------------------------------------------------
      subroutine redistribute_sp_01hz(orig_sp, redist_sp)
        integer, parameter::  rbands = 56
        type(sp_data_block)  orig_sp, redist_sp

c--   Intialize redist_sp with 0.01 width bins starting at 0.03 Hz

        redist_sp%bands = rbands
        do i = 3, 2 + rbands
          redist_sp%freq(i-2) = i/100.0
          redist_sp%band_width(i-2) = 0.01
        end do

        call redistribute_sp(orig_sp, redist_sp)

      end subroutine


c-- REDISTRIBUTE_SP_2DATAWELL --------------------------------------------------
c  Subroutine that redistributes a spectrum into the datawell 64-band format.
c  The redist work is done by the general REDISTIBUTE_SP routine.
c-------------------------------------------------------------------------------
      subroutine redistribute_sp_2datawell(orig_sp, redist_sp)
        integer, parameter::  rbands = 64
        type(sp_data_block)  orig_sp, redist_sp

c--   Intialize redist_sp with the datawell spectral layout

        redist_sp%bands = rbands
        do i = 1, rbands
          if (i .lt. 16) then
            redist_sp%freq(i) = 0.020 + i * 0.005
            redist_sp%band_width(i) = 0.0050
          else if (i .eq. 16) then
            redist_sp%freq(i) = 0.10125
            redist_sp%band_width(i) = 0.0075
          else
            redist_sp%freq(i) = 0.10 + (i - 16) * 0.01
            redist_sp%band_width(i) = 0.0100
          end if
        end do

        call redistribute_sp(orig_sp, redist_sp)

      end subroutine


c-- REDISTRIBUTE_SP ------------------------------------------------------------
c  Subroutine that redistributes a spectrum into a new spectral layout.
c  The frequencies, band count, and band widths must be set in redist_sp
c  before calling.
c
c  NOTE: This code assumes that the frequencies given in the sp_data are
c  center frquencies except for 0.10 Hz bands of 0.0075 width; these are
c  assumed to be datawell's transition band, running from 0.0975 to 0.1050.
c-------------------------------------------------------------------------------
      subroutine redistribute_sp(orig_sp, redist_sp)

      integer   b, i, rbands
      logical:: bin_complete, missing_dir

      real
     *  bot_orig, bot_redist,
     *  cos_sum, cos_avg, curr_energy,
     *  sin_sum, sin_avg,
     *  top_orig, top_redist

      type(sp_data_block)  orig_sp, redist_sp

c--   Initialize the new spectral dist (redist_sp)

        rbands = redist_sp%bands
        do i = 1, rbands
          redist_sp%ener_dens(i) = 0.00
          redist_sp%dir(i) = -1
          redist_sp%check(i) = -1
          redist_sp%a1(i) = 0
          redist_sp%b1(i) = 0
          redist_sp%a2(i) = 0
          redist_sp%b2(i) = 0
        end do

c--   Do the business - loop over the new bins, adding in each of the original
c--   spectral bands to the appropriate bin. Partition bands where necessary.

        do i = 1, rbands                !* loop over new bins
          cos_sum = 0
          sin_sum = 0
          missing_dir = .false.
          top_redist = redist_sp%freq(i) + redist_sp%band_width(i)/2
          bot_redist = redist_sp%freq(i) - redist_sp%band_width(i)/2
          if (redist_sp%freq(i) .eq. 0.10 .and. 
     *      redist_sp%band_width(i) .eq. .0075) then   !* datawell's uneven band
            top_redist = 0.1050
            bot_redist = 0.0975
          end if

          do b = 1, orig_sp%bands       !* loop over original bands
            top_orig = orig_sp%freq(b) + orig_sp%band_width(b)/2
            bot_orig = orig_sp%freq(b) - orig_sp%band_width(b)/2
            if (orig_sp%freq(b) .eq. 0.10 .and. 
     *        orig_sp%band_width(b) .eq. .0075) then   !* datawell's uneven band
              top_orig = 0.1050
              bot_orig = 0.0975
            end if

c--   If the full band falls into the current bin, add the entire contents

            if (top_orig .le. top_redist .and. bot_orig .ge. bot_redist) then
              curr_energy = orig_sp%ener_dens(b) * orig_sp%band_width(b)
              call band_calcs(curr_energy, orig_sp, redist_sp, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the bottom of the band falls in the bin, add in the appropriate portion

            else if (bot_orig .lt. top_redist .and.
     *               bot_orig .ge. bot_redist) then
              curr_energy = orig_sp%ener_dens(b) * (top_redist-bot_orig)
              call band_calcs(curr_energy, orig_sp, redist_sp, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the top of the band falls in the bin, add in the appropriate portion

            else if (top_orig .gt. bot_redist .and.
     *               top_orig .le. top_redist) then
              curr_energy = orig_sp%ener_dens(b) * (top_orig-bot_redist)
              call band_calcs(curr_energy, orig_sp, redist_sp, sin_sum, cos_sum, 
     *               missing_dir, i, b)

c--   If the middle of the band falls in the bin, add in the appropriate portion

            else if (top_orig .ge. top_redist .and.
     *               bot_orig .le. bot_redist) then
              curr_energy = orig_sp%ener_dens(b) * (top_redist-bot_redist)
              call band_calcs(curr_energy, orig_sp, redist_sp, sin_sum, cos_sum, 
     *               missing_dir, i, b)

            end if
          end do


c--   Calculate direction and calc ener_dens once bin is complete

          if (redist_sp%ener_dens(i) .gt. 0) then
            redist_sp%ener_dens(i) =
     *        redist_sp%ener_dens(i) / redist_sp%band_width(i)
            if (.not. missing_dir) then
              sin_avg = sin_sum/redist_sp%ener_dens(i)
              cos_avg = cos_sum/redist_sp%ener_dens(i)
              redist_sp%dir(i) = to_degrees(ATAN2(sin_avg,cos_avg))
              if (redist_sp%dir(i) .lt. 0)
     *          redist_sp%dir(i) = redist_sp%dir(i)+360.

              redist_sp%a1(i) = redist_sp%a1(i) / redist_sp%band_width(i)
              redist_sp%b1(i) = redist_sp%b1(i) / redist_sp%band_width(i)
              redist_sp%a2(i) = redist_sp%a2(i) / redist_sp%band_width(i)
              redist_sp%b2(i) = redist_sp%b2(i) / redist_sp%band_width(i)
            else
              redist_sp%dir(i) = -1
              redist_sp%a1(i) = 0
              redist_sp%b1(i) = 0
              redist_sp%a2(i) = 0
              redist_sp%b2(i) = 0
            end if
          else
            redist_sp%dir(i) = -1
          end if
        end do


c--   Normalize once energy redistributed

        call normalize_sp(redist_sp)

      end subroutine


c-- BAND_CALCS -----------------------------------------------------------------
c  Helper for REDISTRIBUTE_SP; adds components of original spectral layout
c  into the redistributed layout, weighting by energy
c-------------------------------------------------------------------------------
        subroutine band_calcs(curr_energy, orig_sp, redist_sp, sin_sum, cos_sum, 
     *               missing_dir, i, b)
          integer  b, i         !* b = curr band in original, i = curr in new
          logical  missing_dir
          real     cos_sum, curr_energy, sin_sum
          type(sp_data_block)  orig_sp, redist_sp

          if (curr_energy .eq. 0.0) return

          redist_sp%ener_dens(i) = redist_sp%ener_dens(i) + curr_energy
          if (orig_sp%dir(b) .eq. -1) then
            missing_dir = .true.
          else
            redist_sp%a1(i) = redist_sp%a1(i) + curr_energy*orig_sp%a1(b)
            redist_sp%b1(i) = redist_sp%b1(i) + curr_energy*orig_sp%b1(b)
            redist_sp%a2(i) = redist_sp%a2(i) + curr_energy*orig_sp%a2(b)
            redist_sp%b2(i) = redist_sp%b2(i) + curr_energy*orig_sp%b2(b)
            sin_sum = sin_sum + curr_energy*sin(to_radians(orig_sp%dir(b)))
            cos_sum = cos_sum + curr_energy*cos(to_radians(orig_sp%dir(b)))
          end if

        end subroutine


c-- HARVEST_FORMAT -------------------------------------------------------------
c  Converts the directional spectrum into the format bor uses for
c  modeling Harvest data. This involves trimming the frequency bands:
c  instead of 56 bands from .03 to .58 Hz, it leaves 22 bands, from 
c  .04 to .25 Hz. NOTE: Must be called after REDISTRIBUTE_SP.
c-------------------------------------------------------------------------------
        subroutine harvest_format(mem_out)
          type(mem_data) mem_out

          do j = 2, 23
            do i = 1, mem_out%dir_bands
              mem_out%ds(i,j-1)=mem_out%ds(i,j)*10000.*(0.01)*5.
            end do
            mem_out%freq(j-1) = mem_out%freq(j)
          end do

          mem_out%freq_bands = 22
        end subroutine


c-- MEM_EST --------------------------------------------------------------------
c
c  Subroutine to make MEM estimate of the directional spectrum
c  This version reads in normalized four. coeff., so no division by a0
c
c  Input:  Energy and 4 normalized directional fourier coeff.
c          from a buoy or slope array.
c
c  Output: Directional spectrum in 5 degree directional bins (in
c           (ds) the "compass heading" coordinate frame).
c
c            72 directional bands. 1=5 degress....72=360 degrees
c            where the direction is the compass heading from which
c            the wave energy is arriving. e.g. 270=from west, 180=
c            from south etc.
c
c  The ds estimate is set to 0 for any band in which direction is not
c  resolved (dir = -1) or where the coeffs could not be normalized.
c-------------------------------------------------------------------------------
      subroutine mem_est(sp_data, mem_out)

        real chk, d(360)
        type(sp_data_block) sp_data
        type(mem_data) mem_out

c-- Initialize number of dir, freq bands

        mem_out%freq_bands = sp_data%bands
        mem_out%dir_bands = MEM_dir_bands

c-- Loop thru freq bands
c-- moments are first four fourier coeff. of the directional
c-- distribution, MEM uses normalized values

        do i= 1, sp_data%bands

          mem_out%freq(i) = sp_data%freq(i)
          mem_out%band_width(i) = sp_data%band_width(i)

c-- get MEM estimate (d) with 1 deg resolution, d is returned in
c-- compass coordinates

          if (sp_data%dir(i) .ne. -1) call mem(sp_data%a1(i), sp_data%a2(i),
     *      sp_data%b1(i), sp_data%b2(i), d, chk)


c-- merge into 5 deg directional bins, multiply by 0.2 to get
c-- units of m^2/Hz-deg

          do j = 5, 355, 5
            if (sp_data%dir(i) .ne. -1) then
              mem_out%ds(j/5,i) =
     *          .2*sp_data%ener_dens(i)*(d(j-2)+d(j-1)+d(j)+d(j+1)+d(j+2))
            else
              mem_out%ds(j/5,i) = 0
            end if

          end do

          if (sp_data%dir(i) .ne. -1) then
            mem_out%ds(72,i) =
     *        0.2*sp_data%ener_dens(i)*(d(358)+d(359)+d(360)+d(1)+d(2))
          else
            mem_out%ds(72,i) = 0
          end if

        end do

        do k = 5, 360, 5
          mem_out%dir(k/5) = k
        end do

        return
      end subroutine


c-- MEM ------------------------------------------------------------------------
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
        subroutine mem(a1,a2,b1,b2,s,chk)
          real    a1, a2, b1, b2, chk, s(360)
          call memd(a1,a2,b1,b2,1,360,1.0,s,chk)
        end subroutine


c-- MEMD -----------------------------------------------------------------------
c  Does the calcs for MEM above. Call directly if do not need full 360 degrees
c  or 0.1-degree resolution. Only returns values for integer degrees; res
c  must be <= 1.0, and the start direction, begin, must be > 0.
c
c  Modified 6/2011, added normalization to handle narrow peaks in hindcast data
c    When running the full directional span (1 to 360), normalized by tot
c    instead of 360.
c-------------------------------------------------------------------------------
        subroutine memd(a1,a2,b1,b2,begin,ndeg,res,s,chk)
          integer begin, dir, n, ndeg
          real    a1, a2, b1, b2, chk, s(360), offset, res, rn
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


c-- ARRAY_MEM_EST --------------------------------------------------------------
c
c  Subroutine to make 2d estimate of the directional spectrum for array data
c
c  Input:  sp_data_block, slope array-formatted coeffs
c
c  Output: Directional spectrum in 5 degree directional bins
c
c  The ds estimate is set to 0 for any band in which direction is not
c  resolved (dir = -1) or where the coeffs could not be normalized.
c-------------------------------------------------------------------------------
      subroutine array_mem_est(sp_data, mem_out)

        integer angle
        real a0(SP_max_coeffs), direct_sp(0:359)
        type(sp_data_block) sp_data
        type(mem_data) mem_out

c-- Initialize number of dir, freq bands

        mem_out%freq_bands = sp_data%bands
        mem_out%dir_bands = MEM_dir_bands
        pi = get_pi()

c-- Loop thru freq bands

        do i= 1, sp_data%bands

          mem_out%freq(i) = sp_data%freq(i)
          sp_data%a1(i) = sp_data%a1(i) * (pi*10000*sp_data%band_width(i))
          sp_data%b1(i) = sp_data%b1(i) * (pi*10000*sp_data%band_width(i))
          sp_data%a2(i) = sp_data%a2(i) * (pi*10000*sp_data%band_width(i))
          sp_data%b2(i) = sp_data%b2(i) * (pi*10000*sp_data%band_width(i))
          a0(i) = sp_data%ener_dens(i)*10000*sp_data%band_width(i)/pi


c-- Get direct_sp values for each angle

          do angle = 0, 359
            rad_a = to_radians(REAL(angle))
            direct_sp(angle) = to_radians(a0(i)/2. +
     *        sp_data%a1(i)*COS(1.*rad_a) + sp_data%b1(i)*SIN(1.*rad_a) +
     *        sp_data%a2(i)*COS(2.*rad_a) + sp_data%b2(i)*SIN(2.*rad_a))
            if (direct_sp(angle) .lt. 0) direct_sp(angle) = direct_sp(angle)+2*pi
          end do

c-- merge into 5 deg directional bins

          do j = 5, 355, 5
            if (sp_data%dir(i) .ne. -1) then
              mem_out%ds(j/5,i) = direct_sp(j-2)+direct_sp(j-1)+direct_sp(j)+
     *          direct_sp(j+1)+direct_sp(j+2)
            else
              mem_out%ds(j/5,i) = 0
            end if

          end do

          if (sp_data%dir(i) .ne. -1) then
            mem_out%ds(72,i) = direct_sp(358)+direct_sp(359)+direct_sp(0)+
     *        direct_sp(1)+direct_sp(2)
          else
            mem_out%ds(72,i) = 0
          end if

        end do

        do k = 5, 360, 5
          mem_out%dir(k/5) = k
        end do

        return
      end subroutine


c-------------------------------------------------------------------------------
c  mem_to_sp.f
c
c  Subroutine that transforms a 2d spectra to a standard sp_data_block,
c  i.e. energy and directional fourier coefficients.
c-------------------------------------------------------------------------------
      subroutine mem_to_sp(mem_in, out_sp, ecode)

      integer::  ecode, ref_bands, tbands(SP_max_coeffs)
      logical    filter_energy
      real check_sum, check_total

      character*100 ref_file

      type(mem_data) mem_in
      type(sp_data_block) out_sp


c--   Initialize the output sp_data_block

        out_sp%bands = mem_in%freq_bands
        out_sp%freq = mem_in%freq
        out_sp%band_width = mem_in%band_width
        do i = 1, out_sp%bands
          out_sp%check(i) = 1.0
        end do


c--   Calculate freq spectrum at prediction site by multiplying the
c--   MEM spectrum energies (m^2) by the transfer coefficients.  

        do j = 1, out_sp%bands
          out_sp%ener_dens(j) = 0.
          out_sp%a1(j) = 0.
          out_sp%b1(j) = 0.
          out_sp%a2(j) = 0.
          out_sp%b2(j) = 0.
          check_total = 0.
          check_sum = 0.

          do i = 1, mem_in%dir_bands

            if (mem_in%ds(i,j) .gt. 0) then
              band_e = mem_in%ds(i,j) * 360.0/mem_in%dir_bands


c--   Predicted directional moments (use directional coeff array variables for 
c--   this, but these are not normalized by energy, so true E*cos(theta) etc...)

              out_sp%ener_dens(j) = out_sp%ener_dens(j) + band_e
              out_sp%a1(j) = out_sp%a1(j) + band_e*COS(to_radians(1.0*mem_in%dir(i)))
              out_sp%b1(j) = out_sp%b1(j) + band_e*SIN(to_radians(1.0*mem_in%dir(i)))
              out_sp%a2(j) = out_sp%a2(j) + band_e*COS(to_radians(2.0*mem_in%dir(i)))
              out_sp%b2(j) = out_sp%b2(j) + band_e*SIN(to_radians(2.0*mem_in%dir(i)))
            endif
          end do	!* End direction loop

          if (out_sp%ener_dens(j) .gt. 0.0) then
            out_sp%a1(j) = out_sp%a1(j) / out_sp%ener_dens(j)
            out_sp%b1(j) = out_sp%b1(j) / out_sp%ener_dens(j)
            out_sp%a2(j) = out_sp%a2(j) / out_sp%ener_dens(j)
            out_sp%b2(j) = out_sp%b2(j) / out_sp%ener_dens(j)
            out_sp%dir(j) = to_degrees(ATAN2(out_sp%b1(j),out_sp%a1(j)))
            if (out_sp%dir(j) .lt. 0) out_sp%dir(j) = out_sp%dir(j) + 360
          else
            out_sp%dir(j) =  -1.0
          end if
        end do		!* End frequency loop
        end subroutine

      end module
