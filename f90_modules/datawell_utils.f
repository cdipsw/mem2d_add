c-- DATAWELL_UTILS -------------------------------------------------------------
c
c   The datawell_utils module is designed for use with datawell directional
c   buoys. It pulls parameters and spectral data out of an array of
c   datawell vectors (see the VECTORS module) that represent one spectral
c   transmission (this is 320 vectors for a Mark I buoy, 288 for a Mark II).
c
c   There are two main types of functions in this module: parameter
c   functions, which are passed a vector array and return the appropriate
c   parameter, and spectral functions, which are passed both a vector array
c   and the desired frequency band. Both types of functions rely on 
c   'return_vector', a function which locates the vector that contains the
c   desired information and passes it back.
c
c   2013-11-15 Added 'vstat' as argument, to track vector quality
c
c   Used by: .detox/detox, .fdisp/logfd
c
c-------------------------------------------------------------------------------
        module datawell_utils

        use datawell_vectors
        use dates
        use unit_conversions

        save

        integer DU_block_size
        integer DU_magnetic_var
        integer DU_trans_size
        logical DU_mark_I
        logical DU_mark_III

        integer, parameter:: DU_freq_count = 64
        real, parameter:: DU_frequencies(64) = (/0.025, 0.03, 0.035, 0.04,
     *     0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 0.075, 0.08, 0.085, 0.09,
     *     0.095, 0.10125, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19,
     *     0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.30,
     *     0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41,
     *     0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.52,
     *     0.53, 0.54, 0.55, 0.56, 0.57, 0.58/)
        real, parameter:: DU_bandwidths(64) = (/0.005, 0.005, 0.005, 0.005,
     *    0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005,
     *    0.005, 0.0075, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     *    0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     *    0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     *    0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     *    0.01, 0.01, 0.01, 0.01, 0.01, 0.01/)


        contains

   
c-- INIT_BUOY ------------------------------------------------------------------
c       
c       Initializes the globals needed to process spectral data. 
c       NOTE: This routine should be called before any others in this module
c   
c-------------------------------------------------------------------------------
        subroutine init_buoy(mark_number)
          integer mark_number
          if (mark_number .eq. 1) then            !* Mark I buoy
            DU_mark_I = .true.
            DU_block_size = 10
            DU_trans_size = 320
          else                           !* Mark II or Mark III buoy
            DU_block_size = 18
            DU_mark_I = .false.
            DU_trans_size = 288
          end if
          if (mark_number .eq. 3) then
            DU_mark_III = .true.
          else
            DU_mark_III = .false.
          end if
           
        end subroutine


c-- RETURN_VECTOR --------------------------------------------------------------
c
c   Returns the dw_vector from the requested page and line (i.e. finds the 
c   requested position in the vector array)
c
c-------------------------------------------------------------------------------
          type(dw_vector) function return_vector(vecs, page, vec_no)
            integer page, vec_no
            type(dw_vector) vecs(320)
            return_vector = vecs(page*DU_block_size + vec_no)
          end function


c-- DW_UPDATE_VSTAT ------------------------------------------------------------
c
c   Sets the vector status based on the error byte. 
c
c-------------------------------------------------------------------------------
          subroutine dw_update_vstat(vec, vstatus)
            integer vstatus
            type(dw_vector) vec
            if (vec%error .gt. 1 .and. vstatus .lt. 2) then
              vstatus = 2
            else if (vec%error .eq. 1 .and. vstatus .lt. 1) then
              vstatus = 1
            end if
          end subroutine


c-- GET_BATTERY, GET_FZ, GET_HS, GET_INCLIN, etc. ------------------------------
c
c   The following parameter functions return datawell parameters from the 
c   the appropriate parts of the vector array. See the datawell manual for 
c   the formulas used in these calculations.
c
c-------------------------------------------------------------------------------
          integer function get_battery(vecs, vstat)		!* Battery voltage
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,6,2)
            call dw_update_vstat(vec, vstatus)
            if (DU_mark_I) then
              get_battery = 2*bottom(vec%sys2,3) + 8	!* 22-8 for Mk I
            else
              get_battery = bottom(vec%sys2,3) + 19	!* 26-19 for Mk II
            end if
            if (PRESENT(vstat)) vstat = vstatus
          end function


          integer function get_battery_status(vecs, vstat)	!* Battery status 0-7
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,6,2)
            call dw_update_vstat(vec, vstatus)
            get_battery_status = bottom(vec%sys2,3)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          integer function get_wol(vecs, vstat)		!* weeks of life, batt
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,6,2)
            call dw_update_vstat(vec, vstatus)
            get_wol = up(bottom(vec%sys1,4),4) + top(vec%sys2,4)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_fz(vecs, vstat)			!* Zee frequency?
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,2,2)
            call dw_update_vstat(vec, vstatus)
            get_fz = vec%sys2/400.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_Hs(vecs, vstat)			!* Wave height (m)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,1,2)
            call dw_update_vstat(vec, vstatus)
            get_Hs = (up(bottom(vec%sys1,4),8) + vec%sys2)/100.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_inclin(vecs, vstat)		!* Inclination
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,15,2)
            call dw_update_vstat(vec, vstatus)
            get_inclin = 0.703125 * (vec%sys2 + 
     *        bottom(vec%sys1,4)/16. - 128.)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_zaoff(vecs, vstat)			!* Vert accel offset (m/s^2)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            integer raw_lat
            vstatus = 0
            vec = return_vector(vecs,7,2)
            call dw_update_vstat(vec, vstatus)
            get_zaoff = add_sign((up(bottom(vec%sys1,4),8)+vec%sys2),12) / 800.0
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_xaoff(vecs, vstat)			!* x accel offset (m/s^2)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            integer raw_lat
            vstatus = 0
            vec = return_vector(vecs,8,2)
            call dw_update_vstat(vec, vstatus)
            get_xaoff = add_sign((up(bottom(vec%sys1,4),8)+vec%sys2),12) / 800.0
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_yaoff(vecs, vstat)			!* y accel offset (m/s^2)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            integer raw_lat
            vstatus = 0
            vec = return_vector(vecs,9,2)
            call dw_update_vstat(vec, vstatus)
            get_yaoff = add_sign((up(bottom(vec%sys1,4),8)+vec%sys2),12) / 800.0
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_lat(vecs, vstat)			!* Latitude (dec deg)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec1, vec2
            integer raw_lat
            vstatus = 0
            vec1 = return_vector(vecs,10,2)
            call dw_update_vstat(vec1, vstatus)
            vec2 = return_vector(vecs,11,2)
            call dw_update_vstat(vec2, vstatus)
            raw_lat = up((up(bottom(vec1%sys1,4),8)+vec1%sys2),12) +
     *        up(bottom(vec2%sys1,4),8)+vec2%sys2
            get_lat = (add_sign(raw_lat,24)/real(2**23))*90.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_long(vecs, vstat)			!* Longitude (dec deg)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec1, vec2
            integer raw_long
            vstatus = 0
            vec1 = return_vector(vecs,12,2)
            call dw_update_vstat(vec1, vstatus)
            vec2 = return_vector(vecs,13,2)
            call dw_update_vstat(vec2, vstatus)
            raw_long = up((up(bottom(vec1%sys1,4),8)+vec1%sys2),12) +
     *        up(bottom(vec2%sys1,4),8)+vec2%sys2
c           get_long = abs(add_sign(raw_long,24)/real(2**23)*180.)
            get_long = add_sign(raw_long,24)/real(2**23)*180.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_orient(vecs, vstat)		!* Orientation (deg)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,14,2)
            call dw_update_vstat(vec, vstatus)
            get_orient = vec%sys2 * 1.406
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_peak_power(vecs, vstat)		!* Peak power
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,3,2)
            call dw_update_vstat(vec, vstatus)
            get_peak_power = exp(-1*(up(bottom(vec%sys1,4),8) + 
     *       vec%sys2)/200.)*5000.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_reftemp(vecs, vstat)		!* Reference temp
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,4,2)
            call dw_update_vstat(vec, vstatus)
            get_reftemp = (up(bottom(vec%sys1,2),8) + vec%sys2)/20. - 5
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_airtemp(vecs, vstat)		!* CAT4 air temp
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,4,2)
            call dw_update_vstat(vec, vstatus)
            get_airtemp = (up(bottom(vec%sys1,2),8) + vec%sys2)/10. - 35
            if (PRESENT(vstat)) vstat = vstatus
          end function


          logical function is_CAT4(vecs, vstat)			!* CAT4 equipped
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,5,2)
            is_CAT4 = BTEST(vec%sys1, 2)
            call dw_update_vstat(vec, vstatus)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          logical function airT_evap_flag(vecs, vstat)		!* evaporation uncertainty flag
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,4,2)
            airT_evap_flag = BTEST(vec%sys1, 2)
            call dw_update_vstat(vec, vstatus)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          logical function airT_solar_flag(vecs, vstat)		!* solar uncertainty flag
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,4,2)
            airT_solar_flag = BTEST(vec%sys1, 3)
            call dw_update_vstat(vec, vstatus)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          type(date_block) function get_sp_time(vecs, vstat)	!* Sp start time
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            integer elapsed_secs, proc_time, pT, Tn
            vstatus = 0
            vec = return_vector(vecs,0,2)
            call dw_update_vstat(vec, vstatus)
            pT = bottom(vec%sys1,4)
            Tn = bottom(vec%sys2,4)
            vec = return_vector(vecs,6,2)
            call dw_update_vstat(vec, vstatus)
            proc_time = up(bottom(vec%sys1,4),4) + top(vec%sys2,4)
            if (DU_mark_I) then
              elapsed_secs = nint(12.5*proc_time + (Tn-1)*250. + 1800.)
            else if (DU_mark_III) then
c             if (Tn .le. 0) then
c               elapsed_secs = 1600
c             else
                elapsed_secs = (Tn-1)*225 + 1800
c             end if
            else
              elapsed_secs = nint(12.5*proc_time + (pT+Tn-1)*225. + 1806.25)
            end if
c           write(6,*) 'Tn, elapsed secs: ', Tn, elapsed_secs
            get_sp_time = subtract_seconds(vecs(1)%time, elapsed_secs)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_temp(vecs, vstat)			!* Sea surface temp (C)
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,5,2)
            call dw_update_vstat(vec, vstatus)
            get_temp = (up(bottom(vec%sys1,2),8) + vec%sys2)/20. - 5
            if (PRESENT(vstat)) vstat = vstatus
          end function


          integer function get_trans(vecs, vstat)		!* Transmission no.
            integer vstatus
            integer,optional::  vstat
            type(dw_vector) vecs(320), vec
            vstatus = 0
            vec = return_vector(vecs,0,2)
            call dw_update_vstat(vec, vstatus)
            get_trans = bottom(vec%sys2,4)
            if (PRESENT(vstat)) vstat = vstatus
          end function


c-- GET_DIR, GET_FREQ, GET_POWER, etc. -----------------------------------------
c
c   The following spectral functions return spectral values for 
c   the requested band of the vector array. See the datawell manual for 
c   the formulas used in these calculations.
c
c-------------------------------------------------------------------------------
          real function get_dir(vecs, band, vstat)	!* direction in degrees
            integer,optional::  vstat
            integer band, vstatus
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              vec = return_vector(vecs,(band-1)/2,3+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,3+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_dir = vec%sys2 * 360./256.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_freq(vecs, band, vstat)	!* frequency in Hz
            integer,optional::  vstat
            integer band, vstatus
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              vec = return_vector(vecs,(band-1)/2,3+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,3+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            if (band .lt. 16) then
              get_freq = (5 + bottom(vec%sys1,6)) * 5./1000.
            else
              get_freq = (2*bottom(vec%sys1,6) - 10) * 5./1000.
            end if
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_power(vecs, band, vstat)	!* relative power dens.
            integer,optional::  vstat
            integer band, vstatus
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              vec = return_vector(vecs,(band-1)/2,4+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,4+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_power = exp(-1*(up(bottom(vec%sys1,4),8)+vec%sys2)/200.)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_spread(vecs, band, vstat)	!* spread in radians
            integer,optional::  vstat
            integer sl, band, vstatus
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              sl = 0
              vec = return_vector(vecs,(band-1)/2,5+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,3+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
              sl = top(vec%sys1,2)
              vec = return_vector(vecs,(band-1)/4,5+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_spread = to_radians((vec%sys1 + sl/4.) * 0.4476)
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_dw_a2(vecs, band, vstat)	!* datawell's a2, relative
            integer,optional::  vstat
            integer a2l, band, vstatus			!* to magetic North
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              a2l = 0
              vec = return_vector(vecs,(band-1)/2,5+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,4+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
              a2l = top(vec%sys1,2)
              vec = return_vector(vecs,(band-1)/4,5+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_dw_a2 = (vec%sys2 + a2l/4. - 128.)/128.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_dw_b2(vecs, band, vstat)	!* datawell's b2, relative
            integer,optional::  vstat
            integer b2l, band, vstatus			!* to magnetic North
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              b2l = 0
              vec = return_vector(vecs,(band-1)/2,6+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,4+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
              b2l = bottom(top(vec%sys1,4),2)
              vec = return_vector(vecs,(band-1)/4,6+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_dw_b2 = (vec%sys1 + b2l/4. - 128.)/128.
            if (PRESENT(vstat)) vstat = vstatus
          end function


          real function get_check(vecs, band, vstat)	!* check factor
            integer,optional::  vstat
            integer band, vstatus
            type(dw_vector) vecs(320), vec
            vstatus = 0
            if (DU_mark_I) then
              vec = return_vector(vecs,(band-1)/2,6+4*(mod(band-1,2)))
              call dw_update_vstat(vec, vstatus)
            else
              vec = return_vector(vecs,(band-1)/4,6+4*(mod(band-1,4)))
              call dw_update_vstat(vec, vstatus)
            end if
            get_check = vec%sys2/100.
            if (PRESENT(vstat)) vstat = vstatus
          end function


c-- CALC_A1_B1_A2_B2 -----------------------------------------------------------
c
c   Calculates the direction-corrected a's and b's for CDIP's spectral file.
c   The formulas used are based on code in the original buoy_fi:
c
c     extracting a1,  from the data.
c     datawell provides spreading s, mean direction theta and a2, b2,
c     plus the following relationships (pp 9 directional waverider manual)
c
c     s = sqrt(2-2m)
c     tan(theta) = b1/a1
c     m = sqrt(a1^2 + b1^2)              (this is the correct expression!!)
c
c     (s^2)/2 = 1 - sqrt(a1^2 + b1^2)
c     (s^2)/2 = 1 - sqrt(a1^2 + (a1^2)*tan^2(theta))
c     (s^2)/2 = 1 - a1*sqrt(1 + tan^2(theta))
c     (s^2)/2 = 1 - a1/cos(theta)
c     a1 = (1 - (s^2)/2)*cos(theta)
c     similarly
c     b1 = (1 - (s^2)/2)*sin(theta)
c
c     do band = 1,bands                                !* calc a1, b1
c      direction_mean = theta(cycle,band)
c      rad_theta = theta(cycle,band)*deg2rad           !* direction radians
c      rad_spread = spread(cycle,band)*deg2rad         !* spread, radians
c      rad_spread_sq = rad_spread**2
c      temp_val = 1. - rad_spread_sq/2.                !* removed sqrt, 7/15/99
c      a1_temp = temp_val*cos(rad_theta)
c      b1_temp = temp_val*sin(rad_theta)
c      a2_temp = a2_coeff(cycle,band)
c      b2_temp = b2_coeff(cycle,band)
c 
c      a1_prime = a1_temp*cos(theta_prime) -           !* rotate to true
c    .            b1_temp*sin(theta_prime)             !* north system
c      b1_prime = a1_temp*sin(theta_prime) + 
c    .            b1_temp*cos(theta_prime)
c
c      theta_mean = atan2(b1_prime, a1_prime)     !* wrt true north
c      theta_p = 0.5*atan2(b2_temp, a2_temp)      !* unrotated principal angle
c      theta_pr = theta_mean + theta_p
c      two_theta_rotate = 2.*theta_mean
c
c      Datawell gives a2, b2 with respect to a1, b1. That is, the
c      principal angle atan(b2/a2) is wrt to the mean angle,
c      atan(b1/a1) to conform to wavdas we convert the a2, b2 and principal
c      angle to be wrt true north
c
c      a2_prime = a2_temp*cos(two_theta_rotate) -      !* a2 and b2 with respect
c    .            b2_temp*sin(two_theta_rotate)        !* to true north
c      b2_prime = a2_temp*sin(two_theta_rotate) +
c    .            b2_temp*cos(two_theta_rotate)
c
c      the DU_ variable are "datawell_utils" global variables.
c      the a1,a2,b1,b2 below are same as a1_prime, etc.
c-------------------------------------------------------------------------------

          subroutine calc_a1_b1_a2_b2(theta,spread,dw_a2,dw_b2,a1,b1,a2,b2)

            real a1, b1, dw_a2, dw_b2, real_dir, real_var, spread, 
     *        theta, temp_a1, temp_a2

c-- changes magnetic degrees variation to real radians degrees.
            real_var = to_radians(real(DU_magnetic_var))

            a1_temp = (1.-spread**2/2.)*cos(to_radians(theta-DU_magnetic_var))
            b1_temp = (1.-spread**2/2.)*sin(to_radians(theta-DU_magnetic_var))

            a1 = a1_temp*cos(real_var) - b1_temp*sin(real_var)
            b1 = a1_temp*sin(real_var) + b1_temp*cos(real_var)

            two_theta_rotate = 2. * atan2(b1,a1)		!*atan2(b1,a1)= theta_mean

c-- dw_a2 and dw_b2 are datawell given values 
            a2 = dw_a2*cos(two_theta_rotate) - dw_b2*sin(two_theta_rotate)
            b2 = dw_a2*sin(two_theta_rotate) + dw_b2*cos(two_theta_rotate)

          end subroutine 


        end
