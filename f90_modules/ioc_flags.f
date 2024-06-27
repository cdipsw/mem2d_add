c-- IOC_FLAGS ------------------------------------------------------------------
c
c   Sets attributes for IOC flag variables in netCDF datasets.
c-------------------------------------------------------------------------------
      module ioc_flags

      use netcdf_utils

      implicit none

      save 

      integer,parameter::  IOC_primary_flag_count = 5, 
     *  IOC_wave_flag_count = 19, 
     *  IOC_sst_flag_count = 8, 
     *  IOC_cat4_flag_count = 9, 
     *  IOC_wavefreq_flag_count = 3, 
     *  IOC_xyz_flag_count = 5,
     *  IOC_acm_flag_count = 15,
     *  IOC_upcross_flag_count = 13,
     *  IOC_zcross_flag_count = 18,
     *  IOC_model_flag_count = 3,
     *  IOC_warning_count = 5

      byte::  IOC_primary_flag_vals(IOC_primary_flag_count), 
     *  IOC_wave_flag_vals(IOC_wave_flag_count), 
     *  IOC_sst_flag_vals(IOC_sst_flag_count), 
     *  IOC_cat4_flag_vals(IOC_cat4_flag_count), 
     *  IOC_wavefreq_flag_vals(IOC_wavefreq_flag_count), 
     *  IOC_xyz_flag_vals(IOC_xyz_flag_count),
     *  IOC_acm_flag_vals(IOC_acm_flag_count),
     *  IOC_upcross_flag_vals(IOC_upcross_flag_count),
     *  IOC_zcross_flag_vals(IOC_zcross_flag_count),
     *  IOC_model_flag_vals(IOC_model_flag_count)

      character*100::  IOC_primary_flags(IOC_primary_flag_count), 
     *  IOC_wave_flags(IOC_wave_flag_count), 
     *  IOC_sst_flags(IOC_sst_flag_count), 
     *  IOC_cat4_flags(IOC_cat4_flag_count), 
     *  IOC_wavefreq_flags(IOC_wavefreq_flag_count), 
     *  IOC_xyz_flags(IOC_xyz_flag_count),
     *  IOC_acm_flags(IOC_acm_flag_count),
     *  IOC_upcross_flags(IOC_upcross_flag_count),
     *  IOC_zcross_flags(IOC_zcross_flag_count),
     *  IOC_model_flags(IOC_model_flag_count),
     *  IOC_warnings(IOC_warning_count)

      data IOC_primary_flag_vals /1, 2, 3, 4, 9/
      data IOC_primary_flags /'good', 'not_evaluated', 'questionable', 'bad',
     *                        'missing'/

      data IOC_wave_flag_vals /0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18/
      data IOC_wave_flags /'unspecified', 'sensor_issues', 'Hs_out_of_range',
     *  'Tp_out_of_range', 'Ta_out_of_range', 'elevated_check_factors', 
     *  'Hs_spike', 'Ta_spike', 'low_freq_energy_spike', 'excessive_low_freq_energy',
     *  'hf_transmission_errors_fixed', 'hf_transmission_errors_present',
     *  'directional_coeffs_out_of_range', 'incomplete_spectrum', 'spectrum_layout_modified',
     *  'too_few_segments', 'inclination_off', 'max_energy_at_highest_freq', 'freq_over_hull_response_limit' /

      data IOC_sst_flag_vals /0, 1, 2, 3, 4, 5, 6, 7/
      data IOC_sst_flags /'unspecified', 'sensor_issues', 'SST_out_of_range', 
     *  'SST_spike', 'SST_max_change_exceeded', 'hf_transmission_errors_fixed', 
     *  'hf_transmission_errors_present', 'reference_temperature_off'/

      data IOC_cat4_flag_vals /0, 1, 2, 3, 4, 5, 6, 7, 8/
      data IOC_cat4_flags /'unspecified', 'sensor_issues', 'airT_out_of_range', 'airT_spike', 
     *  'airT_max_change_exceeded', 'solar_induced_uncertainty', 'evaporation_detected', 
     *  'hf_transmission_errors_fixed', 'hf_transmission_errors_present'/

      data IOC_wavefreq_flag_vals /0, 1, 2/
      data IOC_wavefreq_flags /'unspecified', 'out_of_range', 'above_hull_response_limit'/

      data IOC_xyz_flag_vals /0, 1, 2, 3, 4/
      data IOC_xyz_flags /'unspecified', 'hf_transmission_errors_fixed',
     *  'hs_transmission_errors_present', 'gps_gap_present', 'gps_gap_in_filter_window'/

      data IOC_acm_flag_vals /0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14/
      data IOC_acm_flags /'unspecified', 'sensor_issues', 'acm_failure', 'speed_out_of_range', 
     *  'speed_stddev_out_of_range', 'direction_stddev_out_of_range', 
     *  'signal_strength_low', 'vertical_speed_out_of_range', 'vert_speed_stddev_out_of_range',
     *  'hf_transmission_errors_fixed', 'hf_transmission_errors_present', 'elevated_buoy_speed',
     *  'buoy_speed_unknown', 'speed_spike', 'speed_max_change_exceeded'/

      data IOC_upcross_flag_vals /0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12/
      data IOC_upcross_flags /'unspecified', 'sensor_issues', 'low_coverage',
     *  'Hmax_out_of_range', 'Tmax_out_of_range', 'Hmax_spike', 'Tmax_spike',
     *  'hf_transmission_errors_fixed', 'hf_transmission_errors_present', 'freq_over_response_limit',
     *  'mean_wave_height_too_small', 'rogue_warning', 'rogue_limit_exceeded'/

      data IOC_zcross_flag_vals /0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17/
      data IOC_zcross_flags /'unspecified', 'sensor_issues', 'gaps_present', 'period_out_of_range',
     *  'height_out_of_range', 'horiz_motion_out_of_range', 'consecutive_values', 'max_accel_exceeded', 
     *  'too_few_waves_in_window', 'windowed_height_low', 'height_spike', 'period_spike', 
     *  'excessive_horiz_motion', 'peak_to_h3_ratio_bad', 'rogue_bad_direction', 'rogue_warning', 
     *  'rogue_limit_exceeded', 'in_filter_window_of_bad_value'/

      data IOC_model_flag_vals /0, 1, 2/
      data IOC_model_flags /'unspecified', 'insufficient_input', 'low_energy'/

      data IOC_warnings /'Hs_max_change_exceeded', 'Ta_max_change_exceeded', 
     *  'SST_max_change_exceeded', 'ACM_max_change_exceeded', 'CAT4_max_change_exceeded'/

      contains

c-- IOC_ASSIGN_FLAG1_ATTS ------------------------------------------------------
c  Assigns the standard attributes to an IOC primary flag variable
c-------------------------------------------------------------------------------
        subroutine ioc_assign_flag1_atts(grpid, varid, group_name)
          integer::                  grpid, varid
          byte                       fvalues(IOC_primary_flag_count)
          character*(*)              group_name
          character*100              longname, secondary
          character*1000             meanings

          fvalues = IOC_primary_flag_vals
          longname = 'primary '//TRIM(group_name)//' QC flag'
          if (TRIM(group_name) .eq. 'zcross') longname = 'primary zero-crossing QC flag'
          meanings = ioc_meaning_list(IOC_primary_flags)
          call nc_assign_attributes(grpid, varid, longname, 'NULL', 'BYTE', 'status_flag', minv=1.0, maxv=9.0) 
          call nc_call_func(nf90_put_att(grpid, varid, 'flag_values', fvalues))
          call nc_call_func(nf90_put_att(grpid, varid, 'flag_meanings', TRIM(meanings)))
          call nc_call_func(nf90_put_att(grpid, varid, 'reference', 
     *      'Ocean Data Standards, UNESCO 2013 - IOC Manuals and Guides, 54, Volume 3 Version 1'))
          secondary = TRIM(group_name)//'FlagSecondary'
          call nc_call_func(nf90_put_att(grpid, varid, 'ancillary_variables', TRIM(secondary)))
          call nc_call_func(nf90_put_att(grpid, varid, 'coverage_content_type', NC_cctype_qinfo))
        end subroutine
          

c-- IOC_ASSIGN_FLAG2_ATTS ------------------------------------------------------
c  Assigns the standard atrributes to a secondary flag variable.
c-------------------------------------------------------------------------------
        subroutine ioc_assign_flag2_atts(grpid, varid, group_name)
          integer                   grpid, varid
          real                      max_val
          character*(*)             group_name
          character*1000            fmeanings, longname
          byte, allocatable::       fvalues(:)

          if (group_name .eq. 'wave') then
            ALLOCATE(fvalues(IOC_wave_flag_count))
            fvalues = IOC_wave_flag_vals
            max_val = IOC_wave_flag_vals(IOC_wave_flag_count)
            fmeanings = ioc_meaning_list(IOC_wave_flags)
          else if (group_name .eq. 'sst') then
            ALLOCATE(fvalues(IOC_sst_flag_count))
            fvalues =  IOC_sst_flag_vals
            max_val = IOC_sst_flag_vals(IOC_sst_flag_count)
            fmeanings = ioc_meaning_list(IOC_sst_flags)
          else if (group_name .eq. 'cat4') then
            ALLOCATE(fvalues(IOC_cat4_flag_count))
            fvalues =  IOC_cat4_flag_vals
            max_val = IOC_cat4_flag_vals(IOC_cat4_flag_count)
            fmeanings = ioc_meaning_list(IOC_cat4_flags)
          else if (group_name .eq. 'waveFrequency') then
            ALLOCATE(fvalues(IOC_wavefreq_flag_count))
            fvalues = IOC_wavefreq_flag_vals
            max_val = IOC_wavefreq_flag_vals(IOC_wavefreq_flag_count)
            fmeanings = ioc_meaning_list(IOC_wavefreq_flags)
          else if (group_name .eq. 'model') then
            ALLOCATE(fvalues(IOC_model_flag_count))
            fvalues = IOC_model_flag_vals
            max_val = IOC_model_flag_vals(IOC_model_flag_count)
            fmeanings = ioc_meaning_list(IOC_model_flags)
          else if (group_name .eq. 'xyz') then
            ALLOCATE(fvalues(IOC_xyz_flag_count))
            fvalues = IOC_xyz_flag_vals
            max_val = IOC_xyz_flag_vals(IOC_xyz_flag_count)
            fmeanings = ioc_meaning_list(IOC_xyz_flags)
          else if (group_name .eq. 'acm') then
            ALLOCATE(fvalues(IOC_acm_flag_count))
            fvalues = IOC_acm_flag_vals
            max_val = IOC_acm_flag_vals(IOC_acm_flag_count)
            fmeanings = ioc_meaning_list(IOC_acm_flags)
          else if (group_name .eq. 'upcross') then
            ALLOCATE(fvalues(IOC_upcross_flag_count))
            fvalues = IOC_upcross_flag_vals
            max_val = IOC_upcross_flag_vals(IOC_upcross_flag_count)
            fmeanings = ioc_meaning_list(IOC_upcross_flags)
          else if (group_name .eq. 'zcross') then
            ALLOCATE(fvalues(IOC_zcross_flag_count))
            fvalues = IOC_zcross_flag_vals
            max_val = IOC_zcross_flag_vals(IOC_zcross_flag_count)
            fmeanings = ioc_meaning_list(IOC_zcross_flags)
          else
            ALLOCATE(fvalues(1))
            fvalues = (/ 0 /)
            max_val = 0
            fmeanings = 'unspecified'
          end if

          longname = 'secondary '//TRIM(group_name)//' QC flag'
          if (TRIM(group_name) .eq. 'zcross') longname = 'secondary QC flag'
          call nc_assign_attributes(grpid, varid, longname, 'NULL', 'BYTE', minv=0.0, maxv=max_val) 
          call nc_call_func(nf90_put_att(grpid, varid, 'flag_values', fvalues))
          call nc_call_func(nf90_put_att(grpid, varid, 'flag_meanings', TRIM(fmeanings)))
          call nc_call_func(nf90_put_att(grpid, varid, 'reference', 'http://cdip.ucsd.edu/documentation'))
          call nc_call_func(nf90_put_att(grpid, varid, 'coverage_content_type', NC_cctype_qinfo))
        end subroutine


c-- IOC_MEANING_LIST -----------------------------------------------------------
c  Creates an attribute string with a list of the possible flag meanings
c-------------------------------------------------------------------------------
        character*1000 function ioc_meaning_list(flag_array)
          integer                   i
          character*100             flag_array(:)

          ioc_meaning_list = TRIM(flag_array(1))
          do i = 2, SIZE(flag_array)
            ioc_meaning_list = TRIM(ioc_meaning_list)//' '//TRIM(flag_array(i))
          end do
        end function


      end !* END MODULE
