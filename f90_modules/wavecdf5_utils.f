c-- WAVECDF5_UTILS -------------------------------------------------------------
c
c   Utility functions for higher-level handling of wc5_datasets
c-------------------------------------------------------------------------------
      module wavecdf5_utils

      use dates
      use gudb_deploy_info
      use metadata_utils
      use wavecdf5_data

      implicit none

      save 

      contains


c-- WC5U_LOAD_TIMESPAN_DATA ----------------------------------------------------
c  Creates a wc5_dataset loaded with the requested data type for the given id
c  (stn id, mop id, etc.). Error codes: 1001=no datasets found, 1002=no records
c-------------------------------------------------------------------------------
        subroutine wc5u_load_timespan_data(id, tspan, qc_on, forecast_mode, groups, oset, errcode,
     *               min_freq_arg, max_freq_arg, opendap)
          integer             errcode, groups, grp_check, i, nccount, str_start
          real                min_freq, max_freq
          real,optional::     min_freq_arg, max_freq_arg
          logical             forecast_mode, mop_mode, need_xy, opendap_mode, qc_on
          logical,optional::  opendap
          character*100       datapath, id, ncfiles(100), ncname
          type(time_span)     tspan
          type(wc5_dataset)   fset, mk3set, oset, wset

          datapath = '/project/WNC/WNC_DATA/'
          if (PRESENT(min_freq_arg)) then
            min_freq = min_freq_arg
          else
            min_freq = 0.0
          end if
          if (PRESENT(max_freq_arg)) then
            max_freq = max_freq_arg
          else
            max_freq = 10.0
          end if
          opendap_mode = .false.
          if (PRESENT(opendap)) then
            if (opendap .eqv. .true.) opendap_mode = .true.
          end if

          if (BTEST(groups, WC5_xyz_bit)) then
            need_xy = .true.
          else
            need_xy = .false.
          end if

          call wc5u_compile_dataset_list(id, tspan, qc_on, need_xy, forecast_mode, ncfiles, nccount)
          if (nccount .lt. 1) then
            errcode = 1001
            return
          end if

          if (opendap_mode) then
            datapath = 'https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/'
            call wc5u_opendap_dataset_list(ncfiles, nccount, datapath)
          end if

          call wc5_initialize_set(oset)
          do i = 1, nccount
            ncname = TRIM(datapath)//TRIM(ncfiles(i))
            if (qc_on) then
              call wc5_load_set(ncname, groups, wset, tspan, min_freq, max_freq, errcode)
              if (errcode .eq. 0) call wc5_apply_flags(wset, fset)
            else
              call wc5_load_set(ncname, groups, fset, tspan, min_freq, max_freq, errcode)
            end if
            if (errcode .eq. 0) then
              if (fset%wave%freq_count .eq. 100 .and. oset%wave%freq_count .eq. 64) then
                call wc5u_convert_mk4_to_mk3(fset, mk3set)
                call wc5_combine_replace_sets(oset, mk3set)
                call wc5_deallocate_set(mk3set)
              else if (fset%wave%freq_count .eq. 64 .and. oset%wave%freq_count .eq. 100) then
                call wc5u_convert_mk4_to_mk3(oset, mk3set)
                call wc5_deallocate_waves(oset)
                oset%wave%time_count = 0
                oset%wave%freq_count = 0
                call wc5_combine_replace_sets(oset, mk3set)
                call wc5_combine_replace_sets(oset, fset)
                call wc5_deallocate_set(mk3set)
              else
                call wc5_combine_replace_sets(oset, fset)
              end if
              call wc5_assign_grpids(oset, fset)
            end if
          end do

          grp_check = wc5_get_group_value_from_counts(oset)
          if (grp_check .lt. 1) then
            errcode = 1002
          else
            errcode = 0
          end if

          call wc5_deallocate_set(wset)
          call wc5_deallocate_set(fset)
        end subroutine


c-- WC5U_OPENDAP_DATASET_LIST --------------------------------------------------
c  Adjusts a dataset list so that the entries are opendap urls rather than
c  /project file names. Note that XARCHIVE and XREALTIME dsets are excluded.
c-------------------------------------------------------------------------------
        subroutine wc5u_opendap_dataset_list(ncfiles, nccount, datapath)
          integer             i, nccount, str_start, tmpcount
          logical             omit_file(100)
          character*100       datapath, ncfiles(100)

          omit_file = .false.
          do i = 1, nccount
            str_start = INDEX(ncfiles(i),'XARCHIVE')
            if (str_start .le. 0) str_start = INDEX(ncfiles(i),'XREALTIME')
            if (str_start .gt. 0) omit_file(i) = .true.
          end do

          tmpcount = 0
          do i = 1, nccount
            if (.not. omit_file(i)) then
              tmpcount = tmpcount + 1
              ncfiles(tmpcount) = ncfiles(i)
            end if
          end do
          nccount = tmpcount

          do i = 1, nccount
            str_start = INDEX(ncfiles(i),'ARCHIVE')
            if (str_start .gt. 0) ncfiles(i)(str_start:str_start+6) = 'archive'
            str_start = INDEX(ncfiles(i),'REALTIME')
            if (str_start .gt. 0) ncfiles(i)(str_start:str_start+7) = 'realtime'
            str_start = MAX(INDEX(ncfiles(i),'OWI'), INDEX(ncfiles(i),'NDBC'),
     *        INDEX(ncfiles(i),'WW3'), INDEX(ncfiles(i),'CFSR'))
            if (str_start .gt. 0) then
              str_start = INDEX(datapath,'/cdip/')
              datapath(str_start:str_start+9) = '/external/'
            end if
          end do
        end subroutine


c-- WC5U_CONVERT_MK4_TO_MK3 ----------------------------------------------------
c  Converts a DWR4 dataset to Mk3 format by restructuring the spectral layout
c  and moving system variables from the dwr4 group to a dwr group.
c-------------------------------------------------------------------------------
        subroutine wc5u_convert_mk4_to_mk3(mk4set, mk3set)
          type(wc5_dataset)   mk3set, mk4set, rebandset

            if (mk4set%dwr4%time_count .gt. 0) then
              call wc5u_fill_dwr_from_dwr4(mk4set)
              mk4set%gps%merit = .false.
              mk4set%gps%new_fix = .true.
              mk4set%gps%mod_ok = .true.
              mk4set%gps%flags = 3
            end if

            if (mk4set%wave%time_count .gt. 0 .and. mk4set%wave%freq_count .gt. 0) then
              call wc5_reband_mk4_to_mk3(mk4set, rebandset)
              mk4set%wave%time_count = 0
              mk4set%wave%freq_count = 0
              mk4set%source%file_count = 0
            end if

            call wc5_initialize_set(mk3set)
            call wc5_combine_replace_sets(mk3set, mk4set)
            call wc5_assign_logicals(mk3set, mk4set)
            rebandset%is_dwr = .true.
            call wc5_combine_replace_sets(mk3set, rebandset)
            mk3set%is_mk4 = .false.
            mk3set%is_dwr = .true.

            call wc5_deallocate_set(rebandset)
        end subroutine


c-------------------------------------------------------------------------------
c  WC5_FILL_DWR_FROM_DWR4 - routine for aggregating datasets, copies relevant 
c    fields from the dwr4 group into the dwr group
c-------------------------------------------------------------------------------
      subroutine wc5u_fill_dwr_from_dwr4(wset)
        type(wc5_dataset)    wset

        wset%dwr%time_count = wset%dwr4%time_count
        call wc5_allocate_set(wset)

        wset%dwr%times = wset%dwr4%times
        wset%dwr%src_index = wset%dwr4%src_index
        wset%dwr%wol = wset%dwr4%wol
        wset%dwr%za_off = wset%dwr4%za_off
        wset%dwr%xa_off = wset%dwr4%xa_off
        wset%dwr%ya_off = wset%dwr4%ya_off
        wset%dwr%orient = wset%dwr4%orient_mean
        wset%dwr%inclin = wset%dwr4%inclin_mean
        wset%dwr%batt = WC5_int_fill
      end subroutine



c-- WC5U_LOAD_TIMESPAN_1D_VARIABLE ---------------------------------------------
c  Loads the requested variable and flag (if present) for the given id
c  (stn id, mop id, etc.). Loads a max of one million records.
c  Error codes: 1001=no datasets found, 1002=no records
c-------------------------------------------------------------------------------
        subroutine wc5u_load_timespan_1d_variable(id, tspan, qc_on, forecast_mode, var_name, 
     *               time_vals, var_vals, flag_vals, unit_label, long_name, std_name, errcode, band)
          integer             dimids(3), errcode,  i, nccount, ncid, recs, recs_qc, time_count, varid, vdims, vtype
          integer             dvarid, eindex, fband, fvarid, recs_add, sindex, tmp_times(1000000)
          integer             counts(1), counts_2d(2), starts(1), starts_2d(2)
          integer,optional::  band
          real                tmp_vals(1000000)
          byte                good_flag, tmp_flags(1000000)
          logical             var_2d, forecast_mode, found, load_flags, mop_mode, need_xy, qc_on
          character*100       datapath, dim_name, flag_list, flag_name, id, ncfiles(100), ncname, var_name
          character*100       long_name, std_name, unit_label
          type(time_span)     tspan
          integer,allocatable::  time_full(:), time_vals(:)
          real,allocatable::     var_add(:), var_vals(:)
          byte,allocatable::     flag_add(:), flag_vals(:)

          if (PRESENT(band)) then
            var_2d = .true.
            fband = band
          else
            var_2d = .false.
            fband = 0
          end if
          if (fband .le. 0) var_2d = .false.

          datapath = '/project/WNC/WNC_DATA/'
          need_xy = .false.

          call wc5u_compile_dataset_list(id, tspan, qc_on, need_xy, forecast_mode, ncfiles, nccount)
          if (nccount .lt. 1) then
            errcode = 1001
            return
          end if

          recs = 0
          load_flags = .false.
          tmp_flags = 0
          unit_label = '-'
          long_name = '-'
          std_name = '-'
          do i = 1, nccount
            ncname = TRIM(datapath)//TRIM(ncfiles(i))
            errcode = nf90_open(ncname, NF90_NOWRITE, ncid)
            if (errcode .eq. NF90_NOERR) then
              errcode = nf90_inq_varid(ncid, var_name, varid)
              if (errcode .eq. NF90_NOERR) then
                call nc_call_func(nf90_inquire_variable(ncid, varid, var_name, vtype, vdims, dimids))
                if (vdims .ne. 1 .and. (vdims .ne. 2 .or. .not. var_2d)) then
                  errcode = 1010
                  return
                end if
                if (unit_label .eq. '-') then
                  errcode = nf90_get_att(ncid, varid, 'units', unit_label)
                  if (errcode .ne. NF90_NOERR) unit_label = '-'
                end if
                if (long_name .eq. '-') then
                  errcode = nf90_get_att(ncid, varid, 'long_name', long_name)
                  if (errcode .ne. NF90_NOERR) long_name = '-'
                end if
                if (std_name .eq. '-') then
                  errcode = nf90_get_att(ncid, varid, 'standard_name', std_name)
                  if (errcode .ne. NF90_NOERR) std_name = '-'
                end if
                errcode = nf90_get_att(ncid, varid, 'ancillary_variables', flag_list)
                if (errcode .eq. NF90_NOERR) then
                  load_flags = .true.
                  flag_name = flag_list(1:INDEX(flag_list,' ')-1)
                  call nc_call_func(nf90_inq_varid(ncid, flag_name, fvarid))
                end if
                if (var_2d) then
                  call nc_call_func(nf90_inquire_dimension(ncid, dimids(2), dim_name, time_count))
                else
                  call nc_call_func(nf90_inquire_dimension(ncid, dimids(1), dim_name, time_count))
                end if
                allocate(time_full(time_count))
                call nc_call_func(nf90_inq_varid(ncid, dim_name, dvarid))
                call nc_call_func(nf90_get_var(ncid, dvarid, time_full))
                call nc_find_time_index(time_full, time_count, tspan%start, 3, sindex, found)
                if (found) call nc_find_time_index(time_full, time_count, tspan%end, 2, eindex, found)
                if (found .and. eindex .ge. sindex) then
                  recs_add = (eindex - sindex) + 1
                  allocate(var_add(recs_add), flag_add(recs_add))
                  starts(1) = sindex
                  counts(1) = recs_add
                  if (var_2d) then
                    starts_2d(1) = fband
                    counts_2d(1) = 1
                    starts_2d(2) = sindex
                    counts_2d(2) = recs_add
                    call nc_call_func(nf90_get_var(ncid, varid, var_add, starts_2d, counts_2d))
                  else
                    call nc_call_func(nf90_get_var(ncid, varid, var_add, starts, counts))
                  end if
                  tmp_vals(recs+1:recs+recs_add) = var_add
                  tmp_times(recs+1:recs+recs_add) = time_full(sindex:eindex)
                  flag_add = 0
                  if (load_flags) call nc_call_func(nf90_get_var(ncid, fvarid, flag_add, starts, counts))
                  tmp_flags(recs+1:recs+recs_add) = flag_add
                  recs = recs + recs_add
                  deallocate(var_add, flag_add)
                end if
                deallocate(time_full)
              end if
            end if
          end do

          if (recs .lt. 1) then
            errcode = 1002
          else
            if (load_flags .and. qc_on) then
              if (var_name(1:3) .eq. 'gps') then
                good_flag = 3
              else
                good_flag = 1
              end if
              recs_qc = COUNT(tmp_flags .eq. good_flag)
              allocate(var_vals(recs_qc), time_vals(recs_qc), flag_vals(recs_qc))
              var_vals = PACK(tmp_vals, (tmp_flags .eq. good_flag))
              time_vals = PACK(tmp_times, (tmp_flags .eq. good_flag))
              flag_vals = PACK(tmp_flags, (tmp_flags .eq. good_flag))
            else
              allocate(var_vals(recs), time_vals(recs), flag_vals(recs))
              var_vals = tmp_vals(1:recs)
              time_vals = tmp_times(1:recs)
              flag_vals = tmp_flags(1:recs)
            end if
            errcode = 0
          end if

        end subroutine


c-- WC5U_COMPILE_DATASET_LIST --------------------------------------------------
c  Creates a list of wavecdf5 datasets corresponding to the given id (stn id,
c  mop id, etc.). Used by ndar, wnc_type_avail
c-------------------------------------------------------------------------------
        subroutine wc5u_compile_dataset_list(orig_id, tspan, qc_on, need_xy, forecast_mode, ncfiles, nccount)
          integer        errcode, i, j, nccount, nullunit
          logical        dep_mode, forecast_mode, hull_mode, mop_mode, need_xy, qc_on, stn_mode, tophat_mode
          character*2    county_prefixes(19)
          character*6    suffix
          character*100  id, ncfiles(100), ncname, orig_id, subdir
          type(di_deploy_set)     deploy_set
          type(time_span)         dspan, tspan

          data county_prefixes / 'D0', 'D1', 'OC', 'L0', 'L1', 'VE', 'B0', 'B1', 
     *      'SL', 'MO', 'SC', 'SM', 'SF', 'MA', 'SN', 'M0', 'M1', 'HU', 'DN' /

          nullunit = 90
          open(unit=nullunit, file='/dev/null')

c--   Identify relevant deployments, datasets in station mode

          id = orig_id
          stn_mode = .false.
          hull_mode = .false.
          tophat_mode = .false.
          mop_mode = .false.
          dep_mode = .false.
          if (LEN_TRIM(id) .eq. 3) then
            stn_mode = .true.
            id = TRIM(id)//'p1'
          else if (LEN_TRIM(id) .eq. 5) then
            if (IACHAR(id(1:1)) .gt. IACHAR('9')) then
              mop_mode = .true.
            else
              stn_mode = .true.
            end if
          else if (LEN_TRIM(id) .eq. 7 .and. id(1:1) .eq. 'S' .and. id(5:5) .eq. 'D') then
            dep_mode = .true.
          else if (LEN_TRIM(id) .eq. 6 .and. id(1:1) .eq. 'T') then
            tophat_mode = .true.
          else if (LEN_TRIM(id) .eq. 6 .and. id(1:1) .eq. 'H') then
            hull_mode = .true.
          end if
        
          if (stn_mode .or. tophat_mode .or. hull_mode) then
            if (stn_mode) then
              deploy_set = gdi_load_timespan_deployments(id(1:5), tspan, errcode, nullunit)
            else if (tophat_mode) then
              deploy_set = gdi_load_timespan_deployments(id(2:6), tspan, errcode, nullunit, tophat=.true.)
            else if (hull_mode) then
              deploy_set = gdi_load_timespan_deployments(id(2:6), tspan, errcode, nullunit, hull=.true.)
            endif
            if ((.not. stn_mode) .or. (.not. qc_on) .or. need_xy) then
              call make_deploy_set_filelist(deploy_set, ncfiles, nccount)
            else
              call make_historic_set_filelist(deploy_set, ncfiles, nccount)        
c-- Break out as subroutine
              i = 1
              do while (i .le. nccount)
                ncname = '/project/WNC/WNC_DATA/'//TRIM(ncfiles(i))
                call wc5u_check_time_coverage(ncname, dspan, errcode)
                if (errcode .eq. 0) then
                  if (.not. time_spans_overlap(tspan, dspan)) then
                    do j = i, nccount-1
                      ncfiles(j) = ncfiles(j+1)
                    end do
                    nccount = nccount - 1
                  end if
                end if
                i = i + 1
              end do
c-- End subroutine
            end if
          else if (mop_mode) then
            if (id(1:2) .eq. 'BP') then
              subdir = 'MODELS/MOP_validation/'
            else if (ANY(county_prefixes .eq. id(1:2))) then
              subdir = 'MODELS/MOP_alongshore/'
            else 
              subdir = 'MODELS/misc/'
            end if 
            if (forecast_mode) then
              nccount = 1
              ncfiles(1) = TRIM(subdir)//TRIM(id)//'_forecast.nc'
            else
              nccount = 2
              ncfiles(1) = TRIM(subdir)//TRIM(id)//'_hindcast.nc'
              ncfiles(2) = TRIM(subdir)//TRIM(id)//'_nowcast.nc'
            end if
          else if (dep_mode) then
            call wc5u_compile_active_deployment_dataset_list(id(2:4), id(6:7), need_xy, ncfiles, nccount)
          else
            nccount = 1
            ncfiles(1) = id
          end if

          if (need_xy) then
            do i = 1, nccount
              if (INDEX(ncfiles(i), '_rt.nc') .gt. 1) then
                do j = i, nccount - 1
                  ncfiles(j) = ncfiles(j+1)
                end do
                nccount = nccount - 1
              end if
            end do
          else
            do i = 1, nccount
              if (INDEX(ncfiles(i), '_xy.nc') .gt. 1) then
                do j = i, nccount - 1
                  ncfiles(j) = ncfiles(j+1)
                end do
                nccount = nccount - 1
              end if
            end do
          end if
          close(nullunit)
        end subroutine


c-- WC5U_COMPILE_ACTIVE_DEPLOYMENT_DATASET_LIST --------------------------------
c  Lists all possible datasets for an active deployment, including all states
c-------------------------------------------------------------------------------
        subroutine wc5u_compile_active_deployment_dataset_list(stn, dep, need_xy, ncfiles, nccount)
          integer        i, nccount
          logical        need_xy
          character*2    dep
          character*3    stn
          character*6    suffix
          character*100  ncfiles(100)

          nccount = 5
          if (need_xy) then
            suffix = '_xy.nc'
          else
            suffix = '_rt.nc'
          end if
          ncfiles(1) = 'PREDEPLOY/'//stn//'p0_d'//dep//suffix
          ncfiles(2) = 'MOORED/'//stn//'p1_d'//dep//suffix
          ncfiles(3) = 'ARCHIVE/'//stn//'p1/'//stn//'p1_d'//dep//'.nc'
          ncfiles(4) = 'OFFSITE/'//stn//'p2_d'//dep//suffix
          ncfiles(5) = 'RECOVERED/'//stn//'p3_d'//dep//suffix
        end subroutine


c-- WC5U_GET_LATEST_ACTIVE_DEPLOYMENT_DATASET_NAME -----------------------------
c  Returns the name of the latest dataset for a deployment, checking all states.
c-------------------------------------------------------------------------------
        character*100 function wc5u_get_latest_active_deployment_dataset_name(stn, dep, need_xy)
          integer        i, nccount
          logical        exists, need_xy
          character*2    dep
          character*3    stn
          character*100  full_name, ncfiles(100)

          wc5u_get_latest_active_deployment_dataset_name = 'NULL'
          call wc5u_compile_active_deployment_dataset_list(stn, dep, need_xy, ncfiles, nccount)
          do i = nccount, 1, -1
            full_name = '/project/WNC/WNC_DATA/'//TRIM(ncfiles(i))
            inquire(file=full_name,exist=exists)
            if (exists) then
              wc5u_get_latest_active_deployment_dataset_name = ncfiles(i) 
              return
            end if
          end do
        end function


c-- WC5U_LOAD_STATION_METADATA -------------------------------------------------
c  Loads the station name, location, and depth from a station dataset.
c-------------------------------------------------------------------------------
        subroutine wc5u_load_station_metadata(ncfile, stn_name, stn_loc, stn_depth, errcode)
          integer               errcode, errcode2, dimids(1), i, name_length, ncid, varid, vtype, vdims
          integer               counts(1), starts(1)
          real                  lat, lon, stn_depth
          character*100         ncfile, stn_name, vname
          character,allocatable::  char_vals(:)
          type(location)        stn_loc

          errcode = 0
          stn_depth = WC5_real_fill
          stn_loc = init_location(0.0, 0.0)
          stn_name = WC5_char_fill

          errcode = nf90_open(ncfile, NF90_NOWRITE, ncid)
          if (errcode .eq. 0) then
            errcode = nf90_inq_varid(ncid, 'metaStationName', varid)
            if (errcode .ne. 0) errcode = nf90_inq_varid(ncid, 'metaSiteLabel', varid)
            if (errcode .eq. 0) then       
              call nc_call_func(nf90_inquire_variable(ncid, varid, vname, vtype, vdims, dimids))
              call nc_call_func(nf90_inquire_dimension(ncid, dimids(1), vname, name_length))
              allocate(char_vals(name_length))
              starts(1) = 1
              counts(1) = name_length
              call nc_call_func(nf90_get_var(ncid, varid, char_vals, starts, counts))
              stn_name = ''
              do i = 1, name_length
                stn_name(i:i) = char_vals(i)
              end do
            end if
            errcode = nf90_inq_varid(ncid, 'metaStationLatitude', varid)
            if (errcode .ne. 0) errcode = nf90_inq_varid(ncid, 'metaDeployLatitude', varid)
            if (errcode .ne. 0) errcode = nf90_inq_varid(ncid, 'metaLatitude', varid)
            if (errcode .eq. 0) then
              call nc_call_func(nf90_get_var(ncid, varid, lat))
              errcode = nf90_inq_varid(ncid, 'metaStationLongitude', varid)
              if (errcode .ne. 0) errcode = nf90_inq_varid(ncid, 'metaDeployLongitude', varid)
              if (errcode .ne. 0) errcode = nf90_inq_varid(ncid, 'metaLongitude', varid)
              if (errcode .eq. 0) then
                call nc_call_func(nf90_get_var(ncid, varid, lon))
                stn_loc = init_location(lat, lon)
              end if
            end if
            errcode = nf90_inq_varid(ncid, 'metaWaterDepth', varid)
            if (errcode .eq. 0) call nc_call_func(nf90_get_var(ncid, varid, stn_depth))
          end if
          errcode2 = nf90_close(ncid)
          return
        end subroutine


c-- WC5U_CHECK_TIME_COVERAGE ---------------------------------------------------
c  Checks the start and end metadata in a wc5 file, returns as a time_span
c-------------------------------------------------------------------------------
        subroutine wc5u_check_time_coverage(ncfile, tspan, errcode)
          integer               errcode, errcode2, ncid
          character*20          timestr
          character*100         ncfile
          type(date_block)      etime, stime
          type(time_span)       tspan

          errcode = 0
          errcode = nf90_open(ncfile, NF90_NOWRITE, ncid)
          if (errcode .eq. 0) then
            errcode = nf90_get_att(ncid, NF90_GLOBAL, 'time_coverage_start', timestr)
            if (errcode .eq. 0) then
              stime = parse_iso_8601_date(timestr)
              errcode = nf90_get_att(ncid, NF90_GLOBAL, 'time_coverage_end', timestr)
              if (errcode .eq. 0) then
                etime = parse_iso_8601_date(timestr)
                tspan = init_time_span(stime, etime)
              end if
            end if
          end if
          errcode2 = nf90_close(ncid)
          return
        end subroutine


c-- WC5U_YEAR_AVAIL_STRING -----------------------------------------------------
c  Returns a comma-delimited string of years for which data are available. 
c  Status flags are not checked; list may include years without good records.
c-------------------------------------------------------------------------------
        subroutine wc5u_year_avail_string(id, year_str)
          integer               curr_year, errcode, i, last_year, nccount
          character*100         id, ncname, ncfiles(100)
          character*500         year_str
          type(time_span)       tspan

          tspan = init_time_span(init_date(1970,1,1,0,0,0), init_date(2037,1,1,0,0,0))
          call wc5u_compile_dataset_list(id, tspan, .false., .false., .false., ncfiles, nccount)
          year_str = ''
          last_year = -1
          do i = 1, nccount
            ncname = '/project/WNC/WNC_DATA/'//TRIM(ncfiles(i))
            call wc5u_check_time_coverage(ncname, tspan, errcode)
            if (errcode .eq. 0) then
              do curr_year = tspan%start%year, tspan%end%year
                if (curr_year .ne. last_year) then
                  if (last_year .ne. -1) year_str = TRIM(year_str)//','
                  write(year_str,'(a,i4)') TRIM(year_str), curr_year
                  last_year = curr_year
                end if
              end do
            end if
          end do
          return
        end subroutine


c-- WC5U_MONTH_AVAIL_STRING ----------------------------------------------------
c  Returns a comma-delimited string of months for which data are available in
c  given year. Status flags are not checked; list may include months without 
c  good records.
c-------------------------------------------------------------------------------
        subroutine wc5u_month_avail_string(id, yr, month_str)
          integer               curr_mo, end_mo, errcode, i, last_mo, nccount, start_mo, yr
          character*100         id, ncname, ncfiles(100)
          character*500         month_str
          type(time_span)       tspan

          tspan = init_time_span(init_date(yr,1,1,0,0,0), init_date(yr,12,31,23,59,59))
          call wc5u_compile_dataset_list(id, tspan, .false., .false., .false., ncfiles, nccount)
          month_str = ''
          last_mo = -1
          do i = 1, nccount
            ncname = '/project/WNC/WNC_DATA/'//TRIM(ncfiles(i))
            call wc5u_check_time_coverage(ncname, tspan, errcode)
            if (errcode .eq. 0) then
              if (tspan%start%year .lt. yr) then
                start_mo = 1
              else if (tspan%start%year .eq. yr) then
                start_mo = tspan%start%month
              else
                start_mo = 13
              end if
              if (tspan%end%year .gt. yr) then
                end_mo = 12
              else if (tspan%end%year .eq. yr) then
                end_mo = tspan%end%month
              else
                end_mo = 0
              end if

              do curr_mo = start_mo, end_mo
                if (curr_mo .gt. last_mo) then
                  if (last_mo .ne. -1) month_str = TRIM(month_str)//','
                  write(month_str,'(a,i02.2)') TRIM(month_str), curr_mo
                  last_mo = curr_mo
                end if
              end do
            end if
          end do
          return
        end subroutine


c-- WC5U_COPY_METADATA ---------------------------------------------------------
c  Copies the meta variables and global atts from one file to another
c-------------------------------------------------------------------------------
        subroutine wc5u_copy_metadata(ncsource, ncdest, errcode)
          integer               errcode, ncid1, ncid2
          character*100         ncsource, ncdest, prefix

          call nc_call_func(nf90_open(ncsource, NF90_NOWRITE, ncid1))
          call nc_call_func(nf90_open(ncdest, NF90_WRITE, ncid2))

          prefix = '/meta'
          call nc_copy_variables(ncid1, ncid2, prefix, errcode)
          call nc_call_func(nf90_redef(ncid2))
          call nc_copy_global_atts(ncid1, ncid2, errcode)

          call nc_call_func(nf90_close(ncid1))
          call nc_call_func(nf90_close(ncid2))
        end subroutine


c-- WC5U_WINDOW_WAVES ----------------------------------------------------------
c  Averages wave records using the given window size (in seconds); output
c  times are assigned to the top of the hour.
c  NOTE: all records flagged good.
c-------------------------------------------------------------------------------
        subroutine wc5u_window_waves(iset, oset, window_size, verbose)

        integer             count, end_stamp, eloc, i, j, k, l, rec_count, sloc, start_stamp, window_size
        real                cos_combo, sin_combo
        logical             be_verbose, efound, sfound
        logical,optional::  verbose
        type(date_block)    end_date, start_date
        type(wc5_dataset)   iset, oset

          if (PRESENT(verbose)) then
            be_verbose = verbose
          else
            be_verbose = .false.
          end if

          start_stamp = iset%wave%times(1) - MOD(iset%wave%times(1),3600)
          end_stamp = iset%wave%times(iset%wave%time_count) - MOD(iset%wave%times(iset%wave%time_count),3600)
          if (MOD(iset%wave%times(iset%wave%time_count),3600) .gt. 1800) end_stamp = end_stamp + 3600

          rec_count = 0
          do i = start_stamp, end_stamp, 3600
            if (MINVAL(ABS(iset%wave%times - i)) .le. window_size/2) rec_count = rec_count + 1
          end do
          if (be_verbose) write(6,*) ' window_waves() rec count: ', rec_count
          if (rec_count .le. 0) return

          call wc5_initialize_set(oset)
          oset%groups = WC5_include_wave_params + WC5_include_wave_spectra
          oset%wave%time_count = rec_count
          oset%wave%freq_count = iset%wave%freq_count
          if (iset%is_2d_model) then
            oset%wave%dir_count = iset%wave%dir_count
            oset%is_2d_model = .true.
          end if
          call wc5_allocate_set(oset)

          oset%wave%freqs = iset%wave%freqs
          oset%wave%bw = iset%wave%bw
          if (iset%is_2d_model) oset%wave%dirs = iset%wave%dirs

          rec_count = 0
          do k = start_stamp, end_stamp, 3600
            start_date = timestamp_to_date(k - window_size/2)
            end_date = timestamp_to_date(k + window_size/2 - 1)
            call nc_find_time_index(iset%wave%times, iset%wave%time_count, start_date, 3, sloc, sfound)
            call nc_find_time_index(iset%wave%times, iset%wave%time_count, end_date, 2, eloc, efound)
            if (sfound .and. efound .and. sloc .le. eloc) then
              rec_count = rec_count + 1
              oset%wave%times(rec_count) = k

              do i = 1, oset%wave%freq_count
                oset%wave%a0(i,rec_count) = 0.0
                oset%wave%a1(i,rec_count) = 0.0
                oset%wave%a2(i,rec_count) = 0.0
                oset%wave%b1(i,rec_count) = 0.0
                oset%wave%b2(i,rec_count) = 0.0
                oset%wave%check(i,rec_count) = 0.0
                if (oset%is_2d_model) then
                  do l = 1, oset%wave%dir_count 
                    oset%wave%dirspec(l,i,rec_count) = 0.0
                  end do
                end if
                sin_combo = 0.0
                cos_combo = 0.0

c--  Directional coeffs must be unnormalized, merged

                do j = sloc, eloc
                  oset%wave%a0(i,rec_count) = oset%wave%a0(i,rec_count) + iset%wave%a0(i,j)
                  oset%wave%a1(i,rec_count) = oset%wave%a1(i,rec_count) + iset%wave%a0(i,j)*iset%wave%a1(i,j)
                  oset%wave%a2(i,rec_count) = oset%wave%a2(i,rec_count) + iset%wave%a0(i,j)*iset%wave%a2(i,j)
                  oset%wave%b1(i,rec_count) = oset%wave%b1(i,rec_count) + iset%wave%a0(i,j)*iset%wave%b1(i,j)
                  oset%wave%b2(i,rec_count) = oset%wave%b2(i,rec_count) + iset%wave%a0(i,j)*iset%wave%b2(i,j)
                  oset%wave%check(i,rec_count) = oset%wave%check(i,rec_count) + iset%wave%a0(i,j)*iset%wave%check(i,j)
                  sin_combo = sin_combo + iset%wave%a0(i,j) * SIN(to_radians(iset%wave%mdir(i,j)))
                  cos_combo = cos_combo + iset%wave%a0(i,j) * COS(to_radians(iset%wave%mdir(i,j)))
                  if (oset%is_2d_model) then
                    do l = 1, oset%wave%dir_count 
                      oset%wave%dirspec(l,i,rec_count) = oset%wave%dirspec(l,i,rec_count) + iset%wave%dirspec(l,i,j)
                      if (be_verbose .and. iset%wave%dirspec(l,i,j) .lt. 0) write(6,*) iset%wave%dirspec(l,i,j)
                    end do
                  end if
                end do

c--  Now do energy and renormalize directional coeff.

                if (oset%wave%a0(i,rec_count) .gt. 0.0) then
                  count = eloc - sloc + 1
                  oset%wave%a0(i,rec_count) = oset%wave%a0(i,rec_count) / count
                  oset%wave%a1(i,rec_count) = oset%wave%a1(i,rec_count) / count / oset%wave%a0(i,rec_count)
                  oset%wave%a2(i,rec_count) = oset%wave%a2(i,rec_count) / count / oset%wave%a0(i,rec_count)
                  oset%wave%b1(i,rec_count) = oset%wave%b1(i,rec_count) / count / oset%wave%a0(i,rec_count)
                  oset%wave%b2(i,rec_count) = oset%wave%b2(i,rec_count) / count / oset%wave%a0(i,rec_count)
                  oset%wave%check(i,rec_count) = oset%wave%check(i,rec_count) / count / oset%wave%a0(i,rec_count)
                  if (oset%is_2d_model) then
                    do l = 1, oset%wave%dir_count 
                      oset%wave%dirspec(l,i,rec_count) = oset%wave%dirspec(l,i,rec_count) / count
                    end do
                  end if

c--  Last recalculate direction

                  oset%wave%mdir(i,rec_count) = to_degrees(ATAN2(sin_combo,cos_combo))
                  if (oset%wave%mdir(i,rec_count) .lt. 0) oset%wave%mdir(i,rec_count) = oset%wave%mdir(i,rec_count)+360.
                else
                  oset%wave%mdir(i,rec_count) = WC5_real_fill
                end if
              end do
              if (be_verbose) write(6,*) '  completed rec ', rec_count
            end if
          end do

          oset%wave%flags = 1
          oset%wave%flags2 = 0
          oset%wave%fflags = 1
          oset%wave%fflags2 = 0
          call wc5_assign_logicals(oset, iset)
          call wc5_recalculate_params(oset, .true.)
        end subroutine


c-- WC5U_MATCH_WAVE_TIMES_TRIM -------------------------------------------------
c   Trims off records from sets that don't have a matching time. New sets
c   contain wave data only.
c-------------------------------------------------------------------------------
        subroutine wc5u_match_wave_times_trim(iset1, iset2, oset1, oset2, errcode)

        integer             count, errcode, i, idx1, idx2
        type(wc5_dataset)   iset1, iset2, oset1, oset2

       
          count = 0
          do i = 1, iset1%wave%time_count
            if (ANY(iset2%wave%times .eq. iset1%wave%times(i))) count = count + 1
          end do

          if (count .eq. 0) then
            errcode = 1
            return
          else
            idx1 = 1
            idx2 = 1
            do while (idx1 .le. iset1%wave%time_count .and. idx2 .le. iset2%wave%time_count)
              if (iset1%wave%times(idx1) .lt. iset2%wave%times(idx2)) then
                iset1%wave%flags(idx1) = 4
                idx1 = idx1 + 1
              else if (iset2%wave%times(idx2) .lt. iset1%wave%times(idx1)) then
                iset2%wave%flags(idx2) = 4
                idx2 = idx2 + 1
              else
                if (iset1%wave%flags(idx1) .ne. iset2%wave%flags(idx2)) then
                  iset1%wave%flags(idx1) = 4
                  iset2%wave%flags(idx2) = 4
                end if
                idx1 = idx1 + 1
                idx2 = idx2 + 1
              end if
            end do

            do i = idx1, iset1%wave%time_count
              iset1%wave%flags(i) = 4
            end do
            do i = idx2, iset2%wave%time_count
              iset2%wave%flags(i) = 4
            end do

            call wc5_apply_flags(iset1, oset1)
            call wc5_apply_flags(iset2, oset2)
            errcode = 0
          end if
        end subroutine


c-- WC5U_MATCH_WAVE_TIMES_FILL -------------------------------------------------
c   Matches times in the second set to the first by trimming extra times and
c   adding in missing times, with missing data markers and flag set to 'missing'
c   New set contains wave data only.
c-------------------------------------------------------------------------------
        subroutine wc5u_match_wave_times_fill(bset, oset, nset)

        integer             count, errcode, i, oidx, bidx
        logical             continue_loop
        type(wc5_dataset)   bset, nset, oset

       
          count = bset%wave%time_count
          call wc5_initialize_set(nset, groups=WC5_include_wave_params+WC5_include_wave_spectra)
          nset%wave%time_count = count
          nset%wave%freq_count = bset%wave%freq_count
          call wc5_allocate_set(nset)
          nset%wave%freqs = oset%wave%freqs
          nset%wave%bw = oset%wave%bw
          nset%wave%fflags = oset%wave%fflags
          nset%wave%fflags2 = oset%wave%fflags2
          call wc5_empty_wave_vars(nset)
          call wc5_empty_spectral_vars(nset)
          nset%wave%times = oset%wave%times

          oidx = 1
          do bidx = 1, count
            continue_loop = .false.
            if (oidx .le. oset%wave%time_count) then
              if (oset%wave%times(oidx) .lt. bset%wave%times(bidx)) continue_loop = .true.
            end if
            do while (continue_loop)
              oidx = oidx + 1
              continue_loop = .false.
              if (oidx .le. oset%wave%time_count) then
                if (oset%wave%times(oidx) .lt. bset%wave%times(bidx)) continue_loop = .true.
              end if
            end do
            if (oidx .le. oset%wave%time_count) then
              if (oset%wave%times(oidx) .eq. bset%wave%times(bidx)) then
                call wc5u_copy_wave_record(oset, nset, oidx, bidx)
                oidx = oidx + 1
              end if
            end if
          end do
          
        end subroutine


c-- WC5U_STANDARDIZE_TIME_STEP -------------------------------------------------
c   Copies a dataset into one with a standard time step across the given 
c   timespan, filling with missing data markers where necessary.
c   New set contains wave data only.
c-------------------------------------------------------------------------------
        subroutine wc5u_standardize_time_step(iset, tspan, tstep, nset)

        integer             count, errcode, i, inx, newx, start_tstamp, tstep
        type(time_span)     tspan
        type(wc5_dataset)   iset, nset

          count = secs_diff(tspan%start, tspan%end)/tstep + 1
          call wc5_initialize_set(nset, groups=WC5_include_wave_params+WC5_include_wave_spectra)
          nset%wave%time_count = count
          nset%wave%freq_count = iset%wave%freq_count
          call wc5_allocate_set(nset)
          nset%wave%freqs = iset%wave%freqs
          nset%wave%bw = iset%wave%bw
          nset%wave%fflags = iset%wave%fflags
          nset%wave%fflags2 = iset%wave%fflags2
          call wc5_empty_wave_vars(nset)
          call wc5_empty_spectral_vars(nset)

          inx = 1
          start_tstamp = date_to_timestamp(tspan%start)
          do newx = 1, count
            nset%wave%times(newx) = start_tstamp + (newx-1)*tstep
            do while (iset%wave%times(inx) .lt. nset%wave%times(newx) .and. inx .lt. iset%wave%time_count)
              inx = inx + 1
            end do
            if (iset%wave%times(inx) .eq. nset%wave%times(newx)) then
              call wc5u_copy_wave_record(iset, nset, inx, newx)
              if (inx .lt. iset%wave%time_count) inx = inx + 1
            end if
          end do
          
        end subroutine


c-- WC5U_COPY_WAVE_RECORD ------------------------------------------------------
c   Copies a record from one set to another, covering all wave vars
c-------------------------------------------------------------------------------
        subroutine wc5u_copy_wave_record(oset, nset, oidx, nidx)
          integer             oidx, nidx
          type(wc5_dataset)   nset, oset

          nset%wave%flags(nidx) = oset%wave%flags(oidx)
          nset%wave%flags2(nidx) = oset%wave%flags2(oidx)
          nset%wave%hs(nidx) = oset%wave%hs(oidx)
          nset%wave%tp(nidx) = oset%wave%tp(oidx)
          nset%wave%dp(nidx) = oset%wave%dp(oidx)
          nset%wave%ta(nidx) = oset%wave%ta(oidx)
c         nset%wave%model_input(nidx) = oset%wave%model_input(oidx)
          nset%wave%dm(nidx) = oset%wave%dm(oidx)
          nset%wave%sxy(nidx) = oset%wave%sxy(oidx)
          nset%wave%sxx(nidx) = oset%wave%sxx(oidx)
          nset%wave%src_index(nidx) = oset%wave%src_index(oidx)
          nset%wave%psdmax(nidx) = oset%wave%psdmax(oidx)
          nset%wave%spreadmax(nidx) = oset%wave%spreadmax(oidx)
          nset%wave%tint(nidx) = oset%wave%tint(oidx)
          nset%wave%tener(nidx) = oset%wave%tener(oidx)
          nset%wave%tm13(nidx) = oset%wave%tm13(oidx)
          nset%wave%tcrest(nidx) = oset%wave%tcrest(oidx)
          nset%wave%iqp(nidx) = oset%wave%iqp(oidx)
          nset%wave%tz(nidx) = oset%wave%tz(oidx)

          nset%wave%a0(:,nidx) = oset%wave%a0(:,oidx)
          nset%wave%mdir(:,nidx) = oset%wave%mdir(:,oidx)
          nset%wave%a1(:,nidx) = oset%wave%a1(:,oidx)
          nset%wave%b1(:,nidx) = oset%wave%b1(:,oidx)
          nset%wave%a2(:,nidx) = oset%wave%a2(:,oidx)
          nset%wave%b2(:,nidx) = oset%wave%b2(:,oidx)
          nset%wave%check(:,nidx) = oset%wave%check(:,oidx)
          nset%wave%dspread(:,nidx) = oset%wave%dspread(:,oidx)
          nset%wave%m2(:,nidx) = oset%wave%m2(:,oidx)
          nset%wave%n2(:,nidx) = oset%wave%n2(:,oidx)
          if (ALLOCATED(nset%wave%dirspec)) nset%wave%dirspec(:,:,nidx) = oset%wave%dirspec(:,:,oidx)
        end subroutine


c-- WC5U_GET_EARLIEST_OBS_TIME -------------------------------------------------
c   Returns the earliest observation timestamp in the dataset
c-------------------------------------------------------------------------------
        integer function wc5u_get_earliest_obs_time(wset)
          integer             min_tstamp
          type(wc5_dataset)   wset

          min_tstamp = HUGE(min_tstamp)
          if (wset%wave%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%wave%times(1))
          if (wset%sst%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%sst%times(1))
          if (wset%gps%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%gps%times(1))
          if (wset%acm%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%acm%times(1))
          if (wset%dwr4%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%dwr4%times(1))
          if (wset%dwr%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%dwr%times(1))
          if (wset%upcross%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%upcross%times(1))
          if (wset%cat4%time_count .ge. 1) min_tstamp = MIN(min_tstamp, wset%cat4%times(1))

          wc5u_get_earliest_obs_time = min_tstamp 
        end function 

       
c-- WC5U_GET_LATEST_OBS_TIME -------------------------------------------------
c   Returns the latest observation timestamp in the dataset
c-------------------------------------------------------------------------------
        integer function wc5u_get_latest_obs_time(wset)
          integer             max_tstamp
          type(wc5_dataset)   wset

          max_tstamp = -1
          if (wset%wave%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%wave%times(wset%wave%time_count))
          if (wset%sst%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%sst%times(wset%sst%time_count))
          if (wset%gps%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%gps%times(wset%gps%time_count))
          if (wset%acm%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%acm%times(wset%acm%time_count))
          if (wset%dwr4%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%dwr4%times(wset%dwr4%time_count))
          if (wset%dwr%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%dwr%times(wset%dwr%time_count))
          if (wset%upcross%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%upcross%times(wset%upcross%time_count))
          if (wset%cat4%time_count .ge. 1) max_tstamp = MAX(max_tstamp, wset%cat4%times(wset%cat4%time_count))

          wc5u_get_latest_obs_time = max_tstamp 
        end function 

       
      end !* END MODULE
