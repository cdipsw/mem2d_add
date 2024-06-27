c-- GUDB_DEPLOY_INFO -----------------------------------------------------------
c   The gudb_deploy_info module is a extension of CDIP's old deploy_info module,
c   which handles 'deploy_log' files. It contains methods that assemble and 
c   manipulate deployment metadata taken from GUDB via the API (or from the 
c   cache when necessary) but is limited to the 'moored' state of buoys, to 
c   align with the 'deploy_log' files used in CDIP's early netCDF developement.
c-------------------------------------------------------------------------------
        module gudb_deploy_info

        use dates
        use deploy_info
        use gudb_objects

        implicit none

        save

        contains


c-- GDI_LOAD_DEPLOY_SET --------------------------------------------------------
c   Reads the data from a gudb_deployment_set into a gdi_deploy_set object
c-------------------------------------------------------------------------------
          subroutine gdi_load_deploy_set(stream, dset, err_code, err_unit, hull, tophat)
            integer              i, idx
            integer::            err_unit, err_code
            logical,optional::   hull, tophat
            logical              hull_mode, tophat_mode
            character*3          station
            character*5          stream
            type(date_block)           rt_end_date
            type(di_deploy_set)        dset
            type(gudb_buoy_state)      moored_state
            type(gudb_deployment_set)  gset
            type(gudb_station)         gstation

            hull_mode = .false.
            if (PRESENT(hull)) then
              if (hull .eqv. .true.) hull_mode = .true.
            end if
            tophat_mode = .false.
            if (PRESENT(tophat)) then
              if (tophat .eqv. .true.) tophat_mode = .true.
            end if

            if (hull_mode .or. tophat_mode) then
              dset%station = 'XXX'
            else
              if (LEN_TRIM(stream) .eq. 3) stream = TRIM(stream)//'p1'
              dset%station = stream(1:3)
            end if
            dset%stream = stream
            dset%check_realtime = -1

c--   Open deploy file and read header, data

            dset%deploy_count = 0
            if (hull_mode) then
              gset = gudb_load_hull_history(dset%stream, err_code, err_unit)
            else if (tophat_mode) then
              gset = gudb_load_tophat_history(dset%stream, err_code, err_unit)
            else
              gset = gudb_load_deployment_history(dset%station, err_code, err_unit)
            end if

            if (err_code .eq. 0) then
              do i = 1, gset%dcount
                moored_state = gudb_get_state_by_type(gset%deploy(i)%gbuoystates, 'm')
                if (moored_state%state .eq. 'm') then
                  dset%deploy_count = dset%deploy_count + 1
                  idx = dset%deploy_count
                  dset%deployments(idx)%station = gset%deploy(i)%gstation%cdip_id
                  dset%deployments(idx)%dset_name = gset%deploy(i)%gstation%stn_name
                  if (dset%station .eq. 'XXX') then
                    dset%deployments(idx)%stream = gset%deploy(i)%gstation%cdip_id//'p1'
                  else
                    dset%deployments(idx)%stream = stream
                  end if
                  dset%deployments(idx)%deploy_no = gset%deploy(i)%deploy_num
                  dset%deployments(idx)%start_date = moored_state%start_date
                  dset%deployments(idx)%end_date = moored_state%end_date
                  dset%deployments(idx)%deploy_site = gset%deploy(i)%deploy_site
                  rt_end_date = add_seconds(moored_state%end_date, 3600*24*7)
                  if (is_between(current_utc(), moored_state%start_date, rt_end_date)) dset%check_realtime = 1
                end if
              end do
            end if

            return
          end subroutine


c-- GDI_LOAD_TIMESPAN_DEPLOYMENTS ----------------------------------------------
c   Returns a di_deploy_set with the relevant deployments for the given tspan.
c   Called directly with the 5-digit station id if no deploy info is loaded.
c-------------------------------------------------------------------------------
          type(di_deploy_set) function gdi_load_timespan_deployments(stream, tspan, ecode, eunit, hull, tophat)
            integer              ecode, eunit
            logical,optional::   hull, tophat
            logical              hull_mode, tophat_mode
            character*5          stream
            type(time_span)      tspan
            type(di_deploy_set)  dset, tmp_dset

            hull_mode = .false.
            if (PRESENT(hull)) then
              if (hull .eqv. .true.) hull_mode = .true.
            end if
            tophat_mode = .false.
            if (PRESENT(tophat)) then
              if (tophat .eqv. .true.) tophat_mode = .true.
            end if

            call gdi_load_deploy_set(stream, dset, ecode, eunit, hull_mode, tophat_mode)
            if (ecode .eq. 2) then
              tmp_dset%check_realtime = 1
              tmp_dset%deploy_count = 0
            else if (ecode .ne. 0) then
              if (eunit .gt. 0) write(eunit,'(a)') 'ERROR, aborting: loading deployment log'
              call exit(1)
            else
              call deployments_by_timespan(dset, tmp_dset, tspan, ecode)
            end if

            tmp_dset%stream = stream
            gdi_load_timespan_deployments = tmp_dset
            return
          end function


c-- GDI_MAKE_WAVECDF5_SET_FILELIST ---------------------------------------------
c   Returns an array with relevant wavecdf5 datasets for both observational
c   and model data.
c-------------------------------------------------------------------------------
          subroutine gdi_make_wavecdf5_set_filelist(id, tspan, load_type, qc_flag, forecast_mode, fnames, fcount)
            integer              errcode, fcount, i, load_type, nullunit
            logical              dep_mode, forecast_mode, mop_mode, need_xy, qc_flag, stn_mode
c           character*(*)        id
            character*100        id
            character*2          county_prefixes(19)
            character*100        fnames(*), subdir
            type(di_deploy_set)  dset
            type(time_span)      tspan

            nullunit = 90
            open(unit=nullunit, file='/dev/null')
            data county_prefixes / 'D0', 'D1', 'OC', 'L0', 'L1', 'VE', 'B0', 'B1',
     *        'SL', 'MO', 'SC', 'SM', 'SF', 'MA', 'SN', 'M0', 'M1', 'HU', 'DN' /

            fcount = 0
            stn_mode = .false.
            mop_mode = .false.
            dep_mode = .false.
            need_xy = .false.
            if (BTEST(load_type,2)) need_xy = .true.

            if (LEN_TRIM(id) .eq. 3) then
              stn_mode = .true.
              id = TRIM(id)//'p1'
            else if (LEN_TRIM(id) .eq. 7 .and. id(1:1) .eq. 'S' .and. id(5:5) .eq. 'D') then
              dep_mode = .true.
            else if (LEN_TRIM(id) .eq. 5) then
             if (IACHAR(id(1:1)) .gt. IACHAR('9')) then
                mop_mode = .true.
              else
                stn_mode = .true.
              end if
            end if

            if (stn_mode) then
              dset = gdi_load_timespan_deployments(id(1:5), tspan, errcode, nullunit)

              if (qc_flag .and. (.not. BTEST(load_type,4)) .and. (.not. BTEST(load_type,2))) then
                call make_historic_set_filelist(dset, fnames, fcount)
              else
                call make_deploy_set_filelist(dset, fnames, fcount)
              end if
            else if (dep_mode) then
               call gdi_compile_active_deployment_dataset_list(id(2:4), id(6:7), need_xy, fnames, fcount)
            else if (mop_mode) then
              if (id(1:2) .eq. 'BP') then
                subdir = 'MODELS/MOP_validation/'
              else if (ANY(county_prefixes .eq. id(1:2))) then
                subdir = 'MODELS/MOP_alongshore/'
              else
                subdir = 'MODELS/misc/'
              end if
              if (forecast_mode) then
                fcount = 1
                fnames(1) = TRIM(subdir)//TRIM(id)//'_forecast.nc'
              else
                fcount = 2
                fnames(1) = TRIM(subdir)//TRIM(id)//'_hindcast.nc'
                fnames(2) = TRIM(subdir)//TRIM(id)//'_nowcast.nc'
              end if
            else
              fcount = 1
              fnames(1) = id
            end if
            close(nullunit)
          end subroutine

c-- GDI_COMPILE_ACTIVE_DEPLOYMENT_DATASET_LIST ---------------------------------
c  Lists all possible datasets for an active deployment, including all states
c  Copy of WC5U_COMPILE; needs clean up
c-------------------------------------------------------------------------------
        subroutine gdi_compile_active_deployment_dataset_list(stn, dep, need_xy, ncfiles, nccount)
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

      end module
