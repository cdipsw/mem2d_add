c-- DEPLOY_INFO ----------------------------------------------------------------
c   The deploy_info module contains methods that assemble and manipulate 
c   deployment information used with CDIP netCDF datasets. Data is read from 
c   .data04/WNC_DATA/metadata/deploy_log files.
c-------------------------------------------------------------------------------

        module deploy_info

        use archive_info
        use dates
        use file_ops
        use locations
        use strings

        implicit none

        save


        integer, parameter :: DI_max_deployments = 100

        type di_deployment
          integer       deploy_no, start_frame, end_frame
          character*3   station
          character*5   stream
          character*100 dset_name
          type(date_block) start_date, end_date
          type(location)   deploy_site
        end type

        type di_deploy_set
           integer      deploy_count, check_realtime
           character*3  station
           character*5  stream
           type(di_deployment) deployments(DI_max_deployments)
        end type

        contains


c-- LOAD_DEPLOY_FILE -----------------------------------------------------------
c   Reads the data from a deploy_log into a di_deploy_set object
c-------------------------------------------------------------------------------
          subroutine load_deploy_file(stream, dset, err_code, err_unit)
            integer          sfrm, efrm, hdr_size
            integer::        err_unit, err_code, temp_unit=99
            character*1      tab
            character*5      stream
            character*500    line
            type(date_block) open_frame_date   !* Future date, for open frames
            type(di_deploy_set)  dset

            tab = ACHAR(9)
            open_frame_date = init_date(2100,1,1,0,0,0)
            if (LEN_TRIM(stream) .eq. 3) stream = TRIM(stream)//'p1'

            dset%station = stream(1:3)
            dset%stream = stream
            dset%check_realtime = -1

c--   Open deploy file and read header, data

            open(temp_unit,file='/project/WNC/WNC_DATA/metadata/deploy_logs/'//
     *        stream//'_deploy_log',form='formatted',status='old',iostat=err_code)
            if (err_code .ne. 0) then
              if (err_unit .gt. 0) write(err_unit,'(a,i5)') 
     *          'ERROR (DI-load_deploy_file): opening file, code = ',err_code
              return
            end if

            read(temp_unit,'(a500)',iostat=err_code) line
            do while (err_code .eq. 0)
              dset%deploy_count = dset%deploy_count + 1
              dset%deployments(dset%deploy_count)%station = dset%station
              dset%deployments(dset%deploy_count)%stream = dset%stream
              dset%deployments(dset%deploy_count)%deploy_no = get_field_int(line,tab,1)
              dset%deployments(dset%deploy_count)%start_frame = get_field_int(line,tab,2)
              dset%deployments(dset%deploy_count)%end_frame = get_field_int(line,tab,3)
              read(temp_unit,'(a500)',iostat=err_code) line
            end do

            if (err_code .gt. 0 .and. err_unit .gt. 0) write(err_unit,'(a,i5)') 
     *        'ERROR (DI-load_deploy_file): reading file, code = ',err_code

            close(temp_unit,iostat=err_code)
            return
          end subroutine



c-- GET_DEPLOY_DETAILS ---------------------------------------------------------
c   Fills in the start and end dates and position info for a di_deploy_set
c-------------------------------------------------------------------------------
          subroutine get_deploy_details(dset, err_code, err_unit)
            integer             err_code, err_unit, i, tframe
            logical             found
            type(ai_time_frame) arch_frame
            type(date_block)    curr_utc
            type(di_deploy_set) dset

            call load_arch_file(dset%station, err_code, err_unit)
            do i = 1, dset%deploy_count
              dset%deployments(i)%start_date = AI_data%frames(1,dset%deployments(i)%start_frame)%start_date
              dset%deployments(i)%end_date = AI_data%frames(1,dset%deployments(i)%end_frame)%end_date
              dset%deployments(i)%deploy_site = AI_data%frames(1,dset%deployments(i)%start_frame)%deploy_site
            end do

            if (dset%deploy_count .ge. 1) then
              tframe = dset%deployments(dset%deploy_count)%end_frame + 1
            else
              tframe = 1
            end if
            if (AI_data%frames(1,tframe)%frame .ne. 0) then
              curr_utc = current_utc()
              arch_frame = get_arch_frame(1, curr_utc, found, err_unit)
              if (found) then
                dset%check_realtime = 1
              else
                dset%check_realtime = 0
              end if
            else
              dset%check_realtime = 0
            end if
          end subroutine


c-- DEPLOYMENT_BY_DATE ---------------------------------------------------------
c   Returns the di_deployment that contains the specified time. Sets err_code to
c   1 if no deployment containing the time is found.
c-------------------------------------------------------------------------------
          subroutine deployment_by_date(dset, rdate, dep, err_code)
            integer               err_code, i
            type(date_block)      rdate
            type(di_deploy_set)   dset
            type(di_deployment)   dep
            err_code = 1
            do i = 1, dset%deploy_count
              if (is_between(rdate, dset%deployments(i)%start_date, dset%deployments(i)%end_date)) then
                err_code = 0
                dep = dset%deployments(i)
              end if
            end do
          end subroutine


c-- DEPLOYMENTS_BY_TIMEPSAN ----------------------------------------------------
c   Returns an array of deployments that cover the requested time_span. 
c-------------------------------------------------------------------------------
          subroutine deployments_by_timespan(dset, sub_dset, rspan, err_code)
            integer          err_code, i
            type(date_block)      ctime
            type(time_span)       rspan
            type(di_deploy_set)   dset, sub_dset

            err_code = 1
            ctime = current_utc()
            sub_dset%station = dset%station
            sub_dset%stream = dset%stream
            sub_dset%deploy_count = 0

            do i = 1, dset%deploy_count
              if (is_between(rspan%start, dset%deployments(i)%start_date, dset%deployments(i)%end_date) .or.
     *            is_between(rspan%end, dset%deployments(i)%start_date, dset%deployments(i)%end_date) .or.
     *            (is_before(rspan%start, dset%deployments(i)%start_date) .and.
     *             is_after(rspan%end, dset%deployments(i)%end_date))) then
                err_code = 0
                sub_dset%deploy_count = sub_dset%deploy_count + 1
                sub_dset%deployments(sub_dset%deploy_count) = dset%deployments(i)
              end if
            end do

            sub_dset%check_realtime = 0
            if (dset%check_realtime .eq. 1 .and. sub_dset%deploy_count .eq. 0) then
              sub_dset%check_realtime = 1
            else if (dset%check_realtime .eq. 1 .and. sub_dset%deploy_count .gt. 0) then
              if (is_after(rspan%end, sub_dset%deployments(sub_dset%deploy_count)%end_date) .or.
     *          is_after(add_seconds(sub_dset%deployments(sub_dset%deploy_count)%end_date,3600*24*7), ctime))
     *            sub_dset%check_realtime = 1
            end if
          end subroutine


c-- LOAD_TIMESPAN_DEPLOYMENTS --------------------------------------------------
c   Returns a di_deploy_set with the relevant deployments for the given tspan.
c   Called directly with the 5-digit station id if no deploy info is loaded.
c-------------------------------------------------------------------------------
          type(di_deploy_set) function load_timespan_deployments(stream, tspan, ecode, eunit)
            integer              ecode, eunit
            character*5          stream
            type(time_span)      tspan
            type(di_deploy_set)  dset, tmp_dset

            call load_deploy_file(stream, dset, ecode, eunit)
            if (ecode .eq. 2) then
              tmp_dset%check_realtime = 1
              tmp_dset%deploy_count = 0
            else if (ecode .ne. 0) then
              if (eunit .gt. 0) write(eunit,'(a)') 'ERROR, aborting: loading deployment log'
              call exit(1)
            else
              call get_deploy_details(dset, ecode, eunit)
              call deployments_by_timespan(dset, tmp_dset, tspan, ecode)
            end if

            tmp_dset%stream = stream
            load_timespan_deployments = tmp_dset
            return
          end function


c-- MAKE_DEPLOY_SET_FILELIST ---------------------------------------------------
c   Returns an array of dataset names for a di_deploy_set
c-------------------------------------------------------------------------------
          subroutine make_deploy_set_filelist(dset, fnames, fcount)
            integer              fcount, i
            character*2          deploy_number  
            character*100        fnames(*)
            type(date_block)     ctime
            type(di_deploy_set)  dset

            ctime = current_utc()
            fcount = 0
            do i = 1, dset%deploy_count
              if (.not. is_after(dset%deployments(i)%end_date, ctime)) then
                fnames(i) = get_deploy_dataset_name(dset%deployments(i))
                fcount = fcount + 1
              end if
            end do
            if (dset%check_realtime .ge. 1 .and. dset%deploy_count .ge. 1) then
              write(deploy_number,'(i02.2)') dset%deployments(dset%deploy_count)%deploy_no
              fcount = fcount + 1
              if (dset%stream .eq. '073p1') then
                fnames(fcount) = 'XREALTIME/073p1_rt.nc'
              else
                fnames(fcount) = 'MOORED/'//dset%deployments(dset%deploy_count)%stream//'_d'//deploy_number//'_rt.nc'
                fcount = fcount + 1
                fnames(fcount) = 'MOORED/'//dset%deployments(dset%deploy_count)%stream//'_d'//deploy_number//'_xy.nc'
              end if
            end if
          end subroutine


c-- MAKE_HISTORIC_SET_FILELIST -------------------------------------------------
c   Returns an array with just the historic and realtime datasets for public-
c   only, non-displacement data requests.
c-------------------------------------------------------------------------------
          subroutine make_historic_set_filelist(dset, fnames, fcount)
            integer              fcount, i
            character*100        fnames(*)
            type(di_deploy_set)  dset
            fcount = 0
            fcount = fcount + 1
            fnames(fcount) = 'XARCHIVE/'//dset%stream//'/'//dset%stream//'_historic.nc'
            if (dset%deploy_count .ge. 1) then
              fcount = fcount + 1
              fnames(fcount) = 'ARCHIVE/'//dset%stream//'/'//dset%stream//'_historic.nc'
            end if
            if (dset%check_realtime .ge. 1) then
              if (dset%deploy_count .gt. 0) then
                fcount = fcount + 1
                fnames(fcount) = 'REALTIME/'//dset%deployments(dset%deploy_count)%stream//'_rt.nc'
              end if
              if (dset%stream .eq. '073p1') then
                fcount = fcount + 1
                fnames(fcount) = 'XREALTIME/073p1_rt.nc'
              end if
            end if
          end subroutine


c-- MAKE_WAVECDF5_SET_FILELIST -------------------------------------------------
c   Returns an array with relevant wavecdf5 datasets for both observational
c   and model data.
c-------------------------------------------------------------------------------
          subroutine make_wavecdf5_set_filelist(id, tspan, load_type, qc_flag,
     *                                          forecast_mode, fnames, fcount)
            integer              errcode, fcount, i, load_type, nullunit
            logical              forecast_mode, mop_mode, qc_flag, stn_mode
            character*(*)        id
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
            if (LEN_TRIM(id) .eq. 3) then
              stn_mode = .true.
              id = TRIM(id)//'p1'
            else if (LEN_TRIM(id) .eq. 5) then
              if (IACHAR(id(1:1)) .gt. IACHAR('9')) then
                mop_mode = .true.
              else
                stn_mode = .true.
              end if
            end if

            if (stn_mode) then
              dset = load_timespan_deployments(id(1:5), tspan, errcode, nullunit)

              if (qc_flag .and. (.not. BTEST(load_type,4)) .and. (.not. BTEST(load_type,2))) then
                call make_historic_set_filelist(dset, fnames, fcount)
              else
                call make_deploy_set_filelist(dset, fnames, fcount)
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


c-- GET_DEPLOY_DATASET_NAME ----------------------------------------------------
c   Returns the name of the netCDF dataset for the deployment
c-------------------------------------------------------------------------------
          character*100 function get_deploy_dataset_name(dep)
            type(di_deployment)   dep
            character*2 deploy_number  

            write(deploy_number,'(i02.2)') dep%deploy_no
            get_deploy_dataset_name = 'ARCHIVE/'//dep%stream//'/'//dep%stream//'_d'//deploy_number//'.nc'
          end function


      end module
