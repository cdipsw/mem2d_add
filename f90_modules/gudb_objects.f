c-- GUDB_OBJECTS ---------------------------------------------------------------
c   Routines for accessing and manipulating information from the Grand Unified
c   Database, or GUDB.
c
c   REST requests are sent to the URL specified in the text file
c   /project/WNC/GUDB_meta/REST_url.dat (if available)
c
c   NOTE 1: Must link with '-L/usr/lib -lcurl' on build for this module to work
c   NOTE 2: Accompanying c routine, gudb_rest_request_.c, will work with URL
c             output up to 200,000 characters
c-------------------------------------------------------------------------------

      module gudb_objects

        use dates
        use file_ops
        use gudb_lookup_utils
        use json_parser
        use locations
        use strings

        implicit none

        save

        integer,parameter::   GUDB_int_fill = -99999
        real,parameter::      GUDB_real_fill = -999.99
        character*10          GUDB_buoy_types(5) 
        character*100::       GUDB_cache_dir = "/project/WNC/GUDB_meta/"
        character*100::       GUDB_rest_url = "NULL"

        data GUDB_buoy_types / 'DWR-M1', 'DWR-M2', 'DWR-M3', 'DWR-M4', 'DWR-GPS' /

        type gudb_station
          real               target_depth, target_freq
          character*3        cdip_id
          character*5        wmo_id
          character*20       funder_acronym(5), operator_acronym(5)
          character*100      funder_name(5), operator_name(5), stn_name
          type(date_block)   decomm_date
          type(location)     target_site
        end type

        type gudb_station_list
          integer              scount
          type(gudb_station)   stn(1000)
        end type

        type gudb_stnorder_list
          integer              scount
          character*3          stn(1000)
        end type

        type gudb_tophat_show
          logical            page_no_update, mail_offsite, page_offsite
          integer            notify_bitmask
          character*3        cdip_id		!* DEPRECATED
          character*100      tophat_id
          character*500      position_updates
        end type

        type gudb_show_list
          integer                scount
          type(gudb_tophat_show) show(1000)
        end type

        type gudb_stn_comm
          logical            iridium_comm, net_comm, rxc_comm, ssh_comm
          integer            iridium_mode, update_interval, iridium_interval
          real               update_coverage
          character*3        cdip_id
          character*100      tophat_id
        end type

        type gudb_comm_list
          integer              ccount
          type(gudb_stn_comm)  comm(1000)
        end type

        type gudb_hull
          real               diameter
          integer            buoy_type
          character*100      alloy, serial_no
          logical            is_acm
        end type

        type gudb_tophat
          integer            buoy_type
          character*100      gps_card_version, serial_no
          character*500      notes
          logical            is_solar, is_cat4
        end type

        type gudb_mooring
          real               total_length
          type(date_block)   deploy_date
          character*10       float_type, acoustic_release
          logical            marker_float
        end type

        type gudb_buoy_state
          character*1        state
          type(date_block)   start_date, end_date
        end type

        type gudb_buoy_state_set
          integer                 scount
          type(gudb_buoy_state)   bstate(4)
        end type

        type gudb_data_hold
          logical            acm, cat4, sst, wave
          type(date_block)   start_date, end_date
        end type

        type gudb_data_hold_set
          integer                hcount
          type(gudb_data_hold)   dhold(20)
        end type

        type gudb_proc_fix
          integer            fix_id
          type(date_block)   start_date, end_date
        end type

        type gudb_proc_fix_set
          integer                fcount
          type(gudb_proc_fix)    pfix(20)
        end type

        type gudb_deployment
          integer            deploy_num
          real               depth, declination, inclination
          logical            cat4_antenna, acm_enabled
          character*20       firmware
          type(date_block)   start_date, end_date
          type(location)     deploy_site

          type(gudb_station)         gstation
          type(gudb_hull)            ghull
          type(gudb_mooring)         gmooring
          type(gudb_tophat)          gtophat
          type(gudb_buoy_state_set)  gbuoystates
          type(gudb_data_hold_set)   gdataholds
          type(gudb_proc_fix_set)    gprocfixes
        end type

        type gudb_deployment_set
          integer                 dcount
          type(gudb_deployment)   deploy(200)
        end type

        contains


c-- GUDB_INIT_STATION ----------------------------------------------------------
c   Initializes a gudb_station object, blank
c-------------------------------------------------------------------------------
          type(gudb_station) function gudb_init_station()
            gudb_init_station%funder_acronym = ''
            gudb_init_station%funder_name = ''
            gudb_init_station%operator_acronym = ''
            gudb_init_station%operator_name = ''
            gudb_init_station%target_freq = GUDB_real_fill
            gudb_init_station%target_depth = GUDB_real_fill
            gudb_init_station%cdip_id = ''
            gudb_init_station%wmo_id = ''
            gudb_init_station%stn_name = ''
            gudb_init_station%decomm_date = init_date(2100,1,1,0,0,0)
            gudb_init_station%target_site = init_location(0.0,0.0)
          end function


c-- GUDB_INIT_DEPLOYMENT -------------------------------------------------------
c   Initializes a gudb_deployment object, blank
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_init_deployment()
            gudb_init_deployment%deploy_num = GUDB_int_fill
            gudb_init_deployment%depth = GUDB_real_fill
            gudb_init_deployment%inclination = GUDB_real_fill
            gudb_init_deployment%declination = GUDB_real_fill
            gudb_init_deployment%cat4_antenna = .false.
            gudb_init_deployment%acm_enabled = .false.
            gudb_init_deployment%firmware = ''
            gudb_init_deployment%start_date = init_date(2100,1,1,0,0,0)
            gudb_init_deployment%end_date = init_date(2100,1,1,0,0,0)
            gudb_init_deployment%deploy_site = init_location(0.0,0.0)
            gudb_init_deployment%gstation = gudb_init_station()
            gudb_init_deployment%ghull = gudb_init_hull()
            gudb_init_deployment%gtophat = gudb_init_tophat()
            gudb_init_deployment%gmooring = gudb_init_mooring()
            gudb_init_deployment%gbuoystates = gudb_init_buoy_state_set()
            gudb_init_deployment%gdataholds = gudb_init_data_hold_set()
            gudb_init_deployment%gprocfixes = gudb_init_proc_fix_set()
          end function


c-- GUDB_INIT_DEPLOYMENT_SET ---------------------------------------------------
c   Initializes a gudb_deployment_set object, blank
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_init_deployment_set()
            integer   i
            do i = 1, 200
              gudb_init_deployment_set%deploy(i) = gudb_init_deployment()
            end do
            gudb_init_deployment_set%dcount = 0
          end function


c-- GUDB_INIT_TOPHAT -----------------------------------------------------------
c   Initializes a gudb_tophat object, blank
c-------------------------------------------------------------------------------
          type(gudb_tophat) function gudb_init_tophat()
            gudb_init_tophat%buoy_type = -1
            gudb_init_tophat%gps_card_version = ''
            gudb_init_tophat%serial_no = ''
            gudb_init_tophat%notes = ''
            gudb_init_tophat%is_solar = .false.
            gudb_init_tophat%is_cat4 = .false.
          end function


c-- GUDB_INIT_HULL -------------------------------------------------------------
c   Initializes a gudb_hull object, blank
c-------------------------------------------------------------------------------
          type(gudb_hull) function gudb_init_hull()
            gudb_init_hull%buoy_type = -1
            gudb_init_hull%alloy = ''
            gudb_init_hull%serial_no = ''
            gudb_init_hull%diameter = GUDB_real_fill
            gudb_init_hull%is_acm = .false.
          end function


c-- GUDB_INIT_MOORING ----------------------------------------------------------
c   Initializes a gudb_mooring object, blank
c-------------------------------------------------------------------------------
          type(gudb_mooring) function gudb_init_mooring()
            gudb_init_mooring%total_length = -1.0
            gudb_init_mooring%deploy_date = init_date(2100,1,1,0,0,0)
            gudb_init_mooring%float_type = ''
            gudb_init_mooring%acoustic_release = ''
            gudb_init_mooring%marker_float = .false.
          end function


c-- GUDB_INIT_STN_SHOW ---------------------------------------------------------
c   Initializes a gudb_tophat_show object, blank
c-------------------------------------------------------------------------------
          type(gudb_tophat_show) function gudb_init_tophat_show()
            gudb_init_tophat_show%cdip_id = ''
            gudb_init_tophat_show%tophat_id = ''
            gudb_init_tophat_show%page_no_update = .false.
            gudb_init_tophat_show%page_offsite = .false.
            gudb_init_tophat_show%mail_offsite = .false.
            gudb_init_tophat_show%position_updates = ''
            gudb_init_tophat_show%notify_bitmask = 0
          end function


c-- GUDB_INIT_SHOW_LIST --------------------------------------------------------
c   Initializes a gudb_show_list object, blank
c-------------------------------------------------------------------------------
          type(gudb_show_list) function gudb_init_show_list()
            integer    i
            gudb_init_show_list%scount = 0
            do i = 1, 1000
              gudb_init_show_list%show(i) = gudb_init_tophat_show()
            end do
          end function


c-- GUDB_INIT_STATION_LIST -----------------------------------------------------
c   Initializes a gudb_station_list object, blank
c-------------------------------------------------------------------------------
          type(gudb_station_list) function gudb_init_station_list()
            integer    i
            gudb_init_station_list%scount = 0
            do i = 1, 1000
              gudb_init_station_list%stn(i) = gudb_init_station()
            end do
          end function


c-- GUDB_INIT_STN_COMM ---------------------------------------------------------
c   Initializes a gudb_stn_comm object, blank
c-------------------------------------------------------------------------------
          type(gudb_stn_comm) function gudb_init_stn_comm()
            gudb_init_stn_comm%cdip_id = ''
            gudb_init_stn_comm%tophat_id = ''
            gudb_init_stn_comm%iridium_comm = .false.
            gudb_init_stn_comm%net_comm = .false.
            gudb_init_stn_comm%rxc_comm = .false.
            gudb_init_stn_comm%ssh_comm = .false.
            gudb_init_stn_comm%iridium_mode = -1
            gudb_init_stn_comm%iridium_interval = -1
            gudb_init_stn_comm%update_interval = -1
            gudb_init_stn_comm%update_coverage = -1.0
          end function


c-- GUDB_INIT_COMM_LIST --------------------------------------------------------
c   Initializes a gudb_comm_list object, blank
c-------------------------------------------------------------------------------
          type(gudb_comm_list) function gudb_init_comm_list()
            integer    i
            gudb_init_comm_list%ccount = 0
            do i = 1, 1000
              gudb_init_comm_list%comm(i) = gudb_init_stn_comm()
            end do
          end function


c-- GUDB_INIT_STNORDER_LIST ----------------------------------------------------
c   Initializes a gudb_stnorder_list object, blank
c-------------------------------------------------------------------------------
          type(gudb_stnorder_list) function gudb_init_stnorder_list()
            integer    i
            gudb_init_stnorder_list%scount = 0
            do i = 1, 1000
              gudb_init_stnorder_list%stn(i) = ''
            end do
          end function


c-- GUDB_INIT_BUOY_STATE -------------------------------------------------------
c   Initializes a gudb_buoy_state object, blank
c-------------------------------------------------------------------------------
          type(gudb_buoy_state) function gudb_init_buoy_state()
            gudb_init_buoy_state%state = ''
            gudb_init_buoy_state%start_date = init_date(2100,1,1,0,0,0)
            gudb_init_buoy_state%end_date = init_date(2100,1,1,0,0,0)
          end function


c-- GUDB_INIT_BUOY_STATE_SET ---------------------------------------------------
c   Initializes a gudb_buoy_state_set object, blank
c-------------------------------------------------------------------------------
          type(gudb_buoy_state_set) function gudb_init_buoy_state_set()
            integer   i
            do i = 1, 4
              gudb_init_buoy_state_set%bstate(i) = gudb_init_buoy_state()
            end do
          end function


c-- GUDB_INIT_DATA_HOLD --------------------------------------------------------
c   Initializes a gudb_data_hold object, blank
c-------------------------------------------------------------------------------
          type(gudb_data_hold) function gudb_init_data_hold()
            gudb_init_data_hold%acm = .false.
            gudb_init_data_hold%cat4 = .false.
            gudb_init_data_hold%sst = .false.
            gudb_init_data_hold%wave = .false.
            gudb_init_data_hold%start_date = init_date(2100,1,1,0,0,0)
            gudb_init_data_hold%end_date = init_date(2100,1,1,0,0,0)
          end function


c-- GUDB_INIT_DATA_HOLD_SET ----------------------------------------------------
c   Initializes a gudb_data_hold_set object, blank
c-------------------------------------------------------------------------------
          type(gudb_data_hold_set) function gudb_init_data_hold_set()
            integer   i
            do i = 1, 20
              gudb_init_data_hold_set%dhold(i) = gudb_init_data_hold()
            end do
            gudb_init_data_hold_set%hcount = 0
          end function


c-- GUDB_INIT_PROC_FIX ---------------------------------------------------------
c   Initializes a gudb_proc_fix object, blank
c-------------------------------------------------------------------------------
          type(gudb_proc_fix) function gudb_init_proc_fix()
            gudb_init_proc_fix%fix_id = -1
            gudb_init_proc_fix%start_date = init_date(2100,1,1,0,0,0)
            gudb_init_proc_fix%end_date = init_date(2100,1,1,0,0,0)
          end function


c-- GUDB_INIT_PROC_FIX_SET -----------------------------------------------------
c   Initializes a gudb_proc_fix_set object, blank
c-------------------------------------------------------------------------------
          type(gudb_proc_fix_set) function gudb_init_proc_fix_set()
            integer   i
            do i = 1, 20
              gudb_init_proc_fix_set%pfix(i) = gudb_init_proc_fix()
            end do
            gudb_init_proc_fix_set%fcount = 0
          end function


c-- GUDB_LOAD_HULL -------------------------------------------------------------
c   Loads a gudb_hull object for the given serial no, requests and parses
c-------------------------------------------------------------------------------
          type(gudb_hull) function gudb_load_hull(serial, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*5         serial
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(10, cvals, csize, .false., err_code, err_unit, tophatid=serial)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load hull'
              gudb_load_hull = gudb_init_hull()
              return
            else
              gudb_load_hull = gudb_parse_hull(cvals(1:csize), err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_STATION ----------------------------------------------------------
c   Loads a gudb_station object for the given stn id - requests and parses
c-------------------------------------------------------------------------------
          type(gudb_station) function gudb_load_station(stn, err_code, err_unit, use_cache, max_cache_age)
            integer             csize, err_unit, err_code, max_age
            integer,optional::  max_cache_age
            logical             cache_call
            logical,optional::  use_cache
            character*3         stn
            character*200       url
            character(200000)    cvals

            call gudb_init_cache_args(cache_call, max_age, use_cache, max_cache_age)
            if (cache_call) then
              call gudb_manage_cache_request(1, max_age, cvals, csize, .true., err_code, err_unit, station=stn)
            else
              call gudb_manage_request(1, cvals, csize, .true., err_code, err_unit, station=stn)
            end if

            if (err_code .ne. 0) then
              if (err_unit .ne. -1) write(err_unit,'(a)') ' rest request failed: load station'
              gudb_load_station = gudb_init_station()
              return
            else
              gudb_load_station = gudb_parse_station(cvals(1:csize), err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_STATION_FROM_WMOID -----------------------------------------------
c   Loads a gudb_station object for the given wmo id - requests and parses
c-------------------------------------------------------------------------------
          type(gudb_station) function gudb_load_station_from_wmoid(wmo_id, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*5         wmo_id
            character*200       url
            character(200000)    cvals

            call gudb_manage_request(15, cvals, csize, .true., err_code, err_unit, wmoid=wmo_id)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load station from wmoid'
              gudb_load_station_from_wmoid = gudb_init_station()
              return
            else
              gudb_load_station_from_wmoid = gudb_parse_station(cvals(1:csize), err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_STNORDER_LIST  ---------------------------------------------------------
c   Loads a gudb_stnorder_list object - requests and parses
c-------------------------------------------------------------------------------
          type(gudb_stnorder_list) function gudb_load_stnorder_list(err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*200       url
            character(200000)    cvals

            call gudb_manage_request(14, cvals, csize, .false., err_code, err_unit)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load stnorder'
              gudb_load_stnorder_list = gudb_init_stnorder_list()
              return
            else
              gudb_load_stnorder_list = gudb_parse_stnorder_list(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_FULL_STATION_LIST  -----------------------------------------------
c   Loads a gudb_station_list object - requests and parses
c-------------------------------------------------------------------------------
          type(gudb_station_list) function gudb_load_full_station_list(err_code, err_unit, use_cache, max_cache_age)
            integer             csize, err_unit, err_code, max_age
            integer,optional::  max_cache_age
            logical             cache_call
            logical,optional::  use_cache
            character*200       url
            character(200000)   cvals

            call gudb_init_cache_args(cache_call, max_age, use_cache, max_cache_age)
            if (cache_call) then
              call gudb_manage_cache_request(17, max_age, cvals, csize, .false., err_code, err_unit)
            else
              call gudb_manage_request(17, cvals, csize, .false., err_code, err_unit)
            end if

            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load station list'
              gudb_load_full_station_list = gudb_init_station_list()
              return
            else
              gudb_load_full_station_list = gudb_parse_station_list(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_GET_STATION_FUNDER_ACRONYMS -------------------------------------------
c   Returns a comma-delimited string of funder acronyms: 'USACE,PACIOOS'
c-------------------------------------------------------------------------------
          character*100 function gudb_get_station_funder_acronyms(gstation, delim)
            integer             i
            character*1         delim, delimiter
            type(gudb_station)  gstation

            gudb_get_station_funder_acronyms = ''
            delimiter = ''
            do i = 1, 5
              if (TRIM(gstation%funder_acronym(i)) .ne. '') then
                gudb_get_station_funder_acronyms = TRIM(gudb_get_station_funder_acronyms)//TRIM(delimiter)//
     *            TRIM(gstation%funder_acronym(i))
                delimiter = TRIM(delim)
              end if
            end do
          end function


c-- GUDB_GET_STATION_OPERATOR_ACRONYMS -----------------------------------------
c   Returns a comma-delimited string of operator acronyms: 'USACE,PACIOOS'
c-------------------------------------------------------------------------------
          character*100 function gudb_get_station_operator_acronyms(gstation, delim)
            integer             i
            character*1         delim, delimiter
            type(gudb_station)  gstation

            gudb_get_station_operator_acronyms = ''
            delimiter = ''
            do i = 1, 5
              if (TRIM(gstation%operator_acronym(i)) .ne. '') then
                gudb_get_station_operator_acronyms = TRIM(gudb_get_station_operator_acronyms)//TRIM(delimiter)//
     *            TRIM(gstation%operator_acronym(i))
                delimiter = TRIM(delim)
              end if
            end do
          end function


c-- GUDB_LOAD_DEPLOYMENT -------------------------------------------------------
c   Loads a gudb_deployment object for the given tophat and time
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_load_deployment(tophat, ftime, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*(*)       tophat
            character*20        time_str
            character*200       url
            character(200000)    cvals
            type(date_block)    ftime

            call gudb_manage_request(2, cvals, csize, .true., err_code, err_unit, reqtime=ftime, tophatid=tophat)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load deployment'
              gudb_load_deployment = gudb_init_deployment()
              return
            else
              gudb_load_deployment = gudb_parse_deployment(cvals(1:csize), err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_DEPLOYMENT_HISTORY -----------------------------------------------
c   Loads a gudb_deployment set holding all deployments for the given station
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_load_deployment_history(stn, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*3         stn
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(3, cvals, csize, .true., err_code, err_unit, station=stn)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load deployment history'
              gudb_load_deployment_history = gudb_init_deployment_set()
              return
            else
              gudb_load_deployment_history = gudb_parse_deployment_set(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_HULL_HISTORY -----------------------------------------------------
c   Loads a gudb_deployment set holding all deployments for the given hull id
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_load_hull_history(hull, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*5         hull
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(12, cvals, csize, .true., err_code, err_unit, tophatid=hull)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load hull history'
              gudb_load_hull_history = gudb_init_deployment_set()
              return
            else
              gudb_load_hull_history = gudb_parse_deployment_set(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_TOPHAT_HISTORY ---------------------------------------------------
c   Loads a gudb_deployment set holding all deployments for the given tophat id
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_load_tophat_history(tophat, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*5         tophat
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(13, cvals, csize, .true., err_code, err_unit, tophatid=tophat)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load tophat history'
              gudb_load_tophat_history = gudb_init_deployment_set()
              return
            else
              gudb_load_tophat_history = gudb_parse_deployment_set(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_TOPHAT -----------------------------------------------------------
c   Loads a gudb_tophat for the given tophat id
c-------------------------------------------------------------------------------
          type(gudb_tophat) function gudb_load_tophat(tophat, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*5         tophat
            character*200       url
            character*1000      tophat_str
            character(200000)   cvals

            call gudb_manage_request(16, cvals, csize, .false., err_code, err_unit, tophatid=tophat)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load tophat history'
              gudb_load_tophat = gudb_init_tophat()
              return
            else
              tophat_str = cvals(1:csize)
              gudb_load_tophat = gudb_parse_tophat(tophat_str, err_code, err_unit)
            end if
          end function


c-- GUDB_TRIM_DEPLOYMENT_SET ---------------------------------------------------
c   Trims a set to the given timespan, based on the selected state
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_trim_deployment_set(gdset, tspan, state_idx)
            integer                     i, sidx
            integer,optional::          state_idx
            type(gudb_buoy_state)       bstate
            type(gudb_deployment_set)   gdset
            type(time_span)             tspan, dspan

            sidx = -1
            if (PRESENT(state_idx)) then
              if (state_idx .ge. 0 .and. state_idx .le. 3) sidx = state_idx
            end if

            gudb_trim_deployment_set = gudb_init_deployment_set()
            do i = 1, gdset%dcount
              dspan = init_time_span(gdset%deploy(i)%start_date, gdset%deploy(i)%end_date)
              if (sidx .eq. -1 .and. is_in_time_span(gdset%deploy(i)%start_date, tspan) .or. 
     *            is_in_time_span(gdset%deploy(i)%end_date, tspan) .or. time_spans_overlap(tspan, dspan)) then
                gudb_trim_deployment_set%dcount = gudb_trim_deployment_set%dcount + 1
                gudb_trim_deployment_set%deploy(gudb_trim_deployment_set%dcount) = gdset%deploy(i)
              else if (sidx .ge. 0) then
                call gudb_assign_buoy_state_end_dates(gdset%deploy(i)%gbuoystates, gdset%deploy(i)%end_date)
                bstate = gudb_get_state_by_index(gdset%deploy(i)%gbuoystates, sidx)
                dspan = init_time_span(bstate%start_date, bstate%end_date)
                if (is_in_time_span(bstate%start_date, tspan) .or.  is_in_time_span(bstate%end_date, tspan) .or.
     *              time_spans_overlap(tspan, dspan)) then
                  gudb_trim_deployment_set%dcount = gudb_trim_deployment_set%dcount + 1
                  gudb_trim_deployment_set%deploy(gudb_trim_deployment_set%dcount) = gdset%deploy(i)
                end if
              end if
            end do
          end function


c-- GUDB_LOAD_DEPLOYMENT_SNAPSHOT ----------------------------------------------
c   Loads a gudb_deployment set holding all deployments for the given time
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_load_deployment_snapshot(stime, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*200       url
            character(200000)   cvals
            type(date_block)    stime

            call gudb_manage_request(5, cvals, csize, .true., err_code, err_unit, reqtime=stime)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load deployment snapshot'
              gudb_load_deployment_snapshot = gudb_init_deployment_set()
              return
            else
              gudb_load_deployment_snapshot = gudb_parse_deployment_set(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_LOAD_DEPLOYMENT_WINDOW ------------------------------------------------
c   Loads a gudb_deployment set holding all deployments open between times
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_load_deployment_window(stime, etime, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*200       url
            character(200000)   cvals
            type(date_block)    etime, stime

            call gudb_manage_request(11, cvals, csize, .true., err_code, err_unit, reqtime=stime, reqtime2=etime)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load deployment window'
              gudb_load_deployment_window = gudb_init_deployment_set()
              return
            else
              gudb_load_deployment_window = gudb_parse_deployment_set(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_GET_DEPLOYMENT_BY_NUMBER ----------------------------------------------
c   Returns the deployment from a set which matches the given deploy number
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_deployment_by_number(gdset, dep)
            integer::                  dep, i
            type(gudb_deployment_set)  gdset

            gudb_get_deployment_by_number = gudb_init_deployment()
            do i = 1, gdset%dcount 
              if (gdset%deploy(i)%deploy_num .eq. dep) then
                gudb_get_deployment_by_number = gdset%deploy(i)
                return
              end if
            end do
          end function


c-- GUDB_COUNT_DEPLOYMENTS_BY_DATE ---------------------------------------------
c   Counts the number of deployments in the set which span the given date.
c-------------------------------------------------------------------------------
          integer function gudb_count_deployments_by_date(gdset, search_date)
            integer::                  i
            type(date_block)           search_date
            type(gudb_deployment_set)  gdset

            gudb_count_deployments_by_date = 0
            do i = 1, gdset%dcount 
              if (is_between(search_date, gdset%deploy(i)%start_date, gdset%deploy(i)%end_date))
     *          gudb_count_deployments_by_date = gudb_count_deployments_by_date + 1
            end do
          end function


c-- GUDB_GET_DEPLOYMENT_BY_DATE ------------------------------------------------
c   Returns the deployment from a set which spans the given date. If more than
c   one deployment spans the date, the optional 'cnt' arg can be used.
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_deployment_by_date(gdset, search_date, cnt)
            integer::                  count, i, num_found
            integer,optional::         cnt
            type(date_block)           search_date
            type(gudb_deployment_set)  gdset

            if (PRESENT(cnt)) then
              count = cnt
            else
              count = 1
            end if

            gudb_get_deployment_by_date = gudb_init_deployment()
            num_found = 0
            do i = 1, gdset%dcount 
              if (is_between(search_date, gdset%deploy(i)%start_date, gdset%deploy(i)%end_date)) then
                num_found = num_found + 1
                if (num_found .eq. count) then
                  gudb_get_deployment_by_date = gdset%deploy(i)
                  return
                end if
              end if
            end do
          end function


c-- GUDB_GET_DEPLOYMENT_BY_DATE_AND_STATE --------------------------------------
c   Returns the deployment from a set which spans the given date in the
c   specified state.
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_deployment_by_date_and_state(gdset, search_date, cstate)
            integer::                  count, i, num_found
            character*1                cstate
            type(date_block)           search_date
            type(gudb_buoy_state)      bstate
            type(gudb_deployment_set)  gdset

            gudb_get_deployment_by_date_and_state = gudb_init_deployment()
            do i = 1, gdset%dcount 
              if (is_between(search_date, gdset%deploy(i)%start_date, gdset%deploy(i)%end_date)) then
                bstate = gudb_get_state_by_date(gdset%deploy(i)%gbuoystates, search_date)
                if (bstate%state .eq. cstate) then
                  gudb_get_deployment_by_date_and_state = gdset%deploy(i)
                  return
                end if
              end if
            end do
          end function


c-- GUDB_GET_LAST_COMPLETE_DEPLOYMENT ------------------------------------------
c   Returns the last deployment from a set which has a complete moored state.
c   A complete moored state will be for a buoy which has been flagged offsite
c   or recovered, or is past the deployment end date (if the buoy is lost).
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_last_complete_deployment(gdset)
            integer::                  i
            logical                    found
            type(date_block)           ctime
            type(gudb_deployment_set)  gdset

            found = .false.
            ctime = current_utc()
            gudb_get_last_complete_deployment = gudb_init_deployment()
            i = gdset%dcount
            do while (.not. found .and. i .ge. 1)
              if (gdset%deploy(i)%gbuoystates%bstate(gdset%deploy(i)%gbuoystates%scount)%state .eq. 'o' .or.
     *            gdset%deploy(i)%gbuoystates%bstate(gdset%deploy(i)%gbuoystates%scount)%state .eq. 'r' .or.
     *            is_before(gdset%deploy(i)%end_date,ctime)) then
                found = .true.
                gudb_get_last_complete_deployment = gdset%deploy(i)
              else
                i = i - 1
              end if
            end do
          end function


c-- GUDB_COUNT_DEPLOYMENTS_BY_STATION ------------------------------------------
c   Counts the number of deployments in the set for the given station.
c-------------------------------------------------------------------------------
          integer function gudb_count_deployments_by_station(gdset, search_stn)
            integer::                  i
            character*3                search_stn
            type(gudb_deployment_set)  gdset

            gudb_count_deployments_by_station = 0
            do i = 1, gdset%dcount 
              if (gdset%deploy(i)%gstation%cdip_id .eq. search_stn)
     *          gudb_count_deployments_by_station = gudb_count_deployments_by_station + 1
            end do
          end function


c-- GUDB_GET_DEPLOYMENT_BY_STATION ---------------------------------------------
c   Returns the deployment from a set which matches the given station. If more
c   than one deployment matches, the optional 'cnt' arg can be used.
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_deployment_by_station(gdset, search_stn, cnt)
            integer::                  count, i, num_found
            integer,optional::         cnt
            character*3                search_stn
            type(gudb_deployment_set)  gdset

            if (PRESENT(cnt)) then
              count = cnt
            else
              count = 1
            end if

            gudb_get_deployment_by_station = gudb_init_deployment()
            num_found = 0
            do i = 1, gdset%dcount 
              if (gdset%deploy(i)%gstation%cdip_id .eq. search_stn) then
                num_found = num_found + 1
                if (num_found .eq. count) then
                  gudb_get_deployment_by_station = gdset%deploy(i)
                  return
                end if
              end if
            end do
          end function


c-- GUDB_GET_DEPLOY_GAUGE_INDEX ------------------------------------------------
c   Returns the old-archive gauge index based on buoy type.
c-------------------------------------------------------------------------------
          integer function gudb_get_deploy_gauge_index(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gdeploy%gtophat%buoy_type .eq. 1) then
              gudb_get_deploy_gauge_index = 5
            else if (gdeploy%gtophat%buoy_type .eq. 2) then
              gudb_get_deploy_gauge_index = 6
            else if (gdeploy%gtophat%buoy_type .eq. 3) then
              gudb_get_deploy_gauge_index = 19
            else if (gdeploy%gtophat%buoy_type .eq. 4) then
              gudb_get_deploy_gauge_index = 24
            else if (gdeploy%gtophat%buoy_type .eq. 5) then
              gudb_get_deploy_gauge_index = 23
            else
              gudb_get_deploy_gauge_index = -1
            end if
          end function


c-- GUDB_GET_DEPLOY_DATA_INDEX -------------------------------------------------
c   Returns the old-archive data index based on buoy type.
c-------------------------------------------------------------------------------
          integer function gudb_get_deploy_data_index(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gdeploy%gtophat%buoy_type .le. 3 .or. gdeploy%gtophat%buoy_type .eq. 5) then
              gudb_get_deploy_data_index = 3
            else if (gdeploy%gtophat%buoy_type .eq. 4) then
              gudb_get_deploy_data_index = 15
            else
              gudb_get_deploy_data_index = -1
            end if
          end function


c-- GUDB_GET_DEPLOY_SAMPLE_RATE ------------------------------------------------
c   Gets the sample rate based on buoy_type
c-------------------------------------------------------------------------------
          real function gudb_get_deploy_sample_rate(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gudb_is_mk4_deployment(gdeploy)) then
              gudb_get_deploy_sample_rate = 2.56
            else 
              gudb_get_deploy_sample_rate = 1.28
            end if
          end function


c-- GUDB_GET_DEPLOY_SAMPLE_LENGTH ----------------------------------------------
c   Gets the sample length based on buoy_type
c-------------------------------------------------------------------------------
          integer function gudb_get_deploy_sample_length(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gudb_is_mk4_deployment(gdeploy)) then
              gudb_get_deploy_sample_length = 1800
            else 
              gudb_get_deploy_sample_length = 1600
            end if
          end function


c-- GUDB_GET_DEPLOY_WATCH_CIRCLE -----------------------------------------------
c   Calculates the watch circle radius in meters based on the water depth
c   and total mooring length. If the mooring length is not set, it assumed
c   to be twice the depth (i.e. a 2:1 scope).
c-------------------------------------------------------------------------------
          real function gudb_get_deploy_watch_circle(gdeploy)
            real                   moor_length
            type(gudb_deployment)  gdeploy

            if (gdeploy%gmooring%total_length .le. 0.0) then
              moor_length = 2.0 * gdeploy%depth
            else 
              moor_length = gdeploy%gmooring%total_length
            end if
            gudb_get_deploy_watch_circle = SQRT(moor_length**2 - gdeploy%depth**2)
          end function


c-- GUDB_IS_MK4_DEPLOYMENT -----------------------------------------------------
c   Returns true for DWR4 deployments
c-------------------------------------------------------------------------------
          logical function gudb_is_mk4_deployment(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gdeploy%gtophat%buoy_type .eq. 4) then
              gudb_is_mk4_deployment = .true.
            else 
              gudb_is_mk4_deployment = .false.
            end if
          end function


c-- GUDB_IS_DWRG_DEPLOYMENT -----------------------------------------------------
c   Returns true for DWR-G deployments
c-------------------------------------------------------------------------------
          logical function gudb_is_dwrg_deployment(gdeploy)
            type(gudb_deployment)  gdeploy

            if (gdeploy%gtophat%buoy_type .eq. 5) then
              gudb_is_dwrg_deployment = .true.
            else 
              gudb_is_dwrg_deployment = .false.
            end if
          end function


c-- GUDB_IS_SYNCED_DEPLOYMENT --------------------------------------------------
c   Returns true if firmware for the Mk3 deployment is a synced version.
c-------------------------------------------------------------------------------
          logical function gudb_is_synced_deployment(gdeploy)
            integer                fw_length, fw_version
            type(gudb_deployment)  gdeploy

            if (gdeploy%firmware .eq. 'null' .or. LEN_TRIM(gdeploy%firmware) .lt. 3) then
              gudb_is_synced_deployment = .false.
            else 
              fw_length = LEN_TRIM(gdeploy%firmware)
              read(gdeploy%firmware(fw_length-1:fw_length),'(i2)') fw_version
              if (fw_version .ge. 40 .and. fw_version .ne. 41) then
                gudb_is_synced_deployment = .true.
              else
                gudb_is_synced_deployment = .false.
              end if
            end if
          end function


c-- GUDB_GET_DEPLOYMENT_FROM_FILENAME ------------------------------------------
c   Returns a gudb_deployment corresponding to the given file
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_get_deployment_from_filename(fname, err_code, err_unit)
            integer           err_code, err_unit
            character*20      tophat
            character*(*)     fname
            type(date_block)  ftime

            gudb_get_deployment_from_filename = gudb_init_deployment()
            tophat = gudb_get_tophat_id_from_filename(fname)            
            ftime = gudb_get_time_from_filename(fname)            
            if (tophat .ne. '' .and. ftime%year .ne. 1900) then
              gudb_get_deployment_from_filename = gudb_load_deployment(tophat, ftime, err_code, err_unit)
            else 
              err_code = 1
            end if
          end function


c-- GUDB_PARSE_DEPLOYMENT ------------------------------------------------------
c   Parses deployment elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_deployment) function gudb_parse_deployment(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*1000      bstate_str, fix_str, hold_str, hull_str, mooring_str, station_str, tophat_str
            character*(*)       jstring

            gudb_parse_deployment = gudb_init_deployment()

            gudb_parse_deployment%deploy_num = json_get_field_integer(jstring, 'deploy_num', err_code)
            gudb_parse_deployment%depth = json_get_field_real(jstring, 'depth', err_code)
            gudb_parse_deployment%declination = json_get_field_real(jstring, 'mag_declination', err_code)
            gudb_parse_deployment%inclination = json_get_field_real(jstring, 'mag_inclination', err_code)
            gudb_parse_deployment%cat4_antenna = json_get_field_logical(jstring, 'CAT4_antenna', err_code)
            gudb_parse_deployment%acm_enabled = json_get_field_logical(jstring, 'ACM_enabled', err_code)
            gudb_parse_deployment%firmware = json_get_field_string(jstring, 'tophat_firmware', err_code)
            gudb_parse_deployment%deploy_site%lat = json_get_field_real(jstring, 'latitude', err_code)
            gudb_parse_deployment%deploy_site%long = json_get_field_real(jstring, 'longitude', err_code)
            gudb_parse_deployment%start_date = json_get_field_datetime(jstring, 'start_date', err_code)
            gudb_parse_deployment%end_date = json_get_field_datetime(jstring, 'end_date', err_code)

            station_str = json_get_field_string(jstring, 'station', err_code)
            gudb_parse_deployment%gstation =  gudb_parse_station(station_str, err_code, err_unit)
            hull_str = json_get_field_string(jstring, 'hull', err_code)
            gudb_parse_deployment%ghull =  gudb_parse_hull(hull_str, err_code, err_unit)
            mooring_str = json_get_field_string(jstring, 'mooring', err_code)
            gudb_parse_deployment%gmooring =  gudb_parse_mooring(mooring_str, err_code, err_unit)
            tophat_str = json_get_field_string(jstring, 'tophat', err_code)
            gudb_parse_deployment%gtophat =  gudb_parse_tophat(tophat_str, err_code, err_unit)
            bstate_str = json_get_field_string(jstring, 'buoy_state_deployment', err_code)
            gudb_parse_deployment%gbuoystates = gudb_parse_buoy_state_set(bstate_str, err_code, err_unit)
            call gudb_assign_buoy_state_end_dates(gudb_parse_deployment%gbuoystates, gudb_parse_deployment%end_date)
            hold_str = json_get_field_string(jstring, 'data_hold_deployment', err_code)
            gudb_parse_deployment%gdataholds = gudb_parse_data_hold_set(hold_str, err_code, err_unit)
            fix_str = json_get_field_string(jstring, 'processing_fix_deployment', err_code)
            gudb_parse_deployment%gprocfixes = gudb_parse_proc_fix_set(fix_str, err_code, err_unit)
          end function


c-- GUDB_PARSE_DEPLOYMENT_SET --------------------------------------------------
c   Parses a buoy deployment set from a json string
c-------------------------------------------------------------------------------
          type(gudb_deployment_set) function gudb_parse_deployment_set(jstring, jsize, err_code, err_unit)
            integer::           acount, err_unit, err_code, i, jsize
            character*5000      sarray(200)
            character*200000    jstring

            gudb_parse_deployment_set = gudb_init_deployment_set()

            call json_get_object_array(jstring(1:jsize), err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_deployment_set%deploy(i) = gudb_parse_deployment(sarray(i), err_code, err_unit)
            end do
            gudb_parse_deployment_set%dcount = acount
          end function


c-- GUDB_PARSE_DATA_HOLD_SET ---------------------------------------------------
c   Parses a data hold set from a json string
c-------------------------------------------------------------------------------
          type(gudb_data_hold_set) function gudb_parse_data_hold_set(jstring, err_code, err_unit)
            integer::           acount, err_unit, err_code, i
            character*(*)       jstring
            character*5000      sarray(10)

            gudb_parse_data_hold_set = gudb_init_data_hold_set()

            call json_get_object_array(jstring, err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_data_hold_set%dhold(i) = gudb_parse_data_hold(sarray(i), err_code, err_unit)
            end do
            gudb_parse_data_hold_set%hcount = acount
          end function


c-- GUDB_PARSE_DATA_HOLD -------------------------------------------------------
c   Parses a data hold from a json string
c-------------------------------------------------------------------------------
          type(gudb_data_hold) function gudb_parse_data_hold(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_data_hold = gudb_init_data_hold()

            gudb_parse_data_hold%start_date = json_get_field_datetime(jstring, 'start_date', err_code)
            gudb_parse_data_hold%end_date = json_get_field_datetime(jstring, 'end_date', err_code)
            gudb_parse_data_hold%acm = json_get_field_logical(jstring, 'acm', err_code)
            gudb_parse_data_hold%cat4 = json_get_field_logical(jstring, 'cat4', err_code)
            gudb_parse_data_hold%sst = json_get_field_logical(jstring, 'sst', err_code)
            gudb_parse_data_hold%wave = json_get_field_logical(jstring, 'wave', err_code)
          end function


c-- GUDB_CHECK_DATA_HOLD -------------------------------------------------------
c   Checks if a hold is on the given type and time; returns .true. if so.
c-------------------------------------------------------------------------------
          logical function gudb_check_data_hold(gholds, ctime, ctype)
            integer                   i
            character*(*)             ctype
            type(date_block)          ctime
            type(gudb_data_hold_set)  gholds

            gudb_check_data_hold = .false.
            do i = 1, gholds%hcount
              if (is_between(ctime, gholds%dhold(i)%start_date, gholds%dhold(i)%end_date)) then
                if ((ctype .eq. 'wave' .or. ctype .eq. 'upcross').and. gholds%dhold(i)%wave) 
     *            gudb_check_data_hold = .true.
                if (ctype .eq. 'sst' .and. gholds%dhold(i)%sst) gudb_check_data_hold = .true.
                if (ctype .eq. 'acm' .and. gholds%dhold(i)%acm) gudb_check_data_hold = .true.
                if (ctype .eq. 'cat4' .and. gholds%dhold(i)%cat4) gudb_check_data_hold = .true.
              end if
            end do
          end function


c-- GUDB_PARSE_PROC_FIX_SET ----------------------------------------------------
c   Parses a processing fix set from a json string
c-------------------------------------------------------------------------------
          type(gudb_proc_fix_set) function gudb_parse_proc_fix_set(jstring, err_code, err_unit)
            integer::           acount, err_unit, err_code, i
            character*(*)       jstring
            character*5000      sarray(10)

            gudb_parse_proc_fix_set = gudb_init_proc_fix_set()

            call json_get_object_array(jstring, err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_proc_fix_set%pfix(i) = gudb_parse_proc_fix(sarray(i), err_code, err_unit)
            end do
            gudb_parse_proc_fix_set%fcount = acount
          end function


c-- GUDB_PARSE_PROC_FIX --------------------------------------------------------
c   Parses a processing fix from a json string
c-------------------------------------------------------------------------------
          type(gudb_proc_fix) function gudb_parse_proc_fix(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_proc_fix = gudb_init_proc_fix()

            gudb_parse_proc_fix%start_date = json_get_field_datetime(jstring, 'start_date', err_code)
            gudb_parse_proc_fix%end_date = json_get_field_datetime(jstring, 'end_date', err_code)
            gudb_parse_proc_fix%fix_id = json_get_field_integer(jstring, 'fix', err_code)
          end function


c-- GUDB_CHECK_PROC_FIX --------------------------------------------------------
c   Checks if fixes are set for the given time; returns fix_ids if so.
c-------------------------------------------------------------------------------
          logical function gudb_check_proc_fix(gfixes, ctime, fix_id)
            integer                   fix_id, i
            type(date_block)          ctime
            type(gudb_proc_fix_set)   gfixes

            gudb_check_proc_fix = .false.
            do i = 1, gfixes%fcount
              if (gfixes%pfix(i)%fix_id .eq. fix_id .and. 
     *            is_between(ctime, gfixes%pfix(i)%start_date, gfixes%pfix(i)%end_date)) gudb_check_proc_fix = .true.
            end do
          end function


c-- GUDB_PARSE_BUOY_STATE_SET --------------------------------------------------
c   Parses a buoy state set from a json string
c-------------------------------------------------------------------------------
          type(gudb_buoy_state_set) function gudb_parse_buoy_state_set(jstring, err_code, err_unit)
            integer::           acount, err_unit, err_code, i
            character*(*)       jstring
            character*5000      sarray(4)

            gudb_parse_buoy_state_set = gudb_init_buoy_state_set()

            call json_get_object_array(jstring, err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_buoy_state_set%bstate(i) = gudb_parse_buoy_state(sarray(i), err_code, err_unit)
            end do
            gudb_parse_buoy_state_set%scount = acount
          end function


c-- GUDB_PARSE_BUOY_STATE ------------------------------------------------------
c   Parses a buoy state from a json string
c-------------------------------------------------------------------------------
          type(gudb_buoy_state) function gudb_parse_buoy_state(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_buoy_state = gudb_init_buoy_state()

            gudb_parse_buoy_state%state = json_get_field_string(jstring, 'buoy_state', err_code)
            gudb_parse_buoy_state%start_date = json_get_field_datetime(jstring, 'start_date', err_code)
          end function


c-- GUDB_GET_STATE_BY_TYPE -----------------------------------------------------
c   Return the state object from the given set matching the state type - 
c   [p]redeploy, [m]oored, [o]ffsite, [r]ecovered
c-------------------------------------------------------------------------------
          type(gudb_buoy_state) function gudb_get_state_by_type(bsset, state_char)
            integer::                  i
            character*(1)              state_char
            type(gudb_buoy_state_set)  bsset

            gudb_get_state_by_type = gudb_init_buoy_state()
            do i = 1, bsset%scount
              if (bsset%bstate(i)%state .eq. state_char) then
                gudb_get_state_by_type = bsset%bstate(i)
                return
              end if
            end do
          end function


c-- GUDB_GET_STATE_BY_INDEX ----------------------------------------------------
c   Return the state object from the given set matching the state index - 
c   0 -predeploy, 1 - moored, 2 - offsite, 3 - recovered
c-------------------------------------------------------------------------------
          type(gudb_buoy_state) function gudb_get_state_by_index(bsset, idx)
            integer::                  idx
            character*(1)              state_char
            type(gudb_buoy_state_set)  bsset

            gudb_get_state_by_index = gudb_init_buoy_state()
            if (idx .eq. 0) then
              state_char = 'p'
            else if (idx .eq. 1) then
              state_char = 'm'
            else if (idx .eq. 2) then
              state_char = 'o'
            else if (idx .eq. 3) then
              state_char = 'r'
            else
              return
            end if
            gudb_get_state_by_index = gudb_get_state_by_type(bsset, state_char)
          end function


c-- GUDB_GET_STATE_BY_DATE -----------------------------------------------------
c   Return the state object from the given set corresponding to the given date
c-------------------------------------------------------------------------------
          type(gudb_buoy_state) function gudb_get_state_by_date(bsset, search_date)
            integer::                  i
            type(date_block)           search_date
            type(gudb_buoy_state_set)  bsset

            gudb_get_state_by_date = gudb_init_buoy_state()
            do i = 1, bsset%scount
              if (is_between(search_date, bsset%bstate(i)%start_date, bsset%bstate(i)%end_date)) then
                gudb_get_state_by_date = bsset%bstate(i)
                return
              end if
            end do
          end function


c-- GUDB_GET_STATE_STRING ------------------------------------------------------
c   Returns a string representation of the given state: 'moored', 'offsite', etc
c-------------------------------------------------------------------------------
          character*10 function gudb_get_state_string(gstate, upper)
            logical,optional::    upper
            logical               upcase
            type(gudb_buoy_state) gstate

            upcase = .false.
            if (PRESENT(upper)) then
              if (upper .eqv. .true.) upcase = .true.
            end if

            if (gstate%state .eq. 'm') then
              gudb_get_state_string = 'moored'
              if (upcase) gudb_get_state_string = 'MOORED'
            else if (gstate%state .eq. 'o') then
              gudb_get_state_string = 'offsite'
              if (upcase) gudb_get_state_string = 'OFFSITE'
            else if (gstate%state .eq. 'p') then
              gudb_get_state_string = 'predeploy'
              if (upcase) gudb_get_state_string = 'PREDEPLOY'
            else if (gstate%state .eq. 'r') then
              gudb_get_state_string = 'recovered'
              if (upcase) gudb_get_state_string = 'RECOVERED'
            else 
              gudb_get_state_string = 'undefined'
              if (upcase) gudb_get_state_string = 'UNDEFINED'
            end if
          end function


c-- GUDB_GET_STATE_STREAM ------------------------------------------------------
c   Returns a stream representation of the given state: 'p0', 'p1', etc
c-------------------------------------------------------------------------------
          character*2 function gudb_get_state_stream(gstate)
            type(gudb_buoy_state) gstate

            if (gstate%state .eq. 'm') then
              gudb_get_state_stream = 'p1'
            else if (gstate%state .eq. 'o') then
              gudb_get_state_stream = 'p2'
            else if (gstate%state .eq. 'p') then
              gudb_get_state_stream = 'p0'
            else if (gstate%state .eq. 'r') then
              gudb_get_state_stream = 'p3'
            else 
              gudb_get_state_stream = 'p9'
            end if
          end function


c-- GUDB_GET_STATE_INDEX -------------------------------------------------------
c   Returns an integer representation of the given state: 0, 1, 2, 3 (or 9)
c-------------------------------------------------------------------------------
          integer function gudb_get_state_index(gstate)
            character*2           stream
            type(gudb_buoy_state) gstate

            stream = gudb_get_state_stream(gstate)
            read(stream(2:2),'(i1)') gudb_get_state_index
          end function


c-- GUDB_ASSIGN_BUOY_STATE_END_DATES -------------------------------------------
c   Assigns end dates to the buoy states in a set. The end date is the either 
c   the start of the next state, or, for the last, the end of the deployment.
c-------------------------------------------------------------------------------
          subroutine gudb_assign_buoy_state_end_dates(bsset, dep_edate)
            integer                     i
            type(date_block)            dep_edate
            type(gudb_buoy_state_set)   bsset

            do i = 1, bsset%scount
              if (i .lt. bsset%scount) then
                bsset%bstate(i)%end_date = subtract_seconds(bsset%bstate(i+1)%start_date, 1)
              else
                bsset%bstate(i)%end_date = dep_edate
              end if
            end do
          end subroutine


c-- GUDB_PARSE_HULL ------------------------------------------------------------
c   Parses hull elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_hull) function gudb_parse_hull(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_hull = gudb_init_hull()

            gudb_parse_hull%serial_no = json_get_field_string(jstring, 'serial_no', err_code)
            gudb_parse_hull%alloy = json_get_field_string(jstring, 'alloy', err_code)
            gudb_parse_hull%buoy_type = json_get_field_integer(jstring, 'buoy_type', err_code)
            gudb_parse_hull%diameter = json_get_field_real(jstring, 'diameter', err_code)
            gudb_parse_hull%is_acm = json_get_field_logical(jstring, 'current_profiler', err_code)
          end function


c-- GUDB_PARSE_MOORING ---------------------------------------------------------
c   Parses mooring elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_mooring) function gudb_parse_mooring(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_mooring = gudb_init_mooring()

            gudb_parse_mooring%total_length = json_get_field_real(jstring, 'total_length', err_code)
            gudb_parse_mooring%deploy_date = json_get_field_datetime(jstring, 'deploy_date', err_code)
            gudb_parse_mooring%float_type = json_get_field_string(jstring, 'float_type', err_code)
            gudb_parse_mooring%acoustic_release = json_get_field_string(jstring, 'acoustic_release', err_code)
            gudb_parse_mooring%marker_float = json_get_field_logical(jstring, 'marker_float', err_code)
          end function


c-- GUDB_PARSE_TOPHAT ----------------------------------------------------------
c   Parses tophat elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_tophat) function gudb_parse_tophat(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_tophat = gudb_init_tophat()

            gudb_parse_tophat%serial_no = json_get_field_string(jstring, 'serial_no', err_code)
            gudb_parse_tophat%buoy_type = json_get_field_integer(jstring, 'buoy_type', err_code)
            gudb_parse_tophat%gps_card_version = json_get_field_string(jstring, 'gps_card_version', err_code)
            gudb_parse_tophat%is_solar = json_get_field_logical(jstring, 'solar', err_code)
            gudb_parse_tophat%is_cat4 = json_get_field_logical(jstring, 'CAT4_modification', err_code)
          end function


c-- GUDB_PARSE_STATION ---------------------------------------------------------
c   Parses station elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_station) function gudb_parse_station(jstring, err_code, err_unit)
            integer::           err_unit, err_code, fcount, i, ocount
            character*5000      funder_strings(10), oper_strings(10)
            character*(*)       jstring

            gudb_parse_station = gudb_init_station()

            gudb_parse_station%cdip_id = json_get_field_string(jstring, 'cdip_id', err_code)
            do while (LEN_TRIM(gudb_parse_station%cdip_id) .lt. 3)
              gudb_parse_station%cdip_id = '0'//TRIM(gudb_parse_station%cdip_id)
            end do
            gudb_parse_station%wmo_id = json_get_field_string(jstring, 'wmo_id', err_code)
            gudb_parse_station%stn_name = json_get_field_string(jstring, 'name', err_code)
            gudb_parse_station%target_site%lat = json_get_field_real(jstring, 'target_latitude', err_code)
            gudb_parse_station%target_site%long = json_get_field_real(jstring, 'target_longitude', err_code)
            gudb_parse_station%target_depth = json_get_field_real(jstring, 'target_depth', err_code)
            gudb_parse_station%decomm_date = json_get_field_datetime(jstring, 'decomm_date', err_code)

            call json_get_field_string_array(jstring, 'funders', err_code, fcount, funder_strings)
            if (err_code .eq. 0) then
              do i = 1, fcount
                gudb_parse_station%funder_acronym(i) = funder_strings(i)(1:INDEX(funder_strings(i),' - ')-1)
                gudb_parse_station%funder_name(i) = 
     *            funder_strings(i)(INDEX(funder_strings(i),' - ')+3:LEN_TRIM(funder_strings(i)))
              end do
            end if

            call json_get_field_string_array(jstring, 'operators', err_code, ocount, oper_strings)
            if (err_code .eq. 0) then
              do i = 1, ocount
                gudb_parse_station%operator_acronym(i) = oper_strings(i)(1:INDEX(oper_strings(i),' - ')-1)
                gudb_parse_station%operator_name(i) = 
     *            oper_strings(i)(INDEX(oper_strings(i),' - ')+3:LEN_TRIM(oper_strings(i)))
              end do
            end if
          end function


c-- GUDB_PARSE_STATION_LIST ----------------------------------------------------
c   Parses a station list from a json string
c-------------------------------------------------------------------------------
          type(gudb_station_list) function gudb_parse_station_list(jstring_orig, jsize_orig, err_code, err_unit)
            integer::           acount, err_unit, err_code, i, idx, jsize_orig, jsize
            character*5000      sarray(1000)
            character*200000    jstring_orig, jstring

            gudb_parse_station_list = gudb_init_station_list()

            idx = INDEX(jstring_orig, '"results":')
            if (idx > 0) then
              idx = idx + 10
              jstring = jstring_orig(idx:jsize_orig-1)
              jsize = jsize_orig - idx
            else
              jstring = TRIM(jstring_orig)
              jsize = jsize_orig
            end if

            call json_get_object_array(jstring(1:jsize), err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_station_list%stn(i) = gudb_parse_station(sarray(i), err_code, err_unit)
            end do
            gudb_parse_station_list%scount = acount
          end function


c-- GUDB_LOAD_STN_SHOW ---------------------------------------------------------
c   Loads a gudb_tophat_show object for the given stn id. DEPRECATED!
c-------------------------------------------------------------------------------
          type(gudb_tophat_show) function gudb_load_stn_show(stn, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*3         stn
            character*200       url
            character*5000      show_string
            character(200000)   cvals

            gudb_load_stn_show = gudb_init_tophat_show()
            gudb_load_stn_show%cdip_id = stn

            call gudb_manage_request(4, cvals, csize, .true., err_code, err_unit, station=stn)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load stn show'
              return
            else
              show_string = json_get_field(cvals(1:csize), 'station_show', err_code)
              if (err_code .eq. 0) then
                gudb_load_stn_show%page_no_update = json_get_field_logical(show_string, 'page_no_update', err_code)
                gudb_load_stn_show%mail_offsite = json_get_field_logical(show_string, 'mail_offsite', err_code)
                gudb_load_stn_show%page_offsite = json_get_field_logical(show_string, 'page_offsite', err_code)
                gudb_load_stn_show%position_updates = json_get_field_string(show_string, 'position_updates', err_code)
                gudb_load_stn_show%notify_bitmask = json_get_field_integer(show_string, 
     *            'supplemental_notifications', err_code)
              end if
            end if
          end function


c-- GUDB_LOAD_TOPHAT_SHOW ------------------------------------------------------
c   Loads a gudb_tophat_show object for the given tophat id.
c-------------------------------------------------------------------------------
          type(gudb_tophat_show) function gudb_load_tophat_show(tophat, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*(*)       tophat
            character*200       url
            character*5000      show_string
            character(200000)   cvals

            gudb_load_tophat_show = gudb_init_tophat_show()
            gudb_load_tophat_show%tophat_id = tophat

            call gudb_manage_request(8, cvals, csize, .true., err_code, err_unit, tophatid=tophat)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load tophat show'
              return
            else
              show_string = cvals(1:csize)
              gudb_load_tophat_show = gudb_parse_tophat_show(show_string, err_code, err_unit)
            end if
          end function


c-- GUDB_PARSE_TOPHAT_SHOW -----------------------------------------------------
c   Parses show elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_tophat_show) function gudb_parse_tophat_show(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            logical             enabled
            character*1000      irid_str, net_str, rxc_str, ssh_str, stn_str, tophat_str
            character*(*)       jstring

            gudb_parse_tophat_show = gudb_init_tophat_show()

            tophat_str = json_get_field(jstring, 'tophat', err_code)
	    gudb_parse_tophat_show%tophat_id = json_get_field_string(tophat_str, 'serial_no', err_code)

            gudb_parse_tophat_show%page_no_update = json_get_field_logical(jstring, 'page_no_update', err_code)
            gudb_parse_tophat_show%mail_offsite = json_get_field_logical(jstring, 'mail_offsite', err_code)
            gudb_parse_tophat_show%page_offsite = json_get_field_logical(jstring, 'page_offsite', err_code)
            gudb_parse_tophat_show%position_updates = 
     *        json_get_field_string(jstring, 'position_updates', err_code)
            gudb_parse_tophat_show%notify_bitmask = 
     *        json_get_field_integer(jstring, 'supplemental_notifications', err_code)
          end function


c-- GUDB_LOAD_SHOW_LIST --------------------------------------------------------
c   Loads a list of gudb_tophat_shows holding all active show settings
c-------------------------------------------------------------------------------
          type(gudb_show_list) function gudb_load_show_list(err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(9, cvals, csize, .false., err_code, err_unit)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load show list'
              gudb_load_show_list = gudb_init_show_list()
              return
            else
              gudb_load_show_list = gudb_parse_show_list(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_PARSE_SHOW_LIST -------------------------------------------------------
c   Parses a show list from a json string
c-------------------------------------------------------------------------------
          type(gudb_show_list) function gudb_parse_show_list(jstring, jsize, err_code, err_unit)
            integer::           acount, err_unit, err_code, i, jsize
            character*5000      sarray(1000)
            character*200000    jstring

            gudb_parse_show_list = gudb_init_show_list()

            call json_get_object_array(jstring(1:jsize), err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_show_list%show(i) = gudb_parse_tophat_show(sarray(i), err_code, err_unit)
            end do
            gudb_parse_show_list%scount = acount
          end function


c-- GUDB_GET_SHOW_BY_TOPHAT ---------------------------------------------------
c   Returns the show entry from a list which matches the given tophat.
c-------------------------------------------------------------------------------
          type(gudb_tophat_show) function gudb_get_show_by_tophat(gshowlist, tophat)
            integer::                  i
            character*(*)              tophat
            type(gudb_show_list)       gshowlist

            gudb_get_show_by_tophat = gudb_init_tophat_show()
            do i = 1, gshowlist%scount 
              if (gshowlist%show(i)%tophat_id .eq. tophat) then
                gudb_get_show_by_tophat = gshowlist%show(i)
                return
              end if
            end do
          end function


c-- GUDB_LOAD_STN_COMM ---------------------------------------------------------
c   Loads a gudb_stn_comm object for the given stn id.
c-------------------------------------------------------------------------------
          type(gudb_stn_comm) function gudb_load_stn_comm(stn, err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*3         stn
            character(200000)   cvals

            gudb_load_stn_comm = gudb_init_stn_comm()
            gudb_load_stn_comm%cdip_id = stn

            call gudb_manage_request(6, cvals, csize, .true., err_code, err_unit, station=stn)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load stn comm'
              return
            else
              gudb_load_stn_comm = gudb_parse_stn_comm(cvals(1:csize), err_code, err_unit)
            end if
          end function


c-- GUDB_PARSE_STN_COMM --------------------------------------------------------
c   Parses comm elements from a json string
c-------------------------------------------------------------------------------
          type(gudb_stn_comm) function gudb_parse_stn_comm(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            logical             enabled
            character*1000      irid_str, net_str, rxc_str, ssh_str, stn_str, tophat_str
            character*(*)       jstring

            gudb_parse_stn_comm = gudb_init_stn_comm()

            tophat_str = json_get_field(jstring, 'tophat', err_code)
	    gudb_parse_stn_comm%tophat_id = json_get_field_string(tophat_str, 'serial_no', err_code)
            irid_str = json_get_field(tophat_str, 'iridium_comm', err_code)
            if (irid_str .ne. 'null') then
              enabled = json_get_field_logical(irid_str, 'enabled', err_code)
              if (enabled) gudb_parse_stn_comm%iridium_comm = .true.
              gudb_parse_stn_comm%iridium_mode = json_get_field_integer(irid_str, 'mode', err_code)
              gudb_parse_stn_comm%iridium_interval = json_get_field_integer(irid_str, 'call_interval_seconds', err_code)
            end if

            stn_str = json_get_field(jstring, 'station', err_code)
	    gudb_parse_stn_comm%cdip_id = json_get_field(stn_str, 'cdip_id', err_code)
            net_str = json_get_field(stn_str, 'net_comm', err_code)
            if (net_str .ne. 'null') then
              enabled = json_get_field_logical(net_str, 'enabled', err_code)
              if (enabled) gudb_parse_stn_comm%net_comm = .true.
            end if
            rxc_str = json_get_field(stn_str, 'rxc_comm', err_code)
            if (rxc_str .ne. 'null') then
              enabled = json_get_field_logical(rxc_str, 'enabled', err_code)
              if (enabled) gudb_parse_stn_comm%rxc_comm = .true.
            end if
            ssh_str = json_get_field(stn_str, 'ssh_comm', err_code)
            if (ssh_str .ne. 'null') then
              enabled = json_get_field_logical(ssh_str, 'enabled', err_code)
              if (enabled) gudb_parse_stn_comm%ssh_comm = .true.
            end if

            if (gudb_parse_stn_comm%net_comm .or. gudb_parse_stn_comm%rxc_comm .or. gudb_parse_stn_comm%ssh_comm) then
              gudb_parse_stn_comm%update_interval = 1800
              gudb_parse_stn_comm%update_coverage = 100.0
            else if (gudb_parse_stn_comm%iridium_comm .and. gudb_parse_stn_comm%iridium_mode .gt. 0) then
              gudb_parse_stn_comm%update_interval = gudb_parse_stn_comm%iridium_interval
              if (gudb_parse_stn_comm%iridium_mode .eq. 1) then
                gudb_parse_stn_comm%update_coverage = 2.08
              else if (gudb_parse_stn_comm%iridium_mode .eq. 9) then
                gudb_parse_stn_comm%update_coverage = 50.0
              else if (gudb_parse_stn_comm%iridium_mode .eq. 11) then
                gudb_parse_stn_comm%update_coverage = 12.5
              else
                gudb_parse_stn_comm%update_coverage = 100.0
              end if
            else
              gudb_parse_stn_comm%update_interval = 1800
              gudb_parse_stn_comm%update_coverage = 100.0
            end if
          end function


c-- GUDB_LOAD_COMM_LIST --------------------------------------------------------
c   Loads a list of gudb_stn_comms set holding all active comm settings
c-------------------------------------------------------------------------------
          type(gudb_comm_list) function gudb_load_comm_list(err_code, err_unit)
            integer::           csize, err_unit, err_code
            character*200       url
            character(200000)   cvals

            call gudb_manage_request(7, cvals, csize, .false., err_code, err_unit)
            if (err_code .ne. 0) then
              write(err_unit,'(a)') ' rest request failed: load comm list'
              gudb_load_comm_list = gudb_init_comm_list()
              return
            else
              gudb_load_comm_list = gudb_parse_comm_list(cvals, csize, err_code, err_unit)
            end if
          end function


c-- GUDB_PARSE_COMM_LIST -------------------------------------------------------
c   Parses a comm list from a json string
c-------------------------------------------------------------------------------
          type(gudb_comm_list) function gudb_parse_comm_list(jstring, jsize, err_code, err_unit)
            integer::           acount, err_unit, err_code, i, jsize
            character*5000      sarray(1000)
            character*200000    jstring

            gudb_parse_comm_list = gudb_init_comm_list()

            call json_get_object_array(jstring(1:jsize), err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_comm_list%comm(i) = gudb_parse_stn_comm(sarray(i), err_code, err_unit)
            end do
            gudb_parse_comm_list%ccount = acount
          end function


c-- GUDB_COUNT_COMMS_BY_STATION ------------------------------------------------
c   Counts the number of comm entries in the list for the given station.
c-------------------------------------------------------------------------------
          integer function gudb_count_comms_by_station(gcommlist, search_stn)
            integer::                  i
            character*3                search_stn
            type(gudb_comm_list)       gcommlist

            gudb_count_comms_by_station = 0
            do i = 1, gcommlist%ccount 
              if (gcommlist%comm(i)%cdip_id .eq. search_stn)
     *          gudb_count_comms_by_station = gudb_count_comms_by_station + 1
            end do
          end function


c-- GUDB_GET_COMM_BY_STATION ---------------------------------------------------
c   Returns the comm entry from a list which matches the given station. If more
c   than one comm matches, the optional 'cnt' arg can be used.
c-------------------------------------------------------------------------------
          type(gudb_stn_comm) function gudb_get_comm_by_station(gcommlist, search_stn, cnt)
            integer::                  count, i, num_found
            integer,optional::         cnt
            character*3                search_stn
            type(gudb_comm_list)       gcommlist

            if (PRESENT(cnt)) then
              count = cnt
            else
              count = 1
            end if

            gudb_get_comm_by_station = gudb_init_stn_comm()
            num_found = 0
            do i = 1, gcommlist%ccount 
              if (gcommlist%comm(i)%cdip_id .eq. search_stn) then
                num_found = num_found + 1
                if (num_found .eq. count) then
                  gudb_get_comm_by_station = gcommlist%comm(i)
                  return
                end if
              end if
            end do
          end function


c-- GUDB_GET_COMM_BY_TOPHAT ----------------------------------------------------
c   Returns the comm entry from a list which matches the given tophat.
c-------------------------------------------------------------------------------
          type(gudb_stn_comm) function gudb_get_comm_by_tophat(gcommlist, tophat)
            integer::                  i
            character*(*)              tophat
            type(gudb_comm_list)       gcommlist

            gudb_get_comm_by_tophat = gudb_init_stn_comm()
            do i = 1, gcommlist%ccount 
              if (gcommlist%comm(i)%tophat_id .eq. tophat) then
                gudb_get_comm_by_tophat = gcommlist%comm(i)
                return
              end if
            end do
          end function


c-- GUDB_PARSE_STNORDER_LIST ---------------------------------------------------
c   Parses a stnorder list from a json string
c-------------------------------------------------------------------------------
          type(gudb_stnorder_list) function gudb_parse_stnorder_list(jstring, jsize, err_code, err_unit)
            integer::           acount, err_unit, err_code, i, jsize
            character*5000      sarray(1000)
            character*200000    jstring

            gudb_parse_stnorder_list = gudb_init_stnorder_list()

            call json_get_object_array(jstring(1:jsize), err_code, acount, sarray)
            do i = 1, acount
              gudb_parse_stnorder_list%stn(i) = gudb_parse_stnorder_entry(sarray(i), err_code, err_unit)
            end do
            gudb_parse_stnorder_list%scount = acount
          end function


c-- GUDB_PARSE_STNORDER_ENTRY---------------------------------------------------
c   Parses stnorder stations from a json string
c-------------------------------------------------------------------------------
          type(character*3) function gudb_parse_stnorder_entry(jstring, err_code, err_unit)
            integer::           err_unit, err_code
            character*(*)       jstring

            gudb_parse_stnorder_entry = json_get_field_string(jstring, 'station', err_code)
            do while (LEN_TRIM(gudb_parse_stnorder_entry) .lt. 3)
              gudb_parse_stnorder_entry = '0'//TRIM(gudb_parse_stnorder_entry)
            end do
          end function


c-- GUDB_GET_REST_URL ----------------------------------------------------------
c   Reads the url for the REST metadata service from /project/WNC/GUDB_meta/
c-------------------------------------------------------------------------------
          character*100 function gudb_get_rest_url()
            integer         errcode
            character*100   config_file, rest_url

            gudb_get_rest_url = 'https://meta.cdipdata.org/api/'
            config_file = TRIM(GUDB_cache_dir)//'REST_url.dat'
            open(unit=99,file=TRIM(config_file),action='read',status='old',iostat=errcode)
            if (errcode .eq. 0) then
              read(99,'(a)',iostat=errcode) rest_url
              if (errcode .eq. 0) gudb_get_rest_url = rest_url
              close(99)
            end if
          end function


c-- GUDB_MANAGE_REQUEST --------------------------------------------------------
c   Constructs the url for a REST request and submits it, checking cached files
c   as needed. Request types (rtype):
c     1) station;  2) deployment; 3) dep history; 4) stn show; 5) dep snapshot
c     6) stn comm; 7) comm list; 8) tophat show; 9) show list; 10) hull;
c    11) dep window; 12) hull history; 13) tophat history; 14) stnorder list;
c    15) station from WMOid; 16) tophat; 17) full station list
c-------------------------------------------------------------------------------
          subroutine gudb_manage_request(rtype, cvals, csize, check_id, err_code, err_unit, station,
     *                 reqtime, tophatid, reqtime2, wmoid)
            integer::           csize, err_code, err_unit, rtype, time_diff
            logical             check_id
            character*14        time_label1, time_label2
            character*20        time_str1, time_str2
            character*100       cache_file
            character*200       url
            character(200000)   cvals
            character*3,optional::         station
            character*5,optional::         wmoid
            character*(*),optional::       tophatid	!* tophat or hull id

            type(date_block)               ctime
            type(date_block),optional::    reqtime, reqtime2
            type(gudb_deployment)          tdeploy

            if (GUDB_rest_url .eq. 'NULL') GUDB_rest_url = gudb_get_rest_url()
            call gudb_set_url_and_cache_file(url, cache_file, rtype, station,
     *                 reqtime, tophatid, reqtime2, wmoid)

            time_diff = HUGE(time_diff)
            if (rtype .eq. 2 .or. rtype .eq. 5 .or. rtype .eq. 11) then
              ctime = current_utc();
              time_diff = secs_diff(reqtime, ctime)
            end if

            call gudb_submit_request(url, cvals, csize, check_id, err_code)

            if (err_code .eq. 0) then
              if (rtype .ne. 2 .or. time_diff .lt. 3600*24) 
     *          call gudb_cache_response(cache_file, csize, cvals)
            else if (rtype .ne. 2 .or. err_code .eq. 1) then
              write(err_unit,'(a,i6)') ' - REST request failed, checking cache. err_code = ', err_code
              call gudb_retrieve_cache(cache_file, err_code, csize, cvals)
              if (err_code .eq. 0 .and. rtype .eq. 2) then
                tdeploy = gudb_parse_deployment(cvals(1:csize), err_code, 6)
                if (err_code .eq. 0 .and. (.not. is_between(reqtime, tdeploy%start_date, tdeploy%end_date))) 
     *            err_code = 100
              end if
              if (err_code .eq. 0) write(err_unit,'(a)') ' - metadata loaded from request cache'
            else 
              write(err_unit,'(a,i6)') ' - REST request failed, err_code = ', err_code
            end if
          end subroutine


c-- GUDB_MANAGE_CACHE_REQUEST --------------------------------------------------
c   Checks the cache for the requested metadata. If cache data is missing or
c   older than max_age, a new REST request will be made. (See above for 
c   rtype codes.) Set max_age to -1 to accept cache data of any age.
c-------------------------------------------------------------------------------
          subroutine gudb_manage_cache_request(rtype, max_age, cvals, csize, check_id, err_code, err_unit, station,
     *                 reqtime, tophatid, reqtime2, wmoid)
            integer::           cache_age, csize, err_code, err_unit, max_age, rtype, time_diff
            logical             check_id
            character*14        time_label1, time_label2
            character*20        time_str1, time_str2
            character*100       cache_file
            character*200       url
            character(200000)   cvals
            character*3,optional::         station
            character*5,optional::         wmoid
            character*(*),optional::       tophatid	!* tophat or hull id
            type(date_block)               ctime
            type(date_block),optional::    reqtime, reqtime2
            type(gudb_deployment)          tdeploy

            if (GUDB_rest_url .eq. 'NULL') GUDB_rest_url = gudb_get_rest_url()
            call gudb_set_url_and_cache_file(url, cache_file, rtype, station,
     *                 reqtime, tophatid, reqtime2, wmoid)

            if (max_age .gt. 0) then
              cache_age = gudb_get_cache_age(cache_file)
              if (cache_age .eq. -1) cache_age = HUGE(cache_age)
            end if
 
            if (max_age .lt. 0 .or. cache_age .lt. max_age) then
              call gudb_retrieve_cache(cache_file, err_code, csize, cvals)
              if (err_code .eq. 0 .and. rtype .eq. 2) then
                tdeploy = gudb_parse_deployment(cvals(1:csize), err_code, 6)
                if (err_code .eq. 0 .and. (.not. is_between(reqtime, tdeploy%start_date, tdeploy%end_date))) 
     *            err_code = 100
              end if
              if (err_code .eq. 0) then
c               write(err_unit,'(a)') ' - metadata loaded from request cache'
                return
              end if
            end if

c           write(err_unit,'(a)') ' - cache not found or outdated, checking gudb'
            call gudb_manage_request(rtype, cvals, csize, check_id, err_code, err_unit, station,
     *                 reqtime, tophatid, reqtime2, wmoid)
          end subroutine


c-- GUDB_GET_CACHE_AGE ---------------------------------------------------------
c  Returns the age of the given file in seconds, or -1 in the case of an error.
c-------------------------------------------------------------------------------
          integer function gudb_get_cache_age(cfile)
            integer            ecode, stat_array(13)
            character*100      cfile
            type(date_block)   ctime

            call STAT(cfile, stat_array, ecode)
            if (ecode .ne. 0) then
              gudb_get_cache_age = -1
            else
              ctime = current_utc()
              gudb_get_cache_age = date_to_timestamp(ctime) - stat_array(10)
            end if 
          end function
     


c-- GUDB_SET_URL_AND_CACHE_FILE ------------------------------------------------
c   Sets the url and cache file for a REST request. Request types (rtype):
c     1) station;  2) deployment; 3) dep history; 4) stn show; 5) dep snapshot
c     6) stn comm; 7) comm list; 8) tophat show; 9) show list; 10) hull;
c    11) dep window; 12) hull history; 13) tophat history; 14) stnorder list;
c    15) station from WMOid; 16) tophat; 17) full station list
c-------------------------------------------------------------------------------
          subroutine gudb_set_url_and_cache_file(url, cache_file, rtype, station,
     *                 reqtime, tophatid, reqtime2, wmoid)
            integer::           rtype
            character*14        time_label1, time_label2
            character*20        time_str1, time_str2
            character*100       cache_file
            character*200       url
            character*3,optional::         station
            character*5,optional::         wmoid
            character*(*),optional::       tophatid	!* tophat or hull id
            type(date_block),optional::    reqtime, reqtime2

            if (rtype .eq. 1) then
              url = TRIM(GUDB_rest_url)//'station_list/?cdip_id='//station//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/stations/'//station//'.json'
            else if (rtype .eq. 2) then
              time_str1 = write_iso_8601_date(reqtime)
              url = TRIM(GUDB_rest_url)//'deployment_list/?tophat__serial_no='//
     *          TRIM(tophatid)//'&span_date='//TRIM(time_str1)//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/deployments/'//TRIM(tophatid)//'_latest.json'
            else if (rtype .eq. 3) then
              url = TRIM(GUDB_rest_url)//'deployment_list/?station='//TRIM(station)//'&ordering=start_date&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/deployments/'//station//'.json'
            else if (rtype .eq. 4) then
              url = TRIM(GUDB_rest_url)//'show_list/?station='//station//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/shows/'//station//'.json'
            else if (rtype .eq. 5) then
              time_str1 = write_iso_8601_date(reqtime)
              url = TRIM(GUDB_rest_url)//'deployment_list/?span_date='//TRIM(time_str1)//'&format=json'
              time_label1 = make_datestring(reqtime)
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/deployments/'//time_label1(1:8)//'_snapshot.json'
            else if (rtype .eq. 6) then
              url = TRIM(GUDB_rest_url)//'comm_list/?station='//station//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/comms/'//station//'.json'
            else if (rtype .eq. 7) then
              url = TRIM(GUDB_rest_url)//'comm_list/?format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/comms/comm_list.json'
            else if (rtype .eq. 8) then
              url = TRIM(GUDB_rest_url)//'show_list/?tophat__serial_no='//TRIM(tophatid)//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/shows/'//TRIM(tophatid)//'.json'
            else if (rtype .eq. 9) then
              url = TRIM(GUDB_rest_url)//'show_list/?format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/shows/show_list.json'
            else if (rtype .eq. 10) then
              url = TRIM(GUDB_rest_url)//'dwhull_list/?serial_no='//TRIM(tophatid)//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/hulls/'//TRIM(tophatid)//'.json'
            else if (rtype .eq. 11) then
              time_str1 = write_iso_8601_date(reqtime)
              time_str2 = write_iso_8601_date(reqtime2)
              url = TRIM(GUDB_rest_url)//'deployment_list/?start_date__lte='//TRIM(time_str2)//'&end_date__gte='//
     *          TRIM(time_str1)//'&format=json'
              time_label1 = make_datestring(reqtime)
              time_label2 = make_datestring(reqtime2)
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/deployments/'//time_label1(1:8)//'-'//
     *          time_label2(1:8)//'_window.json'
            else if (rtype .eq. 12) then
              url = TRIM(GUDB_rest_url)//'deployment_list/?hull__serial_no='//TRIM(tophatid)//
     *          '&ordering=start_date&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/hulls/'//TRIM(tophatid)//'.json'
            else if (rtype .eq. 13) then
              url = TRIM(GUDB_rest_url)//'deployment_list/?tophat__serial_no='//TRIM(tophatid)//
     *          '&ordering=start_date&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/tophats/'//TRIM(tophatid)//'.json'
            else if (rtype .eq. 14) then
              url = TRIM(GUDB_rest_url)//'stnorder_list/?format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/stations/stnorder.json'
            else if (rtype .eq. 15) then
              url = TRIM(GUDB_rest_url)//'station_list/?wmo_id='//wmoid//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/stations/'//wmoid//'.json'
            else if (rtype .eq. 16) then
              url = TRIM(GUDB_rest_url)//'dwtophat_list/?serial_no='//TRIM(tophatid)//'&format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/tophats/'//TRIM(tophatid)//'.json'
            else if (rtype .eq. 17) then
              url = TRIM(GUDB_rest_url)//'station_list/?format=json'
              cache_file = TRIM(GUDB_cache_dir)//'request_cache/stations/full_stn_list.json'
            end if
          end subroutine


c-- GUDB_SUBMIT_REQUEST --------------------------------------------------------
c   Makes the rest request to the specficied url; wrapper for the c routine
c   'gudb_rest_request'
c-------------------------------------------------------------------------------
          subroutine gudb_submit_request(url, cvals, csize, check_id, err_code)
            integer::           csize, err_code
            logical             check_id
            character*200       url
            character(200000)   cvals

c           write(6,*) 'GUDB call: ', TRIM(url)
            cvals = REPEAT(' ',200000)
            call gudb_rest_request(url, cvals, csize, err_code)

            if (err_code .eq. 0 .and. csize .eq. 2 .and. cvals(1:2) .eq. '[]') then
              err_code = 0
            else if (err_code .eq. 0 .and. check_id .and. (INDEX(cvals(1:csize), 'cdip_id') .le. 0 .and.
     *          INDEX(cvals(1:csize), 'tophat') .le. 0)) then
              err_code = 2
            end if
            return
          end subroutine


c-- GUDB_CACHE_RESPONSE --------------------------------------------------------
c   Creates a text copy of the response to a REST request in the cache dir.
c-------------------------------------------------------------------------------
          subroutine gudb_cache_response(cache_file, csize, cvals)
            integer::           csize, ecode, funit
            character*100       cache_file
            character(200000)   cvals

            funit = 99
            open(funit, file=TRIM(cache_file), action='write', iostat=ecode)
            if (ecode .eq. 0) then
              write(funit, '(a)') cvals(1:csize)
              close(funit)
            end if
            return
          end subroutine


c-- GUDB_RETRIEVE_CACHE --------------------------------------------------------
c   Loads a .json file from the GUDB cache directory
c-------------------------------------------------------------------------------
          subroutine gudb_retrieve_cache(cache_file, ecode, csize, cvals)
            integer::           csize, ecode, funit
            character           c_char
            character*100       cache_file
            character(200000)   cvals

            funit = 99
            open(funit, file=TRIM(cache_file), action='read', status='old', iostat=ecode)
            if (ecode .eq. 0) then
              csize = 0
              read(funit, '(a1,$)', iostat=ecode) c_char
              do while (ecode .eq. 0)
                csize = csize + 1
                cvals(csize:csize) = c_char
                read(funit, '(a1,$)', iostat=ecode) c_char
              end do
              if (csize .gt. 1) ecode = 0
              close(funit)
            end if
            return
          end subroutine


c-- GUDB_PARSE_LOGICAL ---------------------------------------------------------
c   Assign a fortran logical value from json 'True', 'False' strings
c-------------------------------------------------------------------------------
          logical function gudb_parse_logical(l_str)
            character*(*)    l_str
            if (l_str .eq. 'True') then
              gudb_parse_logical = .true.
            else if (l_str .eq. 'False') then
              gudb_parse_logical = .false.
            end if
          end function


c-- GUDB_LOAD_WINDOWED_STNORDER_LIST -------------------------------------------
c   Lists stations with moored data in the given window, using CDIP's standard
c   stnorder ordering.
c-------------------------------------------------------------------------------
          subroutine gudb_load_windowed_stnorder_list(stime, etime, ordered_stns, ordered_count, 
     *                 ordered_lats, ordered_lons, ordered_depths, err_code)
            integer        active_count, err_code, i, idx(1), ordered_count
            real           active_depths(200), active_lats(200), active_lons(200)
            real           ordered_depths(200), ordered_lats(200), ordered_lons(200)
            character*3    active_stns(200), ordered_stns(200)
            type(date_block)            stime, etime
            type(gudb_deployment)       dep
            type(gudb_buoy_state)       mstate
            type(gudb_deployment_set)   gdset
            type(gudb_stnorder_list)    gstnorder

            gdset = gudb_load_deployment_window(stime, etime, err_code, 6)

            active_stns = '   '
            active_count = 0
            ordered_stns = '   '
            ordered_count = 0

            do i = 1, gdset%dcount
              dep = gdset%deploy(i)
              mstate = gudb_get_state_by_type(dep%gbuoystates, 'm')
              if (is_between(stime, mstate%start_date, mstate%end_date) .or. 
     *          is_between(etime, mstate%start_date, mstate%end_date) .or. 
     *          is_between(mstate%start_date, stime, etime)) then
                if (ANY(active_stns .eq. dep%gstation%cdip_id) .eqv. .false.) then
                  active_count = active_count + 1
                  active_stns(active_count) = dep%gstation%cdip_id
                  active_lats(active_count) = dep%deploy_site%lat
                  active_lons(active_count) = dep%deploy_site%long
                  active_depths(active_count) = dep%depth
                end if
              end if
            end do

            err_code = 0
            gstnorder = gudb_load_stnorder_list(err_code, 6)
            if (err_code .ne. 0) return

            do i = 1, gstnorder%scount
              if (ANY(active_stns .eq. gstnorder%stn(i))) then
                idx = MINLOC(active_lats, (active_stns .eq. gstnorder%stn(i)))
                ordered_count = ordered_count + 1
                ordered_stns(ordered_count) = gstnorder%stn(i)
                ordered_lats(ordered_count) = active_lats(idx(1))
                ordered_lons(ordered_count) = active_lons(idx(1))
                ordered_depths(ordered_count) = active_depths(idx(1))
              end if
            end do
          end subroutine


c-- GUDB_INIT_CACHE_ARGS -------------------------------------------------------
c   Sets the cache arguments for GUDB cache/rest requests
c-------------------------------------------------------------------------------
          subroutine gudb_init_cache_args(cache_call, max_age, use_cache, max_cache_age)
            integer             max_age
            integer,optional::  max_cache_age
            logical             cache_call
            logical,optional::  use_cache

            cache_call = .false.
            max_age = -1
            if (PRESENT(use_cache)) then
              if (use_cache .eqv. .true.) cache_call = .true.
              if (PRESENT(max_cache_age)) max_age = max_cache_age
            end if
          end subroutine

      end module
