c-- METADATA_UTILS -------------------------------------------------------------
c   The metadata_utils module contains methods for working with CF/ACDD/NCEI
c   standard metadata
c-------------------------------------------------------------------------------
        module metadata_utils

        use archive_info
        use dates
        use netcdf_utils
        use proc_streams
        use strings
        use unit_conversions
        use wmo_utils

        implicit none
        save
 
        integer,parameter::   META_max_atts = 100
        integer,parameter::   META_max_keys = 100
        integer,parameter::   META_max_vars = 20
        integer,parameter::   META_max_dims = 20
        integer,parameter::   META_array_max = 1000

        integer,parameter::   META_int_type = 1
        integer,parameter::   META_real_type = 2
        integer,parameter::   META_char_type = 3
        integer,parameter::   META_byte_type = 4
        integer,parameter::   META_dble_type = 5

        character*500,parameter:: META_default_comment = 'All values are '//
     *    'decoded directly from the instruments in accordance with the '//
     *    'manufacturers documentation EXCEPT for those with the attribute '//
     *    ':additional_processing which describes further data handling '//
     *    'performed by CDIP.'
        character*100,parameter:: META_default_ack = 'CDIP is primarily '//
     *    'supported by the U.S. Army Corps of Engineers (USACE). '

        character*100,parameter:: META_wave_keywords(10) = 
     *    (/"EARTH SCIENCE          ", "OCEANS                 ", 
     *      "OCEAN WAVES            ", "GRAVITY WAVES          ", 
     *      "WIND WAVES             ", "SIGNIFICANT WAVE HEIGHT", 
     *      "WAVE FREQUENCY         ", "WAVE PERIOD            ",
     *      "WAVE SPECTRA           ", "WIND WAVES             "/)

        character*100,parameter:: META_sst_keywords(5) = 
     *    (/"EARTH SCIENCE          ", "OCEANS                 ", 
     *      "OCEAN TEMPERATURE      ", "SEA SURFACE TEMPERATURE", 
     *      "WATER TEMPERATURE      "/)

        character*100,parameter:: META_wind_keywords(4) = 
     *    (/"EARTH SCIENCE", "OCEANS       ", "OCEAN WINDS  ", "SURFACE WINDS"/)

        character*100,parameter:: META_curr_keywords(4) = 
     *    (/"EARTH SCIENCE    ", "OCEANS           ", "OCEAN CIRCULATION", 
     *      "OCEAN CURRENTS   "/) 

        character*100,parameter:: META_airt_keywords(4) = 
     *    (/"EARTH SCIENCE          ", "ATMOSPHERE             ", 
     *      "ATMOSPHERIC TEMPERATURE", "SURFACE TEMPERATURE    "/) 

        type meta_dimension_list
          integer         dcount, dvalue(META_max_dims)
          character*100   dname(META_max_dims)
        end type

        type meta_attribute_list
          integer         acount, atype(META_max_atts)
          character*100   aname(META_max_atts)
          character*500   avalue(META_max_atts)
        end type

        type meta_variable_list
          integer                     vcount, vtype(META_max_vars), vsize(META_max_vars)
          integer                     varid(META_max_vars)
          character*100               vname(META_max_vars)
          character*100               vvalue(META_max_vars, META_array_max)
          type(meta_attribute_list)   vatts(META_max_vars)
        end type


        contains


c-- GENERATE_UUID --------------------------------------------------------------
c   Creates a Version 4 UUID from pseudo-random numbers based on the time.
c-------------------------------------------------------------------------------
          character*36 function generate_uuid()
            integer      i, seed(33), vhex
            real         rand(16)
            character*2  shex(16)

c--   Generate seed and 16 random numbers

            do i = 1, 16, 2
              call SYSTEM_CLOCK(seed(i))
              seed(i+1) = date_to_timestamp(current_utc())
            end do
            call RANDOM_SEED(put=seed)
            call RANDOM_NUMBER(rand)

c--   Convert random numbers to hex values, compose string

            do i = 1, 16
              vhex = 256 * rand(i)
              write(shex(i)(1:2),'(z2.2)') vhex
            end do

            generate_uuid = shex(1)//shex(2)//shex(3)//shex(4)//'-'//shex(5)//
     *        shex(6)//'-4'//shex(7)(2:2)//shex(8)//'-9'//shex(9)(2:2)//
     *        shex(10)//'-'//shex(11)//shex(12)//shex(13)//shex(14)//shex(15)//
     *        shex(16)
            return
          end function


c-- META_INITIALIZE_ATTRIBUTES -------------------------------------------------
c  Initializes the string object holding attributes used for all CDIP datasets.
c-------------------------------------------------------------------------------
          subroutine meta_initialize_attributes(atts)
            type(meta_attribute_list)   atts

            atts%acount = 0
            return
          end subroutine


c-- META_INITIALIZE_DIMENSIONS -------------------------------------------------
c  Initializes the object holding metadata dimension info
c-------------------------------------------------------------------------------
          subroutine meta_initialize_dimensions(mdims)
            type(meta_dimension_list)   mdims

            mdims%dcount = 0
            return
          end subroutine


c-- META_INITIALIZE_VARIABLES --------------------------------------------------
c  Initializes the object holding a metadata variable (name + attirbutes)
c-------------------------------------------------------------------------------
          subroutine meta_initialize_variables(mvars)
            integer                     i
            type(meta_variable_list)    mvars

            mvars%vcount = 0
            do i = 1, META_max_vars
              call meta_initialize_attributes(mvars%vatts(i))
            end do
            return
          end subroutine


c-- META_COUNT_ATTRIBUTES ------------------------------------------------------
c  Counts the number of attributes which have been set
c-------------------------------------------------------------------------------
          integer function meta_count_attributes(atts)
            type(meta_attribute_list)   atts
            
            meta_count_attributes = atts%acount
            return
          end function


c-- META_COUNT_VARIABLES -------------------------------------------------------
c  Counts the number of variables in a meta_variable array
c-------------------------------------------------------------------------------
          integer function meta_count_variables(mvars)
            type(meta_variable_list)    mvars

            meta_count_variables = mvars%vcount
            return
          end function


c-- META_ADD_ATTRIBUTE ---------------------------------------------------------
c  Adds an attribute to the given list
c-------------------------------------------------------------------------------
          subroutine meta_add_attribute(atts, aname, aval, atype)
            integer                     atype, i, idx
            character*(*)               aname, aval
            type(meta_attribute_list)   atts

            idx = -1
            do i = 1, atts%acount
              if (TRIM(atts%aname(i)) .eq. aname) then
                idx = i
              end if
            end do
            if (idx .eq. -1) then
              atts%acount = atts%acount + 1
              idx = atts%acount
            end if

            atts%aname(idx) = TRIM(aname)
            atts%avalue(idx) = TRIM(aval)
            atts%atype(idx) = atype
            return
          end subroutine


c-- META_REMOVE_ATTRIBUTE ------------------------------------------------------
c  Removes an attribute from the given list (att remains but will not be output)
c-------------------------------------------------------------------------------
          subroutine meta_remove_attribute(atts, aname)
            character*(*)               aname
            type(meta_attribute_list)   atts

            call meta_add_attribute(atts, aname, '', -1)
          end subroutine


c-- META_GET_ATTRIBUTE ---------------------------------------------------------
c  Returns the value of the given attribute, or '' if not found
c-------------------------------------------------------------------------------
          character*500 function meta_get_attribute(atts, aname)
            integer                     i
            character*(*)               aname
            type(meta_attribute_list)   atts

            meta_get_attribute = ''
            do i = 1, atts%acount
              if (TRIM(atts%aname(i)) .eq. aname) then
                meta_get_attribute = atts%avalue(i)
              end if
            end do
            return
          end function


c-- META_TIME_ATTLIST ----------------------------------------------------------
c  Sets the default attributes for a time variable
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_time_attlist()

            call meta_initialize_attributes(meta_time_attlist)
            call meta_add_attribute(meta_time_attlist, 'long_name', 'UTC time', META_char_type)
            call meta_add_attribute(meta_time_attlist, 'standard_name', 'time', META_char_type)
            call meta_add_attribute(meta_time_attlist, 'units', 'seconds since 1970-01-01 00:00:00 UTC', META_char_type)
            call meta_add_attribute(meta_time_attlist, 'calendar', 'standard', META_char_type)
            call meta_add_attribute(meta_time_attlist, 'axis', 'T', META_char_type)
            call meta_add_attribute(meta_time_attlist, 'coverage_content_type', NC_cctype_coord, META_char_type)
            return
          end function


c-- META_TIME_BOUNDS_ATTLIST ---------------------------------------------------
c  Sets the default attributes for a time bounds variable
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_time_bounds_attlist()

            call meta_initialize_attributes(meta_time_bounds_attlist)
            call meta_add_attribute(meta_time_bounds_attlist, 'long_name', 'time cell bounds', META_char_type)
            call meta_add_attribute(meta_time_bounds_attlist, 'units', 'seconds since 1970-01-01 00:00:00 UTC', 
     *        META_char_type)
            call meta_add_attribute(meta_time_bounds_attlist, 'calendar', 'standard', META_char_type)
            call meta_add_attribute(meta_time_bounds_attlist, 'coverage_content_type', NC_cctype_ainfo, META_char_type)
            return
          end function


c-- META_LATITUDE_ATTLIST ------------------------------------------------------
c  Sets the default attributes for a latitide variable
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_latitude_attlist(include_axis)
            logical   include_axis

            call meta_initialize_attributes(meta_latitude_attlist)
            call meta_add_attribute(meta_latitude_attlist, 'standard_name', 'latitude', META_char_type)
            call meta_add_attribute(meta_latitude_attlist, 'units', 'degrees_north', META_char_type)
            call meta_add_attribute(meta_latitude_attlist, 'valid_min', '-90.0', META_real_type)
            call meta_add_attribute(meta_latitude_attlist, 'valid_max', '90.0', META_real_type)
            if (include_axis) then
              call meta_add_attribute(meta_latitude_attlist, 'axis', 'Y', META_char_type)
              call meta_add_attribute(meta_latitude_attlist, 'coverage_content_type', NC_cctype_coord, META_char_type)
            else
              call meta_add_attribute(meta_latitude_attlist, 'coverage_content_type', NC_cctype_rinfo, META_char_type)
            end if
            call meta_add_attribute(meta_latitude_attlist, 'ncei_name', 'LATITUDE', META_char_type)
            return
          end function


c-- META_LONGITUDE_ATTLIST ------------------------------------------------------
c  Sets the default attributes for a longitude variable
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_longitude_attlist(include_axis)
            logical   include_axis

            call meta_initialize_attributes(meta_longitude_attlist)
            call meta_add_attribute(meta_longitude_attlist, 'standard_name', 'longitude', META_char_type)
            call meta_add_attribute(meta_longitude_attlist, 'units', 'degrees_east', META_char_type)
            call meta_add_attribute(meta_longitude_attlist, 'valid_min', '-180.0', META_real_type)
            call meta_add_attribute(meta_longitude_attlist, 'valid_max', '180.0', META_real_type)
            if (include_axis) then
              call meta_add_attribute(meta_longitude_attlist, 'axis', 'X', META_char_type)
              call meta_add_attribute(meta_longitude_attlist, 'coverage_content_type', NC_cctype_coord, META_char_type)
            else
              call meta_add_attribute(meta_longitude_attlist, 'coverage_content_type', NC_cctype_rinfo, META_char_type)
            end if
            call meta_add_attribute(meta_longitude_attlist, 'ncei_name', 'LONGITUDE', META_char_type)
            return
          end function


c-- META_DEPTH_ATTLIST ---------------------------------------------------------
c  Sets the default attributes for a depth variable
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_depth_attlist(use_sea_level)
            logical   use_sea_level

            call meta_initialize_attributes(meta_depth_attlist)
            if (use_sea_level) then
              call meta_add_attribute(meta_depth_attlist, 'standard_name', 
     *          'sea_floor_depth_below_sea_level', META_char_type)
            else
              call meta_add_attribute(meta_depth_attlist, 'standard_name', 
     *          'sea_floor_depth_below_sea_surface', META_char_type)
            end if
            call meta_add_attribute(meta_depth_attlist, 'long_name', 'water depth', META_char_type)
            call meta_add_attribute(meta_depth_attlist, 'units', 'meter', META_char_type)
            call meta_add_attribute(meta_depth_attlist, '_FillValue', NC_real_fill_string, META_real_type)
            call meta_add_attribute(meta_depth_attlist, 'ncei_name', 'DEPTH - BOTTOM', META_char_type)
            call meta_add_attribute(meta_depth_attlist, 'coverage_content_type', NC_cctype_rinfo, META_char_type)
            return
          end function


c-- META_ADD_VARIABLE ----------------------------------------------------------
c  Adds an variable to the given list
c-------------------------------------------------------------------------------
          subroutine meta_add_variable(mvars, vname, vval, vtype)
            integer                     vtype
            character*(*)               vname, vval
            type(meta_variable_list)    mvars

            mvars%vcount = mvars%vcount + 1
            mvars%vname(mvars%vcount) = TRIM(vname)
            mvars%vvalue(mvars%vcount,1) = TRIM(vval)
            mvars%vtype(mvars%vcount) = vtype
            mvars%vsize(mvars%vcount) = 1
            return
          end subroutine


c-- META_VARIABLE_EXISTS -------------------------------------------------------
c  Checks if a variable already exists
c-------------------------------------------------------------------------------
          logical function meta_variable_exists(mvars, vname)
            integer                     i
            character*(*)               vname
            type(meta_variable_list)    mvars

            meta_variable_exists = .false.
            do i = 1, mvars%vcount
              if (mvars%vname(i) .eq. vname) meta_variable_exists = .true.
            end do
          end function


c-- META_ADD_GRIDMAP -----------------------------------------------------------
c  Adds a metaGridMapping variable to the variable list
c-------------------------------------------------------------------------------
          subroutine meta_add_gridmap(mvars)
            type(meta_variable_list)    mvars

            call meta_add_variable(mvars, 'metaGridMapping', '_', META_char_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'grid_mapping_name',
     *        'latitude_longitude', META_char_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'longitude_of_prime_meridian',
     *        '0.0', META_real_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'semi_major_axis', '6378137.0', META_dble_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'inverse_flattening', '298.257223563', META_dble_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'coverage_content_type', NC_cctype_rinfo, META_char_type)
            return
          end subroutine


c-- META_ADD_DIMENSION ---------------------------------------------------------
c  Adds an dimension to the given list
c-------------------------------------------------------------------------------
          subroutine meta_add_dimension(mdims, dname, dval)
            integer                     dval
            character*(*)               dname
            type(meta_dimension_list)   mdims

            mdims%dcount = mdims%dcount + 1
            mdims%dname(mdims%dcount) = TRIM(dname)
            mdims%dvalue(mdims%dcount) = dval
            return
          end subroutine


c-- META_ADD_ARRAY_VARIABLE ----------------------------------------------------
c  Adds an variable to the given list
c-------------------------------------------------------------------------------
          subroutine meta_add_array_variable(mvars, vname, vval, vtype, vsize)
            integer                     i, vsize, vtype
            character*(*)               vname
            character*(100)             vval(vsize)
            type(meta_variable_list)    mvars

            mvars%vcount = mvars%vcount + 1
            mvars%vname(mvars%vcount) = TRIM(vname)
            do i = 1, vsize
              mvars%vvalue(mvars%vcount,i) = TRIM(vval(i))
            end do
            mvars%vtype(mvars%vcount) = vtype
            mvars%vsize(mvars%vcount) = vsize
            return
          end subroutine


c-- META_ASSIGN_MODEL_VARIABLES ------------------------------------------------
c  Assigns meta variables for model datasets.
c-------------------------------------------------------------------------------
          subroutine meta_assign_model_variables(mvars, site, lat, lon, depth, norm)
            real              lat, lon, depth
            real,optional::   norm
            character*(*)     site
            character*(20)    depth_str, lat_str, lon_str, norm_str
            type(meta_variable_list)    mvars

            call meta_add_variable(mvars, 'metaSiteLabel', site, META_char_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'long_name', 'site label', META_char_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), '_FillValue', '_', META_char_type)
            call meta_add_attribute(mvars%vatts(mvars%vcount), 'cf_role', 'timeseries_id', META_char_type)
            write(lat_str,'(f12.6)') lat
            call meta_add_variable(mvars, 'metaLatitude', TRIM(lat_str), META_real_type)
            mvars%vatts(mvars%vcount) = meta_latitude_attlist(.true.)
            if (lon .gt. 180.0) lon = lon - 360.0
            write(lon_str,'(f12.6)') lon
            call meta_add_variable(mvars, 'metaLongitude', TRIM(lon_str), META_real_type)
            mvars%vatts(mvars%vcount) = meta_longitude_attlist(.true.)
            write(depth_str,'(f8.2)') depth
            call meta_add_variable(mvars, 'metaWaterDepth', TRIM(depth_str), META_real_type)
            mvars%vatts(mvars%vcount) = meta_depth_attlist(.true.)
            if (PRESENT(norm)) then
              write(norm_str,'(f8.2)') norm
              call meta_add_variable(mvars, 'metaShoreNormal', TRIM(norm_str), META_real_type)
              call meta_add_attribute(mvars%vatts(mvars%vcount), 'long_name', 'shore normal', META_char_type)
              call meta_add_attribute(mvars%vatts(mvars%vcount), 'units', 'degrees_true', META_char_type)
              call meta_add_attribute(mvars%vatts(mvars%vcount), 'valid_min', '0.0', META_real_type)
              call meta_add_attribute(mvars%vatts(mvars%vcount), 'valid_max', '360.0', META_real_type)
              call meta_add_attribute(mvars%vatts(mvars%vcount), '_FillValue', NC_real_fill_string, META_real_type)
            end if

          end subroutine


c-- META_ASSIGN_STATIC_ATTRIBUTES ----------------------------------------------
c  Assigns attributes that are used for all CDIP datasets.
c-------------------------------------------------------------------------------
          subroutine meta_assign_static_attributes(atts, is_grid, is_xy_only)
            character*20                iso_date
            character*36                auuid
            logical                     gridded_data, xy_only
            logical,optional::          is_grid, is_xy_only
            type(meta_attribute_list)   atts
            type(date_block)            utc_time

            gridded_data = .false.
            if (PRESENT(is_grid)) then
              if (is_grid) gridded_data = .true.
            end if

            xy_only = .false.
            if (PRESENT(is_xy_only)) then
              if (is_xy_only) xy_only = .true.
            end if
            
            utc_time = current_utc()
            iso_date = write_iso_8601_date(utc_time)

c-  Attributes from Unidata's NetCDF Attribute Convention for Dataset Discovery

            call meta_add_attribute(atts, 'naming_authority', 'edu.ucsd.cdip', META_char_type)
            call meta_add_attribute(atts, 'keywords_vocabulary', 
     *        'Global Change Master Directory (GCMD) Earth Science Keywords', META_char_type)
            call meta_add_attribute(atts, 'date_created', iso_date, META_char_type)
            call meta_add_attribute(atts, 'date_issued', iso_date, META_char_type)
            call meta_add_attribute(atts, 'date_modified', iso_date, META_char_type)
            call meta_add_attribute(atts, 'creator_name', 'Coastal Data Information Program, SIO/UCSD', META_char_type)
            call meta_add_attribute(atts, 'creator_url', 'http://cdip.ucsd.edu', META_char_type)
            call meta_add_attribute(atts, 'creator_email', 'www@cdip.ucsd.edu', META_char_type)
            call meta_add_attribute(atts, 'creator_institution', 'Scripps Institution of Oceanography, UCSD', 
     *        META_char_type)
            call meta_add_attribute(atts, 'creator_country', 'USA', META_char_type)
            call meta_add_attribute(atts, 'creator_sector', 'academic', META_char_type)
            call meta_add_attribute(atts, 'publisher_name', 'Coastal Data Information Program, SIO/UCSD', 
     *        META_char_type)
            call meta_add_attribute(atts, 'publisher_url', 'http://cdip.ucsd.edu', META_char_type)
            call meta_add_attribute(atts, 'publisher_email', 'www@cdip.ucsd.edu', META_char_type)
            call meta_add_attribute(atts, 'publisher_country', 'USA', META_char_type)
            call meta_add_attribute(atts, 'publisher_institution', 'Scripps Institution of Oceanography, UCSD', 
     *        META_char_type)
            call meta_add_attribute(atts, 'institution', 
     *        'Scripps Institution of Oceanography, University of California San Diego', META_char_type)
            call meta_add_attribute(atts, 'project', 'Coastal Data Information Program (CDIP)', META_char_type)
            call meta_add_attribute(atts, 'processing_level', 
     *        'QA/QC information available at http://cdip.ucsd.edu/documentation', META_char_type)
            call meta_add_attribute(atts, 'standard_name_vocabulary', 'CF Standard Name Table v79', 
     *         META_char_type)
            call meta_add_attribute(atts, 'Conventions', 'ACDD-1.3, CF-1.8, IOOS-1.2', META_char_type)
            call meta_add_attribute(atts, 'license', 
     *        'These data may be redistributed and used without restriction.', META_char_type)

c-  Additional attributes from the NetCDF Climate and Forecast (CF) metdata conventions
c-  and the NCEI NetCDF template and guidance table

            if (gridded_data) then
              call meta_add_attribute(atts, 'cdm_data_type', 'Grid', META_char_type)
              call meta_add_attribute(atts, 'ncei_template_version', 'NCEI_NetCDF_Grid_Template_v2.0', META_char_type)
            else
              call meta_add_attribute(atts, 'cdm_data_type', 'Station', META_char_type)
              call meta_add_attribute(atts, 'featureType', 'timeSeries', META_char_type)
              if (xy_only .eqv. .false.) call meta_add_attribute(atts, 'ncei_template_version',
     *          'NCEI_NetCDF_TimeSeries_Orthogonal_Template_v2.0', META_char_type)
            end if

            call meta_add_attribute(atts, 'references', 'http://cdip.ucsd.edu/documentation', META_char_type)

            auuid = generate_uuid()
            call meta_add_attribute(atts, 'uuid', auuid, META_char_type)

            return
          end subroutine


c-- META_ASSIGN_ARCHIVE_ATTRIBUTES ---------------------------------------------
c  Assigns attributes based on info from CDIP's sensor archive
c-------------------------------------------------------------------------------
          subroutine meta_assign_archive_attributes(id, sdate, edate, recs, hist, 
     *                 atts, meta_vars, is_xy_only)
            integer              chan, chan_count, chans(PS_max_frames), errcode, i, label_length
            integer              recs, secs, var_count
            real                 lat_offset, lon_offset, maxlat, maxlon, minlat, minlon, watch_depth
            logical              found, xy_only, is_ndir_buoy, is_dir_buoy, is_mk4, multi_channel
            logical,optional::   is_xy_only
            character*(*)        id, hist
            character*2          strm
            character*3          stn
            character*5          wmo_id
            character*10         srecs, srate
            character*11         deplat, deplon, depth, declin
            character*14         edatestr, sdatestr
            character*19         spname
            character*20         time_res
            character*100        gauge_str, keywords(META_max_keys), meta_id, mlink
            character*100        serial_no, sname, vname, vatt
            character*500        ack, comm, doc_url, title, key_comp, contrib_names, contrib_roles
            character*500        serial_str, instrument_label
            type(date_block)          edate, sdate
            type(ai_time_frame)       tframe, temp_frame
            type(meta_attribute_list) atts
            type(meta_variable_list)  meta_vars

c--   Assemble needed archive information

            stn = id(1:3)
            strm = id(4:5)
            edatestr = make_datestring(edate)
            sdatestr = make_datestring(sdate)

            spname = 'sp'//id(1:5)//sdatestr(1:12)
            chans = 0
            call find_associated_channels(spname, chans, errcode, 6)
            chan_count = 0
            multi_channel = .false.
            do i = 1, PS_max_streams
              if (chans(i) .ne. 0 ) chan_count = chan_count + 1
            end do
            if (chan_count .gt. 1) multi_channel = .true.

            is_ndir_buoy = .false.
            is_dir_buoy = .false.
            is_mk4 = .false.
            xy_only = .false.
            if (PRESENT(is_xy_only)) then
              if (is_xy_only .eqv. .true.) xy_only = .true.
            end if

            call load_arch_file(stn, errcode, 6)
            call load_proc_file(stn, errcode, 6)
            i = 1
            found = .false.
            do while (i .le. chan_count .and. (.not. found))
              chan = chans(i)
              tframe = get_arch_frame(chan, edate, found, 6)
              if (.not. found) then
                tframe = get_arch_frame(chan, sdate, found, 6)
                if (.not. found) tframe = get_following_frame(chan, sdate, found, 6)
              end if
              if (found .and. is_after(tframe%start_date, edate)) found = .false.
              i = i + 1
            end do

            if (found .and. is_before(tframe%start_date, edate)) then
              if (is_directional_buoy(tframe)) is_dir_buoy = .true.
              if (tframe%data_index .eq. 2) is_ndir_buoy = .true.
              if (tframe%gauge_index .eq. 24) is_mk4 = .true.

c--   Calculate watch circle radius in degrees, bounding box. (Use 1.8*depth+200m for buoy padding.)

              if (is_dir_buoy .or. is_ndir_buoy) then
                watch_depth = tframe%water_depth
              else
                watch_depth = 1000.0
              end if
              lat_offset = (1.8*watch_depth+10000)/(111200.0 * 100.0)
              lon_offset = (1.8*watch_depth+10000)/(111200.0 * 100.0 * COS(to_radians(tframe%deploy_site%lat)))
              minlat = tframe%deploy_site%lat - lat_offset
              maxlat = tframe%deploy_site%lat + lat_offset
              minlon = tframe%deploy_site%long - lon_offset
              maxlon = tframe%deploy_site%long + lon_offset
              write(depth,'(i10)') NINT(tframe%water_depth/100.0)

c--   Global attributes from the ACDD

              if (is_dir_buoy .and. xy_only) then
                title = 'Sea surface displacement values'
              else if (is_mk4) then
                title = 'Directional wave, sea surface temperature, and surface current measurements'
              else if (is_dir_buoy) then
                title = 'Directional wave and sea surface temperature measurements'
              else if (multi_channel) then
                title = 'Directional wave measurements'
              else 
                title = 'Wave measurements'
              end if
              if (multi_channel) then
                gauge_str = 'pressure sensor array'
              else
                gauge_str = AI_gauge_types(tframe%gauge_index)
              end if
              title = TRIM(title)//' collected in situ by '//TRIM(gauge_str)//' located near '//
     *          TRIM(AI_data%stn_name)//' from '//write_sql_date(sdate)//' to '//write_sql_date(edate)//'.'
              call meta_add_attribute(atts, 'title', TRIM(title), META_char_type)

              write(srecs,'(i10)') recs
              write(srate,'(f5.3)') tframe%sample_rate
              if (xy_only) then
                title = TRIM(title)//' Continuous displacement records were sampled at '//TRIM(ADJUSTL(srate))//
     *            'Hz in a water depth of approximately '//TRIM(ADJUSTL(depth))//' meters.'
              else 
                title = TRIM(title)//' A total of '//TRIM(ADJUSTL(srecs))//' wave samples were '//
     *            'analyzed for this site, where the water depth is approximately '//TRIM(ADJUSTL(depth))//' meters.'
              end if

              call meta_add_attribute(atts, 'summary', TRIM(title), META_char_type)

              call meta_initialize_keywords(keywords)
              if (xy_only) then
                call meta_add_keywords(keywords, META_wave_keywords(1:5), 5)
              else
                call meta_add_keywords(keywords, META_wave_keywords, 10)
                if (is_dir_buoy) call meta_add_keywords(keywords, META_sst_keywords, 5)
                if (is_mk4) call meta_add_keywords(keywords, META_curr_keywords, 4)
              end if
              call meta_add_region_keywords(keywords, tframe%deploy_site)
              key_comp = meta_compile_keyword_list(keywords)
              call meta_add_attribute(atts, 'keywords', TRIM(key_comp), META_char_type)

              meta_id = 'CDIP_'//TRIM(id)//'_'//sdatestr(1:8)//'-'//edatestr(1:8)
              if (xy_only) meta_id = TRIM(meta_id)//'_xy'
              call meta_add_attribute(atts, 'id', TRIM(meta_id), META_char_type)
              call meta_add_attribute(atts, 'cdip_station_id', stn, META_char_type)
              call meta_add_attribute(atts, 'platform_id', id(1:5), META_char_type)
              wmo_id = get_wmo_id(stn)
              if (wmo_id .ne. 'NULL') then
                call meta_add_attribute(atts, 'wmo_id', TRIM(wmo_id), META_char_type)
                call meta_add_attribute(atts, 'wmo_platform_code', TRIM(wmo_id), META_char_type)
              end if

              hist = write_iso_8601_date(current_utc())//': dataset created; '//TRIM(hist)//
     *          ' If date_modified is after date_created, contact CDIP for details of changes.'
              call meta_add_attribute(atts, 'history', TRIM(hist), META_char_type)

              if (is_dir_buoy) then
                comm = META_default_comment
                call meta_add_attribute(atts, 'comment', TRIM(comm), META_char_type)
              end if

              ack = TRIM(META_default_ack)
              if (AI_data%funding(1:6) .eq. 'USACE/') ack = TRIM(ack)//' Station partner: '//
     *          TRIM(AI_data%funding(7:))//' ;'
              ack = TRIM(ack)//' Field operator: '//TRIM(AI_data%operator)
              call meta_add_attribute(atts, 'acknowledgment', TRIM(ack), META_char_type)

              mlink = 'http://cdip.ucsd.edu/metadata/'//TRIM(stn)//TRIM(strm)
              call meta_add_attribute(atts, 'metadata_link', TRIM(mlink), META_char_type)
              call meta_add_attribute(atts, 'infoUrl', TRIM(mlink), META_char_type)

              contrib_names = TRIM(AI_data%operator)//', '//TRIM(AI_data%funding)
              contrib_roles = 'station operation, station funding'
              if (INDEX(contrib_names,'CDIP') .le. 0) then
                contrib_names = TRIM(contrib_names)//', CDIP'
                contrib_roles = TRIM(contrib_roles)//', data management'
              end if
              call meta_add_attribute(atts, 'contributor_name', TRIM(contrib_names), META_char_type)
              call meta_add_attribute(atts, 'contributor_role', TRIM(contrib_roles), META_char_type)

              call meta_add_geospatial_atts(atts, minlat, maxlat, minlon, maxlon)

              call meta_add_attribute(atts, 'time_coverage_start', write_iso_8601_date(sdate), META_char_type)
              call meta_add_attribute(atts, 'time_coverage_end', write_iso_8601_date(edate), META_char_type)
              secs = secs_diff(sdate, edate)
              call meta_add_attribute(atts, 'time_coverage_duration', write_iso_8601_duration(secs, 1), META_char_type)
              if (xy_only) then
                time_res = 'P1S'
              else
                if (tframe%data_index .eq. 3) then
                  secs = 1800
                else
                  secs = secs / recs
                end if
                time_res = write_iso_8601_duration(secs, 2)
              end if
              call meta_add_attribute(atts, 'time_coverage_resolution', TRIM(time_res), META_char_type)

c--   Global attributes from the CF metdata conventions

              call meta_add_attribute(atts, 'source', 'insitu observations', META_char_type)

c--   Global attributes from the NCEI templates + CDIP extras

              instrument_label = ''
              serial_str = ''
              do i = 1, chan_count
                temp_frame = get_arch_frame(chans(i), sdate, found, 6)
                if (.not. found) temp_frame = get_following_frame(chan, sdate, found, 6)
                if (found .and. is_after(tframe%start_date, edate)) found = .false.
                if (found) then
                  instrument_label = TRIM(instrument_label)//TRIM(AI_gauge_types(temp_frame%gauge_index))//';'
                  serial_str = TRIM(serial_str)//TRIM(temp_frame%serial_number)//';'
                end if
              end do
              label_length = LEN_TRIM(instrument_label)-1
              instrument_label = replace_char(instrument_label, ' ', '_')
              sname = TRIM(AI_data%stn_name)//' '//TRIM(get_stream_name(strm))//' - '//stn//strm

              if (INDEX(instrument_label, 'buoy') .gt. 0) then
                call meta_add_attribute(atts, 'platform', 'wave_buoy', META_char_type)
                call meta_add_attribute(atts, 'platform_name', TRIM(sname), META_char_type)
                call meta_add_attribute(atts, 'platform_vocabulary', 'http://mmisw.org/ont/ioos/platform', 
     *            META_char_type)
              else
                call meta_add_attribute(atts, 'platform', 'metaPlatform', META_char_type)
              end if

              call meta_add_attribute(atts, 'instrument', 'metaInstrumentation', META_char_type)

              call meta_add_variable(meta_vars, 'metaStationName', TRIM(sname), META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'station name', META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'standard_name', 'platform_name', 
     *          META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'cf_role', 'timeseries_id', META_char_type)

              do i = 1, 2
                if (i .eq. 1) then
                  vname = 'metaPlatform'
                  vatt = 'platform_attributes'
                else
                  vname = 'metaInstrumentation'
                  vatt = 'instrumentation_attributes'
                end if
                call meta_add_variable(meta_vars, TRIM(vname), '_', META_char_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', TRIM(vatt), 
     *            META_char_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'make_model', 
     *            instrument_label(1:label_length), META_char_type)
                if (is_dir_buoy) then
                  serial_no = 'Hull: '//TRIM(tframe%serial_number)// '; hatchcover: '//TRIM(tframe%top_hat)
                  doc_url = 'http://datawell.nl'
                else
                  serial_no = serial_str(1:LEN_TRIM(serial_str)-1)
                  doc_url = 'http://cdip.ucsd.edu'
                end if
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'serial_number', serial_no, META_char_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'documentation_url', 
     *            TRIM(doc_url), META_char_type)
                if (is_dir_buoy .or. is_ndir_buoy) call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 
     *            'mooring', 'Datawell-compliant', META_char_type)
              end do
           
              write(deplat,'(f11.6)') tframe%deploy_site%lat
              write(deplon,'(f11.6)') tframe%deploy_site%long
              call meta_add_variable(meta_vars, 'metaDeployLatitude', deplat, META_real_type)
              meta_vars%vatts(meta_vars%vcount) = meta_latitude_attlist(.true.)
              call meta_add_variable(meta_vars, 'metaDeployLongitude', deplon, META_real_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'deployment longitude', 
     *          META_char_type)
              meta_vars%vatts(meta_vars%vcount) = meta_longitude_attlist(.true.)

              write(depth,'(f11.2)') tframe%water_depth/100.0
              call meta_add_variable(meta_vars, 'metaWaterDepth', depth, META_real_type)
              meta_vars%vatts(meta_vars%vcount) = meta_depth_attlist(.false.)

              if (is_dir_buoy) then
                write(declin,'(f7.2)') tframe%magnetic_var
                call meta_add_variable(meta_vars, 'metaDeclination', declin, META_real_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'magnetic declination', 
     *            META_char_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'units', 'degree', META_char_type)
              end if

c             make_model, serial_number, calibration_date, factory_calibrated, 
c             user_calibrated, calibration_report, accuracy, valid_range, and precision.
c
c             'sea_name'?

            end if
          end subroutine


c-- META_ASSIGN_MODEL_ATTRIBUTES -----------------------------------------------
c  Assigns attributes for MOP model output
c-------------------------------------------------------------------------------
          subroutine meta_assign_model_attributes(id, sdate, hours, gap, locs, loc_count, hist, init_file, atts)
            integer              errcode, gap, hours, i, loc_count, recs, secs
            real                 rmaxlat, rmaxlon, rminlat, rminlon
            character*(*)        id, hist
            character*10         srecs
            character*14         edatestr, sdatestr
            character*19         etimestr, stimestr
            character*100        init_file, keywords(META_max_keys), meta_id, mlink
            character*500        ack, comm, summ, title, key_comp, contrib_names, contrib_roles
            type(date_block)          edate, end_date_bound, sdate, start_date_bound
            type(location)            locs(*)
            type(meta_attribute_list) atts

c--   Assemble needed location info

            rminlat = 90.0
            rmaxlat = -90.0
            rminlon = 180.0
            rmaxlon = -180.0
            do i = 1, loc_count
              if (locs(i)%lat .gt. rmaxlat) rmaxlat = locs(i)%lat
              if (locs(i)%lat .lt. rminlat) rminlat = locs(i)%lat
              if (locs(i)%long .gt. rmaxlon) rmaxlon = locs(i)%long
              if (locs(i)%long .lt. rminlon) rminlon = locs(i)%long
            end do

            edate = add_seconds(sdate, (hours-1)*3600)
            sdatestr = make_datestring(sdate)
            edatestr = make_datestring(edate)
            stimestr = write_date(sdate)
            etimestr = write_date(edate)

            recs = INT(hours/gap) + 1

c--   Global attributes from the ACDD

            if (id(LEN_TRIM(id)-2:) .eq. '.nc') id(LEN_TRIM(id)-2:LEN_TRIM(id)) = '   '
            title = 'Bulk wave parameters and directional wave spectra as predicted by the '//
     *        'CDIP MOP version 1.1 model for the sites defined in '//TRIM(id)//'.'
            call meta_add_attribute(atts, 'title', TRIM(title), META_char_type)
            summ = 'The CDIP Monitoring and Prediction (MOP) System output of spectral wave predictions '//
     *        'for the sites defined in '//TRIM(id)//' from '//stimestr(1:16)//' to '//etimestr(1:16)//' UTC.'
            call meta_add_attribute(atts, 'summary', TRIM(summ), META_char_type)
            call meta_add_attribute(atts, 'contributor_name', 'CDIP', META_char_type)
            call meta_add_attribute(atts, 'contributor_role', 'model design and implementation', META_char_type)

            comm = "Details on the MOP wave model are provided in OReilly, W.C. et al (2016). "//
     *        'The California Coastal Wave Monitoring and Prediction System. Manuscript submitted to '//
     *        'Coastal Engineering.'
            call meta_add_attribute(atts, 'comment', TRIM(comm), META_char_type)

            call meta_initialize_keywords(keywords)
            call meta_add_keywords(keywords, META_wave_keywords, 10)
            call meta_add_region_keywords(keywords, locs(1))
            key_comp = meta_compile_keyword_list(keywords)
            call meta_add_attribute(atts, 'keywords', TRIM(key_comp), META_char_type)

            meta_id = 'MOPv1.1_'//TRIM(id)//'_'//sdatestr(1:10)//'-'//edatestr(1:10)
            call meta_add_attribute(atts, 'id', TRIM(meta_id), META_char_type)
            call meta_add_attribute(atts, 'MOP_initialization_file', TRIM(init_file), META_char_type)
            call meta_add_attribute(atts, 'MOP_output_site_definitions', TRIM(id), META_char_type)


c           call meta_add_attribute(atts, 'cdm_data_type', 'Station', META_char_type)

            hist = write_iso_8601_date(current_utc())//': dataset created; '//TRIM(hist)//
     *        ' If date_modified is after date_created, contact CDIP for details of changes.'
            call meta_add_attribute(atts, 'history', TRIM(hist), META_char_type)

c           comm = META_default_comment
c           call meta_add_attribute(atts, 'comment', TRIM(comm), META_char_type)

            call meta_add_attribute(atts, 'acknowledgment', TRIM(META_default_ack), META_char_type)

            mlink = 'http://cdip.ucsd.edu/MOP_v1.1/'
            call meta_add_attribute(atts, 'metadata_link', TRIM(mlink), META_char_type)
            call meta_add_attribute(atts, 'infoUrl', TRIM(mlink), META_char_type)

            call meta_add_geospatial_atts(atts, rminlat, rmaxlat, rminlon, rmaxlon)

            start_date_bound = subtract_seconds(sdate, 1800)
            end_date_bound = subtract_seconds(edate, -1800)
            call meta_add_attribute(atts, 'time_coverage_start', write_iso_8601_date(start_date_bound), META_char_type)
            call meta_add_attribute(atts, 'time_coverage_end', write_iso_8601_date(end_date_bound), META_char_type)
            secs = secs_diff(sdate, edate) + 3600
            call meta_add_attribute(atts, 'time_coverage_duration', write_iso_8601_duration(secs, 1), META_char_type)
            secs = gap*3600
            call meta_add_attribute(atts, 'time_coverage_resolution', write_iso_8601_duration(secs, 2), 
     *        META_char_type)

c--   Global attributes from the CF metdata conventions

            call meta_add_attribute(atts, 'source', 'model output', META_char_type)

          end subroutine


c-- META_ASSIGN_COMBO_ARCHIVE_ATTRIBUTES ---------------------------------------
c  Assigns attributes based on info from CDIP's sensor archive
c-------------------------------------------------------------------------------
          subroutine meta_assign_combo_archive_attributes(id, sdate, edate, recs, hist, 
     *                 atts, meta_vars, lats, lons, depths, declins, dep_count, minlat, maxlat,
     *                 minlon, maxlon, is_datawell, mdims, stn_lat, stn_lon)
            integer              chan, dep_count, errcode, i, label_length, recs
            integer              secs, var_count
            real                 lat_offset, lon_offset, minlat, maxlat, minlon, maxlon, stn_lat, stn_lon
            real                 declins(META_array_max), depths(META_array_max)
            real                 lats(META_array_max), lons(META_array_max)
            logical              found, is_datawell
            character*(*)        id, hist
            character*2          strm
            character*3          stn
            character*10         min_depth, max_depth, srecs
            character*11         depth, declin, stn_latstr, stn_lonstr
            character*14         edatestr, sdatestr
            character*19         edate_sql, sdate_sql
            character*20         maxlat_str, maxlon_str, minlat_str, minlon_str
            character*100        declinstrs(META_array_max), depthstrs(META_array_max)
            character*100        latstrs(META_array_max), lonstrs(META_array_max), meta_id
            character*100        depth_str, gauge_str, inst_label, keywords(META_max_keys), mlink, sname, vname, vatt
            character*500        ack, comm, title, key_comp, contrib_names, contrib_roles, stnlat_comment
            type(location)            loc
            type(date_block)          edate, sdate
            type(meta_attribute_list) atts
            type(meta_dimension_list) mdims
            type(meta_variable_list)  meta_vars

c--   Assemble needed archive information

            stn = id(1:3)
            strm = id(4:5)
            chan = 1
            edatestr = make_datestring(edate)
            sdatestr = make_datestring(sdate)
            edate_sql = write_sql_date(edate)
            sdate_sql = write_sql_date(sdate)

            call load_arch_file(stn, errcode, 6)
            call load_proc_file(stn, errcode, 6)

c--   Global attributes from the ACDD

            if (is_datawell) then
              if (dep_count .gt. 1) then
                gauge_str = 'Datawell Waverider buoys'
              else
                gauge_str = 'Datawell Waverider buoy'
              end if
              title = 'Directional wave and sea surface temperature'
            else
              gauge_str = 'sensors'
              title = 'Wave'
            end if
            title = TRIM(title)//' measurements collected in situ by '//TRIM(gauge_str)//' located near '//
     *        TRIM(AI_data%stn_name)//' from '//sdate_sql(1:10)//' to '//edate_sql(1:10)//'.'
            call meta_add_attribute(atts, 'title', TRIM(title), META_char_type)

            write(srecs,'(i10)') recs
            write(min_depth,'(i10)') NINT(MINVAL(depths(1:dep_count)))
            write(max_depth,'(i10)') NINT(MAXVAL(depths(1:dep_count)))
            if (min_depth .ne. max_depth) then
              depth_str = TRIM(ADJUSTL(min_depth))//' to '//TRIM(ADJUSTL(max_depth))
            else
              depth_str = TRIM(ADJUSTL(min_depth))
            end if
            title = TRIM(title)//' This dataset includes publicly-released data only, excluding all records flagged '//
     *        'bad by quality control procedures. '
            title = TRIM(title)//' A total of '//TRIM(ADJUSTL(srecs))//' wave samples were analyzed for this area, '//
     *        'where the water depth is approximately '//TRIM(depth_str)//' meters.'

            call meta_add_attribute(atts, 'summary', TRIM(title), META_char_type)

            loc = init_location(lats(1), lons(1))
            call meta_initialize_keywords(keywords)
            call meta_add_keywords(keywords, META_wave_keywords, 10)
            if (is_datawell) call meta_add_keywords(keywords, META_sst_keywords, 5)
            call meta_add_region_keywords(keywords, loc)
            key_comp = meta_compile_keyword_list(keywords)
            call meta_add_attribute(atts, 'keywords', TRIM(key_comp), META_char_type)

            meta_id = 'CDIP_'//TRIM(id)//'_'//sdatestr(1:8)//'-'//edatestr(1:8)//'_historic'
            call meta_add_attribute(atts, 'id', TRIM(meta_id), META_char_type)

            call meta_add_attribute(atts, 'cdm_data_type', 'Station', META_char_type)

            hist = write_iso_8601_date(current_utc())//': dataset created; '//TRIM(hist)//
     *        ' If date_modified is after date_created, contact CDIP for details of changes.'
            call meta_add_attribute(atts, 'history', TRIM(hist), META_char_type)

            comm = 'Multiple deployments may be included in this dataset. Please refer to the per-'//
     *        'deployment datasets - as referenced by sourceFilename and the xxxSourceIndex '//
     *        'variables - for more complete metadata.'
            if (is_datawell) comm = TRIM(comm)//' '//TRIM(META_default_comment)
            call meta_add_attribute(atts, 'comment', TRIM(comm), META_char_type)

            ack = TRIM(META_default_ack)
            if (AI_data%funding(1:6) .eq. 'USACE/') ack = TRIM(ack)//' Station partner: '//
     *        TRIM(AI_data%funding(7:))//' ;'
            ack = TRIM(ack)//' Field operator: '//TRIM(AI_data%operator)
            call meta_add_attribute(atts, 'acknowledgment', TRIM(ack), META_char_type)

            mlink = 'http://cdip.ucsd.edu/metadata/'//TRIM(stn)//TRIM(strm)
            call meta_add_attribute(atts, 'metadata_link', TRIM(mlink), META_char_type)
            call meta_add_attribute(atts, 'infoUrl', TRIM(mlink), META_char_type)

            contrib_names = TRIM(AI_data%operator)//', '//TRIM(AI_data%funding)
            contrib_roles = 'station operation, station funding'
            if (INDEX(contrib_names,'CDIP') .le. 0) then
              contrib_names = TRIM(contrib_names)//', CDIP'
              contrib_roles = TRIM(contrib_roles)//', data management'
            end if
            call meta_add_attribute(atts, 'contributor_name', TRIM(contrib_names), META_char_type)
            call meta_add_attribute(atts, 'contributor_role', TRIM(contrib_roles), META_char_type)

            call meta_add_geospatial_atts(atts, minlat, maxlat, minlon, maxlon)

            call meta_add_attribute(atts, 'time_coverage_start', write_iso_8601_date(sdate), META_char_type)
            call meta_add_attribute(atts, 'time_coverage_end', write_iso_8601_date(edate), META_char_type)
            secs = secs_diff(sdate, edate)
            call meta_add_attribute(atts, 'time_coverage_duration', write_iso_8601_duration(secs, 1), META_char_type)
            if (is_datawell) then
              secs = 1800
            else
              secs = secs / recs
            end if
            call meta_add_attribute(atts, 'time_coverage_resolution', write_iso_8601_duration(secs, 2), 
     *        META_char_type)

c--   Global attributes from the CF metdata conventions

            call meta_add_attribute(atts, 'source', 'insitu observations', META_char_type)

c--   Global attributes from the NCEI templates + CDIP extras

            inst_label = 'Multiple sensors possibly used. Please refer to per-deployment datasets.'
            label_length = LEN_TRIM(inst_label)
            call meta_add_attribute(atts, 'platform', 'metaPlatform', META_char_type)
            call meta_add_attribute(atts, 'instrument', 'metaInstrumentation', META_char_type)

            sname = TRIM(AI_data%stn_name)//' '//TRIM(get_stream_name(strm))//' - '//stn//strm
            call meta_add_variable(meta_vars, 'metaStationName', TRIM(sname), META_char_type)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'station name', META_char_type)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'cf_role', 'timeseries_id', META_char_type)

            write(stn_latstr,'(f11.6)') stn_lat
            write(stn_lonstr,'(f11.6)') stn_lon
            stnlat_comment = 'Averaged over all records. Record-specific deployment positions can be obtained from '//
     *        'metaDeployLatitude and metaDeployLongitude as indexed by waveSourceIndex, sstSourceIndex, etc.'
            call meta_add_variable(meta_vars, 'metaStationLatitude', stn_latstr, META_real_type)
            meta_vars%vatts(meta_vars%vcount) = meta_latitude_attlist(.true.)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 
     *        'station average latitude', META_char_type)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'comment', TRIM(stnlat_comment), META_char_type)
            call meta_add_variable(meta_vars, 'metaStationLongitude', stn_lonstr, META_real_type)
            meta_vars%vatts(meta_vars%vcount) = meta_longitude_attlist(.true.)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 
     *        'station average longitude', META_char_type)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'comment', TRIM(stnlat_comment), META_char_type)

            do i = 1, 2
              if (i .eq. 1) then
                vname = 'metaPlatform'
                vatt = 'platform_attributes'
              else
                vname = 'metaInstrumentation'
                vatt = 'instrumentation_attributes'
              end if
              call meta_add_variable(meta_vars, TRIM(vname), '_', META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', TRIM(vatt), 
     *          META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'make_model', 
     *          inst_label(1:label_length), META_char_type)
              if (is_datawell) then
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'documentation_url', 
     *            'http://datawell.nl', META_char_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'mooring', 'Datawell-compliant', 
     *            META_char_type)
              else
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'documentation_url', 
     *            'http://cdip.ucsd.edu', META_char_type)
              end if
            end do
           
            do i = 1, dep_count
              write(latstrs(i),'(f11.6)') lats(i)
              write(lonstrs(i),'(f11.6)') lons(i)
              write(depthstrs(i),'(f11.2)') depths(i)
              write(declinstrs(i),'(f7.2)') declins(i)
            end do
            call meta_initialize_dimensions(mdims)
            call meta_add_dimension(mdims, 'metaDeployCount', dep_count)
            call meta_add_array_variable(meta_vars, 'metaDeployLatitude', latstrs(1:dep_count), 
     *        META_real_type, dep_count)
            meta_vars%vatts(meta_vars%vcount) = meta_latitude_attlist(.false.)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'deployment latitude', 
     *        META_char_type)

            call meta_add_array_variable(meta_vars, 'metaDeployLongitude', lonstrs(1:dep_count), 
     *        META_real_type, dep_count)
            meta_vars%vatts(meta_vars%vcount) = meta_longitude_attlist(.false.)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'deployment longitude', 
     *        META_char_type)

            call meta_add_array_variable(meta_vars, 'metaWaterDepth', depthstrs(1:dep_count), META_real_type, dep_count)
            meta_vars%vatts(meta_vars%vcount) = meta_depth_attlist(.false.)

            call meta_add_array_variable(meta_vars, 'metaDeclination', declinstrs(1:dep_count), 
     *        META_real_type, dep_count)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'magnetic declination', 
     *        META_char_type)
            call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'units', 'degree', META_char_type)

          end subroutine


c-- META_INITIALIZE_KEYWORDS ---------------------------------------------------
c  Initializes the string object holding keywords used for all CDIP datasets.
c-------------------------------------------------------------------------------
          subroutine meta_initialize_keywords(keys)
            character*100      keys(META_max_keys)

            keys(:) = 'NULL'
            return
          end subroutine


c-- META_ADD_GEOSPATIAL_ATTS ---------------------------------------------------
c  Adds the geospatial extents and atts
c-------------------------------------------------------------------------------
          subroutine meta_add_geospatial_atts(atts, minlat, maxlat, minlon, maxlon)
            real             minlat, maxlat, minlon, maxlon
            character*20     minlat_str, maxlat_str, minlon_str, maxlon_str
            type(meta_attribute_list)    atts

            write(minlat_str,'(f20.10)') minlat
            write(maxlat_str,'(f20.10)') maxlat
            write(minlon_str,'(f20.10)') minlon
            write(maxlon_str,'(f20.10)') maxlon
            call meta_add_attribute(atts, 'geospatial_lat_min', minlat_str, META_real_type)
            call meta_add_attribute(atts, 'geospatial_lat_max', maxlat_str, META_real_type)
            call meta_add_attribute(atts, 'geospatial_lat_units', 'degrees_north', META_char_type)
            call meta_add_attribute(atts, 'geospatial_lat_resolution', '0.0001', META_real_type)
            call meta_add_attribute(atts, 'geospatial_lon_min', minlon_str, META_real_type)
            call meta_add_attribute(atts, 'geospatial_lon_max', maxlon_str, META_real_type)
            call meta_add_attribute(atts, 'geospatial_lon_units', 'degrees_east', META_char_type)
            call meta_add_attribute(atts, 'geospatial_lon_resolution', '0.0001', META_real_type)
            call meta_add_attribute(atts, 'geospatial_vertical_min', '0.0', META_real_type)
            call meta_add_attribute(atts, 'geospatial_vertical_max', '0.0', META_real_type)
            call meta_add_attribute(atts, 'geospatial_vertical_units', 'meters', META_char_type)
            call meta_add_attribute(atts, 'geospatial_vertical_origin', 'sea surface', META_char_type)
            call meta_add_attribute(atts, 'geospatial_vertical_positive', 'up', META_char_type)
            call meta_add_attribute(atts, 'geospatial_vertical_resolution', '1.0', META_real_type)
          end subroutine



c-- META_COUNT_KEYWORDS --------------------------------------------------------
c  Counts the number of keywords which have been set
c-------------------------------------------------------------------------------
          integer function meta_count_keywords(keys)
            integer           i
            character*100     keys(META_max_keys)

            i = 0
            do while (keys(i+1) .ne. 'NULL' .and. keys(i+1) .ne. '')
              i = i + 1
            end do
            meta_count_keywords = i
            return
          end function


c-- META_ADD_KEYWORDS ----------------------------------------------------------
c  Assigns keywords for a wave sensor
c-------------------------------------------------------------------------------
          subroutine meta_add_keywords(keys, add, add_count)
            integer,parameter::   wave_count = 10
            integer               add_count, i, key_count
            character*100   keys(META_max_keys), add(*)

            key_count = meta_count_keywords(keys)
            do i = 1, add_count
              if (.not. ANY(keys .eq. add(i))) then
                key_count = key_count + 1
                keys(key_count) = add(i)
              end if
            end do
            return
          end subroutine


c-- META_ADD_REGION_KEYWORDS ---------------------------------------------------
c  Assigns keyword for the given location
c-------------------------------------------------------------------------------
          subroutine meta_add_region_keywords(keys, site)
            integer               key_count
            character*100         keys(META_max_keys), add(META_max_keys)
            type(location)        site

            call meta_initialize_keywords(add)
            if (site%long .gt. 120.) then
              add(1:3) = (/"OCEAN                ", "PACIFIC OCEAN        ", 
     *                     "WESTERN PACIFIC OCEAN"/)
            else if (site%long .lt. -140. .and. site%lat .lt. 40.) then
              add(1:3) = (/"OCEAN                ", "PACIFIC OCEAN        ", 
     *                     "CENTRAL PACIFIC OCEAN"/)
            else if (site%long .lt. -120. .and. site%lat .gt. 40.) then
              add(1:3) = (/"OCEAN              ", "PACIFIC OCEAN      ", 
     *                     "NORTH PACIFIC OCEAN"/)
            else if (site%long .lt. -100. .and. site%lat .gt. 0.) then
              add(1:3) = (/"OCEAN                ", "PACIFIC OCEAN        ", 
     *                     "EASTERN PACIFIC OCEAN"/)
            else if (site%long .lt. -76. .and. site%lat .gt. 41. .and.
     *               site%long .gt. -93. .and. site%lat .lt. 49.) then
              add(1:4) = (/"CONTINENT               ", "NORTH AMERICA           ",
     *                     "UNITED STATES OF AMERICA", "GREAT LAKES             "/)
            else if (site%long .gt. -100. .and. site%lat .gt. 0.) then
              add(1:3) = (/"OCEAN               ", "ATLANTIC OCEAN      ", 
     *                     "NORTH ATLANTIC OCEAN"/)
            else if (site%long .gt. -70. .and. site%lat .lt. 0.) then
              add(1:3) = (/"OCEAN               ", "ATLANTIC OCEAN      ", 
     *                     "SOUTH ATLANTIC OCEAN"/)
            end if

            if (meta_count_keywords(add) .gt. 0) call meta_add_keywords(keys, 
     *        add, meta_count_keywords(add))
            return
          end subroutine


c-- META_COMPILE_KEYWORD_LIST --------------------------------------------------
c  Combines a keyword array into a single text string
c-------------------------------------------------------------------------------
          character*500 function meta_compile_keyword_list(keys)
            integer             i
            character*100       keys(META_max_keys)

            do i = 1, meta_count_keywords(keys)
              if (i .gt. 1) then
                meta_compile_keyword_list = TRIM(meta_compile_keyword_list)//
     *            ", "//TRIM(keys(i))
              else
                meta_compile_keyword_list = TRIM(keys(i))
              end if
            end do
            return
          end function


c-- META_ADD_STD_ATT_TO_LIST ---------------------------------------------------
c  Adds atts to a standard att list; helper for META_GET_STN_ATTRIBUTE_LIST
c-------------------------------------------------------------------------------
          subroutine meta_add_std_att_to_list(att_list, atype, aname, aval)
            integer                      atype, idx
            character*(*)                aname
            character*(*)                aval
            type(meta_attribute_list)    att_list

            att_list%acount = att_list%acount + 1
            att_list%atype(att_list%acount) = atype
            att_list%aname(att_list%acount) = aname
            att_list%avalue(att_list%acount) = aval
            return
          end subroutine


c-- META_GET_STD_ATTRIBUTE_LIST ------------------------------------------------
c  Returns the attributes standard to the given variable name
c-------------------------------------------------------------------------------
          type(meta_attribute_list) function meta_get_std_attribute_list(var_name)
            character*(*)               var_name

            call meta_initialize_attributes(meta_get_std_attribute_list)

            if (var_name == 'sourceFilename') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name',
     *        'source file name')
            if (var_name == 'waveTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'UTC sample start time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *       'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'waveTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *       'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'waveFlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *       'long_name', 'primary wave QC flag')
            if (var_name == 'waveFlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *       'long_name', 'secondary wave QC flag')
            if (var_name == 'waveHs') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'significant wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_significant_height')
            endif
            if (var_name == 'waveTp') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'peak wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_period_at_variance_spectral_density_maximum')
            endif
            if (var_name == 'waveTa') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'average wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', '
     *          sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment')
            endif
            if (var_name == 'waveDp') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'peak wave direction')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degreeT')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_from_direction')
            endif
            if (var_name == 'wavePeakPSD') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'peak wave power spectral density')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'm*m/hertz')
            endif
            if (var_name == 'waveTz') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'spectral zero-upcross wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_zero_upcrossing_period')
            endif
            if (var_name == 'wavePeakSpread') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'peak wave directional spread')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'waveTi') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'integral wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
            endif
            if (var_name == 'waveTe') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'energy wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment')
            endif
            if (var_name == 'waveTm13') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'sqrt(m1/m3) wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
            endif
            if (var_name == 'waveTc') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'crest wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
            endif
            if (var_name == 'waveInverseQp') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'inverse Godas peakedness')
            endif
            if (var_name == 'waveSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *        'long_name', 'source file index')
            if (var_name == 'waveFrequency') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'band center frequency')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'hertz')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'wave_frequency')
            endif
            if (var_name == 'waveFrequencyBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'frequency cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'hertz')
            endif
            if (var_name == 'waveFrequencyFlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *        'long_name', 'primary waveFrequency QC flag')
            if (var_name == 'waveFrequencyFlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *        'long_name', 'secondary waveFrequency QC flag')
            if (var_name == 'waveBandwidth') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'bandwidth')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'hertz')
            endif
            if (var_name == 'waveEnergyDensity') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'band energy density')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter^2 second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_variance_spectral_density')
            endif
            if (var_name == 'waveMeanDirection') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'band mean direction')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degreeT')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_from_direction')
            endif
            if (var_name == 'waveA1Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band a1 Fourier coefficient')
            if (var_name == 'waveB1Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band b1 Fourier coefficient')
            if (var_name == 'waveA2Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band a2 Fourier coefficient')
            if (var_name == 'waveB2Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band b2 Fourier coefficient')
            if (var_name == 'waveCheckFactor') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'band check factor (inverse of wave ellipticity)')
            if (var_name == 'waveSpread') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'band directional spread')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'waveM2Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band centered sine Fourier coefficient')
            if (var_name == 'waveN2Value') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'band centered cosine Fourier coefficient')
            if (var_name == 'sstTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'UTC sample time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'sstTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'sstFlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'primary sst QC flag')
            if (var_name == 'sstFlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'secondary sst QC flag')
            if (var_name == 'sstSeaSurfaceTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'sea surface temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'sea_surface_temperature')
            endif
            if (var_name == 'sstReferenceTemp') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'reference temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'sstSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'gpsTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'UTC sample time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'gpsTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'gpsLatitude') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'buoy latitude')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees_north')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'latitude')
            endif
            if (var_name == 'gpsLongitude') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'buoy longitude')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees_east')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'longitude')
            endif
            if (var_name == 'gpsSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'acmTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'UTC sample start time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'acmTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'acmFlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'primary acm QC flag')
            if (var_name == 'acmFlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'secondary acm QC flag')
            if (var_name == 'acmSpeed') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'current speed at 0.75m depth')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter/second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'sea_water_speed')
            endif
            if (var_name == 'acmSpeedStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'current speed standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter/second')
            endif
            if (var_name == 'acmDirection') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'current direction at 0.75m depth')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degreeT')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'direction_of_sea_water_velocity')
            endif
            if (var_name == 'acmDirectionStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'current direction standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'acmSignalStrength1') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'received signal strength (dBr), transducer 1')
            if (var_name == 'acmSignalStrength2') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'received signal strength (dBr), transducer 2')
            if (var_name == 'acmSignalStrength3') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'received signal strength (dBr), transducer 3')
            if (var_name == 'acmStatus') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'ACM status')
            if (var_name == 'acmSeaSurfaceTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'water temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'sea_surface_temperature')
            endif
            if (var_name == 'acmVerticalSpeed') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'vertical current speed at 0.75m depth')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter/second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'upward_sea_water_velocity')
            endif
            if (var_name == 'acmVerticalSpeedStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'vertical current speed standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter/second')
            endif
            if (var_name == 'acmSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'dwr4Time' .or. var_name == 'dwrTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'spectrum UTC start time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'dwr4TimeBounds' .or. var_name == 'dwrTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'spectrum time bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'dwr4Uptime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'uptime')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
            endif
            if (var_name == 'dwr4BatteryWeeksOfLife' .or. var_name == 'dwrBatteryWeeksOfLife') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'battery weeks of life')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'weeks')
            endif
            if (var_name == 'dwr4EnergyUsed') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'energy used from batteries')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'joule')
            endif
            if (var_name == 'dwr4SourceIndex' .or. var_name == 'dwrSourceIndex') call 
     *        meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'source file index')
            if (var_name == 'dwr4EnergyToBoostcaps') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'energy to boostcaps')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'joule')
            endif
            if (var_name == 'dwr4ZAccelMaxCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'vertical accelerometer threshold count')
            if (var_name == 'dwr4XAccelMaxCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'x-axis accelerometer threshold count')
            if (var_name == 'dwr4YAccelMaxCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'y-axis accelerometer threshold count')
            if (var_name == 'dwr4PitchMaxCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'pitch threshold count')
            if (var_name == 'dwr4RollMaxCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'roll threshold count')
            if (var_name == 'dwr4HatchTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'hatch electronics temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'dwr4BatteryVoltage') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'battery voltage')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'volt')
            endif
            if (var_name == 'dwrBatteryLevel') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *        'long_name', 'battery level')
            if (var_name == 'dwr4ZAccelerometerOffset' .or. var_name == 'dwrZAccelerometerOffset') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'vertical accelerometer offset')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter^2/second')
            endif
            if (var_name == 'dwr4XAccelerometerOffset' .or. var_name == 'dwrXAccelerometerOffset') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'x-axis accelerometer offset')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter^2/second')
            endif
            if (var_name == 'dwr4YAccelerometerOffset' .or. var_name == 'dwrYAccelerometerOffset') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'y-axis accelerometer offset')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter^2/second')
            endif
            if (var_name == 'dwrOrientation') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'buoy orientation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees')
            endif
            if (var_name == 'dwrInclination') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'magnetic inclination')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees')
            endif
            if (var_name == 'dwr4OrientStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'orientation standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'dwr4InclinMean') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'average inclination')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'dwr4InclinStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'inclination standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'dwr4MagFieldLengthMean') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'average magnetic field length')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'dwr4MagFieldLengthStdDev') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'magnetic field length standard deviation')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
            endif
            if (var_name == 'dwr4SensorTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'accelerometer temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'upcrossTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'nominal UTC start time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'upcrossTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'upcrossFlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'primary upcross QC flag')
            if (var_name == 'upcrossFlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'secondary upcross QC flag')
            if (var_name == 'upcrossSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'upcrossCrestCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'number of crests')
            if (var_name == 'upcrossWaveCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'number of waves')
            if (var_name == 'upcrossHavg') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'mean upcross wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_mean_height')
            endif
            if (var_name == 'upcrossHmax') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'maximum upcross wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_maximum_height')
            endif
            if (var_name == 'upcrossHsRMS') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'Hs estimate from upcross RMS wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
            endif
            if (var_name == 'upcrossHofTmax') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'height of Tmax wave')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
            endif
            if (var_name == 'upcrossTofHmax') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'period of Hmax wave')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_period_of_highest_wave')
            endif
            if (var_name == 'upcrossTz') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'average upcross wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_zero_upcrossing_period')
            endif
            if (var_name == 'upcrossTmax') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'maximum upcross wave period')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_maximum_period')
            endif
            if (var_name == 'upcrossBandwidth') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'bandwidth of upcross waves')
            if (var_name == 'upcrossCoverage') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'sample coverage')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'percent')
            endif
            if (var_name == 'upcrossHtenth') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'H1/10 upcross wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_mean_height_of_highest_tenth')
            endif
            if (var_name == 'upcrossHthird') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'H1/3 upcross wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_significant_height')
            endif
            if (var_name == 'upcrossTofHtenth') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'wave period of H1/10')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_mean_period_of_highest_tenth')
            endif
            if (var_name == 'upcrossTofHthird') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'wave period of H1/3')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'second')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_surface_wave_significant_period')
            endif
            if (var_name == 'upcrossQuantileHeight') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'quantiles of upcross wave height')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
            endif
            if (var_name == 'syncTime') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'spectrum UTC start time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'syncTimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'spectrum time bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'syncSourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'syncSegmentCount') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'segment count')
            if (var_name == 'syncSegmentsUsed') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'segments used')
            if (var_name == 'syncSamples') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'number of samples')
            if (var_name == 'syncLastDisplacements') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'final hex displacements')
            if (var_name == 'cat4Time') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'UTC sample time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'time')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'cat4TimeBounds') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'time cell bounds')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 
     *          'seconds since 1970-01-01 00:00:00 UTC')
            endif
            if (var_name == 'cat4FlagPrimary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'primary cat4 QC flag')
            if (var_name == 'cat4FlagSecondary') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'secondary cat4 QC flag')
            if (var_name == 'cat4AirTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 
     *          'air temperature at 2m above sea surface')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'air_temperature')
            endif
            if (var_name == 'cat4SourceIndex') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'source file index')
            if (var_name == 'cat4StatusFlags') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'cat4 status flags')
            if (var_name == 'cat4WhiteTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'white temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'cat4BlackTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'black temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'cat4MetalTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'metal temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'cat4GroovedTemperature') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'grooved temperature')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'Celsius')
            endif
            if (var_name == 'metaStationName') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'station name')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'platform_name')
            endif
            if (var_name == 'metaPlatform') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'platform_attributes')
            if (var_name == 'metaInstrumentation') call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 
     *          'long_name', 'instrumentation_attributes')
            if (var_name == 'metaDeployLatitude') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'latitude')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees_north')
            endif
            if (var_name == 'metaDeployLongitude') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 'longitude')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degrees_east')
            endif
            if (var_name == 'metaWaterDepth') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'standard_name', 
     *          'sea_floor_depth_below_sea_surface')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'water depth')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'meter')
            endif
            if (var_name == 'metaDeclination') then
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'long_name', 'magnetic declination')
              call meta_add_std_att_to_list(meta_get_std_attribute_list, 3, 'units', 'degree')
           endif
           return
          end function

c-- META_ADD_ATTRIBUTES_NC -----------------------------------------------------
c  Assigns netCDF attributes to the given grpid/varid.
c-------------------------------------------------------------------------------
        subroutine meta_add_attributes_nc(grpid, varid, atts)
          integer               ecode, i, grpid, varid, intval
          real                  realval
          real*8                dbleval
          type(meta_attribute_list)   atts

          ecode = nf90_redef(grpid)

          do i = 1, meta_count_attributes(atts)
            if (atts%atype(i) .eq. META_char_type) then
              call nc_call_func(nf90_put_att(grpid, varid, TRIM(atts%aname(i)), TRIM(atts%avalue(i))))
            else if (atts%atype(i) .eq. META_int_type) then
              read(atts%avalue(i),'(i50)') intval
              call nc_call_func(nf90_put_att(grpid, varid, TRIM(atts%aname(i)), intval))
            else if (atts%atype(i) .eq. META_real_type) then
              read(atts%avalue(i),'(f50.25)') realval
              call nc_call_func(nf90_put_att(grpid, varid, TRIM(atts%aname(i)), realval))
            else if (atts%atype(i) .eq. META_dble_type) then
              read(atts%avalue(i),'(f50.25)') dbleval
              call nc_call_func(nf90_put_att(grpid, varid, TRIM(atts%aname(i)), dbleval))
            end if
          end do
          return
        end subroutine


c-- META_ADD_GLOBAL_ATTRIBUTES_NC ----------------------------------------------
c  Assigns global netCDF attributes to the given id.
c-------------------------------------------------------------------------------
        subroutine meta_add_global_attributes_nc(grpid, atts)
          integer               grpid
          type(meta_attribute_list)   atts

          call meta_add_attributes_nc(grpid, NF90_GLOBAL, atts)
          return
        end subroutine


c-- META_DEF_METADATA_ARRAY_VARIABLES_NC ---------------------------------------
c  Defines netCDF metadata dimensions and array variables for the given id.
c-------------------------------------------------------------------------------
        subroutine meta_def_metadata_array_variables_nc(grpid, metavar, metadim)
          integer                    tmp_dimid, grpid, i, j
          character*100              dim_name
          type(meta_variable_list)   metavar
          type(meta_dimension_list)  metadim

          do i = 1, metadim%dcount
            if (metadim%dvalue(i) .gt. 1) then
              call nc_call_func(nf90_def_dim(grpid, metadim%dname(i), metadim%dvalue(i), tmp_dimid))
              do j = 1, metavar%vcount
                if (metavar%vsize(j) .eq. metadim%dvalue(i)) then
                  if (metavar%vtype(j) .eq. META_int_type) then
                    call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(j)), NF90_INT, tmp_dimid, 
     *                metavar%varid(j)))
                    call meta_add_attributes_nc(grpid, metavar%varid(j), metavar%vatts(j))
                  else if (metavar%vtype(j) .eq. META_real_type) then
                    call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(j)), NF90_FLOAT, tmp_dimid, 
     *                metavar%varid(j)))
                    call meta_add_attributes_nc(grpid, metavar%varid(j), metavar%vatts(j))
                  end if
                end if
              end do
            end if
          end do
          call meta_def_metadata_variables_nc(grpid, metavar)
        end subroutine


c-- META_DEF_METADATA_VARIABLES_NC ---------------------------------------------
c  Defines netCDF metadata variables and attributes for the given id.
c-------------------------------------------------------------------------------
        subroutine meta_def_metadata_variables_nc(grpid, metavar)
          integer                    char_length, char_dimid, grpid, i
          character*100              dim_name
          type(meta_variable_list)   metavar

          if (.not. meta_variable_exists(metavar, 'metaGridMapping')) call meta_add_gridmap(metavar)

          do i = 1, meta_count_variables(metavar)
            if (metavar%vsize(i) .eq. 1) then
              if (metavar%vtype(i) .eq. META_int_type) then
                call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_INT, varid=metavar%varid(i)))
              else if (metavar%vtype(i) .eq. META_real_type) then
                call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_FLOAT, varid=metavar%varid(i)))
              else if (metavar%vtype(i) .eq. META_byte_type) then
                call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_BYTE, varid=metavar%varid(i)))
              else 
                char_length = LEN_TRIM(metavar%vvalue(i,1))
                if (char_length .gt. 1) then
                  dim_name = TRIM(metavar%vname(i))//'Length'
                  call nc_call_func(nf90_def_dim(grpid, dim_name, char_length, char_dimid))
                  call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)),NF90_CHAR,char_dimid,metavar%varid(i)))
                else
                  call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_CHAR, varid=metavar%varid(i)))
                end if
              end if
              call meta_add_attributes_nc(grpid, metavar%varid(i), metavar%vatts(i))
            end if
          end do
          return
        end subroutine


c-- META_PUT_METADATA_VARIABLES_NC ---------------------------------------------
c  Fills netCDF metadata variables for the given id.
c-------------------------------------------------------------------------------
        subroutine meta_put_metadata_variables_nc(grpid, metavar)
          byte                       byteval
          integer                    grpid, i, j, varid, intval(1), count(1), start(1)
          real                       realval(1)
          character*100              dim_name
          type(meta_variable_list)   metavar

          count(1) = 1
          do i = 1, meta_count_variables(metavar)
            start(1) = 1
            do j = 1, metavar%vsize(i)
              if (metavar%vtype(i) .eq. META_int_type) then
                read(metavar%vvalue(i,j),'(i50)') intval(1)
                call nc_call_func(nf90_put_var(grpid, metavar%varid(i), intval, start, count))
              else if (metavar%vtype(i) .eq. META_real_type) then
                read(metavar%vvalue(i,j),'(f50.25)') realval(1)
                call nc_call_func(nf90_put_var(grpid, metavar%varid(i), realval, start, count))
              else if (metavar%vtype(i) .eq. META_byte_type) then
                read(metavar%vvalue(i,j),'(i5)') byteval
                call nc_call_func(nf90_put_var(grpid, metavar%varid(i), byteval))
              else 
                call nc_call_func(nf90_put_var(grpid, metavar%varid(i), TRIM(metavar%vvalue(i,j))))
              end if
              start(1) = start(1) + 1
            end do
          end do
          return
        end subroutine

      end module
