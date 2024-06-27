c-- GUDB_METADATA_UTILS --------------------------------------------------------
c   The metadata_utils module contains methods for working with CF/ACDD/NCEI
c   standard metadata - UPDATED TO USE GUDB rather than .arch/.proc
c-------------------------------------------------------------------------------
        module gudb_metadata_utils

        use gudb_objects
        use metadata_utils

        implicit none
        save
 

        contains


c-- META_ASSIGN_GUDB_ATTRIBUTES ------------------------------------------------
c  Assigns attributes based on info from CDIP's gudb_deploy object.
c-------------------------------------------------------------------------------
          subroutine meta_assign_gudb_attributes(gdeploy, sdate, edate, recs, hist, 
     *                 atts, meta_vars, is_xy_only, is_zcrossing)
            integer              errcode, i
            integer              recs, secs, var_count
            real                 lat_offset, lon_offset, maxlat, maxlon, minlat, minlon, srate
            real                 padding, watch_circle_radius
            logical              xy_only, zcrossing
            logical,optional::   is_xy_only, is_zcrossing
            character*(*)        hist
            character*2          strm
            character*3          stn
            character*4          hull_size_str
            character*5          stn5, wmo_id
            character*10         dep_id, mlength, srecs, srate_str
            character*11         deplat, deplon, depth, declin
            character*14         edatestr, sdatestr
            character*20         time_res
            character*100        funder, keywords(META_max_keys), meta_id, mlink, mooring, operator, partner
            character*100        serial_no, sname, vname, vatt
            character*500        ack, comm, doc_url, title, key_comp, contrib_names, contrib_roles
            character*500        instrument_label
            type(date_block)          edate, sdate, state_date
            type(gudb_buoy_state)     gstate
            type(gudb_deployment)     gdeploy
            type(meta_attribute_list) atts
            type(meta_variable_list)  meta_vars

              xy_only = .false.
              if (PRESENT(is_xy_only)) then
                if (is_xy_only .eqv. .true.) xy_only = .true.
              end if

              zcrossing = .false.
              if (PRESENT(is_zcrossing)) then
                if (is_zcrossing .eqv. .true.) zcrossing = .true.
              end if

c--   Assemble needed archive information

              stn = gdeploy%gstation%cdip_id
              if (zcrossing) then
                state_date = add_seconds(sdate, 500)
                gstate = gudb_get_state_by_date(gdeploy%gbuoystates, state_date)
              else
                gstate = gudb_get_state_by_date(gdeploy%gbuoystates, sdate)
              end if
              strm = gudb_get_state_stream(gstate)

              edatestr = make_datestring(edate)
              sdatestr = make_datestring(sdate)

c--   Calculate watch circle radius in degrees, bounding box. (Use 100m/200m for 
c--   buoy padding for buoys under/over 400m depth.)

              watch_circle_radius = gudb_get_deploy_watch_circle(gdeploy)
              padding = 100.0
              if (gdeploy%depth .ge. 400.0) padding = 200.0
              lat_offset = (watch_circle_radius + padding) / (111200.0)
              lon_offset = (watch_circle_radius + padding) / (111200.0 * COS(to_radians(gdeploy%deploy_site%lat)))
              minlat = gdeploy%deploy_site%lat - lat_offset
              maxlat = gdeploy%deploy_site%lat + lat_offset
              minlon = gdeploy%deploy_site%long - lon_offset
              maxlon = gdeploy%deploy_site%long + lon_offset
              write(depth,'(i10)') NINT(gdeploy%depth)

c--   Global attributes from the ACDD

              if (stn .eq. 'CAL') then
                title = 'Buoy validation measurements'
              else if (zcrossing) then
                title = 'Zero-crossing wave records derived from sea surface displacement values'
              else if (xy_only) then
                title = 'Sea surface displacement values'
              else if (gdeploy%acm_enabled .and. gdeploy%cat4_antenna) then
                title = 'Directional wave, sea surface temperature, air temperature, and surface current measurements'
              else if (gdeploy%acm_enabled) then
                title = 'Directional wave, sea surface temperature, and surface current measurements'
              else if (gdeploy%cat4_antenna) then
                title = 'Directional wave, sea surface temperature, and air temperature measurements'
              else 
                title = 'Directional wave and sea surface temperature measurements'
              end if
              instrument_label = 'Datawell '//TRIM(GUDB_buoy_types(gdeploy%gtophat%buoy_type))//
     *          ' directional buoy'	!* FIX
              title = TRIM(title)//' collected in situ by '//TRIM(instrument_label)//' located near '//
     *          TRIM(gdeploy%gstation%stn_name)//' from '//write_sql_date(sdate)//' to '//write_sql_date(edate)//'.'
              call meta_add_attribute(atts, 'title', TRIM(title), META_char_type)

              write(srecs,'(i10)') recs
              srate = gudb_get_deploy_sample_rate(gdeploy)    !* FIX
              write(srate_str,'(f5.3)') srate
              if (xy_only .or. zcrossing) then
                title = TRIM(title)//' Continuous displacement records were sampled at '//TRIM(ADJUSTL(srate_str))//
     *            'Hz in a water depth of approximately '//TRIM(ADJUSTL(depth))//' meters.'
              else if (stn .ne. 'CAL') then
                title = TRIM(title)//' A total of '//TRIM(ADJUSTL(srecs))//' wave samples were '//
     *            'analyzed for this site, where the water depth is approximately '//TRIM(ADJUSTL(depth))//' meters.'
              end if

              call meta_add_attribute(atts, 'summary', TRIM(title), META_char_type)

              call meta_initialize_keywords(keywords)
              if (xy_only .or. zcrossing) then
                call meta_add_keywords(keywords, META_wave_keywords(1:5), 5)
              else
                call meta_add_keywords(keywords, META_wave_keywords, 10)
                call meta_add_keywords(keywords, META_sst_keywords, 5)
                if (gdeploy%acm_enabled) call meta_add_keywords(keywords, META_curr_keywords, 4)
                if (gdeploy%cat4_antenna) call meta_add_keywords(keywords, META_airt_keywords, 4)
              end if
              if (stn .ne. 'CAL') call meta_add_region_keywords(keywords, gdeploy%deploy_site)
              key_comp = meta_compile_keyword_list(keywords)
              call meta_add_attribute(atts, 'keywords', TRIM(key_comp), META_char_type)

              if (stn .eq. 'CAL') then
                meta_id = 'ValidationRun_'//TRIM(gdeploy%ghull%serial_no)//'_'//sdatestr(1:8)//'-'//edatestr(1:8)
              else
                call meta_add_attribute(atts, 'cdip_station_id', stn, META_char_type)
                stn5 = stn//strm
                call meta_add_attribute(atts, 'platform_id', stn5, META_char_type)
                write(dep_id,'(2a,i02.2)') stn5, '_d', gdeploy%deploy_num
                call meta_add_attribute(atts, 'cdip_deployment_id', dep_id, META_char_type)
                wmo_id = gdeploy%gstation%wmo_id
                if (LEN_TRIM(wmo_id) .eq. 5) then
                  call meta_add_attribute(atts, 'wmo_id', TRIM(wmo_id), META_char_type)
                  call meta_add_attribute(atts, 'wmo_platform_code', TRIM(wmo_id), META_char_type)
                end if
                meta_id = 'CDIP_'//stn//strm//'_'//sdatestr(1:8)//'-'//edatestr(1:8)
              end if
              if (xy_only) meta_id = TRIM(meta_id)//'_xy'
              if (zcrossing) meta_id = TRIM(meta_id)//'_zc'
              call meta_add_attribute(atts, 'id', TRIM(meta_id), META_char_type)

              hist = write_iso_8601_date(current_utc())//': dataset created; '//TRIM(hist)//
     *          ' If date_modified is after date_created, contact CDIP for details of changes.'
              call meta_add_attribute(atts, 'history', TRIM(hist), META_char_type)

              comm = META_default_comment
              call meta_add_attribute(atts, 'comment', TRIM(comm), META_char_type)

              if (stn .ne. 'CAL') then
                gdeploy%gstation = gudb_load_station(stn, errcode, 6)
                funder = gudb_get_station_funder_acronyms(gdeploy%gstation, '/')
                operator = gudb_get_station_operator_acronyms(gdeploy%gstation, '/')
                ack = TRIM(META_default_ack)
                if (funder .ne. 'USACE') then
                  partner = meta_gudb_get_funder_partners(funder,'/')
                  ack = TRIM(ack)//' Station partner: '//TRIM(partner)//' ;'
                end if
                ack = TRIM(ack)//' Field operator: '//TRIM(operator)
                call meta_add_attribute(atts, 'acknowledgment', TRIM(ack), META_char_type)

                mlink = 'http://cdip.ucsd.edu/metadata/'//TRIM(stn)//TRIM(strm)
                call meta_add_attribute(atts, 'metadata_link', TRIM(mlink), META_char_type)
                call meta_add_attribute(atts, 'infoUrl', TRIM(mlink), META_char_type)

                contrib_names = ''
                contrib_roles = ''
                if (LEN_TRIM(operator) .gt. 0 .or. LEN_TRIM(funder) .gt. 0) then
                  contrib_names = TRIM(operator)//', '//TRIM(funder)
                  contrib_roles = 'station operation, station funding'
                end if
                if (INDEX(contrib_names,'CDIP') .le. 0) then
                  if (LEN_TRIM(contrib_names) .gt. 0) contrib_names = TRIM(contrib_names)//', '
                  if (LEN_TRIM(contrib_roles) .gt. 0) contrib_roles = TRIM(contrib_roles)//', '
                  contrib_names = TRIM(contrib_names)//'CDIP'
                  contrib_roles = TRIM(contrib_roles)//'data management'
                end if
                call meta_add_attribute(atts, 'contributor_name', TRIM(contrib_names), META_char_type)
                call meta_add_attribute(atts, 'contributor_role', TRIM(contrib_roles), META_char_type)
              end if

              call meta_add_geospatial_atts(atts, minlat, maxlat, minlon, maxlon)

              call meta_add_attribute(atts, 'time_coverage_start', write_iso_8601_date(sdate), META_char_type)
              call meta_add_attribute(atts, 'time_coverage_end', write_iso_8601_date(edate), META_char_type)
              secs = secs_diff(sdate, edate)
              call meta_add_attribute(atts, 'time_coverage_duration', write_iso_8601_duration(secs, 4), META_char_type)
              if (xy_only .or. zcrossing) then
                time_res = ''
                write(time_res(1:7),'(a,f4.2,a)') 'PT', 1.0/srate, 'S'
              else
                secs = 1800
                time_res = write_iso_8601_duration(secs, 2)
              end if
              call meta_add_attribute(atts, 'time_coverage_resolution', TRIM(time_res), META_char_type)

c--   Global attributes from the CF metdata conventions

              call meta_add_attribute(atts, 'source', 'insitu observations', META_char_type)

c--   Global attributes from the NCEI templates + CDIP extras

              call meta_add_attribute(atts, 'instrument', 'metaInstrumentation', META_char_type)

              if (stn .eq. 'CAL') then
                sname = 'CDIP buoy validation facility'
              else
                sname = TRIM(gdeploy%gstation%stn_name)//' BUOY - '//stn//strm
              end if
              call meta_add_attribute(atts, 'platform', 'wave_buoy', META_char_type)
              call meta_add_attribute(atts, 'platform_vocabulary', 'http://mmisw.org/ont/ioos/platform', META_char_type)
              call meta_add_attribute(atts, 'platform_name', TRIM(sname), META_char_type)

              call meta_add_variable(meta_vars, 'metaStationName', TRIM(sname), META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'station name', META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'standard_name', 'platform_name', 
     *          META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'cf_role', 'timeseries_id', META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'coverage_content_type', NC_cctype_rinfo,
     *          META_char_type)

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
     *            TRIM(instrument_label), META_char_type)
                serial_no = 'Hull: '//TRIM(gdeploy%ghull%serial_no)// '; hatchcover: '//TRIM(gdeploy%gtophat%serial_no)
                doc_url = 'http://datawell.nl'
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'serial_number', serial_no, META_char_type)
                write(hull_size_str,'(f4.2)') gdeploy%ghull%diameter
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'hull_diameter_meters', 
     *            hull_size_str, META_real_type)
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'documentation_url', 
     *            TRIM(doc_url), META_char_type)
                mooring = 'Datawell-compliant'
                if (gdeploy%gmooring%marker_float) mooring = TRIM(mooring)//
     *            ', with additional marker float for increased visibility'
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'mooring', TRIM(mooring), META_char_type)
                if (gdeploy%gmooring%total_length .gt. 0.0) then
                   write(mlength,'(f10.2)') gdeploy%gmooring%total_length
                   call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'mooring_length_meters', mlength, 
     *               META_real_type)
                end if
                call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'coverage_content_type', NC_cctype_rinfo,
     *            META_char_type)
              end do
           
              write(deplat,'(f11.6)') gdeploy%deploy_site%lat
              write(deplon,'(f11.6)') gdeploy%deploy_site%long
              call meta_add_variable(meta_vars, 'metaDeployLatitude', deplat, META_real_type)
              meta_vars%vatts(meta_vars%vcount) = meta_latitude_attlist(.true.)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'deployment latitude', 
     *          META_char_type)
              call meta_add_variable(meta_vars, 'metaDeployLongitude', deplon, META_real_type)
              meta_vars%vatts(meta_vars%vcount) = meta_longitude_attlist(.true.)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'deployment longitude', 
     *          META_char_type)

              if (stn .ne. 'CAL') then
                write(depth,'(f11.2)') gdeploy%depth
                call meta_add_variable(meta_vars, 'metaWaterDepth', depth, META_real_type)
                meta_vars%vatts(meta_vars%vcount) = meta_depth_attlist(.false.)
              end if

              write(declin,'(f7.2)') gdeploy%declination
              call meta_add_variable(meta_vars, 'metaDeclination', declin, META_real_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'long_name', 'magnetic declination', 
     *          META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'units', 'degree', META_char_type)
              call meta_add_attribute(meta_vars%vatts(meta_vars%vcount), 'coverage_content_type', NC_cctype_rinfo,
     *          META_char_type)

c             make_model, serial_number, calibration_date, factory_calibrated, 
c             user_calibrated, calibration_report, accuracy, valid_range, and precision.
c
c             'sea_name'?
          end subroutine


c-- META_ASSIGN_COMBO_GUDB_ATTRIBUTES ------------------------------------------
c  Assigns attributes based on info from CDIP's sensor archive
c-------------------------------------------------------------------------------
          subroutine meta_assign_combo_gudb_attributes(gstn, sdate, edate, recs, hist, 
     *                 atts, meta_vars, lats, lons, depths, declins, dep_count, minlat, maxlat,
     *                 minlon, maxlon, is_datawell, mdims, stn_lat, stn_lon)
            integer              dep_count, errcode, i, label_length, recs
            integer              secs, var_count
            real                 lat_offset, lon_offset, minlat, maxlat, minlon, maxlon, stn_lat, stn_lon
            real                 declins(META_array_max), depths(META_array_max)
            real                 lats(META_array_max), lons(META_array_max)
            logical              found, is_datawell
            character*(*)        hist
            character*2          strm
            character*3          stn
            character*10         min_depth, max_depth, srecs
            character*11         depth, declin, stn_latstr, stn_lonstr
            character*14         edatestr, sdatestr
            character*19         edate_sql, sdate_sql
            character*20         maxlat_str, maxlon_str, minlat_str, minlon_str
            character*100        funder, operator, partner
            character*100        declinstrs(META_array_max), depthstrs(META_array_max)
            character*100        latstrs(META_array_max), lonstrs(META_array_max), meta_id
            character*100        depth_str, gauge_str, inst_label, keywords(META_max_keys), mlink, sname, vname, vatt
            character*500        ack, comm, title, key_comp, contrib_names, contrib_roles, stnlat_comment
            type(location)            loc
            type(date_block)          edate, sdate
            type(gudb_station)        gstn
            type(meta_attribute_list) atts
            type(meta_dimension_list) mdims
            type(meta_variable_list)  meta_vars

c--   Assemble needed archive information

            stn = gstn%cdip_id
            strm = 'p1'
            edatestr = make_datestring(edate)
            sdatestr = make_datestring(sdate)
            edate_sql = write_sql_date(edate)
            sdate_sql = write_sql_date(sdate)

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
     *        TRIM(gstn%stn_name)//' from '//sdate_sql(1:10)//' to '//edate_sql(1:10)//'.'
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

            meta_id = 'CDIP_'//stn//strm//'_'//sdatestr(1:8)//'-'//edatestr(1:8)//'_historic'
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

            ack = META_default_ack
            funder = gudb_get_station_funder_acronyms(gstn, '/')
            operator = gudb_get_station_operator_acronyms(gstn, '/')
            if (funder .ne. 'USACE') then
              partner = meta_gudb_get_funder_partners(funder,'/')
              ack = TRIM(ack)//' Station partner: '//TRIM(partner)//' ;'
            end if
            ack = TRIM(ack)//' Field operator: '//TRIM(operator)
            call meta_add_attribute(atts, 'acknowledgment', TRIM(ack), META_char_type)

            mlink = 'http://cdip.ucsd.edu/metadata/'//TRIM(stn)//TRIM(strm)
            call meta_add_attribute(atts, 'metadata_link', TRIM(mlink), META_char_type)
            call meta_add_attribute(atts, 'infoUrl', TRIM(mlink), META_char_type)

            contrib_names = TRIM(operator)//', '//TRIM(funder)
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
            call meta_add_attribute(atts, 'time_coverage_duration', write_iso_8601_duration(secs, 4), META_char_type)
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

            sname = TRIM(gstn%stn_name)//' BUOY - '//stn//strm
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


          character*100 function meta_gudb_get_funder_partners(list, delim)
            integer         i
            character*1     del, delim
            character*100   list
            character*500   list_500

            list_500 = list
            del = ''
            meta_gudb_get_funder_partners = ''
            do i = 1, number_of_fields(list_500, delim)
              if (get_field(list_500, delim, i) .ne. 'USACE') then
                meta_gudb_get_funder_partners = TRIM(meta_gudb_get_funder_partners)//TRIM(del)//
     *            get_field(list_500, delim, i)
                del = delim
              end if
            end do
          end function

      end module
