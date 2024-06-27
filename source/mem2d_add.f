c-------------------------------------------------------------------------------
c- MEM2D_ADD - creates a new waveCDF dataset with wave data plus MEM 2D
c-  spectra for the requested station and dates. The 2D spectra is in the CDIP-
c-  standard 5-degree layout, and stored in the var 'waveDirectionalSpectrum'.
c-  Please see the cdipsw/mem2d_add project on github for more details.
c-
c-  Arguments: 1 - 3-digit station number (required)
c-             2 - Start time, YYYYMMDD or portion thereof (required)
c-             3 - end time, YYYYMMDD (optional); max request length = 1 year
c-------------------------------------------------------------------------------
      program mem2d_add

      use dates
      use gudb_metadata_utils
      use wavecdf5_utils
      use wc5_mem_utils

      implicit none

      integer::      args, ecode, load_type
      character*14   date_str1, date_str2
      character*100  id, outname
      character*500  comment, history, keywords, summary, title
      logical        forecast_mode, qc_on

      type(date_block)            start_date, end_date
      type(gudb_deployment_set)   gdset_full, gdset_trim
      type(meta_attribute_list)   atts
      type(meta_variable_list)    mvars
      type(time_span)             tspan
      type(wc5_dataset)           wset

        write(6,'(a)') 'MEM2D_ADD(): adding 2D MEM spectra to CDIP waveCDF datasets'
        args = IARGC()


c-  Read args, set station id and date range

        if (args .ge. 1) then
          call getarg(1, id)
          if (id(1:1) .eq. 'h' .or. id(1:1) .eq. 'H') then
            ecode = system('head -10 ./mem2d_add.f')
            call exit(0)
          end if
        end if

        if (args .lt. 2) then
          write(6,'(a)') ' - two arguments required, STN and DATE (YYYYMMDD, or portion thereof); exiting'
          call exit(0)
        end if

        call getarg(2, date_str1)
        start_date = parse_datestring(complete_datestring(date_str1,'start'))
        end_date = parse_datestring(complete_datestring(date_str1,'end'))

        if (args .ge. 3) then
          call getarg(3, date_str2)
          end_date = parse_datestring(complete_datestring(date_str2,'end'))
        else
          date_str2 = ''
        end if
        if (secs_diff(start_date, end_date) .gt. 366*24*3600) then
          end_date = start_date
          end_date%year = end_date%year + 1
          end_date = subtract_seconds(end_date, 1)
        end if


c-  Load wave data, add 2D spectra

        load_type = WC5_include_wave_params + WC5_include_wave_spectra
        tspan = init_time_span(start_date, end_date)
        qc_on = .true.
        forecast_mode = .false.
        write(6,'(a)') ' - loading data via thredds.cdip'
        call wc5u_load_timespan_data(id, tspan, qc_on, forecast_mode, load_type, wset, ecode, opendap=.true.)
        if (ecode .ne. 0) then
          write(6,'(a)') ' - error loading data, exiting'
          call exit(0)
        else
          write(6,'(a,i10)') ' - load complete, record count = ', wset%wave%time_count
        end if

        call wc5m_add_2d_cdip(wset)
        write(6,'(a)') ' - added 2D MEM spectra'


c-  Initialize output dataset, load metadata

        outname = TRIM(id)//'p1_'//TRIM(date_str1)//'_mem2d.nc'
        wset%groups = load_type
        call wc5_create_ncfile(wset, outname, ecode, load_type, .false.)

        gdset_full = gudb_load_deployment_history(id(1:3), ecode, 6)
        gdset_trim = gudb_trim_deployment_set(gdset_full, tspan, 1)
        write(6,'(a)') ' - loaded metadata'

        call meta_initialize_variables(mvars)
        call meta_initialize_attributes(atts)
        call meta_assign_static_attributes(atts)
        history = 'program, arguments: mem2d_add v1.0, '//TRIM(id)//' '//TRIM(date_str1)//' '//TRIM(date_str2)//'. '

        call meta_assign_gudb_attributes(gdset_trim%deploy(1), timestamp_to_date(wset%wave%times(1)), 
     *    timestamp_to_date(wset%wave%times(wset%wave%time_count)), wset%wave%time_count, history, atts, mvars)
        comment = 'The 2D spectra included in this dataset are MEM estimates derived from the '//
     *    'higher-order fourier coeffecients. Alternate 2D spectra can be generated with different estimators.'
        call meta_add_attribute(atts, 'comment', comment, META_char_type)
        call meta_add_attribute(atts, 'id', outname(1:LEN_TRIM(outname)-3), META_char_type)
        title = 'CDIP station '//id(1:3)//' wave data with added 2D spectral estimates.'
        call meta_add_attribute(atts, 'title', title, META_char_type)
        summary = TRIM(title)//' 2D spectra estimated with the Maximum Entropy Method (MEM). '//
     *    'See CDIP MEM2D on github for more details.'
        call meta_add_attribute(atts, 'summary', summary, META_char_type)
        keywords = 'EARTH SCIENCE, OCEANS, OCEAN WAVES, GRAVITY WAVES, WIND WAVES, SIGNIFICANT WAVE HEIGHT, '//
     *    'WAVE FREQUENCY, WAVE PERIOD, WAVE SPECTRA'
        call meta_add_attribute(atts, 'keywords', keywords, META_char_type)


c-  Fill output dataset

        write(6,'(a)') ' - writing metadata'
        call wc5_add_global_attributes(wset%ncid, atts)
        call wc5_def_metadata_variables(wset%ncid, mvars)
        call nc_call_func(nf90_enddef(wset%ncid))
        call wc5_put_metadata_variables(wset%ncid, mvars)

        write(6,'(a)') ' - filling nc file'
        call wc5_fill_ncfile(wset, ecode, load_type)
        write(6,'(2a)') ' - created file: ', TRIM(outname)

       end
