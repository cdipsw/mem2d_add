c-- XML_WAVE_DATA --------------------------------------------------------------
c
c   Defines and manipulates the CDIPWaveDataset xml standard. A xml dataset
c   contains parameter data, spectral data, and metadata for up to one month
c   for a single point or station.
c 
c   Used by: .data05/net_model
c-------------------------------------------------------------------------------

      module xml_wave_data

      use error_utils
      use locations
      use dates
      use file_ops
      use mem_ds
      use spectral
      use xml_elements

      save 

      integer,parameter::   XML_max_sites = 1
      integer,parameter::   XML_max_times = 1500
      integer,parameter::   XML_max_bands = 256

      type xml_wave_dataset
        character*22     proc_code
        type(date_block) proc_time
        character*500    comment

        type(location)   site_loc
        character*5      site_id
        integer          shore_normal
        real             depth

        integer          time_count
        logical          is_sensor, is_directional, is_nearshore, use_time_span
        logical          has_surfzone
        type(date_block) time(XML_max_times), start_time, end_time
        integer          time_gap

        integer          passes_qc(XML_max_times), public_release(XML_max_times)
        integer          record_null(XML_max_times)
        character*80     model_input_buoys(XML_max_times)

        real             Hs(XML_max_times), Tp(XML_max_times), Ta(XML_max_times) 
        real             Sxy(XML_max_times), Sxx(XML_max_times)
        real             Vm(XML_max_times), Hb(XML_max_times)
        integer          Dp(XML_max_times), Dm(XML_max_times)

        integer          band_count
        real             sample_rate, sample_length
        real             frequency(XML_max_bands) 
        real             band_width(XML_max_bands)
        real             ener_dens(XML_max_bands,XML_max_times) 
        real             mean_dir(XML_max_bands,XML_max_times)
        real             a1(XML_max_bands,XML_max_times)
        real             a2(XML_max_bands,XML_max_times)
        real             b1(XML_max_bands,XML_max_times)
        real             b2(XML_max_bands,XML_max_times)
        real             check_factor(XML_max_bands,XML_max_times)
        real             bin_coverage(XML_max_bands,XML_max_times)
      end type


c--  XML_tag_labels - labels that correspond to the xml tags

       character*50::
     *   XML_set_tag =               'CDIPWaveDataset',
     *   XML_params_tag =            'ParameterValues',
     *   XML_spectra_tag =           'SpectralValues',
     *   XML_proc_code_tag =         'ProcessingCode',
     *   XML_proc_time_tag =         'ProcessingTime',
     *   XML_comment_tag =           'Comment',
     *   XML_site_loc_tag =          'SiteLocation',
     *   XML_site_id_tag =           'SiteID',
     *   XML_shore_normal_tag =      'ShoreNormal',
     *   XML_depth_tag =             'Depth',
     *   XML_time_count_tag =        'TimeCount',
     *   XML_time_tag =              'Times',
     *   XML_start_time_tag =        'StartTime',
     *   XML_end_time_tag =          'EndTime',
     *   XML_time_gap_tag =          'TimeGap',
     *   XML_record_null_tag =       'RecordNull',
     *   XML_passes_qc_tag =         'PassesQC',
     *   XML_public_release_tag =    'PublicRelease',
     *   XML_model_input_buoys_tag = 'ModelInputBuoys',
     *   XML_Hs_tag =                'HsValues',
     *   XML_Tp_tag =                'TpValues',
     *   XML_Ta_tag =                'TaValues',
     *   XML_Dp_tag =                'DpValues',
     *   XML_Dm_tag =                'DmValues',
     *   XML_Sxy_tag =               'SxyValues',
     *   XML_Sxx_tag =               'SxxValues',
     *   XML_Vm_tag =                'VmValues',
     *   XML_Hb_tag =                'HbValues',
     *   XML_band_count_tag =        'BandCount',
     *   XML_sample_rate_tag =       'SampleRate',
     *   XML_sample_length_tag =     'SampleLength',
     *   XML_frequency_tag =         'Frequencies',
     *   XML_band_width_tag =        'Bandwidths',
     *   XML_ener_dens_tag =         'EnergyDensities',
     *   XML_mean_dir_tag =          'MeanDirections',
     *   XML_a1_tag =                'a1Values',
     *   XML_a2_tag =                'a2Values',
     *   XML_b1_tag =                'b1Values',
     *   XML_b2_tag =                'b2Values',
     *   XML_check_factor_tag =      'CheckFactors',
     *   XML_bin_coverage_tag =      'BinInputCoverage'

       character*500::
     *   XML_sensor_comment = 'SENSOR CHANGE: Sensor type changes within '//
     *     'file, check archive for details.',
     *   XML_layout_comment = 'LAYOUT CHANGE: Spectral layout changes within '//
     *     'file, check archive for details.'

       character*22 XML_processing_codes(4)

       data XML_processing_codes /'1: Net_model output   ',
     *        '2: sensor observations',
     *        '3: WW3 model output   ',
     *        '4: OWI hindcast output'/

      contains


c-- INIT_XML_WAVES -------------------------------------------------------------
c Initializes an empty xml_wave_dataset
c-------------------------------------------------------------------------------
        subroutine init_xml_waves(xml_set, proc_code)
          type(xml_wave_dataset) xml_set
          integer proc_code

          xml_set%proc_code = XML_processing_codes(proc_code)
          if (proc_code .eq. 2) then
            xml_set%is_sensor = .true.
          else
            xml_set%is_sensor = .false.
          end if

          xml_set%time_count = 0
          xml_set%band_count = 0
          xml_set%shore_normal = -1
          xml_set%comment = ''
          xml_set%use_time_span = .false.
          xml_set%is_nearshore = .false.
          xml_set%has_surfzone = .false.
          xml_set%is_directional = .true.

        end subroutine

          
c-- ADD_WAVE_RECORD ------------------------------------------------------------
c Adds a wave record - in the form of spectral data - to an xml_wave_dataset
c Err codes:  -9 : spectral layouts do not match
c-------------------------------------------------------------------------------
        subroutine add_wave_record(xml_set, sp_data, sp_hdr, pub_flag, err_code)
          integer   err_code, pub_val
          logical   append_record, add_record, pub_flag
          logical   tmask(XML_max_times), tmask2d(XML_max_bands, XML_max_times)
          type(date_block)       sp_time
          type(sp_data_block)    redist_sp, sp_data
          type(sp_hdr_block)     sp_hdr
          type(xml_wave_dataset) tmp_set, xml_set

          if (xml_set%time_count .eq. 0) then
            xml_set%band_count = sp_data%bands
            xml_set%sample_rate = sp_hdr%sample_rate
            xml_set%sample_length = REAL(sp_hdr%sample_length)
            do i = 1, sp_data%bands
              xml_set%frequency(i) = sp_data%freq(i)
              xml_set%band_width(i) = sp_data%band_width(i)
            end do
            xml_set%site_id = sp_hdr%filename(3:7)
            xml_set%proc_time = current_utc()
            if (xml_set%proc_code .eq. XML_processing_codes(1))
     *        xml_set%site_loc = sp_hdr%position
            if (MAXVAL(sp_data%dir(1:sp_data%bands)) .le. 0) 
     *        xml_set%is_directional = .false.
  
          else if (xml_set%band_count .ne. sp_data%bands) then
            if (sp_hdr%sample_rate .ne. xml_set%sample_rate) then
              call add_xml_comment(xml_set, XML_sensor_comment)
            else
              call add_xml_comment(xml_set, XML_layout_comment)
            end if
            redist_sp%bands = xml_set%band_count
            do i = 1, xml_set%band_count
              redist_sp%freq(i) = xml_set%frequency(i)
              redist_sp%band_width(i) = xml_set%band_width(i)
            end do
            call redistribute_sp(sp_data, redist_sp)
            sp_data = redist_sp
          end if


c--   Find correct index for the new spectrum

          sp_time = get_filetime(sp_hdr%filename)
          sp_time%sec = 0
          if (xml_set%time_count .eq. 0 .or. 
     *         is_after(sp_time, xml_set%time(xml_set%time_count))) then
            add_record = .true.
            append_record = .true.
            idx = xml_set%time_count + 1
          else
            append_record = .false.
            i = 1
            idx = -1
            do while (idx .eq. -1)
              if (ABS(secs_diff(sp_time, xml_set%time(i))) <= 60) then
                idx = i
                add_record = .false.
              else if (is_before(sp_time, xml_set%time(i))) then
                add_record = .true.
                idx = i
              end if
              i = i + 1
            end do
          end if

          if (add_record) then
            xml_set%time_count = xml_set%time_count + 1
            if (.not. append_record) then
              call init_xml_waves(tmp_set, 2)

              tmp_set%time = CSHIFT(xml_set%time, -1)
              if (.not. xml_set%is_sensor) then
c             if (xml_set%proc_code .eq. XML_processing_codes(1)) then
                tmp_set%passes_qc = CSHIFT(xml_set%passes_qc, -1)
                tmp_set%model_input_buoys = CSHIFT(xml_set%model_input_buoys, -1)
              else
                tmp_set%public_release = CSHIFT(xml_set%public_release, -1)
              end if
              tmp_set%Hs = CSHIFT(xml_set%Hs, -1)
              tmp_set%Tp = CSHIFT(xml_set%Tp, -1)
              tmp_set%Ta = CSHIFT(xml_set%Ta, -1)
              tmp_set%ener_dens = CSHIFT(xml_set%ener_dens, -1, 2)
              if (xml_set%is_directional) then
                tmp_set%Dp = CSHIFT(xml_set%Dp, -1)
                tmp_set%mean_dir = CSHIFT(xml_set%mean_dir, -1, 2)
                tmp_set%a1 = CSHIFT(xml_set%a1, -1, 2)
                tmp_set%b1 = CSHIFT(xml_set%b1, -1, 2)
                tmp_set%a2 = CSHIFT(xml_set%a2, -1, 2)
                tmp_set%b2 = CSHIFT(xml_set%b2, -1, 2)
                if (xml_set%is_sensor) then
                  tmp_set%check_factor = CSHIFT(xml_set%check_factor, -1, 2)
                else
                  tmp_set%bin_coverage = CSHIFT(xml_set%bin_coverage, -1, 2)
                end if
              end if
              if (xml_set%is_nearshore) then
                tmp_set%Dm = CSHIFT(xml_set%Dm, -1)
                tmp_set%Sxy = CSHIFT(xml_set%Sxy, -1)
                tmp_set%Sxx = CSHIFT(xml_set%Sxx, -1)
              end if
              if (xml_set%has_surfzone) then
                tmp_set%Vm = CSHIFT(xml_set%Vm, -1)
                tmp_set%Hb = CSHIFT(xml_set%Hb, -1)
              end if

              do j = 1, xml_set%time_count
                if (j .le. idx) then
                  tmask(j) = .true.
                  do k = 1, xml_set%band_count
                    tmask2d(k,j) = .true.
                  end do
                else 
                  tmask(j) = .false.
                  do k = 1, xml_set%band_count
                    tmask2d(k,j) = .false.
                  end do
                end if 
              end do
            end if 
          end if

          xml_set%time(idx) = sp_time

          pub_val = 0
          if (pub_flag) pub_val = 1
            if (.not. xml_set%is_sensor) then
            xml_set%passes_qc(idx) = pub_val
            if (xml_set%proc_code .eq. XML_processing_codes(1))
     *        xml_set%model_input_buoys(idx) = ':'
          else
            xml_set%public_release(idx) = pub_val
          end if

          xml_set%Hs(idx) = sp_hdr%Hs
          xml_set%Tp(idx) = sp_hdr%Tp
          xml_set%Ta(idx) = sp_hdr%Ta
          if (xml_set%is_directional) xml_set%Dp(idx) = sp_hdr%Dp
          if (xml_set%is_nearshore) then
            xml_set%Dm(idx) = -99
            xml_set%Sxy(idx) = -9.9999
            xml_set%Sxx(idx) = -9.9999
          end if
          if (xml_set%has_surfzone) then
            xml_set%Vm(idx) = -99.99
            xml_set%Hb(idx) = -9.99
          end if

          do i = 1, sp_data%bands
            xml_set%ener_dens(i, idx) = sp_data%ener_dens(i)
            if (xml_set%is_directional) then
              xml_set%mean_dir(i, idx) = sp_data%dir(i)
              if (sp_data%dir(i) .ne. -1) then
                xml_set%a1(i,idx) = sp_data%a1(i)
                xml_set%b1(i,idx) = sp_data%b1(i)
                xml_set%a2(i,idx) = sp_data%a2(i)
                xml_set%b2(i,idx) = sp_data%b2(i)
              end if
              if (sp_data%check(i) .eq. -1) sp_data%check(i) = -.99
              if (xml_set%is_sensor) then
                xml_set%check_factor(i,idx) = sp_data%check(i)
              else
                xml_set%bin_coverage(i,idx) = sp_data%check(i)
              end if
            end if
          end do

          if (add_record .and. .not. append_record) then
            xml_set%time = MERGE(xml_set%time, tmp_set%time, tmask)
            if (xml_set%is_sensor) then
              xml_set%public_release = MERGE(xml_set%public_release, tmp_set%public_release, tmask)
            else
              xml_set%passes_qc = MERGE(xml_set%passes_qc, tmp_set%passes_qc, tmask)
              xml_set%model_input_buoys = MERGE(xml_set%model_input_buoys, tmp_set%model_input_buoys, tmask)
            end if
            xml_set%Hs = MERGE(xml_set%Hs, tmp_set%Hs, tmask)
            xml_set%Tp = MERGE(xml_set%Tp, tmp_set%Tp, tmask)
            xml_set%Ta = MERGE(xml_set%Ta, tmp_set%Ta, tmask)
            xml_set%ener_dens = MERGE(xml_set%ener_dens, tmp_set%ener_dens, tmask2d)
            if (xml_set%is_directional) then
              xml_set%Dp = MERGE(xml_set%Dp, tmp_set%Dp, tmask)
              xml_set%mean_dir = MERGE(xml_set%mean_dir, tmp_set%mean_dir, tmask2d)
              xml_set%a1 = MERGE(xml_set%a1, tmp_set%a1, tmask2d)
              xml_set%b1 = MERGE(xml_set%b1, tmp_set%b1, tmask2d)
              xml_set%a2 = MERGE(xml_set%a2, tmp_set%a2, tmask2d)
              xml_set%b2 = MERGE(xml_set%b2, tmp_set%b2, tmask2d)
              if (xml_set%is_sensor) then
                xml_set%check_factor = MERGE(xml_set%check_factor, tmp_set%check_factor, tmask2d)
              else
                xml_set%bin_coverage = MERGE(xml_set%bin_coverage, tmp_set%bin_coverage, tmask2d)
              end if
            end if
            if (xml_set%is_nearshore) then
              xml_set%Dm = MERGE(xml_set%Dm, tmp_set%Dm, tmask)
              xml_set%Sxy = MERGE(xml_set%Sxy, tmp_set%Sxy, tmask)
              xml_set%Sxx = MERGE(xml_set%Sxx, tmp_set%Sxx, tmask)
            end if
            if (xml_set%has_surfzone) then
              xml_set%Vm = MERGE(xml_set%Vm, tmp_set%Vm, tmask)
              xml_set%Hb = MERGE(xml_set%Hb, tmp_set%Hb, tmask)
            end if
          end if

          if (add_record) call check_time_span(xml_set)
        end subroutine
          

c-- REMOVE_WAVE_RECORD ---------------------------------------------------------
c Removes the wave record at the given time
c-------------------------------------------------------------------------------
        subroutine remove_wave_record(xml_set, sp_time, err_code)
          integer   err_code
          type(date_block)       sp_time
          type(xml_wave_dataset) tmp_set1, tmp_set2, xml_set

          err_code = 0
          check = get_xml_index(xml_set, sp_time, 0)
          if (check .gt. 0) then
            call split_set(xml_set, tmp_set1, sp_time, err_code)
            if (err_code .ne. 0 .or. tmp_set1%time_count .lt. 2) return
            call split_set(tmp_set1, tmp_set2, tmp_set1%time(2), err_code)
            if (err_code .ne. 0) return
            call merge_sets(xml_set, tmp_set2, err_code)
          end if
          return
        end subroutine


c-- ADD_NULL_RECORD ------------------------------------------------------------
c Adds a full spectral null wave record to an xml_wave_dataset. 
c-------------------------------------------------------------------------------
        subroutine add_null_record(xml_set, xtime, sp_data, err_code)
          integer                err_code
          logical                pub_flag
          character*14           x_timestr
          type(date_block)       xtime
          type(sp_data_block)    sp_data
          type(sp_hdr_block)     sp_hdr
          type(xml_wave_dataset) xml_set

          sp_hdr%Hs = -1
          sp_hdr%Tp = -1
          sp_hdr%Dp = -1
          sp_hdr%Ta = -1
          x_timestr = make_datestring(xtime)
          sp_hdr%filename(1:19) = 'sp'//xml_set%site_id//x_timestr(1:12)
          call add_wave_record(xml_set, sp_data, sp_hdr,.true.,err_code)
          idx = get_xml_index(xml_set, xtime, 1)
          if (xml_set%is_sensor) then
            xml_set%public_release(idx) = 0
          else
            xml_set%passes_qc(idx) = 0
            xml_set%model_input_buoys(idx) = ':'
          end if

        end subroutine


c-- LOAD_XML_WAVES -------------------------------------------------------------
c Loads data from XML_DATA into a xml_wave_dataset
c-------------------------------------------------------------------------------
        subroutine load_xml_waves(mop, yearmo, xml_set, err_code)
          integer::              err_code
          character*5            mop
          character*6            yearmo
          character*10           xfile_type
          character*100          xfile
          type(xml_wave_dataset) xml_set

          if (yearmo(1:1) .eq. 'f') then
            xfile_type = 'forecast'
          else
            xfile_type = yearmo
          end if

          xfile = '/project/data05/XML_DATA/'//mop//'/'//mop//'_'//
     *      TRIM(xfile_type)//'.xml'
          call read_xml_waves(xfile, xml_set, err_code, 6)
        end subroutine


c-- LOAD_XML_HINDCAST ----------------------------------------------------------
c Loads data from hindcast XML_DATA into a xml_wave_dataset
c-------------------------------------------------------------------------------
        subroutine load_xml_hindcast(mop, yearmo, xml_set, err_code, suffix)
          integer::              err_code
          character*2,optional:: suffix
          character*5            mop
          character*6            yearmo
          character*100          xfile
          type(xml_wave_dataset) xml_set

          if (.not. PRESENT(suffix)) suffix = 'hc'
          xfile = '/project/data05/XML_DATA/'//mop//'/'//mop//'_'//
     *      yearmo//'_'//suffix//'.xml'
          call read_xml_waves(xfile, xml_set, err_code, 6)
        end subroutine


c-- READ_XML_WAVES -------------------------------------------------------------
c Loads data from a xml file into a xml_wave_dataset in the specfied file.
c-------------------------------------------------------------------------------
        subroutine read_xml_waves(xfile, xml_set, err_code, err_unit)
          integer::              dp_temp(1500), err_code, err_unit
          integer::              eindex, sindex, runit=199
          character*3            bands
          character*5            times
          character*19           temp_time, time_strs(1500)
          character*30           rd_fmt, skip_tag
          character*50           xtag, tname
          character*100          blank, xfile
          character*500          comment_field

          type(xml_wave_dataset) xml_set
  
          blank = ''
          call open_read(runit, blank, xfile, err_code, 6)
          if (err_code .ne. 0) return

          read(runit, '(a50)', iostat=err_code) xtag
          do while (err_code .eq. 0)
            BACKSPACE(runit)
            sindex = INDEX(xtag, '<')
            eindex = INDEX(xtag, '>')
            tname = xtag(sindex+1:eindex-1) 
            if (tname .eq. XML_proc_code_tag) then
              read(runit,'(16x,a22)',iostat=err_code) xml_set%proc_code
              if (xml_set%proc_code .eq. XML_processing_codes(2)) then
                call init_xml_waves(xml_set, 2)
              else if (xml_set%proc_code .eq. XML_processing_codes(3)) then
                call init_xml_waves(xml_set, 3)
              else
                call init_xml_waves(xml_set, 1)
              end if
              xml_set%is_directional = .false.
              xml_set%is_nearshore = .false.
              xml_set%use_time_span = .false.
            else if (tname .eq. XML_proc_time_tag) then 
              read(runit,'(16x,a19)',iostat=err_code) temp_time
              xml_set%proc_time = parse_sql_date(temp_time)
            else if (tname .eq. XML_comment_tag) then 
              read(runit,'(9x,a)') comment_field
              eindex = INDEX(comment_field,'<',.true.)
              xml_set%comment = comment_field(1:eindex-1)
            else if (tname .eq. XML_site_loc_tag) then 
              read(runit,*,iostat=err_code) skip_tag, xml_set%site_loc%lat,
     *          xml_set%site_loc%long
            else if (tname .eq. XML_site_id_tag) then 
              read(runit,'(8x,a5)',iostat=err_code) xml_set%site_id
            else if (tname .eq. XML_shore_normal_tag) then 
              read(runit,*) skip_tag, xml_set%shore_normal
            else if (tname .eq. XML_depth_tag) then 
              read(runit,*) skip_tag, xml_set%depth
            else if (tname .eq. XML_time_count_tag) then 
              read(runit,*,iostat=err_code) skip_tag, xml_set%time_count
              write(times(1:5),'(i5)',iostat=err_code) xml_set%time_count
            else if (tname .eq. XML_time_tag) then 
              rd_fmt = '(7x,'//TRIM(ADJUSTL(times))//'(a19,1x))'
              read(runit,rd_fmt,iostat=err_code) (time_strs(i), i=1,
     *          xml_set%time_count)
              do i = 1, xml_set%time_count
                xml_set%time(i) = parse_sql_date(time_strs(i))
              end do
            else if (tname .eq. XML_start_time_tag) then 
              read(runit,'(11x,a19)',iostat=err_code) temp_time
              xml_set%start_time = parse_sql_date(temp_time)
              xml_set%use_time_span = .true.
            else if (tname .eq. XML_end_time_tag) then 
              read(runit,'(9x,a19)',iostat=err_code) temp_time
              xml_set%end_time = parse_sql_date(temp_time)
            else if (tname .eq. XML_time_gap_tag) then 
              read(runit,'(9x,i6)') xml_set%time_gap
            else if (tname .eq. XML_record_null_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%record_null(i), 
     *          i=1, xml_set%time_count)
            else if (tname .eq. XML_passes_qc_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%passes_qc(i), i=1,
     *          xml_set%time_count)
            else if (tname .eq. XML_public_release_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%public_release(i),
     *          i=1, xml_set%time_count)
            else if (tname .eq. XML_model_input_buoys_tag) then 
              read(runit,*,iostat=err_code) skip_tag, 
     *          (xml_set%model_input_buoys(i), i=1, xml_set%time_count)
            else if (tname .eq. XML_Hs_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Hs(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Tp_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Tp(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Ta_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Ta(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Dp_tag) then 
              read(runit,*,iostat=err_code) skip_tag, 
     *          (dp_temp(i), i=1,xml_set%time_count)
              xml_set%is_directional = .true.
              do i = 1, xml_set%time_count
                xml_set%Dp(i) = REAL(dp_temp(i))
              end do
            else if (tname .eq. XML_Dm_tag) then 
              read(runit,*,iostat=err_code) skip_tag, 
     *          (dp_temp(i), i=1,xml_set%time_count)
              xml_set%is_nearshore = .true.
              do i = 1, xml_set%time_count
                xml_set%Dm(i) = REAL(dp_temp(i))
              end do
            else if (tname .eq. XML_Sxy_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Sxy(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Sxx_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Sxx(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Vm_tag) then 
              xml_set%has_surfzone = .true.
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Vm(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_Hb_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%Hb(i), 
     *          i=1,xml_set%time_count)
            else if (tname .eq. XML_band_count_tag) then 
              read(runit,*,iostat=err_code) skip_tag, xml_set%band_count
              write(bands(1:3),'(i3)',iostat=err_code) xml_set%band_count
            else if (tname .eq. XML_sample_rate_tag) then 
              read(runit,*,iostat=err_code) skip_tag, xml_set%sample_rate
            else if (tname .eq. XML_sample_length_tag) then 
              read(runit,*,iostat=err_code) skip_tag, xml_set%sample_length
            else if (tname .eq. XML_frequency_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%frequency(i), 
     *          i=1,xml_set%band_count)
            else if (tname .eq. XML_band_width_tag) then 
              read(runit,*,iostat=err_code) skip_tag, (xml_set%band_width(i), 
     *          i=1,xml_set%band_count)
            else if (tname .eq. XML_ener_dens_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%ener_dens(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_mean_dir_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%mean_dir(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_a1_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%a1(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_a2_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%a2(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_b1_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%b1(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_b2_tag) then 
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%b2(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_check_factor_tag) then
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%check_factor(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else if (tname .eq. XML_bin_coverage_tag) then
              read(runit,*,iostat=err_code) skip_tag, ((xml_set%bin_coverage(j,k), 
     *          j=1,xml_set%band_count), k=1,xml_set%time_count)
            else
              read(runit, *, iostat=err_code)
            end if
            if (err_code .ne. 0) then
              close(runit)
              return
            end if
            read(runit, '(a50)', iostat=err_code) xtag
          end do

          close(runit)
          if (err_code .eq. -1) err_code = 0

          if (xml_set%use_time_span) then
            do i = 1, xml_set%time_count
              xml_set%time(i) = add_seconds(xml_set%start_time,xml_set%time_gap*(i-1))
            end do
          end if


        end subroutine


c-- WRITE_XML_WAVES ------------------------------------------------------------
c  Writes data from a xml_wave_dataset to a file (or to std out)
c-------------------------------------------------------------------------------
        subroutine write_xml_waves(xunit, xml_set, err_code, err_unit)
          integer                acount, err_code, err_unit, xunit
          character*5            band_count_str, shore_normal_str
          character*7            time_gap_str, time_count_str
          character*9            depth_str, sample_length_str, sample_rate_str
          character*19           end_time_str, proc_time_str, start_time_str
          character*22           site_loc_str
          character*80           anames(10), avalues(10)
          type(xml_wave_dataset) xml_set, blank_set

          acount = 0

          call write_xml_start_tag(xunit, XML_set_tag, acount, anames, avalues,
     *      err_code)
          write(xunit,'()')

          call write_xml_element(xunit, XML_proc_code_tag, xml_set%proc_code, 
     *      acount, anames, avalues, err_code)
          if (LEN_TRIM(xml_set%comment) .gt. 1) call write_xml_element(xunit, 
     *      XML_comment_tag, TRIM(xml_set%comment), acount, anames, avalues, err_code)
          call write_xml_element(xunit, XML_site_id_tag, xml_set%site_id, 
     *      acount, anames, avalues, err_code)

          proc_time_str = write_sql_date(xml_set%proc_time)
          call write_xml_element(xunit, XML_proc_time_tag, proc_time_str, 
     *      acount, anames, avalues, err_code)
          if (xml_set%site_loc%lat .ne. 0) then
            write(site_loc_str(1:22),'(1x,f9.5,1x,f10.5,1x)') xml_set%site_loc%lat, 
     *       xml_set%site_loc%long
           call write_xml_element(xunit, XML_site_loc_tag, site_loc_str, 
     *       acount, anames, avalues, err_code)
          end if
          if (xml_set%shore_normal .ne. -1) then
            write(shore_normal_str(1:5),'(1x,i3,1x)') xml_set%shore_normal
            call write_xml_element(xunit, XML_shore_normal_tag, shore_normal_str,
     *         acount, anames, avalues, err_code)
          end if
          if (xml_set%depth .ne. 0) then
            write(depth_str(1:9),'(1x,f7.2,1x)') xml_set%depth
            call write_xml_element(xunit, XML_depth_tag, depth_str, acount, 
     *         anames, avalues, err_code)
          end if
          if (xml_set%time_count .gt. 0) then
            write(time_count_str(1:7),'(1x,i5,1x)') xml_set%time_count
            call write_xml_element(xunit, XML_time_count_tag, time_count_str, 
     *         acount, anames, avalues, err_code)
          end if
          if (xml_set%use_time_span) then
            start_time_str = write_sql_date(xml_set%start_time)
            call write_xml_element(xunit, XML_start_time_tag, 
     *        start_time_str, acount, anames, avalues, err_code)
            end_time_str = write_sql_date(xml_set%end_time)
            call write_xml_element(xunit, XML_end_time_tag, end_time_str,
     *        acount, anames, avalues, err_code)
            write(time_gap_str(1:7),'(1x,i5,1x)') xml_set%time_gap
            call write_xml_element(xunit, XML_time_gap_tag, time_gap_str, 
     *        acount, anames, avalues, err_code)
          else
            call write_xml_start_tag(xunit, XML_time_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              write(xunit, '(2a,$)') write_sql_date(xml_set%time(i)),';'
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_time_tag, err_code)
          end if

cc        call write_xml_element(xunit, XML_record_null_tag, 
cc   *      record_null_str, acount, anames, avalues, err_code)

          if (xml_set%proc_code .eq. XML_processing_codes(1)) then
            call write_xml_start_tag(xunit, XML_model_input_buoys_tag, acount, 
     *        anames, avalues, err_code)
            do i = 1, xml_set%time_count
              write(xunit, '(1x,a,$)') TRIM(xml_set%model_input_buoys(i))
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_model_input_buoys_tag, err_code)
          end if

          if (xml_set%is_sensor) then
            call write_xml_start_tag(xunit, XML_public_release_tag, acount, 
     *        anames, avalues, err_code)
            do i = 1, xml_set%time_count
              write(xunit, '(1x,i1,$)') xml_set%public_release(i)
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_public_release_tag, err_code)
          else
            call write_xml_start_tag(xunit, XML_passes_qc_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              write(xunit, '(1x,i1,$)') xml_set%passes_qc(i)
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_passes_qc_tag, err_code)
          end if


cc-   Write parameter values

          if (xml_set%time_count .gt. 0) then
            call write_xml_start_tag(xunit, XML_params_tag, acount, anames, 
     *        avalues, err_code)
            write(xunit,'()')

            call write_xml_start_tag(xunit, XML_Hs_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              write(xunit, '(1x,f5.2,$)') xml_set%Hs(i)
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_Hs_tag, err_code)

            call write_xml_start_tag(xunit, XML_Tp_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              if (xml_set%Tp(i) .ge. 100.0) then
                write(xunit, '(1x,f5.0,$)') xml_set%Tp(i)
              else
                write(xunit, '(1x,f5.2,$)') xml_set%Tp(i)
              end if
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_Tp_tag, err_code)

            call write_xml_start_tag(xunit, XML_Ta_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              if (xml_set%Ta(i) .ge. 100.0) then
                write(xunit, '(1x,f5.0,$)') xml_set%Ta(i)
              else
                write(xunit, '(1x,f5.2,$)') xml_set%Ta(i)
              end if
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_Ta_tag, err_code)

            if (xml_set%is_directional) then
              call write_xml_start_tag(xunit, XML_Dp_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,i3,$)') xml_set%Dp(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Dp_tag, err_code)
            end if

            if (xml_set%is_nearshore) then
              call write_xml_start_tag(xunit, XML_Dm_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,i3,$)') xml_set%Dm(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Dm_tag, err_code)

              call write_xml_start_tag(xunit, XML_Sxy_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,f7.4,$)') xml_set%Sxy(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Sxy_tag, err_code)

              call write_xml_start_tag(xunit, XML_Sxx_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,f7.4,$)') xml_set%Sxx(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Sxx_tag, err_code)
            end if

            if (xml_set%has_surfzone) then

              call write_xml_start_tag(xunit, XML_Vm_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,f6.2,$)') xml_set%Vm(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Vm_tag, err_code)

              call write_xml_start_tag(xunit, XML_Hb_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                write(xunit, '(1x,f5.2,$)') xml_set%Hb(i)
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_Hb_tag, err_code)

            end if

            call write_xml_end_tag(xunit, XML_params_tag, err_code)
          end if


cc-   Write spectral values

          if (xml_set%band_count .gt. 0) then
            call write_xml_start_tag(xunit, XML_spectra_tag, acount, anames, 
     *        avalues, err_code)
            write(xunit,'()')

            write(band_count_str(1:5),'(1x,i3,1x)') xml_set%band_count
            call write_xml_element(xunit, XML_band_count_tag, 
     *        band_count_str, acount, anames, avalues, err_code)

            if (xml_set%sample_rate .gt. 0) then
              write(sample_rate_str(1:9),'(1x,f7.4,1x)') xml_set%sample_rate
              call write_xml_element(xunit, XML_sample_rate_tag, 
     *          sample_rate_str, acount, anames, avalues, err_code)
            end if

            if (xml_set%sample_length .gt. 0) then
              write(sample_length_str(1:9),'(1x,f7.1,1x)') xml_set%sample_length
              call write_xml_element(xunit, XML_sample_length_tag, 
     *          sample_length_str, acount, anames, avalues, err_code)
            end if

            call write_xml_start_tag(xunit, XML_frequency_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%band_count
              write(xunit, '(1x,f6.4,$)') xml_set%frequency(i)
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_frequency_tag, err_code)

            call write_xml_start_tag(xunit, XML_band_width_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%band_count
              write(xunit, '(1x,f6.4,$)') xml_set%band_width(i)
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_band_width_tag, err_code)

            call write_xml_start_tag(xunit, XML_ener_dens_tag, acount, anames, 
     *        avalues, err_code)
            do i = 1, xml_set%time_count
              do j = 1, xml_set%band_count
                write(xunit, '(1x,f9.4,$)') xml_set%ener_dens(j,i)
              end do
            end do
            write(xunit,'(a,$)') ' '
            call write_xml_end_tag(xunit, XML_ener_dens_tag, err_code)

            if (xml_set%is_directional) then

              call write_xml_start_tag(xunit, XML_mean_dir_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                do j = 1, xml_set%band_count
                  write(xunit, '(1x,i3,$)') NINT(xml_set%mean_dir(j,i))
                end do
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_mean_dir_tag, err_code)

              call write_xml_start_tag(xunit, XML_a1_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                do j = 1, xml_set%band_count
                  write(xunit, '(1x,f7.4,$)') xml_set%a1(j,i)
                end do
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_a1_tag, err_code)

              call write_xml_start_tag(xunit, XML_b1_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                do j = 1, xml_set%band_count
                  write(xunit, '(1x,f7.4,$)') xml_set%b1(j,i)
                end do
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_b1_tag, err_code)

              call write_xml_start_tag(xunit, XML_a2_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                do j = 1, xml_set%band_count
                  write(xunit, '(1x,f7.4,$)') xml_set%a2(j,i)
                end do
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_a2_tag, err_code)

              call write_xml_start_tag(xunit, XML_b2_tag, acount, anames, 
     *          avalues, err_code)
              do i = 1, xml_set%time_count
                do j = 1, xml_set%band_count
                  write(xunit, '(1x,f7.4,$)') xml_set%b2(j,i)
                end do
              end do
              write(xunit,'(a,$)') ' '
              call write_xml_end_tag(xunit, XML_b2_tag, err_code)

              if (xml_set%proc_code .eq. XML_processing_codes(2)) then
                call write_xml_start_tag(xunit, XML_check_factor_tag, acount,
     *            anames, avalues, err_code)
                do i = 1, xml_set%time_count
                  do j = 1, xml_set%band_count
                    write(xunit, '(1x,f4.2,$)') xml_set%check_factor(j,i)
                  end do
                end do
                write(xunit,'(a,$)') ' '
                call write_xml_end_tag(xunit, XML_check_factor_tag, err_code)
              else if (xml_set%proc_code .eq. XML_processing_codes(1)) then
                call write_xml_start_tag(xunit, XML_bin_coverage_tag, acount,
     *            anames, avalues, err_code)
                do i = 1, xml_set%time_count
                  do j = 1, xml_set%band_count
                    write(xunit, '(1x,f4.2,$)') xml_set%bin_coverage(j,i)
                  end do
                end do
                write(xunit,'(a,$)') ' '
                call write_xml_end_tag(xunit, XML_bin_coverage_tag, err_code)
              end if

            end if

            call write_xml_end_tag(xunit, XML_spectra_tag, err_code)
          end if

          call write_xml_end_tag(xunit, XML_set_tag, err_code)

        end subroutine
   

c-- GET_XML_INDEX --------------------------------------------------------------
c Returns the index of the record closest to the given time, within the time
c range specfied. Returns -1 if no record is found.
c-------------------------------------------------------------------------------
        integer function get_xml_index(xml_set, sp_time, window_mins)
          type(date_block)       sp_time
          type(xml_wave_dataset) xml_set
          integer                window_mins, window_secs

          window_secs = 60*window_mins
          min_secs = window_secs+1
          do i = 1, xml_set%time_count
            if (ABS(secs_diff(sp_time,xml_set%time(i))) .lt. min_secs) then
              min_secs = ABS(secs_diff(sp_time,xml_set%time(i)))
              get_xml_index = i
            end if
          end do
          if (min_secs .gt. window_secs) get_xml_index = -1

        end function


c-- CHECK_TIME_SPAN ------------------------------------------------------------
c Checks if the use_time_span flag should be true or false
c-------------------------------------------------------------------------------
        subroutine check_time_span(xml_set)
          integer timediff, timegap
          logical passes
          type(xml_wave_dataset) xml_set

          if (xml_set%time_count .lt. 3) then
            passes = .false.
          else
            passes = .true.
            timegap = secs_diff(xml_set%time(1), xml_set%time(2))
            i = 3
            do while (passes .and. i .le. xml_set%time_count)
              timediff = secs_diff(xml_set%time(i-1), xml_set%time(i))
              if (timediff .ne. timegap) passes = .false.
              i = i + 1
            end do
          end if

          if (passes) then
            xml_set%use_time_span = .true.
            xml_set%start_time = xml_set%time(1)
            xml_set%end_time = xml_set%time(xml_set%time_count)
            xml_set%time_gap = timegap
          else
            xml_set%use_time_span = .false.
          end if
        end subroutine


c-- EXTRACT_XML_SPECTRUM -------------------------------------------------------
c Takes data from an xml_set and loads it into a sp_data_block
c-------------------------------------------------------------------------------
        subroutine extract_xml_spectrum(xml_set, sp_index, sp_data, err_code)
          integer::              err_code, sp_index
          type(sp_data_block)    sp_data
          type(xml_wave_dataset) xml_set

          sp_data%bands = xml_set%band_count
          do i = 1, xml_set%band_count
            sp_data%freq(i) = xml_set%frequency(i)
            sp_data%band_width(i) = xml_set%band_width(i)
            sp_data%ener_dens(i) = xml_set%ener_dens(i,sp_index)
            if (xml_set%is_directional) then
              sp_data%dir(i) = xml_set%mean_dir(i,sp_index)
              sp_data%a1(i) = xml_set%a1(i,sp_index)
              sp_data%b1(i) = xml_set%b1(i,sp_index)
              sp_data%a2(i) = xml_set%a2(i,sp_index)
              sp_data%b2(i) = xml_set%b2(i,sp_index)
              if (xml_set%is_sensor) then
                sp_data%check(i) = xml_set%check_factor(i,sp_index)
              else
                sp_data%check(i) = xml_set%bin_coverage(i,sp_index)
              end if
            end if
          end do
        end subroutine


c-- MAKE_XML_SPHEADER ----------------------------------------------------------
c Takes data from an xml_set and writes a pseudo-sp header
c-------------------------------------------------------------------------------
        subroutine make_xml_spheader(xml_set, sp_index, out_unit, err_code)
          integer::              err_code, out_unit, sp_index
          character*14           datestr
          character*19           filename
          character*500          sea_list, swl_list, scomment
          type(xml_wave_dataset) xml_set

          i = sp_index
          datestr = make_datestring(xml_set%time(i))
          filename = 'xp'//xml_set%site_id//datestr(1:12)
          write(out_unit,'(a,7/)') filename
          write(out_unit,'(a,f5.2,a,f5.2,a,i3,a,f5.2)') 'Hs(m): ',xml_set%Hs(i),
     *      '   Tp(s): ',xml_set%Tp(i),'   Dp(deg): ',xml_set%Dp(i),
     *      '   Ta(s): ',xml_set%Ta(i)

          if (xml_set%is_sensor) then
            scomment = ''
          else
            scomment = xml_set%model_input_buoys(i)
            swl_list = get_field(scomment, ':', 1)
            sea_list = get_field(scomment, ':', 2)
            scomment = 'Offshore buoys: '//TRIM(swl_list)//';  Local buoys: '
     *        //TRIM(sea_list)
          end if

          write(out_unit,'(a)') TRIM(scomment)
          write(out_unit,'(1x,a,3x,a,6x,a,3x,a,5x,a,7x,a,7x,a,7x,a,4x,a)')
     *      'freq', 'Band','energy', 'Dmean','a1', 'b1','a2', 'b2', 'Check'
          write(out_unit,'(2x,a,4x,a,5x,a,4x,a,39x,a)') 'Hz', 'width',
     *      'm*m/Hz', 'deg', 'factor'
        end subroutine


c-- MERGE_SETS -----------------------------------------------------------------
c Merges the data from two sets into a single set. Note: if times in the two
c sets are intermixed, do not use this routine; call ADD_WAVE_RECORD instead.
c-------------------------------------------------------------------------------
        subroutine merge_sets(base_set, add_set, err_code)
          integer::              acount, bcount, err_code, fcount
          type(xml_wave_dataset) add_set, base_set

          acount = add_set%time_count
          bcount = base_set%time_count

          if (secs_diff(base_set%time(bcount),add_set%time(1)) .le. 0) then
            write(6,'(a)') 'merge_sets() ERROR: overlapping times in xml'
            err_code = -1
            return
          end if

          if (base_set%band_count .ne. add_set%band_count) then
            write(6,'(a)') 'merge_sets() ERROR: spectral layout mismatch'
            err_code = -2
            return
          end if
         
          base_set%time_count = acount + bcount

          base_set%time(bcount+1:bcount+acount) = add_set%time(1:acount)
          base_set%Hs(bcount+1:bcount+acount) = add_set%Hs(1:acount)
          base_set%Tp(bcount+1:bcount+acount) = add_set%Tp(1:acount)
          base_set%Ta(bcount+1:bcount+acount) = add_set%Ta(1:acount)
          if (base_set%is_directional) 
     *      base_set%Dp(bcount+1:bcount+acount) = add_set%Dp(1:acount)
          if (base_set%is_nearshore) then
            base_set%Dm(bcount+1:bcount+acount) = add_set%Dm(1:acount)
            base_set%Sxy(bcount+1:bcount+acount) = add_set%Sxy(1:acount)
            base_set%Sxx(bcount+1:bcount+acount) = add_set%Sxx(1:acount)
          end if
          if (base_set%has_surfzone) then
            base_set%Vm(bcount+1:bcount+acount) = add_set%Vm(1:acount)
            base_set%Hb(bcount+1:bcount+acount) = add_set%Hb(1:acount)
          end if

          fcount = base_set%band_count
          base_set%ener_dens(1:fcount,bcount+1:bcount+acount) = 
     *      add_set%ener_dens(1:fcount,1:acount)
          if (base_set%is_directional) then
            base_set%mean_dir(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%mean_dir(1:fcount,1:acount)
            base_set%a1(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%a1(1:fcount,1:acount)
            base_set%b1(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%b1(1:fcount,1:acount)
            base_set%a2(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%a2(1:fcount,1:acount)
            base_set%b2(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%b2(1:fcount,1:acount)
          end if

          if (base_set%is_sensor) then
            base_set%public_release(bcount+1:bcount+acount) = 
     *        add_set%public_release(1:acount)
            base_set%check_factor(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%check_factor(1:fcount,1:acount)
          else
            base_set%model_input_buoys(bcount+1:bcount+acount) = 
     *        add_set%model_input_buoys(1:acount)
            base_set%passes_qc(bcount+1:bcount+acount) = 
     *        add_set%passes_qc(1:acount)
            base_set%bin_coverage(1:fcount,bcount+1:bcount+acount) = 
     *        add_set%bin_coverage(1:fcount,1:acount)
          end if

          call check_time_span(base_set)

        end subroutine


c-- SPLIT_SET ------------------------------------------------------------------
c Splits the given set into a two sets, dividing at the given time.
c-------------------------------------------------------------------------------
        subroutine split_set(base_set, add_set, split_time, err_code)
          integer::              bcount, err_code, scount, shift
          type(date_block)       split_time, null_time
          type(xml_wave_dataset) base_set, add_set

          bcount = base_set%time_count
          scount = get_xml_index(base_set, split_time, 60*24*31)
          if (scount .lt. 1) then
            err_code = scount-100
            return
          endif

          if (secs_diff(base_set%time(scount),split_time) .gt. 0) 
     *      scount = scount + 1

          if (scount .gt. bcount) then
            err_code = scount
            return
          end if

          shift = scount - 1
          add_set = base_set

          null_time = init_date(0,0,0,0,0,0)
          add_set%time = EOSHIFT(add_set%time, shift, null_time)
          add_set%Hs = EOSHIFT(add_set%Hs, shift)
          add_set%Tp = EOSHIFT(add_set%Tp, shift)
          add_set%Ta = EOSHIFT(add_set%Ta, shift)
          if (add_set%is_directional) 
     *      add_set%Dp = EOSHIFT(add_set%Dp, shift)
          if (add_set%is_nearshore) then
            add_set%Dm = EOSHIFT(add_set%Dm, shift)
            add_set%Sxy = EOSHIFT(add_set%Sxy, shift)
            add_set%Sxx = EOSHIFT(add_set%Sxx, shift)
          end if
          if (add_set%has_surfzone) then
            add_set%Vm = EOSHIFT(add_set%Vm, shift)
            add_set%Hb = EOSHIFT(add_set%Hb, shift)
          end if

          add_set%ener_dens = EOSHIFT(add_set%ener_dens, shift, DIM=2)
          if (base_set%is_directional) then
            add_set%mean_dir = EOSHIFT(add_set%mean_dir, shift, DIM=2)
            add_set%a1 = EOSHIFT(add_set%a1, shift, DIM=2)
            add_set%b1 = EOSHIFT(add_set%b1, shift, DIM=2)
            add_set%a2 = EOSHIFT(add_set%a2, shift, DIM=2)
            add_set%b2 = EOSHIFT(add_set%b2, shift, DIM=2)
          end if

          if (base_set%is_sensor) then
            add_set%public_release = EOSHIFT(add_set%public_release, shift)
            add_set%check_factor = EOSHIFT(add_set%check_factor, shift, DIM=2)
          else
            add_set%model_input_buoys = EOSHIFT(add_set%model_input_buoys, shift)
            add_set%passes_qc = EOSHIFT(add_set%passes_qc, shift)
            add_set%bin_coverage = EOSHIFT(add_set%bin_coverage, shift, DIM=2)
          end if

          base_set%time_count = shift
          add_set%time_count = bcount - shift
          call check_time_span(base_set)
          call check_time_span(add_set)

        end subroutine


c-- REDISTRIBUTE_SET -----------------------------------------------------------
c Changes the spectral layout for all records to the a layout
c-------------------------------------------------------------------------------
        subroutine redistribute_set(xml_set, new_set, sp_layout, err_code)
          integer                 err_code, Dp, proc_code, pub
          real                    Hs, Ta, Tp
          logical                 pub_flag
          character*5             id
          character*14            timestr
          character*500           comment
          type(sp_data_block)     redist_sp, orig_sp, sp_layout
          type(sp_hdr_block)      sp_hdr
          type(xml_wave_dataset)  new_set, xml_set

          err_code = 0
          read(xml_set%proc_code(1:1),'(i1)') proc_code
          call init_xml_waves(new_set, proc_code)

          id = xml_set%site_id
          sp_hdr%sample_rate = xml_set%sample_rate
          sp_hdr%sample_length = xml_set%sample_length
          sp_hdr%position = xml_set%site_loc

          do i = 1, xml_set%time_count
            redist_sp = sp_layout
            call extract_xml_spectrum(xml_set, i, orig_sp, err_code)
            if (err_code .eq. 0) then
              call redistribute_sp(orig_sp, redist_sp)
              call calc_bulk_params(redist_sp, Hs, Tp, Dp, Ta)
              sp_hdr%Hs = Hs
              sp_hdr%Tp = Tp
              sp_hdr%Dp = Dp
              sp_hdr%Ta = Ta
              timestr = make_datestring(xml_set%time(i))
              sp_hdr%filename(1:19) = 'sp'//id//timestr(1:12)
              if (proc_code .eq. 1) then
                pub = xml_set%passes_qc(i)
              else
                pub = xml_set%public_release(i)
              end if
              if (pub .eq. 1) then
                pub_flag = .true.
              else
                pub_flag = .false.
              end if
              call add_wave_record(new_set, redist_sp, sp_hdr, pub_flag, 
     *          err_code)
              if (proc_code .eq. 1) 
     *          new_set%model_input_buoys(i) = xml_set%model_input_buoys(i)
            end if
          end do

          comment = "WARNING: created by redistribute_set(), some fields not "//
     *      "set. ORIGINAL COMMENT: "//TRIM(xml_set%comment)
          call add_xml_comment(new_set, comment)

        end subroutine

c-- FILTER_FREQUENCIES ---------------------------------------------------------
c Takes data from an xml_set and limits the freqs between low and high
c-------------------------------------------------------------------------------
        subroutine filter_frequencies(xml_set, low, high, err_code)
          integer                 err_code, low_index, high_index, Dp
          real                    low, high, Hs, Ta, Tp
          type(sp_data_block)     sp_data
          type(xml_wave_dataset)  xml_set

          err_code = 0
          low_index = 0
          high_index = xml_set%band_count+1

          do i = 1, xml_set%band_count
            if (xml_set%frequency(i) .lt. low-0.0001 .and. i .gt. low_index) 
     *       low_index = i
            if (xml_set%frequency(i) .gt. high+0.0001 .and. i .lt. high_index)
     *       high_index = i
          end do

          xml_set%band_count = high_index - low_index - 1
          if (xml_set%band_count .le. 0) then
            err_code = 1
            write(6,'(a)') 'filter_frequencies() error: empty spectra'
            return
          end if

          xml_set%frequency = EOSHIFT(xml_set%frequency,low_index)
          xml_set%band_width = EOSHIFT(xml_set%band_width,low_index)
          xml_set%ener_dens = EOSHIFT(xml_set%ener_dens,low_index,DIM=1)
          if (xml_set%is_directional) then
            xml_set%mean_dir = EOSHIFT(xml_set%mean_dir,low_index,DIM=1)
            xml_set%a1 = EOSHIFT(xml_set%a1,low_index,DIM=1)
            xml_set%a2 = EOSHIFT(xml_set%a2,low_index,DIM=1)
            xml_set%b1 = EOSHIFT(xml_set%b1,low_index,DIM=1)
            xml_set%b2 = EOSHIFT(xml_set%b2,low_index,DIM=1)
  
            if (xml_set%proc_code .eq. XML_processing_codes(2)) then
              xml_set%check_factor = EOSHIFT(xml_set%check_factor,low_index,DIM=1)
            else if (xml_set%proc_code .eq. XML_processing_codes(1)) then
              xml_set%bin_coverage = EOSHIFT(xml_set%bin_coverage,low_index,DIM=1)
            end if
          end if

c--  Recalculate parameters for smaller spectra

          do j = 1, xml_set%time_count
            if (xml_set%Hs(j) .ne. -1) then
              call extract_xml_spectrum(xml_set, j, sp_data, err_code) 
              call calc_bulk_params(sp_data, Hs, Tp, Dp, Ta)
              xml_set%Hs(j) = Hs
              xml_set%Tp(j) = Tp
              xml_set%Dp(j) = Dp
              xml_set%Ta(j) = Ta
            end if
          end do
          
        end subroutine


c-- CALCULATE_MOMENTS ----------------------------------------------------------
c Calculates the spectral moments m0, m1, m2, m4, and m-1
c-------------------------------------------------------------------------------
        subroutine calculate_moments(xml_set, m0, m1, m2, m4, n1)
          real  bener, m0(1500), m1(1500), m2(1500), m4(1500), n1(1500)
          type(xml_wave_dataset)  xml_set

          do i = 1, xml_set%time_count
            m0(i) = 0.0
            m1(i) = 0.0
            m2(i) = 0.0
            m4(i) = 0.0
            n1(i) = 0.0
            do j = 1, xml_set%band_count
              bener = xml_set%ener_dens(j,i) * xml_set%band_width(j)
              m0(i) = m0(i) + bener
              m1(i) = m1(i) + bener*xml_set%frequency(j)
              m2(i) = m2(i) + bener*xml_set%frequency(j)**2
              m4(i) = m4(i) + bener*xml_set%frequency(j)**4
              n1(i) = n1(i) + bener*xml_set%frequency(j)**(-1)
            end do
          end do
        
        end subroutine


c-- UNSHOAL_SPECTRA ------------------------------------------------------------
c Uses the given shoaling coeffs to calculate deep water spectra
c-------------------------------------------------------------------------------
        subroutine unshoal_spectra(xml_set, shoal)
          integer                 errcode
          real                    shoal(*)
          type(sp_data_block)     sp_data
          type(xml_wave_dataset)  xml_set

          do i = 1, xml_set%time_count
            do j = 1, xml_set%band_count
              xml_set%ener_dens(j,i) = xml_set%ener_dens(j,i) / shoal(j)
            end do
            call extract_xml_spectrum(xml_set, i, sp_data, errcode)
            call calc_bulk_params(sp_data, xml_set%Hs(i), xml_set%Tp(i), 
     *        xml_set%Dp(i), xml_set%Ta(i))
          end do
          xml_set%is_nearshore = .false.	!* unset Sxx, Sxy, Dm
        
        end subroutine


c-- ADD_XML_DIRECTION ----------------------------------------------------------
c Adds null directional fields to a set which has is_directional equal to false.
c Call before adding directional data to a previously non-directional set.
c-------------------------------------------------------------------------------
        subroutine add_xml_direction(xml_set)
          type(xml_wave_dataset)  xml_set
        
          if (xml_set%is_directional) return

          xml_set%is_directional = .true.

          do i = 1, xml_set%time_count
            xml_set%Dp(i) = -1
            do j = 1, xml_set%band_count
              xml_set%mean_dir(j,i) = -1.0
              xml_set%a1(j,i) = 0.0
              xml_set%b1(j,i) = 0.0
              xml_set%a2(j,i) = 0.0
              xml_set%b2(j,i) = 0.0
              xml_set%check_factor(j,i) = -.99
            end do
          end do
          
        end subroutine


c-- ADD_XML_COMMENT ------------------------------------------------------------
c Adds the text to the comment field if it is not already present
c-------------------------------------------------------------------------------
        subroutine add_xml_comment(xml_set, comment)
          type(xml_wave_dataset)  xml_set
          character*500           comment
       
          if (xml_set%comment .eq. '') then
            xml_set%comment = TRIM(comment)
          else if (INDEX(xml_set%comment, comment) .le. 0) then
            xml_set%comment = TRIM(xml_set%comment)//'; '//TRIM(comment)
          end if
        end subroutine


      end !* END MODULE
