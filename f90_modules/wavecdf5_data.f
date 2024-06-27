c-- WAVECDF5_DATA --------------------------------------------------------------
c
c   Defines and manipulates CDIP's netcdf wave datasets. A wavecdf5 dataset
c   contains all the station data in separate groups, and is created using HDF5.
c-------------------------------------------------------------------------------
      module wavecdf5_data

      use datawell_utils
      use dates
      use deploy_info
      use ioc_flags
      use mem_ds
      use metadata_utils
      use mop_utils
      use netcdf
      use netcdf_utils
      use spectral
      use xml_wave_data

      implicit none

      save 

      integer,parameter::   WC5_include_wave_params = 1
      integer,parameter::   WC5_include_wave_spectra = 2
      integer,parameter::   WC5_include_xyz = 4
      integer,parameter::   WC5_include_sst = 8
      integer,parameter::   WC5_include_gps = 16
      integer,parameter::   WC5_include_acm = 32
      integer,parameter::   WC5_include_source = 64
      integer,parameter::   WC5_include_dwr4 = 128
      integer,parameter::   WC5_include_dwr = 256
      integer,parameter::   WC5_include_upcross = 512
      integer,parameter::   WC5_include_sync = 1024
      integer,parameter::   WC5_include_cat4 = 2048

      integer,parameter::   WC5_non_dw_groups = 67
      integer,parameter::   WC5_mk1_groups = 331
      integer,parameter::   WC5_mk2_groups = 347
      integer,parameter::   WC5_mk3_groups = 347
      integer,parameter::   WC5_mk4_groups = 1787
      integer,parameter::   WC5_model_groups = 3

      integer,parameter::   WC5_wave_bit = 0
      integer,parameter::   WC5_spectra_bit = 1
      integer,parameter::   WC5_xyz_bit = 2
      integer,parameter::   WC5_sst_bit = 3
      integer,parameter::   WC5_gps_bit = 4
      integer,parameter::   WC5_acm_bit = 5
      integer,parameter::   WC5_source_bit = 6
      integer,parameter::   WC5_dwr4_bit = 7
      integer,parameter::   WC5_dwr_bit = 8
      integer,parameter::   WC5_upcross_bit = 9
      integer,parameter::   WC5_sync_bit = 10
      integer,parameter::   WC5_cat4_bit = 11

      integer,parameter::   WC5_int_fill = -99999
      real,parameter::      WC5_real_fill = -999.99
      character,parameter:: WC5_char_fill = NF90_FILL_CHAR
      byte,parameter::      WC5_byte_fill = -127

      integer,parameter::   WC5_filename_length = 30	!* changed 2018/5/18 from 19 to 30
      integer,parameter::   WC5_modelinput_length = 100
      integer,parameter::   WC5_upcross_quantile_length = 23

      type wc5_source_group
        integer  count_dimid, flength_dimid, grpid, name_varid

        integer  file_count, file_name_length
        character, allocatable::  file_name(:,:)
      end type

      type wc5_wave_group
        integer  grpid, time_dimid, freq_dimid, dir_dimid, time_varid, freq_varid, dir_varid
        integer  fbounds_varid, tbounds_varid, dbounds_varid
        integer  dp_varid, hs_varid, tp_varid, ta_varid, sptype_varid
        integer  bw_varid, a0_varid, mdir_varid, check_varid
        integer  a1_varid, a2_varid, b1_varid, b2_varid, dirspec_varid
        integer  spread_varid, m2_varid, n2_varid, psdmax_varid, sprdmax_varid, tz_varid
        integer  flags_varid, flags2_varid, fflags_varid, fflags2_varid, src_varid
        integer  modelinput_varid, dm_varid, sxy_varid, sxx_varid
        integer  tint_varid, tener_varid, tm13_varid, tcrest_varid, iqp_varid

        integer  time_count, freq_count, dir_count, tbounds_bot_offset, tbounds_top_offset
        character, allocatable:: model_input(:,:)
        integer, allocatable::   times(:), src_index(:)
        byte, allocatable::      flags(:), flags2(:), fflags(:), fflags2(:)
        real, allocatable::      dp(:), hs(:), tp(:), ta(:), dm(:), sxy(:), sxx(:)
        real, allocatable::      freqs(:), bw(:), a0(:,:), mdir(:,:), check(:,:)
        real, allocatable::      a1(:,:), a2(:,:), b1(:,:), b2(:,:), dirs(:), dirspec(:,:,:)
        real, allocatable::      dspread(:,:), m2(:,:), n2(:,:), tz(:), psdmax(:), spreadmax(:)
        real, allocatable::      tint(:), tener(:), tm13(:), tcrest(:), iqp(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_xyz_group
        integer  grpid, count_dimid, src_varid, stime_varid, srate_varid, delay_varid
        integer  xdisp_varid, ydisp_varid, zdisp_varid, flags_varid, flags2_varid

        integer  rec_count, start_time
        real     sample_rate, filter_delay
        integer, allocatable::   src_index(:)
        byte, allocatable::      flags(:), flags2(:)
        real, allocatable::      xdisp(:), ydisp(:), zdisp(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_sst_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  flags_varid, flags2_varid, sstC_varid, reftemp_varid
        integer  src_varid

        integer  time_count
        integer, allocatable::   times(:), src_index(:)
        byte, allocatable::      flags(:), flags2(:)
        real, allocatable::      sstC(:), reftemp(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_gps_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  flags_varid, latitude_varid, longitude_varid
        integer  src_varid

        integer  time_count
        integer, allocatable::   times(:), src_index(:)
        byte, allocatable::      flags(:)
        real, allocatable::      latitude(:), longitude(:)
        logical, allocatable::   hf_errors(:), merit(:), new_fix(:), mod_ok(:)
      end type

      type wc5_acm_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  flags_varid, flags2_varid, speed_varid, dir_varid, speedstd_varid, dirstd_varid
        integer  rssi1_varid, rssi2_varid, rssi3_varid, csst_varid, status_varid
        integer  vert_varid, vertstd_varid
        integer  src_varid

        integer  time_count
        integer, allocatable::   times(:), src_index(:)
        byte, allocatable::      flags(:), flags2(:), cstatus(:)
        real, allocatable::      speed(:), dir(:), speedstd(:), dirstd(:)
        real, allocatable::      rssi1(:), rssi2(:), rssi3(:), csst(:)
        real, allocatable::      vert(:), vertstd(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_dwr_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  wol_varid, batt_varid
        integer  xa_off_varid,  ya_off_varid, za_off_varid, orient_varid, inclin_varid
        integer  src_varid

        integer  time_count
        integer, allocatable::   times(:), src_index(:), wol(:), batt(:)
        real, allocatable::      za_off(:), xa_off(:), ya_off(:)
        real, allocatable::      orient(:), inclin(:)
      end type

      type wc5_dwr4_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  uptime_varid, enerused_varid, eboost_varid, wol_varid
        integer  hatchtemp_varid, voltage_varid, za_off_varid, za_max_varid
        integer  xa_off_varid, xa_max_varid, ya_off_varid, ya_max_varid 
        integer  orient_mean_varid, orient_dev_varid, inclin_mean_varid, inclin_dev_varid 
        integer  maglength_mean_varid, maglength_dev_varid, pitch_max_varid, roll_max_varid
        integer  src_varid, sensortemp_varid

        integer  time_count
        integer, allocatable::   times(:), uptime(:), wol(:), enerused(:), src_index(:)
        integer, allocatable::   eboost(:), za_max(:), xa_max(:), ya_max(:)
        integer, allocatable::   pitch_max(:), roll_max(:)
        real, allocatable::      hatchtemp(:), voltage(:), za_off(:), xa_off(:), ya_off(:)
        real, allocatable::      orient_mean(:), orient_dev(:), inclin_mean(:), inclin_dev(:)
        real, allocatable::      maglength_mean(:), maglength_dev(:), sensortemp(:)
      end type

      type wc5_upcross_group
        integer  grpid, time_dimid, time_varid, tbounds_varid
        integer  flags_varid, flags2_varid, ncrests_varid, nwaves_varid, bwidth_varid, cov_varid
        integer  Havg_varid, Hmax_varid, Hrms_varid, Htmax_varid, Thmax_varid, Tavg_varid, Tmax_varid
        integer  H10_varid, H3_varid, T10_varid, T3_varid, Th10_varid, Th3_varid, Ht10_varid, Ht3_varid
        integer  Hquant_varid, Tquant_varid
        integer  src_varid

        integer  time_count, quant_count
        integer, allocatable::   times(:), src_index(:), num_crests(:), num_waves(:)
        byte, allocatable::      flags(:), flags2(:)
        real, allocatable::      Havg(:), Hmax(:), Hrms(:), H_at_Tmax(:), T_at_Hmax(:), Tavg(:), Tmax(:)
        real, allocatable::      H10(:), H3(:), HofT10(:), HofT3(:), H_quantile(:,:)
        real, allocatable::      T10(:), T3(:), TofH10(:), TofH3(:), T_quantile(:,:)
        real, allocatable::      bandwidth(:), coverage(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_sync_group
        integer  grpid, time_dimid, time_varid, src_varid, tbounds_varid
        integer  segs_varid, segcnt_varid, samples_varid, disp_varid

        integer  time_count
        integer, allocatable::   times(:), seg_count(:), segs_used(:), samples(:), src_index(:)
        character, allocatable:: disp_hex(:,:)
      end type

      type wc5_cat4_group
        integer  grpid, time_dimid, time_varid, flags_varid, flags2_varid, src_varid, tbounds_varid
        integer  airt_varid, status_varid, white_varid, black_varid, metal_varid, grooved_varid

        integer  time_count
        integer, allocatable::   times(:), status(:), src_index(:)
        real, allocatable::      airt(:), white(:), black(:), metal(:), grooved(:)
        byte, allocatable::      flags(:), flags2(:)
        logical, allocatable::   pub_tf(:)
      end type

      type wc5_dataset
        integer                  ncid, groups
        logical                  classic, unlimited, use_groups
        logical                  is_dwr, is_mk3, is_mk4, is_net_model, is_2d_model, is_non_dw, is_ndbc_obs, is_1Hz_data
        logical                  is_nearshore, is_directional, is_energy_basin, is_stn_aggregate, is_xy_only, is_CAT4
        logical                  is_historic_aggregate, is_dwrg
        type(wc5_source_group)   source
        type(wc5_wave_group)     wave
        type(wc5_gps_group)      gps
        type(wc5_sst_group)      sst
        type(wc5_xyz_group)      xyz
        type(wc5_acm_group)      acm
        type(wc5_dwr_group)      dwr
        type(wc5_dwr4_group)     dwr4
        type(wc5_upcross_group)  upcross
        type(wc5_sync_group)     sync
        type(wc5_cat4_group)     cat4
      end type

      contains


c-- WC5_INITIALIZE_SET ---------------------------------------------------------
c   Prepares a wc5_dataset for use
c-------------------------------------------------------------------------------
        subroutine wc5_initialize_set(wset, gauge, groups)
          integer              i
          integer,optional::   gauge, groups
          type(wc5_dataset), intent(out)::   wset

          call wc5_deallocate_set(wset)
          wset%source%file_count = 0
          wset%source%file_name_length = 0
          wset%wave%time_count = 0
          wset%wave%freq_count = 0
          wset%wave%dir_count = 0
          wset%wave%tbounds_bot_offset = WC5_int_fill
          wset%wave%tbounds_top_offset = WC5_int_fill
          wset%xyz%rec_count = 0
          wset%sst%time_count = 0
          wset%gps%time_count = 0
          wset%acm%time_count = 0
          wset%dwr%time_count = 0
          wset%dwr4%time_count = 0
          wset%upcross%time_count = 0
          wset%upcross%quant_count = WC5_upcross_quantile_length
          wset%sync%time_count = 0
          wset%cat4%time_count = 0
          wset%source%grpid = -1
          wset%wave%grpid = -1
          wset%xyz%grpid = -1
          wset%sst%grpid = -1
          wset%gps%grpid = -1
          wset%acm%grpid = -1
          wset%dwr%grpid = -1
          wset%dwr4%grpid = -1
          wset%upcross%grpid = -1
          wset%sync%grpid = -1
          wset%cat4%grpid = -1
          wset%unlimited = .false.
          wset%use_groups = .false.
          wset%classic = .true.
          wset%is_energy_basin = .false.
          wset%is_stn_aggregate = .false.
          wset%is_historic_aggregate = .false.
          wset%is_ndbc_obs = .false.
          wset%is_net_model = .false.
          wset%is_2d_model = .false.
          wset%is_xy_only = .false.
          wset%is_nearshore = .false.
          wset%is_1Hz_data = .false.
          wset%is_CAT4 = .false.
          wset%is_dwrg = .false.
          wset%is_dwr = .false.
          wset%is_mk4 = .false.
          wset%groups = 0
          if (PRESENT(gauge)) then
            call wc5_init_logicals_from_gauge(wset, gauge)
            call wc5_init_groups_from_gauge(wset, gauge)
          end if
          if (PRESENT(groups)) wset%groups = groups
          call wc5_deallocate_set(wset)
        end subroutine


c-- WC5_INIT_MISSING DISPLACEMENTS ---------------------------------------------
c  Initializes displacement values in a wc5_dataset as missing
c-------------------------------------------------------------------------------
        subroutine wc5_init_missing_displacements(wset, start_stamp, dcount, srate, fdelay)
          integer, intent(in)::   dcount, start_stamp
          real, intent(in)::      fdelay, srate
          integer,allocatable::   zflag1(:), zflag2(:)
          real,allocatable::      x(:), y(:), z(:)
          type(wc5_dataset)   wset

          wset%xyz%rec_count = dcount
          wset%xyz%filter_delay = fdelay
          wset%xyz%start_time = start_stamp
          wset%xyz%sample_rate = srate

          call wc5_allocate_set(wset)
          allocate(x(dcount), y(dcount), z(dcount), zflag1(dcount), zflag2(dcount))
          x = WC5_real_fill
          y = WC5_real_fill
          z = WC5_real_fill
          zflag1 = 9
          zflag2 = 0

          if (wset%xyz%rec_count .gt. 0) then
            wset%xyz%src_index = WC5_int_fill
            wset%xyz%zdisp = z
            wset%xyz%xdisp = x
            wset%xyz%ydisp = y
            wset%xyz%flags = zflag1
            wset%xyz%flags2 = zflag2
          end if
        end subroutine


c-- WC5_COMBINE_REPLACE_SETS ---------------------------------------------------
c   Adds a second dataset into the first set.
c   NOTE: for this implementation, bset and aset are assumed to have a single
c   sp group with the same spectral layout.
c-------------------------------------------------------------------------------
        subroutine wc5_combine_replace_sets(base_set, add_set)
          integer                            errcode
          integer, allocatable::             aindices(:), bindices(:)
          type(wc5_dataset), intent(inout):: base_set
          type(wc5_dataset)::                add_set, new_set, combo_set

          errcode = 0
          if (base_set%wave%freq_count .gt. 0 .and. add_set%wave%freq_count .gt. 0 .and. 
     *        base_set%wave%freq_count .ne. add_set%wave%freq_count) then
            call wc5_redistribute_spectra_nosp(add_set, new_set, base_set, errcode)
            add_set = new_set
          end if
          if (errcode .ne. 0) return

          call wc5_initialize_set(combo_set)
          call wc5_combine_sets(base_set, add_set, combo_set)
          if (base_set%wave%time_count .eq. 0 .and. add_set%wave%time_count .gt. 0) then
            call wc5_assign_logicals(combo_set, add_set)
          else
            call wc5_assign_logicals(combo_set, base_set)
          end if
          combo_set%groups = base_set%groups
          call wc5_deallocate_set(base_set)
          base_set = combo_set
          call wc5_deallocate_set(combo_set)
        end subroutine


c-- WC5_ASSIGN_LOGICALS --------------------------------------------------------
c   Sets the logicals - is_dwr, is_nearshore, etc. - for the first set.
c-------------------------------------------------------------------------------
        subroutine wc5_assign_logicals(dest_set, source_set)
          type(wc5_dataset)      dest_set, source_set
          
          dest_set%is_dwr = source_set%is_dwr
          dest_set%is_mk3 = source_set%is_mk3
          dest_set%is_mk4 = source_set%is_mk4
          dest_set%is_dwrg = source_set%is_dwrg
          dest_set%is_1Hz_data = source_set%is_1Hz_data
          dest_set%is_net_model = source_set%is_net_model
          dest_set%is_2d_model = source_set%is_2d_model
          dest_set%is_non_dw = source_set%is_non_dw
          dest_set%is_nearshore = source_set%is_nearshore
          dest_set%is_directional = source_set%is_directional
          dest_set%is_ndbc_obs = source_set%is_ndbc_obs
          dest_set%is_energy_basin = source_set%is_energy_basin
          dest_set%is_stn_aggregate = source_set%is_stn_aggregate
          dest_set%is_xy_only = source_set%is_xy_only
          dest_set%is_CAT4 = source_set%is_CAT4
          dest_set%is_historic_aggregate = source_set%is_historic_aggregate
        end subroutine


c-- WC5_ASSIGN_GRPIDS ----------------------------------------------------------
c   Adssigns the group ids for the first set.
c-------------------------------------------------------------------------------
        subroutine wc5_assign_grpids(dest_set, source_set)
          type(wc5_dataset)      dest_set, source_set
          
          dest_set%source%grpid = source_set%source%grpid
          dest_set%wave%grpid = source_set%wave%grpid
          dest_set%xyz%grpid = source_set%xyz%grpid
          dest_set%sst%grpid = source_set%sst%grpid
          dest_set%gps%grpid = source_set%gps%grpid
          dest_set%acm%grpid = source_set%acm%grpid
          dest_set%dwr%grpid = source_set%dwr%grpid
          dest_set%dwr4%grpid = source_set%dwr4%grpid
          dest_set%upcross%grpid = source_set%upcross%grpid
          dest_set%sync%grpid = source_set%sync%grpid
          dest_set%cat4%grpid = source_set%cat4%grpid
        end subroutine


c-- WC5_COMBINE_SETS -----------------------------------------------------------
c   Combines two datasets into a single set, and maintains the original sets.
c   (Call wc5_combine_replace_sets if original sets are no longer needed.)
c   NOTE: for this implementation, bset and aset are assumed to have 
c   the same spectral layout.
c-------------------------------------------------------------------------------
        subroutine wc5_combine_sets(aset, bset, cset)
          integer                            i
          integer, allocatable::             aindices(:), bindices(:)
          character*100                      a_sourcename, b_sourcename
          type(wc5_dataset)                  aset, bset
          type(wc5_dataset), intent(out)::   cset

c--  Combine source groups; first check if bset is sourced from the same file as aset

          if (bset%source%file_count .eq. 1 .and. aset%source%file_count .ge. 1) then
            a_sourcename = REPEAT(' ', 100)
            b_sourcename = REPEAT(' ', 100)
            do i = 1, WC5_filename_length
              b_sourcename(i:i) = bset%source%file_name(i,1)
              a_sourcename(i:i) = aset%source%file_name(i,aset%source%file_count)
            end do
            if (TRIM(a_sourcename) .eq. TRIM(b_sourcename)) then
              bset%source%file_count = 0
              bset%wave%src_index = 0
            end if
          end if
        
          call wc5_initialize_set(cset)
          cset%source%file_count = aset%source%file_count + bset%source%file_count
          call wc5_allocate_set(cset)
          if (cset%source%file_count .gt. 0) then
            if (aset%source%file_count .gt. 0)
     *        cset%source%file_name(:,1:aset%source%file_count) = aset%source%file_name
            if (bset%source%file_count .gt. 0)
     *        cset%source%file_name(:,aset%source%file_count+1:cset%source%file_count) = bset%source%file_name
          end if

c--  Combine wave groups

          if (MAX(aset%wave%time_count,bset%wave%time_count) .gt. 0) then
            allocate (aindices(aset%wave%time_count), bindices(bset%wave%time_count))
            call wc5_combine_indices(aset%wave%time_count, bset%wave%time_count, aindices, bindices, aset%wave%times, 
     *        bset%wave%times, cset%wave%time_count)
            cset%wave%freq_count = MAX(aset%wave%freq_count,bset%wave%freq_count)
            cset%wave%dir_count = MAX(aset%wave%dir_count,bset%wave%dir_count)

            call wc5_allocate_set(cset)

            if (aset%wave%freq_count .ne. 0) then
              cset%wave%bw = aset%wave%bw
              cset%wave%freqs = aset%wave%freqs
              cset%wave%fflags = aset%wave%fflags
              cset%wave%fflags2 = aset%wave%fflags2
            else if (bset%wave%freq_count .ne. 0) then
              cset%wave%bw = bset%wave%bw
              cset%wave%freqs = bset%wave%freqs
              cset%wave%fflags = bset%wave%fflags
              cset%wave%fflags2 = bset%wave%fflags2
            end if

            if (aset%wave%dir_count .ne. 0) then
              cset%wave%dirs = aset%wave%dirs
            else if (bset%wave%dir_count .ne. 0) then
              cset%wave%dirs = bset%wave%dirs
            end if

            if (cset%wave%time_count .gt. 0) then
              call wc5_combine_waves(aset, cset, aindices, 0)
              call wc5_combine_waves(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine xyz groups

          if (MAX(aset%xyz%rec_count,bset%xyz%rec_count) .gt. 0) then
            allocate (aindices(aset%xyz%rec_count), bindices(bset%xyz%rec_count))
            if (aset%xyz%rec_count .eq. 0) then
              cset%xyz%sample_rate = bset%xyz%sample_rate 
              cset%xyz%filter_delay = bset%xyz%filter_delay 
              cset%xyz%start_time = bset%xyz%start_time 
              call wc5_combine_indices_xyz(aset%xyz%rec_count, bset%xyz%rec_count, aindices, bindices, 
     *          bset%xyz%start_time, bset%xyz%start_time, bset%xyz%sample_rate, cset%xyz%rec_count)
            else if (bset%xyz%rec_count .eq. 0) then
              cset%xyz%sample_rate = aset%xyz%sample_rate 
              cset%xyz%filter_delay = aset%xyz%filter_delay 
              cset%xyz%start_time = aset%xyz%start_time 
              call wc5_combine_indices_xyz(aset%xyz%rec_count, bset%xyz%rec_count, aindices, bindices, 
     *          aset%xyz%start_time, aset%xyz%start_time, aset%xyz%sample_rate, cset%xyz%rec_count)
            else
              cset%xyz%sample_rate = MAX(aset%xyz%sample_rate, bset%xyz%sample_rate)
              cset%xyz%filter_delay = MAX(aset%xyz%filter_delay, bset%xyz%filter_delay)
              cset%xyz%start_time = MIN(aset%xyz%start_time, bset%xyz%start_time)
              call wc5_combine_indices_xyz(aset%xyz%rec_count, bset%xyz%rec_count, aindices, bindices, 
     *          aset%xyz%start_time, bset%xyz%start_time, aset%xyz%sample_rate, cset%xyz%rec_count)
            end if

            call wc5_allocate_set(cset)
            cset%xyz%flags = 9
            cset%xyz%flags2 = 0
            if (cset%xyz%start_time .eq. 0) cset%xyz%start_time = MAX(aset%xyz%start_time, bset%xyz%start_time)

            if (cset%xyz%rec_count .gt. 0) then
              call wc5_combine_xyz(aset, cset, aindices, 0)
              call wc5_combine_xyz(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine sst groups

          if (MAX(aset%sst%time_count,bset%sst%time_count) .gt. 0) then
            allocate (aindices(aset%sst%time_count), bindices(bset%sst%time_count))
            call wc5_combine_indices(aset%sst%time_count, bset%sst%time_count, aindices, 
     *        bindices, aset%sst%times, bset%sst%times, cset%sst%time_count)
            call wc5_allocate_set(cset)

            if (cset%sst%time_count .gt. 0) then
              call wc5_combine_sst(aset, cset, aindices, 0)
              call wc5_combine_sst(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine gps groups

          if (MAX(aset%gps%time_count,bset%gps%time_count) .gt. 0) then
            allocate (aindices(aset%gps%time_count), bindices(bset%gps%time_count))
            call wc5_combine_indices(aset%gps%time_count, bset%gps%time_count, aindices, 
     *        bindices, aset%gps%times, bset%gps%times, cset%gps%time_count)
            call wc5_allocate_set(cset)

            if (cset%gps%time_count .gt. 0) then
              call wc5_combine_gps(aset, cset, aindices, 0)
              call wc5_combine_gps(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine acm groups

          if (MAX(aset%acm%time_count,bset%acm%time_count) .gt. 0) then
            allocate (aindices(aset%acm%time_count), bindices(bset%acm%time_count))
            call wc5_combine_indices(aset%acm%time_count, bset%acm%time_count, aindices, 
     *        bindices, aset%acm%times, bset%acm%times, cset%acm%time_count)
            call wc5_allocate_set(cset)

            if (cset%acm%time_count .gt. 0) then
              call wc5_combine_acm(aset, cset, aindices, 0)
              call wc5_combine_acm(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine dwr groups

          if (MAX(aset%dwr%time_count,bset%dwr%time_count) .gt. 0) then
            allocate (aindices(aset%dwr%time_count), bindices(bset%dwr%time_count))
            call wc5_combine_indices(aset%dwr%time_count, bset%dwr%time_count, aindices, bindices,
     *        aset%dwr%times, bset%dwr%times, cset%dwr%time_count)
            call wc5_allocate_set(cset)

            if (cset%dwr%time_count .gt. 0) then
              call wc5_combine_dwr(aset, cset, aindices, 0)
              call wc5_combine_dwr(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine dwr4 groups

          if (MAX(aset%dwr4%time_count,bset%dwr4%time_count) .gt. 0) then
            allocate (aindices(aset%dwr4%time_count), bindices(bset%dwr4%time_count))
            call wc5_combine_indices(aset%dwr4%time_count, bset%dwr4%time_count, aindices, 
     *        bindices, aset%dwr4%times, bset%dwr4%times, cset%dwr4%time_count)
            call wc5_allocate_set(cset)

            if (cset%dwr4%time_count .gt. 0) then
              call wc5_combine_dwr4(aset, cset, aindices, 0)
              call wc5_combine_dwr4(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine upcross groups

          if (MAX(aset%upcross%time_count,bset%upcross%time_count) .gt. 0) then
            allocate (aindices(aset%upcross%time_count), bindices(bset%upcross%time_count))
            call wc5_combine_indices(aset%upcross%time_count, bset%upcross%time_count, aindices, 
     *        bindices, aset%upcross%times, bset%upcross%times, cset%upcross%time_count)
            call wc5_allocate_set(cset)

            if (cset%upcross%time_count .gt. 0) then
              call wc5_combine_upcross(aset, cset, aindices, 0)
              call wc5_combine_upcross(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine sync groups

          if (MAX(aset%sync%time_count,bset%sync%time_count) .gt. 0) then
            allocate (aindices(aset%sync%time_count), bindices(bset%sync%time_count))
            call wc5_combine_indices(aset%sync%time_count, bset%sync%time_count, aindices, 
     *        bindices, aset%sync%times, bset%sync%times, cset%sync%time_count)
            call wc5_allocate_set(cset)

            if (cset%sync%time_count .gt. 0) then
              call wc5_combine_sync(aset, cset, aindices, 0)
              call wc5_combine_sync(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

c--  Combine cat4 groups

          if (MAX(aset%cat4%time_count,bset%cat4%time_count) .gt. 0) then
            allocate (aindices(aset%cat4%time_count), bindices(bset%cat4%time_count))
            call wc5_combine_indices(aset%cat4%time_count, bset%cat4%time_count, aindices, 
     *        bindices, aset%cat4%times, bset%cat4%times, cset%cat4%time_count)
            call wc5_allocate_set(cset)

            if (cset%cat4%time_count .gt. 0) then
              call wc5_combine_cat4(aset, cset, aindices, 0)
              call wc5_combine_cat4(bset, cset, bindices, aset%source%file_count)
            end if
            deallocate (aindices, bindices)
          end if

        end subroutine


c-- WC5_FILL_CONTINUOUS_SUBSET -------------------------------------------------
c   Adds records from a subset of a larger set into the main set. Both sets
c   should have equal spacing in the time dimension.
c-------------------------------------------------------------------------------
        subroutine wc5_fill_continuous_subset(subset, fullset, source_offset)
          integer                  base_xindex, cloc, i, prev_offset, source_offset, total_recs
          integer, allocatable::   findices(:), sindices(:)
          logical                  found
          type(wc5_dataset)        fullset, subset

c--  Add to source group 

          if (subset%source%file_count .gt. 0) 
     *      fullset%source%file_name(:,source_offset:source_offset+subset%source%file_count-1) = subset%source%file_name

c--  Fill wave group

          prev_offset = source_offset - 1

          if (subset%wave%time_count .gt. 0 .and. fullset%wave%time_count .gt. 0) then
            if (fullset%wave%freq_count .eq. 0 .and. subset%wave%freq_count .ne. 0) then
              fullset%wave%freq_count = subset%wave%freq_count
              call wc5_allocate_set(fullset)
              fullset%wave%bw = subset%wave%bw
              fullset%wave%freqs = subset%wave%freqs
              fullset%wave%fflags = subset%wave%fflags
              fullset%wave%fflags2 = subset%wave%fflags2
            end if
            if (subset%wave%dir_count .ne. 0 .and. fullset%wave%dir_count .eq. 0) then
              fullset%wave%dir_count = subset%wave%dir_count
              call wc5_allocate_set(fullset)
              fullset%wave%dirs = subset%wave%dirs
            end if

            allocate (sindices(subset%wave%time_count))
            call nc_find_time_index(fullset%wave%times, fullset%wave%time_count, 
     *        timestamp_to_date(subset%wave%times(1)), 1, cloc, found)
            if (found) then
c             write(6,*) 'Wave idx: ', cloc
              do i = 1, subset%wave%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_waves(subset, fullset, sindices, prev_offset)
              deallocate (sindices)
            end if
          end if

c--  Fill xyz group

          if (fullset%xyz%rec_count .gt. 0 .and. subset%xyz%rec_count .gt. 0) then
            if (fullset%xyz%sample_rate .eq. 0) fullset%xyz%sample_rate = subset%xyz%sample_rate
            if (fullset%xyz%filter_delay .eq. 0) fullset%xyz%filter_delay = subset%xyz%filter_delay
            allocate (sindices(subset%xyz%rec_count))
            base_xindex = NINT((subset%xyz%start_time - fullset%xyz%start_time)*fullset%xyz%sample_rate)
            sindices(1) = NINT((subset%xyz%start_time - fullset%xyz%start_time)*fullset%xyz%sample_rate)
            do i = 1, subset%xyz%rec_count
              sindices(i) = base_xindex + i - 1
              if (sindices(i) .lt. 0) sindices(i) = 0
            end do
            call wc5_combine_xyz(subset, fullset, sindices, prev_offset)
            deallocate (sindices)
          end if

c--  Fill sst group

          if (subset%sst%time_count .gt. 0 .and. fullset%sst%time_count .gt. 0) then
            allocate (sindices(subset%sst%time_count))
            call nc_find_time_index(fullset%sst%times, fullset%sst%time_count, 
     *        timestamp_to_date(subset%sst%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%sst%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_sst(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill gps group

          if (subset%gps%time_count .gt. 0 .and. fullset%gps%time_count .gt. 0) then
            allocate (sindices(subset%gps%time_count))
            call nc_find_time_index(fullset%gps%times, fullset%gps%time_count, 
     *        timestamp_to_date(subset%gps%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%gps%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_gps(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill acm group

          if (subset%acm%time_count .gt. 0 .and. fullset%acm%time_count .gt. 0) then
            allocate (sindices(subset%acm%time_count))
            call nc_find_time_index(fullset%acm%times, fullset%acm%time_count, 
     *        timestamp_to_date(subset%acm%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%acm%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_acm(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill dwr group

          if (fullset%dwr%time_count .gt. 0 .and. subset%dwr%time_count .gt. 0) then
            allocate (sindices(subset%dwr%time_count))
            call nc_find_time_index(fullset%dwr%times, fullset%dwr%time_count, 
     *        timestamp_to_date(subset%dwr%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%dwr%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_dwr(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill dwr4 group

          if (fullset%dwr4%time_count .gt. 0 .and. subset%dwr4%time_count .gt. 0) then
            allocate (sindices(subset%dwr4%time_count))
            call nc_find_time_index(fullset%dwr4%times, fullset%dwr4%time_count, 
     *        timestamp_to_date(subset%dwr4%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%dwr4%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_dwr4(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill upcross group

          if (fullset%upcross%time_count .gt. 0 .and. subset%upcross%time_count .gt. 0) then
            allocate (sindices(subset%upcross%time_count))
            call nc_find_time_index(fullset%upcross%times, fullset%upcross%time_count, 
     *        timestamp_to_date(subset%upcross%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%upcross%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_upcross(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill sync group

          if (fullset%sync%time_count .gt. 0 .and. subset%sync%time_count .gt. 0) then
            allocate (sindices(subset%sync%time_count))
            call nc_find_time_index(fullset%sync%times, fullset%sync%time_count, 
     *        timestamp_to_date(subset%sync%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%sync%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_sync(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

c--  Fill cat4 group

          if (fullset%cat4%time_count .gt. 0 .and. subset%cat4%time_count .gt. 0) then
            allocate (sindices(subset%cat4%time_count))
            call nc_find_time_index(fullset%cat4%times, fullset%cat4%time_count, 
     *        timestamp_to_date(subset%cat4%times(1)), 1, cloc, found)
            if (found) then
              do i = 1, subset%cat4%time_count
                sindices(i) = cloc + i - 1
              end do
              call wc5_combine_cat4(subset, fullset, sindices, prev_offset)
            end if
            deallocate (sindices)
          end if

        end subroutine


c-- WC5_APPLY_FLAGS -------------------------------------------------------------
c   Uses the primary flag values to limit a dataset to 'good' records.
c   Note that the xyz group is excluded since it cannot be resized. Other
c   groups without flags use the 'src_index' variable as a proxy flag.
c-------------------------------------------------------------------------------
        subroutine wc5_apply_flags(aset, bset, copy_xyz)
          integer                            groups, gps_fval, shape_2d(2), shape_3d(3)
          integer, allocatable::             flags2d(:,:), flags3d(:,:,:)
          logical                            include_xyz
          logical, optional::                copy_xyz
          logical, allocatable::             logicals2d(:,:)
          type(wc5_dataset), intent(in)::    aset
          type(wc5_dataset), intent(out)::   bset

          groups = wc5_get_group_value(aset)
          call wc5_initialize_set(bset)
          call wc5_assign_logicals(bset, aset)

          include_xyz = .false.
          if (PRESENT(copy_xyz)) then
            if (copy_xyz .eqv. .true.) include_xyz = .true.
          end if

c--  Add source group

          if (aset%source%file_count .gt. 0) then
            bset%source%file_count = aset%source%file_count
            call wc5_allocate_set(bset)
            if (bset%source%file_count .gt. 0) bset%source%file_name = aset%source%file_name
          end if

c--  Flag off wave group

          if (aset%wave%time_count .gt. 0) then
            bset%wave%time_count = COUNT(aset%wave%flags .eq. 1)
            if (bset%wave%time_count .gt. 0) then

              bset%wave%freq_count = aset%wave%freq_count
              bset%wave%dir_count = aset%wave%dir_count

              call wc5_allocate_set(bset)

              bset%wave%times = PACK(aset%wave%times, (aset%wave%flags .eq. 1))
              bset%wave%flags = PACK(aset%wave%flags, (aset%wave%flags .eq. 1))
              bset%wave%flags2 = PACK(aset%wave%flags2, (aset%wave%flags .eq. 1))
              bset%wave%pub_tf = PACK(aset%wave%pub_tf, (aset%wave%flags .eq. 1))

              if (aset%is_dwr .or. aset%is_mk4 .or. MAXVAL(aset%wave%src_index) .gt. 0) 
     *          bset%wave%src_index = PACK(aset%wave%src_index, (aset%wave%flags .eq. 1))
c             if (aset%is_net_model) bset%wave%model_input(:,indices(i)) = aset%wave%model_input(:,i)

              bset%wave%hs = PACK(aset%wave%hs, (aset%wave%flags .eq. 1))
              bset%wave%tp = PACK(aset%wave%tp, (aset%wave%flags .eq. 1))
              bset%wave%ta = PACK(aset%wave%ta, (aset%wave%flags .eq. 1))
              bset%wave%dp = PACK(aset%wave%dp, (aset%wave%flags .eq. 1))

              if (aset%is_dwr .or. aset%is_mk4) then
                bset%wave%tz = PACK(aset%wave%tz, (aset%wave%flags .eq. 1))
                bset%wave%psdmax = PACK(aset%wave%psdmax, (aset%wave%flags .eq. 1))
              end if
              if (aset%is_mk4) then
                bset%wave%spreadmax = PACK(aset%wave%spreadmax, (aset%wave%flags .eq. 1))
                bset%wave%tint = PACK(aset%wave%tint, (aset%wave%flags .eq. 1))
                bset%wave%tener = PACK(aset%wave%tener, (aset%wave%flags .eq. 1))
                bset%wave%tm13 = PACK(aset%wave%tm13, (aset%wave%flags .eq. 1))
                bset%wave%tcrest = PACK(aset%wave%tcrest, (aset%wave%flags .eq. 1))
                bset%wave%iqp = PACK(aset%wave%iqp, (aset%wave%flags .eq. 1))
              end if
 
              if (aset%wave%freq_count .gt. 0) then
                bset%wave%bw = aset%wave%bw
                bset%wave%freqs = aset%wave%freqs
                bset%wave%fflags = aset%wave%fflags
                bset%wave%fflags2 = aset%wave%fflags2
                if (aset%wave%dir_count .ne. 0) bset%wave%dirs = aset%wave%dirs

                allocate (flags2d(aset%wave%freq_count, aset%wave%time_count))
                flags2d = SPREAD(aset%wave%flags, 1, aset%wave%freq_count)
                shape_2d = (/ bset%wave%freq_count, bset%wave%time_count /)

                bset%wave%a0 = RESHAPE(PACK(aset%wave%a0, flags2d .eq. 1), shape_2d)
                bset%wave%a1 = RESHAPE(PACK(aset%wave%a1, flags2d .eq. 1), shape_2d)
                bset%wave%a2 = RESHAPE(PACK(aset%wave%a2, flags2d .eq. 1), shape_2d)
                bset%wave%b1 = RESHAPE(PACK(aset%wave%b1, flags2d .eq. 1), shape_2d)
                bset%wave%b2 = RESHAPE(PACK(aset%wave%b2, flags2d .eq. 1), shape_2d)
                bset%wave%mdir = RESHAPE(PACK(aset%wave%mdir, flags2d .eq. 1), shape_2d)

                if (aset%is_dwr .or. aset%is_mk4) then
                  bset%wave%check = RESHAPE(PACK(aset%wave%check, flags2d .eq. 1), shape_2d)
                  bset%wave%dspread = RESHAPE(PACK(aset%wave%dspread, flags2d .eq. 1), shape_2d)
                  bset%wave%m2 = RESHAPE(PACK(aset%wave%m2, flags2d .eq. 1), shape_2d)
                  bset%wave%n2 = RESHAPE(PACK(aset%wave%n2, flags2d .eq. 1), shape_2d)
                end if

                if (aset%is_nearshore) then
                  bset%wave%dm = PACK(aset%wave%dm, (aset%wave%flags .eq. 1))
                  bset%wave%sxy = PACK(aset%wave%sxy, (aset%wave%flags .eq. 1))
                  bset%wave%sxx = PACK(aset%wave%sxx, (aset%wave%flags .eq. 1))
                end if

c               if (aset%is_net_model) bset%wave%check(:,indices(i)) = aset%wave%check(:,i)
                if (aset%is_2d_model) then
                  allocate (flags3d(aset%wave%dir_count, aset%wave%freq_count, aset%wave%time_count))
                  flags3d = SPREAD(flags2d, 1, aset%wave%dir_count)
                  shape_3d = (/ bset%wave%dir_count, bset%wave%freq_count, bset%wave%time_count /)
                  bset%wave%dirspec = RESHAPE(PACK(aset%wave%dirspec, flags3d .eq. 1), shape_3d)
                  deallocate(flags3d)
                end if
                deallocate(flags2d)
              end if
            end if

          end if

c--  Copy xyz group if requested

          if (include_xyz .and. aset%xyz%rec_count .gt. 0) then
            bset%xyz%rec_count = aset%xyz%rec_count
            call wc5_allocate_set(bset)
            bset%xyz%start_time = aset%xyz%start_time
            bset%xyz%sample_rate = aset%xyz%sample_rate
            bset%xyz%filter_delay = aset%xyz%filter_delay
            bset%xyz%src_index = aset%xyz%src_index
            bset%xyz%flags = aset%xyz%flags
            bset%xyz%flags2 = aset%xyz%flags2
            bset%xyz%xdisp = aset%xyz%xdisp
            bset%xyz%ydisp = aset%xyz%ydisp
            bset%xyz%zdisp = aset%xyz%zdisp
          end if

c--  Flag off sst group

          if (aset%sst%time_count .gt. 0) then
            bset%sst%time_count = COUNT(aset%sst%flags .eq. 1)

            if (bset%sst%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%sst%times = PACK(aset%sst%times, (aset%sst%flags .eq. 1))
              bset%sst%flags = PACK(aset%sst%flags, (aset%sst%flags .eq. 1))
              bset%sst%flags2 = PACK(aset%sst%flags2, (aset%sst%flags .eq. 1))
              bset%sst%pub_tf = PACK(aset%sst%pub_tf, (aset%sst%flags .eq. 1))
              bset%sst%src_index = PACK(aset%sst%src_index, (aset%sst%flags .eq. 1))
              bset%sst%sstC = PACK(aset%sst%sstC, (aset%sst%flags .eq. 1))
              bset%sst%reftemp = PACK(aset%sst%reftemp, (aset%sst%flags .eq. 1))
            end if
          end if

c--  Flag off gps group

          if (aset%gps%time_count .gt. 0) then
            if (MAXVAL(aset%gps%flags) .eq. WC5_byte_fill) then
              gps_fval = WC5_byte_fill
            else
              gps_fval = 3
            end if
            bset%gps%time_count = COUNT(aset%gps%flags .eq. gps_fval)

            if (bset%gps%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%gps%times = PACK(aset%gps%times, (aset%gps%flags .eq. gps_fval))
              bset%gps%flags = PACK(aset%gps%flags, (aset%gps%flags .eq. gps_fval))
              bset%gps%src_index = PACK(aset%gps%src_index, (aset%gps%flags .eq. gps_fval))
              bset%gps%latitude = PACK(aset%gps%latitude, (aset%gps%flags .eq. gps_fval))
              bset%gps%longitude = PACK(aset%gps%longitude, (aset%gps%flags .eq. gps_fval))
              bset%gps%merit = PACK(aset%gps%merit, (aset%gps%flags .eq. gps_fval))
              bset%gps%new_fix = PACK(aset%gps%new_fix, (aset%gps%flags .eq. gps_fval))
              bset%gps%mod_ok = PACK(aset%gps%mod_ok, (aset%gps%flags .eq. gps_fval))
              bset%gps%hf_errors = PACK(aset%gps%hf_errors, (aset%gps%flags .eq. gps_fval))
            end if
          end if

c--  Flag off the dwr group based on the 'src_index' variable

          if (aset%dwr%time_count .gt. 0) then
            bset%dwr%time_count = COUNT(aset%dwr%src_index .ne. WC5_int_fill)

            if (bset%dwr%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%dwr%times = PACK(aset%dwr%times, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%src_index = PACK(aset%dwr%src_index, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%wol = PACK(aset%dwr%wol, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%batt = PACK(aset%dwr%batt, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%za_off = PACK(aset%dwr%za_off, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%xa_off = PACK(aset%dwr%xa_off, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%ya_off = PACK(aset%dwr%ya_off, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%orient = PACK(aset%dwr%orient, (aset%dwr%src_index .ne. WC5_int_fill))
              bset%dwr%inclin = PACK(aset%dwr%inclin, (aset%dwr%src_index .ne. WC5_int_fill))
            end if
          end if

c--  Flag off the dwr4 group based on the 'src_index' variable

          if (aset%dwr4%time_count .gt. 0) then
            bset%dwr4%time_count = COUNT(aset%dwr4%src_index .ne. WC5_int_fill)

            if (bset%dwr4%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%dwr4%times = PACK(aset%dwr4%times, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%uptime = PACK(aset%dwr4%uptime, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%enerused = PACK(aset%dwr4%enerused, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%src_index = PACK(aset%dwr4%src_index, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%wol = PACK(aset%dwr4%wol, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%eboost = PACK(aset%dwr4%eboost, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%za_max = PACK(aset%dwr4%za_max, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%xa_max = PACK(aset%dwr4%xa_max, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%ya_max = PACK(aset%dwr4%ya_max, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%pitch_max = PACK(aset%dwr4%pitch_max, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%roll_max = PACK(aset%dwr4%roll_max, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%hatchtemp = PACK(aset%dwr4%hatchtemp, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%voltage = PACK(aset%dwr4%voltage, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%za_off = PACK(aset%dwr4%za_off, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%xa_off = PACK(aset%dwr4%xa_off, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%ya_off = PACK(aset%dwr4%ya_off, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%orient_mean = PACK(aset%dwr4%orient_mean, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%orient_dev = PACK(aset%dwr4%orient_dev, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%inclin_mean = PACK(aset%dwr4%inclin_mean, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%inclin_dev = PACK(aset%dwr4%inclin_dev, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%maglength_mean = PACK(aset%dwr4%maglength_mean, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%maglength_dev = PACK(aset%dwr4%maglength_dev, (aset%dwr4%src_index .ne. WC5_int_fill))
              bset%dwr4%sensortemp = PACK(aset%dwr4%sensortemp, (aset%dwr4%src_index .ne. WC5_int_fill))
            end if
          end if

c--  Flag off acm group

          if (aset%acm%time_count .gt. 0) then
            bset%acm%time_count = COUNT(aset%acm%flags .eq. 1)

            if (bset%acm%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%acm%times = PACK(aset%acm%times, (aset%acm%flags .eq. 1))
              bset%acm%flags = PACK(aset%acm%flags, (aset%acm%flags .eq. 1))
              bset%acm%flags2 = PACK(aset%acm%flags2, (aset%acm%flags .eq. 1))
              bset%acm%pub_tf = PACK(aset%acm%pub_tf, (aset%acm%flags .eq. 1))
              bset%acm%src_index = PACK(aset%acm%src_index, (aset%acm%flags .eq. 1))
              bset%acm%cstatus = PACK(aset%acm%cstatus, (aset%acm%flags .eq. 1))
              bset%acm%speed = PACK(aset%acm%speed, (aset%acm%flags .eq. 1))
              bset%acm%dir = PACK(aset%acm%dir, (aset%acm%flags .eq. 1))
              bset%acm%speedstd = PACK(aset%acm%speedstd, (aset%acm%flags .eq. 1))
              bset%acm%dirstd = PACK(aset%acm%dirstd, (aset%acm%flags .eq. 1))
              bset%acm%rssi1 = PACK(aset%acm%rssi1, (aset%acm%flags .eq. 1))
              bset%acm%rssi2 = PACK(aset%acm%rssi2, (aset%acm%flags .eq. 1))
              bset%acm%rssi3 = PACK(aset%acm%rssi3, (aset%acm%flags .eq. 1))
              bset%acm%csst = PACK(aset%acm%csst, (aset%acm%flags .eq. 1))
              bset%acm%vert = PACK(aset%acm%vert, (aset%acm%flags .eq. 1))
              bset%acm%vertstd = PACK(aset%acm%vertstd, (aset%acm%flags .eq. 1))
            end if
          end if

c--  Flag off upcross group

          if (aset%upcross%time_count .gt. 0) then
            bset%upcross%time_count = COUNT(aset%upcross%flags .eq. 1)

            if (bset%upcross%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%upcross%times = PACK(aset%upcross%times, (aset%upcross%flags .eq. 1))
              bset%upcross%flags = PACK(aset%upcross%flags, (aset%upcross%flags .eq. 1))
              bset%upcross%flags2 = PACK(aset%upcross%flags2, (aset%upcross%flags .eq. 1))
              bset%upcross%pub_tf = PACK(aset%upcross%pub_tf, (aset%upcross%flags .eq. 1))
              bset%upcross%src_index = PACK(aset%upcross%src_index, (aset%upcross%flags .eq. 1))
              bset%upcross%num_crests = PACK(aset%upcross%num_crests, (aset%upcross%flags .eq. 1))
              bset%upcross%num_waves = PACK(aset%upcross%num_waves, (aset%upcross%flags .eq. 1))
              bset%upcross%Havg = PACK(aset%upcross%Havg, (aset%upcross%flags .eq. 1))
              bset%upcross%Hmax = PACK(aset%upcross%Hmax, (aset%upcross%flags .eq. 1))
              bset%upcross%Hrms = PACK(aset%upcross%Hrms, (aset%upcross%flags .eq. 1))
              bset%upcross%H_at_Tmax = PACK(aset%upcross%H_at_Tmax, (aset%upcross%flags .eq. 1))
              bset%upcross%T_at_Hmax = PACK(aset%upcross%T_at_Hmax, (aset%upcross%flags .eq. 1))
              bset%upcross%Tavg = PACK(aset%upcross%Tavg, (aset%upcross%flags .eq. 1))
              bset%upcross%Tmax = PACK(aset%upcross%Tmax, (aset%upcross%flags .eq. 1))
              bset%upcross%bandwidth = PACK(aset%upcross%bandwidth, (aset%upcross%flags .eq. 1))
              bset%upcross%coverage = PACK(aset%upcross%coverage, (aset%upcross%flags .eq. 1))

              bset%upcross%H10 = PACK(aset%upcross%H10, (aset%upcross%flags .eq. 1))
              bset%upcross%TofH10 = PACK(aset%upcross%TofH10, (aset%upcross%flags .eq. 1))
              bset%upcross%H3 = PACK(aset%upcross%H3, (aset%upcross%flags .eq. 1))
              bset%upcross%TofH3 = PACK(aset%upcross%TofH3, (aset%upcross%flags .eq. 1))
c             bset%upcross%T10 = PACK(aset%upcross%T10, (aset%upcross%flags .eq. 1))
c             bset%upcross%HofT10 = PACK(aset%upcross%HofT10, (aset%upcross%flags .eq. 1))
c             bset%upcross%T3 = PACK(aset%upcross%T3, (aset%upcross%flags .eq. 1))
c             bset%upcross%HofT3 = PACK(aset%upcross%HofT3, (aset%upcross%flags .eq. 1))

              allocate (flags2d(aset%upcross%quant_count, aset%upcross%time_count))
              flags2d = SPREAD(aset%upcross%flags, 1, aset%upcross%quant_count)
              shape_2d = (/ bset%upcross%quant_count, bset%upcross%time_count /)
              bset%upcross%H_quantile = RESHAPE(PACK(aset%upcross%H_quantile, flags2d .eq. 1), shape_2d)
c             bset%upcross%T_quantile = RESHAPE(PACK(aset%upcross%T_quantile, flags2d .eq. 1), shape_2d)
              deallocate(flags2d)
            end if
          end if

c--  Flag off the sync group based on the 'src_index' variable

          if (aset%sync%time_count .gt. 0) then
            bset%sync%time_count = COUNT(aset%sync%src_index .ne. WC5_int_fill)

            if (bset%sync%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%sync%times = PACK(aset%sync%times, (aset%sync%src_index .ne. WC5_int_fill))
              bset%sync%src_index = PACK(aset%sync%src_index, (aset%sync%src_index .ne. WC5_int_fill))
              bset%sync%seg_count = PACK(aset%sync%seg_count, (aset%sync%src_index .ne. WC5_int_fill))
              bset%sync%segs_used = PACK(aset%sync%segs_used, (aset%sync%src_index .ne. WC5_int_fill))
              bset%sync%samples = PACK(aset%sync%samples, (aset%sync%src_index .ne. WC5_int_fill))

              allocate (logicals2d(18, aset%sync%time_count))
              logicals2d = SPREAD((aset%sync%src_index .ne. WC5_int_fill), 1, 18)
              shape_2d = (/ 18, bset%sync%time_count /)
              bset%sync%disp_hex = RESHAPE(PACK(aset%sync%disp_hex, logicals2d .eqv. .true.), shape_2d)
              deallocate(logicals2d)
            end if
          end if

c--  Flag off cat4 group

          if (aset%cat4%time_count .gt. 0) then
            bset%cat4%time_count = COUNT(aset%cat4%flags .eq. 1)

            if (bset%cat4%time_count .gt. 0) then
              call wc5_allocate_set(bset)
              bset%cat4%times = PACK(aset%cat4%times, (aset%cat4%flags .eq. 1))
              bset%cat4%flags = PACK(aset%cat4%flags, (aset%cat4%flags .eq. 1))
              bset%cat4%flags2 = PACK(aset%cat4%flags2, (aset%cat4%flags .eq. 1))
              bset%cat4%pub_tf = PACK(aset%cat4%pub_tf, (aset%cat4%flags .eq. 1))
              bset%cat4%src_index = PACK(aset%cat4%src_index, (aset%cat4%flags .eq. 1))
              bset%cat4%airt = PACK(aset%cat4%airt, (aset%cat4%flags .eq. 1))
              bset%cat4%status = PACK(aset%cat4%status, (aset%cat4%flags .eq. 1))
              bset%cat4%white = PACK(aset%cat4%white, (aset%cat4%flags .eq. 1))
              bset%cat4%black = PACK(aset%cat4%black, (aset%cat4%flags .eq. 1))
              bset%cat4%metal = PACK(aset%cat4%metal, (aset%cat4%flags .eq. 1))
              bset%cat4%grooved = PACK(aset%cat4%grooved, (aset%cat4%flags .eq. 1))
            end if
          end if
        end subroutine


c-- WC5_TRIM_SOURCE_NAMES ------------------------------------------------------
c   Removes entries with blank filenames from the source group.
c-------------------------------------------------------------------------------
        subroutine wc5_trim_source_names(wset)
          integer                   file_count, i, j
          character*100             fname
          character, allocatable::  name_store(:,:)
          type(wc5_dataset)         wset

          allocate(name_store(WC5_filename_length, wset%source%file_count))
          file_count = 0
          do i = 1, wset%source%file_count
            fname = ''
            do j = 1, WC5_filename_length
              fname(j:j) = wset%source%file_name(j,i)
            end do
            if (LEN_TRIM(fname) .gt. 0) then
              if (IACHAR(fname(1:1)) .ge. IACHAR('a') .and. IACHAR(fname(1:1)) .le. IACHAR('z')) then
c               write(6,*) '->', TRIM(fname), '<-'
                file_count = file_count + 1
                do j = 1, WC5_filename_length
                  name_store(j,i) = fname(j:j)
                end do
              end if
            end if
          end do

          write(6,*) 'File count: ', file_count
          deallocate(wset%source%file_name)
          if (file_count .gt. 0) then
            wset%source%file_count = file_count
            allocate(wset%source%file_name(WC5_filename_length, file_count))
            do i = 1, wset%source%file_count
              do j = 1, WC5_filename_length
                wset%source%file_name(j,i) = name_store(j,i)
              end do
            end do
          end if
        end subroutine


c-- WC5_TRIM_WAVE_FREQS --------------------------------------------------------
c   Uses the primary frequency flag values to trim wave data back to the
c   the specified 'good' frequencies.
c-------------------------------------------------------------------------------
        subroutine wc5_trim_wave_freqs(aset, bset, errcode)
          integer                            errcode, groups, gps_fval, shape_2d(2), shape_3d(3)
          integer, allocatable::             flags2d(:,:), flags3d(:,:,:)
          type(wc5_dataset), intent(in)::    aset
          type(wc5_dataset), intent(out)::   bset

          groups = wc5_get_group_value(aset)
          call wc5_initialize_set(bset)

c--  Trim wave frequencies

          if (aset%wave%freq_count .gt. 0) then
            bset%wave%time_count = aset%wave%time_count
            bset%wave%freq_count = COUNT(aset%wave%fflags .eq. 1)
            if (bset%wave%freq_count .eq. aset%wave%freq_count) then
              bset = aset
            else if (bset%wave%freq_count .gt. 0) then
              bset%wave%dir_count = aset%wave%dir_count
              call wc5_allocate_set(bset)

              bset%wave%times = aset%wave%times
              bset%wave%flags = aset%wave%flags
              bset%wave%flags2 = aset%wave%flags2
              bset%wave%pub_tf = aset%wave%pub_tf

              if (aset%is_dwr .or. aset%is_mk4 .or. MAXVAL(aset%wave%src_index) .gt. 0) 
     *          bset%wave%src_index = aset%wave%src_index
c             if (aset%is_net_model) bset%wave%model_input(:,indices(i)) = aset%wave%model_input(:,i)

              if (aset%is_mk4) then
                bset%wave%spreadmax = WC5_real_fill
                bset%wave%tint = WC5_real_fill
                bset%wave%tener = WC5_real_fill
                bset%wave%tm13 = WC5_real_fill
                bset%wave%tcrest = WC5_real_fill
                bset%wave%iqp = WC5_real_fill
              end if

              bset%wave%bw = PACK(aset%wave%bw, (aset%wave%fflags .eq. 1))
              bset%wave%freqs = PACK(aset%wave%freqs, (aset%wave%fflags .eq. 1))
              bset%wave%fflags = 1
              bset%wave%fflags2 = PACK(aset%wave%fflags2, (aset%wave%fflags .eq. 1))
              if (aset%wave%dir_count .ne. 0) bset%wave%dirs = aset%wave%dirs

              allocate (flags2d(aset%wave%freq_count, aset%wave%time_count))
              flags2d = SPREAD(aset%wave%fflags, 2, aset%wave%time_count)
              shape_2d = (/ bset%wave%freq_count, bset%wave%time_count /)

              bset%wave%a0 = RESHAPE(PACK(aset%wave%a0, flags2d .eq. 1), shape_2d)
              bset%wave%a1 = RESHAPE(PACK(aset%wave%a1, flags2d .eq. 1), shape_2d)
              bset%wave%a2 = RESHAPE(PACK(aset%wave%a2, flags2d .eq. 1), shape_2d)
              bset%wave%b1 = RESHAPE(PACK(aset%wave%b1, flags2d .eq. 1), shape_2d)
              bset%wave%b2 = RESHAPE(PACK(aset%wave%b2, flags2d .eq. 1), shape_2d)
              bset%wave%mdir = RESHAPE(PACK(aset%wave%mdir, flags2d .eq. 1), shape_2d)

              if (aset%is_dwr .or. aset%is_mk4) then
                bset%wave%check = RESHAPE(PACK(aset%wave%check, flags2d .eq. 1), shape_2d)
                bset%wave%dspread = RESHAPE(PACK(aset%wave%dspread, flags2d .eq. 1), shape_2d)
                bset%wave%m2 = RESHAPE(PACK(aset%wave%m2, flags2d .eq. 1), shape_2d)
                bset%wave%n2 = RESHAPE(PACK(aset%wave%n2, flags2d .eq. 1), shape_2d)
              end if

              if (aset%is_nearshore) then
                bset%wave%dm = WC5_real_fill
                bset%wave%sxy = WC5_real_fill
                bset%wave%sxx = WC5_real_fill
              end if

              if (aset%is_net_model) bset%wave%check = RESHAPE(PACK(aset%wave%check, flags2d .eq. 1), shape_2d)

              if (aset%is_2d_model) then
                allocate (flags3d(aset%wave%dir_count, aset%wave%freq_count, aset%wave%time_count))
                flags3d = SPREAD(flags2d, 1, aset%wave%dir_count)
                shape_3d = (/ bset%wave%dir_count, bset%wave%freq_count, bset%wave%time_count /)
                bset%wave%dirspec = RESHAPE(PACK(aset%wave%dirspec, flags3d .eq. 1), shape_3d)
                deallocate(flags3d)
              end if
              deallocate(flags2d)

              call wc5_recalculate_params(bset, .true.)
            else
              errcode = 2
            end if

          else
            errcode = 1
          end if
        end subroutine


c-- WC5_COMBINE_INDICES --------------------------------------------------------
c   Calculates the array indices for combining sets, helper to WC5_COMBINE_SETS
c-------------------------------------------------------------------------------
        subroutine wc5_combine_indices(amax, bmax, aindices, bindices, atimes, btimes, ccount)
          integer     amax, aindices(*), bmax, bindices(*), ccount, atimes(*), btimes(*)
          integer     aidx, bidx

          if (amax .le. 0 .and. bmax .le. 0) return

          aidx = 1
          bidx = 1
          ccount = 0
          do while (aidx .le. amax .or. bidx .le. bmax)
            ccount = ccount + 1
            if (aidx .gt. amax) then
              bindices(bidx) = ccount
              bidx = bidx + 1
            else if (bidx .gt. bmax) then
              aindices(aidx) = ccount
              aidx = aidx + 1
            else if (btimes(bidx) .lt. atimes(aidx)) then
              bindices(bidx) = ccount
              bidx = bidx + 1
            else if (atimes(aidx) .lt. btimes(bidx)) then
              aindices(aidx) = ccount
              aidx = aidx + 1
            else 
              aindices(aidx) = ccount
              bindices(bidx) = ccount
              aidx = aidx + 1
              bidx = bidx + 1
            end if
          end do
        end subroutine


c-- WC5_COMBINE_INDICES_XYZ ----------------------------------------------------
c   Calculates the array indices for xyz sets, helper to WC5_COMBINE_SETS.
c-------------------------------------------------------------------------------
        subroutine wc5_combine_indices_xyz(amax, bmax, aindices, bindices, astart, bstart, srate, ccount)
          real        srate
          integer     amax, aindices(*), bmax, bindices(*), ccount, i
          integer     aoffset, astart, boffset, bstart, start_time

          if (amax .le. 0 .and. bmax .le. 0) return

          start_time = MIN(astart, bstart)
          aoffset = NINT((astart-start_time)*srate)
          do i = 1, amax
            aindices(i) = aoffset + i
          end do
          boffset = NINT((bstart-start_time)*srate)
          do i = 1, bmax
            bindices(i) = boffset + i
          end do
          if (amax .eq. 0) then
            ccount = bindices(bmax)
          else if (bmax .eq. 0) then
            ccount = aindices(amax)
          else
            ccount = MAX(aindices(amax), bindices(bmax))
          end if
        end subroutine


c-- WC5_COMBINE_WAVES ----------------------------------------------------------
c   Used to fill in the wave group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_waves(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          if (iset%is_dwr) oset%is_dwr = .true.
          if (iset%is_mk4) oset%is_mk4 = .true.

          do i = 1, iset%wave%time_count
            oset%wave%times(indices(i)) = iset%wave%times(i)
            if (oset%wave%flags(indices(i)) .eq. WC5_byte_fill .or. 
     *           iset%wave%flags(i) .le. oset%wave%flags(indices(i))) then
              oset%wave%flags(indices(i)) = iset%wave%flags(i)
              oset%wave%flags2(indices(i)) = iset%wave%flags2(i)
              oset%wave%pub_tf(indices(i)) = iset%wave%pub_tf(i)
            end if

            if (iset%is_dwr .or. iset%is_mk4 .or. MAXVAL(iset%wave%src_index) .gt. 0) 
     *        oset%wave%src_index(indices(i)) = iset%wave%src_index(i) + offset
            if (iset%is_net_model) oset%wave%model_input(:,indices(i)) = iset%wave%model_input(:,i)

            if (iset%wave%hs(i) .ne. WC5_real_fill) oset%wave%hs(indices(i)) = iset%wave%hs(i)
            if (iset%wave%tp(i) .ne. WC5_real_fill) oset%wave%tp(indices(i)) = iset%wave%tp(i)
            if (iset%wave%ta(i) .ne. WC5_real_fill) oset%wave%ta(indices(i)) = iset%wave%ta(i)
            if (iset%is_directional .and. iset%wave%dp(i) .ne. WC5_real_fill) 
     *        oset%wave%dp(indices(i)) = iset%wave%dp(i)

            if (iset%is_dwr .or. iset%is_mk4) then
              if (iset%wave%psdmax(i) .ne. WC5_real_fill) oset%wave%psdmax(indices(i)) = iset%wave%psdmax(i)
              if (iset%wave%tz(i) .ne. WC5_real_fill) oset%wave%tz(indices(i)) = iset%wave%tz(i)
            end if
            if (iset%is_mk4) then
              if (iset%wave%spreadmax(i) .ne. WC5_real_fill) oset%wave%spreadmax(indices(i)) = iset%wave%spreadmax(i)
              if (iset%wave%tint(i) .ne. WC5_real_fill) oset%wave%tint(indices(i)) = iset%wave%tint(i)
              if (iset%wave%tener(i) .ne. WC5_real_fill) oset%wave%tener(indices(i)) = iset%wave%tener(i)
              if (iset%wave%tm13(i) .ne. WC5_real_fill) oset%wave%tm13(indices(i)) = iset%wave%tm13(i)
              if (iset%wave%tcrest(i) .ne. WC5_real_fill) oset%wave%tcrest(indices(i)) = iset%wave%tcrest(i)
              if (iset%wave%iqp(i) .ne. WC5_real_fill) oset%wave%iqp(indices(i)) = iset%wave%iqp(i)
            end if

            if (iset%is_nearshore) then
              if (iset%wave%dm(i) .ne. WC5_real_fill) oset%wave%dm(indices(i)) = iset%wave%dm(i)
              if (iset%wave%sxy(i) .ne. WC5_real_fill) oset%wave%sxy(indices(i)) = iset%wave%sxy(i)
              if (iset%wave%sxx(i) .ne. WC5_real_fill) oset%wave%sxx(indices(i)) = iset%wave%sxx(i)
            end if

            if (iset%wave%freq_count .gt. 0) then
              if (MAXVAL(iset%wave%a0(:,i)) .ne. WC5_real_fill) oset%wave%a0(:,indices(i)) = iset%wave%a0(:,i)
              if (iset%is_directional) then
                if (MAXVAL(iset%wave%a1(:,i)) .ne. WC5_real_fill) oset%wave%a1(:,indices(i)) = iset%wave%a1(:,i)
                if (MAXVAL(iset%wave%a2(:,i)) .ne. WC5_real_fill) oset%wave%a2(:,indices(i)) = iset%wave%a2(:,i)
                if (MAXVAL(iset%wave%b1(:,i)) .ne. WC5_real_fill) oset%wave%b1(:,indices(i)) = iset%wave%b1(:,i)
                if (MAXVAL(iset%wave%b2(:,i)) .ne. WC5_real_fill) oset%wave%b2(:,indices(i)) = iset%wave%b2(:,i)
                if (MAXVAL(iset%wave%mdir(:,i)) .ne. WC5_real_fill) oset%wave%mdir(:,indices(i)) = iset%wave%mdir(:,i)
              end if

              if (iset%is_dwr .or. iset%is_mk4) then
                if (MAXVAL(iset%wave%check(:,i)) .ne. WC5_real_fill) oset%wave%check(:,indices(i)) = iset%wave%check(:,i)
                if (MAXVAL(iset%wave%dspread(:,i)) .ne. WC5_real_fill) oset%wave%dspread(:,indices(i)) = iset%wave%dspread(:,i)
                if (MAXVAL(iset%wave%m2(:,i)) .ne. WC5_real_fill) oset%wave%m2(:,indices(i)) = iset%wave%m2(:,i)
                if (MAXVAL(iset%wave%n2(:,i)) .ne. WC5_real_fill) oset%wave%n2(:,indices(i)) = iset%wave%n2(:,i)
              end if

              if (iset%is_net_model) oset%wave%check(:,indices(i)) = iset%wave%check(:,i)
              if (iset%is_2d_model) oset%wave%dirspec(:,:,indices(i)) = iset%wave%dirspec(:,:,i)
            end if

          end do
        end subroutine


c-- WC5_COMBINE_XYZ ------------------------------------------------------------
c   Used to fill in the xyz group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_xyz(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%xyz%rec_count
            if (indices(i) .ge. 1 .and. indices(i) .le. oset%xyz%rec_count) then
              oset%xyz%flags(indices(i)) = iset%xyz%flags(i)
              oset%xyz%flags2(indices(i)) = iset%xyz%flags2(i)
              oset%xyz%src_index(indices(i)) = iset%xyz%src_index(i) + offset
              oset%xyz%pub_tf(indices(i)) = iset%xyz%pub_tf(i)
              oset%xyz%xdisp(indices(i)) = iset%xyz%xdisp(i)
              oset%xyz%ydisp(indices(i)) = iset%xyz%ydisp(i)
              oset%xyz%zdisp(indices(i)) = iset%xyz%zdisp(i)
            end if
          end do
        end subroutine


c-- WC5_COMBINE_SST ------------------------------------------------------------
c   Used to fill in the sst group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_sst(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%sst%time_count
            oset%sst%times(indices(i)) = iset%sst%times(i)
            oset%sst%flags(indices(i)) = iset%sst%flags(i)
            oset%sst%flags2(indices(i)) = iset%sst%flags2(i)
            oset%sst%pub_tf(indices(i)) = iset%sst%pub_tf(i)
            oset%sst%src_index(indices(i)) = iset%sst%src_index(i) + offset
            oset%sst%sstC(indices(i)) = iset%sst%sstC(i)
            oset%sst%reftemp(indices(i)) = iset%sst%reftemp(i)
          end do
        end subroutine


c-- WC5_COMBINE_GPS ------------------------------------------------------------
c   Used to fill in the gps group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_gps(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%gps%time_count
            oset%gps%times(indices(i)) = iset%gps%times(i)
            oset%gps%flags(indices(i)) = iset%gps%flags(i)
            oset%gps%merit(indices(i)) = iset%gps%merit(i)
            oset%gps%src_index(indices(i)) = iset%gps%src_index(i) + offset
            oset%gps%latitude(indices(i)) = iset%gps%latitude(i)
            oset%gps%longitude(indices(i)) = iset%gps%longitude(i)
            oset%gps%new_fix(indices(i)) = iset%gps%new_fix(i)
            oset%gps%mod_ok(indices(i)) = iset%gps%mod_ok(i)
            oset%gps%hf_errors(indices(i)) = iset%gps%hf_errors(i)
          end do
        end subroutine


c-- WC5_COMBINE_ACM ------------------------------------------------------------
c   Used to fill in the acm group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_acm(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%acm%time_count
            oset%acm%times(indices(i)) = iset%acm%times(i)
            oset%acm%flags(indices(i)) = iset%acm%flags(i)
            oset%acm%flags2(indices(i)) = iset%acm%flags2(i)
            oset%acm%pub_tf(indices(i)) = iset%acm%pub_tf(i)
            oset%acm%src_index(indices(i)) = iset%acm%src_index(i) + offset
            oset%acm%cstatus(indices(i)) = iset%acm%cstatus(i)
            oset%acm%speed(indices(i)) = iset%acm%speed(i)
            oset%acm%dir(indices(i)) = iset%acm%dir(i)
            oset%acm%speedstd(indices(i)) = iset%acm%speedstd(i)
            oset%acm%dirstd(indices(i)) = iset%acm%dirstd(i)
            oset%acm%rssi1(indices(i)) = iset%acm%rssi1(i)
            oset%acm%rssi2(indices(i)) = iset%acm%rssi2(i)
            oset%acm%rssi3(indices(i)) = iset%acm%rssi3(i)
            oset%acm%csst(indices(i)) = iset%acm%csst(i)
            oset%acm%vert(indices(i)) = iset%acm%vert(i)
            oset%acm%vertstd(indices(i)) = iset%acm%vertstd(i)
          end do
        end subroutine


c-- WC5_COMBINE_DWR ------------------------------------------------------------
c   Used to fill in the dwr group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_dwr(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%dwr%time_count
            oset%dwr%times(indices(i)) = iset%dwr%times(i)
            oset%dwr%src_index(indices(i)) = iset%dwr%src_index(i) + offset
            oset%dwr%wol(indices(i)) = iset%dwr%wol(i)
            oset%dwr%batt(indices(i)) = iset%dwr%batt(i)
            oset%dwr%za_off(indices(i)) = iset%dwr%za_off(i)
            oset%dwr%xa_off(indices(i)) = iset%dwr%xa_off(i)
            oset%dwr%ya_off(indices(i)) = iset%dwr%ya_off(i)
            oset%dwr%orient(indices(i)) = iset%dwr%orient(i)
            oset%dwr%inclin(indices(i)) = iset%dwr%inclin(i)
          end do
        end subroutine


c-- WC5_COMBINE_DWR4 --- ---------------------------------------------------------
c   Used to fill in the dwr4 group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_dwr4(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%dwr4%time_count
            oset%dwr4%times(indices(i)) = iset%dwr4%times(i)
            oset%dwr4%src_index(indices(i)) = iset%dwr4%src_index(i) + offset
            oset%dwr4%uptime(indices(i)) = iset%dwr4%uptime(i)
            oset%dwr4%wol(indices(i)) = iset%dwr4%wol(i)
            oset%dwr4%enerused(indices(i)) = iset%dwr4%enerused(i)
            oset%dwr4%eboost(indices(i)) = iset%dwr4%eboost(i)
            oset%dwr4%za_max(indices(i)) = iset%dwr4%za_max(i)
            oset%dwr4%xa_max(indices(i)) = iset%dwr4%xa_max(i)
            oset%dwr4%ya_max(indices(i)) = iset%dwr4%ya_max(i)
            oset%dwr4%pitch_max(indices(i)) = iset%dwr4%pitch_max(i)
            oset%dwr4%roll_max(indices(i)) = iset%dwr4%roll_max(i)
            oset%dwr4%hatchtemp(indices(i)) = iset%dwr4%hatchtemp(i)
            oset%dwr4%voltage(indices(i)) = iset%dwr4%voltage(i)
            oset%dwr4%za_off(indices(i)) = iset%dwr4%za_off(i)
            oset%dwr4%xa_off(indices(i)) = iset%dwr4%xa_off(i)
            oset%dwr4%ya_off(indices(i)) = iset%dwr4%ya_off(i)
            oset%dwr4%orient_mean(indices(i)) = iset%dwr4%orient_mean(i)
            oset%dwr4%orient_dev(indices(i)) = iset%dwr4%orient_dev(i)
            oset%dwr4%inclin_mean(indices(i)) = iset%dwr4%inclin_mean(i)
            oset%dwr4%inclin_dev(indices(i)) = iset%dwr4%inclin_dev(i)
            oset%dwr4%maglength_mean(indices(i)) = iset%dwr4%maglength_mean(i)
            oset%dwr4%maglength_dev(indices(i)) = iset%dwr4%maglength_dev(i)
            oset%dwr4%sensortemp(indices(i)) = iset%dwr4%sensortemp(i)
          end do
        end subroutine


c-- WC5_COMBINE_UPCROSS --------------------------------------------------------
c   Used to fill in the upcross group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_upcross(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%upcross%time_count
            oset%upcross%times(indices(i)) = iset%upcross%times(i)
            oset%upcross%flags(indices(i)) = iset%upcross%flags(i)
            oset%upcross%flags2(indices(i)) = iset%upcross%flags2(i)
            oset%upcross%pub_tf(indices(i)) = iset%upcross%pub_tf(i)
            oset%upcross%src_index(indices(i)) = iset%upcross%src_index(i) + offset
            oset%upcross%num_crests(indices(i)) = iset%upcross%num_crests(i)
            oset%upcross%num_waves(indices(i)) = iset%upcross%num_waves(i)
            oset%upcross%Havg(indices(i)) = iset%upcross%Havg(i)
            oset%upcross%Hmax(indices(i)) = iset%upcross%Hmax(i)
            oset%upcross%Hrms(indices(i)) = iset%upcross%Hrms(i)
            oset%upcross%H_at_Tmax(indices(i)) = iset%upcross%H_at_Tmax(i)
            oset%upcross%T_at_Hmax(indices(i)) = iset%upcross%T_at_Hmax(i)
            oset%upcross%Tavg(indices(i)) = iset%upcross%Tavg(i)
            oset%upcross%Tmax(indices(i)) = iset%upcross%Tmax(i)
            oset%upcross%bandwidth(indices(i)) = iset%upcross%bandwidth(i)
            oset%upcross%coverage(indices(i)) = iset%upcross%coverage(i)

            oset%upcross%H10(indices(i)) = iset%upcross%H10(i)
            oset%upcross%H3(indices(i)) = iset%upcross%H3(i)
            oset%upcross%TofH10(indices(i)) = iset%upcross%TofH10(i)
            oset%upcross%TofH3(indices(i)) = iset%upcross%TofH3(i)
            oset%upcross%H_quantile(:,indices(i)) = iset%upcross%H_quantile(:,i)

c           oset%upcross%T10(indices(i)) = iset%upcross%T10(i)
c           oset%upcross%T3(indices(i)) = iset%upcross%T3(i)
c           oset%upcross%HofT10(indices(i)) = iset%upcross%HofT10(i)
c           oset%upcross%HofT3(indices(i)) = iset%upcross%HofT3(i)
c           oset%upcross%T_quantile(:,indices(i)) = iset%upcross%T_quantile(:,i)
          end do
        end subroutine


c-- WC5_COMBINE_SYNC -----------------------------------------------------------
c   Used to fill in the sync group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_sync(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%sync%time_count
            oset%sync%times(indices(i)) = iset%sync%times(i)
            oset%sync%src_index(indices(i)) = iset%sync%src_index(i) + offset
            oset%sync%seg_count(indices(i)) = iset%sync%seg_count(i)
            oset%sync%segs_used(indices(i)) = iset%sync%segs_used(i)
            oset%sync%samples(indices(i)) = iset%sync%samples(i)
            oset%sync%disp_hex(:,indices(i)) = iset%sync%disp_hex(:,i)
          end do
        end subroutine


c-- WC5_COMBINE_CAT4 -----------------------------------------------------------
c   Used to fill in the cat4 group in WC5_COMBINE_SETS 
c-------------------------------------------------------------------------------
        subroutine wc5_combine_cat4(iset, oset, indices, offset)
          integer            i, indices(*), offset
          type(wc5_dataset)  iset, oset

          do i = 1, iset%cat4%time_count
            oset%cat4%times(indices(i)) = iset%cat4%times(i)
            oset%cat4%flags(indices(i)) = iset%cat4%flags(i)
            oset%cat4%flags2(indices(i)) = iset%cat4%flags2(i)
            oset%cat4%pub_tf(indices(i)) = iset%cat4%pub_tf(i)
            oset%cat4%src_index(indices(i)) = iset%cat4%src_index(i) + offset
            oset%cat4%airt(indices(i)) = iset%cat4%airt(i)
            oset%cat4%status(indices(i)) = iset%cat4%status(i)
            oset%cat4%white(indices(i)) = iset%cat4%white(i)
            oset%cat4%black(indices(i)) = iset%cat4%black(i)
            oset%cat4%metal(indices(i)) = iset%cat4%metal(i)
            oset%cat4%grooved(indices(i)) = iset%cat4%grooved(i)
          end do
        end subroutine



c-- WC5_QUICK_LOAD -------------------------------------------------------------
c   Loads the ids, dimension sizes, and coordinate variables from a wc5 file
c-------------------------------------------------------------------------------
        subroutine wc5_quick_load(wc_name, wset, included_groups, errcode, 
     *               allow_write)
          integer              errcode, included_groups, write_status
          character*100        wc_name
          logical              classic, is_xy_only
          logical, optional::  allow_write
          type(wc5_dataset)::  wset

          write_status = NF90_NOWRITE
          if (PRESENT(allow_write)) then
            if (allow_write) write_status = NF90_WRITE
          end if

          classic = wset%classic
          is_xy_only = wset%is_xy_only
          call wc5_initialize_set(wset)
          wset%classic = classic
          wset%is_xy_only = is_xy_only

          call nc_call_func(nf90_open(wc_name, write_status, wset%ncid))
          call wc5_read_ids(wset, errcode)
          wset%groups = wc5_get_group_value(wset)
          call wc5_read_dimensions(wset, errcode)
          call wc5_allocate_set(wset, .false.)
          call wc5_load_coord_vars(wset, included_groups, errcode)

        end subroutine


c-- WC5_LOAD_FULL_SET ---------------------------------------------------------
c   Load an entire wavecdf dataset.
c-------------------------------------------------------------------------------
        subroutine wc5_load_full_set(wc5_name, dset, ecode)
          integer                 ecode
          character*100           wc5_name
          type(wc5_dataset), intent(out)::   dset

          call wc5_load_set(wc5_name, 4095, dset, errcode=ecode)
        end subroutine


c-- WC5_LOAD_FULL_PARAMS -------------------------------------------------------
c   Load all parameters but no spectra from a wavecdf dataset.
c-------------------------------------------------------------------------------
        subroutine wc5_load_full_params(wc5_name, wset, errcode)
          integer                 errcode
          character*100           wc5_name
          type(time_span)         timespan
          type(wc5_dataset), intent(out)::   wset

          timespan%start = init_date(1950,1,1,0,0,0)
          timespan%end = init_date(2020,1,1,0,0,0)

          call wc5_load_set(wc5_name, 1, wset, timespan, -99.9, 100.0, errcode)
        end subroutine


c-- WC5_LOAD_PARAMS ------------------------------------------------------------
c   Load parameters but no spectra for the timespan from a wavecdf dataset.
c-------------------------------------------------------------------------------
        subroutine wc5_load_params(wc5_name, wset, timespan, errcode)
          integer                 errcode
          character*100           wc5_name
          type(time_span)         timespan
          type(wc5_dataset), intent(out)::   wset

          call wc5_load_set(wc5_name, 1, wset, timespan, -99.9, 100.0, errcode)
        end subroutine


c-- WC5_READ_DIMENSIONS --------------------------------------------------------
c   Reads the dimensions for the different groups
c-------------------------------------------------------------------------------
        subroutine wc5_read_dimensions(wset, errcode)
          integer                 errcode, i
          character*100           dim_name
          type(wc5_dataset)       wset

          if (wset%source%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%source%grpid, 
     *        wset%source%count_dimid, dim_name, wset%source%file_count))
            call nc_call_func(nf90_inquire_dimension(wset%source%grpid, 
     *        wset%source%flength_dimid, dim_name, wset%source%file_name_length))
          end if

          if (wset%wave%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%wave%grpid, 
     *        wset%wave%time_dimid, dim_name, wset%wave%time_count))
            call nc_call_func(nf90_inquire_dimension(wset%wave%grpid, 
     *        wset%wave%freq_dimid, dim_name, wset%wave%freq_count))
            if (wset%wave%dir_dimid .gt. 0) call nc_call_func(nf90_inquire_dimension(wset%wave%grpid, 
     *        wset%wave%dir_dimid, dim_name, wset%wave%dir_count))
          end if

          if (wset%xyz%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%xyz%grpid, 
     *        wset%xyz%count_dimid, dim_name, wset%xyz%rec_count))
          end if

          if (wset%sst%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%sst%grpid, 
     *        wset%sst%time_dimid, dim_name, wset%sst%time_count))
          end if

          if (wset%gps%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%gps%grpid, 
     *        wset%gps%time_dimid, dim_name, wset%gps%time_count))
          end if

          if (wset%acm%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%acm%grpid, 
     *        wset%acm%time_dimid, dim_name, wset%acm%time_count))
          end if

          if (wset%dwr%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%dwr%grpid, 
     *        wset%dwr%time_dimid, dim_name, wset%dwr%time_count))
          end if

          if (wset%dwr4%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%dwr4%grpid, 
     *        wset%dwr4%time_dimid, dim_name, wset%dwr4%time_count))
          end if

          if (wset%upcross%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%upcross%grpid, 
     *        wset%upcross%time_dimid, dim_name, wset%upcross%time_count))
          end if

          if (wset%sync%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%sync%grpid, 
     *        wset%sync%time_dimid, dim_name, wset%sync%time_count))
          end if

          if (wset%cat4%grpid .ne. -1) then
            call nc_call_func(nf90_inquire_dimension(wset%cat4%grpid, 
     *        wset%cat4%time_dimid, dim_name, wset%cat4%time_count))
          end if

        end subroutine


c-- WC5_LIMIT_DIMENSIONS --------------------------------------------------------
c   Eliminates unneeded groups in a set by setting their sizes to zero. For
c   use before allocating a set; helper for WC5_LOAD_SET.
c-------------------------------------------------------------------------------
        subroutine wc5_limit_dimensions(wset, load_type)
          integer                 load_type
          type(wc5_dataset)       wset

          if (wset%source%file_count .gt. 0 .and. BTEST(load_type, WC5_source_bit) .eqv. .false.)
     *      wset%source%file_count = 0

          if (wset%wave%time_count .gt. 0 .and. BTEST(load_type, WC5_wave_bit) .eqv. .false.) wset%wave%time_count = 0
          if (wset%wave%freq_count .gt. 0 .and. BTEST(load_type, WC5_spectra_bit) .eqv. .false.) 
     *      wset%wave%freq_count = 0
          if (wset%wave%dir_count .gt. 0 .and. BTEST(load_type, WC5_spectra_bit) .eqv. .false.) wset%wave%dir_count = 0
          if (wset%xyz%rec_count .gt. 0 .and. BTEST(load_type, WC5_xyz_bit) .eqv. .false.) wset%xyz%rec_count = 0

          if (wset%sst%time_count .gt. 0 .and. BTEST(load_type, WC5_sst_bit) .eqv. .false.) wset%sst%time_count = 0
          if (wset%gps%time_count .gt. 0 .and. BTEST(load_type, WC5_gps_bit) .eqv. .false.) wset%gps%time_count = 0
          if (wset%acm%time_count .gt. 0 .and. BTEST(load_type, WC5_acm_bit) .eqv. .false.) wset%acm%time_count = 0
          if (wset%dwr%time_count .gt. 0 .and. BTEST(load_type, WC5_dwr_bit) .eqv. .false.) wset%dwr%time_count = 0
          if (wset%dwr4%time_count .gt. 0 .and. BTEST(load_type, WC5_dwr4_bit) .eqv. .false.) wset%dwr4%time_count = 0
          if (wset%upcross%time_count .gt. 0 .and. BTEST(load_type, WC5_upcross_bit) .eqv. .false.) 
     *      wset%upcross%time_count = 0
          if (wset%sync%time_count .gt. 0 .and. BTEST(load_type, WC5_sync_bit) .eqv. .false.) wset%sync%time_count = 0
          if (wset%cat4%time_count .gt. 0 .and. BTEST(load_type, WC5_cat4_bit) .eqv. .false.) wset%cat4%time_count = 0

        end subroutine


c-- WC5_READ_IDS ---------------------------------------------------------------
c   Reads the dimension ids, group ids, and variable ids for a set
c-------------------------------------------------------------------------------
        subroutine wc5_read_ids(wset, errcode, set_flags)
          integer                 errcode, i
          character*4             sp_group_name
          logical                 add_flags
          logical,optional::      set_flags
          type(wc5_dataset)       wset

          if (PRESENT(set_flags)) then
            add_flags = set_flags
          else
            add_flags = .true.
          end if

          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'source', wset%source%grpid)
          else
            wset%source%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%source%grpid, 'sourceCount', wset%source%count_dimid)
          end if
c         if (errcode .eq. NF90_NOERR) then
          if (.false.) then
            call nc_call_func(nf90_inq_dimid(wset%source%grpid, 'sourceCount', wset%source%count_dimid))
            call nc_call_func(nf90_inq_dimid(wset%source%grpid, 'sourceNameLength', wset%source%flength_dimid))
            call nc_call_func(nf90_inq_varid(wset%source%grpid, 'sourceFilename', wset%source%name_varid))
            if (add_flags) then
              wset%is_net_model = .false.
              wset%is_2d_model = .false.
            end if
          else
            wset%source%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'wave', wset%wave%grpid)
          else
            wset%wave%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%wave%grpid, 'waveTime', wset%wave%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%wave%grpid, 'waveTime', wset%wave%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveTime', wset%wave%time_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveTimeBounds', wset%wave%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFlagPrimary', wset%wave%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFlagSecondary', wset%wave%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFrequencyBounds', wset%wave%fbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFrequencyFlagPrimary', wset%wave%fflags_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFrequencyFlagSecondary', wset%wave%fflags2_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveHs', wset%wave%hs_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveTp', wset%wave%tp_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveTa', wset%wave%ta_varid))
            errcode = nf90_inq_varid(wset%wave%grpid, 'waveDp', wset%wave%dp_varid)
            if (errcode .eq. NF90_NOERR) then
              if (add_flags) wset%is_directional = .true.
            else
              wset%wave%dp_varid = -1
              if (add_flags) wset%is_directional = .false.
            end if

            errcode = nf90_inq_varid(wset%wave%grpid, 'waveModelInputSource', wset%wave%modelinput_varid)
            if (errcode .eq. NF90_NOERR) then
              if (add_flags) wset%is_net_model = .true.
            else 
              if (add_flags) wset%is_net_model = .false.
              wset%wave%modelinput_varid = -1
              errcode = nf90_inq_varid(wset%wave%grpid, 'waveSourceIndex', wset%wave%src_varid)
              if (errcode .ne. NF90_NOERR) wset%wave%src_varid = -1
            end if

            wset%wave%psdmax_varid = -1
            wset%wave%sprdmax_varid = -1
            wset%wave%tz_varid = -1
            wset%wave%tint_varid = -1
            wset%wave%tener_varid = -1
            wset%wave%tm13_varid = -1
            wset%wave%tcrest_varid = -1
            wset%wave%iqp_varid = -1

            errcode = nf90_inq_varid(wset%wave%grpid, 'wavePeakPSD', wset%wave%psdmax_varid)
            if (errcode .eq. NF90_NOERR) then
              errcode = nf90_inq_varid(wset%wave%grpid, 'waveTz', wset%wave%tz_varid)
              errcode = nf90_inq_varid(wset%wave%grpid, 'wavePeakSpread', wset%wave%sprdmax_varid)
              if (errcode .eq. NF90_NOERR) then
                if (add_flags) wset%is_dwr = .false.
                if (add_flags) wset%is_mk4 = .true.
                errcode = nf90_inq_varid(wset%wave%grpid, 'waveTi', wset%wave%tint_varid)
                errcode = nf90_inq_varid(wset%wave%grpid, 'waveTe', wset%wave%tener_varid)
                errcode = nf90_inq_varid(wset%wave%grpid, 'waveTm13', wset%wave%tm13_varid)
                errcode = nf90_inq_varid(wset%wave%grpid, 'waveTc', wset%wave%tcrest_varid)
                errcode = nf90_inq_varid(wset%wave%grpid, 'waveInverseQp', wset%wave%iqp_varid)
              else
                if (add_flags) wset%is_dwr = .true.
                if (add_flags) wset%is_mk4 = .false.
              end if
            else
              if (add_flags) wset%is_dwr = .false.
              if (add_flags) wset%is_mk4 = .false.
            end if

            errcode = nf90_inq_varid(wset%wave%grpid, 'waveDm', wset%wave%dm_varid)
            if (errcode .eq. NF90_NOERR) then
              if (add_flags) wset%is_nearshore = .true.
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveSxy', wset%wave%sxy_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveSxx', wset%wave%sxx_varid))
            else
              if (add_flags) wset%is_nearshore = .false.
              wset%wave%dm_varid = -1
              wset%wave%sxx_varid = -1
              wset%wave%sxy_varid = -1
            end if

c           call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'spectrumIndex', wset%wave%sptype_varid))

            call nc_call_func(nf90_inq_dimid(wset%wave%grpid, 'waveFrequency', wset%wave%freq_dimid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveFrequency', wset%wave%freq_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveBandwidth', wset%wave%bw_varid))
            call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveEnergyDensity', wset%wave%a0_varid))
            if (wset%is_directional) then
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveMeanDirection', wset%wave%mdir_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveA1Value', wset%wave%a1_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveB1Value', wset%wave%b1_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveA2Value', wset%wave%a2_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveB2Value', wset%wave%b2_varid))
            end if
            if (wset%is_dwr .or. wset%is_mk4) then
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveCheckFactor', wset%wave%check_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveSpread', wset%wave%spread_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveM2Value', wset%wave%m2_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveN2Value', wset%wave%n2_varid))
            else if (wset%is_net_model) then
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveModelBinInputCoverage', wset%wave%check_varid))
            end if

            errcode = nf90_inq_dimid(wset%wave%grpid, 'waveDirection', wset%wave%dir_dimid)
            if (errcode .eq. NF90_NOERR) then
              wset%is_2d_model = .true.
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveDirection', wset%wave%dir_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveDirectionBounds', wset%wave%dbounds_varid))
              call nc_call_func(nf90_inq_varid(wset%wave%grpid, 'waveDirectionalSpectrum', wset%wave%dirspec_varid))
            else
              wset%is_2d_model = .false.
            end if
          else
            wset%wave%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'xyz', wset%xyz%grpid)
          else
            wset%xyz%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%xyz%grpid, 'xyzCount', wset%xyz%count_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%xyz%grpid, 'xyzCount', wset%xyz%count_dimid))

            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzStartTime', wset%xyz%stime_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzSampleRate', wset%xyz%srate_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzFilterDelay', wset%xyz%delay_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzFlagPrimary', wset%xyz%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzFlagSecondary', wset%xyz%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzZDisplacement', wset%xyz%zdisp_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzXDisplacement', wset%xyz%xdisp_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzYDisplacement', wset%xyz%ydisp_varid))
            call nc_call_func(nf90_inq_varid(wset%xyz%grpid, 'xyzSourceIndex', wset%xyz%src_varid))
          else
            wset%xyz%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'sst', wset%sst%grpid)
          else
            wset%sst%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%sst%grpid, 'sstTime', wset%sst%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%sst%grpid, 'sstTime', wset%sst%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstTime', wset%sst%time_varid))
            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstTimeBounds', wset%sst%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstSeaSurfaceTemperature', wset%sst%sstC_varid))
            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstFlagPrimary', wset%sst%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstFlagSecondary', wset%sst%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstSourceIndex', wset%sst%src_varid))
            if (wset%is_dwr)
     *        call nc_call_func(nf90_inq_varid(wset%sst%grpid, 'sstReferenceTemp', wset%sst%reftemp_varid))
          else
            wset%sst%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'gps', wset%gps%grpid)
          else
            wset%gps%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%gps%grpid, 'gpsTime', wset%gps%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%gps%grpid, 'gpsTime', wset%gps%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%gps%grpid, 'gpsTime', wset%gps%time_varid))
            call nc_call_func(nf90_inq_varid(wset%gps%grpid, 'gpsTimeBounds', wset%gps%tbounds_varid))
            errcode = nf90_inq_varid(wset%gps%grpid, 'gpsStatusFlags', wset%gps%flags_varid)
            call nc_call_func(nf90_inq_varid(wset%gps%grpid, 'gpsLatitude', wset%gps%latitude_varid))
            call nc_call_func(nf90_inq_varid(wset%gps%grpid, 'gpsLongitude', wset%gps%longitude_varid))
            call nc_call_func(nf90_inq_varid(wset%gps%grpid, 'gpsSourceIndex', wset%gps%src_varid))
          else
            wset%gps%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'acm', wset%acm%grpid)
          else
            wset%acm%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%acm%grpid, 'acmTime', wset%acm%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%acm%grpid, 'acmTime', wset%acm%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmTime', wset%acm%time_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmTimeBounds', wset%acm%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmFlagPrimary', wset%acm%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmFlagSecondary', wset%acm%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSpeed', wset%acm%speed_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSpeedStdDev', wset%acm%speedstd_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmDirection', wset%acm%dir_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmDirectionStdDev', wset%acm%dirstd_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSignalStrength1', wset%acm%rssi1_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSignalStrength2', wset%acm%rssi2_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSignalStrength3', wset%acm%rssi3_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmStatus', wset%acm%status_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSeaSurfaceTemperature', wset%acm%csst_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmVerticalSpeed', wset%acm%vert_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmVerticalSpeedStdDev', wset%acm%vertstd_varid))
            call nc_call_func(nf90_inq_varid(wset%acm%grpid, 'acmSourceIndex', wset%acm%src_varid))
          else
            wset%acm%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'dwr', wset%dwr%grpid)
          else
            wset%dwr%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%dwr%grpid, 'dwrTime', wset%dwr%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%dwr%grpid, 'dwrTime', wset%dwr%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrTime', wset%dwr%time_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrTimeBounds', wset%dwr%tbounds_varid))
            errcode = nf90_inq_varid(wset%dwr%grpid, 'dwrBatteryWeeksOfLife', wset%dwr%wol_varid)
            if (errcode .eq. NF90_NOERR) then
              wset%is_mk3 = .true.
            else
              wset%is_mk3 = .false.
              wset%dwr%wol_varid = -1
            end if
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrBatteryLevel', wset%dwr%batt_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrZAccelerometerOffset', wset%dwr%za_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrXAccelerometerOffset', wset%dwr%xa_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrYAccelerometerOffset', wset%dwr%ya_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrOrientation', wset%dwr%orient_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrInclination', wset%dwr%inclin_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr%grpid, 'dwrSourceIndex', wset%dwr%src_varid))
          else
            wset%dwr%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'dwr4', wset%dwr4%grpid)
          else
            wset%dwr4%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%dwr4%grpid, 'dwr4Time', wset%dwr4%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%dwr4%grpid, 'dwr4Time', wset%dwr4%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4Time', wset%dwr4%time_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4TimeBounds', wset%dwr4%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4Uptime', wset%dwr4%uptime_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4BatteryWeeksOfLife', wset%dwr4%wol_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4EnergyUsed', wset%dwr4%enerused_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4EnergyToBoostcaps', wset%dwr4%eboost_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4HatchTemperature', wset%dwr4%hatchtemp_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4BatteryVoltage', wset%dwr4%voltage_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4ZAccelerometerOffset', wset%dwr4%za_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4ZAccelMaxCount', wset%dwr4%za_max_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4XAccelerometerOffset', wset%dwr4%xa_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4XAccelMaxCount', wset%dwr4%xa_max_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4YAccelerometerOffset', wset%dwr4%ya_off_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4YAccelMaxCount', wset%dwr4%ya_max_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4OrientMean', wset%dwr4%orient_mean_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4OrientStdDev', wset%dwr4%orient_dev_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4InclinMean', wset%dwr4%inclin_mean_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4InclinStdDev', wset%dwr4%inclin_dev_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4MagFieldLengthMean', wset%dwr4%maglength_mean_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4MagFieldLengthStdDev',wset%dwr4%maglength_dev_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4PitchMaxCount', wset%dwr4%pitch_max_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4RollMaxCount', wset%dwr4%roll_max_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4SensorTemperature', wset%dwr4%sensortemp_varid))
            call nc_call_func(nf90_inq_varid(wset%dwr4%grpid, 'dwr4SourceIndex', wset%dwr4%src_varid))
          else
            wset%dwr4%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'upcross', wset%upcross%grpid)
          else
            wset%upcross%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%upcross%grpid, 'upcrossTime', wset%upcross%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%upcross%grpid, 'upcrossTime', wset%upcross%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTime', wset%upcross%time_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTimeBounds', wset%upcross%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossFlagPrimary', wset%upcross%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossFlagSecondary', wset%upcross%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossCrestCount', wset%upcross%ncrests_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossWaveCount', wset%upcross%nwaves_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHavg', wset%upcross%Havg_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHmax', wset%upcross%Hmax_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHsRMS', wset%upcross%Hrms_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHofTmax', wset%upcross%Htmax_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTofHmax', wset%upcross%Thmax_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTz', wset%upcross%Tavg_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTmax', wset%upcross%Tmax_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossBandwidth', wset%upcross%bwidth_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossCoverage', wset%upcross%cov_varid))
            call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossSourceIndex', wset%upcross%src_varid))

            errcode = nf90_inq_varid(wset%upcross%grpid, 'upcrossHtenth', wset%upcross%H10_varid)
            if (errcode .eq. NF90_NOERR) then
              call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHthird', wset%upcross%H3_varid))
              call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTofHtenth', wset%upcross%Th10_varid))
              call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTofHthird', wset%upcross%Th3_varid))
              call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossQuantileHeight', wset%upcross%Hquant_varid))
            else
              wset%upcross%H10_varid = -1
              errcode = 0
            end if

c           errcode = nf90_inq_varid(wset%upcross%grpid, 'upcrossTtenth', wset%upcross%T10_varid)
c           if (errcode .eq. NF90_NOERR) then
c             call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossTthird', wset%upcross%H3_varid))
c             call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHofTtenth', wset%upcross%Ht10_varid))
c             call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossHofTthird', wset%upcross%Ht3_varid))
c             call nc_call_func(nf90_inq_varid(wset%upcross%grpid, 'upcrossPeriodQuantile', wset%upcross%Tquant_varid))
c           else
c             wset%upcross%T10_varid = -1
c             errcode = 0
c           end if

          else
            wset%upcross%grpid = -1
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'sync', wset%sync%grpid)
          else
            wset%sync%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%sync%grpid, 'syncTime', wset%sync%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%sync%grpid, 'syncTime', wset%sync%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncTime', wset%sync%time_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncTimeBounds', wset%sync%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncSegmentCount', wset%sync%segcnt_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncSegmentsUsed', wset%sync%segs_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncSamples', wset%sync%samples_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncLastDisplacements', wset%sync%disp_varid))
            call nc_call_func(nf90_inq_varid(wset%sync%grpid, 'syncSourceIndex', wset%sync%src_varid))
          else
            wset%sync%grpid = -1
            errcode = 0
          end if


          if (wset%use_groups) then
            errcode = nf90_inq_grp_ncid(wset%ncid, 'cat4', wset%cat4%grpid)
          else
            wset%cat4%grpid = wset%ncid
            errcode = nf90_inq_dimid(wset%cat4%grpid, 'cat4Time', wset%cat4%time_dimid)
          end if
          if (errcode .eq. NF90_NOERR) then
            call nc_call_func(nf90_inq_dimid(wset%cat4%grpid, 'cat4Time', wset%cat4%time_dimid))

            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4Time', wset%cat4%time_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4TimeBounds', wset%cat4%tbounds_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4AirTemperature', wset%cat4%airt_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4FlagPrimary', wset%cat4%flags_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4FlagSecondary', wset%cat4%flags2_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4SourceIndex', wset%cat4%src_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4StatusFlags', wset%cat4%status_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4WhiteTemperature', wset%cat4%white_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4BlackTemperature', wset%cat4%black_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4MetalTemperature', wset%cat4%metal_varid))
            call nc_call_func(nf90_inq_varid(wset%cat4%grpid, 'cat4GroovedTemperature', wset%cat4%grooved_varid))
          else
            wset%cat4%grpid = -1
            errcode = 0
          end if

          return
        end subroutine


c-- WC5_GET_GROUP_VALUE --------------------------------------------------------
c   Returns the group value for a set based on the groups which are present
c-------------------------------------------------------------------------------
        integer function wc5_get_group_value(wset)
          type(wc5_dataset)       wset

          wc5_get_group_value = 0
          if (wset%source%grpid .ne. -1 .and. wset%source%file_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_source_bit
          if (wset%wave%grpid .ne. -1 .and. wset%wave%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_wave_bit + 2**WC5_spectra_bit
          if (wset%xyz%grpid .ne. -1 .and. wset%xyz%rec_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_xyz_bit
          if (wset%sst%grpid .ne. -1 .and. wset%sst%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_sst_bit
          if (wset%gps%grpid .ne. -1 .and. wset%gps%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_gps_bit
          if (wset%acm%grpid .ne. -1 .and. wset%acm%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_acm_bit
          if (wset%dwr%grpid .ne. -1 .and. wset%dwr%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_dwr_bit
          if (wset%dwr4%grpid .ne. -1 .and. wset%dwr4%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_dwr4_bit
          if (wset%upcross%grpid .ne. -1 .and. wset%upcross%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_upcross_bit
          if (wset%sync%grpid .ne. -1 .and. wset%sync%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_sync_bit
          if (wset%cat4%grpid .ne. -1 .and. wset%cat4%time_count .gt. 0) 
     *      wc5_get_group_value = wc5_get_group_value + 2**WC5_cat4_bit

          return
        end function


c-- WC5_GET_GROUP_VALUE_FROM_COUNTS --------------------------------------------
c   Returns the group value for a set based on the groups have records
c-------------------------------------------------------------------------------
        integer function wc5_get_group_value_from_counts(wset)
          integer                 group_val
          type(wc5_dataset)       wset

          group_val = 0
          if (wset%source%file_count .gt. 0) group_val = group_val + 2**WC5_source_bit
          if (wset%wave%time_count .gt. 0) then
            group_val = group_val + 2**WC5_wave_bit
            if (wset%wave%freq_count .gt. 0) group_val = group_val + 2**WC5_spectra_bit
          end if
          if (wset%xyz%rec_count .gt. 0) group_val = group_val + 2**WC5_xyz_bit
          if (wset%sst%time_count .gt. 0) group_val = group_val + 2**WC5_sst_bit
          if (wset%gps%time_count .gt. 0) group_val = group_val + 2**WC5_gps_bit
          if (wset%acm%time_count .gt. 0) group_val = group_val + 2**WC5_acm_bit
          if (wset%dwr%time_count .gt. 0) group_val = group_val + 2**WC5_dwr_bit
          if (wset%dwr4%time_count .gt. 0) group_val = group_val + 2**WC5_dwr4_bit
          if (wset%upcross%time_count .gt. 0) group_val = group_val + 2**WC5_upcross_bit
          if (wset%sync%time_count .gt. 0) group_val = group_val + 2**WC5_sync_bit
          if (wset%cat4%time_count .gt. 0) group_val = group_val + 2**WC5_cat4_bit
          wc5_get_group_value_from_counts = group_val

          return
        end function


c-- WC5_GET_MIN_TIME -----------------------------------------------------------
c   Returns the earliest time in the dataset by checking all groups
c-------------------------------------------------------------------------------
        integer function wc5_get_min_time(wset)
          integer                 maxval
          type(wc5_dataset)       wset

          maxval = HUGE(maxval)
          wc5_get_min_time = maxval

          if (wset%wave%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%wave%times(1))
          if (wset%sst%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%sst%times(1))
          if (wset%gps%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%gps%times(1))
          if (wset%acm%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%acm%times(1))
          if (wset%dwr%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%dwr%times(1))
          if (wset%dwr4%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%dwr4%times(1))
          if (wset%upcross%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%upcross%times(1))
          if (wset%sync%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%sync%times(1))
          if (wset%cat4%time_count .ge. 1) wc5_get_min_time = MIN(wc5_get_min_time, wset%cat4%times(1))
          if (wc5_get_min_time .eq. maxval) wc5_get_min_time = -1

          return
        end function


c-- WC5_LOAD_COORD_VARS --------------------------------------------------------
c   Reads the coordinate variables of the dataset. Note - ALLOCATE vars first!
c-------------------------------------------------------------------------------
        subroutine wc5_load_coord_vars(wset, load_type, errcode)
          integer                 count_dims(2), errcode, i, j, load_type, start_dims(2)
          character*100           dim_name
          character,allocatable:: file_name_tmp(:,:)
          type(wc5_dataset)       wset

          if (BTEST(load_type, WC5_wave_bit) .and. wset%wave%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%time_varid, wset%wave%times))

          if (BTEST(load_type, WC5_spectra_bit) .and. wset%wave%grpid .ne. -1) then
            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%freq_varid, wset%wave%freqs))
            wset%wave%bw = WC5_real_fill
            wset%wave%fflags = WC5_byte_fill
            wset%wave%fflags2 = WC5_byte_fill
            if (wset%is_2d_model) call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%dir_varid, wset%wave%dirs))
          end if

          if (BTEST(load_type, WC5_gps_bit) .and. wset%gps%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%gps%grpid, wset%gps%time_varid, wset%gps%times))

          if (BTEST(load_type, WC5_sst_bit) .and. wset%sst%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%time_varid, wset%sst%times))

          if (BTEST(load_type, WC5_acm_bit) .and. wset%acm%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%time_varid, wset%acm%times))

          if (BTEST(load_type, WC5_dwr_bit) .and. wset%dwr%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%time_varid, wset%dwr%times))

          if (BTEST(load_type, WC5_dwr4_bit) .and. wset%dwr4%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%time_varid, wset%dwr4%times))

          if (BTEST(load_type, WC5_upcross_bit) .and. wset%upcross%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%time_varid, wset%upcross%times))

          if (BTEST(load_type, WC5_sync_bit) .and. wset%sync%grpid .ne. -1)
     *      call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%time_varid, wset%sync%times))

          if (BTEST(load_type, WC5_cat4_bit) .and. wset%cat4%grpid .ne. -1) then
            errcode = nf90_get_var(wset%cat4%grpid, wset%cat4%time_varid, wset%cat4%times)
            if (errcode .ne. NF90_NOERR) then
              load_type = IBCLR(load_type, WC5_cat4_bit)
              if (BTEST(wset%groups, WC5_cat4_bit)) wset%groups = IBCLR(wset%groups, WC5_cat4_bit)
            end if
          end if

          if (BTEST(load_type, WC5_xyz_bit) .and. wset%xyz%grpid .ne. -1) then
            call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%stime_varid, wset%xyz%start_time))
            call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%srate_varid, wset%xyz%sample_rate))
          end if

          if (BTEST(load_type, WC5_source_bit) .and. wset%source%grpid .ne. -1 .and. wset%is_xy_only) then
            data start_dims / 1, 1 /
            count_dims(2) = wset%source%file_count

            if (wset%source%file_name_length .gt. 0 .and. wset%source%file_name_length .ne. WC5_filename_length) then
              count_dims(1) = wset%source%file_name_length
              allocate(file_name_tmp(wset%source%file_name_length, wset%source%file_count))
              call nc_call_func(nf90_get_var(wset%source%grpid, wset%source%name_varid, file_name_tmp,
     *          start_dims, count_dims))
              do i = 1, wset%source%file_count
                do j = 1, wset%source%file_name_length
                  wset%source%file_name(j,i) = file_name_tmp(j,i)
                end do
              end do
            else
              count_dims(1) = WC5_filename_length
              call nc_call_func(nf90_get_var(wset%source%grpid, wset%source%name_varid, wset%source%file_name,
     *          start_dims, count_dims))
            end if
          end if
        end subroutine


c-- WC5_LOAD_SET ---------------------------------------------------------------
c   Loads a wavecdf dataset for the selected timespan.
c-------------------------------------------------------------------------------
        subroutine wc5_load_set(wc5_name, load_type, oset, timespan, minf, maxf, 
     *              errcode)
          integer                 end_findex, end_tindex, errcode, i, j, load_type
          integer                 logunit, start_tindex, start_findex, xyz_recs
          integer                 count_dim(1), count_dims(2), count2d_dims(3)
          integer                 start_dim(1), start_dims(2), start2d_dims(3)
          real                    filter_delay
          real,optional::         minf, maxf
          logical::               exists, found1, found2, include_spectra, verbose
          logical::               limit_times, limit_freqs, xyz_found
          character,allocatable:: file_name_tmp(:,:)
          character*100           wc5_name
          type(time_span),optional::         timespan
          type(wc5_dataset)                  tset, wset
          type(wc5_dataset), intent(out)::   oset

          if (PRESENT(timespan)) then
            limit_times = .true.
          else
            limit_times = .false.
          end if

          if (PRESENT(minf) .and. PRESENT(maxf)) then
            limit_freqs = .true.
          else
            limit_freqs = .false.
          end if

          errcode = 0
          logunit = 6
          verbose = .false.

          if (INDEX(wc5_name,'http') .eq. 0) then
            inquire(file=wc5_name,exist=exists)
            if (.not. exists) then
              errcode = 1
              return
            end if
          end if

          call wc5_initialize_set(tset)
          call wc5_initialize_set(wset)
          call wc5_initialize_set(oset)

          if (verbose) write(logunit,'(a)') '  Starting netCDF read...'
          errcode = nf90_open(wc5_name, NF90_NOWRITE, wset%ncid)
          if (errcode .ne. 0) return

          call wc5_read_ids(wset, errcode)
          tset = wset

          call wc5_read_dimensions(tset, errcode)
          call wc5_limit_dimensions(tset, load_type)
          if (tset%xyz%rec_count .gt. 0) then
            xyz_found = .true.
            xyz_recs = tset%xyz%rec_count
          else
            xyz_found = .false.
          end if
          tset%xyz%rec_count = 0
          call wc5_allocate_set(tset)
          call wc5_load_coord_vars(tset, load_type, errcode)

c--  Load the source group

          if (BTEST(load_type, WC5_source_bit) .and. wset%source%grpid .ne. -1 .and. tset%source%file_count .gt. 0) then
            wset%source%file_count = tset%source%file_count
            call wc5_allocate_set(wset)

            start_dims(1) = 1
            start_dims(2) = 1
            count_dims(2) = wset%source%file_count

            if (tset%source%file_name_length .gt. 0 .and. tset%source%file_name_length .ne. WC5_filename_length) then
              count_dims(1) = tset%source%file_name_length
              allocate(file_name_tmp(tset%source%file_name_length, tset%source%file_count))
              call nc_call_func(nf90_get_var(wset%source%grpid, wset%source%name_varid, file_name_tmp,
     *          start_dims, count_dims))
              do i = 1, tset%source%file_count
                do j = 1, tset%source%file_name_length
                  wset%source%file_name(j,i) = file_name_tmp(j,i)
                end do
              end do
            else
              count_dims(1) = WC5_filename_length
              call nc_call_func(nf90_get_var(wset%source%grpid, wset%source%name_varid, wset%source%file_name, 
     *          start_dims, count_dims))
            end if
          end if

c--  Load the waves group

          if (BTEST(load_type, WC5_wave_bit) .and. wset%wave%grpid .ne. -1 .and. tset%wave%time_count .gt. 0) then
            if (limit_times) then
              call nc_find_time_index(tset%wave%times, tset%wave%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%wave%times, tset%wave%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%wave%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%wave%time_count = end_tindex - start_tindex + 1
            if (wset%wave%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%wave%time_count = 0
            else

              call wc5_allocate_set(wset)
              start_dim(1) = start_tindex
              count_dim(1) = wset%wave%time_count
              wset%wave%times = tset%wave%times(start_tindex:end_tindex)

              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%flags_varid, wset%wave%flags, start_dim,
     *               count_dim))
              wset%wave%pub_tf = MERGE(.true., .false., wset%wave%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%flags2_varid, wset%wave%flags2, start_dim,
     *               count_dim))

              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%hs_varid, wset%wave%hs, start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tp_varid, wset%wave%tp, start_dim, count_dim))
              errcode = nf90_get_var(wset%wave%grpid, wset%wave%dp_varid, wset%wave%dp, start_dim, count_dim)
              errcode = 0
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%ta_varid, wset%wave%ta, start_dim, count_dim))
              if (wset%is_dwr .or. wset%is_mk4) then
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%psdmax_varid, wset%wave%psdmax, start_dim, 
     *            count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tz_varid, wset%wave%tz, start_dim, count_dim))
              end if
              if (wset%is_mk4) then
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%sprdmax_varid, wset%wave%spreadmax, 
     *            start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tint_varid, wset%wave%tint, 
     *            start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tener_varid, wset%wave%tener, 
     *            start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tm13_varid, wset%wave%tm13, 
     *            start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tcrest_varid, wset%wave%tcrest, 
     *            start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%iqp_varid, wset%wave%iqp, 
     *            start_dim, count_dim))
              end if
              if (wset%is_nearshore) then
                call nc_call_func(nf90_get_var(wset%wave%grpid,wset%wave%dm_varid,wset%wave%dm,start_dim,count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid,wset%wave%sxy_varid,wset%wave%sxy,start_dim,count_dim))
                call nc_call_func(nf90_get_var(wset%wave%grpid,wset%wave%sxx_varid,wset%wave%sxx,start_dim,count_dim))
              end if
              if (wset%is_net_model) then
                start_dims(1) = 1
                start_dims(2) = start_tindex
                count_dims(1) = WC5_modelinput_length
                count_dims(2) = wset%wave%time_count
                errcode = nf90_get_var(wset%wave%grpid, wset%wave%modelinput_varid, wset%wave%model_input, 
     *            start_dims, count_dims)
                if (errcode .ne. NF90_NOERR) wset%wave%model_input = '_'
              else if (.not. wset%is_2d_model .and. wset%wave%src_varid .gt. 0) then
                call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%src_varid, wset%wave%src_index, start_dim, 
     *            count_dim))
              end if


              if (BTEST(load_type, WC5_spectra_bit) .and. tset%wave%freq_count .gt. 0) then
                if (limit_freqs) then
                  call nc_find_freq_index(tset%wave%freqs, tset%wave%freq_count, minf, 
     *              3, start_findex, found1)
                  call nc_find_freq_index(tset%wave%freqs, tset%wave%freq_count, maxf,
     *              2, end_findex, found2)
                else
                  start_findex = 1
                  end_findex = tset%wave%freq_count
                  found1 = .true.
                  found2 = .true.
                end if

                wset%wave%freq_count = end_findex - start_findex + 1
                if (wset%wave%freq_count .lt. 1 .or. .not. found1 .or. .not. found2) then
                  wset%wave%freq_count = 0
                else

                  wset%wave%dir_count = tset%wave%dir_count
                  call wc5_allocate_set(wset)
                  call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%bw_varid, tset%wave%bw))
                  call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%fflags_varid, tset%wave%fflags))
                  call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%fflags2_varid, tset%wave%fflags2))

                  if (wset%wave%dir_count .gt. 0) wset%wave%dirs = tset%wave%dirs
                  wset%wave%freqs = tset%wave%freqs(start_findex:end_findex)
                  wset%wave%bw = tset%wave%bw(start_findex:end_findex)
                  wset%wave%fflags = tset%wave%fflags(start_findex:end_findex)
                  wset%wave%fflags2 = tset%wave%fflags2(start_findex:end_findex)

                  start_dims(1) = start_findex
                  start_dims(2) = start_tindex
                  count_dims(1) = wset%wave%freq_count
                  count_dims(2) = wset%wave%time_count

                  start2d_dims(1) = 1
                  start2d_dims(2) = start_findex
                  start2d_dims(3) = start_tindex
                  count2d_dims(1) = wset%wave%dir_count
                  count2d_dims(2) = wset%wave%freq_count
                  count2d_dims(3) = wset%wave%time_count

                  call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a0_varid, wset%wave%a0, start_dims, count_dims))
                  errcode = nf90_get_var(wset%wave%grpid, wset%wave%mdir_varid,wset%wave%mdir,start_dims,count_dims)
                  if (errcode .eq. NF90_NOERR) then
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a1_varid, wset%wave%a1, start_dims, count_dims))
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%b1_varid, wset%wave%b1, start_dims, count_dims))
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a2_varid, wset%wave%a2, start_dims, count_dims))
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%b2_varid, wset%wave%b2, start_dims, count_dims))
                  end if
                  errcode = 0
                  if (wset%is_net_model .or. wset%is_dwr .or. wset%is_mk4) call nc_call_func(nf90_get_var(wset%wave%grpid, 
     *              wset%wave%check_varid, wset%wave%check, start_dims, count_dims))
                  if (wset%is_2d_model) call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%dirspec_varid, 
     *              wset%wave%dirspec, start2d_dims, count2d_dims))
                  if (wset%is_dwr .or. wset%is_mk4) then
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%spread_varid, wset%wave%dspread, start_dims, 
     *                     count_dims))
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%m2_varid, wset%wave%m2, start_dims, 
     *                count_dims))
                    call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%n2_varid, wset%wave%n2, start_dims, 
     *                count_dims))
                  end if
                end if
              end if
            end if
          end if

c--  Load the displacements (xyz) group

          if (BTEST(load_type, WC5_xyz_bit) .and. wset%xyz%grpid .ne. -1 .and. (tset%xyz%rec_count .gt. 0 .or.
     *         xyz_found .eqv. .true.)) then
            call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%stime_varid, tset%xyz%start_time))
            call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%srate_varid, tset%xyz%sample_rate))
            call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%delay_varid, wset%xyz%filter_delay))
            if (wset%xyz%filter_delay .eq. WC5_real_fill) then
              filter_delay = 0.0
            else
              filter_delay = wset%xyz%filter_delay
            end if
            if (xyz_found) tset%xyz%rec_count = xyz_recs
            if (limit_times) then
              call nc_find_sample_index(tset%xyz%start_time-NINT(filter_delay), tset%xyz%sample_rate, 
     *          tset%xyz%rec_count, date_to_timestamp(timespan%start), 1, start_tindex, found1)
              if (found1 .and. start_tindex .eq. tset%xyz%rec_count) 
     *          call nc_find_sample_index(tset%xyz%start_time-NINT(filter_delay), tset%xyz%sample_rate, 
     *            tset%xyz%rec_count, date_to_timestamp(timespan%start), 3, start_tindex, found1)
              call nc_find_sample_index(tset%xyz%start_time-NINT(filter_delay), tset%xyz%sample_rate, 
     *            tset%xyz%rec_count, date_to_timestamp(timespan%end), 2, end_tindex, found2)	
            else
              start_tindex = 1
              end_tindex = tset%xyz%rec_count
              found1 = .true.
              found2 = .true.
            end if

            wset%xyz%rec_count = end_tindex - start_tindex + 1
            if (wset%xyz%rec_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%xyz%rec_count = 0
            else

              call wc5_allocate_set(wset)
              wset%xyz%start_time = tset%xyz%start_time + INT(FLOAT(start_tindex-1)/tset%xyz%sample_rate)
              wset%xyz%sample_rate = tset%xyz%sample_rate
              start_dim(1) = start_tindex
              count_dim(1) = wset%xyz%rec_count

              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%flags_varid, wset%xyz%flags, start_dim,
     *               count_dim))
              wset%xyz%pub_tf = MERGE(.true., .false., wset%xyz%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%flags2_varid, wset%xyz%flags2, start_dim,
     *               count_dim))

              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%zdisp_varid, wset%xyz%zdisp, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%xdisp_varid, wset%xyz%xdisp, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%ydisp_varid, wset%xyz%ydisp, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%xyz%grpid, wset%xyz%src_varid, 
     *               wset%xyz%src_index, start_dim, count_dim))
            end if
          end if

c--  Load the SST group

          if (BTEST(load_type, WC5_sst_bit) .and. wset%sst%grpid .ne. -1 .and. tset%sst%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%time_varid, tset%sst%times))
            if (limit_times) then
              call nc_find_time_index(tset%sst%times, tset%sst%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%sst%times, tset%sst%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%sst%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%sst%time_count = end_tindex - start_tindex + 1
            if (wset%sst%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%sst%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%sst%times = tset%sst%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%sst%time_count

              call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%flags_varid, wset%sst%flags, start_dim,
     *               count_dim))
              wset%sst%pub_tf = MERGE(.true., .false., wset%sst%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%flags2_varid, wset%sst%flags2, start_dim,
     *               count_dim))

              call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%sstC_varid, wset%sst%sstC, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%src_varid, wset%sst%src_index, 
     *               start_dim, count_dim))
              if (wset%is_dwr) call nc_call_func(nf90_get_var(wset%sst%grpid, wset%sst%reftemp_varid, 
     *          wset%sst%reftemp, start_dim, count_dim))
            end if
          end if

c--  Load the GPS group

          if (BTEST(load_type, WC5_gps_bit) .and. wset%gps%grpid .ne. -1 .and. tset%gps%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%gps%grpid, wset%gps%time_varid, tset%gps%times))
            if (limit_times) then
              call nc_find_time_index(tset%gps%times, tset%gps%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%gps%times, tset%gps%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%gps%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%gps%time_count = end_tindex - start_tindex + 1
            if (wset%gps%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%gps%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%gps%times = tset%gps%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%gps%time_count

              errcode = nf90_get_var(wset%gps%grpid, wset%gps%flags_varid, wset%gps%flags, start_dim, count_dim)
              if (errcode .eq. NF90_NOERR) then
                wset%gps%hf_errors = MERGE(.true., .false., BTEST(wset%gps%flags,3))
                wset%gps%merit = MERGE(.true., .false., BTEST(wset%gps%flags,2))
                wset%gps%new_fix = MERGE(.true., .false., BTEST(wset%gps%flags,1))
                wset%gps%mod_ok = MERGE(.true., .false., BTEST(wset%gps%flags,0))
              end if
              errcode = 0

              call nc_call_func(nf90_get_var(wset%gps%grpid, wset%gps%latitude_varid, wset%gps%latitude, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%gps%grpid, wset%gps%longitude_varid, wset%gps%longitude, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%gps%grpid, wset%gps%src_varid, 
     *               wset%gps%src_index, start_dim, count_dim))
            end if
          end if

c--  Load the ACM group

          if (BTEST(load_type, WC5_acm_bit) .and. wset%acm%grpid .ne. -1 .and. tset%acm%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%time_varid, tset%acm%times))
            if (limit_times) then
              call nc_find_time_index(tset%acm%times, tset%acm%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%acm%times, tset%acm%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%acm%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%acm%time_count = end_tindex - start_tindex + 1
            if (wset%acm%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%acm%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%acm%times = tset%acm%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%acm%time_count

              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%flags_varid, wset%acm%flags, start_dim,
     *               count_dim))
              wset%acm%pub_tf = MERGE(.true., .false., wset%acm%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%flags2_varid, wset%acm%flags2, start_dim,
     *               count_dim))

              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%speed_varid, wset%acm%speed, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%speedstd_varid, wset%acm%speedstd, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%dir_varid, wset%acm%dir, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%dirstd_varid, wset%acm%dirstd, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%rssi1_varid, wset%acm%rssi1, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%rssi2_varid, wset%acm%rssi2, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%rssi3_varid, wset%acm%rssi3, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%csst_varid, wset%acm%csst, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%vert_varid, wset%acm%vert, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%vertstd_varid, wset%acm%vertstd, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%status_varid, wset%acm%cstatus, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%acm%grpid, wset%acm%src_varid, 
     *               wset%acm%src_index, start_dim, count_dim))
            end if
          end if

c--  Load the DWR group

          if (BTEST(load_type, WC5_dwr_bit) .and. wset%dwr%grpid .ne. -1 .and. tset%dwr%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%time_varid, tset%dwr%times))
            if (limit_times) then
              call nc_find_time_index(tset%dwr%times, tset%dwr%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%dwr%times, tset%dwr%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%dwr%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%dwr%time_count = end_tindex - start_tindex + 1
            if (wset%dwr%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%dwr%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%dwr%times = tset%dwr%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%dwr%time_count

              if (wset%is_mk3 .or. wset%is_dwrg) 
     *          call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%wol_varid, wset%dwr%wol, start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%batt_varid, wset%dwr%batt, start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%za_off_varid, wset%dwr%za_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%xa_off_varid, wset%dwr%xa_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%ya_off_varid, wset%dwr%ya_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%orient_varid, wset%dwr%orient, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%inclin_varid, wset%dwr%inclin, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr%grpid, wset%dwr%src_varid, wset%dwr%src_index, 
     *          start_dim, count_dim))
            end if
          end if

c--  Load the DWR4 group

          if (BTEST(load_type, WC5_dwr4_bit) .and. wset%dwr4%grpid .ne. -1 .and. tset%dwr4%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%time_varid, tset%dwr4%times))
            if (limit_times) then
              call nc_find_time_index(tset%dwr4%times, tset%dwr4%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%dwr4%times, tset%dwr4%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%dwr4%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%dwr4%time_count = end_tindex - start_tindex + 1
            if (wset%dwr4%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%dwr4%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%dwr4%times = tset%dwr4%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%dwr4%time_count

              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%uptime_varid, wset%dwr4%uptime, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%wol_varid, wset%dwr4%wol, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%enerused_varid, wset%dwr4%enerused, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%eboost_varid, wset%dwr4%eboost, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%hatchtemp_varid, wset%dwr4%hatchtemp, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%voltage_varid, wset%dwr4%voltage, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%za_off_varid, wset%dwr4%za_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%za_max_varid, wset%dwr4%za_max, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%xa_off_varid, wset%dwr4%xa_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%xa_max_varid, wset%dwr4%xa_max, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%ya_off_varid, wset%dwr4%ya_off, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%ya_max_varid, wset%dwr4%ya_max, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%orient_mean_varid, wset%dwr4%orient_mean, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%orient_dev_varid, wset%dwr4%orient_dev, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%inclin_mean_varid, wset%dwr4%inclin_mean, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%inclin_dev_varid, wset%dwr4%inclin_dev, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%maglength_mean_varid, 
     *          wset%dwr4%maglength_mean, start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%maglength_dev_varid, 
     *          wset%dwr4%maglength_dev, start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%pitch_max_varid, wset%dwr4%pitch_max, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%roll_max_varid, wset%dwr4%roll_max, 
     *          start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%sensortemp_varid, wset%dwr4%sensortemp, 
     *          start_dim, count_dim))

              call nc_call_func(nf90_get_var(wset%dwr4%grpid, wset%dwr4%src_varid, 
     *               wset%dwr4%src_index, start_dim, count_dim))
            end if
          end if

c--  Load the UPCROSS group

          if (BTEST(load_type, WC5_upcross_bit) .and. wset%upcross%grpid .ne. -1 
     *        .and. tset%upcross%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%time_varid, tset%upcross%times))
            if (limit_times) then
              call nc_find_time_index(tset%upcross%times, tset%upcross%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%upcross%times, tset%upcross%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%upcross%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%upcross%time_count = end_tindex - start_tindex + 1
            if (wset%upcross%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%upcross%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%upcross%times = tset%upcross%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%upcross%time_count

              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%flags_varid, wset%upcross%flags, 
     *               start_dim, count_dim))
              wset%upcross%pub_tf = MERGE(.true., .false., wset%upcross%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%flags2_varid, wset%upcross%flags2, 
     *               start_dim, count_dim))

              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%ncrests_varid, wset%upcross%num_crests, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%nwaves_varid, wset%upcross%num_waves, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Havg_varid, wset%upcross%Havg, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Hmax_varid, wset%upcross%Hmax, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Hrms_varid, wset%upcross%Hrms, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Htmax_varid, wset%upcross%H_at_Tmax, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Thmax_varid, wset%upcross%T_at_Hmax, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Tavg_varid, wset%upcross%Tavg, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Tmax_varid, wset%upcross%Tmax, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%bwidth_varid, wset%upcross%bandwidth, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%cov_varid, wset%upcross%coverage, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%src_varid, 
     *               wset%upcross%src_index, start_dim, count_dim))

              start_dims(1) = 1
              start_dims(2) = start_tindex
              count_dims(1) = WC5_upcross_quantile_length
              count_dims(2) = wset%upcross%time_count

              errcode = nf90_get_var(wset%upcross%grpid, wset%upcross%H10_varid, wset%upcross%H10, start_dim, count_dim)
              if (errcode .eq. NF90_NOERR) then
                call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%H3_varid, wset%upcross%H3, 
     *                 start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Th10_varid, wset%upcross%TofH10, 
     *                 start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Th3_varid, wset%upcross%TofH3, 
     *                 start_dim, count_dim))
                call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Hquant_varid, wset%upcross%H_quantile, 
     *                 start_dims, count_dims))
              else
                wset%upcross%H10 = WC5_real_fill
                errcode = 0
              end if

c             errcode = nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%T10_varid, wset%upcross%T10, 
c    *                    start_dim, count_dim))
c             if (errcode .eq. NF90_NOERR) then
c               call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%T3_varid, wset%upcross%T3, 
c    *                 start_dim, count_dim))
c               call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Ht10_varid, wset%upcross%HofT10, 
c    *                 start_dim, count_dim))
c               call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Ht3_varid, wset%upcross%HofT3, 
c    *                 start_dim, count_dim))
c               call nc_call_func(nf90_get_var(wset%upcross%grpid, wset%upcross%Tquant_varid, wset%upcross%T_quantile, 
c    *                 start_dims, count_dims))
c             else
c               wset%upcross%T10 = WC5_real_fill
c               errcode = 0
c             end if

            end if
          end if

c--  Load the SYNC group

          if (BTEST(load_type, WC5_sync_bit) .and. wset%sync%grpid .ne. -1 .and. tset%sync%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%time_varid, tset%sync%times))
            if (limit_times) then
              call nc_find_time_index(tset%sync%times, tset%sync%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%sync%times, tset%sync%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%sync%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%sync%time_count = end_tindex - start_tindex + 1
            if (wset%sync%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%sync%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%sync%times = tset%sync%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%sync%time_count

              start_dims(1) = 1
              start_dims(2) = start_tindex
              count_dims(1) = 18
              count_dims(2) = wset%sync%time_count

              call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%segcnt_varid, wset%sync%seg_count, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%segs_varid, wset%sync%segs_used, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%samples_varid, wset%sync%samples, 
     *               start_dim, count_dim))
              call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%disp_varid, wset%sync%disp_hex, 
     *               start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%sync%grpid, wset%sync%src_varid, wset%sync%src_index, 
     *               start_dim, count_dim))
            end if
          end if

c--  Load the CAT4 group

          if (BTEST(load_type, WC5_cat4_bit) .and. wset%cat4%grpid .ne. -1 .and. tset%cat4%time_count .gt. 0) then
            call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%time_varid, tset%cat4%times))
            if (limit_times) then
              call nc_find_time_index(tset%cat4%times, tset%cat4%time_count, timespan%start, 
     *          3, start_tindex, found1)
              call nc_find_time_index(tset%cat4%times, tset%cat4%time_count, timespan%end, 
     *          2, end_tindex, found2)
            else
              start_tindex = 1
              end_tindex = tset%cat4%time_count
              found1 = .true.
              found2 = .true.
            end if

            wset%cat4%time_count = end_tindex - start_tindex + 1
            if (wset%cat4%time_count .lt. 1 .or. .not. found1 .or. .not. found2) then
              wset%cat4%time_count = 0
            else

              call wc5_allocate_set(wset)
              wset%cat4%times = tset%cat4%times(start_tindex:end_tindex)
              start_dim(1) = start_tindex
              count_dim(1) = wset%cat4%time_count

              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%flags_varid, wset%cat4%flags, start_dim,
     *               count_dim))
              wset%cat4%pub_tf = MERGE(.true., .false., wset%cat4%flags .eq. 1)
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%flags2_varid, wset%cat4%flags2, start_dim,
     *               count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%src_varid, wset%cat4%src_index, 
     *               start_dim, count_dim))

              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%airt_varid, wset%cat4%airt, start_dim, 
     *          count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%status_varid, wset%cat4%status, start_dim, 
     *          count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%white_varid, wset%cat4%white, start_dim, 
     *          count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%black_varid, wset%cat4%black, start_dim, 
     *          count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%metal_varid, wset%cat4%metal, start_dim, 
     *          count_dim))
              call nc_call_func(nf90_get_var(wset%cat4%grpid, wset%cat4%grooved_varid, wset%cat4%grooved, start_dim, 
     *          count_dim))
            end if
          end if


          oset = wset
          oset%groups = wc5_get_group_value(oset)
c         call wc5_subset_match(wset, wset%wave%flags, 1, 1, oset)

          call nc_call_func(nf90_close(wset%ncid))
          call wc5_deallocate_set(wset)
          call wc5_deallocate_set(tset)

          return
        end subroutine


c-- WC5_SUBSET_CLOSEST_TIME ----------------------------------------------------
c  Returns a single-record set with each group holding only the record closest
c  to the given time.
c-------------------------------------------------------------------------------
        subroutine wc5_subset_closest_time(wset, target, groups, cset)
          byte      orig_wave_flag, orig_sst_flag, orig_gps_flag, orig_acm_flag, orig_upcross_flag, orig_cat4_flag
          logical               found
          integer               cloc, groups
          type(date_block)      target
          type(wc5_dataset)     cset, wset

          if (BTEST(groups, WC5_wave_bit)) then
            call nc_find_time_index(wset%wave%times, wset%wave%time_count, target, 1, cloc, found)
            if (found) then
              orig_wave_flag = wset%wave%flags(cloc)
              wset%wave%flags = 4
              wset%wave%flags(cloc) = 1
            end if
          end if

          if (BTEST(groups, WC5_sst_bit)) then
            call nc_find_time_index(wset%sst%times, wset%sst%time_count, target, 1, cloc, found)
            if (found) then
              orig_sst_flag = wset%sst%flags(cloc)
              wset%sst%flags = 4
              wset%sst%flags(cloc) = 1
            end if
          end if

          if (BTEST(groups, WC5_cat4_bit)) then
            call nc_find_time_index(wset%cat4%times, wset%cat4%time_count, target, 1, cloc, found)
            if (found) then
              orig_cat4_flag = wset%cat4%flags(cloc)
              wset%cat4%flags = 4
              wset%cat4%flags(cloc) = 1
            end if
          end if

          if (BTEST(groups, WC5_gps_bit)) then
            call nc_find_time_index(wset%gps%times, wset%gps%time_count, target, 1, cloc, found)
            if (found) then
              orig_gps_flag = wset%gps%flags(cloc)
              wset%gps%flags = 1
              wset%gps%flags(cloc) = 3
            end if
          end if

          if (BTEST(groups, WC5_acm_bit)) then
            call nc_find_time_index(wset%acm%times, wset%acm%time_count, target, 1, cloc, found)
            if (found) then
              orig_acm_flag = wset%acm%flags(cloc)
              wset%acm%flags = 4
              wset%acm%flags(cloc) = 1
            end if
          end if

          if (BTEST(groups, WC5_upcross_bit)) then
            call nc_find_time_index(wset%upcross%times, wset%upcross%time_count, target, 1, cloc, found)
            if (found) then
              orig_upcross_flag = wset%upcross%flags(cloc)
              wset%upcross%flags = 4
              wset%upcross%flags(cloc) = 1
            end if
          end if

          call wc5_apply_flags(wset, cset)
          if (BTEST(groups, WC5_wave_bit)) cset%wave%flags(1) = orig_wave_flag
          if (BTEST(groups, WC5_sst_bit)) cset%sst%flags(1) = orig_sst_flag
          if (BTEST(groups, WC5_cat4_bit)) cset%cat4%flags(1) = orig_cat4_flag
          if (BTEST(groups, WC5_gps_bit)) cset%gps%flags(1) = orig_gps_flag
          if (BTEST(groups, WC5_acm_bit)) cset%acm%flags(1) = orig_acm_flag
          if (BTEST(groups, WC5_upcross_bit)) cset%upcross%flags(1) = orig_upcross_flag

        end subroutine


c-- WC5_SUBSET_MATCH -----------------------------------------------------------
c  Trims the set based on the provided time-dimensioned variable and target
c  value. Records are selected based on the following match_types:
c    1 - elements in the output set are EQUAL TO the target;
c    2 - elements in the output set are GREATER THAN OR EQUAL TO the target;
c    3 - elements in the output set are LESS THAN OR EQUAL TO the target;
c-------------------------------------------------------------------------------
        subroutine wc5_subset_match(wset, mfield, mtype, mvalue, subset)
          integer               i, mfield(*), mtype, mvalue, tcount
          type(wc5_dataset)     subset, wset

          tcount = 0
          do i = 1, wset%wave%time_count
            if ((mtype .eq. 1 .and. mfield(i) .eq. mvalue) .or.
     *          (mtype .eq. 2 .and. mfield(i) .ge. mvalue) .or.
     *          (mtype .eq. 3 .and. mfield(i) .le. mvalue)) tcount = tcount + 1
          end do

          subset%wave%time_count = tcount
          call wc5_allocate_set(subset)
                 
          tcount = 0
          do i = 1, wset%wave%time_count
            if ((mtype .eq. 1 .and. mfield(i) .eq. mvalue) .or.
     *          (mtype .eq. 2 .and. mfield(i) .ge. mvalue) .or.
     *          (mtype .eq. 3 .and. mfield(i) .le. mvalue)) then
              tcount = tcount + 1
              subset%wave%times(tcount) = wset%wave%times(i)
              subset%wave%flags(tcount) = wset%wave%flags(i)
              subset%wave%flags2(tcount) = wset%wave%flags2(i)
              subset%wave%pub_tf(tcount) = wset%wave%pub_tf(i)
              subset%wave%dp(tcount) = wset%wave%dp(i)
              subset%wave%hs(tcount) = wset%wave%hs(i)
              subset%wave%tp(tcount) = wset%wave%tp(i)
              subset%wave%ta(tcount) = wset%wave%ta(i)
            end if
          end do

          return
        end subroutine


c-- WC5_RECALCULATE_PARAMS ------------------------------------------------------
c  Calculates bulk parameters for a set. Useful for when the freq layout
c  has been trimmed or the spectra modified in some other way.
c  NOTE: calcs incorrect if some bands have a0=WC5_real_fill, set mask_missing
c   to true in those cases.
c-------------------------------------------------------------------------------
        subroutine wc5_recalculate_params(wset, use_mask)
          real                      m0, m1, m2, max_ed
          real,allocatable::        a0(:,:)
          integer                   i, j, max_idx
          logical                   mask_missing
          logical,optional::        use_mask
          logical,allocatable::     a0_mask(:,:)
          type(sp_data_block)       sp_data
          type(wc5_dataset)         wset

          mask_missing = .false.
          if (PRESENT(use_mask)) then
            if (use_mask .eqv. .true.) mask_missing = .true.
          end if

          allocate(a0(wset%wave%freq_count,wset%wave%time_count), a0_mask(wset%wave%freq_count,wset%wave%time_count))
          if (mask_missing) then
            a0_mask = MERGE(.true., .false., (wset%wave%a0 .ne. WC5_real_fill))
            a0 = MERGE(wset%wave%a0, 0.0, a0_mask)
          else
            a0 = wset%wave%a0
          end if

          do i = 1, wset%wave%time_count
            if (MAXVAL(wset%wave%a0(:,i)) .ne. WC5_real_fill) then
              max_idx = MAXLOC(wset%wave%a0(:,i),1)
              wset%wave%tp(i) = 1.0 / wset%wave%freqs(max_idx)
              wset%wave%psdmax(i) = wset%wave%a0(max_idx,i)

              m0 = SUM(a0(:,i) * wset%wave%bw)
              m1 = SUM(a0(:,i) * wset%wave%bw * wset%wave%freqs)
              m2 = SUM(a0(:,i) * wset%wave%bw * wset%wave%freqs**2)

              if (m0 .gt. 0.0) then
                wset%wave%hs(i) = 4 * (m0)**(0.5)
                wset%wave%ta(i) = m0 / m1
                wset%wave%tz(i) = (m0 / m2)**(0.5)
              else
                wset%wave%hs(i) = 0.0
                wset%wave%ta(i) = WC5_real_fill
                wset%wave%tz(i) = WC5_real_fill
              end if
              if (wc5_is_complete_spectrum(wset,i)) then
                wset%wave%dp(i) = wset%wave%mdir(max_idx,i)
              else
                wset%wave%dp(i) = WC5_real_fill
              end if
            else
              wset%wave%hs(i) = WC5_real_fill
              wset%wave%tp(i) = WC5_real_fill
              wset%wave%dp(i) = WC5_real_fill
              wset%wave%ta(i) = WC5_real_fill
              wset%wave%tz(i) = WC5_real_fill
              wset%wave%psdmax(i) = WC5_real_fill
            end if
          end do

          deallocate(a0, a0_mask)
          return
        end subroutine


c-- WC5_CALCULATE_MOMENTS ------------------------------------------------------
c  Calculates the spectral moments m0, m1, m2, m4, and m-1
c-------------------------------------------------------------------------------
	subroutine wc5_calculate_moments(wset, m0, m1, m2, m4, n1)
          integer   i,j
	  real	    bener, m0(*), m1(*), m2(*), m4(*), n1(*)
	  type(wc5_dataset), intent(in)::  wset

	  do i = 1, wset%wave%time_count
            m0(i) = 0.0
            m1(i) = 0.0
            m2(i) = 0.0
            m4(i) = 0.0
            n1(i) = 0.0
            do j = 1, wset%wave%freq_count
	      bener = wset%wave%a0(j,i) * wset%wave%bw(j)
	      m0(i) = m0(i) + bener
	      m1(i) = m1(i) + bener*wset%wave%freqs(j)
	      m2(i) = m2(i) + bener*wset%wave%freqs(j)**2
	      m4(i) = m4(i) + bener*wset%wave%freqs(j)**4
	      n1(i) = n1(i) + bener*wset%wave%freqs(j)**(-1)
            end do
          end do
        end subroutine


c-- WC5_UNSHOAL_SPECTRA --------------------------------------------------------
c  Uses the given shoaling coeffs to calculate deep water spectra
c-------------------------------------------------------------------------------
        subroutine wc5_unshoal_spectra(wset, shoal)
          integer                 errcode, i, j
          real                    shoal(*)
          type(sp_data_block)     sp_data
          type(wc5_dataset)       wset

          do i = 1, wset%wave%time_count
            do j = 1, wset%wave%freq_count
              wset%wave%a0(j,i) = wset%wave%a0(j,i) / shoal(j)
            end do
          end do
          call wc5_recalculate_params(wset)
          wset%is_nearshore = .false.        !* unset Sxx, Sxy, Dm
        end subroutine


c-- WC5_TIME_SUBSET -------------------------------------------------------------
c  Returns a subset of the given dataset, all data between start and end
c-------------------------------------------------------------------------------
        subroutine wc5_time_subset(wset, start_date, end_date, owset)
          integer                 end_index, start_index, time_count
          logical                 found
          type(date_block)        end_date, start_date
          type(wc5_dataset), intent(in)::   wset
          type(wc5_dataset), intent(out)::  owset

          owset%wave%time_count = 0

          call nc_find_time_index(wset%wave%times, wset%wave%time_count, start_date, 
     *      3, start_index, found)
          if (.not. found) return

          call nc_find_time_index(wset%wave%times, wset%wave%time_count, end_date, 
     *      2, end_index, found)
          if (.not. found) return

          if (end_index .lt. start_index) return

          time_count = end_index - start_index + 1
         
          owset%wave%time_count = time_count
          call wc5_allocate_set(owset)

          owset%wave%times = wset%wave%times(start_index:end_index)

          owset%wave%flags = wset%wave%flags(start_index:end_index)
          owset%wave%flags2 = wset%wave%flags2(start_index:end_index)
          owset%wave%pub_tf = wset%wave%pub_tf(start_index:end_index)
          owset%wave%hs = wset%wave%hs(start_index:end_index)
          owset%wave%tp = wset%wave%tp(start_index:end_index)
          owset%wave%dp = wset%wave%dp(start_index:end_index)
          owset%wave%ta = wset%wave%ta(start_index:end_index)

          return
        end subroutine


c-- WC5_ALLOCATE_SET ------------------------------------------------------------
c  Allocates arrays for the dataset. IMPORTANT: the relevant %time_count and/or
c  %freq_count values must be set before calling this routine.
c-------------------------------------------------------------------------------
        subroutine wc5_allocate_set(wset, fill)
          integer             fc, i, tc
          logical             fill_vars
          logical, optional:: fill
          type(wc5_dataset)   wset
          
          if (PRESENT(fill)) then
            fill_vars = fill
          else
            fill_vars = .true.
          end if


          if (.not. ALLOCATED(wset%source%file_name) .and. wset%source%file_count .gt. 0) then
            allocate (wset%source%file_name(WC5_filename_length, wset%source%file_count))
            if (fill_vars) then
              wset%source%file_name = WC5_char_fill
            end if
          end if


          if (.not. ALLOCATED(wset%wave%hs) .and. wset%wave%time_count .gt. 0) then
            tc = wset%wave%time_count
            allocate (wset%wave%hs(tc), wset%wave%tp(tc), wset%wave%dp(tc), wset%wave%ta(tc), wset%wave%times(tc), 
     *        wset%wave%flags(tc), wset%wave%flags2(tc), wset%wave%pub_tf(tc), wset%wave%src_index(tc), 
     *        wset%wave%tz(tc), wset%wave%psdmax(tc), wset%wave%model_input(WC5_modelinput_length,tc),
     *        wset%wave%dm(tc), wset%wave%sxy(tc), wset%wave%sxx(tc), wset%wave%spreadmax(tc), wset%wave%tint(tc),
     *        wset%wave%tener(tc), wset%wave%tm13(tc), wset%wave%tcrest(tc), wset%wave%iqp(tc))
            if (fill_vars) then
              wset%wave%hs = WC5_real_fill
              wset%wave%tp = WC5_real_fill
              wset%wave%dp = WC5_real_fill
              wset%wave%ta = WC5_real_fill
              wset%wave%tz = WC5_real_fill
              wset%wave%psdmax = WC5_real_fill
              wset%wave%spreadmax = WC5_real_fill
              wset%wave%tint = WC5_real_fill
              wset%wave%tener = WC5_real_fill
              wset%wave%tm13 = WC5_real_fill
              wset%wave%tcrest = WC5_real_fill
              wset%wave%iqp = WC5_real_fill
              wset%wave%flags = WC5_byte_fill
              wset%wave%flags2 = WC5_byte_fill
              wset%wave%model_input = WC5_char_fill
              wset%wave%pub_tf = .false.
              wset%wave%src_index = -1
            end if
          end if

          if (.not. ALLOCATED(wset%wave%freqs) .and. wset%wave%freq_count .ge. 1) then
            tc = wset%wave%time_count
            fc = wset%wave%freq_count
            if (tc .gt. 0 .and. fc .gt. 0) then
              allocate (wset%wave%freqs(fc), wset%wave%a0(fc,tc), wset%wave%mdir(fc,tc), wset%wave%a1(fc,tc), 
     *          wset%wave%b1(fc,tc), wset%wave%a2(fc,tc), wset%wave%b2(fc,tc), wset%wave%check(fc,tc), 
     *          wset%wave%bw(fc), wset%wave%dspread(fc,tc), wset%wave%m2(fc,tc), wset%wave%n2(fc,tc),
     *          wset%wave%fflags(fc), wset%wave%fflags2(fc))
              if (fill_vars) then
                wset%wave%freqs = WC5_real_fill
                wset%wave%a0 = WC5_real_fill
                wset%wave%mdir = WC5_real_fill
                wset%wave%a1 = WC5_real_fill
                wset%wave%b1 = WC5_real_fill
                wset%wave%a2 = WC5_real_fill
                wset%wave%b2 = WC5_real_fill
                wset%wave%check = WC5_real_fill
                wset%wave%bw = WC5_real_fill
                wset%wave%dspread = WC5_real_fill
                wset%wave%m2 = WC5_real_fill
                wset%wave%n2 = WC5_real_fill
                wset%wave%fflags = WC5_byte_fill
                wset%wave%fflags2 = WC5_byte_fill
              end if
              if (.not. ALLOCATED(wset%wave%dirs) .and. wset%wave%dir_count .gt. 0) then
                allocate(wset%wave%dirs(wset%wave%dir_count), wset%wave%dirspec(wset%wave%dir_count,fc,tc))
                if (fill_vars) then
                  wset%wave%dirs = WC5_real_fill
                  wset%wave%dirspec = WC5_real_fill
                end if
              end if
            end if
          end if


          if (.not. ALLOCATED(wset%xyz%zdisp) .and. wset%xyz%rec_count .gt. 0) then
            tc = wset%xyz%rec_count
            allocate (wset%xyz%zdisp(tc), wset%xyz%xdisp(tc), wset%xyz%ydisp(tc), 
     *        wset%xyz%flags(tc), wset%xyz%flags2(tc), wset%xyz%pub_tf(tc), wset%xyz%src_index(tc))
            if (fill_vars) then
              wset%xyz%zdisp = WC5_real_fill
              wset%xyz%xdisp = WC5_real_fill
              wset%xyz%ydisp = WC5_real_fill
              wset%xyz%flags = WC5_byte_fill
              wset%xyz%flags2 = WC5_byte_fill
              wset%xyz%pub_tf = .false.
              wset%xyz%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%gps%latitude) .and. wset%gps%time_count .gt. 0) then
            tc = wset%gps%time_count
            allocate (wset%gps%latitude(tc), wset%gps%longitude(tc), wset%gps%times(tc), wset%gps%flags(tc), 
     *        wset%gps%merit(tc), wset%gps%src_index(tc), wset%gps%new_fix(tc), wset%gps%mod_ok(tc), 
     *        wset%gps%hf_errors(tc))
            if (fill_vars) then
              wset%gps%latitude = WC5_real_fill
              wset%gps%longitude = WC5_real_fill
              wset%gps%times = WC5_int_fill
              wset%gps%flags = WC5_byte_fill
              wset%gps%merit = .false.
              wset%gps%new_fix = .false.
              wset%gps%mod_ok = .false.
              wset%gps%hf_errors = .false.
              wset%gps%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%sst%sstC) .and. wset%sst%time_count .gt. 0) then
            tc = wset%sst%time_count
            allocate (wset%sst%sstC(tc), wset%sst%times(tc), wset%sst%flags(tc), wset%sst%flags2(tc), 
     *        wset%sst%pub_tf(tc), wset%sst%src_index(tc), wset%sst%reftemp(tc))
            if (fill_vars) then
              wset%sst%sstC = WC5_real_fill
              wset%sst%reftemp = WC5_real_fill
              wset%sst%times = WC5_int_fill
              wset%sst%flags = WC5_byte_fill
              wset%sst%flags2 = WC5_byte_fill
              wset%sst%pub_tf = .false.
              wset%sst%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%acm%speed) .and. wset%acm%time_count .gt. 0) then
            tc = wset%acm%time_count
            allocate (wset%acm%flags(tc), wset%acm%flags2(tc), wset%acm%cstatus(tc), wset%acm%speed(tc), 
     *        wset%acm%dir(tc), wset%acm%speedstd(tc), wset%acm%dirstd(tc), wset%acm%rssi1(tc), wset%acm%rssi2(tc), 
     *        wset%acm%rssi3(tc), wset%acm%csst(tc), wset%acm%vert(tc), wset%acm%vertstd(tc), wset%acm%times(tc), 
     *        wset%acm%pub_tf(tc), wset%acm%src_index(tc))
            if (fill_vars) then
              wset%acm%flags = WC5_byte_fill
              wset%acm%flags2 = WC5_byte_fill
              wset%acm%cstatus = WC5_byte_fill
              wset%acm%speed = WC5_real_fill
              wset%acm%dir = WC5_real_fill
              wset%acm%speedstd = WC5_real_fill
              wset%acm%dirstd = WC5_real_fill
              wset%acm%rssi1 = WC5_real_fill
              wset%acm%rssi2 = WC5_real_fill
              wset%acm%rssi3 = WC5_real_fill
              wset%acm%csst = WC5_real_fill
              wset%acm%vert = WC5_real_fill
              wset%acm%vertstd = WC5_real_fill
              wset%acm%times = WC5_int_fill
              wset%acm%pub_tf = .false.
              wset%acm%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%dwr%orient) .and. wset%dwr%time_count .gt. 0) then
            tc = wset%dwr%time_count
            allocate (wset%dwr%times(tc), wset%dwr%wol(tc), wset%dwr%batt(tc), wset%dwr%za_off(tc), 
     *        wset%dwr%xa_off(tc), wset%dwr%ya_off(tc), wset%dwr%orient(tc), wset%dwr%inclin(tc), 
     *        wset%dwr%src_index(tc))
            if (fill_vars) then
              wset%dwr%times = WC5_int_fill
              wset%dwr%wol = WC5_int_fill
              wset%dwr%batt = WC5_int_fill
              wset%dwr%za_off = WC5_real_fill
              wset%dwr%xa_off = WC5_real_fill
              wset%dwr%ya_off = WC5_real_fill
              wset%dwr%orient = WC5_real_fill
              wset%dwr%inclin = WC5_real_fill
              wset%dwr%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%dwr4%voltage) .and. wset%dwr4%time_count .gt. 0) then
            tc = wset%dwr4%time_count
            allocate (wset%dwr4%times(tc), wset%dwr4%uptime(tc), wset%dwr4%enerused(tc), wset%dwr4%eboost(tc), 
     *        wset%dwr4%za_off(tc), wset%dwr4%za_max(tc), wset%dwr4%xa_off(tc), wset%dwr4%xa_max(tc), 
     *        wset%dwr4%ya_off(tc), wset%dwr4%ya_max(tc), wset%dwr4%pitch_max(tc), wset%dwr4%roll_max(tc),
     *        wset%dwr4%hatchtemp(tc), wset%dwr4%voltage(tc), wset%dwr4%orient_mean(tc), wset%dwr4%wol(tc),
     *        wset%dwr4%orient_dev(tc), wset%dwr4%inclin_mean(tc), wset%dwr4%inclin_dev(tc), wset%dwr4%sensortemp(tc),
     *        wset%dwr4%maglength_mean(tc), wset%dwr4%maglength_dev(tc), wset%dwr4%src_index(tc))
            if (fill_vars) then
              wset%dwr4%times = WC5_int_fill
              wset%dwr4%uptime = WC5_int_fill
              wset%dwr4%wol = WC5_int_fill
              wset%dwr4%enerused = WC5_int_fill
              wset%dwr4%src_index = WC5_int_fill
              wset%dwr4%eboost = WC5_int_fill
              wset%dwr4%za_max = WC5_int_fill
              wset%dwr4%xa_max = WC5_int_fill
              wset%dwr4%ya_max = WC5_int_fill
              wset%dwr4%pitch_max = WC5_int_fill
              wset%dwr4%roll_max = WC5_int_fill
              wset%dwr4%hatchtemp = WC5_real_fill
              wset%dwr4%voltage = WC5_real_fill
              wset%dwr4%za_off = WC5_real_fill
              wset%dwr4%xa_off = WC5_real_fill
              wset%dwr4%ya_off = WC5_real_fill
              wset%dwr4%orient_mean = WC5_real_fill
              wset%dwr4%orient_dev = WC5_real_fill
              wset%dwr4%inclin_mean = WC5_real_fill
              wset%dwr4%inclin_dev = WC5_real_fill
              wset%dwr4%maglength_mean = WC5_real_fill
              wset%dwr4%maglength_dev = WC5_real_fill
              wset%dwr4%sensortemp = WC5_real_fill
            end if
          end if


          if (.not. ALLOCATED(wset%upcross%Hmax) .and. wset%upcross%time_count .gt. 0) then
            tc = wset%upcross%time_count
            allocate (wset%upcross%Hmax(tc), wset%upcross%times(tc), wset%upcross%flags(tc), wset%upcross%flags2(tc), 
     *        wset%upcross%pub_tf(tc), wset%upcross%src_index(tc), wset%upcross%num_crests(tc), 
     *        wset%upcross%num_waves(tc), wset%upcross%Havg(tc), wset%upcross%Hrms(tc), wset%upcross%H_at_Tmax(tc), 
     *        wset%upcross%T_at_Hmax(tc), wset%upcross%Tavg(tc), wset%upcross%Tmax(tc), wset%upcross%bandwidth(tc), 
     *        wset%upcross%coverage(tc), wset%upcross%H10(tc), wset%upcross%H3(tc), wset%upcross%HofT10(tc), 
     *        wset%upcross%HofT3(tc), wset%upcross%H_quantile(WC5_upcross_quantile_length,tc), wset%upcross%T10(tc), 
     *        wset%upcross%T3(tc), wset%upcross%TofH10(tc), wset%upcross%TofH3(tc), 
     *        wset%upcross%T_quantile(WC5_upcross_quantile_length,tc))
            if (fill_vars) then
              wset%upcross%times = WC5_int_fill
              wset%upcross%flags = WC5_byte_fill
              wset%upcross%flags2 = WC5_byte_fill
              wset%upcross%pub_tf = .false.
              wset%upcross%Havg = WC5_real_fill
              wset%upcross%Hmax = WC5_real_fill
              wset%upcross%Hrms = WC5_real_fill
              wset%upcross%H_at_Tmax = WC5_real_fill
              wset%upcross%T_at_Hmax = WC5_real_fill
              wset%upcross%Tavg = WC5_real_fill
              wset%upcross%Tmax = WC5_real_fill
              wset%upcross%bandwidth = WC5_real_fill
              wset%upcross%coverage = WC5_real_fill
              wset%upcross%num_crests = WC5_int_fill
              wset%upcross%num_waves = WC5_int_fill
              wset%upcross%src_index = WC5_int_fill
              wset%upcross%H10 = WC5_real_fill
              wset%upcross%H3 = WC5_real_fill
              wset%upcross%HofT10 = WC5_real_fill
              wset%upcross%HofT3 = WC5_real_fill
              wset%upcross%T10 = WC5_real_fill
              wset%upcross%T3 = WC5_real_fill
              wset%upcross%TofH10 = WC5_real_fill
              wset%upcross%TofH3 = WC5_real_fill
              wset%upcross%H_quantile = WC5_real_fill
              wset%upcross%T_quantile = WC5_real_fill
            end if
          end if


          if (.not. ALLOCATED(wset%sync%segs_used) .and. wset%sync%time_count .gt. 0) then
            tc = wset%sync%time_count
            allocate (wset%sync%segs_used(tc), wset%sync%times(tc), wset%sync%samples(tc), wset%sync%disp_hex(18,tc),
     *        wset%sync%src_index(tc), wset%sync%seg_count(tc))
            if (fill_vars) then
              wset%sync%times = WC5_int_fill
              wset%sync%seg_count = WC5_int_fill
              wset%sync%segs_used = WC5_int_fill
              wset%sync%samples = WC5_int_fill
              wset%sync%disp_hex = WC5_char_fill
              wset%sync%src_index = WC5_int_fill
            end if
          end if


          if (.not. ALLOCATED(wset%cat4%airt) .and. wset%cat4%time_count .gt. 0) then
            tc = wset%cat4%time_count
            allocate (wset%cat4%airt(tc), wset%cat4%times(tc), wset%cat4%flags(tc), wset%cat4%flags2(tc), 
     *        wset%cat4%pub_tf(tc), wset%cat4%src_index(tc), wset%cat4%status(tc), wset%cat4%white(tc), 
     *        wset%cat4%black(tc), wset%cat4%metal(tc), wset%cat4%grooved(tc))
            if (fill_vars) then
              wset%cat4%times = WC5_int_fill
              wset%cat4%flags = WC5_byte_fill
              wset%cat4%flags2 = WC5_byte_fill
              wset%cat4%pub_tf = .false.
              wset%cat4%src_index = WC5_int_fill
              wset%cat4%airt = WC5_real_fill
              wset%cat4%status = WC5_int_fill
              wset%cat4%white = WC5_real_fill
              wset%cat4%black = WC5_real_fill
              wset%cat4%metal = WC5_real_fill
              wset%cat4%grooved = WC5_real_fill
            end if
          end if

        end subroutine 


c-- WC5_DEALLOCATE_SET ----------------------------------------------------------
c  Deallocates arrays for the dataset. 
c-------------------------------------------------------------------------------
        subroutine wc5_deallocate_set(wset)
          integer             i
          type(wc5_dataset)   wset
          
          if (ALLOCATED(wset%source%file_name)) 
     *      deallocate (wset%source%file_name)

          if (ALLOCATED(wset%wave%hs)) 
     *      deallocate (wset%wave%hs, wset%wave%tp, wset%wave%dp, wset%wave%ta, wset%wave%times, wset%wave%flags, 
     *        wset%wave%flags2, wset%wave%tz, wset%wave%psdmax, wset%wave%pub_tf, wset%wave%src_index, 
     *        wset%wave%model_input, wset%wave%dm, wset%wave%sxy, wset%wave%sxx, wset%wave%spreadmax, wset%wave%tint,
     *        wset%wave%tener, wset%wave%tm13, wset%wave%tcrest, wset%wave%iqp)

          if (ALLOCATED(wset%wave%freqs))
     *      deallocate (wset%wave%freqs, wset%wave%a0, wset%wave%mdir, wset%wave%a1, wset%wave%b1, wset%wave%a2, 
     *        wset%wave%b2, wset%wave%check, wset%wave%bw, wset%wave%dspread, wset%wave%m2, wset%wave%n2, 
     *        wset%wave%fflags, wset%wave%fflags2)

          if (ALLOCATED(wset%wave%dirs)) deallocate (wset%wave%dirs)
          if (ALLOCATED(wset%wave%dirspec)) deallocate (wset%wave%dirspec)

          if (ALLOCATED(wset%xyz%zdisp)) deallocate (wset%xyz%zdisp, wset%xyz%xdisp, wset%xyz%ydisp,
     *       wset%xyz%flags, wset%xyz%flags2, wset%xyz%pub_tf, wset%xyz%src_index)

          if (ALLOCATED(wset%gps%latitude)) deallocate (wset%gps%latitude, wset%gps%longitude, wset%gps%times, 
     *      wset%gps%flags, wset%gps%merit, wset%gps%new_fix, wset%gps%mod_ok, wset%gps%src_index, wset%gps%hf_errors)

          if (ALLOCATED(wset%sst%sstC)) deallocate (wset%sst%sstC, wset%sst%times, wset%sst%flags, wset%sst%flags2, 
     *      wset%sst%reftemp, wset%sst%pub_tf, wset%sst%src_index)

          if (ALLOCATED(wset%acm%speed)) deallocate (wset%acm%flags, wset%acm%flags2, wset%acm%cstatus, wset%acm%speed,
     *        wset%acm%dir, wset%acm%speedstd, wset%acm%dirstd, wset%acm%rssi1, wset%acm%rssi2, wset%acm%pub_tf, 
     *        wset%acm%rssi3, wset%acm%csst, wset%acm%vert, wset%acm%vertstd, wset%acm%times, wset%acm%src_index)

          if (ALLOCATED(wset%dwr%orient)) deallocate (wset%dwr%times, wset%dwr%wol, wset%dwr%batt, 
     *        wset%dwr%za_off, wset%dwr%xa_off, wset%dwr%ya_off, wset%dwr%orient, wset%dwr%inclin, 
     *        wset%dwr%src_index)

          if (ALLOCATED(wset%dwr4%voltage))
     *      deallocate (wset%dwr4%times, wset%dwr4%uptime, wset%dwr4%wol, wset%dwr4%enerused, wset%dwr4%eboost, 
     *        wset%dwr4%za_off, wset%dwr4%za_max, wset%dwr4%xa_off, wset%dwr4%xa_max, 
     *        wset%dwr4%ya_off, wset%dwr4%ya_max, wset%dwr4%pitch_max, wset%dwr4%roll_max,
     *        wset%dwr4%hatchtemp, wset%dwr4%voltage, wset%dwr4%orient_mean, 
     *        wset%dwr4%orient_dev, wset%dwr4%inclin_mean, wset%dwr4%inclin_dev, 
     *        wset%dwr4%maglength_mean, wset%dwr4%maglength_dev, wset%dwr4%src_index, wset%dwr4%sensortemp)

          if (ALLOCATED(wset%upcross%Hmax))
     *      deallocate (wset%upcross%flags, wset%upcross%flags2, wset%upcross%num_crests, wset%upcross%num_waves, 
     *        wset%upcross%Havg, wset%upcross%Hmax, wset%upcross%Hrms, wset%upcross%H_at_Tmax, wset%upcross%T_at_Hmax,
     *        wset%upcross%Tavg, wset%upcross%Tmax, wset%upcross%bandwidth, wset%upcross%coverage, wset%upcross%times,
     *        wset%upcross%src_index, wset%upcross%pub_tf)
          if (ALLOCATED(wset%upcross%H10))
     *      deallocate (wset%upcross%H10, wset%upcross%H3, wset%upcross%TofH10, wset%upcross%TofH3, 
     *        wset%upcross%H_quantile)
          if (ALLOCATED(wset%upcross%T10))
     *      deallocate (wset%upcross%T10, wset%upcross%T3, wset%upcross%HofT10, wset%upcross%HofT3, 
     *        wset%upcross%T_quantile)

          if (ALLOCATED(wset%sync%segs_used)) deallocate (wset%sync%times, wset%sync%segs_used, wset%sync%samples, 
     *      wset%sync%disp_hex, wset%sync%src_index, wset%sync%seg_count)

          if (ALLOCATED(wset%cat4%airt)) deallocate (wset%cat4%airt, wset%cat4%times, wset%cat4%flags, wset%cat4%flags2, 
     *      wset%cat4%pub_tf, wset%cat4%src_index, wset%cat4%status, wset%cat4%white, wset%cat4%black, wset%cat4%metal, 
     *      wset%cat4%grooved)

        end subroutine 


c-- WC5_DEALLOCATE_WAVES -------------------------------------------------------
c  Deallocates wave components of the dataset. 
c-------------------------------------------------------------------------------
        subroutine wc5_deallocate_waves(wset)
          type(wc5_dataset)   wset
          
          if (ALLOCATED(wset%wave%hs)) 
     *      deallocate (wset%wave%hs, wset%wave%tp, wset%wave%dp, wset%wave%ta, wset%wave%times, wset%wave%flags, 
     *        wset%wave%flags2, wset%wave%tz, wset%wave%psdmax, wset%wave%pub_tf, wset%wave%src_index, 
     *        wset%wave%model_input, wset%wave%dm, wset%wave%sxy, wset%wave%sxx, wset%wave%spreadmax, wset%wave%tint,
     *        wset%wave%tener, wset%wave%tm13, wset%wave%tcrest, wset%wave%iqp)

          if (ALLOCATED(wset%wave%freqs))
     *      deallocate (wset%wave%freqs, wset%wave%a0, wset%wave%mdir, wset%wave%a1, wset%wave%b1, wset%wave%a2, 
     *        wset%wave%b2, wset%wave%check, wset%wave%bw, wset%wave%dspread, wset%wave%m2, wset%wave%n2, 
     *        wset%wave%fflags, wset%wave%fflags2)

        end subroutine 


c-- WC5_CREATE_NCFILE ----------------------------------------------------------
c  Initializes a NetCDF file for holding wave observations.
c-------------------------------------------------------------------------------
        subroutine wc5_create_ncfile(wset, ncname, errcode, cgroups, enddef)
          byte               acm_masks(3), cat4_masks(4), gps_masks(4)
          byte               pub_masks(1)
          integer::          bounds_dimid, combo_dim(2), combo2d_dim(3),  errcode, groups, i, lats, length_dimid, lons
          integer::          quant_dimid
          integer,optional:: cgroups
          real               max_check
          logical            end_defs
          logical,optional:: enddef
          character*4        null_att, sp_group_name 
          character*100      ancil, coord_str, ncname, time_label
          character*500      comment, gps_comment
          type(wc5_dataset)  wset
          type(meta_attribute_list)  time_atts, time_bounds_atts

          if (PRESENT(cgroups)) then
            groups = cgroups
          else
            groups = wset%groups
          end if

          if (PRESENT(enddef)) then
            end_defs = enddef
          else
            end_defs = .true.
          end if

          if (groups .eq. 0) then
            write(6,'(a)') 'WC5_CREATE_NCFILE error: groups not specified'
            errcode = -1
            return
          end if

          if (wset%classic) then
            call nc_call_func(nf90_create(ncname, NF90_CLASSIC_MODEL, wset%ncid))
          else
            call nc_call_func(nf90_create(ncname, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), wset%ncid))
c           call nc_call_func(nf90_create(ncname, NF90_HDF5, wset%ncid))
          end if
          pub_masks = (/ 1 /)
          if (wset%is_net_model .or. wset%is_2d_model) then
            coord_str = 'metaLatitude metaLongitude'
          else if (wset%is_stn_aggregate) then
            coord_str = 'metaLatitude metaLongitude'
          else if (wset%is_historic_aggregate) then
            coord_str = 'metaStationLatitude metaStationLongitude'
          else
            coord_str = 'metaDeployLatitude metaDeployLongitude'
          end if
          null_att = 'NULL'

          call nc_call_func(nf90_def_dim(wset%ncid, 'metaBoundsCount', 2, bounds_dimid))


c--  Define the source group

          if (BTEST(groups, WC5_source_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'source', wset%source%grpid))
            else
              wset%source%grpid = wset%ncid
            end if
            if (wset%is_xy_only) then
              call nc_call_func(nf90_def_dim(wset%source%grpid, 'sourceCount', 100000, wset%source%count_dimid))
            else if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%source%grpid, 'sourceCount', NF90_UNLIMITED, wset%source%count_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%source%grpid, 'sourceCount', 
     *          wset%source%file_count, wset%source%count_dimid))
            end if

            call nc_call_func(nf90_def_dim(wset%source%grpid, 'sourceNameLength', WC5_filename_length, length_dimid))

            combo_dim = (/length_dimid, wset%source%count_dimid/)
            call nc_call_func(nf90_def_var(wset%source%grpid, 'sourceFilename', 
     *             NF90_CHAR, combo_dim, wset%source%name_varid))
            call nc_assign_attributes(wset%source%grpid, wset%source%name_varid, 'source file name', null_att, 'CHAR')
          end if

c--  Define the waves group

          if (BTEST(groups, WC5_wave_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'wave', wset%wave%grpid))
            else
              wset%wave%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%wave%grpid, 'waveTime', NF90_UNLIMITED, wset%wave%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%wave%grpid, 'waveTime', wset%wave%time_count, wset%wave%time_dimid))
            end if

            ancil = 'waveFlagPrimary waveFlagSecondary'
            combo_dim = (/bounds_dimid, wset%wave%time_dimid/)

            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTime', 
     *             NF90_INT, wset%wave%time_dimid, wset%wave%time_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTimeBounds', 
     *             NF90_INT, combo_dim, wset%wave%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFlagPrimary', 
     *             NF90_BYTE, wset%wave%time_dimid, wset%wave%flags_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFlagSecondary', 
     *             NF90_BYTE, wset%wave%time_dimid, wset%wave%flags2_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveHs', NF90_FLOAT, wset%wave%time_dimid, 
     *             wset%wave%hs_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTp', NF90_FLOAT, wset%wave%time_dimid, 
     *             wset%wave%tp_varid))
            call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTa', NF90_FLOAT, wset%wave%time_dimid, 
     *             wset%wave%ta_varid))

            if (wset%is_directional) then
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveDp', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%dp_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%dp_varid, 'peak wave direction', 'degreeT', 'REAL',
     *          'sea_surface_wave_from_direction', TRIM(coord_str), 0.0, 360.0, TRIM(ancil), 'WAVE DIRECTION')
              if (wset%is_dwr .or. wset%is_mk4) call nc_call_func(nf90_put_att(wset%wave%grpid,wset%wave%dp_varid, 
     *          'additional_processing', 'Magnetic declination - INT(metaDeclination) - added to buoy-returned '//
     *          'direction to shift to degrees true.'))
            end if

            if (wset%is_dwr .or. wset%is_mk4) then
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'wavePeakPSD', 
     *               NF90_FLOAT, wset%wave%time_dimid, wset%wave%psdmax_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%psdmax_varid, 'peak wave power spectral density',
     *               'm*m/hertz', 'REAL', null_att, TRIM(coord_str), 0.0, 5000.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTz', 
     *               NF90_FLOAT, wset%wave%time_dimid, wset%wave%tz_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%tz_varid, 'spectral zero-upcross wave period', 
     *               'second', 'REAL', 'sea_surface_wave_zero_upcrossing_period', TRIM(coord_str), 0.0, 40.0, 
     *               TRIM(ancil))
            end if

            if (wset%is_mk4) then
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'wavePeakSpread', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%sprdmax_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%sprdmax_varid, 'peak wave directional spread',
     *               'degree', 'REAL', null_att, TRIM(coord_str), 0.0, 90.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTi', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%tint_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%tint_varid, 'integral wave period',
     *               'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTe', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%tener_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%tener_varid, 'energy wave period', 'second', 'REAL',
     *          'sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment' , 
     *          TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTm13', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%tm13_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%tm13_varid, 'sqrt(m1/m3) wave period',
     *               'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveTc', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%tcrest_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%tcrest_varid, 'crest wave period',
     *               'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveInverseQp', NF90_FLOAT, wset%wave%time_dimid, 
     *          wset%wave%iqp_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%iqp_varid, 'inverse Goda''s peakedness',
     *               null_att, 'REAL', null_att, TRIM(coord_str), 0.0, 1.0, TRIM(ancil)) 
            end if

            if (wset%is_net_model) then
              call nc_call_func(nf90_def_dim(wset%wave%grpid, 'waveModelInputLength', WC5_modelinput_length, 
     *          length_dimid))
              combo_dim = (/length_dimid, wset%wave%time_dimid/)
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveModelInputSource', NF90_CHAR, combo_dim, 
     *          wset%wave%modelinput_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%modelinput_varid, 'model input sources', null_att,
     *          'CHAR')
            else if (wset%is_dwr .or. wset%is_mk4 .or. MAXVAL(wset%wave%src_index) .gt. 0) then
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveSourceIndex', NF90_INT, wset%wave%time_dimid, 
     *          wset%wave%src_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%src_varid, 'source file index', null_att, 'INT')
            end if 

            if (wset%is_nearshore) then
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveDm', NF90_FLOAT, wset%wave%time_dimid, 
     *               wset%wave%dm_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveSxy', NF90_FLOAT, wset%wave%time_dimid, 
     *               wset%wave%sxy_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveSxx', NF90_FLOAT, wset%wave%time_dimid, 
     *               wset%wave%sxx_varid))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%dm_varid, 'bulk Sxy wave direction',
     *               'degreeT', 'REAL', null_att, TRIM(coord_str), 0.0, 360.0, TRIM(ancil))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%sxy_varid, 'alongshore radiation stress',
     *               'meter^2', 'REAL', null_att, TRIM(coord_str), 0.0, ancillary=TRIM(ancil))
              call nc_assign_attributes(wset%wave%grpid, wset%wave%sxx_varid, 'onshore radiation stress',
     *               'meter^2', 'REAL', null_att, TRIM(coord_str), 0.0, ancillary=TRIM(ancil))
            end if

            time_atts = meta_time_attlist()
            time_bounds_atts = meta_time_bounds_attlist()
            if (wset%is_net_model) then
              time_label = 'UTC prediction time'
            else
              time_label = 'UTC sample start time'
            end if
            call meta_add_attribute(time_atts, 'long_name', time_label, META_char_type)
            call meta_add_attribute(time_atts, 'bounds', 'waveTimeBounds', META_char_type)
            call wc5_add_attributes(wset%wave%grpid, wset%wave%time_varid, time_atts)
            if (.not. wset%is_net_model) then
              time_label = 'UTC sample time'
              call meta_add_attribute(time_atts, 'long_name', time_label, META_char_type)
            end if
            if (wset%is_dwr) call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%time_varid, 'comment', 
     *        'Spectral processing performed on 1600-second samples specified by waveTimeBounds.'))
            if (wset%is_mk4) call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%time_varid, 'comment', 
     *        'Spectral processing performed on 1800-second samples specified by waveTimeBounds.'))
            call meta_add_attribute(time_atts, 'long_name', time_label, META_char_type)
            call wc5_add_attributes(wset%wave%grpid, wset%wave%tbounds_varid, time_bounds_atts)

            call ioc_assign_flag1_atts(wset%wave%grpid, wset%wave%flags_varid, 'wave')
            if (wset%is_net_model .or. wset%is_2d_model) then
c             call ioc_assign_flag2_atts(wset%wave%grpid, wset%wave%flags2_varid, 'wave')
              call ioc_assign_flag2_atts(wset%wave%grpid, wset%wave%flags2_varid, 'model')
            else
              call ioc_assign_flag2_atts(wset%wave%grpid, wset%wave%flags2_varid, 'wave')
            end if

            call nc_assign_attributes(wset%wave%grpid, wset%wave%hs_varid, 'significant wave height', 'meter', 'REAL', 
     *        'sea_surface_wave_significant_height', TRIM(coord_str), 0.0, 20.0, TRIM(ancil), 
     *        'WAVE HEIGHT - SIGNIFICANT')
            call nc_assign_attributes(wset%wave%grpid, wset%wave%tp_varid, 'peak wave period', 'second', 'REAL',
     *        'sea_surface_wave_period_at_variance_spectral_density_maximum', TRIM(coord_str), 0.0, 40.0, TRIM(ancil),
     *        'WAVE - PEAK PERIOD')
            call nc_assign_attributes(wset%wave%grpid, wset%wave%ta_varid, 'average wave period', 'second', 'REAL',
     *        'sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment', TRIM(coord_str), 
     *        0.0, 40.0, TRIM(ancil), 'WAVE PERIOD - AVERAGE')
            if (wset%is_mk4) then
              call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%ta_varid, 'comment', 
     *          'Returned as T1, the mean period, by DWR4 buoys.'))
            else if (wset%is_dwr .or. wset%is_mk4) then
              call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%ta_varid, 'additional_processing', 
     *          'Calculated from the full-spectrum, buoy-returned energy density values.'))
            end if


c-  Add the spectral info

            if (BTEST(groups, WC5_spectra_bit)) then
              call nc_call_func(nf90_def_dim(wset%wave%grpid, 'waveFrequency', wset%wave%freq_count, 
     *               wset%wave%freq_dimid))

              combo_dim = (/bounds_dimid, wset%wave%freq_dimid/)
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFrequency', 
     *               NF90_FLOAT, wset%wave%freq_dimid, wset%wave%freq_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFrequencyBounds', 
     *               NF90_FLOAT, combo_dim, wset%wave%fbounds_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFrequencyFlagPrimary', 
     *               NF90_BYTE, wset%wave%freq_dimid, wset%wave%fflags_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveFrequencyFlagSecondary', 
     *               NF90_BYTE, wset%wave%freq_dimid, wset%wave%fflags2_varid))
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveBandwidth', 
     *               NF90_FLOAT, wset%wave%freq_dimid, wset%wave%bw_varid))

              combo_dim = (/wset%wave%freq_dimid, wset%wave%time_dimid/)
              call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveEnergyDensity', 
     *               NF90_FLOAT, combo_dim, wset%wave%a0_varid))
              if (wset%is_directional) then
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveMeanDirection', 
     *                 NF90_FLOAT, combo_dim, wset%wave%mdir_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveA1Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%a1_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveB1Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%b1_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveA2Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%a2_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveB2Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%b2_varid))
              end if
              if (wset%is_dwr .or. wset%is_mk4) then
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveCheckFactor', 
     *                 NF90_FLOAT, combo_dim, wset%wave%check_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveSpread', 
     *                 NF90_FLOAT, combo_dim, wset%wave%spread_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveM2Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%m2_varid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveN2Value', 
     *                 NF90_FLOAT, combo_dim, wset%wave%n2_varid))
              end if
              if (wset%is_net_model) call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveModelBinInputCoverage', 
     *                 NF90_FLOAT, combo_dim, wset%wave%check_varid))
              if (wset%is_2d_model) then
                call nc_call_func(nf90_def_dim(wset%wave%grpid, 'waveDirection', wset%wave%dir_count, 
     *            wset%wave%dir_dimid))
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveDirection', 
     *            NF90_FLOAT, wset%wave%dir_dimid, wset%wave%dir_varid))
                combo_dim = (/bounds_dimid, wset%wave%dir_dimid/)
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveDirectionBounds', 
     *            NF90_FLOAT, combo_dim, wset%wave%dbounds_varid))
                combo2d_dim = (/wset%wave%dir_dimid, wset%wave%freq_dimid, wset%wave%time_dimid/)
                call nc_call_func(nf90_def_var(wset%wave%grpid, 'waveDirectionalSpectrum', 
     *            NF90_FLOAT, combo2d_dim, wset%wave%dirspec_varid))
              end if

              ancil = 'waveFlagPrimary waveFrequencyFlagPrimary waveFlagSecondary waveFrequencyFlagSecondary'

              call nc_assign_attributes(wset%wave%grpid, wset%wave%freq_varid, 'band center frequency', 
     *          'hertz', null_att, 'wave_frequency')
              call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%freq_varid, 'bounds', 'waveFrequencyBounds'))
              if (wset%is_mk4) then
                comment = "Center frequencies for bands 46 and 79 differ from the native Datawell frequencies. "//
     *            "Note that Datawell values are used in the calculation of frequency-dependent wave parameters."
                call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%freq_varid, 'comment', TRIM(comment)))
              end if
              call nc_assign_attributes(wset%wave%grpid, wset%wave%fbounds_varid, 'frequency cell bounds', 'hertz', 
     *          null_att)
              call nc_assign_attributes(wset%wave%grpid, wset%wave%bw_varid, 'bandwidth', 'hertz', 'REAL', null_att,
     *          nodc='WAVE ENERGY - WIDTH OF SPECTRAL BANDS')
              call ioc_assign_flag1_atts(wset%wave%grpid, wset%wave%fflags_varid, 'waveFrequency')
              call ioc_assign_flag2_atts(wset%wave%grpid, wset%wave%fflags2_varid, 'waveFrequency')
              call nc_assign_attributes(wset%wave%grpid, wset%wave%a0_varid, 'band energy density', 'meter^2 second', 
     *          'REAL', 'sea_surface_wave_variance_spectral_density', TRIM(coord_str), 0.0, ancillary=TRIM(ancil),
     *           nodc='WAVE ENERGY - SPECTRAL VALUE')
              if (wset%is_directional) then
                call nc_assign_attributes(wset%wave%grpid, wset%wave%mdir_varid, 'band mean direction', 'degreeT', 
     *            'REAL', 'sea_surface_wave_from_direction', TRIM(coord_str), 0.0, 360.0, TRIM(ancil), 
     *            'WAVE DIRECTION - AVERAGE')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%a1_varid, 'band a1 Fourier coefficient', null_att, 
     *            'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil), 'WAVE DATA - ANGULAR FOURIER COEFFICIENTS')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%b1_varid, 'band b1 Fourier coefficient', null_att, 
     *            'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil), 'WAVE DATA - ANGULAR FOURIER COEFFICIENTS')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%a2_varid, 'band a2 Fourier coefficient', null_att, 
     *            'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil), 'WAVE DATA - ANGULAR FOURIER COEFFICIENTS')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%b2_varid, 'band b2 Fourier coefficient', null_att, 
     *            'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil), 'WAVE DATA - ANGULAR FOURIER COEFFICIENTS')
                if (wset%is_dwr .or. wset%is_mk4) then 
                  call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%mdir_varid, 'additional_processing', 
     *             'Magnetic declination (metaDeclination) added to buoy-returned direction to shift to degrees true.'))
                  call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%a1_varid, 'additional_processing', 
     *              'Calculated relative to true north from the buoy-returned mean dir, spread, m2, and n2.'))
                  call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%b1_varid, 'additional_processing', 
     *              'Calculated relative to true north from the buoy-returned mean dir, spread, m2, and n2.'))
                  call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%a2_varid, 'additional_processing', 
     *              'Calculated relative to true north from the buoy-returned mean dir, spread, m2, and n2.'))
                  call nc_call_func(nf90_put_att(wset%wave%grpid, wset%wave%b2_varid, 'additional_processing', 
     *              'Calculated relative to true north from the buoy-returned mean dir, spread, m2, and n2.'))
                end if
              end if
              if (wset%is_dwr .or. wset%is_mk4) then
                max_check = 2.55
                if (wset%is_mk4) max_check = 25.0
                call nc_assign_attributes(wset%wave%grpid, wset%wave%check_varid, 
     *            'band check factor (inverse of wave ellipticity)', null_att, 'REAL', null_att, TRIM(coord_str), 0.0,
     *            max_check, TRIM(ancil))
                call nc_assign_attributes(wset%wave%grpid, wset%wave%spread_varid, 'band directional spread', 
     *            'degree', 'REAL', null_att, TRIM(coord_str), 0.0, 90.0, TRIM(ancil))
                call nc_assign_attributes(wset%wave%grpid, wset%wave%m2_varid, 'band centered sine Fourier coefficient',
     *            null_att, 'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil))
                call nc_assign_attributes(wset%wave%grpid,wset%wave%n2_varid,'band centered cosine Fourier coefficient',
     *            null_att, 'REAL', null_att, TRIM(coord_str), -1.0, 1.0, TRIM(ancil))
              end if
              if (wset%is_net_model) call nc_assign_attributes(wset%wave%grpid, wset%wave%check_varid, 
     *            'weighted frequency bin input data coverage', null_att, 'REAL')
              if (wset%is_2d_model) then
                call nc_assign_attributes(wset%wave%grpid, wset%wave%dirspec_varid, 'directional energy density', 
     *            'meter^2 second / degree', 'REAL', 'sea_surface_wave_directional_variance_spectral_density', 
     *            TRIM(coord_str), 0.0, ancillary=TRIM(ancil), nodc='DIRECTIONAL WAVE SPECTRA')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%dir_varid, 'bin center direction', 
     *            'degreesT', null_att, 'wave_direction')
                call nc_assign_attributes(wset%wave%grpid, wset%wave%dbounds_varid, 'direction cell bounds', 
     *            'degreesT', null_att)
              end if
            end if
          end if

c--  Define the xyz group

          if (BTEST(groups, WC5_xyz_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'xyz', wset%xyz%grpid))
            else
              wset%xyz%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%xyz%grpid, 'xyzCount', NF90_UNLIMITED, wset%xyz%count_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%xyz%grpid, 'xyzCount', wset%xyz%rec_count, wset%xyz%count_dimid))
            end if

            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzStartTime', NF90_INT, varid=wset%xyz%stime_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzSampleRate', NF90_FLOAT, varid=wset%xyz%srate_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzFilterDelay', NF90_FLOAT, varid=wset%xyz%delay_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzFlagPrimary', 
     *        NF90_BYTE, wset%xyz%count_dimid, wset%xyz%flags_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzFlagSecondary', 
     *        NF90_BYTE, wset%xyz%count_dimid, wset%xyz%flags2_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzZDisplacement', 
     *        NF90_FLOAT, wset%xyz%count_dimid, wset%xyz%zdisp_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzXDisplacement', 
     *        NF90_FLOAT, wset%xyz%count_dimid, wset%xyz%xdisp_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzYDisplacement', 
     *        NF90_FLOAT, wset%xyz%count_dimid, wset%xyz%ydisp_varid))
            call nc_call_func(nf90_def_var(wset%xyz%grpid, 'xyzSourceIndex', 
     *             NF90_INT, wset%xyz%count_dimid, wset%xyz%src_varid))

            ancil = 'xyzFlagPrimary xyzFlagSecondary'

            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%stime_varid, 'displacement UTC time base', 
     *          'seconds since 1970-01-01 00:00:00 UTC', 'INT')
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%stime_varid, 'calendar', 'standard'))
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%srate_varid, 'displacement output sample rate', 
     *             'hertz', 'REAL') 
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%delay_varid, 'filter output delay', 
     *             'second', 'REAL') 

            call ioc_assign_flag1_atts(wset%xyz%grpid, wset%xyz%flags_varid, 'xyz')
            call ioc_assign_flag2_atts(wset%xyz%grpid, wset%xyz%flags2_varid, 'xyz')

            comment = 'UTC measurement times calculated as xyzStartTime + arrayIndex/xyzSampleRate - xyzFilterDelay, '//
     *        'where arrayIndex is the displacement array index starting from 0.'
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%zdisp_varid, 'sea surface vertical displacement',
     *        'meter', 'REAL', null_att, TRIM(coord_str), 20.47, -20.47, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%zdisp_varid,'name','z displacement'))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%zdisp_varid, 'comment', TRIM(comment)))
c           call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%zdisp_varid,'positive','up'))
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%xdisp_varid, 'sea surface north displacement',
     *        'meter', 'REAL', null_att, TRIM(coord_str), 20.47, -20.47, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%xdisp_varid,'name','x displacement'))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%xdisp_varid, 'comment', TRIM(comment)))
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%ydisp_varid, 'sea surface west displacement',
     *        'meter', 'REAL', null_att, TRIM(coord_str), 20.47, -20.47, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%ydisp_varid,'name','y displacement'))
            call nc_call_func(nf90_put_att(wset%xyz%grpid, wset%xyz%ydisp_varid, 'comment', TRIM(comment)))
            call nc_assign_attributes(wset%xyz%grpid, wset%xyz%src_varid, 'source file index', null_att, 
     *        'INT')
          end if

c--  Define the sst group

          if (BTEST(groups, WC5_sst_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'sst', wset%sst%grpid))
            else
              wset%sst%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%sst%grpid, 'sstTime', NF90_UNLIMITED, wset%sst%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%sst%grpid, 'sstTime', wset%sst%time_count, wset%sst%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%sst%time_dimid/)
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstTime', NF90_INT, wset%sst%time_dimid, 
     *             wset%sst%time_varid))
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstTimeBounds',
     *             NF90_INT, combo_dim, wset%sst%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstFlagPrimary', 
     *             NF90_BYTE, wset%sst%time_dimid, wset%sst%flags_varid))
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstFlagSecondary', 
     *             NF90_BYTE, wset%sst%time_dimid, wset%sst%flags2_varid))
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstSeaSurfaceTemperature', NF90_FLOAT, wset%sst%time_dimid, 
     *             wset%sst%sstC_varid))
            call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstSourceIndex', 
     *             NF90_INT, wset%sst%time_dimid, wset%sst%src_varid))

            ancil = 'sstFlagPrimary sstFlagSecondary'

            call meta_add_attribute(time_atts, 'bounds', 'sstTimeBounds', META_char_type)
            call wc5_add_attributes(wset%sst%grpid, wset%sst%time_varid, time_atts)
            call wc5_add_attributes(wset%sst%grpid, wset%sst%tbounds_varid, time_bounds_atts)

            call ioc_assign_flag1_atts(wset%sst%grpid, wset%sst%flags_varid, 'sst')
            call ioc_assign_flag2_atts(wset%sst%grpid, wset%sst%flags2_varid, 'sst')

            call nc_assign_attributes(wset%sst%grpid, wset%sst%sstC_varid, 'sea surface temperature', 'Celsius','REAL',
     *        'sea_surface_temperature', TRIM(coord_str), -5.0, 46.15, TRIM(ancil), 'SEA SURFACE TEMPERATURE')
            call nc_call_func(nf90_put_att(wset%sst%grpid, wset%sst%sstC_varid,'cell_methods','sstTime: point'))
            call nc_assign_attributes(wset%sst%grpid, wset%sst%src_varid, 'source file index', null_att, 'INT')

            if (wset%is_dwr) then
              call nc_call_func(nf90_def_var(wset%sst%grpid, 'sstReferenceTemp', NF90_FLOAT, wset%sst%time_dimid, 
     *               wset%sst%reftemp_varid))
              call nc_assign_attributes(wset%sst%grpid, wset%sst%reftemp_varid, 'reference temperature', 
     *               'Celsius', 'REAL') 
            end if
          end if

c--  Define the gps group

          if (BTEST(groups, WC5_gps_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'gps', wset%gps%grpid))
            else
              wset%gps%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%gps%grpid, 'gpsTime', NF90_UNLIMITED, wset%gps%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%gps%grpid, 'gpsTime', wset%gps%time_count, wset%gps%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%gps%time_dimid/)
            call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsTime', NF90_INT, wset%gps%time_dimid, 
     *             wset%gps%time_varid))
            call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsTimeBounds', 
     *             NF90_INT, combo_dim, wset%gps%tbounds_varid))
            if (wset%is_dwr) call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsStatusFlags', 
     *             NF90_BYTE, wset%gps%time_dimid, wset%gps%flags_varid))
            call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsLatitude', 
     *             NF90_FLOAT, wset%gps%time_dimid, wset%gps%latitude_varid))
            call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsLongitude', 
     *             NF90_FLOAT, wset%gps%time_dimid, wset%gps%longitude_varid))
            call nc_call_func(nf90_def_var(wset%gps%grpid, 'gpsSourceIndex', 
     *             NF90_INT, wset%gps%time_dimid, wset%gps%src_varid))

            call meta_add_attribute(time_atts, 'bounds', 'gpsTimeBounds', META_char_type)
            call wc5_add_attributes(wset%gps%grpid, wset%gps%time_varid, time_atts)
            call wc5_add_attributes(wset%gps%grpid, wset%gps%tbounds_varid, time_bounds_atts)

            if (wset%is_dwr) then
              call nc_assign_attributes(wset%gps%grpid, wset%gps%flags_varid, 'gps data flags', null_att, 'BYTE')
              gps_masks = (/ 1, 2, 4, 8 /)
              call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%flags_varid, 'flag_masks', gps_masks))
              call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%flags_varid, 'flag_meanings', 
     *          'module_ok new_fix figure_of_merit hf_transmission_error'))
              gps_comment = 'Good GPS positions will have module_ok and new_fix set, i.e. gpsStatusFlags=3. '//
     *          'All other positions should be ignored. Note that some records with gpsStatusFlags=3 may also '//
     *          'be erroneous, these values are not subject to any quality control.'
              call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%flags_varid,'comment',TRIM(gps_comment)))
            end if

            call nc_assign_attributes(wset%gps%grpid, wset%gps%latitude_varid, 'buoy latitude',
     *        'degrees_north', 'REAL', minv=-90.0, maxv=90.0, nodc='LATITUDE')
            call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%latitude_varid,'cell_methods','gpsTime: point'))
            call nc_assign_attributes(wset%gps%grpid, wset%gps%longitude_varid, 'buoy longitude',
     *        'degrees_east', 'REAL', minv=-180.0, maxv=180.0, nodc='LONGITUDE')
            call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%longitude_varid,'cell_methods','gpsTime: point'))
            if (wset%is_dwr) then
              call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%latitude_varid,
     *          'ancillary_variables','gpsStatusFlags'))
              call nc_call_func(nf90_put_att(wset%gps%grpid, wset%gps%longitude_varid,
     *          'ancillary_variables','gpsStatusFlags'))
            end if
            call nc_assign_attributes(wset%gps%grpid, wset%gps%src_varid, 'source file index', null_att, 
     *        'INT')
          end if

c--  Define the acm group

          if (BTEST(groups, WC5_acm_bit) .and. wset%acm%time_count .gt. 0) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'acm', wset%acm%grpid))
            else
              wset%acm%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%acm%grpid, 'acmTime', NF90_UNLIMITED, wset%acm%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%acm%grpid, 'acmTime', wset%acm%time_count, wset%acm%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%acm%time_dimid/)
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmTime', NF90_INT, wset%acm%time_dimid, 
     *             wset%acm%time_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmTimeBounds', 
     *             NF90_INT, combo_dim, wset%acm%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmFlagPrimary', 
     *             NF90_BYTE, wset%acm%time_dimid, wset%acm%flags_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmFlagSecondary', 
     *             NF90_BYTE, wset%acm%time_dimid, wset%acm%flags2_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSpeed', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%speed_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSpeedStdDev', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%speedstd_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmDirection', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%dir_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmDirectionStdDev', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%dirstd_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSignalStrength1', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%rssi1_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSignalStrength2', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%rssi2_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSignalStrength3', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%rssi3_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmStatus', 
     *             NF90_BYTE, wset%acm%time_dimid, wset%acm%status_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSeaSurfaceTemperature', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%csst_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmVerticalSpeed', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%vert_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmVerticalSpeedStdDev', 
     *             NF90_FLOAT, wset%acm%time_dimid, wset%acm%vertstd_varid))
            call nc_call_func(nf90_def_var(wset%acm%grpid, 'acmSourceIndex', 
     *             NF90_INT, wset%acm%time_dimid, wset%acm%src_varid))

            ancil = 'acmFlagPrimary acmFlagSecondary'

            call meta_add_attribute(time_atts, 'bounds', 'acmTimeBounds', META_char_type)
            call wc5_add_attributes(wset%acm%grpid, wset%acm%time_varid, time_atts)
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%time_varid, 'long_name', 'UTC sample start time'))
            call wc5_add_attributes(wset%acm%grpid, wset%acm%tbounds_varid, time_bounds_atts)

            call ioc_assign_flag1_atts(wset%acm%grpid, wset%acm%flags_varid, 'acm')
            call ioc_assign_flag2_atts(wset%acm%grpid, wset%acm%flags2_varid, 'acm')

            call nc_assign_attributes(wset%acm%grpid, wset%acm%speed_varid, 'current speed at 0.75m depth', 
     *        'meter/second', 'REAL', 'sea_water_speed', TRIM(coord_str), 0.0, 3.0, TRIM(ancil), 'CURRENT SPEED')
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%speed_varid,'cell_methods','acmTime: mean'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%speedstd_varid, 'current speed standard deviation', 
     *        'meter/second', 'REAL', null_att, TRIM(coord_str), 0.0, 0.255, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%speedstd_varid,'cell_methods',
     *        'acmTime: standard_deviation'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%dir_varid, 'current direction at 0.75m depth', 'degreeT',
     *        'REAL', 'direction_of_sea_water_velocity', TRIM(coord_str), 0.0, 360.0, TRIM(ancil), 'CURRENT DIRECTION')
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%dir_varid,'cell_methods','acmTime: mean'))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%dir_varid, 'additional_processing', 
     *       'Magnetic declination - INT(metaDeclination) - added to buoy-returned direction to shift '//
     *       'to degrees true.'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%dirstd_varid, 'current direction standard deviation', 
     *        'degree', 'REAL', null_att, TRIM(coord_str), 0.0, 25.5, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%dirstd_varid,'cell_methods',
     *        'acmTime: standard_deviation'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%rssi1_varid, 
     *        'received signal strength (dBr), transducer 1', null_att, 'REAL')
            call nc_assign_attributes(wset%acm%grpid, wset%acm%rssi2_varid, 
     *        'received signal strength (dBr), transducer 2', null_att, 'REAL')
            call nc_assign_attributes(wset%acm%grpid, wset%acm%rssi3_varid, 
     *        'received signal strength (dBr), transducer 3', null_att, 'REAL')

            call nc_assign_attributes(wset%acm%grpid, wset%acm%status_varid, 'ACM status', null_att, 'BYTE')
            acm_masks = (/ 1, 2, 4 /)
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%status_varid, 'flag_masks', acm_masks))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%status_varid, 'flag_meanings', 
     *        'busy adc_okay invalid_matrix'))

            call nc_assign_attributes(wset%acm%grpid, wset%acm%csst_varid, 'water temperature', 'Celsius', 'REAL', 
     *        'sea_surface_temperature', TRIM(coord_str), -10.0, 50.0, TRIM(ancil), 'SEA SURFACE TEMPERATURE')
            call nc_assign_attributes(wset%acm%grpid, wset%acm%vert_varid, 'vertical current speed at 0.75m depth', 
     *        'meter/second', 'REAL', 'upward_sea_water_velocity', TRIM(coord_str), -0.127, 0.127, TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%vert_varid,'cell_methods','acmTime: mean'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%vertstd_varid, 
     *        'vertical current speed standard deviation', 'meter/second', 'REAL', null_att, TRIM(coord_str), 0.0, 0.255, 
     *        TRIM(ancil))
            call nc_call_func(nf90_put_att(wset%acm%grpid, wset%acm%vertstd_varid,'cell_methods',
     *        'acmTime: standard_deviation'))
            call nc_assign_attributes(wset%acm%grpid, wset%acm%src_varid, 'source file index', null_att, 'INT')
          end if

c--  Define the dwr group

          if (BTEST(groups, WC5_dwr_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'dwr', wset%dwr%grpid))
            else
              wset%dwr%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%dwr%grpid, 'dwrTime', NF90_UNLIMITED, wset%dwr%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%dwr%grpid, 'dwrTime', wset%dwr%time_count, 
     *               wset%dwr%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%dwr%time_dimid/)
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrTime', NF90_INT, wset%dwr%time_dimid, 
     *             wset%dwr%time_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrTimeBounds', 
     *             NF90_INT, combo_dim, wset%dwr%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrSourceIndex', 
     *             NF90_INT, wset%dwr%time_dimid, wset%dwr%src_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrBatteryLevel', 
     *             NF90_INT, wset%dwr%time_dimid, wset%dwr%batt_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrZAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr%time_dimid, wset%dwr%za_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrXAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr%time_dimid, wset%dwr%xa_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrYAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr%time_dimid, wset%dwr%ya_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrOrientation', 
     *             NF90_FLOAT, wset%dwr%time_dimid, wset%dwr%orient_varid))
            call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrInclination', 
     *             NF90_FLOAT, wset%dwr%time_dimid, wset%dwr%inclin_varid))

            if (wset%is_mk3 .or. wset%is_dwrg) then
              call nc_call_func(nf90_def_var(wset%dwr%grpid, 'dwrBatteryWeeksOfLife', 
     *               NF90_INT, wset%dwr%time_dimid, wset%dwr%wol_varid))
              call nc_assign_attributes(wset%dwr%grpid, wset%dwr%wol_varid, 'battery weeks of life', 'weeks', 
     *               'INT') 
            end if

            call meta_add_attribute(time_atts, 'bounds', 'dwrTimeBounds', META_char_type)
            call wc5_add_attributes(wset%dwr%grpid, wset%dwr%time_varid, time_atts)
            call wc5_add_attributes(wset%dwr%grpid, wset%dwr%tbounds_varid, time_bounds_atts)

            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%src_varid, 'source file index', null_att, 'INT') 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%batt_varid, 'battery level', null_att, 'INT',
     *             minv=0.0, maxv=7.0) 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%za_off_varid, 'vertical accelerometer offset',
     *             'm/s^2', 'REAL') 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%xa_off_varid, 'x-axis accelerometer offset',
     *             'm/s^2', 'REAL') 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%ya_off_varid, 'y-axis accelerometer offset',
     *             'm/s^2', 'REAL') 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%orient_varid, 'buoy orientation',
     *             'degrees', 'REAL', 'platform_orientation') 
            call nc_assign_attributes(wset%dwr%grpid, wset%dwr%inclin_varid, 'magnetic inclination',
     *             'degrees', 'REAL') 
          end if

c--  Define the dwr4 group

          if (BTEST(groups, WC5_dwr4_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'dwr4', wset%dwr4%grpid))
            else
              wset%dwr4%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%dwr4%grpid, 'dwr4Time', NF90_UNLIMITED, wset%dwr4%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%dwr4%grpid, 'dwr4Time', wset%dwr4%time_count, 
     *               wset%dwr4%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%dwr4%time_dimid/)
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4Time', NF90_INT, wset%dwr4%time_dimid, 
     *        wset%dwr4%time_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4TimeBounds', 
     *             NF90_INT, combo_dim, wset%dwr4%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4Uptime', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%uptime_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4BatteryWeeksOfLife', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%wol_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4EnergyUsed', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%enerused_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4SourceIndex', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%src_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4EnergyToBoostcaps', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%eboost_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4ZAccelMaxCount', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%za_max_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4XAccelMaxCount', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%xa_max_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4YAccelMaxCount', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%ya_max_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4PitchMaxCount', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%pitch_max_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4RollMaxCount', 
     *             NF90_INT, wset%dwr4%time_dimid, wset%dwr4%roll_max_varid))

            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4HatchTemperature', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%hatchtemp_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4BatteryVoltage', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%voltage_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4ZAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%za_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4XAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%xa_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4YAccelerometerOffset', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%ya_off_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4OrientMean', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%orient_mean_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4OrientStdDev', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%orient_dev_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4InclinMean', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%inclin_mean_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4InclinStdDev', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%inclin_dev_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4MagFieldLengthMean', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%maglength_mean_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4MagFieldLengthStdDev', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%maglength_dev_varid))
            call nc_call_func(nf90_def_var(wset%dwr4%grpid, 'dwr4SensorTemperature', 
     *             NF90_FLOAT, wset%dwr4%time_dimid, wset%dwr4%sensortemp_varid))

            call meta_add_attribute(time_atts, 'bounds', 'dwr4TimeBounds', META_char_type)
            call wc5_add_attributes(wset%dwr4%grpid, wset%dwr4%time_varid, time_atts)
            call nc_call_func(nf90_put_att(wset%dwr4%grpid, wset%dwr4%time_varid, 'long_name', 
     *        'spectrum UTC start time'))
            call wc5_add_attributes(wset%dwr4%grpid, wset%dwr4%tbounds_varid, time_bounds_atts)
            call nc_call_func(nf90_put_att(wset%dwr4%grpid, wset%dwr4%tbounds_varid, 'long_name', 
     *        'spectrum time bounds'))
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%uptime_varid, 'uptime', 'second', 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%wol_varid, 'battery weeks of life', 'weeks', 'INT') 
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%enerused_varid, 'energy used from batteries', 
     *        'joule', 'INT') 
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%eboost_varid, 
     *        'energy to boostcaps', 'joule', 'INT') 
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%src_varid, 'source file index', null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%za_max_varid, 
     *        'vertical accelerometer threshold count', null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%xa_max_varid, 
     *        'x-axis accelerometer threshold count', null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%ya_max_varid, 
     *        'y-axis accelerometer threshold count', null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%pitch_max_varid, 'pitch threshold count', 
     *        null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%roll_max_varid, 'roll threshold count', 
     *        null_att, 'INT')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%hatchtemp_varid, 'hatch electronics temperature', 
     *        'Celsius', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%voltage_varid, 'battery voltage', 
     *        'volt', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%za_off_varid, 'vertical accelerometer offset', 
     *        'meter^2/second', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%xa_off_varid, 'x-axis accelerometer offset', 
     *        'meter^2/second', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%ya_off_varid, 'y-axis accelerometer offset', 
     *        'meter^2/second', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%orient_mean_varid, 'average orientation', 
     *        'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%orient_dev_varid, 
     *        'orientation standard deviation', 'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%inclin_mean_varid, 'average inclination', 
     *        'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%inclin_dev_varid, 
     *        'inclination standard deviation', 'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%maglength_mean_varid, 
     *        'average magnetic field length', 'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%maglength_dev_varid, 
     *        'magnetic field length standard deviation', 'degree', 'REAL')
            call nc_assign_attributes(wset%dwr4%grpid, wset%dwr4%sensortemp_varid, 'accelerometer temperature', 
     *        'Celsius', 'REAL')
          end if

c--  Define the upcross group

          if (BTEST(groups, WC5_upcross_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'upcross', wset%upcross%grpid))
            else
              wset%upcross%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%upcross%grpid, 'upcrossTime', NF90_UNLIMITED,wset%upcross%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%upcross%grpid, 'upcrossTime', wset%upcross%time_count, 
     *          wset%upcross%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%upcross%time_dimid/)
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTime', NF90_INT, wset%upcross%time_dimid, 
     *             wset%upcross%time_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTimeBounds', 
     *             NF90_INT, combo_dim, wset%upcross%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossFlagPrimary', 
     *             NF90_BYTE, wset%upcross%time_dimid, wset%upcross%flags_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossFlagSecondary', 
     *             NF90_BYTE, wset%upcross%time_dimid, wset%upcross%flags2_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossSourceIndex', 
     *             NF90_INT, wset%upcross%time_dimid, wset%upcross%src_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossCrestCount', 
     *             NF90_INT, wset%upcross%time_dimid, wset%upcross%ncrests_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossWaveCount', 
     *             NF90_INT, wset%upcross%time_dimid, wset%upcross%nwaves_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHavg', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Havg_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHmax', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Hmax_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHsRMS', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Hrms_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHofTmax', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Htmax_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTofHmax', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Thmax_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTz', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Tavg_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTmax', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Tmax_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossBandwidth', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%bwidth_varid))
            call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossCoverage', 
     *             NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%cov_varid))


            ancil = 'upcrossFlagPrimary upcrossFlagSecondary'

            call meta_add_attribute(time_atts, 'bounds', 'upcrossTimeBounds', META_char_type)
            call wc5_add_attributes(wset%upcross%grpid, wset%upcross%time_varid, time_atts)
            call nc_call_func(nf90_put_att(wset%upcross%grpid, wset%upcross%time_varid, 'long_name', 
     *        'nominal UTC start time'))
            call nc_call_func(nf90_put_att(wset%upcross%grpid, wset%upcross%time_varid, 'comment', 
     *        'Refer to upcrossTimeBounds and upcrossCoverage for timing details.'))
            call wc5_add_attributes(wset%upcross%grpid, wset%upcross%tbounds_varid, time_bounds_atts)
            call nc_call_func(nf90_put_att(wset%upcross%grpid, wset%upcross%tbounds_varid, 'comment', 
     *        'Per Datawell the first and last 256 points (100s) of samples are excluded from the analysis.'))

            call ioc_assign_flag1_atts(wset%upcross%grpid, wset%upcross%flags_varid, 'upcross')
            call ioc_assign_flag2_atts(wset%upcross%grpid, wset%upcross%flags2_varid, 'upcross')

            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%src_varid, 'source file index',null_att, 'INT') 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%ncrests_varid, 'number of crests', 
     *        null_att, 'INT') 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%nwaves_varid, 'number of waves', 
     *        null_att, 'INT') 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Havg_varid, 'mean upcross wave height',
     *        'meter', 'REAL', 'sea_surface_wave_mean_height', TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Hmax_varid, 'maximum upcross wave height', 
     *        'meter', 'REAL', 'sea_surface_wave_maximum_height', TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Hrms_varid, 
     *        'Hs estimate from upcross RMS wave height', 'meter', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, 
     *        TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Htmax_varid, 'height of Tmax wave', 
     *        'meter', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Thmax_varid, 'period of Hmax wave',
     *        'second', 'REAL', 'sea_surface_wave_period_of_highest_wave', TRIM(coord_str), 1.0, 40.0, TRIM(ancil))
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Tavg_varid, 'average upcross wave period', 
     *        'second', 'REAL', 'sea_surface_wave_zero_upcrossing_period', TRIM(coord_str), 1.0, 40.0, TRIM(ancil))
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Tmax_varid, 'maximum upcross wave period', 
     *        'second', 'REAL', 'sea_surface_wave_maximum_period', TRIM(coord_str), 1.0, 40.0, TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%bwidth_varid, 'bandwidth of upcross waves', 
     *        null_att, 'REAL', null_att, TRIM(coord_str), 0.0, 1.0, TRIM(ancil)) 
            call nc_assign_attributes(wset%upcross%grpid, wset%upcross%cov_varid, 'sample coverage', 'percent', 'REAL')
            call nc_call_func(nf90_put_att(wset%upcross%grpid, wset%upcross%cov_varid, 'comment', 
     *        'Percentage of the 1800s of displacement values which are included in the upcross analysis.'))
       
            if (MAXVAL(wset%upcross%H10) .ne. WC5_real_fill) then
              call nc_call_func(nf90_def_dim(wset%source%grpid, 'upcrossQuantileLength', 
     *               WC5_upcross_quantile_length, quant_dimid))
              combo_dim = (/quant_dimid, wset%upcross%time_dimid/)

              call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHtenth', 
     *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%H10_varid))
              call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHthird', 
     *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%H3_varid))
              call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTofHtenth', 
     *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Th10_varid))
              call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTofHthird', 
     *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Th3_varid))
              call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossQuantileHeight', 
     *               NF90_FLOAT, combo_dim, wset%upcross%Hquant_varid))

              call nc_assign_attributes(wset%upcross%grpid, wset%upcross%H10_varid, 'H1/10 upcross wave height',
     *          'meter', 'REAL', 'sea_surface_wave_mean_height_of_highest_tenth',TRIM(coord_str),0.0,20.0,TRIM(ancil))
              call nc_assign_attributes(wset%upcross%grpid, wset%upcross%H3_varid, 'H1/3 upcross wave height',
     *        'meter', 'REAL', 'sea_surface_wave_significant_height', TRIM(coord_str), 0.0, 20.0, TRIM(ancil))
              call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Th10_varid, 'wave period of H1/10', 'second', 
     *          'REAL', 'sea_surface_wave_mean_period_of_highest_tenth', TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
              call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Th3_varid, 'wave period of H1/3',
     *          'second', 'REAL', 'sea_surface_wave_significant_period', TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
              call nc_assign_attributes(wset%upcross%grpid,wset%upcross%Hquant_varid,'quantiles of upcross wave height',
     *          'meter', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
              comment = 'The wave height which p percent of the upcross waves fall below. The 23 p values are '//
     *          'in the set {1, 3, 5, 10, 15, ..., 90, 95, 97, 99}.'
              call nc_call_func(nf90_put_att(wset%upcross%grpid, wset%upcross%Hquant_varid, 'comment', comment))
            end if

c- Values from 0xF2A msg, upcross period quantiles; not yet generated by DWR4 firmware
c           if (MAXVAL(wset%upcross%T10) .ne. WC5_real_fill) then
c             call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTenth', 
c    *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%T10_varid))
c             call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossTthird', 
c    *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%T3_varid))
c             call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHofTtenth', 
c    *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Ht10_varid))
c             call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossHofTthird', 
c    *               NF90_FLOAT, wset%upcross%time_dimid, wset%upcross%Ht3_varid))
c             call nc_call_func(nf90_def_var(wset%upcross%grpid, 'upcrossPeriodQuantile', 
c    *               NF90_FLOAT, combo_dim, wset%upcross%Tquant_varid))
c             call nc_assign_attributes(wset%upcross%grpid, wset%upcross%T10_varid, 'T1/10 upcross wave period', 
c    *          'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
c             call nc_assign_attributes(wset%upcross%grpid, wset%upcross%T3_varid, 'T1/3 upcross wave period',
c    *          'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
c             call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Th10_varid, 'wave height of T1/10',
c    *          'meter', 'REAL', null_att, TRIM(coord_str), 0.0, 20.0, TRIM(ancil))
c             call nc_assign_attributes(wset%upcross%grpid, wset%upcross%Th3_varid, 'wave height of T1/3',
c    *          'meter', 'REAL', null_att, TRIM(coord_str), 0.0, 20.0, TRIM(ancil))
c             call nc_assign_attributes(wset%upcross%grpid,wset%upcross%Hquant_varid,'quantiles of upcross wave period',
c    *          'second', 'REAL', null_att, TRIM(coord_str), 0.0, 40.0, TRIM(ancil))
c           end if

          end if

c--  Define the sync group

          if (BTEST(groups, WC5_sync_bit)) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'sync', wset%sync%grpid))
            else
              wset%sync%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%sync%grpid, 'syncTime', NF90_UNLIMITED, wset%sync%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%sync%grpid, 'syncTime', wset%sync%time_count, 
     *          wset%sync%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%sync%time_dimid/)
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncTime', NF90_INT, wset%sync%time_dimid, 
     *             wset%sync%time_varid))
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncTimeBounds', 
     *             NF90_INT, combo_dim, wset%sync%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncSourceIndex', 
     *             NF90_INT, wset%sync%time_dimid, wset%sync%src_varid))
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncSegmentCount', 
     *             NF90_INT, wset%sync%time_dimid, wset%sync%segcnt_varid))
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncSegmentsUsed', 
     *             NF90_INT, wset%sync%time_dimid, wset%sync%segs_varid))
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncSamples', 
     *             NF90_INT, wset%sync%time_dimid, wset%sync%samples_varid))

            call nc_call_func(nf90_def_dim(wset%sync%grpid, 'syncDispLength', 18, length_dimid))
            combo_dim = (/length_dimid, wset%sync%time_dimid/)
            call nc_call_func(nf90_def_var(wset%sync%grpid, 'syncLastDisplacements', 
     *             NF90_CHAR, combo_dim, wset%sync%disp_varid))

            call meta_add_attribute(time_atts, 'bounds', 'syncTimeBounds', META_char_type)
            call wc5_add_attributes(wset%sync%grpid, wset%sync%time_varid, time_atts)
            call nc_call_func(nf90_put_att(wset%sync%grpid, wset%sync%time_varid, 'long_name', 
     *        'spectrum UTC start time'))
            call wc5_add_attributes(wset%sync%grpid, wset%sync%tbounds_varid, time_bounds_atts)
            call nc_call_func(nf90_put_att(wset%sync%grpid, wset%sync%tbounds_varid, 'long_name', 
     *        'spectrum time bounds'))
            call nc_assign_attributes(wset%sync%grpid, wset%sync%src_varid, 'source file index', null_att, 'INT') 
            call nc_assign_attributes(wset%sync%grpid, wset%sync%segcnt_varid, 'segment count', null_att, 'INT') 
            call nc_assign_attributes(wset%sync%grpid, wset%sync%segs_varid, 'segments used', null_att, 'INT') 
            call nc_assign_attributes(wset%sync%grpid, wset%sync%samples_varid, 'number of samples', null_att, 'INT') 
            call nc_assign_attributes(wset%sync%grpid, wset%sync%disp_varid, 'final hex displacements', 
     *             null_att, 'CHAR') 
          end if

c--  Define the cat4 group

          if (BTEST(groups, WC5_cat4_bit) .and. wset%cat4%time_count .gt. 0) then
            if (wset%use_groups) then
              call nc_call_func(nf90_def_grp(wset%ncid, 'cat4', wset%cat4%grpid))
            else
              wset%cat4%grpid = wset%ncid
            end if
            if (wset%unlimited) then
              call nc_call_func(nf90_def_dim(wset%cat4%grpid, 'cat4Time', NF90_UNLIMITED, wset%cat4%time_dimid))
            else
              call nc_call_func(nf90_def_dim(wset%cat4%grpid, 'cat4Time', wset%cat4%time_count, wset%cat4%time_dimid))
            end if

            combo_dim = (/bounds_dimid, wset%cat4%time_dimid/)
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4Time', NF90_INT, wset%cat4%time_dimid, 
     *             wset%cat4%time_varid))
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4TimeBounds',
     *             NF90_INT, combo_dim, wset%cat4%tbounds_varid))
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4FlagPrimary', 
     *             NF90_BYTE, wset%cat4%time_dimid, wset%cat4%flags_varid))
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4FlagSecondary', 
     *             NF90_BYTE, wset%cat4%time_dimid, wset%cat4%flags2_varid))
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4AirTemperature', NF90_FLOAT, wset%cat4%time_dimid, 
     *             wset%cat4%airt_varid))
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4SourceIndex', 
     *             NF90_INT, wset%cat4%time_dimid, wset%cat4%src_varid))

            ancil = 'cat4FlagPrimary cat4FlagSecondary'

            call meta_add_attribute(time_atts, 'bounds', 'cat4TimeBounds', META_char_type)
            call wc5_add_attributes(wset%cat4%grpid, wset%cat4%time_varid, time_atts)
            call wc5_add_attributes(wset%cat4%grpid, wset%cat4%tbounds_varid, time_bounds_atts)

            call ioc_assign_flag1_atts(wset%cat4%grpid, wset%cat4%flags_varid, 'cat4')
            call ioc_assign_flag2_atts(wset%cat4%grpid, wset%cat4%flags2_varid, 'cat4')

            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%airt_varid, 'air temperature at 2m above sea surface',
     *        'Celsius','REAL', 'air_temperature', TRIM(coord_str), -20.0, 70.0, TRIM(ancil), 'AIR TEMPERATURE')
            call nc_call_func(nf90_put_att(wset%cat4%grpid, wset%cat4%airt_varid,'cell_methods','cat4Time: point'))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%src_varid, 'source file index', null_att, 'INT')

            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4StatusFlags', NF90_BYTE, wset%cat4%time_dimid, 
     *             wset%cat4%status_varid))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%status_varid, 'cat4 status flags', null_att, 'BYTE')
            cat4_masks = (/ 1, 2, 4, 8 /)
            call nc_call_func(nf90_put_att(wset%cat4%grpid, wset%cat4%status_varid, 'flag_masks', cat4_masks))
            call nc_call_func(nf90_put_att(wset%cat4%grpid, wset%cat4%status_varid, 'flag_meanings',
     *        'error busy evaporation_detected solar_induced_uncertainty'))


            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4WhiteTemperature', NF90_FLOAT, wset%cat4%time_dimid, 
     *             wset%cat4%white_varid))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%white_varid, 'white temperature', 'Celsius', 'REAL') 
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4BlackTemperature', NF90_FLOAT, wset%cat4%time_dimid, 
     *             wset%cat4%black_varid))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%black_varid, 'black temperature', 'Celsius', 'REAL') 
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4MetalTemperature', NF90_FLOAT, wset%cat4%time_dimid, 
     *             wset%cat4%metal_varid))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%metal_varid, 'metal temperature', 'Celsius', 'REAL') 
            call nc_call_func(nf90_def_var(wset%cat4%grpid, 'cat4GroovedTemperature', NF90_FLOAT, wset%cat4%time_dimid, 
     *             wset%cat4%grooved_varid))
            call nc_assign_attributes(wset%cat4%grpid, wset%cat4%grooved_varid, 'grooved temperature', 'Celsius', 'REAL') 
          end if

          if (end_defs) call nc_call_func(nf90_enddef(wset%ncid))
        end subroutine


c-- WC5_INIT_LOGICALS_FROM_GAUGE -----------------------------------------------
c  Sets the wset logicals based on the given archive_info gauge_index.
c-------------------------------------------------------------------------------
        subroutine wc5_init_logicals_from_gauge(wset, gauge)
          integer               gauge
          type(wc5_dataset)     wset

          wset%is_dwr = .false.
          wset%is_mk3 = .false.
          wset%is_mk4 = .false.
          wset%is_dwrg = .false.
          wset%is_net_model = .false.
          wset%is_2d_model = .false.
          wset%is_non_dw = .false.
          wset%is_1Hz_data = .false.
          wset%is_nearshore = .false.
          wset%is_directional = .true.
          if (gauge .eq. 5 .or. gauge .eq. 6) then
            wset%is_dwr = .true.
          else if (gauge .eq. 23) then
            wset%is_dwr = .true.
            wset%is_dwrg = .true.
          else if (gauge .eq. 19) then
            wset%is_dwr = .true.
            wset%is_mk3 = .true.
          else if (gauge .eq. 24) then
            wset%is_mk4 = .true.
          else if (gauge .eq. 21) then
            wset%is_non_dw = .true.
            wset%is_net_model = .true.
          else if (gauge .eq. 22) then
            wset%is_non_dw = .true.
            wset%is_2d_model = .true.
          else
            wset%is_non_dw = .true.
            wset%is_directional = .false.
          end if
          return
        end subroutine


c-- WC5_INIT_GROUPS_FROM_GAUGE -------------------------------------------------
c  Sets the wset groups based on the given archive_info gauge_index.
c-------------------------------------------------------------------------------
        subroutine wc5_init_groups_from_gauge(wset, gauge)
          integer               gauge
          type(wc5_dataset)     wset

          if (gauge .eq. 5) then
            wset%groups = WC5_mk1_groups
          else if (gauge .eq. 6) then
            wset%groups = WC5_mk2_groups
          else if (gauge .eq. 19) then
            wset%groups = WC5_mk3_groups
          else if (gauge .eq. 23) then
            wset%groups = WC5_mk2_groups
          else if (gauge .eq. 24) then
            wset%groups = WC5_mk4_groups
          else if (gauge .eq. 21 .or. gauge .eq. 22) then
            wset%groups = WC5_model_groups
          else
            wset%groups = WC5_non_dw_groups
          end if
          return
        end subroutine


c-- WC5_ADD_ATTRIBUTES ---------------------------------------------------------
c  Assigns attributes to the given grpid/varid within a set
c-------------------------------------------------------------------------------
        subroutine wc5_add_attributes(grpid, varid, atts)
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


c-- WC5_ADD_GLOBAL_ATTRIBUTES --------------------------------------------------
c  Assigns global attributes to the set
c-------------------------------------------------------------------------------
        subroutine wc5_add_global_attributes(grpid, atts)
          integer               grpid
          type(meta_attribute_list)   atts

          call wc5_add_attributes(grpid, NF90_GLOBAL, atts)
          return
        end subroutine


c-- WC5_DEF_METADATA_ARRAY_VARIABLES -------------------------------------------
c  Defines metadata dimensions and array variables
c-------------------------------------------------------------------------------
        subroutine wc5_def_metadata_array_variables(grpid, metavar, metadim)
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
                    call wc5_add_attributes(grpid, metavar%varid(j), metavar%vatts(j))
                  else if (metavar%vtype(j) .eq. META_real_type) then
                    call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(j)), NF90_FLOAT, tmp_dimid, 
     *                metavar%varid(j)))
                    call wc5_add_attributes(grpid, metavar%varid(j), metavar%vatts(j))
                  end if
                end if
              end do
            end if
          end do
          call wc5_def_metadata_variables(grpid, metavar)
        end subroutine


c-- WC5_DEF_METADATA_VARIABLES -------------------------------------------------
c  Defines metadata variables and attributes in the root group
c-------------------------------------------------------------------------------
        subroutine wc5_def_metadata_variables(grpid, metavar)
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
                  call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_CHAR, char_dimid, metavar%varid(i)))
                else
                  call nc_call_func(nf90_def_var(grpid, TRIM(metavar%vname(i)), NF90_CHAR, varid=metavar%varid(i)))
                end if
              end if
              call wc5_add_attributes(grpid, metavar%varid(i), metavar%vatts(i))
            end if
          end do
          return
        end subroutine


c-- WC5_PUT_METADATA_VARIABLES -------------------------------------------------
c  Fills metadata variables in the root group
c-------------------------------------------------------------------------------
        subroutine wc5_put_metadata_variables(grpid, metavar)
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


c-- WC5_FILL_NCFILE ------------------------------------------------------------
c  Writes the contents of a wc5_dataset to a .nc file
c-------------------------------------------------------------------------------
        subroutine wc5_fill_ncfile(wset, errcode, cgroups) 
          integer::                errcode, i, groups
          integer, optional::      cgroups
          character*100  ncname 
          type(wc5_dataset), intent(in)::   wset

          if (wset%groups .eq. 0) then
            write(6,'(a)') 'WC5_FILL_NCFILE error: groups not set.'
            call exit(1)
          end if

          if (PRESENT(cgroups)) then
            groups = cgroups
          else
            groups = wset%groups
          end if

          if (BTEST(groups, WC5_source_bit) .and. wset%source%file_count .gt. 0) 
     *      call wc5_put_source_vars(wset, wset, 1, wset%source%file_count) 
          if (BTEST(groups, WC5_wave_bit) .and. wset%wave%time_count .gt. 0) 
     *      call wc5_put_wave_vars(wset, wset, 1, wset%wave%time_count) 
          if (BTEST(groups, WC5_spectra_bit) .and. wset%wave%freq_count .gt. 0) 
     *      call wc5_put_sp_vars(wset, wset, 1, wset%wave%time_count) 
          if (BTEST(groups, WC5_xyz_bit) .and. wset%xyz%rec_count .gt. 0) 
     *      call wc5_put_xyz_vars(wset, wset, 1, wset%xyz%rec_count)
          if (BTEST(groups, WC5_gps_bit) .and. wset%gps%time_count .gt. 0) 
     *      call wc5_put_gps_vars(wset, wset, 1, wset%gps%time_count)
          if (BTEST(groups, WC5_sst_bit) .and. wset%sst%time_count .gt. 0) 
     *      call wc5_put_sst_vars(wset, wset, 1, wset%sst%time_count)
          if (BTEST(groups, WC5_acm_bit) .and. wset%acm%time_count .gt. 0) 
     *      call wc5_put_acm_vars(wset, wset, 1, wset%acm%time_count)
          if (BTEST(groups, WC5_dwr4_bit) .and. wset%dwr4%time_count .gt. 0) 
     *      call wc5_put_dwr4_vars(wset, wset, 1, wset%dwr4%time_count)
          if (BTEST(groups, WC5_dwr_bit) .and. wset%dwr%time_count .gt. 0) 
     *      call wc5_put_dwr_vars(wset, wset, 1, wset%dwr%time_count)
          if (BTEST(groups, WC5_upcross_bit) .and. wset%upcross%time_count .gt. 0) 
     *      call wc5_put_upcross_vars(wset, wset, 1, wset%upcross%time_count)
          if (BTEST(groups, WC5_sync_bit) .and. wset%sync%time_count .gt. 0) 
     *      call wc5_put_sync_vars(wset, wset, 1, wset%sync%time_count)
          if (BTEST(groups, WC5_cat4_bit) .and. wset%cat4%time_count .gt. 0) 
     *      call wc5_put_cat4_vars(wset, wset, 1, wset%cat4%time_count)
          call nc_call_func(nf90_close(wset%ncid))

        end subroutine


c-- WC5_UPDATE_NCFILE ----------------------------------------------------------
c  Updates the contents of an unlimited-dimensioned .nc file. The first dataset 
c  (wset) comes from opening the file and loading the group and dimension info.
c-------------------------------------------------------------------------------
        subroutine wc5_update_ncfile(ncname, aset, errcode) 
          integer::      cloc, errcode, i, j, k, sloc, start_idx, xcount(1), xstart(1)
          logical        found
          byte, allocatable::     xflag1(:), xflag2(:)
          character               ncname*100, schar
          type(wc5_dataset)       aset, wset

          wset%classic = aset%classic
          wset%is_xy_only = aset%is_xy_only
          call wc5_quick_load(ncname, wset, 255, errcode, .true.)

c         call nc_call_func(nf90_open(ncname, NF90_WRITE, wset%ncid))
c         call wc5_initialize_set(wset)
c         call wc5_read_ids(wset, errcode)
c         call wc5_read_dimensions(wset, errcode)
c         call wc5_allocate_set(wset)
c         call wc5_load_coord_vars(wset, 255, errcode)

          if (BTEST(aset%groups, WC5_source_bit) .and. aset%source%file_count .gt. 0) then
            if (aset%is_xy_only) then
              sloc = 1
              schar = wset%source%file_name(1,sloc)
              do while (schar .ne. WC5_char_fill .and. schar .ne. '_' .and. sloc .lt. 100000)
                sloc = sloc + 1
                schar = wset%source%file_name(1,sloc)
              end do
            else
              sloc = wset%source%file_count + 1
            end if
            call wc5_put_source_vars(wset, aset, sloc, aset%source%file_count) 
          end if

          if (.not. aset%is_xy_only .and. aset%wave%time_count .gt. 0) then
            aset%wave%src_index = sloc
            if (wset%wave%time_count .eq. 0) then
              call wc5_put_wave_vars(wset, aset, 1, aset%wave%time_count)
              if (wset%wave%freq_count .gt. 1) call wc5_put_sp_vars(wset, aset, 1, aset%wave%time_count) 
            else
              call nc_find_time_index(wset%wave%times, wset%wave%time_count, 
     *          timestamp_to_date(aset%wave%times(1)), 4, cloc, found)
              if (.not. found .and. aset%wave%times(1) .lt. wset%wave%times(wset%wave%time_count)) then
                write(6,*) 'Overlapping wave data; omitting.'
              else 
                if (.not. found) cloc = wset%wave%time_count+1
                call wc5_put_wave_vars(wset, aset, cloc, aset%wave%time_count) 
                if (wset%wave%freq_count .gt. 1) call wc5_put_sp_vars(wset, aset, cloc, aset%wave%time_count) 
              end if
            end if
          end if

          if (aset%xyz%rec_count .gt. 0) then
            aset%xyz%src_index = sloc
            if (wset%xyz%rec_count .eq. 0) then
              call wc5_put_xyz_vars(wset, aset, 1, aset%xyz%rec_count)
            else
              start_idx = NINT(secs_diff(timestamp_to_date(wset%xyz%start_time), 
     *          timestamp_to_date(aset%xyz%start_time)) * DBLE(wset%xyz%sample_rate)) + 1
              if (start_idx .gt. wset%xyz%rec_count+1) then
                xstart(1) = wset%xyz%rec_count+1
                xcount(1) = start_idx - xstart(1)
                allocate(xflag1(xcount(1)), xflag2(xcount(1)))
                xflag1 = 9
                xflag2 = 0
                call nc_call_func(nf90_put_var(wset%xyz%grpid, wset%xyz%flags_varid, 
     *            xflag1, xstart, xcount))
                call nc_call_func(nf90_put_var(wset%xyz%grpid, wset%xyz%flags2_varid, 
     *            xflag2, xstart, xcount))
                deallocate(xflag1, xflag2)
              end if
              if (start_idx .ge. 1) call wc5_put_xyz_vars(wset, aset, start_idx, aset%xyz%rec_count) 

c             call nc_find_sample_index(wset%xyz%start_time, wset%xyz%sample_rate, wset%xyz%rec_count, 
c    *          aset%xyz%start_time, 4, cloc, found)
c             if (found .or. aset%xyz%start_time .le. 
c    *          wset%xyz%start_time+INT((wset%xyz%rec_count-1)/wset%xyz%sample_rate)) then
c               write(6,*) 'Overlapping xyz data; omitting.'
c             else
c               call wc5_put_xyz_vars(wset, aset, wset%xyz%rec_count+1, aset%xyz%rec_count) 
c             end if

            end if
          end if

          if (.not. aset%is_xy_only .and. aset%sst%time_count .gt. 0) then
            aset%sst%src_index = sloc
            if (wset%sst%time_count .eq. 0) then
              call wc5_put_sst_vars(wset, aset, 1, aset%sst%time_count)
            else
              call nc_find_time_index(wset%sst%times, wset%sst%time_count, 
     *          timestamp_to_date(aset%sst%times(1)), 4, cloc, found)
              if (.not. found .and. aset%sst%times(1) .lt. wset%sst%times(wset%sst%time_count)) then
                write(6,*) 'Overlapping sst data; omitting.'
              else if (found) then
                call wc5_put_sst_vars(wset, aset, cloc, aset%sst%time_count) 
              else
                call wc5_put_sst_vars(wset, aset, wset%sst%time_count+1, aset%sst%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%gps%time_count .gt. 0) then
            aset%gps%src_index = sloc
            if (wset%gps%time_count .eq. 0) then
              call wc5_put_gps_vars(wset, aset, 1, aset%gps%time_count)
            else
              call nc_find_time_index(wset%gps%times, wset%gps%time_count, 
     *          timestamp_to_date(aset%gps%times(1)), 4, cloc, found)
              if (.not. found .and. aset%gps%times(1) .lt. wset%gps%times(wset%gps%time_count)) then
                write(6,*) 'Overlapping gps data; omitting.'
              else if (found) then
                call wc5_put_gps_vars(wset, aset, cloc, aset%gps%time_count) 
              else
                call wc5_put_gps_vars(wset, aset, wset%gps%time_count+1, aset%gps%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%acm%time_count .gt. 0) then
            aset%acm%src_index = sloc
            if (wset%acm%time_count .eq. 0) then
              call wc5_put_acm_vars(wset, aset, 1, aset%acm%time_count)
            else
              call nc_find_time_index(wset%acm%times, wset%acm%time_count, 
     *          timestamp_to_date(aset%acm%times(1)), 4, cloc, found)
              if (.not. found .and. aset%acm%times(1) .lt. wset%acm%times(wset%acm%time_count)) then
                write(6,*) 'Overlapping acm data; omitting.'
              else if (found) then
                call wc5_put_acm_vars(wset, aset, cloc, aset%acm%time_count) 
              else
                call wc5_put_acm_vars(wset, aset, wset%acm%time_count+1, aset%acm%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%dwr%time_count .gt. 0) then
            aset%dwr%src_index = sloc
            if (wset%dwr%time_count .eq. 0) then
              call wc5_put_dwr_vars(wset, aset, 1, aset%dwr%time_count)
            else
              call nc_find_time_index(wset%dwr%times, wset%dwr%time_count, 
     *          timestamp_to_date(aset%dwr%times(1)), 4, cloc, found)
              if (.not. found .and. aset%dwr%times(1) .lt. wset%dwr%times(wset%dwr%time_count)) then
                write(6,*) 'Overlapping dwr data; omitting.'
              else if (found) then
                call wc5_put_dwr_vars(wset, aset, cloc, aset%dwr%time_count) 
              else
                call wc5_put_dwr_vars(wset, aset, wset%dwr%time_count+1, aset%dwr%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%dwr4%time_count .gt. 0) then
            aset%dwr4%src_index = sloc
            if (wset%dwr4%time_count .eq. 0) then
              call wc5_put_dwr4_vars(wset, aset, 1, aset%dwr4%time_count)
            else
              call nc_find_time_index(wset%dwr4%times, wset%dwr4%time_count, 
     *          timestamp_to_date(aset%dwr4%times(1)), 4, cloc, found)
              if (.not. found .and. aset%dwr4%times(1) .lt. wset%dwr4%times(wset%dwr4%time_count)) then
                write(6,*) 'Overlapping dwr4 data; omitting.'
              else if (found) then
                call wc5_put_dwr4_vars(wset, aset, cloc, aset%dwr4%time_count) 
              else
                call wc5_put_dwr4_vars(wset, aset, wset%dwr4%time_count+1, aset%dwr4%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%upcross%time_count .gt. 0) then
            aset%upcross%src_index = sloc
            if (wset%upcross%time_count .eq. 0) then
              call wc5_put_upcross_vars(wset, aset, 1, aset%upcross%time_count)
            else
              call nc_find_time_index(wset%upcross%times, wset%upcross%time_count, 
     *          timestamp_to_date(aset%upcross%times(1)), 4, cloc, found)
              if (.not. found .and. aset%upcross%times(1) .lt. wset%upcross%times(wset%upcross%time_count)) then
                write(6,*) 'Overlapping upcross data; omitting.'
              else if (found) then
                call wc5_put_upcross_vars(wset, aset, cloc, aset%upcross%time_count) 
              else
                call wc5_put_upcross_vars(wset, aset, wset%upcross%time_count+1, aset%upcross%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%sync%time_count .gt. 0) then
            aset%sync%src_index = sloc
            if (wset%sync%time_count .eq. 0) then
              call wc5_put_sync_vars(wset, aset, 1, aset%sync%time_count)
            else
              call nc_find_time_index(wset%sync%times, wset%sync%time_count, 
     *          timestamp_to_date(aset%sync%times(1)), 4, cloc, found)
              if (.not. found .and. aset%sync%times(1) .lt. wset%sync%times(wset%sync%time_count)) then
                write(6,*) 'Overlapping sync data; omitting.'
              else if (found) then
                call wc5_put_sync_vars(wset, aset, cloc, aset%sync%time_count) 
              else
                call wc5_put_sync_vars(wset, aset, wset%sync%time_count+1, aset%sync%time_count) 
              end if
            end if
          end if

          if (.not. aset%is_xy_only .and. aset%cat4%time_count .gt. 0) then
            aset%cat4%src_index = sloc
            if (wset%cat4%time_count .eq. 0) then
              call wc5_put_cat4_vars(wset, aset, 1, aset%cat4%time_count)
            else
              call nc_find_time_index(wset%cat4%times, wset%cat4%time_count, 
     *          timestamp_to_date(aset%cat4%times(1)), 4, cloc, found)
              if (.not. found .and. aset%cat4%times(1) .lt. wset%cat4%times(wset%cat4%time_count)) then
                write(6,*) 'Overlapping cat4 data; omitting.'
              else if (found) then
                call wc5_put_cat4_vars(wset, aset, cloc, aset%cat4%time_count) 
              else
                call wc5_put_cat4_vars(wset, aset, wset%cat4%time_count+1, aset%cat4%time_count) 
              end if
            end if
          end if

          call nc_call_func(nf90_close(wset%ncid))
        end subroutine


c-- WC5_MATCH_SP_LAYOUT --------------------------------------------------------
c  Returns the index of the sp group that matches the given freq layout, or -1
c-------------------------------------------------------------------------------
        logical function wc5_match_sp_layout(aset, bset) 
          type(wc5_dataset)     aset, bset

          wc5_match_sp_layout = .false.
          if (aset%wave%freq_count .eq. bset%wave%freq_count .and.  MAXVAL(aset%wave%freqs-bset%wave%freqs) .eq. 0.0) 
     *      wc5_match_sp_layout = .true.
          return
        end function


c-- WC5_GET_WAVE_VARS ----------------------------------------------------------
c  Reads variable values from the wave group
c-------------------------------------------------------------------------------
        subroutine wc5_get_wave_vars(wset, start_tindex, trecs) 
          integer::      count_dim(1), count_dims(2), i
          integer::      start_tindex, start_dim(1), start_dims(2), trecs
          type(wc5_dataset)::   wset

            start_dim(1) = start_tindex
            count_dim(1) = trecs

            start_dims(1) = 1
            start_dims(2) = start_tindex
            count_dims(1) = WC5_modelinput_length
            count_dims(2) = trecs

            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%flags_varid, wset%wave%flags, start_dim, 
     *             count_dim))
            wset%wave%pub_tf = MERGE(.true., .false., wset%wave%flags .eq. 1)
            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%flags2_varid, wset%wave%flags2, start_dim, 
     *             count_dim))

            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%hs_varid, wset%wave%hs, start_dim, count_dim))
            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%tp_varid, wset%wave%tp, start_dim, count_dim))
            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%ta_varid, wset%wave%ta, start_dim, count_dim))
            if (wset%is_directional) call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%dp_varid, wset%wave%dp, 
     *        start_dim, count_dim))
            if (wset%is_dwr .or. wset%is_mk4 .or. wset%source%file_count .gt. 0) then
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%src_varid, wset%wave%src_index, start_dim, 
     *          count_dim))
            else if (wset%is_net_model) then
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%modelinput_varid, wset%wave%model_input, 
     *          start_dims, count_dims))
            end if
            if (wset%is_nearshore) then
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%dm_varid, wset%wave%dm,start_dim,count_dim))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%sxy_varid, wset%wave%sxy,start_dim,count_dim))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%sxx_varid, wset%wave%sxx,start_dim,count_dim))
            end if
          end subroutine


c-- WC5_GET_SP_VARS ------------------------------------------------------------
c  Reads variable values from the sp group
c-------------------------------------------------------------------------------
        subroutine wc5_get_sp_vars(wset, start_tindex, trecs, start_findex, frecs) 
          integer::      count_dim(1), count_dims(2), count_dims_2d(3), frecs, i
          integer::      start_findex, start_tindex, start_dim(1), start_dims(2), start_dims_2d(3), trecs
          type(wc5_dataset)::   wset

            start_dim(1) = start_findex
            count_dim(1) = frecs

            start_dims(1) = start_findex
            start_dims(2) = start_tindex
            count_dims(1) = frecs
            count_dims(2) = trecs

            start_dims_2d(1) = 1
            start_dims_2d(2) = start_findex
            start_dims_2d(3) = start_tindex
            count_dims_2d(1) = wset%wave%dir_count
            count_dims_2d(2) = frecs
            count_dims_2d(3) = trecs

            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%fflags_varid, wset%wave%fflags, start_dim, 
     *             count_dim))
            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%fflags2_varid, wset%wave%fflags2, start_dim, 
     *             count_dim))

            call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a0_varid, wset%wave%a0, start_dims, count_dims))
            if (wset%is_directional) then
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%mdir_varid,wset%wave%mdir, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a1_varid, wset%wave%a1, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%b1_varid, wset%wave%b1, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%a2_varid, wset%wave%a2, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%b2_varid, wset%wave%b2, start_dims, count_dims))
            end if
            if (wset%is_dwr .or. wset%is_mk4) then
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%check_varid, 
     *             wset%wave%check, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%spread_varid, wset%wave%dspread, start_dims, 
     *             count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%m2_varid, wset%wave%m2, start_dims, count_dims))
              call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%n2_varid, wset%wave%n2, start_dims, count_dims))
            end if
            if (wset%is_net_model) call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%check_varid, 
     *             wset%wave%check, start_dims, count_dims))
            if (wset%is_2d_model) call nc_call_func(nf90_get_var(wset%wave%grpid, wset%wave%dirspec_varid, 
     *             wset%wave%dirspec, start_dims_2d, count_dims_2d))
          end subroutine


c-- WC5_EQUAL_WAVE -------------------------------------------------------------
c  Checks if two wave groups are equal
c-------------------------------------------------------------------------------
        logical function wc5_equal_waves(wave1, wave2)
          type(wc5_wave_group)::   wave1, wave2

           wc5_equal_waves = .true.
           if (wave1%time_count .ne. wave2%time_count) then
             wc5_equal_waves = .false.
             return
           end if

           if (MAXVAL(ABS(wave1%times-wave2%times)) .ne. 0) wc5_equal_waves = .false.
           if (MAXVAL(ABS(wave1%dp-wave2%dp)) .ne. 0) wc5_equal_waves = .false.
           if (MAXVAL(ABS(wave1%hs-wave2%hs)) .ne. 0) wc5_equal_waves = .false.
           if (MAXVAL(ABS(wave1%tp-wave2%tp)) .ne. 0) wc5_equal_waves = .false.
           if (MAXVAL(ABS(wave1%ta-wave2%ta)) .ne. 0) wc5_equal_waves = .false.
           return

        end function


c-- WC5_EQUAL_SP ---------------------------------------------------------------
c  Checks if two sp groups are equal
c-------------------------------------------------------------------------------
        logical function wc5_equal_sp(sp1, sp2)
          type(wc5_wave_group)::   sp1, sp2

           wc5_equal_sp = .true.
           if (sp1%time_count .ne. sp2%time_count .or. sp1%freq_count .ne. sp2%freq_count) then
             wc5_equal_sp = .false.
             return
           end if

           if (MAXVAL(ABS(sp1%times-sp2%times)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%freqs-sp2%freqs)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%bw-sp2%bw)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%a0-sp2%a0)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%mdir-sp2%mdir)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%check-sp2%check)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%a1-sp2%a1)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%a2-sp2%a2)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%b1-sp2%b1)) .ne. 0) wc5_equal_sp = .false.
           if (MAXVAL(ABS(sp1%b2-sp2%b2)) .ne. 0) wc5_equal_sp = .false.
           return

        end function


c-- WC5_EMPTY_WAVE_VARS --------------------------------------------------------
c  Sets all wave vars to missing, for null records
c-------------------------------------------------------------------------------
        subroutine wc5_empty_wave_vars(wset)
          type(wc5_dataset)::   wset

          wset%wave%flags = 9
          wset%wave%flags2 = 0
          wset%wave%hs = WC5_real_fill
          wset%wave%tp = WC5_real_fill
          wset%wave%dp = WC5_real_fill
          wset%wave%ta = WC5_real_fill
          wset%wave%model_input = WC5_char_fill
          wset%wave%dm = WC5_real_fill
          wset%wave%sxy = WC5_real_fill
          wset%wave%sxx = WC5_real_fill
          wset%wave%src_index = WC5_int_fill
          wset%wave%psdmax = WC5_real_fill
          wset%wave%spreadmax = WC5_real_fill
          wset%wave%tint = WC5_real_fill
          wset%wave%tener = WC5_real_fill
          wset%wave%tm13 = WC5_real_fill
          wset%wave%tcrest = WC5_real_fill
          wset%wave%iqp = WC5_real_fill
          wset%wave%tz = WC5_real_fill
        end subroutine


c-- WC5_PUT_WAVE_VARS ----------------------------------------------------------
c  Fills variable values in the wave group
c-------------------------------------------------------------------------------
        subroutine wc5_put_wave_vars(bset, aset, start_index, count) 
          integer::      count, grpid, i, j, pad_secs, start_index
          integer::      tcount(1), tstart(1), tstarts(2), tcounts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset)::   aset, bset	!* base set and add set

          if (.not. wc5_equal_waves(bset%wave,aset%wave)) then
            if (start_index .le. bset%wave%time_count) call wc5_get_wave_vars(bset, 
     *          start_index, bset%wave%time_count-start_index+1)

            i = start_index
            do while (i .le. bset%wave%time_count .and. i - start_index .lt. count)
              j = i - start_index + 1
              if (aset%wave%hs(j) .eq. WC5_real_fill) aset%wave%hs(j) = bset%wave%hs(j)
              if (aset%wave%tp(j) .eq. WC5_real_fill) aset%wave%tp(j) = bset%wave%tp(j)
              if (aset%wave%dp(j) .eq. WC5_real_fill) aset%wave%dp(j) = bset%wave%dp(j)
              if (aset%wave%ta(j) .eq. WC5_real_fill) aset%wave%ta(j) = bset%wave%ta(j)
              if (aset%wave%src_index(j) .eq. WC5_int_fill) aset%wave%src_index(j) = bset%wave%src_index(j)
              i = i + 1
            end do
          end if

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%wave%time_count))
          if (aset%wave%tbounds_bot_offset .ne. WC5_int_fill .and. 
     *        aset%wave%tbounds_top_offset .ne. WC5_int_fill) then
            tbounds(1,:) = aset%wave%times - aset%wave%tbounds_bot_offset
            tbounds(2,:) = aset%wave%times + aset%wave%tbounds_top_offset
          else if (aset%is_net_model) then
            tbounds(1,:) = aset%wave%times - 1800
            tbounds(2,:) = aset%wave%times + 1800
          else if (aset%is_2d_model) then
            pad_secs = (aset%wave%times(2) - aset%wave%times(1)) / 2
            tbounds(1,:) = aset%wave%times - pad_secs
            tbounds(2,:) = aset%wave%times + pad_secs
          else if (aset%is_ndbc_obs) then
            tbounds(1,:) = aset%wave%times
            tbounds(2,:) = aset%wave%times + 2400
          else if (aset%is_mk4) then
            tbounds(1,:) = aset%wave%times
            tbounds(2,:) = aset%wave%times + 1800
          else if (aset%is_1Hz_data) then
            tbounds(1,:) = aset%wave%times
            tbounds(2,:) = aset%wave%times + 2048
          else
            tbounds(1,:) = aset%wave%times
            tbounds(2,:) = aset%wave%times + 1600
          end if

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%time_varid, aset%wave%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%flags_varid, aset%wave%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%flags2_varid, aset%wave%flags2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%hs_varid, aset%wave%hs, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tp_varid, aset%wave%tp, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%ta_varid, aset%wave%ta, tstart, tcount))
            if (bset%is_directional) call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%dp_varid, aset%wave%dp, tstart, 
     *        tcount))
            if (bset%is_net_model) then
              tcounts(1) = WC5_modelinput_length
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%modelinput_varid, aset%wave%model_input, 
     *          tstarts, tcounts))
            end if
            if (bset%is_nearshore) then
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%dm_varid, aset%wave%dm, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%sxy_varid, aset%wave%sxy, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%sxx_varid, aset%wave%sxx, tstart, tcount))
            end if
            if (BTEST(bset%groups, WC5_source_bit) .and. MAXVAL(aset%wave%src_index) .gt. 0) 
     *        call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%src_varid, aset%wave%src_index, tstart, tcount))
            if (bset%is_dwr .or. bset%is_mk4) then
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%psdmax_varid, aset%wave%psdmax, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tz_varid, aset%wave%tz, tstart, tcount))
            end if
            if (bset%is_mk4) then
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%sprdmax_varid, aset%wave%spreadmax, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tint_varid, aset%wave%tint, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tener_varid, aset%wave%tener, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tm13_varid, aset%wave%tm13, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%tcrest_varid, aset%wave%tcrest, tstart, tcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%iqp_varid, aset%wave%iqp, tstart, tcount))
            end if
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_EMPTY_SPECTRAL_VARS ----------------------------------------------------
c  Sets all spectral vars to missing, full null records
c-------------------------------------------------------------------------------
        subroutine wc5_empty_spectral_vars(wset)
          type(wc5_dataset)::   wset

          wset%wave%a0 = WC5_real_fill
          wset%wave%mdir = WC5_real_fill
          wset%wave%a1 = WC5_real_fill
          wset%wave%b1 = WC5_real_fill
          wset%wave%a2 = WC5_real_fill
          wset%wave%b2 = WC5_real_fill
          wset%wave%check = WC5_real_fill
          wset%wave%dspread = WC5_real_fill
          wset%wave%m2 = WC5_real_fill
          wset%wave%n2 = WC5_real_fill
          if (ALLOCATED(wset%wave%dirspec)) wset%wave%dirspec = WC5_real_fill
        end subroutine


c-- WC5_IS_COMPLETE_SPECTRUM ---------------------------------------------------
c  Checks if the spectrum at the given index has values for all spectral fields.
c-------------------------------------------------------------------------------
        logical function wc5_is_complete_spectrum(wset, idx)
          integer               idx
          type(wc5_dataset)::   wset

          wc5_is_complete_spectrum = .true.
          if (MAXVAL(wset%wave%a0(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
          if (MAXVAL(wset%wave%mdir(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
          if (MAXVAL(wset%wave%a1(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
          if (MAXVAL(wset%wave%b1(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
          if (MAXVAL(wset%wave%a2(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
          if (MAXVAL(wset%wave%b2(:,idx)) .eq. WC5_real_fill) wc5_is_complete_spectrum = .false.
        end function


c-- WC5_PUT_SP_VARS ------------------------------------------------------------
c  Fills frequency-dimensioned variable values in the wave group
c-------------------------------------------------------------------------------
        subroutine wc5_put_sp_vars(bset, aset, start_index, count) 
          integer::      count, dstart(1), dcount(1), dcounts(2), fcount(1), fstart(1), fstarts(2), fcounts(2), i, j
          integer::      start_index, tcount(1), tcounts(2), tcounts_2d(3), tstart(1), tstarts(2), tstarts_2d(3)
          real           dwidth
          real,allocatable::    fbounds(:,:), dbounds(:,:)
          type(wc5_dataset)::   aset, bset	!* base set and add set

          if (.not. wc5_equal_sp(bset%wave,aset%wave)) then
            if (start_index .le. bset%wave%time_count) call wc5_get_sp_vars(bset, start_index, 
     *          bset%wave%time_count-start_index+1, 1, bset%wave%freq_count)

            i = start_index
            do while (i .le. bset%wave%time_count .and. i - start_index .lt. count)
              j = i - start_index + 1
              if (aset%wave%a0(1,j) .eq. WC5_real_fill) aset%wave%a0(:,j) = bset%wave%a0(:,j)
              if (MAXVAL(aset%wave%mdir(:,j)) .eq. WC5_real_fill) aset%wave%mdir(:,j) = bset%wave%mdir(:,j)
              if (MAXVAL(aset%wave%a1(:,j)) .eq. WC5_real_fill) aset%wave%a1(:,j) = bset%wave%a1(:,j)
              if (MAXVAL(aset%wave%b1(:,j)) .eq. WC5_real_fill) aset%wave%b1(:,j) = bset%wave%b1(:,j)
              if (MAXVAL(aset%wave%a2(:,j)) .eq. WC5_real_fill) aset%wave%a2(:,j) = bset%wave%a2(:,j)
              if (MAXVAL(aset%wave%b2(:,j)) .eq. WC5_real_fill) aset%wave%b2(:,j) = bset%wave%b2(:,j)
              if (MAXVAL(aset%wave%check(:,j)) .eq. WC5_real_fill) aset%wave%check(:,j) = bset%wave%check(:,j)
              if (MAXVAL(aset%wave%dspread(:,j)) .eq. WC5_real_fill) aset%wave%dspread(:,j) = bset%wave%dspread(:,j)
              if (MAXVAL(aset%wave%m2(:,j)) .eq. WC5_real_fill) aset%wave%m2(:,j) = bset%wave%m2(:,j)
              if (MAXVAL(aset%wave%n2(:,j)) .eq. WC5_real_fill) aset%wave%n2(:,j) = bset%wave%n2(:,j)
              i = i + 1
            end do
          end if

          tcount(1) = count
          tcounts(1) = bset%wave%freq_count
          tcounts(2) = count
          tcounts_2d(1) = bset%wave%dir_count
          tcounts_2d(2) = bset%wave%freq_count
          tcounts_2d(3) = count

          tstart(1) = start_index
          tstarts(1) = 1
          tstarts(2) = start_index
          tstarts_2d(1) = 1
          tstarts_2d(2) = 1
          tstarts_2d(3) = start_index

          fcount(1) = bset%wave%freq_count
          fcounts(1) = 2
          fcounts(2) = bset%wave%freq_count

          fstart(1) = 1
          fstarts(1) = 1
          fstarts(2) = 1

          dstart(1) = 1
          dcount(1) = bset%wave%dir_count
          dcounts(1) = 2
          dcounts(2) = bset%wave%dir_count

          if (MAXVAL(bset%wave%bw) .eq. WC5_real_fill) bset%wave%bw = aset%wave%bw
          if (MAXVAL(bset%wave%fflags) .eq. WC5_byte_fill) bset%wave%fflags = aset%wave%fflags
          if (MAXVAL(bset%wave%fflags2) .eq. WC5_byte_fill) bset%wave%fflags2 = aset%wave%fflags2
          allocate (fbounds(2,bset%wave%freq_count))
          do i = 1, bset%wave%freq_count
            fbounds(1,i) = bset%wave%freqs(i) - bset%wave%bw(i)/2.0
            fbounds(2,i) = bset%wave%freqs(i) + bset%wave%bw(i)/2.0
          end do
c--  Check for oversize top band (e.g. OWI's 23rd band)
          if (fbounds(1,bset%wave%freq_count) .lt. bset%wave%freqs(bset%wave%freq_count-1)) then
            fbounds(1,bset%wave%freq_count) = fbounds(2,bset%wave%freq_count-1)
            fbounds(2,bset%wave%freq_count) = fbounds(1,bset%wave%freq_count) + bset%wave%bw(bset%wave%freq_count)
          end if

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%freq_varid, aset%wave%freqs, fstart, fcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%fbounds_varid, fbounds, fstarts, fcounts))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%bw_varid, aset%wave%bw, fstart, fcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%fflags_varid, aset%wave%fflags, fstart, fcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%fflags2_varid, aset%wave%fflags2, fstart, fcount))
            call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%a0_varid, aset%wave%a0, tstarts, tcounts))
            if (bset%is_directional) then
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%mdir_varid, aset%wave%mdir, tstarts, tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%a1_varid, aset%wave%a1, tstarts, tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%b1_varid, aset%wave%b1, tstarts, tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%a2_varid, aset%wave%a2, tstarts, tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%b2_varid, aset%wave%b2, tstarts, tcounts))
            end if
            if (bset%is_dwr .or. bset%is_mk4 .or. bset%is_net_model)
     *        call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%check_varid, aset%wave%check, tstarts, tcounts))
            if (bset%is_dwr .or. bset%is_mk4) then
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%spread_varid, aset%wave%dspread, tstarts,
     *          tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%m2_varid, aset%wave%m2, tstarts, tcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%n2_varid, aset%wave%n2, tstarts, tcounts))
            end if
            if (bset%is_2d_model) then
              allocate (dbounds(2,bset%wave%dir_count))
              dwidth = (bset%wave%dirs(2) - bset%wave%dirs(1)) / 2.0
              do i = 1, bset%wave%dir_count
                dbounds(1,i) = bset%wave%dirs(i) - dwidth
                dbounds(2,i) = bset%wave%dirs(i) + dwidth
              end do
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%dir_varid, aset%wave%dirs, dstart, dcount))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%dbounds_varid, dbounds, fstarts, dcounts))
              call nc_call_func(nf90_put_var(bset%wave%grpid, bset%wave%dirspec_varid, aset%wave%dirspec, tstarts_2d, 
     *          tcounts_2d))
              deallocate(dbounds)
            end if
          end if
          deallocate(fbounds)
        end subroutine


c-- WC5_PUT_XYZ_VARS -----------------------------------------------------------
c  Fills variable values in the xyz group
c-------------------------------------------------------------------------------
        subroutine wc5_put_xyz_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1)
          type(wc5_dataset), intent(in)::  aset, bset	!* base set and add set

          write(6,*) 'Adding to xyz. Index, count: ', start_index, count
          tcount(1) = count
          tstart(1) = start_index

          if (count .gt. 0 .and. start_index .gt. 0) then
            if (start_index .eq. 1) then
              call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%stime_varid, aset%xyz%start_time))
              call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%srate_varid, aset%xyz%sample_rate))
              call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%delay_varid, aset%xyz%filter_delay))
            end if
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%flags_varid, aset%xyz%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%flags2_varid, aset%xyz%flags2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%zdisp_varid, aset%xyz%zdisp, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%xdisp_varid, aset%xyz%xdisp, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%ydisp_varid, aset%xyz%ydisp, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%xyz%grpid, bset%xyz%src_varid, aset%xyz%src_index, tstart, tcount))
          end if
        end subroutine


c-- WC5_PUT_GPS_VARS -----------------------------------------------------------
c  Fills variable values in the gps group
c-------------------------------------------------------------------------------
        subroutine wc5_put_gps_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset	!* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          call wc5_mask_values(aset%gps%flags, count, aset%gps%mod_ok, aset%gps%new_fix, aset%gps%merit, 
     *      aset%gps%hf_errors)

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%gps%time_count))
          if (bset%is_mk4 .or. aset%is_mk4) then
            tbounds(1,:) = aset%gps%times - 30
            tbounds(2,:) = aset%gps%times + 30
          else if (bset%is_dwrg .or. aset%is_dwrg) then
            tbounds(1,:) = aset%gps%times - 5
            tbounds(2,:) = aset%gps%times + 0
          else
            tbounds(1,:) = aset%gps%times - 60
            tbounds(2,:) = aset%gps%times + 240
          end if

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%time_varid, aset%gps%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%tbounds_varid, tbounds, tstarts, tcounts))
            if (bset%is_dwr)
     *        call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%flags_varid, aset%gps%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%latitude_varid, aset%gps%latitude, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%longitude_varid,aset%gps%longitude, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%gps%grpid, bset%gps%src_varid, aset%gps%src_index, tstart, tcount))
          end if
          deallocate (tbounds)
        end subroutine


c-- WC5_MASK_VALUES ------------------------------------------------------------
c   Assigns the correct bit mask value to the given variable
c-------------------------------------------------------------------------------
        subroutine wc5_mask_values(vals, cnt, bit1, bit2, bit3, bit4, bit5, bit6, bit7)
          integer             cnt, i
          byte                vals(*)
          logical, optional:: bit1(*), bit2(*), bit3(*), bit4(*), bit5(*), bit6(*), bit7(*)

          vals(1:cnt) = 0

          do i = 1, cnt
            if (PRESENT(bit1)) then
               if (bit1(i)) vals(i) = 1
            end if
            if (PRESENT(bit2)) then
               if (bit2(i)) vals(i) = vals(i) + 2
            end if
            if (PRESENT(bit3)) then
               if (bit3(i)) vals(i) = vals(i) + 4
            end if
            if (PRESENT(bit4)) then
               if (bit4(i)) vals(i) = vals(i) + 8
            end if
            if (PRESENT(bit5)) then
               if (bit5(i)) vals(i) = vals(i) + 16
            end if
            if (PRESENT(bit6)) then
               if (bit6(i)) vals(i) = vals(i) + 32
            end if
            if (PRESENT(bit7)) then
               if (bit7(i)) vals(i) = vals(i) + 64
            end if
          end do

          return
        end subroutine


c-- WC5_PUT_SOURCE_VARS --------------------------------------------------------
c  Fills variable values in the source group
c-------------------------------------------------------------------------------
        subroutine wc5_put_source_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcounts(2), tstarts(2)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          if (count .gt. 0 .and. start_index .gt. 0) then
            tstarts(1) = 1
            tcounts(1) = WC5_filename_length
            tstarts(2) = start_index
            tcounts(2) = count
            call nc_call_func(nf90_put_var(bset%source%grpid, 
     *        bset%source%name_varid, aset%source%file_name, tstarts, tcounts))
          end if
        end subroutine


c-- WC5_PUT_SST_VARS -----------------------------------------------------------
c  Fills variable values in the sst group
c-------------------------------------------------------------------------------
        subroutine wc5_put_sst_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%sst%time_count))
          if (aset%is_dwrg .or. bset%is_dwrg) then
            tbounds(1,:) = aset%sst%times - 30
            tbounds(2,:) = aset%sst%times + 30
          else if (aset%is_CAT4 .or. bset%is_CAT4) then
            tbounds(1,:) = aset%sst%times - NINT(66.7)
            tbounds(2,:) = aset%sst%times
          else
            tbounds(1,:) = aset%sst%times - NINT(66.7/2.0)
            tbounds(2,:) = aset%sst%times + NINT(66.7/2.0)
          end if

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%time_varid, aset%sst%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%flags_varid, aset%sst%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%flags2_varid, aset%sst%flags2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%sstC_varid, aset%sst%sstC, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%src_varid, aset%sst%src_index, tstart, tcount))
            if (bset%is_dwr)
     *        call nc_call_func(nf90_put_var(bset%sst%grpid, bset%sst%reftemp_varid, aset%sst%reftemp, tstart, tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_ACM_VARS -----------------------------------------------------------
c  Fills variable values in the acm group
c-------------------------------------------------------------------------------
        subroutine wc5_put_acm_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%acm%time_count))
          tbounds(1,:) = aset%acm%times
          tbounds(2,:) = aset%acm%times + 64

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%time_varid, aset%acm%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%flags_varid, aset%acm%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%flags2_varid, aset%acm%flags2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%speed_varid, aset%acm%speed, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%speedstd_varid, aset%acm%speedstd, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%dir_varid, aset%acm%dir, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%dirstd_varid, aset%acm%dirstd, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%rssi1_varid, aset%acm%rssi1, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%rssi2_varid, aset%acm%rssi2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%rssi3_varid, aset%acm%rssi3, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%csst_varid, aset%acm%csst, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%status_varid, aset%acm%cstatus, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%vert_varid, aset%acm%vert, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%vertstd_varid, aset%acm%vertstd, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%acm%grpid, bset%acm%src_varid, aset%acm%src_index, tstart, tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_DWR_VARS -----------------------------------------------------------
c  Fills variable values in the dwr group
c-------------------------------------------------------------------------------
        subroutine wc5_put_dwr_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%dwr%time_count))
          tbounds(1,:) = aset%dwr%times
          tbounds(2,:) = aset%dwr%times + 1800

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%time_varid, aset%dwr%times,tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%src_varid, aset%dwr%src_index, tstart, 
     *        tcount))
            if (bset%is_mk3 .or. bset%is_dwrg) 
     *        call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%wol_varid, aset%dwr%wol, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%batt_varid, aset%dwr%batt, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%za_off_varid, aset%dwr%za_off, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%xa_off_varid, aset%dwr%xa_off, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%ya_off_varid, aset%dwr%ya_off, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%orient_varid, aset%dwr%orient, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr%grpid, bset%dwr%inclin_varid, aset%dwr%inclin, tstart, 
     *        tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_DWR4_VARS ----------------------------------------------------------
c  Fills variable values in the dwr4 group
c-------------------------------------------------------------------------------
        subroutine wc5_put_dwr4_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%dwr4%time_count))
          tbounds(1,:) = aset%dwr4%times
          tbounds(2,:) = aset%dwr4%times + 1800

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%time_varid, aset%dwr4%times,tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%uptime_varid, aset%dwr4%uptime, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%wol_varid, aset%dwr4%wol, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%enerused_varid, aset%dwr4%enerused, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%eboost_varid, aset%dwr4%eboost, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%hatchtemp_varid, aset%dwr4%hatchtemp, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%voltage_varid, aset%dwr4%voltage, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%za_off_varid, aset%dwr4%za_off, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%za_max_varid, aset%dwr4%za_max, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%xa_off_varid, aset%dwr4%xa_off, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%xa_max_varid, aset%dwr4%xa_max, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%ya_off_varid, aset%dwr4%ya_off, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%ya_max_varid, aset%dwr4%ya_max, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%orient_mean_varid, 
     *        aset%dwr4%orient_mean, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%orient_dev_varid, 
     *        aset%dwr4%orient_dev, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%inclin_mean_varid, 
     *        aset%dwr4%inclin_mean, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%inclin_dev_varid, 
     *        aset%dwr4%inclin_dev, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%maglength_mean_varid, 
     *        aset%dwr4%maglength_mean, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%maglength_dev_varid, 
     *        aset%dwr4%maglength_dev, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%pitch_max_varid, 
     *        aset%dwr4%pitch_max, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%roll_max_varid, 
     *        aset%dwr4%roll_max, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%sensortemp_varid, 
     *        aset%dwr4%sensortemp, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%dwr4%grpid, bset%dwr4%src_varid, 
     *        aset%dwr4%src_index, tstart, tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_UPCROSS_VARS -------------------------------------------------------
c  Fills variable values in the upcross group
c-------------------------------------------------------------------------------
        subroutine wc5_put_upcross_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%upcross%time_count))
          tbounds(1,:) = aset%upcross%times + 0
          tbounds(2,:) = aset%upcross%times + 1800

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%time_varid, aset%upcross%times, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%flags_varid, aset%upcross%flags, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%flags2_varid, aset%upcross%flags2, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%ncrests_varid, aset%upcross%num_crests, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%nwaves_varid, aset%upcross%num_waves, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Havg_varid, aset%upcross%Havg, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Hmax_varid, aset%upcross%Hmax, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Hrms_varid, aset%upcross%Hrms, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Htmax_varid, aset%upcross%H_at_Tmax, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Thmax_varid, aset%upcross%T_at_Hmax, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Tavg_varid, aset%upcross%Tavg, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Tmax_varid, aset%upcross%Tmax, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%bwidth_varid, aset%upcross%bandwidth, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%cov_varid, aset%upcross%coverage, 
     *        tstart, tcount))
            call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%src_varid, aset%upcross%src_index, 
     *        tstart, tcount))

            tcounts(1) = WC5_upcross_quantile_length
            tcounts(2) = count
            tstarts(1) = 1
            tstarts(2) = start_index

            if (MAXVAL(aset%upcross%H10) .ne. WC5_real_fill) then
              call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%H10_varid, aset%upcross%H10, 
     *          tstart, tcount))
              call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%H3_varid, aset%upcross%H3,tstart,tcount))
              call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Th10_varid, aset%upcross%TofH10, 
     *          tstart, tcount))
              call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Th3_varid, aset%upcross%TofH3, 
     *          tstart, tcount))
              call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Hquant_varid, aset%upcross%H_quantile, 
     *          tstarts, tcounts))
            end if

c           if (MAXVAL(aset%upcross%T10) .ne. WC5_real_fill) then
c             call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%T10_varid, aset%upcross%T10, 
c    *          tstart, tcount))
c             call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%T3_varid, aset%upcross%T3,tstart,tcount))
c             call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Ht10_varid, aset%upcross%HofT10, 
c    *          tstart, tcount))
c             call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Ht3_varid, aset%upcross%HofT3, 
c    *          tstart, tcount))
c             call nc_call_func(nf90_put_var(bset%upcross%grpid, bset%upcross%Tquant_varid, aset%upcross%T_quantile, 
c    *          tstarts, tcounts))
c           end if

          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_SYNC_VARS -----------------------------------------------------------
c  Fills variable values in the sync group
c-------------------------------------------------------------------------------
        subroutine wc5_put_sync_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tstarts(1) = 1
          tcounts(2) = count
          tstarts(2) = start_index

          allocate (tbounds(2,aset%sync%time_count))
          tbounds(1,:) = aset%sync%times
          tbounds(2,:) = aset%sync%times + 1600

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%time_varid, aset%sync%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%segcnt_varid, aset%sync%seg_count, tstart, 
     *        tcount))
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%segs_varid, aset%sync%segs_used, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%samples_varid, aset%sync%samples, tstart, tcount))
            tstarts(1) = 1
            tcounts(1) = 18
            tstarts(2) = start_index
            tcounts(2) = count
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%disp_varid, aset%sync%disp_hex, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%sync%grpid, bset%sync%src_varid, aset%sync%src_index, tstart, tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_PUT_CAT4_VARS ----------------------------------------------------------
c  Fills variable values in the cat4 group
c-------------------------------------------------------------------------------
        subroutine wc5_put_cat4_vars(bset, aset, start_index, count) 
          integer::      count, start_index, tcount(1), tstart(1), tcounts(2), tstarts(2)
          integer,allocatable:: tbounds(:,:)
          type(wc5_dataset), intent(in)::  aset, bset   !* base set and add set

          tcount(1) = count
          tstart(1) = start_index

          tcounts(1) = 2
          tcounts(2) = count
          tstarts(1) = 1
          tstarts(2) = start_index

          allocate (tbounds(2,aset%cat4%time_count))
          tbounds(1,:) = aset%cat4%times
          tbounds(2,:) = aset%cat4%times + 10

          if (count .gt. 0 .and. start_index .gt. 0) then
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%time_varid, aset%cat4%times, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%tbounds_varid, tbounds, tstarts, tcounts))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%flags_varid, aset%cat4%flags, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%flags2_varid, aset%cat4%flags2, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%src_varid, aset%cat4%src_index, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%airt_varid, aset%cat4%airt, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%status_varid, aset%cat4%status, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%white_varid, aset%cat4%white, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%black_varid, aset%cat4%black, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%metal_varid, aset%cat4%metal, tstart, tcount))
            call nc_call_func(nf90_put_var(bset%cat4%grpid, bset%cat4%grooved_varid, aset%cat4%grooved, tstart, tcount))
          end if
          deallocate(tbounds)
        end subroutine


c-- WC5_EXTRACT_SP_DATA ---------------------------------------------------------
c Takes data from a wavecdf set and loads it into a sp_data_block
c-------------------------------------------------------------------------------
        subroutine wc5_extract_sp_data(wset, sp_index, sp_data, errcode)
          integer::              errcode, i, sp_index
          type(sp_data_block)    sp_data
          type(wc5_dataset), intent(in)::   wset

          sp_data%bands = wset%wave%freq_count
          do i = 1, sp_data%bands
            sp_data%freq(i) = wset%wave%freqs(i)
            sp_data%band_width(i) = wset%wave%bw(i)
            sp_data%ener_dens(i) = wset%wave%a0(i,sp_index)
            sp_data%dir(i) = wset%wave%mdir(i,sp_index)
            sp_data%a1(i) = wset%wave%a1(i,sp_index)
            sp_data%b1(i) = wset%wave%b1(i,sp_index)
            sp_data%a2(i) = wset%wave%a2(i,sp_index)
            sp_data%b2(i) = wset%wave%b2(i,sp_index)
            sp_data%check(i) = wset%wave%check(i,sp_index)
          end do
        end subroutine


c-- WC5_SET_SP_DATA -------------------------------------------------------------
c Assigns data from a sp_data_block to a wavecdf set
c-------------------------------------------------------------------------------
        subroutine wc5_set_sp_data(wset, sp_index, sp_data, errcode)
          integer::              errcode, i, sp_index
          type(sp_data_block)    sp_data
          type(wc5_dataset)  wset

          errcode = 0
          if (sp_data%bands .ne. wset%wave%freq_count) then
            errcode = 1
            return
          end if

          do i = 1, sp_data%bands
            wset%wave%a0(i,sp_index) = sp_data%ener_dens(i)
            wset%wave%mdir(i,sp_index) = sp_data%dir(i)
            wset%wave%a1(i,sp_index) = sp_data%a1(i)
            wset%wave%b1(i,sp_index) = sp_data%b1(i)
            wset%wave%a2(i,sp_index) = sp_data%a2(i)
            wset%wave%b2(i,sp_index) = sp_data%b2(i)
            wset%wave%check(i,sp_index) = sp_data%check(i)
          end do
        end subroutine


c-- WC5_REBAND_WAVES -----------------------------------------------------------
c  Redistributes data from one spectral layout to another, conserving energy.
c  The output set, oset, should have the correct time/freq dimensions allocated
c  and set before using this routine.
c-------------------------------------------------------------------------------
        subroutine wc5_reband_waves(iset, oset)
          integer,parameter::  max_bands = 128, max_count = 100
          integer i, reband_bins(max_count,max_bands), reband_counts(max_bands)
          real fli(max_bands), fhi(max_bands), flo(max_bands), fho(max_bands)
          real reband_weights(max_count,max_bands)
          byte,allocatable::   redist_flag(:)
          type(wc5_dataset)    iset, oset

          do i = 1, iset%wave%freq_count
            fli(i) = iset%wave%freqs(i) - (iset%wave%bw(i)/2.0)
            fhi(i) = iset%wave%freqs(i) + (iset%wave%bw(i)/2.0)
          end do
          do i = 1, oset%wave%freq_count
            flo(i) = oset%wave%freqs(i) - (oset%wave%bw(i)/2.0)
            fho(i) = oset%wave%freqs(i) + (oset%wave%bw(i)/2.0)
          end do

          call calc_reband_coeffs(iset%wave%freq_count, fli, fhi, oset%wave%freq_count, 
     *      flo, fho, reband_counts, reband_bins, reband_weights)
          call wc5_apply_reband_coeffs(reband_counts, reband_bins, reband_weights, iset, oset)
          call wc5_recalculate_params(oset)

          oset%wave%times = iset%wave%times
          oset%wave%src_index = iset%wave%src_index
          if (iset%source%file_count .gt. 0) oset%source%file_name = iset%source%file_name

          oset%wave%flags = iset%wave%flags
          oset%wave%flags2 = iset%wave%flags2
          oset%wave%fflags = 1
          oset%wave%fflags2 = 0
          allocate(redist_flag(oset%wave%time_count))
          redist_flag = 14
          oset%wave%flags2 = MERGE(redist_flag, iset%wave%flags2, iset%wave%flags2 .eq. 0)
          deallocate(redist_flag)
        end subroutine


c-- WC5_REBAND_MK4_TO_MK3 ------------------------------------------------------
c  Redistributes data from a Mk4 spectral layout to a Mk3 layout. The Mk3
c  dataset will be fully initialized in this subroutine.
c-------------------------------------------------------------------------------
        subroutine wc5_reband_mk4_to_mk3(wset4, wset3)
          type(wc5_dataset)    wset3, wset4

          call wc5_initialize_set(wset3)
          wset3%wave%freq_count = 64
          wset3%wave%time_count = wset4%wave%time_count
          wset3%source%file_count = wset4%source%file_count
          call wc5_allocate_set(wset3)

          wset3%wave%freqs = DU_frequencies
          wset3%wave%bw = DU_bandwidths

          call wc5_reband_waves(wset4, wset3)
        end subroutine


c-- WC5_REBAND_TO_128B ---------------------------------------------------------
c  Redistributes data into a 128-band, 0-0.5Hz layout which is the standard
c  returned by CDIP's pressure sensor processing with a 2048-point, 1Hz sample.
c-------------------------------------------------------------------------------
        subroutine wc5_reband_to_128b(wset, wset128)
          integer              i
          type(wc5_dataset)    wset, wset128

          call wc5_initialize_set(wset128)
          wset128%wave%freq_count = 128
          wset128%wave%time_count = wset%wave%time_count
          wset128%source%file_count = wset%source%file_count
          call wc5_allocate_set(wset128)

          wset128%wave%bw = 0.00390625
          do i = 1, 128
            wset128%wave%freqs(i) = wset128%wave%bw(1) * i
          end do

          call wc5_reband_waves(wset, wset128)
        end subroutine


c-- WC5_APPLY_REBAND_COEFFS ----------------------------------------------------
c  Applies the rebanding coeffs calculated by the spectral.f/calc_reband_coeffs 
c  routine to wc5_dataset. See calc_reband_coeffs() for more details.
c-------------------------------------------------------------------------------
        subroutine wc5_apply_reband_coeffs(reband_counts, reband_bins, 
     *               reband_weights, iset, oset)
          integer,parameter::  max_count = 100
          integer              i, j, k, reband_counts(*), reband_bins(max_count,*)
          real                 reband_weights(max_count,*), twotheta
          logical,allocatable::  check_mask(:), direction_mask(:), energy_mask(:), true_array(:)
          real,allocatable::     curr_energy(:), missing_value(:), check_max(:)
          type(wc5_dataset)      iset, oset

c-  Loop thru output bins

          allocate(curr_energy(iset%wave%time_count), check_mask(iset%wave%time_count), 
     *      check_max(iset%wave%time_count), direction_mask(iset%wave%time_count), energy_mask(iset%wave%time_count), 
     *      missing_value(iset%wave%time_count), true_array(iset%wave%time_count))
          missing_value = WC5_real_fill
          check_max = 2.55
          true_array = .true.

          do i = 1, oset%wave%freq_count
            check_mask = .false.
            energy_mask = .false.
            direction_mask = .false.

            oset%wave%a0(i,:) = WC5_real_fill
            oset%wave%a1(i,:) = WC5_real_fill
            oset%wave%b1(i,:) = WC5_real_fill
            oset%wave%a2(i,:) = WC5_real_fill
            oset%wave%b2(i,:) = WC5_real_fill
            oset%wave%check(i,:) = WC5_real_fill
            oset%wave%dspread(i,:) = WC5_real_fill
            oset%wave%mdir(i,:) = WC5_real_fill
            oset%wave%m2(i,:) = WC5_real_fill
            oset%wave%n2(i,:) = WC5_real_fill

            if (reband_counts(i) .gt. 0) then
              oset%wave%a0(i,:) = 0.0
              oset%wave%a1(i,:) = 0.0
              oset%wave%b1(i,:) = 0.0
              oset%wave%a2(i,:) = 0.0
              oset%wave%b2(i,:) = 0.0
              oset%wave%check(i,:) = 0.0
              oset%wave%dspread(i,:) = 0.0

              do j = 1, reband_counts(i)
                energy_mask = MERGE(true_array, energy_mask, (iset%wave%a0(reband_bins(j,i),:) .eq. WC5_real_fill)) 
                curr_energy = iset%wave%a0(reband_bins(j,i),:) * iset%wave%bw(reband_bins(j,i))
                oset%wave%a0(i,:) = oset%wave%a0(i,:) + curr_energy * reband_weights(j,i)
                direction_mask = MERGE(true_array, direction_mask, 
     *            (iset%wave%a1(reband_bins(j,i),:) .eq. WC5_real_fill)) 
                oset%wave%a1(i,:) = oset%wave%a1(i,:) + iset%wave%a1(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                oset%wave%b1(i,:) = oset%wave%b1(i,:) + iset%wave%b1(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                oset%wave%a2(i,:) = oset%wave%a2(i,:) + iset%wave%a2(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                oset%wave%b2(i,:) = oset%wave%b2(i,:) + iset%wave%b2(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                oset%wave%dspread(i,:) = oset%wave%dspread(i,:) + iset%wave%dspread(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                oset%wave%check(i,:) = oset%wave%check(i,:) + iset%wave%check(reband_bins(j,i),:) *
     *            curr_energy * reband_weights(j,i)
                check_mask = MERGE(true_array, check_mask, (iset%wave%check(reband_bins(j,i),:) .eq. WC5_real_fill))
              end do

              oset%wave%a0(i,:) = oset%wave%a0(i,:) / oset%wave%bw(i)
              oset%wave%a0(i,:) = MERGE(oset%wave%a0(i,:), missing_value, (energy_mask .eqv. .false.))
              oset%wave%a1(i,:) = oset%wave%a1(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%b1(i,:) = oset%wave%b1(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%a2(i,:) = oset%wave%a2(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%b2(i,:) = oset%wave%b2(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%dspread(i,:) = oset%wave%dspread(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%check(i,:) = oset%wave%check(i,:) / (oset%wave%bw(i) * oset%wave%a0(i,:))
              oset%wave%check(i,:) = MERGE(check_max, oset%wave%check(i,:), (oset%wave%check(i,:) .gt. 2.55))

              oset%wave%a1(i,:) = MERGE(oset%wave%a1(i,:), missing_value, (direction_mask .eqv. .false.))
              oset%wave%b1(i,:) = MERGE(oset%wave%b1(i,:), missing_value, (direction_mask .eqv. .false.))
              oset%wave%a2(i,:) = MERGE(oset%wave%a2(i,:), missing_value, (direction_mask .eqv. .false.))
              oset%wave%b2(i,:) = MERGE(oset%wave%b2(i,:), missing_value, (direction_mask .eqv. .false.))
              oset%wave%dspread(i,:) = MERGE(oset%wave%dspread(i,:), missing_value, (direction_mask .eqv. .false.))
              oset%wave%check(i,:) = MERGE(oset%wave%check(i,:), missing_value, (check_mask .eqv. .false.))

c-  Calculate mdir, m2, and n2 based on rebanded coefficients above. The m2/n2
c-  calculations are drawn from .f90/datawell_utils::calc_a1_b1_a2_b2

              do k = 1, iset%wave%time_count
c               if (oset%wave%a1(i,k) .ne. WC5_real_fill) then
                if (oset%wave%a1(i,k) .ne. WC5_real_fill .and. (MAX(oset%wave%a1(i,k),oset%wave%b1(i,k),
     *               oset%wave%a2(i,k),oset%wave%b2(i,k)) - MIN(oset%wave%a1(i,k),oset%wave%b1(i,k),
     *               oset%wave%a2(i,k),oset%wave%b2(i,k)) > 0.0001)) then
                  oset%wave%mdir(i,k) = 90 - to_degrees(ATAN2(oset%wave%a1(i,k), oset%wave%b1(i,k)))
                  if (oset%wave%mdir(i,k) .lt. 0) oset%wave%mdir(i,k) = oset%wave%mdir(i,k) + 360.0
                  if (oset%wave%mdir(i,k) .ge. 360.0) oset%wave%mdir(i,k) = oset%wave%mdir(i,k) - 360.0
  
                  twotheta = 2.0 * ATAN2(oset%wave%b1(i,k), oset%wave%a1(i,k))
                  oset%wave%m2(i,k) = (oset%wave%a2(i,k)/SIN(twotheta) + oset%wave%b2(i,k)/COS(twotheta)) /
     *              (1.0/TAN(twotheta) + TAN(twotheta))
                  oset%wave%n2(i,k) = oset%wave%m2(i,k)/TAN(twotheta) - oset%wave%a2(i,k)/SIN(twotheta)
                end if
              end do
            end if
          end do

          deallocate(curr_energy, check_mask, check_max, direction_mask, energy_mask, missing_value, true_array)
        end subroutine


c-- WC5_REDISTRIBUTE_SPECTRA_NOSP ----------------------------------------------
c  Changes all spectra in the set to the specified layout
c-------------------------------------------------------------------------------
        subroutine wc5_redistribute_spectra_nosp(iset, oset, bset, err_code)
          integer                 err_code, i
          type(wc5_dataset)       bset, iset, oset
          type(sp_data_block)     sp_layout

          err_code = 0
          sp_layout%bands = bset%wave%freq_count
          do i = 1, sp_layout%bands
            sp_layout%band_width(i) = bset%wave%bw(i)
            sp_layout%freq(i) = bset%wave%freqs(i)
          end do

          call wc5_redistribute_spectra(iset, oset, sp_layout, err_code)
        end subroutine


c-- WC5_REDISTRIBUTE_SPECTRA ---------------------------------------------------
c  Changes all spectra in the set to the specified layout
c-------------------------------------------------------------------------------
        subroutine wc5_redistribute_spectra(iset, oset, sp_layout, err_code)
          integer                 err_code, i
          type(sp_data_block)     redist_sp, missing_sp, orig_sp, sp_layout
          type(wc5_dataset)       iset, oset

          err_code = 0
          call wc5_initialize_set(oset)
          oset%wave%time_count = iset%wave%time_count
          oset%wave%freq_count = sp_layout%bands
          call wc5_allocate_set(oset)
          oset%wave%freqs = sp_layout%freq(1:sp_layout%bands)
          oset%wave%bw = sp_layout%band_width(1:sp_layout%bands)
          oset%wave%fflags = 1
          oset%wave%fflags2 = 1

          oset%wave%times = iset%wave%times
          oset%wave%hs = iset%wave%hs
          oset%wave%tp = iset%wave%tp
          oset%wave%dp = iset%wave%dp
          oset%wave%ta = iset%wave%ta
          oset%wave%flags = iset%wave%flags
          oset%wave%flags2 = iset%wave%flags2

          missing_sp = sp_layout
          missing_sp%ener_dens = WC5_real_fill
          missing_sp%dir = WC5_real_fill
          missing_sp%check = WC5_real_fill
          missing_sp%a1 = WC5_real_fill
          missing_sp%b1 = WC5_real_fill
          missing_sp%a2 = WC5_real_fill
          missing_sp%b2 = WC5_real_fill

          do i = 1, iset%wave%time_count
            redist_sp = sp_layout
            call wc5_extract_sp_data(iset, i, orig_sp, err_code)
            if (err_code .eq. 0 .and. MAXVAL(orig_sp%ener_dens) .gt. 0) then
              call redistribute_sp(orig_sp, redist_sp)
            else
              redist_sp = missing_sp
            end if
            call wc5_set_sp_data(oset, i, redist_sp, err_code)
          end do
        end subroutine


c-- WC5_REDISTRIBUTE_9BAND -----------------------------------------------------
c  Creates a 9-band spectral dataset corresponding to the original set
c-------------------------------------------------------------------------------
        subroutine wc5_redistribute_9band(iset, nine_set, errcode)
          integer                 errcode, i
          real                    center_freqs(9), band_widths(9)
          type(sp_data_block)     sp_data
          type(wc5_dataset)       iset, nine_set

          data center_freqs /0.0352, 0.0505, 0.0590, 0.0670, 0.0774, 0.0917,
     *                       0.1125, 0.1458, 0.3333/
          data band_widths  /0.0205, 0.0101, 0.0069, 0.0089, 0.0119, 0.0167,
     *                       0.0250, 0.0417, 0.3333/

          if (iset%wave%time_count .le. 0) return

          sp_data%bands = 9
          sp_data%freq(1:9) = center_freqs
          sp_data%band_width(1:9) = band_widths
          call wc5_redistribute_spectra(iset, nine_set, sp_data, errcode)
        end subroutine


c-- WC5_REDISTRIBUTE_SEASWELL --------------------------------------------------
c  Creates a sea/swell dataset splitting at the given wave period.
c-------------------------------------------------------------------------------
        subroutine wc5_redistribute_seaswell(iset, ss_set, period, errcode)
          integer                 errcode, i
          real                    center_freqs(2), band_widths(2), period
          type(sp_data_block)     sp_data
          type(wc5_dataset)       iset, ss_set

          if (iset%wave%time_count .le. 0) return

          sp_data%bands = 2
          sp_data%freq(1) = (iset%wave%freqs(1)-(iset%wave%bw(1)/2.0) + (1.0/period)) / 2.0
          sp_data%band_width(1) = (1.0/period) - (iset%wave%freqs(1)-(iset%wave%bw(1)/2.0))
          sp_data%freq(2) = ((1.0/period) + iset%wave%freqs(iset%wave%freq_count)+
     *      (iset%wave%bw(iset%wave%freq_count)/2.0)) / 2.0
          sp_data%band_width(2) = iset%wave%freqs(iset%wave%freq_count)+(iset%wave%bw(iset%wave%freq_count)/2.0) -
     *      (1.0/period) 
          write(6,*) sp_data%freq(1), sp_data%freq(2), sp_data%band_width(1), sp_data%band_width(2)
          call wc5_redistribute_spectra(iset, ss_set, sp_data, errcode)
        end subroutine


c-- WC5_SET_DIRSPEC_DATA ---_---------------------------------------------------
c Assigns data from a mem_data block to a wavecdf set's 2D spectra
c-------------------------------------------------------------------------------
        subroutine wc5_set_dirspec_data(wset, sp_index, mem_block, errcode)
          integer::          errcode, i, j, sp_index
          type(mem_data)     mem_block
          type(wc5_dataset)  wset

          errcode = 0
          if (mem_block%freq_bands .ne. wset%wave%freq_count) then
            errcode = 1
            return
          end if

          do i = 1, mem_block%dir_bands
            do j = 1, mem_block%freq_bands
              wset%wave%dirspec(i,j,sp_index) = mem_block%ds(i,j)
            end do
          end do
        end subroutine


c-- WC5_WNC_TO_MOPDEF ----------------------------------------------------------
c  Fills a mop_def object with metadata from a wavecdf5 dataset.
c-------------------------------------------------------------------------------
        subroutine wc5_wnc_to_mopdef(wncname, wncpath, mdef, errcode)
          integer::           dimid, dimid_array(10), dimlength, errcode, ncid, varid
          real                lat, lon, depth, normal
          character*100       dim_name, full_id, site_id, stn_name, wncname, wncpath, wncfull
          type(mop_def)       mdef

          wncfull = TRIM(wncpath)//'/'//TRIM(wncname)
          
          errcode = nf90_open(wncfull, NF90_NOWRITE, ncid)
          if (errcode .ne. 0) return

          errcode = nf90_inq_varid(ncid, 'metaLatitude', varid)
          if (errcode .ne. 0) then
            errcode = nf90_inq_varid(ncid, 'metaStationLatitude', varid)
            if (errcode .ne. 0) then
              errcode = nf90_inq_varid(ncid, 'metaDeployLatitude', varid)
              if (errcode .ne. 0) return
            end if
          end if
          errcode = nf90_get_var(ncid, varid, lat)

          errcode = nf90_inq_varid(ncid, 'metaLongitude', varid)
          if (errcode .ne. 0) then
            errcode = nf90_inq_varid(ncid, 'metaStationLongitude', varid)
            if (errcode .ne. 0) then
              errcode = nf90_inq_varid(ncid, 'metaDeployLongitude', varid)
              if (errcode .ne. 0) return
            end if
          end if
          errcode = nf90_get_var(ncid, varid, lon)

          errcode = nf90_inq_varid(ncid, 'metaWaterDepth', varid)
          if (errcode .ne. 0) then
            depth = -1.0
          else
            errcode = nf90_get_var(ncid, varid, depth)
          end if

          errcode = nf90_inq_varid(ncid, 'metaShoreNormal', varid)
          if (errcode .ne. 0) then
            normal = -1.0
          else
            errcode = nf90_get_var(ncid, varid, normal)
          end if

          stn_name = 'NO_NAME'
          site_id = 'NO_ID'

          errcode = nf90_inq_varid(ncid, 'metaStationName', varid)
          if (errcode .eq. NF90_NOERR) then
            errcode = nf90_inquire_variable(ncid, varid, dimids=dimid_array)
            dimid = dimid_array(1)
            errcode = nf90_inquire_dimension(ncid, dimid, dim_name, dimlength)
            errcode = nf90_get_var(ncid, varid, stn_name(1:dimlength))
            stn_name(dimlength+1:) = ''
            if (errcode .eq. NF90_NOERR .and. site_id .eq. 'NO_ID' .and. LEN_TRIM(stn_name) .ge. 5) 
     *        site_id = stn_name(LEN_TRIM(stn_name)-4:)
          else
            errcode = nf90_inq_varid(ncid, 'metaSiteLabel', varid)
            if (errcode .eq. NF90_NOERR) then
              errcode = nf90_inq_dimid(ncid, 'metaSiteLabelLength', dimid)
              errcode = nf90_inquire_dimension(ncid, dimid, dim_name, dimlength)
              errcode = nf90_get_var(ncid, varid, stn_name(1:dimlength))
              stn_name(dimlength+1:) = ''
              if (errcode .eq. NF90_NOERR .and. site_id .eq. 'NO_ID' .and. LEN_TRIM(stn_name) .ge. 5) 
     *          site_id = stn_name(1:5)
            end if
          end if

          if (site_id .eq. 'NO_ID') then
            errcode = nf90_get_att(ncid, NF90_GLOBAL, 'id', full_id)
            if (errcode .eq. NF90_NOERR .and. LEN_TRIM(full_id) .ge. 10) site_id = full_id(6:10)
          end if

          if (stn_name .eq. 'NO_NAME' .and. site_id .ne. 'NO_ID') stn_name = site_id
      
          errcode = 0
          errcode = nf90_close(ncid)

          mdef%label = TRIM(site_id)
          mdef%name = TRIM(stn_name)
          mdef%position = init_location(lat, lon)
          mdef%depth = depth
          mdef%snormal = normal
          mdef%is_public = .true.

        end subroutine


c-- WC5_WSET_TO_MEM ------------------------------------------------------------
c  Fills a mem_ds object with data from a 2d dataset's dirspec variable.
c-------------------------------------------------------------------------------
        subroutine wc5_wset_to_mem(wset, time_idx, mem_out)
          integer             i, j, time_idx
          type(mem_data)      mem_out
          type(wc5_dataset)   wset

          mem_out%dir_bands = wset%wave%dir_count
          mem_out%dir(1:mem_out%dir_bands) = wset%wave%dirs(1:mem_out%dir_bands)

          mem_out%freq_bands = wset%wave%freq_count
          mem_out%freq(1:mem_out%freq_bands) = wset%wave%freqs(1:mem_out%freq_bands)
          mem_out%band_width(1:mem_out%freq_bands) = wset%wave%bw(1:mem_out%freq_bands)

          do i = 1, mem_out%dir_bands
            do j = 1, mem_out%freq_bands
              mem_out%ds(i,j) = wset%wave%dirspec(i,j,time_idx)
            end do
          end do
        end subroutine


c-- WC5_DIRSPEC_TO_MEMARRAY ----------------------------------------------------
c  Fills a mem_ds object with data from 2d dirspec variable
c-------------------------------------------------------------------------------
        subroutine wc5_dirspec_to_memarray(tc, fc, dc, dirspec, freqs, bws, dirs, memarray)
          integer             dc, fc, i, j, k, tc
          real                lat, lon, depth, normal, freqs(*), bws(*), dirs(*), dirspec(dc,fc,tc)
          type(mem_data)      memarray(*)

          do i = 1, tc
            memarray(i) = init_ds(fc, freqs, bws, dc, dirs, 0.0)
            do j = 1, dc
              do k = 1, fc
                memarray(i)%ds(j,k) = dirspec(j,k,i)
              end do
            end do
          end do
        end subroutine


c-- WC5_MAKE_SPHEADER ----------------------------------------------------------
c Takes data from an wc5 dataset and writes a pseudo-sp header
c-------------------------------------------------------------------------------
        subroutine wc5_make_spheader(wset, sp_index, out_unit, err_code, show_check, id)
          integer::              err_code, i, j, out_unit, sp_index
          logical                show_check
          character*5,optional:: id
          character*5            site_label
          character*14           datestr
          character*19           filename
          character*500          scomment, sea_list, swl_list
          type(wc5_dataset)      wset

          if (PRESENT(id)) then
            site_label = id
          else
            site_label = 'XXXXX'
          end if

          i = sp_index
          datestr = make_datestring(timestamp_to_date(wset%wave%times(i)))
          filename = 'nc'//site_label//datestr(1:12)
          if (show_check) then
            write(out_unit,'(a,5/)') filename
          else 
            write(out_unit,'(3a,$)') 'Time(UTC): ', datestr, '   '
          end if 

          write(out_unit,'(a,f5.2,a,f5.2,a,i3,a,f5.2)') 'Hs(m): ',wset%wave%hs(i),
     *      '   Tp(s): ',wset%wave%tp(i),'   Dp(deg): ',NINT(wset%wave%dp(i)),
     *      '   Ta(s): ',wset%wave%ta(i)

          scomment = ''
          if (wset%is_net_model) then
            do j = 1, 100
              if (wset%wave%model_input(j,i) .eq. '_') then
                scomment(j:j) = ''
              else
                scomment(j:j) = wset%wave%model_input(j,i)
              end if
            end do
            swl_list = get_field(scomment, ':', 1)
            sea_list = get_field(scomment, ':', 2)
            if (.not. show_check) then
              scomment = 'Swell input: '//TRIM(swl_list)//';  Seas input: '
     *          //TRIM(sea_list)
            else
              scomment = 'Offshore buoys: '//TRIM(swl_list)//';  Local buoys: '
     *          //TRIM(sea_list)
            end if
          end if

          write(out_unit,'(a)') TRIM(scomment)
          write(out_unit,'(1x,a,3x,a,6x,a,3x,a,5x,a,7x,a,7x,a,7x,a)')
     *      'freq', 'Band','energy', 'Dmean','a1', 'b1','a2', 'b2'
          write(out_unit,'(2x,a,4x,a,5x,a,4x,a)') 'Hz', 'width',
     *      'm*m/Hz', 'deg'
        end subroutine
           

c-- WC5_GET_GROUP --------------------------------------------------------------
c  Returns the name of the group of the given variable, i.e. the var prefix
c-------------------------------------------------------------------------------
        character*100 function wc5_get_group(varname)
           integer       sindex, eindex
           logical       is_lowercase
           character*100 varname

           is_lowercase = .true.
           sindex = 1
           eindex = 0
           do while (eindex .lt. LEN_TRIM(varname) .and. is_lowercase)
             eindex = eindex + 1
             if (ICHAR(varname(eindex:eindex)) .ge. 65 .and. 
     *         ICHAR(varname(eindex:eindex)) .le. 90) is_lowercase = .false.
           end do

           if (eindex .gt. sindex .and. .not. is_lowercase) then
             wc5_get_group = varname(sindex:eindex-1)
           else
             wc5_get_group = 'NULL'
           end if
        end function


c-- WC5_FLAG_NONPUB_RECS -------------------------------------------------------
c  Sets flags to p4s1 for all records between start and end of a timeframe
c-------------------------------------------------------------------------------
        subroutine wc5_flag_nonpub_recs(wset, stime, etime, lunit)
          integer                i, ststamp, etstamp
          integer, optional::    lunit
          real                   lval
          character*14           timestr
          character*100          comment
          type(wc5_dataset)      wset
          type(date_block)       stime, etime

          ststamp = date_to_timestamp(stime)
          etstamp = date_to_timestamp(etime)
          lval = 0.0
          comment = 'sensor_issues'

          if (wset%wave%time_count .gt. 0) then
            do i = 1, wset%wave%time_count
              if (wset%wave%times(i) .ge. ststamp .and. wset%wave%times(i) .le. etstamp) then
                timestr = make_datestring(timestamp_to_date(wset%wave%times(i)))
                if (PRESENT(lunit)) then
                  call wc5_log_update(lval, timestr, comment, i, lunit, wset%wave%flags(i), 4, 
     *              wset%wave%flags2(i), 1, 'wave')
                else
                  wset%wave%flags(i) = 4
                  wset%wave%flags2(i) = 1
                end if
              end if
            end do
          end if

          if (wset%sst%time_count .gt. 0) then
            do i = 1, wset%sst%time_count
              if (wset%sst%times(i) .ge. ststamp .and. wset%sst%times(i) .le. etstamp) then
                timestr = make_datestring(timestamp_to_date(wset%sst%times(i)))
                if (PRESENT(lunit)) then
                  call wc5_log_update(lval, timestr, comment, i, lunit, wset%sst%flags(i), 
     *              4, wset%sst%flags2(i), 1, 'sst')
                else
                  wset%sst%flags(i) = 4
                  wset%sst%flags2(i) = 1
                end if
              end if
            end do
          end if

          if (wset%acm%time_count .gt. 0) then
            do i = 1, wset%acm%time_count
              if (wset%acm%times(i) .ge. ststamp .and. wset%acm%times(i) .le. etstamp) then
                timestr = make_datestring(timestamp_to_date(wset%acm%times(i)))
                if (PRESENT(lunit)) then
                  call wc5_log_update(lval, timestr, comment, i, lunit, wset%acm%flags(i), 
     *              4, wset%acm%flags2(i), 1, 'acm')
                else
                  wset%acm%flags(i) = 4
                  wset%acm%flags2(i) = 1
                end if
              end if
            end do
          end if

          if (wset%upcross%time_count .gt. 0) then
            do i = 1, wset%upcross%time_count
              if (wset%upcross%times(i) .ge. ststamp .and. wset%upcross%times(i) .le. etstamp) then
                timestr = make_datestring(timestamp_to_date(wset%upcross%times(i)))
                if (PRESENT(lunit)) then
                  call wc5_log_update(lval, timestr, comment, i, lunit, wset%upcross%flags(i), 
     *              4, wset%upcross%flags2(i), 1, 'upcross')
                else
                  wset%upcross%flags(i) = 4
                  wset%upcross%flags2(i) = 1
                end if
              end if
            end do
          end if

          if (wset%cat4%time_count .gt. 0) then
            do i = 1, wset%cat4%time_count
              if (wset%cat4%times(i) .ge. ststamp .and. wset%cat4%times(i) .le. etstamp) then
                timestr = make_datestring(timestamp_to_date(wset%cat4%times(i)))
                if (PRESENT(lunit)) then
                  call wc5_log_update(lval, timestr, comment, i, lunit, wset%cat4%flags(i), 
     *              4, wset%cat4%flags2(i), 1, 'cat4')
                else
                  wset%cat4%flags(i) = 4
                  wset%cat4%flags2(i) = 1
                end if
              end if
            end do
          end if

        end subroutine


c-- WC5_LOG_UPDATE -------------------------------------------------------------
c  Update flags and logs in standard format; helper for wc5_flag_nonpub_recs
c-------------------------------------------------------------------------------
        subroutine wc5_log_update(val, timestr, comment, idx, lunit,
     *               flagvar1, flagval1, flagvar2, flagval2, group)
          integer             idx, flagval1, flagval2, lunit
          byte                flagvar1, flagvar2
          real                val
          character           tab, update_type
          character*5         flagstr
          character*(*)       group, timestr, comment

          tab = ACHAR(9)
          if (flagvar1 .ne. flagval1) then
            flagvar1 = flagval1
            flagvar2 = flagval2
            write(flagstr(1:5),'(a,i1,a,i2.2)') 'p', flagval1, 's', flagval2
            update_type = 'a'
          else
            flagstr = '    w'
            update_type = 'n'
          end if
          write(lunit,'(i6,a,f10.2,10a)') idx, tab, val, tab, timestr, tab,
     *      TRIM(group), tab, flagstr, tab, update_type, tab, TRIM(comment)
        end subroutine


c-- WC5_TO_XMLSET --------------------------------------------------------------
c  Copies wave records from a wc5_dataset into a xml_wave_dataset. NOTE: no
c  metadata fields are set in xset, only the wave records.
c-------------------------------------------------------------------------------
        subroutine wc5_to_xmlset(wset, xset)
          integer                   charcount, cpos, eidx, i, j, sidx, upos
          character*100             input_sources
          type(wc5_dataset)         wset
          type(xml_wave_dataset)    xset

          if (xset%time_count .eq. 0) then
            if ((.not. wset%is_net_model) .and. (.not. wset%is_2d_model)) then
              call init_xml_waves(xset, 2)
              xset%sample_rate = 1.28
              xset%sample_length = 1600
            else
              call init_xml_waves(xset, 1)
            end if
            xset%proc_time = current_utc()
            xset%time_count = wset%wave%time_count
            xset%is_directional = wset%is_directional
            xset%is_nearshore = wset%is_nearshore
            xset%has_surfzone = .false.

            xset%band_count = wset%wave%freq_count
            xset%frequency(1:xset%band_count) = wset%wave%freqs
            xset%band_width(1:xset%band_count) = wset%wave%bw

            sidx = 0
          else
            sidx = xset%time_count
          end if

          xset%time_count = sidx + wset%wave%time_count

          xset%Hs(sidx+1:xset%time_count) = wset%wave%hs
          xset%Tp(sidx+1:xset%time_count) = wset%wave%tp
          xset%Ta(sidx+1:xset%time_count) = wset%wave%ta
          if (xset%is_nearshore) then
            xset%Sxy(sidx+1:xset%time_count) = wset%wave%sxy
            xset%Sxx(sidx+1:xset%time_count) = wset%wave%sxx
          end if
          do i = 1, wset%wave%time_count
            xset%time(sidx+i) = timestamp_to_date(wset%wave%times(i))
            if (wset%wave%flags(i) .eq. 1) then
              xset%passes_qc(sidx+i) = 1
              xset%public_release(sidx+i) = 1
            else
              xset%passes_qc(sidx+i) = 0
              xset%public_release(sidx+i) = 0
            end if
            xset%Dp(sidx+i) = NINT(wset%wave%dp(i))
            if (xset%is_nearshore) xset%Dm(sidx+i) = NINT(wset%wave%dm(i))
            do j = 1, wset%wave%freq_count
              xset%ener_dens(j,sidx+i) = wset%wave%a0(j,i)
              if (wset%wave%mdir(j,sidx+i) .eq. WC5_real_fill) then
                xset%mean_dir(j,sidx+i) = -1.0
                xset%a1(j,sidx+i) = 0.0
                xset%a2(j,sidx+i) = 0.0
                xset%b1(j,sidx+i) = 0.0
                xset%b2(j,sidx+i) = 0.0
              else
                xset%mean_dir(j,sidx+i) = wset%wave%mdir(j,i)
                xset%a1(j,sidx+i) = wset%wave%a1(j,i)
                xset%a2(j,sidx+i) = wset%wave%a2(j,i)
                xset%b1(j,sidx+i) = wset%wave%b1(j,i)
                xset%b2(j,sidx+i) = wset%wave%b2(j,i)
              end if
              if (xset%is_sensor) then
                xset%check_factor(j,sidx+i) = wset%wave%check(j,i)
              else 
                xset%bin_coverage(j,sidx+i) = wset%wave%check(j,i)
              end if
            end do

            xset%model_input_buoys(sidx+i) = ''
            do j = 1, 100
              input_sources(j:j) = wset%wave%model_input(j,i)
            end do
            cpos = INDEX(input_sources, ':')
            upos = INDEX(input_sources, '_')
            charcount = 1
            do j = 1, cpos-1, 6
              if (j .gt. 1) then
                xset%model_input_buoys(sidx+i)(charcount:charcount) = '-'
                charcount = charcount + 1
              end if 
              if (wset%wave%model_input(j,i) .eq. '4' .and. wset%wave%model_input(j+1,i) .eq. '6') then
                xset%model_input_buoys(sidx+i)(charcount:charcount) = '6'
                xset%model_input_buoys(sidx+i)(charcount+1:charcount+2) = input_sources(j+3:j+4)
              else 
                xset%model_input_buoys(sidx+i)(charcount:charcount+2) = input_sources(j:j+2)
              end if
              charcount = charcount + 3
            end do
            xset%model_input_buoys(sidx+i)(charcount:charcount) = ':'
            charcount = charcount + 1
            do j = cpos+1, upos-1, 6
              if (j .gt. cpos+1) then
                xset%model_input_buoys(sidx+i)(charcount:charcount) = '-'
                charcount = charcount + 1
              end if 
              if (wset%wave%model_input(j,i) .eq. '4' .and. wset%wave%model_input(j+1,i) .eq. '6') then
                xset%model_input_buoys(sidx+i)(charcount:charcount) = '6'
                xset%model_input_buoys(sidx+i)(charcount+1:charcount+2) = input_sources(j+3:j+4)
              else 
                xset%model_input_buoys(sidx+i)(charcount:charcount+2) = input_sources(j:j+2)
              end if
              charcount = charcount + 3
            end do
          end do

          call check_time_span(xset)
        end subroutine


c-- WC5_MAKE_TRIMMED_XYZ_SET ---------------------------------------------------
c  Produces a wc5_dataset with source/xyz groups only, starting and ending
c  with the first/last non-missing values.
c-------------------------------------------------------------------------------
        subroutine wc5_make_trimmed_xyz_set(wset, xset, errcode)
          integer               cnt, errcode, i, j, k, l, prev_sidx, scnt
          type(wc5_dataset)     wset, xset

          errcode = 0
          call wc5_initialize_set(xset, groups=WC5_include_source+WC5_include_xyz)

          i = 1
          do while (i .lt. wset%xyz%rec_count .and. wset%xyz%flags(i) .eq. 9)
            i = i + 1
          end do
          j = wset%xyz%rec_count
          do while (j .gt. 1 .and. wset%xyz%flags(j) .eq. 9)
            j = j - 1
          end do

          if (i .le. j) then
            xset%xyz%rec_count = j - i + 1
            xset%xyz%sample_rate = wset%xyz%sample_rate
            xset%xyz%filter_delay = wset%xyz%filter_delay
            xset%xyz%start_time = wset%xyz%start_time + NINT((i-1)/DBLE(wset%xyz%sample_rate))
            call wc5_allocate_set(xset)
            scnt = 0 
            prev_sidx = 0

            do k = i, j
              cnt = k - i + 1
              xset%xyz%flags(cnt) = wset%xyz%flags(k)
              xset%xyz%flags2(cnt) = wset%xyz%flags2(k)
              xset%xyz%xdisp(cnt) = wset%xyz%xdisp(k)
              xset%xyz%ydisp(cnt) = wset%xyz%ydisp(k)
              xset%xyz%zdisp(cnt) = wset%xyz%zdisp(k)
              xset%xyz%src_index(cnt) = wset%xyz%src_index(k)
              if (xset%xyz%src_index(cnt) .ne. prev_sidx) then
                scnt = scnt + 1
                prev_sidx = xset%xyz%src_index(cnt)
              end if
            end do

            xset%source%file_count = scnt
            call wc5_allocate_set(xset)
            scnt = 0
            prev_sidx = 0

            write(6,*) wc5_get_source_filename(wset, 1)
            do k = 1, xset%xyz%rec_count
              if (prev_sidx .ne. xset%xyz%src_index(k)) then
                scnt = scnt + 1
                write(6,*) wc5_get_source_filename(wset, xset%xyz%src_index(k))
                do l = 1, LEN_TRIM(wc5_get_source_filename(wset, xset%xyz%src_index(k)))
                  xset%source%file_name(l,scnt) = wset%source%file_name(l,xset%xyz%src_index(k))
                end do
                prev_sidx = xset%xyz%src_index(k)
              end if
              xset%xyz%src_index(k) = scnt
            end do

          else
            errcode = 1		!* No xyz records found
          end if

          return
        end subroutine


        character*30 function wc5_get_source_filename(wset, idx)
           integer             i, idx
           type(wc5_dataset)   wset 

           do i = 1, WC5_filename_length
             wc5_get_source_filename(i:i) = wset%source%file_name(i,idx)
           end do
           i = WC5_filename_length
           do while (wc5_get_source_filename(i:i) .eq. WC5_char_fill .and. i .gt. 0)
             wc5_get_source_filename(i:i) = ' '
             i = i  - 1
           end do
        end function

      end !* END MODULE
