c-- MOP_UTILS ------------------------------------------------------------------
c   The mop_utils module contains methods that assemble and manipulate 
c   information related to mops - model output points. 
c-------------------------------------------------------------------------------
        module mop_utils

        use archive_info
        use dates
        use file_ops
        use locations
        use spectral
        use strings

        save


        type mop_def
          character*5      label
          character*50     name
          type(location)   position
          real             depth, snormal
          logical          is_public
        end type

        integer, parameter :: MOP_array_size = 12000

        contains


c-- LOAD_MOP_ARRAY -------------------------------------------------------------
c   Loads an array of mops from the given file.
c-------------------------------------------------------------------------------
          subroutine load_mop_array(mop_file, mops, err_code, mop_path)
            integer::        count, err_code, num_fields, munit=99
            real             lat, lon
            character        tab
            character*100               mop_file
            character*100,optional::    mop_path
            character*500               line
            type(mop_def)               mops(MOP_array_size)

            tab = ACHAR(9)
            if (.not. PRESENT(mop_path)) mop_path = '/project/data05/net_model/mops/'

c--   Open mop file

            call open_read(munit, mop_path, mop_file, err_code, 6)
            if (err_code .ne. 0) return

c--   Loop over entries, init mop for each and add to array

            count = 1
            do while (err_code .eq. 0)
              read(munit,'(a500)',iostat=err_code) line
              if (err_code .gt. 0) then
                close(munit)
                return
              else if (err_code .eq. 0) then
                mops(count)%label = get_field(line, tab, 1)

                lat = get_field_real(line, tab, 2)
                lon = get_field_real(line, tab, 3)
                mops(count)%position = init_location(lat, lon)

                mops(count)%depth = get_field_real(line, tab, 4)
                mops(count)%snormal = get_field_real(line, tab, 5)
                mops(count)%is_public = get_field_logical(line, tab, 6)
                mops(count)%name = get_field(line, tab, 7)

                count = count + 1
              end if
            end do

            close(munit)
            err_code = 0

c-   Flag unused entries in array with label = 'NULL'

            do index = count, MOP_array_size
              mops(index)%label = 'NULL'
            end do

          end subroutine


c-- GET_MOP_DEF ----------------------------------------------------------------
c   Returns a mop_def object with fields set for the given label.
c-------------------------------------------------------------------------------
          type(mop_def) function get_mop_def(id, mops, err_code)
            integer::        err_code
            character*5      id
            type(mop_def)    mops(MOP_array_size)
            type(mop_def)    empty_def    !* Never initialized; all zeroes

            get_mop_def = empty_def

c--   Loop over array entries, checking for the requested id

            do index = 1, MOP_array_size
              if (mops(index)%label .eq. id) then
                get_mop_def%label = mops(index)%label 
                get_mop_def%position = mops(index)%position
                get_mop_def%depth = mops(index)%depth
                get_mop_def%snormal = mops(index)%snormal
                get_mop_def%is_public = mops(index)%is_public
                get_mop_def%name = mops(index)%name
                return
              end if
            end do
          end function



c-- GET_MOP_ARRAY_INDEX --------------------------------------------------------
c   Returns a mop_def object with fields set for the given label.
c-------------------------------------------------------------------------------
          integer function get_mop_array_index(id, mops)
            character*5      id
            type(mop_def)    mops(MOP_array_size)

            do index = 1, MOP_array_size
              if (mops(index)%label .eq. id) then
                get_mop_array_index = index
                return
              end if
            end do
            get_mop_array_index = -1
          end function


c-- MAKE_MOP_FRAME -------------------------------------------------------------
c   Creates an ai_time_frame for the given mop. Position, depth, shore mormal
c   are set appropriately; all other fields are filled with N/A or similar
c   values.
c-------------------------------------------------------------------------------
          type(ai_time_frame) function make_mop_frame(mop)
            integer             ecode
            type(mop_def)       mop

            read(mop%label(4:5),'(i2)', iostat=ecode) make_mop_frame%chan
            if (ecode .ne. 0) make_mop_frame%chan = 1
            make_mop_frame%frame = 1
            make_mop_frame%description = 'Model output'

            make_mop_frame%start_date = init_date(1970, 1, 1, 0, 0, 0)
            make_mop_frame%end_date = init_date(2100, 1, 1, 0, 0, 0)

            make_mop_frame%data_index = 14
            make_mop_frame%gauge_index = 21
            make_mop_frame%sample_rate = 0.0
            make_mop_frame%serial_number = "N/A" 
            make_mop_frame%top_hat = "N/A"

            make_mop_frame%deploy_site%lat = mop%position%lat
            make_mop_frame%deploy_site%long = mop%position%long

            make_mop_frame%water_depth = NINT(mop%depth * 100.0)
            make_mop_frame%sensor_elev = -1
            make_mop_frame%magnetic_var = 0.0
            make_mop_frame%inclination = 0.0
            make_mop_frame%max_value = 20.0

            make_mop_frame%station_index = 5
            make_mop_frame%software_index = 1
            make_mop_frame%analysis_index = 1
            make_mop_frame%is_public = mop%is_public
            
            make_mop_frame%cal_factor = 0.0
            make_mop_frame%cal_offset = 0.0
            make_mop_frame%paros_a0 = 0.0
            make_mop_frame%paros_b0 = 0.0
            make_mop_frame%paros_t0 = 0.0

            make_mop_frame%has_gps = .false.
            make_mop_frame%surge_filter = .false.
            make_mop_frame%energy_basin = .false.
            make_mop_frame%radio_modem = .false.
            make_mop_frame%temperature_calibrated = .false.

          end function


c-- INIT_AI_DATA_FROM_MOP ------------------------------------------------------
c   Initializes the AI_data global based on the given mop.
c   Calls GET_MOP_FRAME to create an ai_time_frame.
c-------------------------------------------------------------------------------
          subroutine init_AI_data_from_mop(mop)
            type(mop_def)   mop

            AI_data%stn_number = mop%label(1:3)
            AI_data%funding = "N/A"
            AI_data%operator = "N/A"
            AI_data%stn_name = mop%name
            AI_data%decommission_date = init_date(2100, 1, 1, 0, 0, 0)
            AI_data%frames(1,1) = make_mop_frame(mop)
          end subroutine


c-- INIT_SP_HDR_FROM_MOP -------------------------------------------------------
c   Creates an sp_hdr_block for the given mop. Position, depth, shore normal
c   are set appropriately; all other fields are filled with N/A or similar
c   values.
c-------------------------------------------------------------------------------
          type(sp_hdr_block) function init_sp_hdr_from_mop(mop)
            type(mop_def)       mop

            init_sp_hdr_from_mop%stn_name = mop%name
            init_sp_hdr_from_mop%position = mop%position
            init_sp_hdr_from_mop%sensor_type = SP_file_labels(21)
            init_sp_hdr_from_mop%sensor_elev = mop%depth
            init_sp_hdr_from_mop%shore_normal = mop%snormal

            init_sp_hdr_from_mop%filename = 'XX'//mop%label
            init_sp_hdr_from_mop%source_file = 'NULL'
            init_sp_hdr_from_mop%sample_rate = -1
            init_sp_hdr_from_mop%sample_length = -1
            init_sp_hdr_from_mop%Hs = -1
            init_sp_hdr_from_mop%Tp = -1
            init_sp_hdr_from_mop%Ta = -1
            init_sp_hdr_from_mop%Dp = -1
          end function

      end module
