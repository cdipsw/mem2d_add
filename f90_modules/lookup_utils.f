c-- LOOKUP_UTILS ---------------------------------------------------------------
c
c   The lookup_utils module is the home of all buoydd, index, and farm lookup
c   utilities.
c
c   Important: If you make a change to this module, re-make all programs 
c              that depend on this module. Go to ./depends to find those 
c              dependencies.
c
c-------------------------------------------------------------------------------

      module lookup_utils

      use archive_info
      use file_ops
      use dates
      use proc_streams   !* only used to get public/nonpub from proc archive
      use misc_utils

      save

        interface get_file_path
          module procedure get_file_path_pub_known, get_file_path_no_pub
        end interface

        type file_set
          character*19 rd, df, sp, xy
        end type

        type index_set
          character*19 rd, df(AI_max_channels)
        end type

        type lookup_block
           character*2 file_prefix       !* e.g. df, pm
           character*3 file_type         !* e.g. pmt, dsk
           character*6 data_location     !* e.g. data02
           integer path_type             !* e.g. 1=full, 2=no month, 
                                         !* e.g. 3=no month no strm, 4=no strm
        end type

        integer, parameter:: 
     *       LU_num_file_prefixes = 30,  !* recompile avail src
     *       LU_max_entries = 10000

        character*2  file_prefixes(LU_num_file_prefixes)
        character*6  data_locations(LU_num_file_prefixes)
        character*3  file_types(LU_num_file_prefixes)
        character*17 prefix_name(LU_num_file_prefixes)
        character*1  lu_markers(LU_num_file_prefixes)
        integer      path_types(LU_num_file_prefixes)
        integer      date_fmts(LU_num_file_prefixes)


c-------------------------------------------------------------------------------
c   LOOKUP ARRAYS
c  
c   The following arrays are used in the lookup routines to determine
c   how to find the correct path to a file. If you add a new type,
c   re-make programs that depend on this module. See note at
c   start of this file.
c  
c   FILE PREFIXES: 
c     file prefixes distinguish between file formats. 
c  
c     da = databank air temperture,
c     db = databank barometer, dd = databank direction, de = databank energy
c     dt = databank temperature, dw = databank wind, dp = databank depth
c     df = dskfarm, ds = surge, ix = index, pm = parameter, 
c     sp = spectral, xy = xy, rd = rd file
c     pl = plot (monthly), e.g. compendium, mountain, feather etc.
c     Td = tide 15 min average, td = tide 6 min avg.
c
c   DATA_LOCATIONS:
c     Gives the /project directory holding all of the data of that type
c     
c     data01 - df
c     data02 - xy, sp, ,pm, pl, all databank products
c     data03 - rd
c  
c   FILE TYPE: 
c     specifies the sub-directory, (e.g. dsk_2001) in which the data of
c     the corresponding file prefix is stored. This name should be
c     constructed by pre-pending "d" to the file prefix name. Most
c     of the names don't follow this convention due to historical reasons.
c  
c   PATH TYPE: 
c     Specifies the sub-directory structure of the path to the data.
c     Full general path is: /project/DDDDDD/PPPPPP_DATA/KKK_YYYY/SSS/MMM/TT
c     Where 
c       DDDDDD = partition, PPPPPP = PUBLIC or NONPUB, KKK = file type
c       YYYY = year, SSS = station, MMM = month (jun), TT = stream (p2)
c     
c     There are currently 4 types of path listed below with examples.
c
c     1 = fullpath (with stream and month),  
c         /project/data01/PUBLIC_DATA/dsk_2003/073/may/03/
c     2 = fullpath no month,              
c         /project/dbase3/PUBLIC_DATA/dbw_2003/073/p2/
c     3 = fullpath no month no stream, 
c         /project/data01/INDEX_FILES/dsk_2003/073
c     4 = fullpath no stream, 
c         /project/spaces/SOURCE_DATA/drd_1986/027/jun
c
c   DATE FMTS:
c     Specifies the format of the date key 
c
c     1 = databank fmt    e.g. 200307301245
c     2 = parameter fmt   e.g. 2003 07 30 12 45
c     3 = index or rd fmt e.g. rd08300200307301245
c     4 = tide fmt        e.g. 20030730124536  (has seconds also)
c
c   Modified
c     Grant A. Cameron	08/23/2002	Added rd files
c     Grant A. Cameron	07/31/2003	Added tide (Td and td)
c     Grant A. Cameron	07/31/2003	Added date_fmts array
c-------------------------------------------------------------------------------

        data  file_prefixes
     *    / 'db', 'dd', 'de', 'df', 'dt', 'dw', 'ix', 'pm', 'sp', 'xy', 'bx', 
     *      'pl', 'dp', 'da', 'ds', 'rd', 'Td', 'td', 'mp', 'ls', 'lx', 'is',
     *      'fs', 'fp', 'hv', 'im', 'bv', 'iv', 'iM', 'lm'/
        data  data_locations
     *    /'data02','data02','data02','data01','data02','data02','data01',
     *     'data02','data02','data02','data01','data02','data02','data02',
     *     'data02','data03','data02','data02','data05','data03','data03',
     *     'data03','data05','data05','data03','data03','data03','data03',
     *     'data03','data03'/
        data  file_types
     *    /'dbb','dbd','dbe','dsk','dbt','dbw','dsk','pmt','spc','xyz','dsk',
     *     'plt','dbp','dba','dbs','drd','dTd','dtd','mpc','lsp','lxy','isp',
     *     'fsp','fpm','hva','ims','bva','iva','ims','lms'/
        data  path_types
     *    /   2 ,   2 ,   2 ,   1 ,   2 ,   2 ,   3 ,   2 ,   1 ,   1 ,   3 ,
     *        2 ,   2 ,   2 ,   2 ,   4 ,   2 ,   2 ,   1 ,   1 ,   1 ,   1 ,
     *        1 ,   2 ,   1 ,   1,    1,    1,    1 ,   1/
        data  date_fmts
     *    /   1 ,   1 ,   1 ,   0 ,   1 ,   1 ,   3 ,   2 ,   0 ,   1 ,   3 ,
     *        0 ,   1 ,   1 ,   1 ,   0 ,   4 ,   4 ,   0 ,   0,    0 ,   0 ,
     *        0 ,   2 ,   0 ,   0,    0 ,   0 ,   0 ,   0/
        data  lu_markers
     *    /  'B',  'D',  'E',  ' ',  'S',  'W',  ' ',  ' ',  ' ',  ' ',  ' ',
     *       ' ',  'P',  'A',  ' ',  ' ',  'T',  'T',  ' ',  ' ',  ' ',  ' ',
     *       ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' '/
        data  prefix_name
     *    /'barometer','direction','energy','timeseries','sea temperature',
     *     'wind','index','parameter','spectral','positions','buoy lookup table',
     *     'plot','depth','air temperature','surge','raw data','tide 15min MLLW',
     *     'tide 6min MSL','modeled spectra','logger spectra','logger xyz',
     *     'iridium spectra','forecast spectra','forecast params','dwtp vecs',
     *     'iridium dmf', 'dwtp binary', 'iridium dwtp', 'iridium dmf',
     *     'logger dmf'/


      contains


c-- GET_LAST_DF ----------------------------------------------------------------
c
c   Returns the name of the last df file in the current month's 
c   buoydd_lookup_table. If no lookup table found, returns 'No_file_found'
c
c-------------------------------------------------------------------------------
        character*19 function get_last_df(station)

          character      curr_time*14, file_name*19, station*3
          integer::      entries, err_code
          type(file_set) lookup_data(LU_max_entries)

          curr_time = make_datestring(current_utc())
          file_name = 'bx'//station//'01'//curr_time(1:7)
          call load_bdd_lookup(file_name,lookup_data,entries,err_code,6)
          if (err_code .ne. 0) then
            get_last_df = 'No_file_found'
          else
            get_last_df = lookup_data(entries)%df
          end if

        end function


c-- GET_LAST_RD ----------------------------------------------------------------
c
c   Returns the name of the last rd file in the current month's
c   index file.
c
c-------------------------------------------------------------------------------
        character*19 function get_last_rd(station)

          character      curr_time*14, ix_file_name*19, station*3
          integer::      entries, err_code
          type(index_set) ixsets(LU_max_entries)

          curr_time = make_datestring(current_utc())
          ix_file_name = 'ix'//station//'00'//curr_time(1:6)
          call load_index_table(ix_file_name,ixsets,entries,err_code,6)
          if (err_code .ne. 0) then
            get_last_rd = 'Index_set_not_found'
          else
            get_last_rd = ixsets(entries)%rd
          end if

        end function


c-- RD_LOOKUP, DF_LOOKUP, SP_LOOKUP, XY_LOOKUP ---------------------------------
c
c   Check the buoydd_lookup_tables to return the name of the rd, df, sp, or xy 
c   file that corresponds to the given file - uses the FIRST matching file.
c   (NOTE: sp_lookup will only return a single sp filename, even when multiple
c   sp's were created from a file. To get multiple sp names, use the 
c   FILESET_ARRAY_LOOKUP subroutine below.)
c
c-------------------------------------------------------------------------------
        character*19 function rd_lookup(file_name)
          character  file_name*19
          integer::  err_code
          type(file_set)  entry
          entry = fileset_lookup(file_name,err_code,6)
          if (err_code .eq. 0) then
            rd_lookup = entry%rd
          else
            rd_lookup = 'No_entry_found'
          end if
        end function


        character*19 function df_lookup(file_name)
          character  file_name*19
          integer::  err_code
          type(file_set)  entry
          entry = fileset_lookup(file_name,err_code,6)
          if (err_code .eq. 0) then
            df_lookup = entry%df
          else
            df_lookup = 'No_entry_found'
          end if
        end function


        character*19 function sp_lookup(file_name)
          character  file_name*19
          integer::  err_code
          type(file_set)  entry
          entry = fileset_lookup(file_name,err_code,6)
          if (err_code .eq. 0) then
            sp_lookup = entry%sp
            if (sp_lookup(1:2) .ne. 'sp') sp_lookup = 'No_corresponding_sp'
          else
            sp_lookup = 'No_entry_found'
          end if
        end function


        character*19 function xy_lookup(file_name)
          character  file_name*19
          integer::  err_code
          type(file_set)  entry
          entry = fileset_lookup(file_name,err_code,6)
          if (err_code .eq. 0) then
            xy_lookup = entry%xy
            if (xy_lookup(1:2) .ne. 'xy') xy_lookup = 'No_corresponding_xy'
          else
            xy_lookup = 'No_entry_found'
          end if
        end function


c-- FILESET_LOOKUP -------------------------------------------------------------
c
c   Returns the FIRST fileset (i.e. line) of the buoydd_lookup_table that 
c   contains the given filename. Loads the lookup table; if this has already
c   been done, call FILESET_MATCH, below.
c
c-------------------------------------------------------------------------------
        type(file_set) function fileset_lookup(fname, err_code, err_unit)

          character  fname*19
          integer::  entries, err_unit, err_code
          type(file_set) lu_data(LU_max_entries)

          err_code = 0

          call load_bdd_lookup('df'//fname(3:5)//'01'//fname(8:19),
     *      lu_data,entries,err_code,err_unit)

          fileset_lookup = fileset_match(fname, lu_data, entries, 
     *                            err_code, err_unit)
          return
        end function


c-- FILESET_MATCH --------------------------------------------------------------
c
c   Returns the FIRST fileset (i.e. line) of the buoydd_lookup_table that 
c   contains the given filename - call this if the bx file has already been'
c   loaded using LOAD_BDD_LOOKUP
c
c-------------------------------------------------------------------------------
        type(file_set) function fileset_match(fname, lu_data, entries, 
     *                            err_code, err_unit)

          character  fname*19
          integer::  entries, err_unit, err_code
          type(file_set) lu_data(LU_max_entries)

          do i = 1, entries	!* check for match on each line
            if (lu_data(i)%df .eq. fname .or. lu_data(i)%sp .eq. fname .or. 
     *        lu_data(i)%rd .eq. fname .or. lu_data(i)%xy .eq. fname) then
              fileset_match = lu_data(i)
              return
            end if
          end do

          err_code = 1		!* if no fileset found

        end function


c-- FILESET_ARRAY_LOOKUP -------------------------------------------------------
c
c   Creates an array of filesets from a buoydd_lookup_table that contain the 
c   given filename. Used to get all sp names matching a df, rd, or xy file.
c
c-------------------------------------------------------------------------------
        subroutine fileset_array_lookup(filesets,matches,fname,err_code,err_unit)

          character  fname*19
          integer::  entries, err_unit, err_code, matches
          type(file_set) lu_data(LU_max_entries), filesets(10)

          err_code = 0

          call load_bdd_lookup('df'//fname(3:19),lu_data,entries,
     *      err_code,err_unit)
         
          matches = 0
          do i = 1, entries	!* check for match on each line
            if (lu_data(i)%df .eq. fname .or. lu_data(i)%sp .eq. fname .or. 
     *        lu_data(i)%rd .eq. fname .or. lu_data(i)%xy .eq. fname) then
              matches = matches + 1
              filesets(matches) = lu_data(i)
            end if
          end do

          if (matches .eq. 0) err_code = 1	!* if no fileset found

        end subroutine


c-- UPDATE_BDD_LOOKUP ----------------------------------------------------------
c
c   Adds a fileset to the buoydd_lookup_table
c
c-------------------------------------------------------------------------------
        subroutine update_bdd_lookup(fset, err_code, err_unit)

          character  dskpath*100, lfile*19, table_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          logical    add_line
          type(file_set) fset, lookup_data(LU_max_entries)

          if (fset%df(1:2) .eq. 'df') then
            lfile = fset%df
          else 
            lfile = fset%rd
          end if 

          call load_bdd_lookup(lfile,lookup_data,entries,err_code,err_unit)
          if (err_code .gt. 0) return

          call update_bdd_entry(fset, lookup_data, entries)

          call make_bdd_lookup(lfile,lookup_data,entries,err_code,err_unit)

        end subroutine


c-- UPDATE_BDD_ENTRY -----------------------------------------------------------
c
c   Helper for UPDATE_BDD_LOOKUP; can be called directly if the bx file has
c   already been loaded, and you want to modify it without writing it back out.
c
c-------------------------------------------------------------------------------
        subroutine update_bdd_entry(fset, lookup_data, entries)

          integer::  entries
          logical    add_line
          type(file_set) fset, lookup_data(LU_max_entries)

          add_line = .true.

c--   Check if entry already exists, update it if possible

          do i = 1, entries
            if (fset%rd(1:2) .eq. 'ls' .or. fset%rd(1:2) .eq. 'is' .or.
     *          fset%rd(1:2) .eq. 'hv' .or. fset%rd(1:2) .eq. 'im' .or.
     *          fset%rd(1:2) .eq. 'bv' .or. fset%rd(1:2) .eq. 'iv' .or.
     *          fset%rd(1:2) .eq. 'iM') then
              if (lookup_data(i)%rd .eq. fset%rd) then
                lookup_data(i)%sp = fset%sp
                add_line = .false.
              end if
            else if (fset%rd(1:2) .eq. 'lx') then
              if (lookup_data(i)%rd .eq. fset%rd) then
                lookup_data(i)%xy = fset%xy
                add_line = .false.
              end if
            else if (lookup_data(i)%df .eq. fset%df) then
              if (.not. is_complete_set(lookup_data(i))) then
                lookup_data(i)%sp = fset%sp
                lookup_data(i)%xy = fset%xy
                add_line = .false.
              else if (.not. is_complete_set(fset)) then
                add_line = .false.
              end if
            end if
          end do

c--   Add new line if not found previously

          if (add_line) then
            entries = entries + 1
            lookup_data(entries) = fset
          end if

        end subroutine


c-- LOAD_BDD_LOOKUP ------------------------------------------------------------
c
c   Loads the file_sets from a buoydd_lookup_table into the lookup_data array
c   The number of file_sets found is recorded in the 'entries' variable.
c
c-------------------------------------------------------------------------------
        subroutine load_bdd_lookup(df_name,lookup_data,entries,err_code,err_unit)

          character df_name*19, dskpath*100, table_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          logical    public
          type(file_set) lookup_data(LU_max_entries)

          table_name = 'bx'//df_name(3:13)
          public = .true.	!* all index files are public
          dskpath = get_file_path(table_name(1:19),err_code,public)
          if ( err_code .lt. 0 ) then
            write(err_unit,'(a)') 'ERROR: buoy index table path not found'
            return
          end if

          call open_read(temp_unit,dskpath,table_name,err_code,err_unit)
          if (err_code .ne. 0) then
            entries = 0
            return
          end if

          i = 0
          do while (err_code .eq. 0)
            i = i + 1
            lookup_data(i) = read_fileset(temp_unit,err_code)
          end do
          close(temp_unit)
          if (err_code .lt. 0) err_code = 0
          entries = i - 1

        end subroutine


c-- MAKE_BDD_LOOKUP ------------------------------------------------------------
c
c   Overwrites a buoydd_lookup_table with the filesets in lookup_data; helper 
c   routine for UPDATE_BDD_LOOKUP
c
c-------------------------------------------------------------------------------
        subroutine make_bdd_lookup(df_name,lookup_data,entries,err_code,err_unit)

          character df_name*19, dskpath*100, table_name*100, full_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          logical::  public = .true.	!* index files always public
          type(file_set) lookup_data(LU_max_entries)

          table_name = 'bx'//df_name(3:13)
          dskpath = get_file_path(table_name(1:19),err_code,public)
          if ( err_code .lt. 0 ) then
            write(err_unit,'(a)') 'ERROR: index table path not found'
            return
          end if

          call open_replace(temp_unit,dskpath,table_name,err_code,err_unit)
          if (err_code .ne. 0) return
          do i = 1, entries
            call write_fileset(lookup_data(i),temp_unit,err_code)
          end do
          close(temp_unit)
          full_name = TRIM(dskpath)//TRIM(table_name)
          sys_code = system('sort -u '//full_name//' | sort -k1.8 > '//
     *      TRIM(full_name)//'.tmp')
          sys_code = system('/usr/bin/mv '//TRIM(full_name)//
     *      '.tmp '//full_name)

        end subroutine


c-- MONTHLY_SP_LIST ------------------------------------------------------------
c
c   Creates an array containing all the sp filenames for a month
c
c-------------------------------------------------------------------------------
        subroutine monthly_sp_list(station,year,month,file_list)
          integer    count, entries, err_code, err_unit
          character  month*2, station*3, year*4
          character*19  file_list(LU_max_entries), df_name
          type(file_set) lookup_data(LU_max_entries)
          count = 0
          df_name = 'df'//station//'00'//year//month//'011200'
          call load_bdd_lookup(df_name,lookup_data,entries,err_code,err_unit)
          do i = 1, entries
            if (lookup_data(i)%sp(1:2) .eq. 'sp') then
              count = count + 1
              file_list(count) = lookup_data(i)%sp
            end if
          end do
        end subroutine

          
c-- MONTHLY_XY_LIST ------------------------------------------------------------
c
c   Creates a array containing all the xy filenames for a month
c   Modified to use channel '01' (instead of '00') Aug 2010
c
c-------------------------------------------------------------------------------
        subroutine monthly_xy_list(station,year,month,file_list)
          integer    count, entries, err_code, err_unit
          character  month*2, station*3, year*4
          character*19  file_list(LU_max_entries), df_name
          type(file_set) lookup_data(LU_max_entries)
          count = 0
          df_name = 'df'//station//'01'//year//month//'011200'
          call load_bdd_lookup(df_name,lookup_data,entries,err_code,err_unit)
          do i = 1, entries
            if (lookup_data(i)%xy(1:2) .eq. 'xy') then
              count = count + 1
              file_list(count) = lookup_data(i)%xy
            end if
          end do
        end subroutine

          
c-- IS_COMPLETE_SET ------------------------------------------------------------
c
c   Checks if a fileset has filenames in all four fields
c
c-------------------------------------------------------------------------------
        logical function is_complete_set(fset)
          type(file_set) fset
          if (fset%rd(1:2) .eq. 'rd' .and. fset%df(1:2) .eq. 'df' .and.
     *        fset%sp(1:2) .eq. 'sp' .and. fset%xy(1:2) .eq. 'xy') then
            is_complete_set = .true.
          else
            is_complete_set = .false.
          end if
        end function


c-- READ_FILESET, WRITE_FILESET ------------------------------------------------
c
c   Read and write filesets in the standard 4-column (rd df sp xy) format
c
c-------------------------------------------------------------------------------
        type(file_set) function read_fileset(input_unit,code)
          integer code, input_unit
          read(input_unit,'(a19,3(2x,a19))',iostat=code) read_fileset%rd,
     *      read_fileset%df, read_fileset%sp, read_fileset%xy
        end function


        subroutine write_fileset(fset,output_unit,code)
          integer code, output_unit
          type(file_set) fset
          write(output_unit,'(a19,3(2x,a19))',iostat=code) fset%rd,
     *      fset%df, fset%sp, fset%xy
        end subroutine


c-- FIND_INDEX_SET -------------------------------------------------------------
c
c   Reads the index_sets from a index file (ix08300200108) and returns the set 
c   that contains the given rd name. Sets err_code = 1 if no match found.
c
c-------------------------------------------------------------------------------
        type(index_set) function find_index_set(rd_name, sets, err_code)
          integer         entries, err_code
          character*19    rd_name
          type(index_set) sets(LU_max_entries)
c --  if the index table has not been loaded
c --  20020208 For some reason this "load_index_table" isn't working... BLS
          if (sets(1)%rd(1:2) .ne. 'rd') then
            call load_index_table(rd_name, sets, entries, err_code, 6)
            if (err_code .ne. 0) write(6,'(a)') 'Index file not found!'
          end if

          do i = 1, LU_max_entries
            if (sets(i)%rd .eq. rd_name) then
              err_code = 0
              find_index_set = sets(i)
              return
            end if
          end do
c --  Find closest match
          do i = 1, LU_max_entries
            if (sets(i)%rd(1:17) .eq. rd_name(1:17)) then
              err_code = 1
              find_index_set = sets(i)
              return
            end if
          end do
          err_code = 1		!* if specific entry not found
        end function


c-- UPDATE_INDEX_TABLE ---------------------------------------------------------
c
c   Adds an index_set to the appropriate index table.
c
c-------------------------------------------------------------------------------
        subroutine update_index_table(iset, err_code, err_unit)

          character  dskpath*100, table_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          logical    add_line
          type(index_set) iset, index_data(LU_max_entries)

          call load_index_table(iset%rd,index_data,entries,err_code,err_unit)
          err_code = 0
          add_line = .true.

c--   Check if entry already exists, update it if possible

          do i = 1, entries
            if (index_data(i)%rd .eq. iset%rd) then
              index_data(i) = iset
              add_line = .false.
            end if
          end do

c--   Add new line, recreate buoydd_lookup_table

          if (add_line) then
            entries = entries + 1
            index_data(entries) = iset
          end if
          call make_index_table(iset%rd,index_data,entries,err_code,err_unit)

        end subroutine


c-- MAKE_INDEX_TABLE -----------------------------------------------------------
c
c   Overwrites a index file (ix095...) with the index_sets in index_data; helper
c   routine for UPDATE_INDEX_TABLE 
c
c-------------------------------------------------------------------------------
        subroutine make_index_table(rd_name,index_data,entries,err_code,err_unit)
          character rd_name*19, dskpath*100, table_name*100, full_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          type(index_set) index_data(LU_max_entries)

          table_name = 'ix'//rd_name(3:5)//'00'//rd_name(8:13)
          dskpath = get_file_path(table_name(1:19),err_code)
          if ( err_code .lt. 0 ) then
            write(err_unit,'(a)') 'ERROR: index table path not found'
            return
          end if

          call open_replace(temp_unit,dskpath,table_name,err_code,err_unit)
          if (err_code .ne. 0) return
          do i = 1, entries
            call write_index_set(index_data(i),temp_unit,err_code)
          end do
          close(temp_unit)
          full_name = TRIM(dskpath)//TRIM(table_name)
          sys_code = system('sort -u '//full_name//' | sort -k1.8 > '//
     *      TRIM(full_name)//'.tmp')
          sys_code = system('/usr/bin/mv '//TRIM(full_name)//
     *      '.tmp '//full_name)

        end subroutine


c-- LOAD_INDEX_TABLE -----------------------------------------------------------
c
c   Reads an entire index_lookup_table into an array of index_set entries.
c
c-------------------------------------------------------------------------------
        subroutine load_index_table(rd_name,sets,entries,err_code,err_unit)
          character  rd_name*19, dskpath*100, table_name*100
          integer::  entries, err_unit, err_code, temp_unit = 99
          type(index_set) sets(LU_max_entries)

          entries = 0
          table_name = 'ix'//rd_name(3:5)//'00'//rd_name(8:13)
          dskpath = get_file_path(table_name(1:19),err_code)
          if ( err_code .lt. 0 ) then
            write(err_unit,'(a)') 'ERROR: Path not found in diskfarm'
            return
          end if

          call open_read(temp_unit,dskpath,table_name,err_code,err_unit)
          if (err_code .ne. 0) return
          i = 0
          do while (err_code .eq. 0)
            i = i + 1
            sets(i) = read_index_set(temp_unit,err_code)
          end do

          entries = i - 1
          if (err_code .eq. -1) err_code = 0	!* end-of-file, no error
          close(temp_unit)
        end subroutine


c-- WRITE_INDEX_SET ------------------------------------------------------------
c
c   Writes out an index_set in the standard n(a19,2x) format to the given unit.
c
c-------------------------------------------------------------------------------
        subroutine write_index_set(iset,output_unit,code)
          integer code, output_unit
          type(index_set) iset
          write(output_unit,'(a19,$)',iostat=code) iset%rd
          do i = 1, AI_max_channels
            if (iset%df(i)(1:2) .eq. 'df') then
              write(output_unit,'(2x,a19,$)',iostat=code) iset%df(i)
            end if
          end do
          write(output_unit,'()',iostat=code)
        end subroutine


c-- READ_INDEX_SET -------------------------------------------------------------
c
c   Initalizes an index_set from a line in the standard n(a19,2x) format.
c
c-------------------------------------------------------------------------------
        type(index_set) function read_index_set(input_unit,code)
          integer        chan, code, input_unit
          character*200  line
          type(index_set) empty_set	!* Never initialized; all ""

          read_index_set = empty_set	!* Clear out set
          read(input_unit,'(a200)',iostat=code) line
          if (code .eq. 0) then
            read(line(1:19),'(a19)') read_index_set%rd
            i = 22
            do while (i+18 .le. LEN_TRIM(line))
              read(line(i+5:i+6),'(i2)') chan
              read(line(i:i+18),'(a19)') read_index_set%df(chan)
              i = i + 21
            end do 
          end if
        end function


c-- GET_PROCDIR ----------------------------------------------------------------
c
c   Returns the processing directory for a given df filename (with stream),
c   e.g. df08304200102231430 and partition (e.g. mondat or mass08).
c
c-------------------------------------------------------------------------------
        character*100 function get_procdir(filename, partition)

          character filename*19, partition*6 
          integer mon

          read(filename(12:13),'(i2.2)') mon
          get_procdir = '/project/'//partition//'/'//month_names(mon)//
     *                  '/'//month_names(mon)//'.'//filename(3:5)//
     *                  '.'//filename(6:7)//'/'
        end function



c-- GET_FILE_PATH_PUB_KNOWN ----------------------------------------------------
c
c Calls get_lookup_path to return a 100 char file path given a 
c 19 char filename and if it is known whether the data are public or not.
c Don't call this function, call get_file_path with the logical is_public
c as the third argument.
c
c-------------------------------------------------------------------------------
        character*100 function get_file_path_pub_known(filename, err_code, 
     *                           is_public)

          type(lookup_block) lookup_info
          integer err_code
          character*19  filename
          logical is_public

          lookup_info =  init_lookup_info(filename(1:2), err_code)
          if ( err_code .ne. 0 ) return

          get_file_path_pub_known = 
     *        get_lookup_path(filename, lookup_info, is_public, err_code)


        end function


c-- GET_FILE_PATH_NO_PUB -------------------------------------------------------
c
c Opens an archive to determine if the data are public or not, then calls 
c get_file_path_pub_known to return a 100 char file path given a 
c 19 char filename.
c
c   Modified 
c	Grant A. Cameron	04/17/2002 	Except ix files for pub lookup
c	Grant A. Cameron	08/23/2002 	Except rd files for pub lookup
c-------------------------------------------------------------------------------
        character*100 function get_file_path_no_pub(filename, err_code)

          character filename*19, tmp_name*19, station*3, stream*2
          integer :: farm_unit, err_code, err_unit=6
          logical is_public, found

          type(ai_time_frame) fl_frame
          type(ps_time_frame) ps_frame

          err_code = 0
          get_file_path_no_pub = ''
          if ( LEN_TRIM(filename) .eq. 13 ) then
            tmp_name = TRIM(filename)//'000000'
          else
            tmp_name = filename
          end if
          if ( filename(1:2) .eq. 'mp' .or. filename(1:2) .eq. 'fs' .or. 
     *      filename(1:2) .eq. 'fp') then
            is_public = .true.
          else if ( filename(6:6) .eq. 'p' ) then
            ps_frame = get_file_psframe(filename, err_code, err_unit)
            if ( err_code .ne. 0 ) return
            is_public = ps_frame%is_public
          else if ( filename(2:2) .ne. 'x' .and. filename(1:2) .ne. 'rd' .and.
     *      filename(1:2) .ne. 'ls' .and. filename(1:2) .ne. 'lx' .and.
     *      filename(1:2) .ne. 'is' .and. filename(1:2) .ne. 'hv' .and.
     *      filename(1:2) .ne. 'im' .and. filename(1:2) .ne. 'bv' .and.
     *      filename(1:2) .ne. 'iv' .and. filename(1:2) .ne. 'iM') then
            fl_frame = get_file_frame(tmp_name, err_code) 
            if ( err_code .ne. 0 ) return
            is_public = fl_frame%is_public
          end if
          if ( err_code .ne. 0 ) return
          get_file_path_no_pub = get_file_path_pub_known(filename, err_code, 
     *                               is_public)
          
        end function


c-- INIT_LOOKUP_INFO -----------------------------------------------------------
c
c Given a file_prefix, returns file_type, in a lookup_block
c Returns err_code=1 if file_prefix not found
c
c-------------------------------------------------------------------------------
       type(lookup_block) function init_lookup_info(file_prefix, err_code)

          character*2 file_prefix
          integer idx, err_code

          idx = 1
          do while ( idx .le. LU_num_file_prefixes .and. file_prefixes(idx) .ne. file_prefix)
            idx = idx + 1
          end do
          if ( idx .le. LU_num_file_prefixes ) then
            init_lookup_info%file_type = file_types(idx)
            init_lookup_info%data_location = data_locations(idx)
            init_lookup_info%path_type = path_types(idx)
            init_lookup_info%file_prefix = file_prefix
            err_code = 0
          else
            err_code = 1
          end if
          
       end function


c-- GET_LOOKUP_PATH ------------------------------------------------------------
c
c Uses the lookup table array to return a 100 char file path given a
c 19 char file name. Use this if you will need to use the same lookup table 
c repeatedly (but first you must call load_lookup_table), otherwise use
c get_file_path (interfaced with get_file_path_pub_known and 
c get_file_path_no_pub. 
c
c This works on filenames with the following prefixes: df, sp, de, db, dw, dt, 
c pm, rd or ix.
c
c Error codes are:
c   -1. bombed out somehow...
c    0. if the file (and directory) exists,
c    1. if the path/file is known but does not yet exist.
c
c Note, this routine can be used with a partial filename, e.g. 'df07300199912'.
c
c Modified
c	Grant A. Cameron	08/23/2002	Added path_type=4
c	Grant A. Cameron	01/02/2003	added -2,2 error codes
c	corey                   07/19/2004      removed use of lookup table
c-------------------------------------------------------------------------------
       character*100 function get_lookup_path(filename, lookup_info, 
     *                                                     is_public, err_code)
       
         type(lookup_block) lookup_info
         character file_type*3, cyear*4, cmonth*2
         character stn*3, stream*2
         character*3 months(12)
         character*11 pub_str
         character filename*19, full_name*100, compressed_name*100
         integer err_code, imon
         logical is_public, exists

          data months
     *     /'jan', 'feb', 'mar', 'apr', 'may', 'jun',
     *      'jul', 'aug', 'sep', 'oct', 'nov', 'dec'/

          err_code = -1  !* file not found and path not found

          cyear = filename(8:11)
          cmonth = filename(12:13)
          read(cmonth,'(i2)') imon
          stn = filename(3:5)
          stream = filename(6:7)

          if ( is_public ) then
            pub_str = 'PUBLIC_DATA'
          else
            pub_str = 'NONPUB_DATA'
          end if
          if ( filename(2:2) .eq. 'x' ) pub_str = 'INDEX_FILES'
          if ( filename(1:2) .eq. 'rd' ) pub_str = 'SOURCE_DATA'
          if ( filename(1:2) .eq. 'ls' .or. filename(1:2) .eq. 'lx' .or.
     *      filename(1:2) .eq. 'is' .or. filename(1:2) .eq. 'hv' .or.
     *      filename(1:2) .eq. 'im' .or. filename(1:2) .eq. 'bv' .or.
     *      filename(1:2) .eq. 'iv' .or. filename(1:2) .eq. 'iM') 
     *      pub_str = 'SOURCE_DATA'

          get_lookup_path = ''

          if ( lookup_info%path_type .eq. 1 ) then
            get_lookup_path = '/project/'//lookup_info%data_location//'/'
     *          //pub_str//'/'//lookup_info%file_type//'_'//cyear//'/'//stn
     *          //'/'//months(imon)//'/'//stream//'/'
          else if ( lookup_info%path_type .eq. 2 ) then
            get_lookup_path = '/project/'//lookup_info%data_location//'/'
     *          //pub_str//'/'//lookup_info%file_type//'_'//cyear//'/'//stn
     *          //'/'//stream//'/'
          else if ( lookup_info%path_type .eq. 3 ) then
            get_lookup_path = '/project/'//lookup_info%data_location//'/'
     *          //pub_str//'/'//lookup_info%file_type//'_'//cyear//'/'//stn//'/'
          else 
            get_lookup_path = '/project/'//lookup_info%data_location//'/'
     *          //pub_str//'/'//lookup_info%file_type//'_'//cyear//'/'
     *          //stn//'/'//months(imon)//'/'
          end if

          err_code = 1  !* path found
          full_name = TRIM(get_lookup_path)//filename
          inquire(file=full_name,exist=exists)
          if ( exists ) then
            err_code = 0  !* file found
          else
            compressed_name = TRIM(full_name)//'.Z'
            inquire(file=compressed_name,exist=exists)
            if ( exists ) err_code = 0
          end if

       end function


c-- GET_LOOKUP_IDX -------------------------------------------------------------
c
c For a given file_prefix, returns corresponding array index.
c
c-------------------------------------------------------------------------------
       integer function get_lookup_idx(file_prefix)

         character *2 file_prefix
         integer i
         logical continue_loop

         i = 1
         continue_loop = .false.
         if (file_prefixes(i) .ne. file_prefix) continue_loop = .true.
         do while (continue_loop)
           i = i + 1
           continue_loop = .false.
           if (i .le. LU_num_file_prefixes) then
             if (file_prefixes(i) .ne. file_prefix) continue_loop = .true.
           end if
         end do
         if ( i .eq. LU_num_file_prefixes + 1 ) then
           get_lookup_idx = 0
         else
           get_lookup_idx = i
         end if

       end function

      end
