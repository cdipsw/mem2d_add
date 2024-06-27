c-- NETCDF_UTILS ---------------------------------------------------------------
c
c   Routines for use with netcdf files of any type.
c-------------------------------------------------------------------------------
      module netcdf_utils

      use dates 
      use netcdf

      implicit none

      save 

      integer,parameter::   NC_int_fill = -99999
      real,parameter::      NC_real_fill = -999.99
      character,parameter:: NC_char_fill = '_'
      byte,parameter::      NC_byte_fill = -127
      integer*2,parameter:: NC_short_fill = -32768

      character*7,parameter::      NC_real_fill_string = '-999.99'

      character*22,parameter::     NC_cctype_image = 'image                 '
      character*22,parameter::     NC_cctype_theme = 'thematicClassification'
      character*22,parameter::     NC_cctype_measr = 'physicalMeasurement   '
      character*22,parameter::     NC_cctype_ainfo = 'auxiliaryInformation  '
      character*22,parameter::     NC_cctype_qinfo = 'qualityInformation    '
      character*22,parameter::     NC_cctype_rinfo = 'referenceInformation  '
      character*22,parameter::     NC_cctype_model = 'modelResult           '
      character*22,parameter::     NC_cctype_coord = 'coordinate            '

      contains


c-- NC_VARIABLE_INVENTORY ------------------------------------------------------
c  Returns an array of group/variable names for the given ncid.
c-------------------------------------------------------------------------------
        subroutine nc_variable_inventory(ncid, vlist, vcount)
          integer ncid, vcount
          character*100  vlist(1000), gparent

          gparent = ''
          vcount = 0
          call nc_vars_and_grps(ncid, gparent, vlist, vcount)
        end subroutine

        recursive subroutine nc_vars_and_grps(grpid, gparent, vlist, vcount)
          integer grpid, grpids(100), i, ngrps, nvars, varid, varids(1000), vcount
          character*100  gname, gparent, nparent, vname, vlist(1000)

          call nc_call_func(nf90_inq_grpname(grpid, gname))
          if (TRIM(gname) .ne. '/') gname = TRIM(gname)//'/'
          nparent = TRIM(gparent)//TRIM(gname)

          call nc_call_func(nf90_inq_varids(grpid, nvars, varids))
          do i = 1, nvars
            call nc_call_func(nf90_inquire_variable(grpid, varids(i), vname))
            vcount = vcount + 1
            vlist(vcount) = TRIM(nparent)//TRIM(vname)
          end do

          call nc_call_func(nf90_inq_grps(grpid, ngrps, grpids))
          do i = 1, ngrps
            call nc_vars_and_grps(grpids(i), nparent, vlist, vcount)
          end do
        end subroutine


c-------------------------------------------------------------------------------
c  NC_COPY_VARIABLES copies variables and attributes to a second netcdf file.
c  Only variables starting with prefix are included.
c-------------------------------------------------------------------------------
        subroutine nc_copy_variables(ncid1, ncid2, prefix, ecode)
          integer   ecode, i, ncid1, ncid2, vcount
          character*100  prefix, vlist(1000), vname

c--  Open first file, get list of variables

          call nc_variable_inventory(ncid1, vlist, vcount)

          do i = 1, vcount
            if (vlist(i)(1:LEN_TRIM(prefix)) .eq. prefix) then
              vname = TRIM(vlist(i)(2:))
              call nc_copy_variable(ncid1, ncid2, vname, ecode)
            end if
          end do

      end subroutine


c-------------------------------------------------------------------------------
c  NC_COPY_GLOBAL_ATTS copies NF90_GLOBAL attributes to a second netcdf file.
c-------------------------------------------------------------------------------
      subroutine nc_copy_global_atts(ncid1, ncid2, ecode)
        integer   ecode, i, ncid1, ncid2, natts
        character*100  att_name

        call nc_call_func(nf90_inquire(ncid1, nAttributes=natts))
        do i = 1, natts
          call nc_call_func(nf90_inq_attname(ncid1, NF90_GLOBAL, i, att_name))
          call nc_call_func(nf90_copy_att(ncid1, NF90_GLOBAL, att_name, ncid2, NF90_GLOBAL))
        end do
      end subroutine


c-------------------------------------------------------------------------------
c  NC_COPY_VARIABLE_ATTS copies a variable's attributes to a second netcdf file.
c-------------------------------------------------------------------------------
      subroutine nc_copy_variable_atts(ncid1, ncid2, vname, ecode)
        integer   ecode, i, ncid1, ncid2, numatts, varid1, varid2
        character*(*)   vname
        character*100   att_name

        call nc_call_func(nf90_inq_varid(ncid1, vname, varid1))
        call nc_call_func(nf90_inq_varid(ncid2, vname, varid2))

        call nc_call_func(nf90_inquire_variable(ncid1, varid1, nAtts=numatts))
        call nc_att_copy_helper(ncid1, ncid2, varid1, varid2, numatts)
      end subroutine


      subroutine nc_att_copy_helper(ncid1, ncid2, varid1, varid2, att_count)
        integer          att_count, i, ncid1, ncid2, varid1, varid2
        character*100    att_name

        do i = 1, att_count
          call nc_call_func(nf90_inq_attname(ncid1, varid1, i, att_name))
          call nc_call_func(nf90_copy_att(ncid1, varid1, att_name, ncid2, varid2))
        end do
      end subroutine


c-------------------------------------------------------------------------------
c  NC_COPY_VARIABLE copies a single variable with atts to a second dataset.
c-------------------------------------------------------------------------------
        subroutine nc_copy_variable(ncid1, ncid2, vname, ecode)
          integer          count(1), counts(2), dimids(10), dlen, dlen1, dlen2, ecode, i, ival, j, k,
     *      natts, ncid1, ncid2, ndims, new_dimid, new_dimids(2), start(1), starts(2), vid1, vid2, xtype
          real             rval
          byte             bval
          character        cval
          character*100    att_name, dname, vname

          integer,allocatable::   ivals(:), ivals_2d(:,:)
          real,allocatable::      rvals(:), rvals_2d(:,:)
          byte,allocatable::      bvals(:), bvals_2d(:,:)

          call nc_call_func(nf90_inq_varid(ncid1, vname, vid1))
          write(6,*) ' match: ', TRIM(vname), vid1
          call nc_call_func(nf90_inquire_variable(ncid1, vid1, vname, xtype, ndims, dimids, natts))

          if (ndims .le. 2) then
            if (ndims .eq. 0) then
              ecode = nf90_redef(ncid2)
              call nc_call_func(nf90_def_var(ncid2, vname, xtype, varid=vid2))
              call nc_att_copy_helper(ncid1, ncid2, vid1, vid2, natts)
              call nc_call_func(nf90_enddef(ncid2))
              if (xtype .eq. NF90_FLOAT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, rval))
                call nc_call_func(nf90_put_var(ncid2, vid2, rval))
              else if (xtype .eq. NF90_INT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, ival))
                call nc_call_func(nf90_put_var(ncid2, vid2, ival))
              else if (xtype .eq. NF90_CHAR) then
                call nc_call_func(nf90_get_var(ncid1, vid1, cval))
                call nc_call_func(nf90_put_var(ncid2, vid2, cval))
              else if (xtype .eq. NF90_BYTE) then
                call nc_call_func(nf90_get_var(ncid1, vid1, bval))
                call nc_call_func(nf90_put_var(ncid2, vid2, bval))
              end if

            else if (ndims .eq. 1) then
              ecode = nf90_redef(ncid2)
              call nc_call_func(nf90_inquire_dimension(ncid1, dimids(1), dname, dlen))
              ecode = nf90_inq_dimid(ncid2, dname, new_dimid)
              if (ecode .ne. NF90_NOERR) call nc_call_func(nf90_def_dim(ncid2, dname, dlen, new_dimid))
              call nc_call_func(nf90_def_var(ncid2, vname, xtype, new_dimid, vid2))
              call nc_att_copy_helper(ncid1, ncid2, vid1, vid2, natts)
              call nc_call_func(nf90_enddef(ncid2))
              allocate(ivals(dlen), rvals(dlen), bvals(dlen))
              if (xtype .eq. NF90_FLOAT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, rvals))
                call nc_call_func(nf90_put_var(ncid2, vid2, rvals))
              else if (xtype .eq. NF90_INT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, ivals))
                call nc_call_func(nf90_put_var(ncid2, vid2, ivals))
              else if (xtype .eq. NF90_BYTE) then
                call nc_call_func(nf90_get_var(ncid1, vid1, bvals))
                call nc_call_func(nf90_put_var(ncid2, vid2, bvals))
              else if (xtype .eq. NF90_CHAR) then
                count(1) = 1
                do j = 1, dlen
                  start(1) = j
                  call nc_call_func(nf90_get_var(ncid1, vid1, cval, start, count))
                  call nc_call_func(nf90_put_var(ncid2, vid2, cval, start, count))
                end do
              end if

            else if (ndims .eq. 2) then
              ecode = nf90_redef(ncid2)
              call nc_call_func(nf90_inquire_dimension(ncid1, dimids(1), dname, dlen1))
              ecode = nf90_inq_dimid(ncid2, dname, new_dimids(1))
              if (ecode .ne. NF90_NOERR) call nc_call_func(nf90_def_dim(ncid2, dname, dlen1, new_dimids(1)))
              call nc_call_func(nf90_inquire_dimension(ncid1, dimids(2), dname, dlen2))
              ecode = nf90_inq_dimid(ncid2, dname, new_dimids(2))
              if (ecode .ne. NF90_NOERR) call nc_call_func(nf90_def_dim(ncid2, dname, dlen2, new_dimids(2)))
              call nc_call_func(nf90_def_var(ncid2, vname, xtype, new_dimids, vid2))
              call nc_att_copy_helper(ncid1, ncid2, vid1, vid2, natts)
              call nc_call_func(nf90_enddef(ncid2))
              allocate(ivals_2d(dlen1, dlen2), rvals_2d(dlen1, dlen2), bvals_2d(dlen1, dlen2))
              if (xtype .eq. NF90_FLOAT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, rvals_2d))
                call nc_call_func(nf90_put_var(ncid2, vid2, rvals_2d))
              else if (xtype .eq. NF90_INT) then
                call nc_call_func(nf90_get_var(ncid1, vid1, ivals_2d))
                call nc_call_func(nf90_put_var(ncid2, vid2, ivals_2d))
              else if (xtype .eq. NF90_BYTE) then
                call nc_call_func(nf90_get_var(ncid1, vid1, bvals_2d))
                call nc_call_func(nf90_put_var(ncid2, vid2, bvals_2d))
              else if (xtype .eq. NF90_CHAR) then
                counts(1) = 1
                counts(2) = 1
                do j = 1, dlen1
                  do k = 1, dlen2
                    starts(1) = j
                    starts(2) = k
                    call nc_call_func(nf90_get_var(ncid1, vid1, cval, starts, counts))
                    call nc_call_func(nf90_put_var(ncid2, vid2, cval, starts, counts))
                  end do
                end do
              end if
            end if

          else
            write(6,*) '  nc_copy_variable: dimension > 2, omitting'
            ecode = 1
          end if
        end subroutine



c-- NC_FIND_FREQ_INDEX ---------------------------------------------------------
c  Finds the frequency index within an array equal or closest to the given freq.
c  The 'freqs' argument is the freqs variable of a netcdf dataset.
c  Freq target types: 1 - closest to target;   2 - closest <= target;
c                     3 - closest >= target;   4 - exactly target
c-------------------------------------------------------------------------------
        subroutine nc_find_freq_index(freqs, freq_count, target_freq, 
     *               target_type, cloc, found)
          logical found
          integer cloc, freq_count, i
          integer target_index, target_type
          real    padding, diff, freqs(*), min_diff, target_freq

          cloc = -1
          padding = 0.00001		!* to avoid real precision errors
          min_diff = HUGE(min_diff)
          do i = 1, freq_count
            diff = ABS(freqs(i) - target_freq)
            if (diff < min_diff) then
              cloc = i
              min_diff = diff
            end if
          end do

          found = .true.
          if (target_type .eq. 1) then
            if (cloc .eq. -1) found = .false.
          else if (target_type .eq. 2 .and. freqs(cloc) .gt. target_freq+padding) then
            if (cloc .gt. 1) then
              cloc = cloc - 1
            else
              found = .false.
            end if
          else if (target_type .eq. 3 .and. freqs(cloc) .lt. target_freq-padding) then
            if (cloc .lt. freq_count) then
              cloc = cloc + 1
            else 
              found = .false.
            end if
          else if (target_type .eq. 4) then
            if (freqs(cloc) .lt. target_freq-padding .or. freqs(cloc) .gt. target_freq+padding) found = .false.
          end if

          return
        end subroutine


c-- NC_FIND_TIME_INDEX ---------------------------------------------------------
c  Finds the time index within an array equal or closest to the given time.  
c  The 'times' argument is the times variable of a netcdf dataset.
c  Time target types: 1 - closest to target;   2 - closest pre-target;
c                     3 - closest post-target; 4 - exactly target
c-------------------------------------------------------------------------------
        subroutine nc_find_time_index(times, recs, target_date, target_type, 
     *                                cloc, found)
          logical found, last_loop
          integer cloc, step, recs
          integer target_index, target_time, target_type, times(*)
          type(date_block) target_date, base_date

          if (recs .eq. 0) then
            found = .false.
            return
          end if

          base_date = init_date(1970, 1, 1, 0, 0, 0)
          target_time = secs_diff(base_date, target_date)
          cloc = MAX(1,recs/2)
          step = CEILING(recs/4.0)
          last_loop = .false.

          do while (times(cloc) .ne. target_time .and. step .ge. 1)
c           write(6,*) 'cloc, step', cloc, step
            if (times(cloc) .gt. target_time) then
              cloc = cloc - step
              if (cloc .lt. 1) cloc = 1
            else if (cloc .lt. recs) then
              cloc = cloc + step
              if (cloc .gt. recs) cloc = recs
            end if
            if (step .gt. 1) then
              step = CEILING(step/2.0)
            else if (step .eq. 1 .and. .not. last_loop) then
              step = 1
              last_loop = .true.
            else 
              step = 0
            end if
          end do

          found = .true.
          if (target_type .eq. 1) then
            if (times(cloc) .lt. target_time .and. cloc .lt. recs
     *          .and. (times(cloc+1)-target_time) .lt. 
     *          (target_time-times(cloc))) then
              cloc = cloc + 1
            else if (times(cloc) .gt. target_time .and. cloc .gt. 1
     *          .and. (times(cloc)-target_time) .gt. 
     *          (target_time-times(cloc-1))) then
              cloc = cloc - 1
            end if
          else if (target_type .eq. 2 .and. times(cloc) .gt. target_time) then
            if (cloc .gt. 1) then
              cloc = cloc - 1
            else
              found = .false.
            end if
          else if (target_type .eq. 3 .and. times(cloc) .lt. target_time) then
            if (cloc .lt. recs) then
              cloc = cloc + 1
            else 
              found = .false.
            end if
          else if (target_type .eq. 4) then
            if (times(cloc) .ne. target_time) found = .false.
          end if
            
          return
        end subroutine


c-- NC_FIND_MILLISEC_INDEX -----------------------------------------------------
c  Finds the time index within an array equal or closest to the given time.  
c  The 'times' argument is 'millisecs from 1970-01-01'.
c  Time target types: 1 - closest to target;   2 - closest pre-target;
c                     3 - closest post-target; 4 - exactly target
c-------------------------------------------------------------------------------
        subroutine nc_find_millisec_index(times, recs, target_date, target_type, 
     *                                cloc, found)
          logical found, last_loop
          integer cloc, step, recs
          integer target_index, target_type
          real*8  target_time, times(*)
          type(date_block) target_date, base_date

          if (recs .eq. 0) then
            found = .false.
            return
          end if

          base_date = init_date(1970, 1, 1, 0, 0, 0)
          target_time = DBLE(secs_diff(base_date, target_date)) * DBLE(1000.0)
          cloc = MAX(1,recs/2)
          step = CEILING(recs/4.0)
          last_loop = .false.

          do while (times(cloc) .ne. target_time .and. step .ge. 1)
c           write(6,*) 'cloc, step', cloc, step
            if (times(cloc) .gt. target_time) then
              cloc = cloc - step
              if (cloc .lt. 1) cloc = 1
            else if (cloc .lt. recs) then
              cloc = cloc + step
              if (cloc .gt. recs) cloc = recs
            end if
            if (step .gt. 1) then
              step = CEILING(step/2.0)
            else if (step .eq. 1 .and. .not. last_loop) then
              step = 1
              last_loop = .true.
            else 
              step = 0
            end if
          end do

          found = .true.
          if (target_type .eq. 1) then
            if (times(cloc) .lt. target_time .and. cloc .lt. recs
     *          .and. (times(cloc+1)-target_time) .lt. 
     *          (target_time-times(cloc))) then
              cloc = cloc + 1
            else if (times(cloc) .gt. target_time .and. cloc .gt. 1
     *          .and. (times(cloc)-target_time) .gt. 
     *          (target_time-times(cloc-1))) then
              cloc = cloc - 1
            end if
          else if (target_type .eq. 2 .and. times(cloc) .gt. target_time) then
            if (cloc .gt. 1) then
              cloc = cloc - 1
            else
              found = .false.
            end if
          else if (target_type .eq. 3 .and. times(cloc) .lt. target_time) then
            if (cloc .lt. recs) then
              cloc = cloc + 1
            else 
              found = .false.
            end if
          else if (target_type .eq. 4) then
            if (times(cloc) .ne. target_time) found = .false.
          end if
            
          return
        end subroutine


c-- NC_FIND_SAMPLE_INDEX -------------------------------------------------------
c  Finds the index within an array equal or closest to the given time for
c  variables indexed at a constant sample rate (srate).
c  Time target types: 1 - closest to target;   2 - closest pre-target;
c                     3 - closest post-target; 4 - exactly target
c-------------------------------------------------------------------------------
        subroutine nc_find_sample_index(stime, srate, recs, target_time, 
     *               target_type, cloc, found)
          logical found
          real    srate
          real*8  recs_from_start, dec_part
          integer cloc, recs, stime, target_time, target_type
          integer*8  cloc8

          if (recs .eq. 0) then
            found = .false.
            return
          end if

          recs_from_start = (target_time-stime)*DBLE(srate)
          dec_part = DMOD(recs_from_start, DBLE(1.0))
          cloc8 = recs_from_start - dec_part + 1
          if (dec_part .gt. 0.50) cloc8 = cloc8 + 1
c         cloc8 = NINT(recs_from_start) + 1
          if (target_type .eq. 2 .and. cloc8 .gt. 1) then
            cloc8 = cloc8 - 1
          else if (target_type .eq. 3 .and. cloc8 .lt. recs) then
            cloc8 = cloc8 + 1
          end if

          if ((target_type .eq. 1 .or. target_type .eq. 2) .and. cloc8 .gt. recs) cloc8 = recs
          if ((target_type .eq. 1 .or. target_type .eq. 3) .and. cloc8 .lt. 1) cloc8 = 1

          cloc = cloc8
          if (cloc .ge. 1 .and. cloc .le. recs) then
            found = .true.
          else
            found = .false.
          end if
          return
        end subroutine


c-- NC_PRINT_DIMENSIONLESS_VAR -------------------------------------------------
c  Writes the values of a dimensionless variable to the given output unit
c-------------------------------------------------------------------------------
        subroutine nc_print_dimensionless_var(grpid, varid, vtype, ounit)
          integer      errcode, grpid, varid, ounit, ival, vtype
          character    cval
          real         rval
          real*8       dval

          errcode = NF90_NOERR
          if (vtype .eq. NF90_BYTE .or. vtype .eq. NF90_SHORT .or. vtype .eq. NF90_INT) then
            errcode = nf90_get_var(grpid, varid, ival)
            if (errcode .eq. NF90_NOERR) write(ounit,*) ival
          else if (vtype .eq. NF90_FLOAT) then
            errcode = nf90_get_var(grpid, varid, rval)
            if (errcode .eq. NF90_NOERR) write(ounit,*) rval
          else if (vtype .eq. NF90_DOUBLE) then
            errcode = nf90_get_var(grpid, varid, dval)
            if (errcode .eq. NF90_NOERR) write(ounit,*) dval
          else if (vtype .eq. NF90_CHAR) then
            errcode = nf90_get_var(grpid, varid, cval)
            if (errcode .eq. NF90_NOERR) write(ounit,'(a1)') cval
          else
            write(ounit,'(a)') 'nc_print_dimensionless_var(): type unknown'
          end if
          if (errcode .ne. NF90_NOERR) write(ounit,'(a)') 'nc_print_dimensionless_var(): error'  

          return
        end subroutine


c-- NC_CALL_FUNC ---------------------------------------------------------------
c  Wrapper for call to NETCDF library routines; stops on any errors.
c-------------------------------------------------------------------------------
        subroutine nc_call_func(status)
          integer, intent(in) :: status
    
          if ( status .ne. NF90_NOERR) then 
            write(6,'(a)') NF90_STRERROR(status)
            call nc_stop_program()
          end if
        end subroutine 


c-- NC_STOP_PROGRAM ------------------------------------------------------------
c  Stops execution; broken out for ease of debugging
c-------------------------------------------------------------------------------
        subroutine nc_stop_program()
          stop "Stopped"
        end subroutine 


c-- NC_ASSIGN_ATTRIBUTES -------------------------------------------------------
c  Assigns the standard attributes to the given var: name, units, fill.
c  Values for fill_type are 'REAL', 'INT', 'CHAR', 'BYTE', and 'SHORT'
c-------------------------------------------------------------------------------
        subroutine nc_assign_attributes(grpid, varid, longname, units,
     *               fill_type, stdname, coords, minv, maxv, ancillary, nodc, cctype)
          integer::                  grpid, varid
          real, optional::           minv, maxv
          byte                       bmin, bmax
          character*4                null_att
          character*(*)              longname, units, fill_type
          character*(*), optional::  ancillary, cctype, coords, nodc, stdname

            null_att = 'NULL'
            if (longname .ne. null_att)
     *        call nc_call_func(nf90_put_att(grpid, varid, 'long_name', TRIM(longname)))
            if (units .ne. null_att)
     *        call nc_call_func(nf90_put_att(grpid, varid, 'units', TRIM(units)))

            if (fill_type .eq. 'REAL') then
              call nc_call_func(nf90_put_att(grpid, varid, '_FillValue', NC_real_fill))
            else if (fill_type .eq. 'INT') then
              call nc_call_func(nf90_put_att(grpid, varid, '_FillValue', NC_int_fill))
            else if (fill_type .eq. 'CHAR') then
              call nc_call_func(nf90_put_att(grpid, varid, '_FillValue', NC_char_fill))
            else if (fill_type .eq. 'BYTE') then
              call nc_call_func(nf90_put_att(grpid, varid, '_FillValue', NC_byte_fill))
            else if (fill_type .eq. 'SHORT') then
              call nc_call_func(nf90_put_att(grpid, varid, '_FillValue', NC_short_fill))
            end if

            if (PRESENT(stdname)) then
              if (stdname .ne. null_att) call nc_call_func(nf90_put_att(grpid, varid, 'standard_name', TRIM(stdname)))
            end if

            if (PRESENT(coords)) then
              call nc_call_func(nf90_put_att(grpid, varid, 'coordinates', TRIM(coords)))
              call nc_call_func(nf90_put_att(grpid, varid, 'grid_mapping', 'metaGridMapping'))
            end if
            if (fill_type .eq. 'INT') then
              if (PRESENT(minv)) call nc_call_func(nf90_put_att(grpid, varid, 'valid_min', NINT(minv)))
              if (PRESENT(maxv)) call nc_call_func(nf90_put_att(grpid, varid, 'valid_max', NINT(maxv)))
            else if (fill_type .eq. 'BYTE') then
              if (PRESENT(minv)) then
                bmin = minv
                call nc_call_func(nf90_put_att(grpid, varid, 'valid_min', bmin))
              end if
              if (PRESENT(maxv)) then
                bmax = maxv
                call nc_call_func(nf90_put_att(grpid, varid, 'valid_max', bmax))
              end if
            else
              if (PRESENT(minv)) call nc_call_func(nf90_put_att(grpid, varid, 'valid_min', minv))
              if (PRESENT(maxv)) call nc_call_func(nf90_put_att(grpid, varid, 'valid_max', maxv))
            end if
            if (PRESENT(ancillary)) call nc_call_func(nf90_put_att(grpid,varid, 'ancillary_variables', TRIM(ancillary)))
            if (PRESENT(nodc)) call nc_call_func(nf90_put_att(grpid, varid, 'ncei_name', TRIM(nodc)))
            if (PRESENT(cctype)) call nc_call_func(nf90_put_att(grpid, varid, 'coverage_content_type', TRIM(cctype)))
        end subroutine


c-- NC_ASSIGN_SCALE_ATTS -------------------------------------------------------
c  Assigns offset: and scale_factor: attributes for packed variables
c-------------------------------------------------------------------------------
        subroutine nc_assign_scale_atts(grpid, varid, scale_factor, add_offset)
          integer       grpid, varid
          real          add_offset, scale_factor

          call nc_call_func(nf90_put_att(grpid, varid, 'scale_factor', scale_factor))
          call nc_call_func(nf90_put_att(grpid, varid, 'add_offset', add_offset))
        end subroutine


c-- NC_PACK_PUT_VAR ------------------------------------------------------------
c  Packs a variable using the given scale_factor and add_offset, then writes it
c  Used for packing a 4-byte real into a 2-byte integer
c-------------------------------------------------------------------------------
        integer function nc_pack_put_var(grpid, varid, vals, vcount, scale_factor, add_offset, istart)
          integer                  grpid, tcount(1), tstart(1), varid, vsize, vcount
          integer,optional::       istart
          integer*2,allocatable::  fill_vals(:), packed_vals(:)
          real                     add_offset, scale_factor, vals(vcount)

          if (PRESENT(istart)) then
            tstart(1) = istart
          else
            tstart(1) = 1
          end if
          tcount(1) = vcount

          ALLOCATE(packed_vals(vcount), fill_vals(vcount))
          fill_vals = NC_short_fill
          packed_vals = NINT(vals * (1.0/scale_factor) - add_offset)
          packed_vals = MERGE(fill_vals, packed_vals, (vals .eq. NC_real_fill))
          nc_pack_put_var = nf90_put_var(grpid, varid, packed_vals, tstart, tcount)
          DEALLOCATE(packed_vals)
        end function


c-- NC_GET_UNPACK_VAR ----------------------------------------------------------
c  Reads a var and then unpacks using the given scale_factor and add_offset
c  Used for unpacking a 2-byte integer into a 4-byte real
c-------------------------------------------------------------------------------
        integer function nc_get_unpack_var(grpid, varid, vals, vcount, istart, istride)
          integer                  errcode, grpid, tcount(1), tstart(1), tstride(1), varid, vsize, vcount
          integer,optional::       istart, istride
          integer*2,allocatable::  packed_vals(:)
          real                     add_offset, scale_factor, vals(vcount)

          tstart(1) = 1
          tstride(1) = 1
          tcount(1) = vcount
          if (PRESENT(istart)) tstart(1) = istart
          if (PRESENT(istride)) tstride(1) = istride

          ALLOCATE(packed_vals(vcount))
          errcode = nf90_get_var(grpid, varid, packed_vals, tstart, tcount, tstride)
          if (errcode .ne. NF90_NOERR) then
            DEALLOCATE(packed_vals)
            nc_get_unpack_var = errcode
            return
          end if

          vals = REAL(packed_vals)
          errcode = nf90_get_att(grpid, varid, 'scale_factor', scale_factor)
          if (errcode .eq. 0) vals = vals * scale_factor
          errcode = nf90_get_att(grpid, varid, 'add_offset', add_offset)
          if (errcode .eq. 0) vals = vals + add_offset

          DEALLOCATE(packed_vals)
          nc_get_unpack_var = NF90_NOERR
        end function

      end !* END MODULE
