      subroutine manure_orgmin_read
      
      use input_file_module
      use maximum_data_module
      use fertilizer_data_module
      
      implicit none
   
      integer :: it = 0               !none       |counter
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      integer :: mfrt = 0             !           |
      logical :: i_exist              !none       |check to determine if file exists
      
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file="manure_om.mnu", exist=i_exist)
      if (.not. i_exist .or. "manure_om.mnu" == "null") then
         allocate (manure_db(0:0))
      else
      do  
        open (107,file="manure_om.mnu")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof == 0) 
             read (107,*,iostat=eof) titldum
             if (eof < 0) exit
             imax = imax + 1
           end do
           
        allocate (manure_om(0:imax))
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        do it = 1, imax
          read (107,*,iostat=eof) manure_om(it)%name, manure_om(it)% region,             &
            manure_om(it)%source, manure_om(it)%typ,manure_om(it)%pct_moisture,          &
            manure_om(it)%pct_solids, manure_om(it)%tot_c, manure_om(it)%tot_n,          &
            manure_om(it)%inorg_n, manure_om(it)%org_n, manure_om(it)%tot_p2o5,          &
            manure_om(it)%inorg_p2o5, manure_om(it)%org_p2o5, manure_om(it)%inorg_p,     &
            manure_om(it)%org_p, manure_om(it)%solids, manure_om(it)%water,              &
            manure_om(it)%units, manure_om(it)%sample_size, manure_om(it)%summary_level, &
            manure_om(it)%data_source
          if (eof < 0) exit
        end do
       exit
      enddo
      endif
      
      db_mx%manure_om  = imax 
      
      close (107)
      return
      end subroutine manure_orgmin_read