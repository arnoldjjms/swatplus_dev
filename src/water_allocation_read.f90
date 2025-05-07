      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use hru_module, only : hru
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: k = 0                !none       |counter
      integer :: isrc = 0             !none       |counter
      integer :: ircv = 0             !none       |counter
      integer :: iwro = 0             !none       |number of water allocation objects
      integer :: num_objs = 0
      integer :: num_src = 0
      integer :: num_rcv = 0
      integer :: idmd = 0
      integer :: idb = 0
      integer :: idb_irr = 0
      integer :: ihru = 0
      integer :: isrc_wallo = 0
      integer :: div_found = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file=in_watrts%transfer_wro, exist=i_exist)
      if (.not. i_exist .or. in_watrts%transfer_wro == "null") then
        allocate (wallo(0:0))
      else
      do 
        open (107,file=in_watrts%transfer_wro)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_db = imax
        if (eof < 0) exit
        
        allocate (wallo(imax))
        allocate (wallod_out(imax))
        allocate (wallom_out(imax))
        allocate (walloy_out(imax))
        allocate (walloa_out(imax))

        do iwro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wallo(iwro)%name, wallo(iwro)%rule_typ, wallo(iwro)%src_obs, &
                                                    wallo(iwro)%dmd_obs
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          num_objs = wallo(iwro)%src_obs
          allocate (wallo(iwro)%src(num_objs))
          num_objs = wallo(iwro)%dmd_obs
          allocate (wallo(iwro)%dmd(num_objs))
          allocate (wallod_out(iwro)%dmd(num_objs))
          allocate (wallom_out(iwro)%dmd(num_objs))
          allocate (walloy_out(iwro)%dmd(num_objs))
          allocate (walloa_out(iwro)%dmd(num_objs))
                    
          !! read source object data
          do isrc = 1, wallo(iwro)%src_obs
            read (107,*,iostat=eof) i
            wallo(iwro)%src(i)%num = i
            if (eof < 0) exit
            backspace (107)
              read (107,*,iostat=eof) k, wallo(iwro)%src(i)%ob_typ, wallo(iwro)%src(i)%ob_num,    &
                                                                  wallo(iwro)%src(i)%limit_mon
          end do
          
          !! read demand object data
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do idmd = 1, num_objs
            read (107,*,iostat=eof) i
            wallo(iwro)%dmd(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%dmd(i)%ob_typ, wallo(iwro)%dmd(i)%ob_num,            &
              wallo(iwro)%dmd(i)%dmd_typ, wallo(iwro)%dmd(i)%dmd_typ_name, wallo(iwro)%dmd(i)%amount,   &
              wallo(iwro)%dmd(i)%right, wallo(iwro)%dmd(i)%src_num, wallo(iwro)%dmd(i)%rcv_num
            num_src = wallo(iwro)%dmd(i)%src_num
            allocate (wallo(iwro)%dmd(i)%src(num_src))
            allocate (wallod_out(iwro)%dmd(i)%src(num_src))
            allocate (wallom_out(iwro)%dmd(i)%src(num_src))
            allocate (walloy_out(iwro)%dmd(i)%src(num_src))
            allocate (walloa_out(iwro)%dmd(i)%src(num_src))
            num_rcv = wallo(iwro)%dmd(i)%rcv_num
            allocate (wallo(iwro)%dmd(i)%rcv(num_rcv))
            
            !! for hru irrigation, need to xwalk with irrigation demand decision table
            if (wallo(iwro)%dmd(i)%dmd_typ == "dtbl" .and. wallo(iwro)%dmd(i)%ob_typ == "hru") then
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_lum
                if (wallo(iwro)%dmd(i)%dmd_typ_name == dtbl_lum(idb)%name) then
                  ihru = wallo(iwro)%dmd(i)%ob_num
                  hru(ihru)%irr_dmd_dtbl = idb
                  do idb_irr = 1, db_mx%irrop_db 
                    if (dtbl_lum(idb)%act(1)%option == irrop_db(idb_irr)%name) then
                      wallo(iwro)%dmd(idmd)%irr_eff = irrop_db(idb_irr)%eff
                      wallo(iwro)%dmd(idmd)%surq = irrop_db(idb_irr)%surq
                      exit
                    end if
                  end do
                end if
              end do
            end if
            
            !! for hru irrigation, need to xwalk with irrigation demand decision table
            if (wallo(iwro)%dmd(i)%dmd_typ == "dtbl" .and. wallo(iwro)%dmd(i)%ob_typ /= "hru") then
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_flo
                if (wallo(iwro)%dmd(i)%dmd_typ_name == dtbl_flo(idb)%name) then
                  wallo(iwro)%dmd(idmd)%dtbl_num = idb
                  exit
                end if
              end do
            end if
            
            !! for municipal treatment - recall option for daily, monthly, or annual mass
            if (wallo(iwro)%dmd(i)%dmd_typ == "recall") then
              !! xwalk with recall database
              do idb = 1, db_mx%recall_max
                if (wallo(iwro)%dmd(i)%dmd_typ_name == recall(idb)%name) then
                  wallo(iwro)%dmd(i)%rec_num = idb
                  exit
                end if
              end do
            end if
            
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%dmd(i)%ob_typ, wallo(iwro)%dmd(i)%ob_num,            &
              wallo(iwro)%dmd(i)%dmd_typ, wallo(iwro)%dmd(i)%dmd_typ_name, wallo(iwro)%dmd(i)%amount,   &
              wallo(iwro)%dmd(i)%right, wallo(iwro)%dmd(i)%src_num, wallo(iwro)%dmd(i)%rcv_num,         &
              (wallo(iwro)%dmd(i)%src(isrc), isrc = 1, num_src),                                        &
              (wallo(iwro)%dmd(i)%rcv(isrc), ircv = 1, num_rcv)
            
            !! zero output variables for summing
            do isrc = 1, num_objs
              wallod_out(iwro)%dmd(i)%src(isrc) = walloz
              wallom_out(iwro)%dmd(i)%src(isrc) = walloz
              walloy_out(iwro)%dmd(i)%src(isrc) = walloz
              walloa_out(iwro)%dmd(i)%src(isrc) = walloz
            end do
            
          end do
          
        end do

        exit
      end do
      end if
      close(107)

      return
    end subroutine water_allocation_read
    
    
    
    
    subroutine water_treatment_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: k = 0                !none       |counter
      integer :: isrc = 0             !none       |counter
      integer :: ircv = 0             !none       |counter
      integer :: iwtp = 0             !none       |number of water treatment objects
      integer :: num_objs = 0
      integer :: num_src = 0
      integer :: num_rcv = 0
      integer :: idmd = 0
      integer :: idb = 0
      integer :: idb_irr = 0
      integer :: ihru = 0
      integer :: isrc_wallo = 0
      integer :: div_found = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_treat_read.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_treat_read.wal' == "null") then
        allocate (wtp_om_treat(0:0))
        allocate (wtp_cs_treat(0:0))
      else
      do 
        open (107,file='water_treat_read.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        !db_mx%water_treat = imax
        if (eof < 0) exit
        
        allocate (wtp_om_treat(imax))
        allocate (wtp_cs_treat(imax))

        do iwtp = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit 
          read (107,*,iostat=eof) wtp(iwtp)%name, wtp(iwtp)%init, wtp(iwtp)%stor_mx,    &
                                            wtp(iwtp)%lag_days, wtp(iwtp)%loss_fr
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          !! read concentratin of treated organics - water, sediment and nutrients
          read (107,*,iostat=eof) wtp_om_treat(iwtp)
          
          !! read pseticide concentrations of treated water
          if (cs_db%num_pests > 0) then
            allocate (wtp_cs_treat(iwtp)%pest(cs_db%num_pests))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wtp_cs_treat(iwtp)%pest
          end if
          
          !! read pathogen concentrations of treated water
          if (cs_db%num_paths > 0) then
            allocate (wtp_cs_treat(iwtp)%pest(cs_db%num_paths))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wtp_cs_treat(iwtp)%path
          end if
        end do
          
        exit
      end do
      end if
      close(107)

      return
      end subroutine water_treatment_read