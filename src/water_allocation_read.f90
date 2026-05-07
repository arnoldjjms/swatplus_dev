      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use constituent_mass_module
      use recall_module
      use exco_module
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
      integer :: iwro = 0             !none       |number of water allocation objects
      integer :: num_objs = 0
      integer :: num_src = 0
      integer :: itrn = 0
      integer :: idb = 0
      integer :: idb_irr = 0
      integer :: ihru = 0
      integer :: iexco = 0
      integer :: iexco_om = 0
      integer :: irec = 0
      integer :: iom = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation POU inputs
      inquire (file=in_watrts%transfer_wro, exist=i_exist)
      if (.not. i_exist .or. in_watrts%transfer_wro == "null") then
        allocate (wallo(0:0))
      else
      do 
        open (107,file=in_watrts%transfer_wro)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_pou = imax
        if (eof < 0) exit
        
        allocate (pou(imax))           !! point of use (pou)
        allocate (poud_duty(imax))     !! daily duty and delivery
        allocate (poum_duty(imax))     !! monthly and delivery
        allocate (pouy_duty(imax))     !! yearly and delivery
        allocate (poua_duty(imax))     !! average annual and delivery
        allocate (poud_om(imax))       !! daily hydrographs
        allocate (poum_om(imax))       !! monthly hydrographs
        allocate (pouy_om(imax))       !! yearly hydrographs
        allocate (poua_om(imax))       !! ave annual hydrographs
        !! add constituent types for each pou if needed

        do ipou = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) pou(ipou)%name, pou(ipou)%typ, ipods, ipors, pou(ipou)%dtbl_mx, pou(ipou)%rate_max
          pou(ipou)%pods = ipods
          pou(ipou)%pors = ipors
          
          allocate (pou(ipou)%pod(ipods))
          allocate (poud_duty(ipou)%pod(ipods))
          allocate (poum_duty(ipou)%pod(ipods))
          allocate (pouy_duty(ipou)%pod(ipods))
          allocate (poua_duty(ipou)%pod(ipods))
          allocate (poud_om(ipou)%pod(ipods))       !! daily hydrographs
          allocate (poum_om(ipou)%pod(ipods))       !! monthly hydrographs
          allocate (pouy_om(ipou)%pod(ipods))       !! yearly hydrographs
          allocate (poua_om(ipou)%pod(ipods))       !! ave annual hydrographs
          !! add constituent types for each pou if needed
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          !! read all POD input data
          do ipod = 1, pou(ipou)%pods
            read (107,*,iostat=eof) pou(ipou)%pod(ipod)%num, pou(ipou)%pod(ipod)%name, pou(ipou)%pod(ipod)%typ, &
                pou(ipou)%pod(ipod)%typ_num, pou(ipou)%pod(ipod)%conv_typ, pou(ipou)%pod(ipod)%conv_num,        &
                pou(ipou)%pod(ipod)%dtbl_min, pou(ipou)%pod(ipod)%const_min, pou(ipou)%pod(ipod)%ann_max,       &
                pou(ipou)%pod(ipod)%frac, pou(ipou)%pod(ipod)%comp
            if (eof < 0) exit
          end do
          
          !! read all POR input data
          do ipor = 1, pou(ipou)%pors
            read (107,*,iostat=eof) pou(ipou)%por(ipor)%num, pou(ipou)%por(ipor)%name, pou(ipou)%por(ipor)%typ, &
                pou(ipou)%por(ipor)%typ_num, pou(ipou)%por(ipor)%conv_typ, pou(ipou)%por(ipor)%conv_num,        &
                pou(ipou)%por(ipor)%dtbl_max, pou(ipou)%por(ipor)%const_max, pou(ipou)%por(ipor)%ann_max,       &
                pou(ipou)%por(ipor)%frac
          end do
          
          !! decision table for setting POU duty - max demand
            if (pou(ipou)%dtbl_mx /= "null") then
              if (pou(ipou)%typ == "irr") then
                ihru = wallo(iwro)%trn(i)%rcv%num
                pou(ipou)%dtbl_mx_num = idb
              else
              !! xwalk with con decision table
              do idb = 1, db_mx%dtbl_con
                if (pou(ipou)%dtbl_mx_num == dtbl_con(idb)%name) then
                  pou(ipou)%dtbl_mx_num = idb
                  exit
                end if
              end do
            end if
            
          !! decision table for setting POD fractions
            if (pou(ipou)%dtbl_pod_fr /= "null") then
              !! xwalk with con decision table
              do idb = 1, db_mx%dtbl_con
                if (pou(ipou)%dtbl_pod_fr == dtbl_con(idb)%name) then
                  pou(ipou)%dtbl_pod_fr_num = idb
                  exit
                end if
              end do
            end if
            
          !! decision table for setting POR fractions
            if (pou(ipou)%dtbl_por_fr /= "null") then
              !! xwalk with con decision table
              do idb = 1, db_mx%dtbl_con
                if (pou(ipou)%dtbl_por_fr == dtbl_con(idb)%name) then
                  ihru = wallo(iwro)%trn(i)%rcv%num
                  pou(ipou)%dtbl_por_fr_num = idb
                  exit
                end if
              end do
            end if
            
        end do    !ipou = 1, imax
        
        exit
      end do
      end if
      close(107)
      
      
      eof = 0
      imax = 0
      
      !! read water allocation POD inputs
      inquire (file=in_watrts%transfer_wro, exist=i_exist)
      if (.not. i_exist .or. in_watrts%transfer_wro == "null") then
        allocate (wallo(0:0))
      else
      do 
        open (107,file=in_watrts%transfer_wro)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_pod = imax
        if (eof < 0) exit
        
        allocate (pod(imax))           !! point of use (pou)

        do ipod = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) pod(ipod)%name, pod(ipod)%num, pod(ipod)%typ, ipous
          pod(ipod)%pous = ipous
          
          allocate (pod(ipod)%pou(ipous))
          allocate (podd_om(ipod)%pou(ipous))       !! daily hydrographs
          allocate (podm_om(ipod)%pou(ipous))       !! monthly hydrographs
          allocate (pody_om(ipod)%pou(ipous))       !! yearly hydrographs
          allocate (poda_om(ipod)%pou(ipous))       !! ave annual hydrographs
          !! add constituent types for each pou if needed
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          backspace (107)
          read (107,*,iostat=eof) pod(ipod)%num, pod(ipod)%name, pod(ipod)%pous, pod(ipod)%typ, pod(ipod)%typ_num,   &
                     (pod(ipod)%pou(ipou)%num, pod(ipod)%pou(ipou)%name, pod(ipod)%pou(ipou)%right, ipou = 1, ipous)
          
          !! store the POD number for each POU object for use in water allocation calculations
          select case (pod(ipod)%typ)
          case ("osrc")
            osrc(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("res")
            res_ob(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("cha")
            sd_ch(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("hru")
            hru(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("aqu")
            aqu_d(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("can")
            canal(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("stor")
            wtow(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
          end select
          
        end do    !ipod = 1, imax
        
        exit
      end do
      end if
      close(107)
      

      return
    end subroutine water_allocation_read