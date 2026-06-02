      subroutine wallo_start_day

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine starts the day for water allocation
!!    sets the out of basin source (OSRC) availability, withdrawal for OSRC and canals,
!!    and set water allocation duty (right) and fractions from each POD and to each POR

      use maximum_data_module
      use conditional_module
      use hydrograph_module
      use water_allocation_module
      use recall_module
      
      implicit none
      
      integer :: id = 0         !none       |decision table number
      integer :: ipou = 0       !none       |point of use (POU) number
      integer :: ipod = 0       !none       |point of delivery (POD) number
      integer :: ipor = 0       !none       |point of return (POR) number
      integer :: iosrc = 0      !none       |outside basin source number
      integer :: ihru = 0       !none       |hru number
      integer :: irr = 0        !none       |irrigation object number
      integer :: ican = 0       !none       |canal number
      integer :: istor = 0      !none       |water storage number
      integer :: iom = 0        !none       |pointer to organic-mineral file
      integer :: j = 0          !none       |object number for decision table conditioning - leave 0 for generic tables
      integer :: iob = 0        !none       |current object number for decision table conditioning
      integer :: ird = 0        !none       !irrigation distric number number of hrus
      integer :: irdb = 0       !none       |irrigation farm/district hru data

      !! allocate water (wallo_control) for all non-natural objects
      
        !! Outside Source Objects - POD Objects - typically measured flow or SWAT+ output
          do iosrc = 1, db_mx%out_src
            !! use recall object for transfer
            ipod = osrc(iosrc)%wallo_pod
            iom = recall_db(iosrc)%iorg_min
            select case (recall(iom)%tstep)
              case ("day")    !daily
                osrc_om(iosrc) = recall(iom)%hd(time%day,time%yrs)
              case ("mo")    !monthly
                osrc_om(iosrc) = recall(iom)%hd(time%mo,time%yrs)
              case ("yr")    !yearly
                osrc_om(iosrc) = recall(iom)%hd(1,time%yrs)
              case ("const") !constant
                osrc_om(iosrc) = exco(iom)
              end select
            !! add option for dtbl - probably not needed  
            
            !! allocate and deliver water at start of day
            if (ipod > 0) then
              call wallo_control(ipod)
            end if
          end do
        
        !! allocate and deliver water at start of day for canals
          do ican = 1, db_mx%canal 
            ipod = canal(ican)%wallo_pod
            if (ipod > 0) then
              call wallo_control(ipod)
            end if
          end do
          
        !! allocate and deliver water at start of day for water tower storage
          do istor = 1, db_mx%stor 
            ipod = wtow(istor)%wallo_pod
            if (ipod > 0) then
              call wallo_control(ipod)
            end if
          end do
          
          !! zero water allocation objects and set reset POU finishes to no
          
          !! set water allocation duty (right) and fractions from each POD and to each POR
          do ipou = 1, db_mx%wallo_pou
            !! zero water allocation objects and set reset POU finishes to no
          
            !! compute duty if decision table is used for total irrigation demand
            !pou(ipou)%rate_max = 0.
            if (pou(ipou)%typ == "irr")then
              if (pou(ipou)%dtbl_mx_num > 0) then
                id = pou(ipou)%dtbl_mx_num
                d_tbl => dtbl_flo(id)
                call conditions (j, id)
                call actions (j, iob, id)
                !! reset demand or duty for transfer object
                pou(ipou)%rate_max = trn_m3 * 86400. !convert from m3/s to m3/day
              else
                if (pou(ipou)%rate_max < 1.e-6) then
                do ihru = 1, pou(ipou)%irr%hru_num
                  id = pou(ipou)%irr%dtbl_num(ihru)
                  pou(ipou)%irr%dmd = 0.
                  d_tbl => dtbl_lum(id)
                  call conditions (j, id)
                  call actions (j, iob, id)
                  !! save irrigation demand (from decision table) for each hru
                  !! for "irr", typ_num is the pointer to hruirr_db
                  irdb = pou(ipou)%typ_num
                  j = hruirr_db(irdb)%hru_num(ihru)
                  pou(ipou)%irr%dmd = irrig(j)%demand
                  !! reset demand or duty for transfer object
                  pou(ipou)%rate_max = pou(ipou)%rate_max + trn_m3 * 86400. !convert from m3/s to m3/day
                end do
                end if
              end if
              else
                  if (pou(ipou)%dtbl_mx_num > 0) then
                    id = pou(ipou)%dtbl_mx_num
                    d_tbl => dtbl_flo(id)
                    call conditions (j, id)
                    call actions (j, iob, id)
                    !! reset demand or duty for transfer object
                    pou(ipou)%rate_max = trn_m3 * 86400. !convert from m3/s to m3/day
                  end if
              end if
          
            !! compute source fractions if decision table is used for POD fractions 
            if (pou(ipou)%dtbl_pod_fr_num > 0) then
              id = pou(ipou)%dtbl_pod_fr_num
              if (id > 0) then
                d_tbl => dtbl_flo(id)
                call conditions (j, id)
                call actions (j, iob, id)
                !! reset source fractions for transfer object
                do ipod = 1, pou(ipou)%pods
                  pou(ipou)%pod(ipod)%frac = trn_fr(ipod)
                  pou(ipou)%pod(ipod)%duty = trn_fr(ipod) * pou(ipou)%rate_max
                  poud_met(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%duty
                  poud_met(ipou)%pod(ipod)%deliv = 0.
                end do
              end if
            end if
          
            !! compute source fractions if decision table is used for POR fractions
            if (pou(ipou)%dtbl_por_fr_num > 0) then 
              id = pou(ipou)%dtbl_por_fr_num
              j = ipou
              iob = ipou
              if (id > 0) then
                d_tbl => dtbl_flo(id)
                call conditions (j, id)
                call actions (j, iob, id)
                !! reset source fractions for transfer object
                do ipor = 1, pou(ipou)%pors
                  pou(ipou)%por(ipor)%frac = trn_fr(ipor)
                end do
              end if
            end if
          
          end do    !POU loop
          
      return
      end subroutine wallo_start_day