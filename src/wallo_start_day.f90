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
      use hru_module
      
      implicit none
      
      integer :: id = 0         !none       |decision table number
      integer :: ipou = 0       !none       |point of use (POU) number
      integer :: ipod = 0       !none       |point of delivery (POD) number
      integer :: ipor = 0       !none       |point of return (POR) number
      integer :: iosrc = 0      !none       |outside basin source number
      integer :: irr = 0        !none       |irrigation object number
      integer :: ican = 0       !none       |canal number
      integer :: istor = 0      !none       |water storage number
      integer :: iom = 0        !none       |pointer to organic-mineral file
      integer :: j = 0          !none       |object number for decision table conditioning - leave 0 for generic tables
      integer :: iob = 0        !none       |current object number for decision table conditioning

      !! allocate water (wallo_control) for all non-natural objects
      
        !! Outside Source Objects - POD Objects - typically measured flow or SWAT+ output
          do iosrc = 1, db_mx%out_src
            !! use recall object for transfer
            ipod = osrc(iosrc)%wallo_pod
            iom = recall_db(iosrc)%iorg_min
            select case (recall(iom)%typ)
              case (1)    !daily
                osrc_om(iosrc) = recall(iom)%hd(time%day,time%yrs)
              case (2)    !monthly
                osrc_om(iosrc) = recall(iom)%hd(time%mo,time%yrs)
              case (3)    !yearly
                osrc_om(iosrc) = recall(iom)%hd(1,time%yrs)
              case (4) !constant
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
            !! zero water allocation objects and set reset POU finishes to "n"
            pou(ipou)%fin = "n"
            pou(ipou)%pod(:)%fin = "n"
          
            !! need irrig(j) when irrigating for condition and setting POU demand in dtbl
            pou(ipou)%demand = 0.
            if (pou(ipou)%typ == "irr")then
              do ihru = 1, pou(ipou)%irr%hru_num
                  j = pou(ipou)%irr%hru(ihru)
                  id = pou(ipou)%irr%dtbl_num(ihru)
                  d_tbl => dtbl_lum(id)
                  call conditions (j, id)
                  call actions (j, iob, id)
                  !! irrig(j)%demand, applied, runoff (from decision table) for each hru
                  !! reset demand or duty for transfer object - convert from mm to m3
                  pou(ipou)%demand = pou(ipou)%demand + irrig(j)%demand * hru(ihru)%area_ha * 10.
                  irrig(j)%demand = 0.
              end do
            end if
            
            !! if no dtbl, use the maximum rate every day
            pou(ipou)%demand = pou(ipou)%rate_max * 86400. !convert to m3/s
            !! if decision table, use to set demand
            if (pou(ipou)%dtbl_mx_num > 0) then
              dmd_m3 = 0.
              id = pou(ipou)%dtbl_mx_num
              d_tbl => dtbl_flo(id)
              call conditions (j, id)
              call actions (j, iob, id)
              !! reset demand or duty for transfer object
              pou(ipou)%demand = dmd_m3
            end if
            !! set max rate for the day - convert to m3/s
            pou(ipou)%demand = Min(pou(ipou)%rate_max * 86400., pou(ipou)%demand)
            
            !! compute POD (source) fractions if decision table is used for POD fractions 
            if (pou(ipou)%dtbl_pod_fr_num > 0) then
              id = pou(ipou)%dtbl_pod_fr_num
              d_tbl => dtbl_flo(id)
              call conditions (j, id)
              call actions (j, iob, id)
              !! reset source fractions for transfer object
              do ipod = 1, pou(ipou)%pods
                pou(ipou)%pod(ipod)%frac = trn_fr(ipod)
                pou(ipou)%pod(ipod)%duty = trn_fr(ipod) * pou(ipou)%demand
                poud_met(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%duty
                poud_met(ipou)%pod(ipod)%deliv = 0.
              end do
            else
              !! use input POD (source) fractions
              do ipod = 1, pou(ipou)%pods
                pou(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%frac * pou(ipou)%demand
                poud_met(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%duty
                poud_met(ipou)%pod(ipod)%deliv = 0.
              end do
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