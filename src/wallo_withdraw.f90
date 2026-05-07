      subroutine wallo_withdraw (ipod, ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use aquifer_module
      use reservoir_module
      use time_module
      use recall_module
      
      implicit none 

      integer, intent (in):: ipod           !point of delivery number
      integer, intent (in) :: ipou          !point of use number
      integer :: j = 0              !none       |hru number
      real :: res_min = 0.          !m3         |min reservoir volume for withdrawal
      real :: can_min = 0.          !m3         |min canal volume for withdrawal
      real :: cha_min = 0.          !m3         |minimum allowable flow in channel for withdrawal
      real :: wtow_min = 0.         !m3         |minimum allowable storage in water tower for withdrawal
      real :: rto = 0.              !none       |ratio of water withdrawn to available water in the POD
        
      !! check minimum storage/flow limits and withdraw water from each POD
      select case (pou(ipou)%pod(ipod)%typ)
      !! POD number of each type
      j = pou(ipou)%pod(ipod)%typ_num
      
      !! outside the basin source - daily, monthly, or yearly flow from recall object
      case ("osrc")
        poud_om(ipou)%pod(ipod) = pou(ipou)%pod(ipod)%frac * osrc_om(j)
        osrc_om(j) = (1. - pou(ipou)%pod(ipod)%frac) * osrc_om(j)
        
      !! water tower storage
      case ("stor")
        wtow_min = pou(ipou)%pod(ipod)%const_min * wtow_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (wtow_om_stor(j)%flo > wtow_min) then
          rto = pou(ipou)%pod(ipod)%duty / wtow_om_stor(j)%flo
          pou_om(ipou)%pod(ipod)%hd = rto * wtow_om_stor(j)
          wtow_om_stor(j) = (1. - rto) * wtow_om_stor(j)
        end if
         
      !! divert flowing water from channel source
      case ("cha")
        cha_min = pou(ipou)%pod(ipod)%const_min  !m3 = m3/s * 86400s/d
        !! don't divert when flow is below the minimum - cha_min
        if (ht2%flo > cha_min) then
          rto = pou(ipou)%pod(ipod)%duty / ht2%flo
          pou_om(iwallo)%pod(ipod) = rto * ht2(j)%flo
          ht2%flo = (1. - rto) * ht2%flo
        end if
          
      !! canal source
      case ("can")
        can_min = pou(ipou)%pod(ipod)%const_min * canal_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (canal_om_stor(j)%flo >= can_min) then
          rto = pou(ipou)%pod(ipod)%duty / canal_om_stor(j)%flo
          pou_om(iwallo)%pod(ipod) = rto * canal_om_stor(j)%flo
          canal_om_stor(j) = (1. - rto) * canal_om_stor(j)
        end if
         
      !! reservoir source
      case ("res")
        res_min = pou(ipou)%pod(ipod)%const_min * res_ob(j)%pvol
        !! check if withdrawal takes storage below the minimum
        if (res(j)%flo > res_min) then
          rto = pou(ipou)%pod(ipod)%duty / res(j)%flo
          pou_om(iwallo)%pod(ipod) = rto * res(j)
          res(j) = (1. - rto) * res(j)
        end if
         
      !! aquifer source
      case ("aqu") 
        j = pou(ipou)%pod(ipod)%num
        if (aqu_d(j)%dep_wt < pou(ipou)%pod(ipod)%const_min) then
          pou_om(iwallo)%pod(ipod) = hz
          !! only have flow, no3, and minp(solp) for aquifer
          rto =  (pou(ipou)%pod(ipod)%duty / (10. * aqu_prm(j)%area_ha)) / aqu_d(j)%stor     !mm = m3/(10.*ha)
          aqu_d(j)%stor = (1. - rto) * aqu_d(j)%stor
          aqu_d(j)%no3_st = (1. - rto) * aqu_d(j)%no3_st
          aqu_d(j)%minp = (1. - rto) * aqu_d(j)%minp
          pou_om(iwallo)%pod(ipod)%flo = rto * aqu_d(j)%stor
          pou_om(iwallo)%pod(ipod)%no3 = rto * aqu_d(j)%no3_st
          pou_om(iwallo)%pod(ipod)%solp = rto * aqu_d(j)%minp
        end if
        
      !! gwflow source
      case ("gwf") 
        
      end select
      
      !! add to total flow and om withdrawal for the POU
      pou(ipou)%pod(ipod)%deliv = pou(ipou)%pod(ipod)%deliv + pou_om(ipou)%pod(ipod)%flo
      poud_om(ipou)%pors = poud_om(ipou)%pors + poud_om(ipou)%pod(ipod)
      
      !! add constituents withdrawn to total withdrawal for the POU
      
    return
    end subroutine wallo_withdraw