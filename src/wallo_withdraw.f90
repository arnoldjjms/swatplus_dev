      subroutine wallo_withdraw (ipod, ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use aquifer_module
      use reservoir_module
      use time_module
      use recall_module
      
      implicit none 

      integer, intent (in):: ipod         !point of delivery number
      integer, intent (in) :: ipou        !point of use number
      integer :: ipoud                    !point of delivery number for each point of use number (ipou)
      integer :: j = 0              !none       |source (delivery) object number
      real :: res_min = 0.          !m3         |min reservoir volume for withdrawal
      real :: can_min = 0.          !m3         |min canal volume for withdrawal
      real :: cha_min = 0.          !m3         |minimum allowable flow in channel for withdrawal
      real :: wtow_min = 0.         !m3         |minimum allowable storage in water tower for withdrawal
      real :: rto = 0.              !none       |ratio of water withdrawn to available water in the POD
        
      !! delivery (source) object number from POD object
      j = pod(ipod)%typ_num
      
      !! pod number from delivery object, pou number from loop in wallo_control
      ipoud = pod(ipod)%pou(ipou)%pod_num
      
      !! check minimum storage/flow limits and withdraw water from each POD
      select case (pod(ipod)%typ)
      
      !! outside the basin source - daily, monthly, or yearly flow from recall object
      case ("osrc")
        poud_om(ipou)%pod(ipou) = pou(ipou)%pod(ipoud)%frac * osrc_om(j)
        osrc_om(j) = (1. - pou(ipou)%pod(ipoud)%frac) * osrc_om(j)
        
      !! water tower storage
      case ("stor")
        wtow_min = pou(ipou)%pod(ipoud)%const_min * wtow_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (wtow_om_stor(j)%flo > wtow_min) then
          rto = pou(ipou)%pod(ipoud)%duty / wtow_om_stor(j)%flo
          poud_om(ipou)%pod(ipoud) = rto * wtow_om_stor(j)
          wtow_om_stor(j) = (1. - rto) * wtow_om_stor(j)
        end if
         
      !! divert flowing water from channel source
      case ("cha")
        cha_min = pou(ipou)%pod(ipoud)%const_min  !m3 = m3/s * 86400s/d
        !! don't divert when flow is below the minimum - cha_min
        if (ht2%flo > cha_min) then
          rto = pou(ipou)%pod(ipoud)%duty / ht2%flo
          poud_om(ipou)%pod(ipoud) = rto * ht2
          ht2%flo = (1. - rto) * ht2%flo
        end if
          
      !! canal source
      case ("can")
        can_min = pou(ipou)%pod(ipoud)%const_min * canal_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (canal_om_stor(j)%flo >= can_min) then
          rto = pou(ipou)%pod(ipoud)%duty / canal_om_stor(j)%flo
          poud_om(ipou)%pod(ipoud) = rto * canal_om_stor(j)
          canal_om_stor(j) = (1. - rto) * canal_om_stor(j)
        end if
         
      !! reservoir source
      case ("res")
        res_min = pou(ipou)%pod(ipoud)%const_min * res_ob(j)%pvol
        !! check if withdrawal takes storage below the minimum
        if (res(j)%flo > res_min) then
          rto = pou(ipou)%pod(ipoud)%duty / res(j)%flo
          poud_om(ipou)%pod(ipoud) = rto * res(j)
          res(j) = (1. - rto) * res(j)
        end if
         
      !! aquifer source
      case ("aqu") 
        if (aqu_d(j)%dep_wt < pou(ipou)%pod(ipoud)%const_min) then
          poud_om(ipou)%pod(ipoud) = hz
          !! only have flow, no3, and minp(solp) for aquifer
          rto =  (pou(ipou)%pod(ipoud)%duty / (10. * aqu_prm(j)%area_ha)) / aqu_d(j)%stor     !mm = m3/(10.*ha)
          aqu_d(j)%stor = (1. - rto) * aqu_d(j)%stor
          aqu_d(j)%no3_st = (1. - rto) * aqu_d(j)%no3_st
          aqu_d(j)%minp = (1. - rto) * aqu_d(j)%minp
          poud_om(ipou)%pod(ipoud)%flo = rto * aqu_d(j)%stor
          poud_om(ipou)%pod(ipoud)%no3 = rto * aqu_d(j)%no3_st
          poud_om(ipou)%pod(ipoud)%solp = rto * aqu_d(j)%minp
        end if
        
      !! gwflow source
      case ("gwf") 
        
      end select
      
      !! add to total flow and om withdrawal for the POU
      pou(ipou)%pod(ipod)%deliv = pou(ipou)%pod(ipod)%deliv + poud_om(ipou)%pod(ipod)%flo
      poud_om(ipou)%pors = poud_om(ipou)%pors + poud_om(ipou)%pod(ipod)
      
      !! add constituents withdrawn to total withdrawal for the POU
      
    return
    end subroutine wallo_withdraw