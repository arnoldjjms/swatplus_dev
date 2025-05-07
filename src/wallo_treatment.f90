      subroutine wallo_treatment (iwallo, idmd)
      
      use water_allocation_module
      use hydrograph_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: idmd          !water demand object number
      integer :: itrt = 0           !none       |treatment database number
      
      !! treating water to wtp or use concentrations
      !! treated outflow is a fraction of withdrawal
      outflo_om = trt
      outflo_om%flo = outflo_om%flo * wdraw_om_tot%flo
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      !wallo(iwallo)%dmd(idmd)%trt = ht5   !need to output how much was remoed or added?
      
      return
      end subroutine wallo_treatment