      subroutine pl_waterup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes potential plant evaporation through
!!    the root zone and calculates actual plant water use based on soil
!!    water availability. Also estimates water stress factor.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs
!!                               |on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |
!!    ir          |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    reduc       |none          |fraction of water uptake by plants achieved
!!                               |where the reduction is caused by low water
!!                               |content
!!    sum         |
!!    sump        |
!!    wuse        |mm H2O        |water uptake by plants in each soil layer
!!    sum_wuse    |mm H2O        |water uptake by plants from all layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hru_module, only : hru, ihru, epmax, ipl, ep_day, uptake
      use soil_module
      use plant_module
      use urban_data_module
      
      implicit none
      
      integer :: j           !none      |hru number
      integer :: k           !none      |counter 
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          | 
      integer :: ulu         !          |urban land use from urban.urb
      real :: sum            !          |
      real :: sum_wuse       !mm H2O    |water uptake by plants from all layers
      real :: sum_wusep      !mm H2O    |previous water uptake by plants from all layers
      real :: reduc          !none      |fraction of water uptake by plants achieved
                             !          |where the reduction is caused by low water
                             !          |content
      real :: sump           !          |
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed
      real :: wuse           !mm H2O    |water uptake by plants in each soil layer
      real :: satco          !          | 
      real :: pl_aerfac      !          |
      real :: scparm         !          |  
      real :: uobw           !none      |water uptake normalization parameter
                             !          |This variable normalizes the water uptake so
                             !          |that the model can easily verify that uptake
                             !          |from the different soil layers sums to 1.0
      real :: ubw            !          |the uptake distribution for water is hardwired
      real :: yy             !          | 
      real :: Ks             !!EBndvi coeff de stress comme samir
      integer :: ip          !!EBndvi
      integer :: npl_gro     !!EBndvi
      real :: groot          !!EBndvi
      

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      !! compute aeration stress
      if (soil(j)%sw > soil(j)%sumfc) then
        satco = (soil(j)%sw - soil(j)%sumfc) / (soil(j)%sumul - soil(j)%sumfc)
        pl_aerfac = .1  !.05
        scparm = 100. * (satco - pl_aerfac) / (1.0001 - pl_aerfac)
        if (scparm > 0.) then
          pcom(j)%plstr(ipl)%strsa = 1. - (scparm / (scparm + Exp(2.9014 - .03867 * scparm)))
        else
          pcom(j)%plstr(ipl)%strsa = 1.
        end if
      end if

      !! compute limiting water stress
      if (epmax(ipl) <= 1.e-6) then
        pcom(j)%plstr(ipl)%strsw = 1.
      else
        !! initialize variables
        gx = 0.
        ir = 0
        sump = 0.
        wuse = 0.
        sum_wuse = 0.
        sum_wusep = 0.
        
        if (hru(j)%ndvi%util) then
          npl_gro = 0
          do ip = 1, pcom(j)%npl
            if (pcom(j)%plcur(ip)%gro == "y") then
              npl_gro = npl_gro + 1
            end if
          end do
          if (npl_gro==0) then
            groot = 10. + (soil(j)%zmx - 10.) * (1.25 * hru(j)%ndvi%ndvi_j - 0.13)/0.9  !!EBndvi prof rac comme SAMIR
            if (groot > soil(j)%zmx) groot = soil(j)%zmx
            if (groot < 10.) groot = 10.
          else
            groot = pcom(j)%plg(ipl)%root_dep
          end if
     
        else
          groot = pcom(j)%plg(ipl)%root_dep  !!EBndvi prof racinaire de la culture principale
        end if

        do k = 1, soil(j)%nly
          if (ir > 0) exit

          if (groot <= soil(j)%phys(k)%d) then
            gx = groot
            ir = k
          else
            gx = soil(j)%phys(k)%d
          end if

          if (groot <= 0.01) then
            sum = epmax(ipl) / uptake%water_norm
          else
            sum = epmax(ipl) * (1. - Exp(-uptake%water_dis * gx / groot)) / uptake%water_norm
          end if

          !!EB pour quoi cette formule-la ?  wuse = sum - sump * (1. - pcom(j)%plcur(ipl)%epco)
          wuse = sum - sump + (sump - sum_wusep)*pcom(j)%plcur(ipl)%epco  !!EB la meme formule que dans SWAT2012
          ! adjust for impervious area
          ulu = hru(j)%luse%urb_lu
          !wuse = wuse * urbdb(ulu)%fcimp
          wuse = amin1 (wuse, soil(j)%phys(k)%st)
          
          !!EBndvi coeff de stress Ks
          if (hru(j)%ndvi%util) then
            Ks = soil(j)%phys(k)%st / soil(j)%phys(k)%fc / (1. - 0.55)  !!EB3001
            if (Ks > 1) Ks = 1.
            
          else ! sans NDVI   
            !! adjust uptake if sw is less than 25% of plant available water
            if (soil(j)%phys(k)%st < soil(j)%phys(k)%fc/4.) then
              Ks = Exp(5. * (4. * soil(j)%phys(k)%st / soil(j)%phys(k)%fc - 1.))
            else
              Ks = 1.
            end if
            
          endif
          wuse = wuse * Ks
          !!EBndvi fin
          
          if (soil(j)%phys(k)%st < wuse) then
            wuse = soil(j)%phys(k)%st
          end if
          
          sum_wuse = sum_wuse + wuse
          if (sum_wuse > epmax(ipl)) then
            wuse = epmax(ipl) - sum_wusep
            sum_wuse = epmax(ipl)
          end if
          sump = sum
          sum_wusep = sum_wuse

          soil(j)%phys(k)%st = Max(1.e-6, soil(j)%phys(k)%st - wuse)
          
        end do      !! soil layer loop
        
        !! update total soil water in profile
        soil(j)%sw = 0.
        do k = 1, soil(j)%nly
          soil(j)%sw = soil(j)%sw + soil(j)%phys(k)%st
        end do

        !new epco adjustment requires epmax adjustment of water stress is too high
        pcom(j)%plstr(ipl)%strsw = sum_wuse / epmax(ipl) !* hru(j)%hyd%epco)
        ep_day = ep_day + sum_wuse

      end if

      return
      end subroutine pl_waterup
