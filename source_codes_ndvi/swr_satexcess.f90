      subroutine swr_satexcess(j1)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine moves water to upper layers if saturated and can't perc

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sep         |mm H2O        |micropore percolation from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use septic_data_module
      use hru_module, only : hru, ihru, cbodu, surfq, surqno3, surqsolp, sep_tsincefail, i_sep,  &
        isep, qday, sepday
      use soil_module
      use hydrograph_module
      use basin_module
      use organic_mineral_mass_module
      
      implicit none

      integer :: j                 !none          |HRU number
      integer :: j1                !none          |counter
      integer :: ii                !none          |counter
      integer :: isp               !              | 
      real:: ul_excess             !              |
      real:: qlyr                  !              |
      real:: pormm                 !mm            |porosity in mm depth 
      real:: rtof                  !none          |weighting factor used to partition the 
                                   !              |organic N & P concentration of septic effluent
                                   !              |between the fresh organic and the stable organic pools
      real :: xx                   !              |
      integer :: jj                !              |
      integer :: l                 !              | 
      integer :: nn                !none          |number of soil layers
      integer :: ly                !none          |counter

      j = ihru
      isp = sep(isep)%typ 	   !! J.Jeong 3/09/09
      rtof = 0.5

 	if (sep(isep)%opt == 2 .and. j1 == i_sep(j)) then
	  
	  ii = j1 
	  qlyr = soil(j)%phys(ii)%st
	  
	  ! distribute excess STE to upper soil layers 
	  do while (qlyr > 0 .and. ii > 1)
		 ! distribute STE to soil layers above biozone layer
        if (soil(j)%phys(ii)%st > soil(j)%phys(ii)%ul) then
	      qlyr = soil(j)%phys(ii)%st - soil(j)%phys(ii)%ul 	! excess water moving to upper layer
	      soil(j)%phys(ii)%st = soil(j)%phys(ii)%ul  ! layer saturated
	      soil(j)%phys(ii-1)%st = soil(j)%phys(ii-1)%st + qlyr ! add excess water to upper layer
	    else 
	      qlyr = 0.
	    endif
	  
	    ! Add surface ponding to the 10mm top layer when the top soil layer is saturated
		 !  and surface ponding occurs.
		 if (ii == 2) then
	     qlyr = soil(j)%phys(1)%st - soil(j)%phys(1)%ul
	     ! excess water makes surface runoff
	     if (qlyr > 0) then
            soil(j)%phys(1)%st = soil(j)%phys(1)%ul
            surfq(j) = surfq(j) + qlyr
            endif
	     qlyr = 0.
		 endif

	    ii = ii - 1
	  end do
	endif
	       
      !!EBcoV60.4 if (sep(isep)%opt == 0) then
      if (j1 < soil(j)%nly) then           !!EBcomm cas "perco" : transfert d'eau du haut vers le bas
        if (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul > 1.e-4) then
          sepday = (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul)
          soil(j)%phys(j1)%st = soil(j)%phys(j1)%ul
          soil(j)%phys(j1+1)%st = soil(j)%phys(j1+1)%st + sepday
        end if
      else                            !!EBcomm cas "revap" : transfert d'eau du bas vers le haut

        if (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul > 1.e-4) then
          ul_excess = soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul
          soil(j)%phys(j1)%st = soil(j)%phys(j1)%ul
          nn = soil(j)%nly
          do ly = nn - 1, 1, -1
            soil(j)%phys(ly)%st = soil(j)%phys(ly)%st + ul_excess
            if (soil(j)%phys(ly)%st > soil(j)%phys(ly)%ul) then
              ul_excess = soil(j)%phys(ly)%st - soil(j)%phys(ly)%ul
              soil(j)%phys(ly)%st = soil(j)%phys(ly)%ul
            else
              ul_excess = 0.
              exit
            end if
            if (ly == 1 .and. ul_excess > 0.) then
              !! add ul_excess to depressional storage and then to surfq
              wet(j)%flo = wet(j)%flo + ul_excess
            end if
          end do
          !compute tile flow again after saturation redistribution
        end if
      end if
      !!EBcoV60.4 end if

      return
      end subroutine swr_satexcess
