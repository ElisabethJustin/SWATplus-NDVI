      subroutine ndvi_read (annee)

      use hru_module
      use input_file_module
      use time_module

      implicit none      
 
      integer, intent (in)  ::  annee 
      character (len=80) :: header    !           |header of file
      integer :: iyr                  !           |
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: idhru                !           |
      integer :: util                 !           |
      integer :: jdeb                 !           | 
      integer :: i                    !           |compteur
      real, dimension(37) :: valeurs

      eof = 0

      !read ndvi file
      inquire (file=in_hru%hru_ndvi, exist=i_exist)
      if (i_exist .or. in_hru%hru_ndvi /= "null") then
      do
        open (107,file=in_hru%hru_ndvi)
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        iyr = time%start_yr_ndvi
        
        if (iyr .eq. annee) then
          read (107,*,iostat=eof) idhru, iyr
        end if
        
        do while (iyr < annee)
          read (107,*,iostat=eof) idhru, iyr
          if (eof < 0) exit
        end do
        
        if (iyr .eq. annee) then
        
          backspace (107)

	  do while (iyr .eq. annee)
	    read (107,*,iostat=eof) idhru, iyr, util, jdeb, valeurs
	    hru(idhru)%ndvi%util = util
	    if (util) then
	      hru(idhru)%ndvi%jdeb = jdeb
	      hru(idhru)%ndvi%val = valeurs
	      hru(idhru)%ndvi%maxndvi = maxval(valeurs)
	      do i = 1,37
	        if (valeurs(i) < -1.) exit
	      end do
	      hru(idhru)%ndvi%jfin = (i-1 - 1)*10 + jdeb
	    end if
	    if (eof < 0) exit
	  end do
	
        end if
        
        close (107)
        exit
      
      end do
      endif
      
      return
      end subroutine ndvi_read
