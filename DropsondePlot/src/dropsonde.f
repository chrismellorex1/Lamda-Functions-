!     -------------------------------------------------------

      PROGRAM WUDGET  modified for AWIPS2 cmello 5/2018
!     --------------------------------------------------------

!     Decode TEMPDROP message file and format message into spline analysis
!     input(HSA) format.


      CHARACTER*1  clat, clon
      CHARACTER*3  months(12), mns
      CHARACTER*9 sondeid
      CHARACTER*10 loglocation
      CHARACTER*60 logcomment
      CHARACTER*60 filename
      CHARACTER*70 line
      CHARACTER*80 linehsa
      CHARACTER*80 dropl(200)
      CHARACTER*91 dropl2(200)
      CHARACTER*200 line2
      CHARACTER*5 acid
      CHARACTER*2 obnum
      COMMON /DROPDATA/ idropl, dropl, dropl2, iac
      COMMON /PARAM/ lufi, lufx, lufw, lufp, lufo, lupr, &
                     npltform, nsndtype, fallrate, datarate
      COMMON /MORLOG/ spglat, spglon, ispgtime, spgsecstoadd
      COMMON /DOTLOG/ idropnumber, logtime, rloglat, rloglon, &
                     rlogsp, rlog10ws, rlog10wd, rlogmblws, rlogmblwd, &
                     rlogbtsst, loglocation, sondeid, logcomment
      DATA ilx/70/,ihsa/80/, ifx/60/lub/14/, lubfl/15/
      DATA luf/12/, luo/13/, iwx/1/, iflag/1/, nrecs/0/
      DATA idys/0/, imns/0/, iyrs/0/
      DATA MONTHS/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug', &
                 'Sep','Oct','Nov','Dec'/
! --------------------------------------------------------------

      CALL GETARG(1, filename)
      OPEN (luo, FILE = 'myoutput.hsa', IOSTAT = istat, &
            STATUS = 'unknown', ERR = 9998)

      DO i = 1, ifx
        IF (filename(i:i).EQ.' ') THEN
          filename = filename(1:i-1)
          ife = i - 1
          exit
        ELSE
          ife = ifx
        ENDIF
      ENDDO

      print *,  '("filename=''", A, "''")' ,filename

      OPEN(luf, FILE = filename(1:ife), IOSTAT = istat, STATUS = 'old', &
           ERR = 9999)
      OPEN(lub, STATUS = 'unknown', ACCESS = 'append', ERR=9998)
      OPEN( lubfl,  status = 'unknown', access  = 'append',ERR=9998)

    8 READ(luf, '(A)', END = 50) line
      IF(line(1:1).EQ.";")THEN
         GOTO 50
      ENDIF

      IF(line(1:3).NE."000".AND.line(1:3).ne."NNN".AND.line(1:3).ne."   ".AND.line(1:2).NE."UZ")THEN
         READ(line(30:31), '(I2)') idys
         READ(line(33:35), '(A3)') mns
            DO i = 1, 12
               IF (months(i).EQ.mns) imns = i
            ENDDO
         READ(line(37:38), '(I2)') iyrs
      ENDIF


! KSellwood
! If no date on 1st line of tempdrop get date from the filename

      IF(idys.eq.00.or.imns.eq.00.or.iyrs.eq.00)then
         IF( filename(10:12).eq."xmt".or.filename(9:11).eq."xmt")then
            read(filename(1:2), '(I2)') iyrs
            read(filename(3:4), '(I2)') imns
            read(filename(5:6), '(I2)') idys
         ELSEIF(filename(1:6).EQ."REPNT3")THEN
            READ(filename(15:16),'(I2)') iyrs
            READ(filename(17:18),'(I2)')imns
            READ(filename(19:20),'(I2)') idys
         ELSEIF(filename(18:19).eq.".d")THEN
            READ(filename(3:4), '(I2)') iyrs
            READ(filename(5:6), '(I2)') imns
            READ(filename(7:8), '(I2)') idys
         ELSEIF(filename(20:23).eq.".txt")THEN
            READ(filename(3:4), '(I2)') iyrs
            READ(filename(5:6), '(I2)') imns
            READ(filename(7:8), '(I2)') idys
          ENDIF
       ENDIF

!     Get REL and SPG info.
! KSellwood
! get rid of go to's (get rid of labels later)   

      DO 10
         READ(luf, '(A)', END = 9995) line

! Aircraft identifier (P3 = 42, 43, GIV = 49, AF = 30)

         IF(line(1:5).EQ.'61616')THEN
            READ(line(7:11), '(A)') acid
            READ(line(36:37),'(A)') obnum
            IF(obnum(1:2).EQ.'  ') THEN
               obnum="00"
            ENDIF
            IF(acid(1:1).EQ.'A')THEN
               iac=30
            ELSEIF(acid(5:5).EQ.'F')THEN
               iac=08
            ELSE
               READ(line(11:11),'(I1)') iac
               iac = iac + 40
            ENDIF
            print *, "acid,  iac"
            print *, acid,  iac
            EXIT    !# not sure if this should be here
         ENDIF

 10 CONTINUE

      CLOSE(luf) 
      OPEN(luf, FILE = filename(1:ife), IOSTAT = istat, STATUS = 'old',&
       ERR = 9999)

!     --------------------------------------------------------
!     read the first line of a message.  This version does not
!     require UZNT header, just the XXAA
!     --------------------------------------------------------
DO 100

      READ(luf, '(A)', END = 50) line
         IF (INDEX(line, 'XXAA').NE.0) THEN
         CALL DROP(luo, iwx, iflag, iyrs, imns, idys, line, acid,obnum)

            DO 45 ii = idropl, 1, -1
!               write(lub, 610) dropl(ii)
!             write(lub, '(a)') dropl(ii)
             write(lubfl,'(a)') dropl2(ii)
!               write(lubf,611) dropl2(ii)
  45        ENDDO
         ENDIF
!KSellwood if reading from tempdrop messages uncomment exit statemtment to avoid duplicates
!GOTO 50
 100  CONTINUE

9995   print *,  '(" No Aircraft ID")'

CLOSE(luf)
OPEN(luf, FILE = filename(1:ife), IOSTAT = istat, STATUS = 'old',ERR = 9999)

!--------------------------------------------------------
!     read the first line of the message
!--------------------------------------------------------
DO
   READ(luf, '(A)', END = 99999) line
   IF (INDEX(line, 'XXAA').NE.0) THEN
   CALL DROP(luo, iwx, iflag, iyrs, imns, idys, line, acid,obnum)
      DO  i = idropl, 1, -1
         ! write(lub, 610) dropl(i)
!          write(lubfl,611) dropl2(i)
       write(lubfl,'(a)') dropl2(i)
      ENDDO
   ENDIF
ENDDO


610   format (i2,1x,f7.0,1x,i4,1x,f7.3,1x,f8.3,1x,3(f6.1,1x), &
       f7.1,1x,2(f6.1,1x),a4)
611   format (i2,1x,f7.0,1x,i4,1x,f7.3,1x,f8.3,1x,3(f6.1,1x), &
       f7.1,1x,2(f6.1,1x),a4,1x,a5,1x,a2)


50    GOTO 99999

 9998 print *, '(" CANNOT OPEN OUTPUT FILE ***")'
 9999 print *,  '(" CANNOT OPEN INPUT FILE ***")'

99999 END  !  WUDGET



!     -----------------------------------------------------
      FUNCTION GSNDFALL2(PR,TE,BAD, sfcp)

!     Function returns theoretical fall rate of GPS sonde.
!     If TE is missing, mean West Indies hurricane season
!     TE is used.  Pressure must be valid to compute fall.
! 
!     4/18/01:  Changed mass from 390 to 430 g and changed

!               fudge factor from 1.15 to 1.00.
!     -----------------------------------------------------
!
      PARAMETER (NSTD = 20)
      PARAMETER (NM = 13) 

      DIMENSION PSTD(20), TSTD(20), HMHPA(13), HGTM(13)

      DATA PSTD/100.,150.,200.,250.,300.,350.,400.,450.,500.,550., &
                600.,650.,700.,750.,800.,850.,900.,950.,1000.,1050./
      DATA TSTD/-73.5,-67.6,-55.2,-43.3,-33.2,-24.8,-17.7,-11.9, &
                -6.9,-2.5,1.4,5.1,8.6,11.8,14.6,17.3,19.8,23.0, &
                26.0,26.3/
      DATA HMHPA/50.0,100.0,150.0,200.0,250.0,300.0,400.0,500.0,600.0 &
                ,700.0,850.0,925.0,1025.0/
      DATA HGTM/20732.,16593.,14197.,12412.,10948.,9693.,7604., &
                5895.,4450.,3188.,1551.,821.,0./
   



!     Set constants
!     -------------
      SMASS = 0.430                 !   MASS OF SONDE (KG) 
      CD = .63                      !   DRAG COEFF
      AREA = 0.09                   !   AREA OF PARACHUTE (30 CM x 30 CM)
      GRAV = 9.8                    !   GRAVITY
      ADJFACTOR = 1.00              !   FINAL ADJUSTMENT FACTOR
     IF ((sfcp.LT.1050.).AND.(sfcp.GT.925.)) hmhpa(nm) = sfcp  ! pal

      IF (PR.LE.0.)  THEN
      GSNDFALL2 = BAD
      RETURN
      ENDIF

      IF (TE.GT.BAD) THEN
         T = TE
         ELSE
         CALL POLATE(NSTD,PSTD,TSTD,PR,T,M,BAD)
         IF (T.EQ.BAD)THEN
         GSNDFALL2 = BAD
         RETURN
         ENDIF
      ENDIF

!     Compute density
!     ---------------
      RHO = (PR * 100.)/(287.*(T+273.16))

!     Compute fall speed - theoretical
!     --------------------------------
      GSNDFALL2 = -((2.0*SMASS*GRAV)/(CD*AREA*RHO))**0.5

!     Add empirical fudge factor
!     --------------------------
      GSNDFALL2 = GSNDFALL2*ADJFACTOR

!     Convert m/s to mb/s, added fudge. pal

       CALL POLATE(nm, hmhpa, hgtm, pr, hgt, m, bad)

! KSellwood conversion should be pressure change (not pressure) /  height
! Bottom up calculation
! If surface then use top - bottom fallspeed

       IF(HGT.GT.0.AND.(SFCP - PR).GE.0)THEN
       GSNDFALL2 = GSNDFALL2 * (PR - SFCP) / HGT  ! m/s * mb/m = mb/s
       ELSE
       GSNDFALL2 = BAD
       ENDIF

      RETURN
      END

!    ------------------------------------------- 
      SUBROUTINE POLATE(N,X,Y,XIN,YOUT,M,BAD)
!    ------------------------------------------- 

      DIMENSION X(N),Y(N)
      LOGICAL INC


!     Check for exact match 
!    --------------------- 
      IF (N.LE.0) THEN
      YOUT = BAD
      M = -1
      RETURN
      ENDIF

      DO 15 L=1,N
         IF (X(L).EQ.BAD) EXIT
           IF (XIN.EQ.X(L)) THEN
             YOUT=Y(L)
             M = L
             RETURN
           ENDIF
         
15       CONTINUE

      IF (N.EQ.1) THEN
      YOUT = BAD
      M = -1
      RETURN
      ENDIF  ! GOTO 500


!     Determine if X increases or decreases 
!     ------------------------------------- 
      XMAX = -9999999.
      XMIN = 9999999.

      DO 100 L = 1,N
         IF (X(L).NE.BAD .AND. X(L).GT.XMAX) THEN
            XMAX = X(L)
            LMAX = L
            ENDIF 
         IF (X(L).NE.BAD .AND. X(L).LT.XMIN) THEN
            XMIN = X(L)
            LMIN = L
            ENDIF 
100      CONTINUE

      IF (XMIN.GT.XMAX.OR.XMIN.EQ.XMAX.OR. &
          XIN.LT.XMIN.OR.XIN.GT.XMAX) THEN
         YOUT = BAD
         RETURN
      ENDIF

      INC = .FALSE.
      IF (LMAX.GT.LMIN) INC = .TRUE.
 
 
!     Interpolate
!     -----------
      DO 10 L=1,N-1

         IF (X(L).EQ.BAD .OR. X(L+1).EQ.BAD) THEN
         YOUT = BAD
         M = -1
         RETURN
         ENDIF
         IF (XIN.GT.X(L) .AND. XIN.LT.X(L+1)) EXIT
         IF (XIN.LT.X(L) .AND. XIN.GT.X(L+1)) EXIT
10       CONTINUE          


50    M=L+1 
      DUM=((X(M)-XIN)*(Y(M)-Y(L)))/(X(M)-X(L))
      YOUT=Y(M)-DUM

      IF(Y(M).EQ.BAD .OR. Y(L).EQ.BAD) YOUT = BAD
      IF (.NOT. INC) M = L
      RETURN


      END 

!     --------------------------------------------------------
!     Section 3: Routines relating to message decoding.
!     --------------------------------------------------------


!     -------------------------------------------------
      subroutine drop(lu,iwx,iflag,iyrs,imns,idys,line,acid,obnum)
!     decodes tempdrop message into spline format
!     -------------------------------------------------

      PARAMETER (PI = 3.141592654)

      character*1 clat,clon
      character*2 header(12)
      character*4 tail
      character*30 blank
      character*1  bblank
      character*5 acid
      character*2 obnum
      character*70 line
      character*80 dropl(200)
      character*200 remark
      CHARACTER*9 sondeid
      CHARACTER*10 loglocation
      CHARACTER*60 logcomment
      dimension prs(12)
      logical plev(12),knots,skpwind,dodrop
      integer dcount
      common /dropdata/idropl,dropl
      COMMON /PARAM/ LUFI,LUFX,LUFW,LUFP,LUFO,LUPR, &
                    NPLTFORM,NSNDTYPE,FALLRATE,DATARATE
      COMMON /MORLOG/ spglat, spglon, ispgtime, spgsecstoadd
      COMMON /DOTLOG/ idropnumber, logtime, rloglat, rloglon, &
                     rlogsp, rlog10ws, rlog10wd, rlogmblws, rlogmblwd,&
                     rlogbtsst, loglocation, sondeid, logcomment

      data header /'99','00','92','85','70','50','40','30', &
                   '25','20','15','10'/
      data prs /1070.,1000.,925.,850.,700.,500.,400.,300., &
                250.,200.,150.,100./
      data plev /12*.false./
      data blank /'                             '/
      data bblank/' '/

      idropl = 0
      ihhmm = 9999
      lvl=0
      sfcp = 9999.
      splat = -999.
      splon = -999.
      dodrop = .false.
      obnum = "00"

print *, 'Processing next drop'
!     ------------------------------------------------
!     if iflag=1 then we are already at the XXAA line
!     ------------------------------------------------
!     read the line with the mission number
!     -------------------------------------
 10   if (iflag.ne.1) then
         read(12,'(a)',end=99) line  ! corrected previous statement ??? pal
! but then not sure if intent was to skip a blank line(or lines) or to input
! the mission number, or to read(skip) to the end of file??? so left it as is.
         if(line(1:30).eq.blank)goto 10
      endif

!     read the first data line
!     ------------------------
 40   if (iflag.ne.1) then
         read(12,'(a)',end=99)line
         if(line(1:30).eq.blank)goto 40
      endif

!     read the value of the day
!    check if the winds are in knots or m/s
!     --------------------------------------
      read (line(7:8),'(i2)') iday
      if (iday.gt.50) then
         iday=iday-50
         knots = .true.
         else
         knots = .false.
      endif
!     check for month, year flips
!     ---------------------------
      yy = iyrs
      mm = imns
      if (iday.lt.idys) then
         mm = imns+1
         if (mm.eq.13) then
            mm = 1
            yy = iyrs + 1.
            if (yy.eq.100.) yy = 00.
         endif
      endif
       
      yymmdd = yy * 10000. + float(mm) * 100. + float(iday)

!     read the value of the hour
!     --------------------------
      read (line(9:10),'(i2)') ihour
      igmt = ihour * 100

!     set the value of the highest mandatory wind level reporting
!     -----------------------------------------------------------
      if(line(11:11).eq.'/')then
        maxlvl=1
      elseif(line(11:11).eq.'0')then
        maxlvl=2 
      elseif(line(11:11).eq.'9')then
        maxlvl=3
      elseif(line(11:11).eq.'8')then
        maxlvl=4
      elseif(line(11:11).eq.'7')then
        maxlvl=5
      elseif(line(11:11).eq.'5')then
        maxlvl=6
      elseif(line(11:11).eq.'4')then
        maxlvl=7
      elseif(line(11:11).eq.'3')then
        maxlvl=8
      elseif(line(11:11).eq.'2')then
        maxlvl=10
      elseif(line(11:11).eq.'1')then
        maxlvl=12
      endif

!     read the latitude
!     -----------------

      read (line(15:17),'(f3.1)') alat
 
!     read the quadrant,longitude
!     ---------------------------

      read (line(19:23),'(i1,f4.1)') nquad,alon

!     Assign negative sign to east or south (HSA convention)
!     ------------------------------------------------------

      if (nquad.eq.1 .or. nquad.eq.3) alon = -alon
      if (nquad.eq.5 .or. nquad.eq.3) alat = -alat
!
!     go to column 31 to read the surface group
!     -----------------------------------------

      itag=31

!     Go on to next mandatory level
!     --------------------------------------------

200   CONTINUE  ! for each mandatory level do
        do 205 l = 1,12
          plev(l)=.false.
205     continue

!       count the number of the mandatory level
!       ---------------------------------------

        lvl=lvl+1

!       check to see if 925 level is missing
!       ------------------------------------

        if (lvl.eq.3 .and. line(itag:itag+1).eq.'85') lvl=lvl+1

!       return point for trop and max wind levels
!       -----------------------------------------

210     press = -99.
        temp = -99.
        rh = -99.
        geopot = -99.
        wdir = -99.
        wspd = -99.
        skpwind = .false.

        if(line(itag:itag+1).eq.header(lvl))then  ! decode mandatory level.
          plev(lvl)=.true.
          press=prs(lvl)
          call geo (line,itag,plev,geopot,sfcp)
          itag=itag+6
          call tagtst(itag,line)
          pressx = press
          if (press.eq.1070. .and. sfcp.le.1070.) pressx = sfcp
          call temdew (line,itag,pressx,temp,rh)
          if(lvl.le.maxlvl)then  ! calc. bottom up through each mandatory level.
            itag=itag+6
            call tagtst(itag,line)

!      check if sfc wind group is missing
!           ----------------------------------

            IF (lvl.EQ.1) THEN

              IF (line(itag:itag+1) .eq. '00' .and. &
                line(itag + 6:itag + 7).NE.'00') THEN

                skpwind = .true.
              ELSE
                call wind (line,itag,wdir,wspd,*99)
                call dstouv (wdir,wspd,alat,alon,knots)
                skpwind = .false.

              ENDIF  ! wind group not missing

            
              ELSE  ! not lvl 1
              IF (line(itag:itag+1) .eq. '00' .and. &
                line(itag + 6:itag + 7).NE.'00') THEN
                skpwind = .true.
              ELSE
                call wind (line,itag,wdir,wspd,*99)
                call dstouv (wdir,wspd,alat,alon,knots)
                skpwind = .false.
              ENDIF

            ENDIF  ! other than level 1

          ENDIF  ! calulating bottom up.
             if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99. &
            .or. wdir .ne. -99. .or. wspd .ne. -99.)then
             call out(lu,iwx,yymmdd,igmt,alat,alon,press,temp,rh, &
                 geopot,wdir, wspd,1,acid,obnum)
          endif
         
!          if (.not. skpwind) then
            itag=itag+6
            call tagtst(itag,line)
!          endif

        go to 200  ! end mandatory levels


!     first shift mand levels lat lons then decode tropopause
!     -----------------

      elseif (line(itag:itag+1) .eq. '88') then
        if(line(itag+2:itag+4).eq.'999')then  ! now do tropopause calcs.
          itag=itag+6
          call tagtst(itag,line)
          goto 210
        endif
        geopot = -99.
        read (line(itag+2:itag+4),'(f3.0)') press
        itag=itag+6
        call tagtst(itag,line)
        pressx = press
        call temdew (line,itag,pressx,temp,rh)
        itag=itag+6
        call tagtst(itag,line)
        call wind (line,itag,wdir,wspd,*99)
        call dstouv (wdir,wspd,alat,alon,knots)
        if (temp .ne. -99. .or. rh .ne. -99. .or. &
         wdir .ne. -99. .or. wspd .ne. -99.)call out (lu, &
         iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir, &
         wspd,7,acid,obnum)
!        endif
        itag = itag+6
        call tagtst(itag,line)
        goto 210

!     decode max wind level
!     ---------------------

      elseif (line(itag:itag+1) .eq. '77' .or. &
       line(itag:itag+1) .eq. '66') then
        if(line(itag+2:itag+4).ne.'999')then
          read (line(itag+2:itag+4),'(f3.0)') press
          itag=itag+6
          call tagtst(itag,line)
          call wind (line,itag,wdir,wspd,*99)
          call dstouv (wdir,wspd,alat,alon,knots)
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99. &
           .or. wdir .ne. -99. .or. wspd .ne. -99.)call out (lu, &
           iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir, &
           wspd,6,acid,obnum)
        endif

      endif  ! decode mandatory, tropopause, or,  max wind level.

!dbug
print *, "reading spg/rel "

!     end of part A decoding.  Now look for significant level data.
!     -------------------------------------------------------------
60    read(12,'(a)',end=99)line

!     check if the line has data or duplicates part B
!     decode splash location.
!     -----------------------------------------------

      if(line(1:30).eq.blank) goto 60
      if(line(1:5).eq.'31313') goto 60
      if(line(1:5).eq.'51515') goto 60
      if(line(1:5).eq.'61616') goto 60
      if(line(1:5).eq.'62626') then
        remark = ' '
        itag = 1
        ix = 0

62      ix = ix+1
        if (line(itag:itag).eq.'=') goto 63
        remark(ix:ix) = line(itag:itag)
        itag = itag+1

! KSellwood this returns the wrong value if spg/rel data is continued 
! onto next line so continue to read until you reach an "="

        if (itag.eq.66) then
           if(line(66:66).eq." ")then
             read(12,'(a)') line
             itag = 1
            endif
        endif
        if (itag.eq.67) then
           read(12,'(a)') line
           itag = 1
        endif
          if (line(1:4).eq.'XXBB') goto 63

        if (itag.eq.131) then
#           if(line(131:131).eq." ")then
#              read(12,'(a)') line
#              itag = 1
#           endif
        endif
        if(itag.eq.132)then
           read(12,'(a)') line
        endif

          if (line(l:4).eq.'XXBB') go to 63
        goto 62


63          do 64 i=1,ix
          if (remark(i:i+3).eq.'SPL ') then
            read(remark(i+4:i+19),'(2i2,a1,i3,i2,a1,i4)') &
            ilat1,ilat2,clat,ilon1,ilon2,clon, ihhmm
            splat = float(ilat1)+float(ilat2)/100.
            if (clat.eq.'S') splat = -splat
            splon = float(ilon1)+float(ilon2)/100.
            if (clon.eq.'E') splon = -splon
            ispgtime=ihhmm * 100
!#            igmt=ihhmm    
            spglat=splat
            rloglat=splat
            spglon=splon
            rloglon=splon
            logtime=ispgtime
          print *, "time, lat lon from SPL", igmt, spglat, spglon
          endif
64      continue  ! enddo 64 

        DO 65 i = 1, ix  ! get rel and spg; lat, lon and time.  pal
          IF (remark(i:i+3).EQ.'REL ') THEN
            READ(remark(i+4:i+14), '(2I2, A1, I3, I2, A1)') &
            irellat1, irellat2, clat, irellon1, irellon2, clon
            rloglat = FLOAT(irellat1) + FLOAT(irellat2) / 100.
            IF (clat.eq.'S') rloglat = -rloglat
            rloglon = FLOAT(irellon1) + FLOAT(irellon2) / 100.
            IF (clon.EQ.'E') rloglon = -rloglon
            READ(remark(i+16:i+21), '(I6)') logtime
            READ(remark(i+27:i+37), '(2I2, A1, I3, I2, A1)') &
            ispglat1, ispglat2, clat, ispglon1, ispglon2, clon
            spglat = FLOAT(ispglat1) + FLOAT(ispglat2) / 100.
            IF (clat.eq.'S') spglat = -spglat
            spglon = FLOAT(ispglon1) + FLOAT(ispglon2) / 100.
            IF (clon.EQ.'E') spglon = -spglon
            READ(remark(i+39:i+44), '(I6)') ispgtime
!#            igmt = ispgtime/100
!#            dodrop = .true.
          ENDIF
65      CONTINUE  ! ENDDO 65
         print *, "location / time from REL", rloglat, rloglon, logtime, spglat, spglon, ispgtime

        do 66 i=1,ix
          if (remark(i:i+7).eq.'DLM WND ') then
            itagr = 9
            call wind (remark(i:i+20),itagr,wdir,wspd,*99)
            call dstouv (wdir,wspd,alat,alon,knots)
            read(remark(i+14:i+19),'(2i3)') ip1,ip2
            if (ip1.lt.ip2) ip1 = ip1+1000
            geopot = float(ip1+ip2)/2.0
            press = 1099.
            temp = -99.
            rh = -99.
            if (wdir .ne. -99. .or. wspd .ne. -99.)call out (lu, &
              iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,&
              wspd,8,acid,obnum)

          endif
66      continue  ! enddo 66

        if (line(1:4).ne.'XXBB') goto 60

        endif  ! '62626'
!dbug
         print *, "FINISHED WITH REMARK SECTION"


!     check significant level data 
!     ----------------------------
!dbug
      print *, "NOW READING SIGNIFICANT LEVELS"
 75   if(line(1:4).eq.'XXBB')then
        itag=31

!       added check for case of no sigt/h data -- jlf 1/98
!       --------------------------------------------------
                                                                                            
        call tagtst(itag,line)
        if(line(itag:itag+4).eq.'21212' .or. &
           line(itag:itag+4).eq.'31313') goto 75

        ihead=-11
 70     ihead=ihead+11
        if(ihead.gt.99)ihead=11
        read(line(itag:itag+1),'(i2)')jhead

        if(jhead.eq.ihead)then
          if(line(itag+2:itag+4).eq.'///') then
             itag=itag+6
             call tagtst(itag,line)
             itag=itag+6
             goto 71
             endif
          read(line(itag+2:itag+4),'(i3)')iprs
          if(iprs.lt.100)iprs=iprs+1000.
          press=iprs
          itag=itag+6
          call tagtst(itag,line)
          call temdew (line,itag,press,temp,rh)
          geopot = -99.
          wdir = -99.
          wspd = -99.
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99. &
              .or. wdir .ne. -99. .or. wspd .ne. -99.)call out (lu, &
              iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir, &
              wspd,2,acid,obnum)
!dbug
          itag=itag+6
 71       call tagtst(itag,line)

          if(line(itag:itag+4).eq.'21212' .or. &
            line(itag:itag+4).eq.'31313')goto 75
          goto 70
        endif

!     decode signficant wind levels--added surface decode--SEF 12/17/99
!     -----------------------------------------------------------------
! dbug
          itag=itag+6

       elseif(line(itag:itag+4).eq.'21212')then
        itag=itag+6
        ihead=-11
 30     ihead=ihead+11
        if(ihead.gt.99)ihead=11
        read(line(itag:itag+1),'(i2)',err=75)jhead
        if(jhead.eq.ihead)then
          if(line(itag+2:itag+4).eq.'///') then
             itag=itag+6
             call tagtst(itag,line)
             itag=itag+6
             goto 31
          endif
          read(line(itag+2:itag+4),'(i3)')iprs
          if(iprs.lt.100)iprs=iprs+1000.
          press=iprs
          itag=itag+6
          call tagtst(itag,line)
          call wind (line,itag,wdir,wspd,*99)
          call dstouv (wdir,wspd,alat,alon,knots)
          temp=-99.
          rh=-99.
          geopot=-99.
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99. &
               .or. wdir .ne. -99. .or. wspd .ne. -99.)call out (lu, &
               iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir, &
               wspd,2,acid,obnum)
          itag=itag+6
          call tagtst(itag,line)
 31       goto 30
        endif 
        goto 75

      elseif(line(itag:itag+4).eq.'31313')then
!dbug
        read (line(itag+13:itag+16),'(i4)') ihhmm
        itag = itag+19
        call tagtst(itag,line)
        goto 75

!     decode extrapolated levels in additional data groups
!     ----------------------------------------------------

      elseif(line(itag:itag+4).eq.'51515')then
!dbug

        itag = itag+6
        call tagtst(itag,line)
500     if (line(itag:itag+4).eq.'10190') then
          itag = itag+6
          call tagtst(itag,line)
          do 505 l = 1,12
            plev(l)=.false.
505         continue
          press = -99.
          temp = -99.
          rh = -99.
          geopot = -99.
          wdir = -99.
          wspd = -99.

          do 510 l = 1,12
            if(line(itag:itag+1).eq.header(l))then
               plev(l)=.true.
               press=prs(l)
               call geo (line,itag,plev,geopot,sfcp)
               if (geopot .ne. -99.)call out(lu,iwx,yymmdd,igmt, &
              alat,alon,press,temp,rh,geopot,wdir,wspd,3,acid,obnum)
               endif
510         continue
          itag = itag+6
          call tagtst(itag,line)
          goto 500


!       added loop to re-check for extrapolated levels if doubtful
!       temperature or height groups first appear--SEF 12/17/99
!       ----------------------------------------------------------

        elseif(line(itag:itag+3) .eq. '1016') then
          itag = itag + 12
          call tagtst (itag,line)
          goto 500
        endif
 
        elseif(line(1:4).eq.'NNNN')then
        call dropout(lu,igmt,alat,alon, sfcp,dodrop,acid,obnum)
        return

      elseif(line(1:5).eq.'Sonde')then
        call dropout(lu,igmt,alat,alon, sfcp,dodrop,acid,obnum)
        return
!      endif

      elseif(line(1:3).eq.'000')then
        call dropout(lu,igmt,alat,alon, sfcp,dodrop,acid,obnum)
        return
!      endif 
      elseif(line(1:5).eq.'SONDE')then
        call dropout(lu,igmt,alat,alon, sfcp,dodrop,acid,obnum)
        return
      endif


 99      call dropout(lu,igmt,alat,alon, sfcp,dodrop,acid,obnum)
      return
      end

! ------------------------------------------------------------------
      subroutine geo (line,lptr,plev,geopot,sfcp)
! ------------------------------------------------------------------

      character*70 line
      logical plev
      dimension plev(12)

!     extract the geopential height (modifications by JLF 11/92)

      if (line(lptr+2:lptr+4) .ne. '///') then
        read (line(lptr+2:lptr+4),'(f3.0)') geopot

        if (plev(1)) then                          ! Surface
          if (geopot .lt. 100.) geopot = geopot + 1000.
          sfcp = geopot
          endif

        if (plev(2)) then                          ! 1000 mb
          if (geopot .ge. 500.) geopot = -(geopot-500.)
          endif

        if (plev(3)) then
          if (sfcp.le.925..and.geopot.ge.500.) geopot=-(geopot-500.)
          endif

        if (plev(4)) then
          geopot = geopot+1000.
          if (sfcp.le.950..and.geopot.gt.1500.) geopot=geopot-1000.
          endif

        if (plev(5)) then                          ! 700 mb
          add = 2000.
          if (geopot .lt. 500.) add = 3000.
          if (sfcp.lt.960.) add = 2000.
         geopot = geopot + add
          endif

        if (plev(6) .or. plev(7)) then             ! 500, 400 mb
          geopot = geopot * 10.
          endif

                                                   ! >= 300 mb
        if (plev(8) .or. plev(9) .or. plev(10) &
           .or. plev(11) .or. plev(12)) then 
          geopot = geopot * 10.
          if (geopot.lt.8500.) geopot = geopot + 10000.
          endif
!dbug
!try using default value of 1000mb when no surface pressure is given
!this values is only used for estimating conversion of m/s to mb/s
      else
         if(plev(1)) then
            geopot=1000
            sfcp=geopot
         endif
      endif
      return 
      end


! ---------------------------------------------------------------------
      subroutine wind (line,lptr,wdir,wspd,*)
! ---------------------------------------------------------------------

      character*70 line

!     extract the wind direction and speed
!
      if (line(lptr:lptr+4) .ne. '/////') then
        read (line(lptr:lptr+1),'(f2.0)') wdir
        read (line(lptr+2:lptr+4),'(f3.0)') wspd
      else 
        wdir = -99.
        wspd = -99.
      endif
      return
      end


! ---------------------------------------------------------------------
      subroutine dstouv (wdir,wspd,alat,alon,knots)
! ---------------------------------------------------------------------

      logical knots
      real alat,alon,wdir,wspd

!     convert wind direction and speed to u, v (in m/s)

      if (wdir .ne. -99.) then
        wdir = wdir * 10. 
        if(wspd.ge.500.0)then
          wspd=wspd-500.
          wdir=wdir+5
        endif
        if (knots) wspd = 0.514444 * wspd
        call uvcomp (wdir,wspd)
      endif
      return
      end


! -----------------------------------------------------------------------
      subroutine tagtst(itag,line)
! -----------------------------------------------------------------------

      character*70 line

!     check if the end of the line has been reached and the next line should 
!     be read

                                                                             !      if(itag.gt.47)then
        if(itag.lt.66)then
          do i=itag,itag+5
            if(line(i:i).ne.' ')return
          end do
        endif
        read(12,'(70a)',end=99)line
        itag=1
!     endif
 99   return
      return
      end

! ------------------------------------------------------------------------
      subroutine uvcomp (dir,spd)
! ------------------------------------------------------------------------


!     this subroutine changes dir to u, and spd to v, where dir is
!     given in meteorological degrees.  The original values of dir
!     and spd are destroyed.

      dir = dir
      spd = spd
      return
      end

! ------------------------------------------------------------------------
      subroutine temdew (line,lptr,press,temp,rh)
! ------------------------------------------------------------------------
      character*70 line

!     extract the temperature

      temp = -99.
      rh = -99.

      if (line(lptr:lptr+2) .ne. '///') then
        read (line(lptr:lptr+2),'(f3.1)') atemp
        read (line(lptr+2:lptr+2),'(i1)') ifrac
        if (mod(ifrac,2) .eq. 0) then
          temp = atemp
        else
          temp = -atemp
        endif
      endif

!     extract the dewpoint depression

      if (line(lptr+3:lptr+4) .ne. '//') then
        read (line(lptr+3:lptr+4),'(i2)') idd
        if (idd .gt. 50) then
          dd = float (idd - 50)
        else
          dd = float (idd) / 10.
        endif
        dewpt = temp - dd
        call relhum (press,temp,dewpt,rh)
      endif
      return
      end


! -----------------------------------------------------------------------
      subroutine relhum (press,temp,dewpt,rh)
! -----------------------------------------------------------------------

      parameter (tkelvn = 273.16)
      parameter (em = 9.4051)
      parameter (e = 2353.)

!     compute the relative humidity using the vapor pressure vprs
!     and the saturation vapor pressure svprs

      vprs = 10**(em - e / (dewpt + tkelvn))
      svprs = 10**(em - e / (temp + tkelvn))
      fmixr = vprs / (press - vprs)
      smixr = svprs / (press - svprs)
      rh = 100. * fmixr / smixr
      if(rh.gt.100.)rh=100.
      return
      end

! -----------------------------------------------------------------------
      subroutine out (lu,iwx,yymmdd,igmt,alat,alon,press, &
                       temp,rh,geopot,wdir,wspd,msgtype,acid,obnum)
! ----------------------------------------------------------------------

      character*4 tail
      character*5 acid
      character*2 obnum
      character*80 dropl(200)
      character*91 dropl2(200)
      real alat,alon,wdir,wspd

      common /output/nrecs
      common /dropdata/idropl,dropl,dropl2


      tail='0000'
      if (msgtype.eq.1) tail = 'MANL'     ! DROP/Mandatory
      if (msgtype.eq.2) tail = 'SIGL'     ! DROP/Significant
      if (msgtype.eq.3) tail = 'ADDL'     ! DROP/Additional (51515)
      if (msgtype.eq.4) tail = 'RECO'     ! RECCO
      if (msgtype.eq.5) tail = 'SUPV'     ! SUPPL VTX
      if (msgtype.eq.6) tail = 'MWND'     ! DROP/Max wind
      if (msgtype.eq.7) tail = 'TROP'     ! DROP/Tropopause
      if (msgtype.eq.8) tail = 'DLMW'     ! DROP/DLM wind
      nrecs = nrecs+1

!     Write it to the output file.
!     ------------------------------------------------

!      if (msgtype.le.3 .or. msgtype.eq.6 .or. msgtype.eq.7 .or. &
!          msgtype.eq.8) then
         idropl = idropl+1
         write(dropl(idropl),510) iwx,yymmdd,igmt,alat,alon,press, &
                    temp,rh,geopot,wdir,wspd,tail
         write(dropl2(idropl),511) iwx,yymmdd,igmt,alat,alon,press, &
                    temp,rh,geopot,wdir,wspd,tail,acid,obnum

!         write(15,*) dropl(idropl)

!         else
!         write (lu,510) iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot, &
!                    wdir,wspd,tail
!         endif

      return
510   format (i2,1x,f7.0,1x,i4,1x,f7.3,1x,f8.3,1x,3(f6.1,1x), &
       f7.1,1x,2(f6.1,1x),a4)
511   format (i2,1x,f7.0,1x,i4,1x,f7.3,1x, f8.3,1x,3(f6.1,1x), &
       f7.1,1x,2(f6.1,1x),a4,1x,a5,1x,a2)
!611   format (i2,1x,f7.0,1x,i4,1x,f7.3,1x,f8.3,1x,3(f6.1,1x), &
!       f7.1,1x,2(f6.1,1x),a4,1x,a5)

      end


! --------------------------------------------------------------------------
      subroutine dropout(lu, itime, splat, splon, sfcp,dodrop,acid,obnum)
! --------------------------------------------------------------------------

      character*80 dropl(200),dropx,dropsh
      character*91 dropl2(200),dropxx
      CHARACTER*4 tail
      CHARACTER*2 obnum
      CHARACTER*9 sondeid
      CHARACTER*10 loglocation
      CHARACTER*60 logcomment
      dimension press(200)
      logical sort, dodrop

      common /dropdata/idropl,dropl,dropl2
      COMMON /PARAM/ LUFI,LUFX,LUFW,LUFP,LUFO,LUPR, &
                    NPLTFORM,NSNDTYPE,FALLRATE,DATARATE
      COMMON /MORLOG/ spglat, spglon, ispgtime, spgsecstoadd
      COMMON /DOTLOG/ idropnumber, logtime, rloglat, rloglon, &
                     rlogsp, rlog10ws, rlog10wd, rlogmblws, rlogmblwd, &
                     rlogbtsst, loglocation, sondeid, logcomment

      DATA PI/3.14159265359/, bad/-999./
      sort = .true.

      do 100 i = 1,idropl

         READ(dropl(i)(34:39), *) press(i)
         READ(dropl(i)(17:23), *) storedlat
         READ(dropl(i)(25:32), *) storedlon
           if (splat.ne.-999.)then
               write(dropl(i)(17:33),'(f7.3,1x,f8.3),1x') splat,splon
               write(dropl2(i)(17:33),'(f7.3,1x,f8.3),1x') splat,splon
           endif
!#        itimesecs = HMSTS(logtime)
!#        call NRMIN(itimesecs,round)
!#        itime = logtime/100 + round
 100      continue


         DO i = 1, idropl
         write(dropl(i)(12:15),'(i4)') itime
         write(dropl2(i)(12:15),'(i4)') itime
         ENDDO
!    -----------------
!     sort by pressure
!     ----------------

      do 200 i=1,idropl-1
         do 210 j=i+1,idropl
            if (press(i).lt.press(j)) then
               dropx = dropl(j)
               dropxx = dropl2(j)
               pressx = press(j)
               do 220 k=j,i+1,-1
                  dropl(k) = dropl(k-1)
                  dropl2(k) = dropl2(k-1)
                  press(k) = press(k-1)
220            continue
               dropl(i) = dropx
               dropl2(i) = dropxx
               press(i) = pressx
         endif
210      continue
200   continue    

      return
      end


!--------------------------------
      SUBROUTINE NRMIN(TIME,ROUND)
!--------------------------------

!Value of round determines whether to round up to next minute 

        INTEGER:: ISECS, IMIN, ROUND
!       REAL, INTENT IN::TIME
!       INTEGER, INTENT OUT:: ROUND
        REAL::TIME

        ISECS = MOD(INT(TIME), 3600)
        IMIN = ISECS/60
        ISECS= MOD(isecs, 60)

        ROUND = 0

        IF(ISECS.GT.30) THEN
            IF(IMIN.GE.59)THEN
               ROUND = 41
            ELSE
            ROUND = 1
            ENDIF
        ELSE
           ROUND = 0
        ENDIF

        RETURN
        END

!----------------------
        FUNCTION HMSTS(X)
!-----------------------

!       INTEGER, INTENT INOUT:: X
        INTEGER:: IHR, IMIN, ISECS
        INTEGER:: X

        IHR = X/10000
        IMIN = MOD(X,10000)/100
        ISECS = MOD(MOD(X,10000),100)

       HMSTS = (IHR * 3600) + (IMIN * 60) + ISECS

       RETURN
       END



!----------------------- 
      FUNCTION STHMS(X)
!----------------------- 

!        INTEGER, INTENT INOUT:: X
         INTEGER:: IHR, IMN, ISECS
         INTEGER:: X

         IHR = X/3600
         IMN = MOD(X,3600)/60
         ISECS = MOD(MOD(X,3600),60)
         STHMS = (IHR*10000) + (IMN*100) + ISECS
         RETURN
         END

