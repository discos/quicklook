pro fitslook, pin=pin, x=x, y=y, help=help, freewin=freewin

  ; The procedure iteratively lists the folders in the given
  ; path (pin) and displays on screen **all** the n_sect data streams
  ; of the last complete FITS (ESCS>0.3/Nuraghe>0.3) file recorded in the
  ; last written folder.
  ; Use command /help to have on-line suggestions.
  ;
  ; TOTAL POWER FITS
  ; For continuum observations, the X-axis can be represented as
  ;      sample number (if xtype='s') - default
  ;      elapsed time from the acquisition start (if xtype='t'),
  ;      scanning axis (if x='x')
  ;      azimuth degrees (if xtype='a'),
  ;      elevation degrees (if xtype='e'),
  ;      declination degrees (if xtype='d'),
  ;      right ascension hh.hhh (if xtype='r').
  ; The Y-axis can be represented as
  ;      counts (if y='raw')
  ;      antenna temperature (if y='atemp')
  ;
  ; SPECTRAL FITS
  ; When displaying spectral data, the X-axis is always given as bin number
  ; and the Y-axis is given in raw counts.
  ;
  ; Usage:
  ; fitslook [,pin=] [,x=] [,y=] [,/help]
  ; where
  ; pin = full path to data folder (the one containing the scans subfolders)
  ; x = letter indicating the choice for the x-axis label, default is 's' (for TP only)
  ; y = choice of the data stream, default is 'raw', displaying raw counts,
  ;     while using 'atemp' the antenna temperature - if available (TP only) - is shown
  ;
  ; NOTICE: if using the /freewin option, you will be able to resize the plot
  ; window as you like. Otherwise, it will almost fill up all the available screen.
  ;
  ; if pin is not provided, the path to the data is by default obtained as:
  ; /archive/data/username/yyyymmdd
  ; where username and yyyymmdd are extracted from the running system.

  ; Author: S.Righini
  ; Latest update: March 15th 2016


  timestring=systime(/UTC)
  timeelem=strsplit(timestring,' ',/extract)
  ; DOW MON DD HH:MM:SS YEAR
  year=timeelem[4]
  mon=timeelem[1]
  day=timeelem[2]
  case mon of
    'Jan': mons='01'
    'Feb': mons='02'
    'Mar': mons='03'
    'Apr': mons='04'
    'May': mons='05'
    'Jun': mons='06'
    'Jul': mons='07'
    'Aug': mons='08'
    'Sep': mons='09'
    'Oct': mons='10'
    'Nov': mons='11'
    'Dec': mons='12'
  endcase

  day=string(timeelem[2],format='(I02)')
  sp=path_sep()

  ; getting username from system
  spawn, 'whoami', username
  user=strcompress(username,/remove_all)
  defpath=sp+'archive'+sp+'data'+sp+user+sp+year+mons+day

  ; default settings
  if not (keyword_set(pin))  then pin  = defpath ; default folder for data storage
  if not (keyword_set(x))  then x  = 's'         ; x-axis = sample number
  if not (keyword_set(y))  then y  = 'raw'       ; y-axis = raw counts
  if not (keyword_set(freewin))  then rescale = 'RescaleNo' else rescale = 'RescaleYes' ; can users rescale the plot window?

  if (keyword_set(help) eq 1) then begin
    print, " The procedure iteratively lists the folders in the recording "
    print, " path and displays on screen **all** the available data streams "
    print, " of the last complete FITS (ESCS>0.3/Nuraghe>0.3) file recorded in the "
    print, " last written folder. At least, that's what it's supposed to do..."
    print, " "
    print, " **************** "
    print, " TOTAL POWER FITS "
    print, " **************** "
    print, " Usage: "
    print, " fits_look [,pin=] [,x=] [,y=] [,/help] [,/freewin]"
    print, " where the optional parameters are: "
    print, "  pin = full path to 'parent' data storage folder (the one containing the scan subfolders)"
    print, "  x = letter indicating the choice for the X-axis "
    print, "  y = choice of the data stream, default is 'raw', displaying raw counts,"
    print, "      while using 'atemp' the antenna temperature - if available - is shown"
    print, " If pin is not provided, the path to the data is by default obtained as: "
    print, " /archive/data/username/yyyymmdd "
    print, " where username and yyyymmdd are extracted from the running system. "
    print, " The X-axis can be represented as "
    print, "      sample number (if x='s') - default "
    print, "      elapsed time from the acquisition start (if x='t'), "
    print, "      scanning axis (if x='x') "
    print, "      azimuth degrees (if x='a'), "
    print, "      elevation degrees (if x='e'), "
    print, "      declination degrees (if x='d'), "
    print, "      right ascension hh.hhh (if x='r'). "
    print, " The Y-axis can be represented as "
    print, "      counts (if y='raw') "
    print, "      antenna temperature (if y='atemp') "
    print, " "
    print, " ************* "
    print, " SPECTRAL FITS "
    print, " ************* "
    print, " Usage: "
    print, " fits_look [,pin=] [,/help] [,/freewin] "
    print, " where the optional parameter is: "
    print, "  pin = full path to data folder (the one containing the scans subfolders) "
    print, "  if pin is not provided, the path to the data is by default obtained as: "
    print, "  /archive/data/username/yyyymmdd "
    print, "  where username and yyyymmdd are extracted from the running system. "
    print, " When displaying spectral data, the X-axis is always given as bin number "
    print, " and the Y-axis is given in raw counts, even if the x= and y= parameters are
    print, " set to different options. "
    print, " "
    print, "NOTICE: if using the /freewin option, you will be able to resize the plot "
    print, "window as you like. Otherwise, it will almost fill up the screen.
    print, " "
    print, "It is assumed that FITS files have been acquired "
    print, "with Nuraghe >0.3 or ESCS >0.3 "
    print, "Best of luck! "
  endif
  
    ; setting the plot depending on the operating system
  if path_sep() eq '/' then begin
    set_plot, 'x'    ; this is a Linux/OSX machine
  endif else begin
    set_plot, 'win'  ; this is a Windows machine
  endelse

  if rescale eq 'RescaleNo' then begin
    ; producing a window of proper dimensions - according to the screen in use
    screensize = GET_SCREEN_SIZE(RESOLUTION=resolution)
    xscreen=screensize[0]-20
    yscreen=screensize[1]-20
    ; specific rescaling for the SRT control room large LCD monitor
    if xscreen gt 3000 then xscreen=1700
    ; now the window is actually generated
    WINDOW,0,XS=xscreen,YS=yscreen
  endif

  for i=0,15000 do begin
    ; selecting the last folder (where data is being written NOW)
    ; and refershing plots every 5 seconds
    sp=path_sep()
    listdir = file_search(pin+sp+'*', COUNT=num, /TEST_DIRECTORY, /FULLY_QUALIFY_PATH)
    p=listdir[-1]
    last_fits, path=p, xtype=x, ytype=y
    wait,5
  endfor
end

pro search_headkey, header, stringa, key, value, info, rflag
  ; searching values inside FITS headers, according to the desired keyword
  rflag=1
  search=strmatch(header, stringa)
  index=where(search eq 1)
  sp=path_sep()
  if index ne -1 then begin
    content=strsplit(header(index),'=',/extract)
    key=content[0]
    value_and_info=strsplit(string(content[1]),'/',/extract)
    if stringa ne '*ScheduleName =*' then begin
      value=value_and_info[0]
      info=value_and_info[1]
    endif else begin
      steps=n_elements(value_and_info)
      info=value_and_info[-1]
      value=value_and_info[0]
      for s=1, steps-2 do begin
        value=value+sp+value_and_info[s]
      endfor
    endelse
  endif else begin
    print, 'Header is not ready.'
    rflag=0
  endelse
end


pro last_fits, path=path, xtype=xtype, ytype=ytype

  ; path separator
  sp=path_sep()

  ; custom numbers for charsizes
  xxl=3.50
  large=1.70
  medium=1.55
  small=1.35
  
  ; listing the data FITS files written in the folder.
  ; by searching only for files named '2*.fits' the 'summary.fits' one is avoided
  list=file_search(path+sp+'2*.fits',COUNT=number,/FULLY_QUALIFY_PATH)
  if number eq 0 then begin
    print, 'No FITS files in folder. Waiting...'
    ;Empty window, printing info inside window
    plot, [0,100],[0,100], /NODATA, /DEVICE, xstyle=4, ystyle=4, $
    xticks=[1], xminor=1, $
    yticks=[1], yminor=1
    xyouts, 100,400,'No FITS files in folder. Waiting...',charsize=large*3, charthick=2.0, /DEVICE
    return
  endif

  ; finding the last readable FITS file in the folder
  if number eq 1 then begin
    finfo=file_info(list[0])
    print, 'File = ',list[0]
    openr, Unit, list[0], /get_lun
    eofflag=eof(Unit)
    close, Unit
    free_lun, Unit
  endif else begin
    finfo=file_info(list[-1])
    print, 'File = ',list[-1]
    openr, Unit, list[-1], /get_lun
    eofflag=eof(Unit)
    close, Unit
    free_lun, Unit
  endelse

  if eofflag eq 1 then begin
    if number eq 1 then i=0
    if number gt 1 then i=-1
  endif else begin
    if number eq 1 then begin
      print, 'No completed FITS files in folder. Waiting...'
      return
    endif else begin
      i=-2
      print, 'FITS under acquisition.... Plotting the last completed one.'
    endelse
  endelse

  ; reading the primary header
  mainh=mrdfits(list[i],0,head0,/silent)
  ; reading keywords from primary header
  tarflag=0
  subflag=0
  scanflag=0
  dateflag=0
  siteflag=0
  schedflag=0
  n_sectflag=0
  search_headkey, head0, '*SOURCE*', keylab, target, infolab, tarflag
  search_headkey, head0, '*SubScanID*', keylab, subscan, infolab, subflag
  search_headkey, head0, '*SCANID*', keylab, scan, infolab, scanflag
  search_headkey, head0, '*DATE*', keylab, date, infolab, dateflag
  search_headkey, head0, '*ANTENNA*', keylab, antenna, infolab, siteflag
  search_headkey, head0, '*SubScanType =*', keylab, scantype, infolab, typeflag
  search_headkey, head0, '*ScheduleName =*', keylab, schedule, infolab, schedflag
  search_headkey, head0, '*SECTIONS=*', keylab, n_sect, infolab, n_sectflag

  ; check on the number of keywords successfully retrieved
  totalflag=tarflag+subflag+scanflag+dateflag+siteflag+typeflag+schedflag+n_sectflag
  if totalflag eq 8 then begin
    ut=strmid(date,13,8)
  endif else begin
    print, 'Header is not complete.'
    return
  endelse

  ; handling keywords: extracting meaningful, clean portions
  site=strcompress(antenna,/remove_all)
  schedlabs=strsplit(schedule,sp,/extract)
  cleanname=strsplit(schedlabs[-1],"'",/extract)
  schedname=cleanname[0]
  cleantarget=strsplit(target,"'",/extract)
  target=cleantarget[1]
  cleantype=strsplit(scantype,"'",/extract)
  scant=strcompress(cleantype[1],/remove_all)

  ; reading RAW DATA TABLE (binary data table n.4 of the Nuraghe/ESCS0.3 FITS file)
  data=mrdfits(list[i],4,/SILENT)
  ndat=n_elements(data.time)

  ; reading ANTENNA TEMPERATURE TABLE (binary data table n.5 of the Nuraghe/ESCS0.3 FITS file)
  datak=mrdfits(list[i],5,/SILENT)

  ; computing percentage of "bad track" samples
  flag=ndat-total(data.flag_track)
  pflag=string(flag/ndat*100.0, format='(D5.1)')
  warn=pflag+'%'

  ; measuring the sampling interval
  dt=(data[-1].time-data[0].time)*86400.0/double(ndat-1)

  ; x-axis selection
  if (xtype eq "T") or (xtype eq "t") then begin
    xaxis = (data.time-data[0].time)*24.d0*3600.d0 ; time in sec and starting from the first sample
    xlabel = "Seconds from subscan start"
  endif else begin
    if (xtype eq "S") or (xtype eq "s") then begin
      xaxis = indgen(ndat)
      xlabel = "Sample number"
    endif else begin
      if (xtype eq "A") or (xtype eq "a") then begin
        xaxis = data.az/!pi*180.0
        xlabel = "Azimuth - degrees"
      endif else begin
        if (xtype eq "R") or (xtype eq "r") then begin
          xaxis = (data.raj2000/!pi*180.0)/15.0
          xlabel = "Right Ascension - decimal hours"
        endif else begin
          if (xtype eq "E") or (xtype eq "e") then begin
            xaxis = (data.el/!pi*180.0)
            xlabel = "Elevation - degrees"
          endif else begin
            if (xtype eq "D") or (xtype eq "d") then begin
              xaxis = (data.decj2000/!pi*180.0)
              xlabel = "Declination - degrees"
            endif else begin
              if (xtype eq "X") or (xtype eq "x") then begin
                if scant eq "DEC" then begin
                  xaxis = (data.decj2000/!pi*180.0)
                  xlabel = "Declination - degrees"
                endif
                if scant eq "RA" then begin
                  xaxis = (data.raj2000/!pi*180.0)/15.0
                  xlabel = "Right Ascension - hh.hh"
                endif
                if scant eq "AZ" then begin
                  xaxis = (data.az/!pi*180.0)
                  xlabel = "Azimuth - degrees"
                endif
                if scant eq "EL" then begin
                  xaxis = (data.el/!pi*180.0)
                  xlabel = "Elevation - degrees"
                endif
              endif else begin
                print, ' '
                print, " Wrong input for xtype variable."
                print, " It must be S or s for sample number"
                print, "            T or t for elapsed time"
                print, "            X or x for scanning axis"
                print, "            A or a for azimuth degrees"
                print, "            E or e for elevation degrees"
                print, "            R or r for RA hh.hhh"
                print, "            D or d for declination degrees"
                return
              endelse
            endelse
          endelse
        endelse
      endelse
    endelse
  endelse


  ; reading SECTION TABLE (binary data table n.1 of the Nuraghe/ESCS>0.3 FITS file)
  sectdata=mrdfits(list[i],1,/silent)
  bins=sectdata[0].bins

  ; the number of plots depends on the TP or Spectral flavour of FITS
  if bins eq 1 then begin
    datatype='CONTINUUM'
    streams=dblarr(n_sect,ndat)
    rows=ceil((n_sect+1)/5.0)
    if n_sect le 2 then !p.multi=[0,3,1] else !p.multi=[0,5,rows]
    ; labels for plots
    chtitle=["F0 L","F0 R","F1 L","F1 R","F2 L","F2 R","F3 L","F3 R","F4 L","F4 R","F5 L","F5 R", "F6 L","F6 R", $
             "F7 L","F7 R","F8 L","F8 R"]
  endif else begin
    datatype='SPECTRA'
    streams=dblarr(n_sect*2,bins)
    rows=ceil((n_sect*2+1)/5.0)
    if n_sect*2 le 2 then !p.multi=[0,3,1] else !p.multi=[0,5,rows]
    ; labels for plots
    chtitle=["Sect0 L","Sect0 R","Sect1 L","Sect1 R","Sect2 L","Sect2 R","Sect3 L","Sect3 R","Sect4 L","Sect4 R", $
            "Sect5 L","Sect5 R", "Sect6 L","Sect6 R","Sect7 L","Sect7 R","Sect8 L","Sect8 R"]
  endelse

  ; horizontal coordinates at mid-subscan 
  elevation=data[ndat/2].el/!dpi*180.0
  azimuth=data[ndat/2].az/!dpi*180.0

  ; introductory plot: overall subscan parameters
  plot, [0,1],[0,1], /NODATA, xstyle=4, ystyle=4, $
    xticks=[1], xminor=1, $
    yticks=[1], yminor=1
  xyouts, 0.1, 0.90, datatype, $
    charsize=xxl, charthick=0.4   
  xyouts, 0.1, 0.80, 'UT = '+ut, $
    charsize=large, charthick=0.4
  xyouts, 0.1, 0.70, 'Schedule = '+schedname, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.62, 'Scan '+strtrim(scan,2)+ ' - Subscan '+strtrim(subscan,2), $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.54, 'ScanType = '+scant, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.46, 'Target = '+target, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.38, 'Elevation = '+string(elevation,format='(D4.1)')+' deg', $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.26, 'Azimuth = '+string(azimuth,format='(D5.1)')+' deg', $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.18, 'Sampling int. = '+string(dt, format='(F6.3)')+' s', $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.10, 'Bad track samples = '+warn, $
    charsize=small, charthick=0.4

  if bins eq 1 then begin
    
    ; these are TOTAL POWER data
    
    for st=0, n_sect-1 do begin
      ; reading FEED TABLE (binary data table n.2 of the Nuraghe/ESCS0.4 FITS file)
      rftable=mrdfits(list[i],2,/SILENT)
      bandwidth=rftable[st].bandWidth
      bw=strcompress(string(bandwidth, format='(I4)'),/remove_all)+' MHz'
      frequency=(rftable[st].frequency+bandwidth/2.0)/1000.
      freq=strcompress(string(fix(frequency)),/remove_all)+' GHz'


      ; rms noise of the first 20% samples,
      ; after subtraction of a linear baseline fitted on the samples themselves
      lim=fix(ndat/5.)
      ; for CH0
      base0=streams[st,1:lim]
      lfit0=linfit(xaxis[1:lim],base0,yfit=lf0)
      sam0=base0-lf0
      mom = moment(sam0)

      if ytype eq 'atemp' then begin
        case st of
          0: streams(st,*)=datak.ch0
          1: streams(st,*)=datak.ch1
          2: streams(st,*)=datak.ch2
          3: streams(st,*)=datak.ch3
          4: streams(st,*)=datak.ch4
          5: streams(st,*)=datak.ch5
          6: streams(st,*)=datak.ch6
          7: streams(st,*)=datak.ch7
          8: streams(st,*)=datak.ch8
          9: streams(st,*)=datak.ch9
          10: streams(st,*)=datak.ch10
          11: streams(st,*)=datak.ch11
          12: streams(st,*)=datak.ch12
          13: streams(st,*)=datak.ch13
          14: streams(st,*)=datak.ch14
          15: streams(st,*)=datak.ch15
          16: streams(st,*)=datak.ch16
          17: streams(st,*)=datak.ch17
          18: streams(st,*)=datak.ch18
        endcase
        stat=moment(datak.ch0)
        if stat[0] gt 300.0 then ylabel='Counts' else ylabel='K'
      endif else  begin
        case st of
          0: streams(st,*)=data.ch0
          1: streams(st,*)=data.ch1
          2: streams(st,*)=data.ch2
          3: streams(st,*)=data.ch3
          4: streams(st,*)=data.ch4
          5: streams(st,*)=data.ch5
          6: streams(st,*)=data.ch6
          7: streams(st,*)=data.ch7
          8: streams(st,*)=data.ch8
          9: streams(st,*)=data.ch9
          10: streams(st,*)=data.ch10
          11: streams(st,*)=data.ch11
          12: streams(st,*)=data.ch12
          13: streams(st,*)=data.ch13
          14: streams(st,*)=data.ch14
          15: streams(st,*)=data.ch15
          16: streams(st,*)=data.ch16
          17: streams(st,*)=data.ch17
          18: streams(st,*)=data.ch18
        endcase
        ylabel='Counts'
      endelse

      ; setting fewer ticks when x-axis is a coordinate (to avoid label overlapping)
      if (xtype eq "s") or (xtype eq "S") or (xtype eq "t") or (xtype eq "T") then begin
        ntick=0
      endif else begin
        ntick=3
      endelse

      plot, xaxis,streams[st,*], $
        title = chtitle[st], $
        xtitle = xlabel, $
        ytitle = ylabel, $
        xticks=ntick, $
        ys=1, xs=1, psym=0,charsize=2
      top=max(streams[st,*])
      bottom=min(streams[st,*])
      step=(top-bottom)/10.0
      xyouts, n_elements(xaxis)/10, bottom+step, /DATA,'Cen.Freq = '+string(frequency, format='(D5.2)')+' GHz', $
        charsize=small, charthick=0.4
      xyouts, n_elements(xaxis)/10, bottom+0.5*step, /DATA,'BW = '+BW, $
        charsize=small, charthick=0.4

    endfor

  endif else begin

    for st=0, (n_sect*2)-1 do begin
      
      ; these are SPECTRAL DATA, so I force the output options
      
      xaxis=indgen(bins)
      xlabel='Bin number'

      ; reading FEED TABLE (binary data table n.2 of the Nuraghe/ESCS0.4 FITS file)
      rftable=mrdfits(list[i],2,/SILENT)
      bandwidth=rftable[st].bandWidth
      bw=strcompress(string(bandwidth, format='(D7.2)'),/remove_all)+' MHz'
      frequency=(rftable[st].frequency+bandwidth/2.0)/1000.
      freq=strcompress(string(fix(frequency)),/remove_all)+' GHz'

      case st of
        0: streams(st,*)=data[0].ch0[0:bins-1]
        1: streams(st,*)=data[0].ch0[bins:2*bins-1]
        2: streams(st,*)=data[0].ch1[0:bins-1]
        3: streams(st,*)=data[0].ch1[bins:2*bins-1]
        4: streams(st,*)=data[0].ch2[0:bins-1]
        5: streams(st,*)=data[0].ch2[bins:2*bins-1]
        6: streams(st,*)=data[0].ch3[0:bins-1]
        7: streams(st,*)=data[0].ch3[bins:2*bins-1]
        8: streams(st,*)=data[0].ch4[0:bins-1]
        9: streams(st,*)=data[0].ch4[bins:2*bins-1]
        10: streams(st,*)=data[0].ch5[0:bins-1]
        11: streams(st,*)=data[0].ch5[bins:2*bins-1]
        12: streams(st,*)=data[0].ch6[0:bins-1]
        13: streams(st,*)=data[0].ch6[bins:2*bins-1]
        14: streams(st,*)=data[0].ch7[0:bins-1]
        15: streams(st,*)=data[0].ch7[bins:2*bins-1]
        16: streams(st,*)=data[0].ch8[0:bins-1]
        17: streams(st,*)=data[0].ch8[bins:2*bins-1]
      endcase
      ylabel='Counts'


      plot, xaxis,streams[st,*], $
        title = chtitle[st], $
        xtitle = xlabel, $
        ytitle = ylabel, $
        xticks=ntick, $
        ys=1, xs=1, psym=0,charsize=2
      top=max(streams[st,*])
      bottom=min(streams[st,*])
      step=(top-bottom)/10.0
      xyouts, n_elements(xaxis)/10, bottom+step, /DATA,'Cen.Freq = '+string(frequency, format='(D5.2)')+' GHz', $
        charsize=small, charthick=0.4
      xyouts, n_elements(xaxis)/10, bottom+0.5*step, /DATA,'BW = '+BW, $
        charsize=small, charthick=0.4
    endfor

  endelse

  !p.multi=[0,1,1]

  return
end
