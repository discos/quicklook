pro onoff, dutyc=dutyc, pin=pin, freewin=freewin

  ; The procedure iteratively shows the *RAW* (ON-OFF)/OFF spectra as they are
  ; being recorded.The explicit specification of the duty cicle
  ; - as a string composed like 'N(ON):N(OFF):N(CAL)' - is mandatory
  ;
  ;  Usage:
  ;
  ;   onoff, dutyc='ON:OFF:CAL' [,pin=] [,/help]
  ;
  ;  where the optional parameter is: "
  ;   pin = full path to data folder (the one containing the scans subfolders)
  ;  If pin is not provided, the path to the data is by default obtained as:
  ;   /archive/data/username/yyyymmdd
  ;  where username and yyyymmdd are extracted from the running system.
  ;  When displaying spectral data, the X-axis is always given as bin number
  ;  and the Y-axis is given in raw counts
  ;
  ; NOTICE: if using the /freewin option, you will be able to resize the plot
  ; window as you like. Otherwise, it will almost fill up all the available screen.
  ;
  ;  It is assumed that FITS files have been acquired
  ;  with Nuraghe >0.3 or ESCS >0.3
  ;  Best of luck!

  ;  Author: S.Righini
  ;  Latest update: March 31th 2016


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
  if not (keyword_set(dutyc))  then begin
    print, "Duty cycle MUST be explicitly given giving dutyc='N(ON):N(OFF):N(CAL)'"
    return
  endif

  print, dutyc

  if not (keyword_set(freewin))  then rescale = 'RescaleNo' else rescale = 'RescaleYes' ; can users rescale the plot window?

  if (keyword_set(help) eq 1) then begin
    print, " The procedure iteratively shows the *RAW* (ON-OFF)/OFF spectra as they are "
    print, " being recorded.The explicit specification of the duty cicle "
    print, " - as a string composed like 'N(ON):N(OFF):N(CAL) - is mandatory' "
    print, " "
    print, " Usage: "
    print, " onoff, dutyc='ON:OFF:CAL' [,pin=] [,/help]"
    print, " where the optional parameter is: "
    print, "  pin = full path to data folder (the one containing the scans subfolders) "
    print, "  if pin is not provided, the path to the data is by default obtained as: "
    print, "  /archive/data/username/yyyymmdd "
    print, "  where username and yyyymmdd are extracted from the running system. "
    print, " When displaying spectral data, the X-axis is always given as bin number "
    print, " and the Y-axis is given in raw counts "
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
    ; and refershing plots every 10 seconds
    sp=path_sep()
    listdir = file_search(pin+sp+'2*', COUNT=num, /TEST_DIRECTORY, /FULLY_QUALIFY_PATH)
    p=listdir[-1]
    showdata, path=p, xtype=x, ytype=y, dutystr=dutyc
    wait,10
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


pro showdata, path=path, xtype=xtype, ytype=ytype, dutystr=dutystr

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

  ;folder/scan name
  pathstrings=strsplit(path,sp,/extract)
  scanname=pathstrings[-1]

  ; duty cycle handling
  phases=strsplit(dutystr,':',/extract)
  n_on=fix(phases[0])
  n_off=fix(phases[1])
  n_cal=fix(phases[2])
  fullc=n_on+n_off+n_cal


  ; finding the last readable FITS file in the folder
  if number lt (n_on+n_off) then begin
    print, 'No completed ON-OFF cycle in folder. Waiting...'
  endif else begin
    ; number of cycles present in folder
    cycles=fix(number/fullc)
    print, 'Plotting the (ON-OFF)/OFF integration within the present scan.'
  endelse

  mainh=mrdfits(list[0],0,head0,/silent)
  secth=mrdfits(list[0],1,head1,/silent)
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
  search_headkey, head1, '*Integration =*', keylab, integration, infolab, n_sectflag

  ; handling keywords: extracting meaningful, clean portions
  site=strcompress(antenna,/remove_all)
  schedlabs=strsplit(schedule,sp,/extract)
  cleanname=strsplit(schedlabs[-1],"'",/extract)
  schedname=cleanname[0]
  cleantarget=strsplit(target,"'",/extract)
  target=cleantarget[1]
  cleantype=strsplit(scantype,"'",/extract)
  scant=strcompress(cleantype[1],/remove_all)

  ; reading SECTION TABLE (binary data table n.1 of the Nuraghe/ESCS>0.3 FITS file)
  sectdata=mrdfits(list[0],1,/silent)
  bins=sectdata[0].bins

  ; reading data of first subscan
  data0=mrdfits(list[0],4,/silent)
  ndat=n_elements(data0.time)

  ; horizontal coordinates at mid-subscan for the first subscan
  elevation=data0[ndat/2].el/!dpi*180.0
  azimuth=data0[ndat/2].az/!dpi*180.0

  datatype='SPECTRA'
  streams=dblarr(n_sect*2,bins)
  rows=ceil((n_sect*2+1)/5.0)
  if n_sect*2 le 2 then !p.multi=[0,3,1] else !p.multi=[0,5,rows]
  ; labels for plots
  chtitle=["Sect0 L","Sect0 R","Sect1 L","Sect1 R","Sect2 L","Sect2 R","Sect3 L","Sect3 R","Sect4 L","Sect4 R", $
    "Sect5 L","Sect5 R", "Sect6 L","Sect6 R","Sect7 L","Sect7 R","Sect8 L","Sect8 R"]
  xaxis=indgen(bins)
  xlabel='Bin number'

  integ_on=dblarr(n_sect*2,bins)
  integ_off=dblarr(n_sect*2,bins)

  for c=0, cycles-1 do begin

    for i=0, fullc do begin

      ; reading RAW DATA TABLE (binary data table n.4 of the Nuraghe/ESCS0.3 FITS file)
      data=mrdfits(list[c*fullc+i-1],4,/SILENT)
      ndat=n_elements(data.time)

      bw=strarr(n_sect*2)
      frequency=dblarr(n_sect*2)
      freq=strarr(n_sect*2)

      for st=0, (n_sect*2)-1 do begin

        ; reading FEED TABLE (binary data table n.2 of the Nuraghe/ESCS0.4 FITS file)
        rftable=mrdfits(list[c*fullc+i-1],2,/SILENT)
        bandwidth=rftable[st].bandWidth
        bw[st]=strcompress(string(bandwidth, format='(D7.2)'),/remove_all)+' MHz'
        frequency[st]=double((rftable[st].frequency+bandwidth/2.0))/1000.
        freq[st]=strcompress(string(fix(frequency[st])),/remove_all)+' GHz'

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

        if i lt n_on then begin
          integ_on[st,*]=integ_on[st,*]+streams[st,*]
        endif
        if i ge n_on and i lt (n_on+n_off) then begin
          integ_off[st,*]=integ_off[st,*]+streams[st,*]
        endif

      endfor

    endfor

  endfor

  ; arrays for integration of spectra
  on_spectra=dblarr(n_sect*2,bins)
  off_spectra=dblarr(n_sect*2,bins)
  final_spectra=dblarr(n_sect*2,bins)

  ; initial box, with ancillary info
  plot, [0,1],[0,1], /NODATA, xstyle=4, ystyle=4, $
    xticks=[1], xminor=1, $
    yticks=[1], yminor=1
  xyouts, 0.1, 0.90, 'ON-OFF', $
    charsize=xxl, charthick=0.4
  xyouts, 0.1, 0.80, datatype, $
    charsize=xxl, charthick=0.4
  xyouts, 0.1, 0.70, 'Schedule = '+schedname, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.62, 'Scan = '+scanname, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.54, 'Target = '+target, $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.46, 'Completed ON-OFF cycles = '+strcompress(string(cycles, format='(I)')), $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.38, 'ON Total Integration = '+string(n_on*cycles*double(integration)/1000.0,format='(F7.2)')+' s', $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.26, 'Elevation = '+string(elevation,format='(D4.1)')+' deg', $
    charsize=small, charthick=0.4
  xyouts, 0.1, 0.18, 'Azimuth = '+string(azimuth,format='(D5.1)')+' deg', $
    charsize=small, charthick=0.4

  ; computing and plotting the (ON-OFF)/OFF raw spectra, section by section
  for st=0, (n_sect*2)-1 do begin

    on_spectra[st,*]=integ_on[st,*]/(n_on*cycles)
    off_spectra[st,*]=integ_off[st,*]/(n_off*cycles)
    final_spectra[st,*]=(on_spectra[st,*]-off_spectra[st,*])/off_spectra[st,*]

    top=max(final_spectra[st,*])
    bottom=min(final_spectra[st,*])
    step=(top-bottom)/10.0

    if n_sect eq 4 and st eq 4 then begin
      plot, [0,1],[0,1], /NODATA, xstyle=4, ystyle=4, $
        xticks=[1], xminor=1, $
        yticks=[1], yminor=1
    endif

    plot, xaxis,final_spectra[st,*], $
      title = chtitle[st], $
      xtitle = xlabel, $
      ytitle = ylabel, $
      xticks=ntick, $
      yrange=[bottom-1.5*step,top], $
      xs=1, psym=0,charsize=2
    xyouts, n_elements(xaxis)/10, bottom-0.5*step, /DATA,'Cen.Freq = '+string(frequency[st]*1000.0, format='(D12.6)')+' MHz', $
      charsize=small, charthick=0.4
    xyouts, n_elements(xaxis)/10, bottom-step, /DATA,'BW = '+bw[st], $
      charsize=small, charthick=0.4

  endfor

  !p.multi=[0,1,1]

  return
end
