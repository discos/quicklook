pro onoff, dutyc=dutyc, b0=b0, b1=b1, b2=b2, b3=b3, xtype=xtype, pin=pin, freewin=freewin, help=help

  ; The procedure iteratively shows the *RAW* (ON-OFF)/OFF scans right after they
  ; have been recorded.The explicit specification of the duty cicle
  ; - as a string composed like 'N(ON):N(OFF):N(CAL)' - is at the moment mandatory.
  ;
  ; Optionally, users can indicate the bin box they want to zoom to, section by section.
  ;
  ;  USAGE:
  ;
  ;   onoff, dutyc='ON:OFF:CAL' [,bN=[firstbin,lastbin]] [,xtype='vel'] [,pin='Path_to_main_folder'] [,/help]
  ;
  ;  where the optional parameters are:
  ;
  ;   pin = full path to main data folder (the one containing the scans subfolders)
  ;  If pin is not provided, the path to the data is by default obtained as:
  ;   /archive/data/username/yyyymmdd
  ;  where username and yyyymmdd are extracted from the running system.
  ;
  ;  Users can zoom on a selected range of *bins*, indicating it for the needed
  ;  sections (numbered from 0 to 3), such as b0=[500,1500]
  ;
  ;  The X-axis is by default given as frequency-bin number,
  ;  but velocity (w.r.t. observer) can be selected by using ytype='vel'.
  ;  The Y-axis is always given in raw units (pure number).
  ;
  ;  EXAMPLE:
  ;
  ;  onoff, dutyc='5:5:1', b0=[924,1124], b1=[500,1500], xtype='vel'
  ;
  ;
  ;  NOTICE: if using the /freewin option, you will be able to resize the plot
  ;  window as you like. Otherwise, it will almost fill up all the available screen.
  ;
  ;  It is assumed that FITS files have been acquired
  ;  with Nuraghe >0.3 or ESCS >0.3
  ;  Best of luck!
  ;
  ;  Author: S.Righini
  ;  Latest update: June 23rd 2016

  if (keyword_set(help) eq 1) then begin
    print, "The procedure iteratively shows the *RAW* (ON-OFF)/OFF scans right after they"
    print, "have been recorded.The explicit specification of the duty cicle"
    print, " - as a string composed like 'N(ON):N(OFF):N(CAL)' - is mandatory."
    print, " "
    print, "Optionally, users can indicate the bin box they want to zoom to, section by section."
    print, " "
    print, "USAGE: "
    print, " "
    print, "  onoff, dutyc='ON:OFF:CAL' [,bN=[firstbin,lastbin]] [,xtype='vel'] [,pin='Path_to_main_folder'] [,/help] "
    print, " "
    print, "where the optional parameters are: "
    print, " "
    print, "  pin = full path to main data folder (the one containing the scans subfolders) "
    print, "  If pin is not provided, the path to the data is by default obtained as: "
    print, "  /archive/data/username/yyyymmdd "
    print, "  where username and yyyymmdd are extracted from the running system."
    print, " "
    print, "Users can zoom on a selected range of *bins*, indicating it for the needed "
    print, "sections (numbered from 0 to 3), such as b0=[500,1500], b1=[900,1250], etc."
    print, " "
    print, "The X-axis is by default given as frequency-bin number, "
    print, "but velocity (w.r.t. observer) can be selected by using ytype='vel'. "
    print, "The Y-axis is always given in raw units (pure number). "
    print, " "
    print, "EXAMPLE: "
    print, " "
    print, "onoff, dutyc='5:5:1', b0=[924,1124], b1=[500,1500], xtype='vel'    "
    print, " "
    print, "NOTICE: if using the /freewin option, you will be able to resize the plot "
    print, "window as you like. Otherwise, it will almost fill up all the available screen. "
    print, " "
    print, "It is assumed that FITS files have been acquired with Nuraghe >0.3 or ESCS >0.3 "
    print, "Best of luck! "
  endif

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
  if not (keyword_set(dutyc))  then begin
    print, "Duty cycle MUST be explicitly given giving dutyc='N(ON):N(OFF):N(CAL)'"
    return
  endif
  if not (keyword_set(pin))  then pin  = defpath ; default folder for data storage
  if not (keyword_set(xtype))  then xtype  = 'bin'    ; x-axis = frequency bins
  if not (keyword_set(b0)) then b0=[10,10]  ; dummy value to later identify "full range"
  if not (keyword_set(b1)) then b1=[10,10]  ; dummy value to later identify "full range"
  if not (keyword_set(b2)) then b2=[10,10]  ; dummy value to later identify "full range"
  if not (keyword_set(b3)) then b3=[10,10]  ; dummy value to later identify "full range"
  if not (keyword_set(freewin))  then rescale = 'RescaleNo' else rescale = 'RescaleYes' ; can users rescale the plot window?


  ; setting the plot device depending on the operating system
  if path_sep() eq '/' then begin
    set_plot, 'x'    ; this is a Linux/OSX machine
  endif else begin
    set_plot, 'win'  ; this is a Windows machine
  endelse

  ; setting the plot configuration
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

  ; custom numbers for charsizes
  xxl=3.50
  xl=2.3
  large=1.70
  medium=1.55
  small=1.35

  ; iteration: the quick-look must go on ad libitum
  for i=0,15000 do begin
    ; selecting the second-last folder (where the latest complete scan lies)
    ; and refershing plots every 10 seconds
    sp=path_sep()
    listdir = file_search(pin+sp+'2*', COUNT=num, /TEST_DIRECTORY, /FULLY_QUALIFY_PATH)
    if num lt 2 then begin
      print, 'Waiting for a complete ON-OFF scan...'
      plot, [0,100],[0,100], /NODATA, /DEVICE, xstyle=4, ystyle=4, $
        xticks=[1], xminor=1, $
        yticks=[1], yminor=1
      xyouts, 100,400,'Waiting for a complete ON-OFF scan...',charsize=large*3, charthick=2.0, /DEVICE
    endif else begin
      p=listdir[-2]
      showdata, p, xtype, b0, b1, b2, b3, dutyc
    endelse
    wait,10
  endfor
end



; procedure to read keyword values from headers

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


; procedure that actually reads files and makes computations and plots

pro showdata, path, xt, box0, box1, box2, box3, dutystr

  ; path separator
  sp=path_sep()

  ; custom numbers for charsizes
  xxl=3.50
  xl=2.3
  large=1.70
  medium=1.55
  small=1.35

  ; Listing the data FITS files written in the folder.
  ; By searching only for files named '2*.fits' the 'summary.fits' one is avoided
  list=file_search(path+sp+'2*.fits',COUNT=number,/FULLY_QUALIFY_PATH)
  ; now I need the summary.fits file...
  summaryfits=file_search(path+sp+'summary.fits',COUNT=summarynum,/FULLY_QUALIFY_PATH)

  ; The following checks should be redundant, as the selected folder should always
  ; contain a completed scan.
  ; However, it is better to leave them, as users might have interrupted a scan, producing
  ; an incomplete acquisition or even empty folders.
  if number eq 0 then begin
    print, 'No FITS files in folder. Waiting...'
    ;Empty window, printing info inside window
    plot, [0,100],[0,100], /NODATA, /DEVICE, xstyle=4, ystyle=4, $
      xticks=[1], xminor=1, $
      yticks=[1], yminor=1
    xyouts, 100,400,'No FITS files in folder. Waiting...',charsize=large*3, charthick=2.0, /DEVICE
    return
  endif
  if summarynum eq 0 then begin
    print, 'No Summary file in folder. Waiting...'
    ;Empty window, printing info inside window
    plot, [0,100],[0,100], /NODATA, /DEVICE, xstyle=4, ystyle=4, $
      xticks=[1], xminor=1, $
      yticks=[1], yminor=1
    xyouts, 100,400,'No summary.fits file in folder. Waiting...',charsize=large*3, charthick=2.0, /DEVICE
    return
  endif

  ;Everything's fine. So I extract the folder/scan name
  pathstrings=strsplit(path,sp,/extract)
  scanname=pathstrings[-1]

  ; Extracting duty cycle details from input string
  phases=strsplit(dutystr,':',/extract)
  n_on=fix(phases[0])
  n_off=fix(phases[1])
  n_cal=fix(phases[2])
  fullc=n_on+n_off+n_cal
  count_on_spectra=0
  datatype='SPECTRA'

  ; Has at least one cycle been completed?
  if number lt (n_on+n_off) then begin
    print, 'No completed ON-OFF cycle in folder. Waiting...'
  endif else begin
    ; number of cycles present in folder
    cycles=fix(number/fullc)
    print, 'Plotting the (ON-OFF)/OFF integration of the latest completed scan '
  endelse

  ; reading headers
  mainh=mrdfits(list[0],0,head0,/silent)
  secth=mrdfits(list[0],1,head1,/silent)
  summaryhead=mrdfits(summaryfits[0],0,sumhead,/silent)

  ; reading keywords from headers
  tarflag=0
  subflag=0
  scanflag=0
  dateflag=0
  siteflag=0
  schedflag=0
  n_sectflag=0
  integrationflag=0
  restfreqflag=0
  search_headkey, head0, '*SOURCE*', keylab, target, infolab, tarflag
  search_headkey, head0, '*SubScanID*', keylab, subscan, infolab, subflag
  search_headkey, head0, '*SCANID*', keylab, scan, infolab, scanflag
  search_headkey, head0, '*DATE*', keylab, date, infolab, dateflag
  search_headkey, head0, '*ANTENNA*', keylab, antenna, infolab, siteflag
  search_headkey, head0, '*SubScanType =*', keylab, scantype, infolab, typeflag
  search_headkey, head0, '*ScheduleName =*', keylab, schedule, infolab, schedflag
  search_headkey, head0, '*SECTIONS=*', keylab, n_sect, infolab, n_sectflag
  search_headkey, head1, '*Integration =*', keylab, integration, infolab, integrationflag
  search_headkey, sumhead, '*RESTFREQ1 =*', keylab, restfreq, infolab, restfreqflag

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

  ; redefining boundaries in order to plot all the bins
  default=[10,10]
  if box0[0] eq default[0] and box0[1] eq default[1]  then box0=[0,bins-1]
  if box1[0] eq default[0] and box1[1] eq default[1]  then box1=[0,bins-1]
  if box2[0] eq default[0] and box2[1] eq default[1]  then box2=[0,bins-1]
  if box3[0] eq default[0] and box3[1] eq default[1]  then box3=[0,bins-1]

  ; reading data of first subscan
  data0=mrdfits(list[0],4,/silent)
  ndat=n_elements(data0.time)

  ; horizontal coordinates at mid-subscan for the first subscan
  elevation=data0[ndat/2].el/!dpi*180.0
  azimuth=data0[ndat/2].az/!dpi*180.0

  ; array for spectra storage
  streams=dblarr(n_sect*2,bins)
  integ_on=dblarr(n_sect*2,bins)
  integ_off=dblarr(n_sect*2,bins)

  ;how many rows in multi-plot?
  rows=ceil((n_sect*2+1)/5.0)
  if n_sect*2 le 2 then !p.multi=[0,3,1] else !p.multi=[0,5,rows]
  ; labels for plots
  chtitle=["Sect0 L","Sect0 R","Sect1 L","Sect1 R","Sect2 L","Sect2 R","Sect3 L","Sect3 R","Sect4 L","Sect4 R", $
    "Sect5 L","Sect5 R", "Sect6 L","Sect6 R","Sect7 L","Sect7 R","Sect8 L","Sect8 R"]

  ; cycling on... cycles. A scan my contain more than one.
  for c=0, cycles-1 do begin

    ; within a cycle
    for i=0, fullc-1 do begin

      ; reading RAW DATA TABLE (binary data table n.4 of the Nuraghe/ESCS0.3 FITS file)
      data=mrdfits(list[c*fullc+i],4,/SILENT)
      ndat=n_elements(data.time)

      ; section-dependant parameters and data: storage arrays
      bw=strarr(n_sect*2)
      frequency=dblarr(n_sect*2)
      freq=strarr(n_sect*2)
      xaxis=dblarr(n_sect*2,bins)
      xlabel=strarr(n_sect*2)

      ; section-by-section operations
      for st=0, (n_sect*2)-1 do begin

        ; reading FEED TABLE (binary data table n.2 of the Nuraghe/ESCS0.4 FITS file)
        rftable=mrdfits(list[c*fullc+i],2,/SILENT)
        bandwidth=rftable[st].bandWidth   ; MHz
        bw[st]=strcompress(string(bandwidth, format='(D7.2)'),/remove_all)+' MHz' ; string
        frequency[st]=double((rftable[st].frequency+bandwidth/2.0))/1000.    ; GHz
        freq[st]=strcompress(string(fix(frequency[st])),/remove_all)+' GHz'  ; string
        initfreq=rftable[st].frequency       ; MHz
        freqstep=rftable[st].bandWidth/double(bins)  ; MHz/bin

        ; various types of x-axis data
        binaxis=indgen(bins)                                   ; x-axis in bins
        freqaxis=double(initfreq)+double(binaxis)*freqstep     ; x-axis in frequency
        rvelaxis= -((freqaxis-restfreq)/restfreq)*299792.458d  ; x-axis in radial velocity wrt observer

        n_spectra=n_elements(data.time) ; how many spectra inside the FITS file?

        ; assigning the x-axis data according to the user's selection
        case strlowcase(xt) of
          'bin': begin
            xaxis[st,*]=binaxis
            xlabel[st]='Frequency bin number'
          end
          'vel': begin
            xaxis[st,*]=rvelaxis
            xlabel[st]='Radial velocity wrt observer (km/s)'
          end
          'freq': begin
            xaxis[st,*]=freqaxis
            xlabel[st]='Frequency (MHz)'
          end
          else: begin
            xaxis[st,*]=binaxis
            xlabel[st]='Frequency bin number'
          end
        endcase

        ylabel='(ON-OFF)/OFF'  ; this is constant for all cases


        ; accumulation of spectra for this file and section
        for spec=0, n_spectra-1 do begin
          case st of
            0: streams(st,*)=streams(st,*)+data[spec].ch0[0:bins-1]
            1: streams(st,*)=streams(st,*)+data[spec].ch0[bins:2*bins-1]
            2: streams(st,*)=streams(st,*)+data[spec].ch1[0:bins-1]
            3: streams(st,*)=streams(st,*)+data[spec].ch1[bins:2*bins-1]
            4: streams(st,*)=streams(st,*)+data[spec].ch2[0:bins-1]
            5: streams(st,*)=streams(st,*)+data[spec].ch2[bins:2*bins-1]
            6: streams(st,*)=streams(st,*)+data[spec].ch3[0:bins-1]
            7: streams(st,*)=streams(st,*)+data[spec].ch3[bins:2*bins-1]
            8: streams(st,*)=streams(st,*)+data[spec].ch4[0:bins-1]
            9: streams(st,*)=streams(st,*)+data[spec].ch4[bins:2*bins-1]
            10: streams(st,*)=streams(st,*)+data[spec].ch5[0:bins-1]
            11: streams(st,*)=streams(st,*)+data[spec].ch5[bins:2*bins-1]
            12: streams(st,*)=streams(st,*)+data[spec].ch6[0:bins-1]
            13: streams(st,*)=streams(st,*)+data[spec].ch6[bins:2*bins-1]
            14: streams(st,*)=streams(st,*)+data[spec].ch7[0:bins-1]
            15: streams(st,*)=streams(st,*)+data[spec].ch7[bins:2*bins-1]
            16: streams(st,*)=streams(st,*)+data[spec].ch8[0:bins-1]
            17: streams(st,*)=streams(st,*)+data[spec].ch8[bins:2*bins-1]
          endcase
        endfor

        ; computing the average spectrum
        streams(st,*)=streams(st,*)/n_spectra

        ; is it an ON subscan, or an OFF subscan? Knowing the duty cycle, we can tell.
        if i lt n_on then begin
          integ_on[st,*]=integ_on[st,*]+streams[st,*]
          count_on_spectra=count_on_spectra+n_spectra
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
    charsize=medium, charthick=0.4
  xyouts, 0.1, 0.46, 'Completed ON-OFF cycles = '+strcompress(string(cycles, format='(I)')), $
    charsize=medium, charthick=0.4
  xyouts, 0.1, 0.38, 'ON Total Integration = '+string(count_on_spectra/(n_sect*2)*double(integration)/1000.0,format='(F7.2)')+' s', $
    charsize=medium, charthick=0.4
  xyouts, 0.1, 0.26, 'Elevation = '+string(elevation,format='(D4.1)')+' deg', $
    charsize=medium, charthick=0.4
  xyouts, 0.1, 0.18, 'Azimuth = '+string(azimuth,format='(D5.1)')+' deg', $
    charsize=medium, charthick=0.4

  ; computing and plotting the integrated (ON-OFF)/OFF raw spectra, section by section
  for st=0, (n_sect*2)-1 do begin

    on_spectra[st,*]=integ_on[st,*]/(n_on*cycles)
    off_spectra[st,*]=integ_off[st,*]/(n_off*cycles)
    final_spectra[st,*]=(on_spectra[st,*]-off_spectra[st,*])/off_spectra[st,*]

    ; setting a few parameters for a correct display of data and labels
    top=max(final_spectra[st,*])
    bottom=min(final_spectra[st,*])
    step=(top-bottom)/10.0

    if n_sect eq 4 and st eq 4 then begin
      plot, [0,1],[0,1], /NODATA, xstyle=4, ystyle=4, $
        xticks=[1], xminor=1, $
        yticks=[1], yminor=1
    endif

    ; assigning the user-defined display bin range (if any)
    case st of

      0: begin
        first=box0[0]
        last=box0[1]
      end

      1: begin
        first=box0[0]
        last=box0[1]
      end

      2: begin
        first=box1[0]
        last=box1[1]
      end

      3: begin
        first=box1[0]
        last=box1[1]
      end

      4: begin
        first=box2[0]
        last=box2[1]
      end

      5: begin
        first=box2[0]
        last=box2[1]
      end

      6: begin
        first=box3[0]
        last=box3[1]
      end

      7: begin
        first=box3[0]
        last=box3[1]
      end

      else: begin
        first=0
        last=bins-1
      end

    endcase

    ; x-position for xyouts labels
    xpos=min([xaxis[st,first],xaxis[st,last]])+abs(xaxis[st,last]-xaxis[st,first])/10.0

    ; finally, the quick-look of this section. Hooray!
    plot, xaxis[st,first:last],final_spectra[st,first:last], $
      title = chtitle[st], $
      xtitle = xlabel[st], $
      ytitle = ylabel, $
      xticks=ntick, $
      xs=1, $
      yrange=[bottom-1.5*step,top], $
      charsize=xl
    xyouts, xpos, bottom-0.7*step, /DATA,'Cen.SkyFreq. = '+string(frequency[st]*1000.0, format='(D12.6)')+' MHz', $
      charsize=small, charthick=0.4
    xyouts, xpos, bottom-1.5*step, /DATA,'BW = '+bw[st], $
      charsize=small, charthick=0.4

  endfor

  !p.multi=[0,1,1]

  return
end
