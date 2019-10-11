   10  rem bbuser=current bbs user; bbpass=current bbs user pass
   20  rem userno=current bbs user (rec) number
   30  rem lastdate$,lasttime$=current bbs user last login date/time
   40  rem userec$=packed user record for pack/unpack subs
   50  rem lastmsg=last msg that bbs had when user quit
   60  rem himsg=current last msg in message file
   70  rem recnum=pointer to record in file for subs
   80  files$="mybbsusers.dat"
   90  print "Welcome to Jadawin's place"
  100  print
  110  print "Username: ";
  120  usr$=user$:print usr$:REM input usr$:rem DEBUG ONLY
  130  pass$="":print "Password: ";
  140  a$=inkey$: if a$=chr$(13) then 180
  150  if a$=chr$(127) or a$=chr$(8) then pass$=left$(pass$,abs(len(pass$)-1)):print chr$(8)" "chr$(8);:goto 140
  160  pass$=pass$+a$: print "*";
  170  goto 140
  180  print:if len(pass$)<1 then 130
  190  open files$, as #1
  200  userno=0
  210  if eof(1)<0 then 380
  220  userno=userno+1
  230  input# 1,userec$
  240  gosub 1600
  250  if bbuser$<>usr$ then 210
  260  if bbpass$<>pass$ then 2260
  270  sleep .5
  280  print:print "Welcome ";bbuser$
  290  print:print "Your last logon was on ";lastdate$;" at ";lasttime$
  300  recnum=userno
  310  gosub 1890
  320  lastdate$=date$
  330  lasttime$=time$
  340  gosub 1680: rem pack up userec$
  350  print# 1,userec$
  360  close #1
  370  goto 510
  380  Print "New User ";usr$;
  390  bbuser$=usr$
  400  bbpass$=pass$
  410  lastdate$=date$
  420  lasttime$=time$
  430  lastmsg=0
  440  userno=userno+1
  450  gosub 1680
  460  print# 1,userec$
  470  close #1
  480  sleep 1
  490  print " created."
  500  rem
  510  rem BBS main
  520  open "mybbslog.dat", as #1
  530  gosub 1990: rem seek to eof
  540  rem log use
  550  print# 1,date$ +"," +time$ + ","+usr$
  560  close #1
  570  files$="mybbsmsgs.dat"
  580  gosub 2050: rem find eof recnum
  590  himsg=recnum
  600  sleep .5:print
  610  print "There are ";himsg-lastmsg;" new messages out of ";himsg;" total."
  620  print
  630  print "Command: M(essages, Q(uit: ";
  640  c$=inkey$
  650  c$=ups$(c$)
  660  if c$="Q" then print "Quit":goto 2140
  670  if c$="M" then print "Messages":gosub 690
  680  goto 620
  690  rem messages
  700  open files$, as #1
  710  print
  720  print "Messages: N(ew, O(ld, P(ost, E(xit: ";
  730  c$=inkey$
  740  c$=ups$(c$)
  750  if c$="E" then print "Exit":close #1:return
  760  if c$="N" then print "New":gosub 800
  770  if c$="O" then print "Old":gosub 1090
  780  if c$="P" then print "Post":gosub 1170
  790  goto 710
  800  print:print "New Messages"
  810  if himsg-lastmsg>0 then 840
  820  print:print "No new messages"
  830  goto 1080
  840  recnum=lastmsg+1
  850  gosub 1890
  860  if eof(1)<0 then print "End of messages":goto 1080
  870  input# 1,msgrec$
  880  recnum=recnum+1
  890  sleep .5
  900  gosub 1740: rem unpack message
  910  print:print msgdate$;" ";msgtime$
  920  print msgsubj$
  930  print "Message: R(ead, N(ext, E(xit: ";
  940  c$=inkey$
  950  c$=ups$(c$)
  960  if c$="N" then print "Next":goto 860
  970  if c$="E" then print "Exit":goto 1080
  980  if c$<>"R" then print:goto 930
  990  print "Read"
 1000  print msgbody$
 1010  print:print "Message: R(eply, N(ext, E(xit: ";
 1020  c$=inkey$
 1030  c$=ups$(c$)
 1040  if c$="E" then print "Exit":goto 1080
 1050  if c$="R" then print "Reply":gosub 1200
 1060  print "Next"
 1070  goto 860
 1080  return
 1090  rem read old messages
 1100  print:print "Old Messages"
 1110  if himsg>0 then 1140
 1120  print:print "No old messages"
 1130  goto 1160
 1140  gosub 1840
 1150  goto 860: rem read loop
 1160  return
 1170  rem post
 1180  msgbody$=""
 1190  msgsubj$=""
 1200  rem reply
 1210  currecnum=recnum
 1220  msgdate$=date$
 1230  msgtime$=time$
 1240  if len(msgsubj$)<1 then 1270
 1250  msgsubj$="From: "+bbuser$+" Subj: Re:"+right$(msgsubj$,len(msgsubj$)-instr(msgsubj$,"j: ")-3)
 1260  goto 1290
 1270  input "Subject; ",a$
 1280  msgsubj$="From: "+bbuser$+" Subj: "+a$
 1290  print "Message, terminate with <CR>"
 1300  input ": ",a$
 1310  print:print "Post: K(eep, D(iscard: ";
 1320  c$=inkey$
 1330  c$=ups$(c$)
 1340  if c$="D" then print "Discard":goto 1450
 1350  if c$<>"K" then 1310
 1360  print "Keep"
 1370  msgbody$=a$: rem this could be better
 1380  gosub 1810: rem pack msgrec$
 1390  gosub 1990
 1400  print# 1,msgrec$
 1410  himsg=himsg+1
 1420  sleep 0.5:print "Posted"
 1430  recnum=currecnum+1 : rem set file pointer to last message read +1
 1440  gosub 1890
 1450  return
 1460  rem uppercase a string
 1470  for i=1 to len(string$)
 1480  mid$(string$,i,1)=chr$(val(mid$(string$,i,1)) mod 64+32)
 1490  return
 1500  rem convert datetime$ to a number
 1510  timenum=val(mid$(datetime$,10,2))*60*60+val(mid$(datetime$,13,2))*60+val(mid$(datetime$,16,2))
 1520  restore:daynum=0
 1530  read m$,m
 1540  daynum=daynum+m
 1550  if m$<>mid$(datetime$,4,3) then 1530
 1560  daynum=daynum+val(left$(datetime$,2))-m
 1570  datetime=val(mid$(datetime$,8,2))*100000000+daynum*100000+timenum
 1580  return:rem datetime=YYjjj.ssssss
 1590  data "Jan",31,"Feb",28,"Mar",31,"Apr",30,"May",31,"Jun",30,"Jul",31,"Aug",31,"Sep",30,"Oct",31,"Nov",30,"Dec",31
 1600  rem unpack user record from userec$
 1610  bbuser$=left$(userec$,instr(userec$,"|",0))
 1620  bbpass$=mid$(userec$,len(bbuser$)+2,instr(userec$,"|",len(bbuser$)+2)-len(bbuser$)-1)
 1630  l=len(bbuser$+bbpass$)
 1640  lastdate$=mid$(userec$,l+3,9)
 1650  lasttime$=mid$(userec$,l+12,8)
 1660  lastmsg=val(right$(userec$,6))
 1670  return
 1680  rem pack user record into userec$
 1690  userec$=bbuser$+"|"+bbpass$+"|"+lastdate$+lasttime$
 1700  lastmsg$=str$(lastmsg)
 1710  lastmsg$=spc$(6-len(lastmsg$))+lastmsg$
 1720  userec$=userec$+lastmsg$
 1730  return
 1740  rem unpack a message record from msgrec
 1750  msgdate$=left$(msgrec$,9)
 1760  msgtime$=mid$(msgrec$,10,8)
 1770  msgsubj$=mid$(msgrec$,18,instr(msgrec$,"|")-17)
 1780  msgbody$=right$(msgrec$,len(msgrec$)-(instr(msgrec$,"|")+1))
 1790  return
 1800  return
 1810  rem pack msgrec$
 1820  msgrec$=msgdate$+msgtime$+msgsubj$+"|"+msgbody$
 1830  rem FILES$ contains file name, number is always 1
 1840  rem Restore file pointer to BOF for file #1
 1850  close #1
 1860  open files$, as #1
 1870  recnum=0
 1880  return
 1890  rem Position file point at record recnum for file #1
 1900  r=recnum
 1910  gosub 1840
 1920  recnum=r
 1930  if eof(1)<0 then return
 1940  if r<2 then 1980
 1950  input# 1,a$
 1960  r=r-1
 1970  goto 1930
 1980  return
 1990  rem append to file #1
 2000  rem seek to EOF file #1
 2010  if eof(1)<0 THEN 2040
 2020  input# 1,a$
 2030  goto 2000
 2040  return
 2050  rem count the records in files$
 2060  recnum=0
 2070  open files$, as #2
 2080  if eof(2)<0 then 2120
 2090  input# 2,r$
 2100  recnum=recnum+1
 2110  goto 2080
 2120  close #2
 2130  return
 2140  rem update user record as user quits
 2150  print:print "Goodbye ";bbuser$;" ";
 2160  files$="mybbsusers.dat"
 2170  open files$, as #1
 2180  recnum=userno
 2190  gosub 1890
 2200  lastmsg=himsg
 2210  gosub 1680
 2220  print# 1,userec$
 2230  sleep 1
 2240  print "updated.":print
 2250  close #1:goto 2270
 2260  print "Illegal access"
 2270  end
