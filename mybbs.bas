    1  rem bbuser=current bbs user; bbpass=current bbs user pass
    2  rem userno=current bbs user (rec) number
    3  rem lastdate$,lasttime$=current bbs user last login date/time
    4  rem userec$=packed user record for pack/unpack subs
    5  rem lastmsg=last msg that bbs had when user quit
    6  rem himsg=current last msg in message file
    7  rem recnum=pointer to record in file for subs
   10  files$="mybbsusers.dat"
   30  print "Welcome to Jadawin's place"
   40  print
   50  print "Username: ";
   60  input usr$:rem DEBUG ONLY
   70  input "Password: ",pass$
   80  open files$, as #1
   85  userno=0
   90  if eof(1)<0 then 150
   91  userno=userno+1
  100  input# 1,userec$
  105  gosub 8200
  110  if bbuser$<>usr$ then 90
  120  if bbpass$<>pass$ then 9990
  125  sleep .5
  130  print:print "Welcome ";bbuser$
  131  print:print "Your last logon was on ";lastdate$;" at ";lasttime$
  132  recnum=userno
  135  gosub 9100
  136  lastdate$=date$
  137  lasttime$=time$
  138  gosub 8300: rem pack up userec$
  139  print# 1,userec$
  140  close #1
  145  goto 200
  150  Print "New User ";usr$;
  151  bbuser$=usr$
  152  bbpass$=pass$
  153  lastdate$=date$
  154  lasttime$=time$
  155  lastmsg=0
  156  userno=userno+1
  160  gosub 8300
  170  print# 1,userec$
  180  close #1
  185  sleep 1
  186  print " created."
  190  rem
  200  rem BBS main
  201  open "mybbslog.dat", as #1
  202  gosub 9200: rem seek to eof
  203  rem log use
  205  print# 1,date$ +"," +time$ + ","+usr$
  206  close #1
  207  files$="mybbsmsgs.dat"
  208  gosub 9300: rem find eof recnum
  209  himsg=recnum
  210  sleep .5:print
  211  print "There are ";himsg-lastmsg;" new messages out of ";himsg;" total."
  212  print
  220  print "Command: M(essages, Q(uit: ";
  230  c$=inkey$
  240  c$=chr$(asc(c$) mod 32 + 64)
  250  if c$="Q" then print "Quit":goto 9800
  260  if c$="M" then print "Messages":gosub 300
  270  goto 212
  300  rem messages
  305  open files$, as #1
  310  print
  320  print "Messages: N(ew, O(ld, P(ost, E(xit: ";
  330  c$=inkey$
  340  c$=chr$(asc(c$) mod 32 + 64)
  345  if c$="E" then print "Exit":goto 212
  350  if c$="N" then print "New":gosub 5000
  360  if c$="O" then print "Old":gosub 5200
  370  if c$="P" then print "Post":gosub 5400
  390  goto 310
 5000  print:print "New Messages"
 5010  if himsg-lastmsg>0 then 5050
 5020  print:print "No new messages"
 5030  goto 5190
 5050  recnum=lastmsg+1
 5060  gosub 9100
 5065  if eof(1)<0 then print "End of messages":goto 5190
 5070  input# 1,msgrec$
 5071  recnum=recnum+1
 5075  sleep .5
 5080  gosub 8400: rem unpack message
 5090  print:print msgdate$;" ";msgtime$
 5100  print msgsubj$
 5110  print "Message: R(ead, N(ext, E(xit: ";
 5120  c$=inkey$
 5130  c$=chr$(asc(c$) mod 32 + 64)
 5140  if c$="N" then print "Next":goto 5065
 5141  if c$="E" then print "Exit":goto 5190
 5150  if c$<>"R" then print:goto 5110
 5155  print "Read"
 5160  print msgbody$
 5161  print:print "Message: R(eply, N(ext, E(xit: ";
 5162  c$=inkey$
 5163  c$=chr$(asc(c$) mod 32 + 64)
 5164  if c$="E" then print "Exit":goto 5190
 5165  if c$="R" then print "Reply":gosub 5405
 5166  print "Next"
 5170  goto 5065
 5190  return
 5200  rem read old messages
 5210  print:print "Old Messages"
 5220  if himsg>0 then 5250
 5230  print:print "No old messages"
 5240  goto 5390
 5250  gosub 9000
 5260  goto 5065: rem read loop
 5390  return
 5400  rem post
 5403  msgbody$=""
 5404  msgsubj$=""
 5405  rem reply
 5406  currecnum=recnum
 5408  msgdate$=date$
 5410  msgtime$=time$
 5420  if len(msgsubj$)<1 then 5490
 5430  msgsubj$="From: "+bbuser$+" Subj: Re:"+right$(msgsubj$,len(msgsubj$)-instr(msgsubj$,"j: ")-3)
 5440  goto 5520
 5490  input "Subject; ",a$
 5500  msgsubj$="From: "+bbuser$+" Subj: "+a$
 5520  print "Message, terminate with <CR>"
 5530  input ": ",a$
 5531  print:print "Post: K(eep, D(iscard: ";
 5532  c$=inkey$
 5533  c$=chr$(asc(c$) mod 32 + 64)
 5534  if c$="D" then print "Discard":goto 5590
 5535  if c$<>"K" then 5531
 5536  print "Keep"
 5540  msgbody$=a$: rem this could be better
 5550  gosub 8500: rem pack msgrec$
 5560  gosub 9200
 5570  print# 1,msgrec$
 5580  himsg=himsg+1
 5585  sleep 0.5:print "Posted"
 5586  recnum=currecnum+1 : rem set file pointer to last message read +1
 5587  gosub 9100
 5590  return
 8000  rem uppercase a string
 8010  for i=1 to len(string$)
 8020  mid$(string$,i,1)=chr$(val(mid$(string$,i,1)) mod 64+32)
 8030  return
 8100  rem convert datetime$ to a number
 8110  timenum=val(mid$(datetime$,10,2))*60*60+val(mid$(datetime$,13,2))*60+val(mid$(datetime$,16,2))
 8120  restore:daynum=0
 8130  read m$,m
 8140  daynum=daynum+m
 8150  if m$<>mid$(datetime$,4,3) then 8130
 8160  daynum=daynum+val(left$(datetime$,2))-m
 8170  datetime=val(mid$(datetime$,8,2))*100000000+daynum*100000+timenum
 8180  return:rem datetime=YYjjj.ssssss
 8190  data "Jan",31,"Feb",28,"Mar",31,"Apr",30,"May",31,"Jun",30,"Jul",31,"Aug",31,"Sep",30,"Oct",31,"Nov",30,"Dec",31
 8200  rem unpack user record from userec$
 8210  bbuser$=left$(userec$,instr(userec$,"|",0))
 8215  bbpass$=mid$(userec$,len(bbuser$)+2,instr(userec$,"|",len(bbuser$)+2)-len(bbuser$)-1)
 8216  l=len(bbuser$+bbpass$)
 8220  lastdate$=mid$(userec$,l+3,9)
 8230  lasttime$=mid$(userec$,l+12,8)
 8240  lastmsg=val(right$(userec$,6))
 8250  return
 8300  rem pack user record into userec$
 8310  userec$=bbuser$+"|"+bbpass$+"|"+lastdate$+lasttime$
 8320  lastmsg$=str$(lastmsg)
 8330  lastmsg$=spc$(6-len(lastmsg$))+lastmsg$
 8340  userec$=userec$+lastmsg$
 8350  return
 8400  rem unpack a message record from msgrec
 8410  msgdate$=left$(msgrec$,9)
 8420  msgtime$=mid$(msgrec$,10,8)
 8430  msgsubj$=mid$(msgrec$,18,instr(msgrec$,"|")-17)
 8440  msgbody$=right$(msgrec$,len(msgrec$)-(instr(msgrec$,"|")+1))
 8450  return
 8490  return
 8500  rem pack msgrec$
 8510  msgrec$=msgdate$+msgtime$+msgsubj$+"|"+msgbody$
 8999  rem FILES$ contains file name, number is always 1
 9000  rem Restore file pointer to BOF for file #1
 9010  close #1
 9020  open files$, as #1
 9025  recnum=0
 9030  return
 9100  rem Position file point at record recnum for file #1
 9110  r=recnum
 9120  gosub 9000
 9121  recnum=r
 9125  if eof(1)<0 then return
 9130  if r<2 then 9150
 9135  input# 1,a$
 9140  r=r-1
 9145  goto 9125
 9150  return
 9200  rem append to file #1
 9210  rem seek to EOF file #1
 9215  if eof(1)<0 THEN 9240
 9220  input# 1,a$
 9230  goto 9210
 9240  return
 9300  rem count the records in files$
 9310  recnum=0
 9320  open files$, as #2
 9330  if eof(2)<0 then 9370
 9340  input# 2,r$
 9350  recnum=recnum+1
 9360  goto 9330
 9370  close #2
 9380  return
 9800  rem update user record as user quits
 9810  print:print "Goodbye ";bbuser$;" ";
 9820  files$="mybbsusers.dat"
 9830  open files$, as #1
 9840  recnum=userno
 9850  gosub 9100
 9860  lastmsg=himsg
 9870  gosub 8300
 9880  print# 1,userec$
 9883  sleep 1
 9885  print "updated.":print
 9890  close #1:goto 9999
 9990  print "Illegal access"
 9999  end
