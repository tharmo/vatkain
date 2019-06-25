unit ngrams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type tsimmat=class(tobject)
  rows,cols:word;
  vals,vars:array of word;

  kertotaulu:array of word;
  slist:tstringlist;
  procedure list(sl:tstringlist);
  function haegramlist(luku:word;var sanums,sapainot:tlist;sl:tstringlist;res,respainot:tlist):word;
  function kerro(luku:word;var sanums,safreks:tlist;sl:tstringlist;res,resfreks:tlist):tlist;
  constructor create(r,c:word;fn:string;sl:tstringlist);
end;
type tsimisanat=class(tobject)
  matx:tsimmat;
  //simmat: arra
  infile,outfile:file;
  listfname:string;
  slist:tstringlist;
  constructor create(c:word;fn,sfn:string);
end;
procedure turkuana;
implementation
 uses riimiutils,math,syno;
 type  thava=class(tobject)
   ocs:array[0..17] of word;
   constructor create;
 end;
constructor thava.create;
begin
    fillchar(ocs,sizeof(ocs),0);
end;
procedure turkuana;
var inf:textfile;spari,vormu,luokat,lnames,sijat,havas:tstringlist;rivi,keissi:ansistring;
    vocs:array of integer;

  procedure kirjaa(verbi,nomform,nom:string);
  var vnum,nfnum:integer;hava:thava;
  begin
   vnum:=havas.indexof(verbi);
   nfnum:=sijat.indexof(nomform);
   if nfnum<1 then  writeln('??????????',vnum,verbi,nfnum,nomform);
   try
   if vnum<0 then
   begin
    //writeln('new:',verbi,' ',nomform);
    hava:=thava.create;
    havas.addobject(verbi,hava);
    vnum:=havas.indexof(verbi);
   end else hava:=thava(havas.objects[vnum]);
   inc(hava.ocs[nfnum]);
   inc(hava.ocs[0]);
   //if verbi='astua' then if nomform='Aux' then writeln(verbi,' ',nom);
   except writeln('?!!!!',vnum,verbi,nfnum,nomform);end;

  end;
  function regp(lka,frm:string):tstringlist;
  var i,lnum:integer;
  begin
   lnum:=luokat.indexof(lka);
   if lnum<0 then begin result:=tstringlist.create;result.duplicates:=dupignore;RESULT.SORTED:=TRUE;luokat.addobject(lka,result);exit; end
   else result:=tstringlist(luokat.objects[lnum]);
   result.add(frm);
  end;
  function dowrd(lk,frm:string):string;
  var vf:string;
      procedure reg(st,x:string);
      begin result:=st;keissi:=frm; end;
  begin
       result:='';
       vormu.delimitedtext:=frm;
       try
       if (lk='NOUN') or (lk='PROPN') then reg('Nou',vormu.values['Case']) else
       if lk='ADJ' then reg('Adj',vormu.values['Case']) else
       if lk='PRON' then reg('Pro',vormu.values['Case']) else
       if lk='NUM' then reg('Num',vormu.values['Case']) else
       if (lk='VERB') or (lk='AUX') then
       begin
         vf:=vormu.values['VerbForm'];
         if vf='Fin' then reg('V'+ifs(vormu.values['Negative']='','fi','ei'),vormu.values['Mood'])
         else if vf='Inf' then reg('VI'+vormu.values['InfForm'],vormu.values['Case'])
         else if vf='Part' then reg('V'+vormu.values['PartForm'],vormu.values['Case'])      //past.pres,agt,neg (1kpl)
         else if vf='Past' then reg('V'+vormu.values['PastForm'],vormu.values['Case']);

       end else reg(lk,'-');//+'_'+spari[2]+'xxxxxx';
       except writeln('--------------------------');end;
   end;
  type stex=string[255];
  var i,j,ii,prs,a1,a2:integer;thislka,alk:tstringlist;vf,att1,att2,lk,apair:string;
      freqs:array[0..24] of array[0..24] of longword;
      marg1,marg2:array[0..24] of longword;
      exs:array[0..24] of array[0..24] of stex;
      elen:word;  sum,exces:integer;
      expec:longint;
      loput:tstringlist;
      pstart:word;

begin
 setlength(vocs,10000);
 fillchar(exs,sizeof(exs),0);
 fillchar(freqs,sizeof(freqs),0);
 fillchar(marg1,sizeof(marg1),0);
 fillchar(marg2,sizeof(marg2),0);
 havas:=tstringlist.create;
 assign(inf,'lauseet.turku');
 reset(inf);
 spari:=tstringlist.Create;
 spari.Delimiter:='&';
 spari.strictDelimiter:=true;
 alk:=tstringlist.Create;
 vormu:=tstringlist.Create;
 luokat:=tstringlist.Create;
 sijat:=tstringlist.Create;
 loput:=tstringlist.Create;
 luokat.sorted:=true;
 vormu.Delimiter:='|';
 vormu.sorted:=true;
 alk.sorted:=true;
 havas.sorted:=true;
 luokat.sorted:=true;
 alk.duplicates:=dupignore;
 luokat.duplicates:=dupignore;
 ii:=0;
 //quic & dirty ... ei v‰li‰
 lnames:=tstringlist.Create;
 lnames.commatext:='Adj,ADP,ADV,CONJ,INTJ,Nou,Num,Pro,PUNCT,SCONJ,SYM,VAgt,Vei,Vfi,VI1,VI2,VI3,VNeg,VPast,VPres,X';
 sijat.commatext:='A,Abe,Abl,Acc,Ade,All,Aux,Com,Ela,Ess,Gen,Ill,Ine,Ins,Nom,Par,Tra';
 loput.commatext:='tot,tta,lta,acc,lla,lle,Aux,ine,sta,na,N,han,ssa,oin,===,a,ksi';
 while not eof(inf) do
 begin
    ii:=ii+1;if ii>1000000 then break;
     readln(inf,rivi);
     spari.delimitedtext:=rivi;
   if spari[1]='VERB' then  //m‰‰re on verbi
   if spari[3]='VERB' then  //p‰‰sna  verbi
   if pos('Case=',rivi)<1 then  //p‰‰sna  verbi
   if spari[0]<>'neg' then
   if spari[0]<>'advcl' then
   if spari[0]<>'conj' then
   if spari[0]<>'cop' then //continue;
   if (spari[5]<>'ei') and (spari[6]<>'ei') then
   begin
        kirjaa(spari[6],'Aux',spari[5]+'.'+spari[6]+'.'+spari[7]+'.'+spari[8]+'.'+spari[9]+'//'+spari[0]);
      //writeln(^j,spari[0],': ',spari[5],' ',spari[6],'      ' ,spari[9] ); //verbi m‰‰reen‰
       continue;
   end;
     //writeln(spari.count);continue;
     if spari[3]<>'VERB' then  continue;//p‰‰sana ei ole verbi
     if (spari[1]='') or (spari[3]='') then   continue;
     att2:=dowrd(spari[3],spari[4]);
     if att2<>'Vfi' then continue;
      att1:=dowrd(spari[1],spari[2]);
      a1:=lnames.indexof(att1);
      a2:=lnames.indexof(att2);
      try
      if pos('#',spari[6])>0 then continue;
      if pos(spari[6][length(spari[6])],'a‰')<=0 then
      begin continue;end;
      except writeln('*****,',spari[6],spari.count,'    ',rivi);end;
try      if vormu.values['Case']='' then continue;
      //writeln(spari[6]);//,' ',vormu.values['Case']);//,'  #',spari[6][length(spari[6])],pos(spari[6][length(spari[6])],'a‰'));//,' ',spari[5]);
      //writeln(vormu.values['Case']);//,'  #',spari[6][length(spari[6])],pos(spari[6][length(spari[6])],'a‰'));//,' ',spari[5]);
      kirjaa(spari[6],vormu.values['Case'],spari[5]);
except writeln('!!,',spari[6],spari.count,'    ',rivi);end;
      continue;

      try  //muillekin kuin verbeille, skipataan toistaiseksi
      if a1>=0 then if a2>=0 then
      begin
       inc(freqs[a1,a2]);inc(marg1[a1]);inc(marg2[a2]);
       elen:=length(exs[a1,a2]);
       //writeln(a1,' ',a2,' ',ii,' ',elen);
       //if ii>1000 then if elen<240 then
       begin exs[a1,a2]:=exs[a1,a2]+' '+copy(spari[5],1,255-elen);end;
      end;
      except writeln(a1,' . ',a2);end;

 end;
  for i:=0 to havas.count-1 do
  //if thava(havas.objects[i]).ocs[0]>1000 then
  begin
    try
    write(^j,havas[i],',');
    for j:=0 to 16 do //if thava(havas.objects[i]).ocs[j]>-990 then write(' ',loput[j],'/',thava(havas.objects[i]).ocs[j]);
    write(',',thava(havas.objects[i]).ocs[j]);
    except writeln('???',expec);end;
  end;

 exit;
 writeln('<h3>mink‰ sarakkeita rivit m‰‰ritt‰v‰t</h3><table border="1"><tr><td>=</td>');
 for i:=0 to lnames.count-1 do writeln('<td>',lnames[i],'<br>',marg2[i],'</td>');
 writeln('</tr><tr>');
 for i:=0 to lnames.count-1 do
 begin
  writeln('<td>',lnames[i],'</td>');
  for j:=0 to lnames.count-1 do
  begin
    try
  expec:=round((marg1[i]*marg2[j] /10000000));//+(marg2[i]*marg2[j] div 1000000));
    except writeln('<td>???',expec);end;
  try
  exces:=1*freqs[i,j] div (expec+1);
  writeln('<td title="',inttostr(exces div 10)+exs[i,j],'">',ifs(exces>50, inttostr(exces div 10),''),'</td>');//lnames[i],' ', marg1[i],' ',marg2[i]);
  except writeln('<td>!!!',expec);end;
  end;
  //writeln('<td title="',exs[i,j],'">',ifs(exp>1,inttostr(freqs[i,j]*100 div (marg1[i]+1)),''),'</td>');//lnames[i],' ', marg1[i],' ',marg2[i]);
  writeln('<td>',marg1[i],'</td></tr><tr>');
 end;
 writeln('</table>-------------------------');
 exit;
 writeln('<h3>mit‰ rivej‰  mitk‰ sarakkeet m‰‰ritt‰v‰t</h3>P‰‰sana rivill‰, m‰‰re sarakkeella<table border="1"><tr><td>=</td>');
 for i:=0 to lnames.count-1 do writeln('<td>',lnames[i],'<br>',marg1[i],'</td>');
 writeln('</tr><tr>');
 for i:=0 to lnames.count-1 do
 begin
  writeln('<td>',lnames[i],'</td>');
  sum:=0;
  for j:=0 to lnames.count-1 do
  begin
   writeln('<td title="',exs[j,i],'">'//,(freqs[j,i]*100) div (marg2[i]+1),'</td>');//lnames[i],' ', marg1[i],' ',marg2[i]);
  ,ifs(100*freqs[j,i]/(1+marg2[i])>1,inttostr(freqs[j,i]*100 div (marg2[i]+1)),''),'</td>');
   sum:=sum+freqs[j,i]

  end;
  writeln('<td>',marg2[i],'</td><td>',sum,'</td></tr><tr>');

 end;
 writeln('</table>-------------------------');
  exit;
 for i:=0 to luokat.count-1 do writeln(^j,luokat[i],':',tstringlist(luokat.OBJECTS[I]).commatext);
 writeln(^j,'KAIKKI ',luokat.count,': ',luokat.commatext);
 //for i:=0 to luokat.count-1 do writeln(luokat[i]);

end;

 procedure tsimmat.list(sl:tstringlist);
 var i,j:word;
 begin
   for i:=0 to 52 do
   begin
      writeln('<li>',i,'<b>',sl[i+1],'</b>::');
      for j:=1 to cols-1 do if vars[(i)*cols+j]=0 then break else writeln(sl[vars[i*cols+j]+1],vals[i*cols+j]);
   end;
 end;
 function tsimmat.kerro(luku:word;var sanums,safreks:tlist;sl:tstringlist;res,resfreks:tlist):tlist;
 var w1,w2,w3,w4,san1,saf1,san2,saf2,san3,saf3,san4,saf4,
   //sanum,safre,sanz,maxi:word;
     hits,misses:integer;
     st:string;
   function _isin(san:word):word;
   var i:word;
   begin
     result:=0;
     //for i:=0 to

   end;
   var ykshits:double;w:word;d:boolean;
 begin   //res ei oo tulos vaan eka, lyhyt lista. tulosta ei viel‰ tuukaan
  //maxi:=0;
  // listgrams;
  writeln('<h3>:kerro: ',sanums.count,'</h3><ul>');
  cols:=64;
  setlength(kertotaulu,30000);
  //if 1=1 then
  writeln('kerro:',sanums.Count,'<ul>');
  for w1:=0 to sanums.count-1 do
  begin
     d:=w1>9999;
    //if w1>20 then break;
    san1:=integer(sanums[w1]);
    if san1=0 then continue;
    saf1:=integer(safreks[w1]);
    writeln('<h3><b>',sl[san1],'</b>', saf1,'/',integer(vals[(san1-1)*cols]),'</h3><ul title="',SL[SAN1],'">');//sl[vars[(w+1)*cols]],vars[(w+1)*cols],':');
    for w2:=1 to 63  do
    begin

     hits:=0; misses:=0;
     try
     san2:=integer(vars[(san1-1)*cols+w2]);
     if san2=0 THEN continue;
     saf2:=integer(vals[(saN1-1)*cols+w2]);
     if d then writeln('<li><b>',sl[san2+1],saf2,'</b> ',vals[san2*cols],'<ul></ul>');
     if saf2=0 then continue;
     //if res.indexof(pointer(sanum))>0 then hits:=hits+(1) else misses:=misses+safre;
     except writeln('failw2:',w3);end;
     st:='';
     for w3:=1 to 63  do //if w3<>W2 THEN
     begin
      //if san2=w3 then continue;
      try
      san3:=integer(vars[(san2)*cols+w3]);
      //if san3=san2 then continue;
      saf3:=min(10,integer(vals[(saN2)*cols+w3]));
      if saf3=0 then continue;
      except writeln('failw3:',w3);end;

      ykshits:=0;
      //for w4:=1 to 8  do writeln(sl[integer(vars[(san2)*cols+w4]+1)]);        continue;
      //writeln('<ul><li><b>',sl[san3+1],'</b>');
      for w4:=1 to 63  do //if w2<>W4 THEN
      begin
       //if san2=w3 then continue;
        san4:=integer(vars[(san3)*cols+w4]);
         if san4+1=san1 then continue;
        saf4:=integer(vals[(san3)*cols+w4]);
        //writeln('<sup style="font-size:8px">',sl[vars[(san2)*cols+w4]+1],san2,'</sup>');
        //writeln(san1,'_',san4+1);
       // if //(san3=san4) or
        if (sanums.indexof(pointer(san4+1))>0)
        then //continue;
        begin
          st:=st+(' (<sup style="font-size:10">'+inttostr((min(saf2,min(saf3,saf4))))+sl[san3+1]+inttostr(saf3)+'/'+sl[san4+1]+inttostr(saf4)+'</sup>) ');
         // writeln(' ',sl[san4+1],round(ykshits),'!',saf2,'/',saf3,'/',saf4);
         //ykshits:=ykshits+(100/((10-saf2)*(10-saf3)+1));
         ykshits:=ykshits+(min(saf2,min(saf3,saf4)));
        end;// else writeln('<sub>',sl[san4+1],'</sub>');
      end;
      hits:=hits+round(ykshits);
      //if ykshits>0 then writeln('><b>',round(ykshits),'=',hits,'</b>!');
      //writeln('</ul>');
       ;//else writeln('.');//<LI> ',sl[sanz],'/',hits,'/',sapaino, '=<em>', (hits*sapaino),'</em> ',misses);
    end;
   //if (hits*sapaino>45) then
   if d then
   begin
      //    writeLN(slist[vars[W*64+33]],' /');//,vals[i*64+j],' ');
     // if (sanums.indexof(pointer(san2+1))<0) then
     //if 10*hits/vals[san2*cols]>1 then
     writeln('<li><b  style="color:green">',sl[san2+1],round(10*hits/vals[san2*cols]),'</b>',  '>',hits,'/',vals[san2*cols],' ',saf2, '=', (hits*saf1),'<b> ','</b> ',st)
     //else     writeln('<li><b  style="color:red">',sl[san2+1],round(10*hits/vals[san2*cols]),'</b>',  '>',hits,'/',vals[san2*cols],' ',saf2, '=', (hits*saf1),'<b> ','</b> ',st);
     // writeln('</ul>');
    end;
     try
     kertotaulu[san2+1]:=kertotaulu[san2+1]+hits;
     except writeln( san1*cols+san2+1,'>',hits);end;
   end;
   writeln('</ul>');
  end;
  //exit;
  //writeln('<h3>:kerro ',luku,'</h3>');
 { for w1:=0 to sanums.count-1 do
  begin
   sanz:=integer(sanums[w]-1);
   sapaino:=integer(safreks[w]-1);
    //writeln('<li>',sl[sanz+1],sanz+1,vars[);
    for ss:=1 to luku do
    if vars[sanz*cols+ss]=0 then break else
    begin
     //spaino:=integer(sanums[w]-1);
     sanum:=integer(vars[sanz*cols+ss]+1);
     npaino:=integer(vals[sanz*cols+ss]+1);
      if sanum>=24555 then continue;
      try
      //writeln(sanum);//,sl[sanum]);
      kertotaulu[sanum]:=kertotaulu[sanum]+min(sapaino,npaino);
      if maxi<kertotaulu[sanum] then writeln(sl[sanz+1],'/',sl[vars[sanz*cols+ss]+1],':');
      maxi:=max(maxi,kertotaulu[sanum]);
      except writeln('FAILW:',sanz,'/',sanum);//,sl[sanz+1],'/',sl[vars[sanz*cols]+1],':');
      end;
    end;
 end;
 }
 writeln('<h1>cors:</h1>');
 for w:=0 to 27551 do
 if kertotaulu[w]>10  then
 try
 if kertotaulu[w]>100  then
 writeln('<li>',sl[w],' ',kertotaulu[w]);
 res.add(tobject(pointer(w)));
 resfreks.add(tobject(pointer(w)));
 except writeln('<li>,xxx:', sl[w]);end;

end;

function tsimmat.haegramlist(luku:word;var sanums,sapainot:tlist;sl:tstringlist;res,respainot:tlist):word;
 var w,i,j,hit:integer;sanum,sapaino,apaino:word;resindex:integer;
  procedure addcoco(wrd,paino:integer);
  begin
   //hit:=vars[(r-1)*cols+i];   //vitun zerobase
   if paino=0 then exit;
   resindex:=res.indexof(tobject(pointer(wrd)));
   if resindex<0 then
   begin
    //apaino:=min(6,vals[(sanum-1)*cols+i]);
    res.add(pointer(wrd));
    if paino>100 then
    writeln('<li style="color:red">',sl[wrd],'/',paino,'</li>');
    //writeln('',sl[wrd],'/',paino);
    respainot.add(pointer(paino));
   end else
   begin
    //writeln('<span style="xcolor:blue">',sl[wrd],integer(respainot[resindex]),'+',paino,'</span>');
    respainot[resindex]:=pointer(
      (respainot[resindex])+
       round((100-integer(respainot[resindex]))*((100-paino)/1000))) ;
    //respainot[resindex]:=respainot[resindex]+paino ;
     //if sl[wrd]='lˆyly' then
     //if integer(respainot[resindex])>100 then
     //writeln('<span style="color:red">','=',integer(respainot[resindex]),'</span>') else
     writeln('<span style="color:blue">',sl[wrd],'==',integer(respainot[resindex]),'</span>');
   end;
  end;
 begin
   //listgrams;  exit;
   setlength(kertotaulu,30000);
   writeln('<li>DOGL',res.count,'/',respainot.count);
   //for w:=0 to sanums.count-1 do     writeln('<li>',sl[integer(sanums[w])],integer(sapainot[w]));
   hit:=-1;
   for w:=0 to sanums.count-1 do
   begin
    sanum:=integer(sanums[w]);
    try
    sapaino:=integer(sapainot[w]);
    //sapaino:=sapaino;
    except writeln('FAILL');end;
    writeln('<b>::',sl[sanum],sapaino*sapaino,'</b>');
    addcoco(sanum,sapaino*sapaino);
    //if res.indexof(tobject(pointer(sanum)))<0 then
    //res.add(pointer(sanum));   respainot.add(pointer(sapaino*sapaino));
    //sanum:=sanum-1;
    //writeln('<li>',i,'<b>',sl[i+1],'</b>::');
    //for j:=1 to cols-1 do if vars[(i)*cols+j]=0 then break else writeln(sl[vars[i*cols+j]+1],vals[i*cols+j]);
   for i:=1 to luku-1 do //cols-1 do
   if vars[(sanum-1)*cols+i]=0 then continue
   else begin
    hit:=vars[(sanum-1)*cols+i];   //vitun zerobase
    if hit<0 then continue;
    apaino:=vals[(sanum-1)*cols+i];
     if apaino>100  then
    writeln('<li>[',apaino,sl[hit+1],sapaino,']');
    addcoco(hit+1,sapaino*apaino)
    //resindex:=res.indexof(tobject(pointer(hit+1)));
    {if resindex<0 then
    begin
     apaino:=min(6,vals[(sanum-1)*cols+i]);
     if apaino<1 then continue;
     res.add(pointer(hit+1));
     respainot.add(pointer(sapaino*apaino));
     //writeln('+',sl[hit+1],apaino*sapaino);
    end else
    begin
      respainot[resindex]:=respainot[resindex]+sapaino*apaino*2 ;
      //writeln('>',sl[hit+1],apaino*sapaino,'=',integer(respainot[resindex]));

    end;}
    //writeln('',hit)

    end;
   end;
   for i:=0 to respainot.Count-1 do respainot[i]:=pointer(integer(respainot[i]) div 10);
   writeln('<li>DIDGL',res.count,'/',respainot.count);
 end;

constructor tsimisanat.create(c:word;fn,sfn:string);
begin
slist:=tstringlist.create;
listfname:=sfn;
slist:=tstringlist.create;
slist.loadfromfile(listfname);
matx:=tsimmat.create(slist.count,c,fn,slist);

end;
constructor tsimmat.create(r,c:word;fn:string;sl:tstringlist);
vAR ii,I,J,jj,ci,cj,jval,n,biggest:WORD;
    tot,thisi,thisj,ij:longword;
    rap:string;
    //cous:array of byte;

begin
 slist:=sl;
 writeln('gosims:',slist.count,'*',r,'*',c);
 // setlength(cous,30001*30001);
  rows:=r;cols:=c;
  //setlength(vars,r*c);
  //setlength(vals,r*c);
  setlength(vars,30001*64);
  setlength(vals,30001*64);

  READbin(r,c,2,vars,fn+'.spar') ;
  READbin(r,c,2,vals,fn+'_v.spar') ;
  //fillchar(nvars[0],sizeof(nvars),0);
  //fillchar(nvals[0],sizeof(nvals),0);
  //for i:=1 to rows-1 do
  //exit;
   for i:=9190 to 200 do
  begin
    writeln(^j^j^j'<li>',slist[i],'#',vars[i*64],vals[i*64],':');
   for j:=1 to 64 do
    if vals[i*64+j]<10 then continue else
    writeln(slist[vars[i*64+j]],'#',vals[i*64+j],':');
  end;
end;
end.

