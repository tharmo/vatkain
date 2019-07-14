unit riimiuus;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,verbikama,nominit,sanataulu,strutils,riimiutils;
const vahvatverbiluokat=[52..63,76];
const vahvatnominiluokat=[1..31,76];
const rimis=64;
type string31=string[31];

type tlka=record esim:string;kot,ekasis,vikasis:word;vahva:boolean;vikasana:word;end;
type tsis=record ekaav,vikaav:word;sis:string[8];vikasana,lk:word;end;
type tsanainfo=record sana:string31;num,lka,sija:word;sanalka:byte; end;
type tsinfotaulu=class(tobject)
  taulu:array of tsanainfo;
  wcount:integer; //ei yli word...pitänee laajentaa myöhemmin longwordiksi

  procedure add(gsana:string31;nnum,nlka,nsija:word;nsanalka:byte);
  constructor create(n:word);
end;
//type tverlvok=record ekaav,vikaav:word;vok:array[0..1] of char;end;
type tav=record ekasana,takia,vikasana,sis:word;h,v,av:string[1];end;  //lisää viekä .takia - se josta etuvokaalit alkavat

type tsanasto=class(tobject)
 vcount,ncount:integer;
 lks:array[0..80] of tlka;
 siss:array[0..2047] of tsis;
 avs:array[0..2047] of tav;
 sans:array[0..65535] of tsan;
 sanataulu:array[0..65535] of string[20];
 verbit:tverbit;
 nominit:tnominit;
 eitaivu:tstaulu;
 hakulista:tstringlist;
 hitlist:array[1..30000] of word;hitcount:integer;
 //sanoja:tstringlist;
 slist:tstringlist;
 resutaulu:tsinfotaulu;
 isad: bitpacked array [0..27546] of Boolean;   //risadjbin;
  procedure readadjbin;
  function haenumero(sana:ANSISTRING;debug,rev:boolean):word;
  procedure generatelist(wlist:tlist;all:boolean);
  procedure addtolist(sana:ansistring);
//rocedure luohaku;
 //0-pohjasia, mutta data alkaa 1:stä
 procedure luesanat(fn:string);
 procedure listaa;
 function generateverb(snum:word):ansistring;
 function generatenom(snum:word;all:boolean):ansistring;
 procedure concor;
 function etsiyks(hakusana,hakuakon,hakukoko:string;hakueietu,hakueitaka:boolean;sika:tsija;aresu:tstringlist;onjolist:tlist;var hits:word;d:boolean):word;
 constructor create;
 procedure parsewend(anend:string);
 procedure rajat;
 procedure tutkimuodot;
 function generateone(sana,muoto:word):string;
 procedure luokaavat;
 procedure luomuodot;
 procedure tutkimuotoerot;
 //function generateone(luokka:tlka;sana:tsan;muoto:tsija):string;
 end;
var sanasto:tsanaSTO;
type twend=record
    endi:string[16];
    st:string[4];
    eatkon,eatdif1,eatvok,dblcon:boolean;

    //vokdbl,konvex,kondbl,difmuu:boolean;

end;
var tcounts:array[0..48] of array [0..33] of array[0..10] of word;tc_l,tc_m:word; //tmp

implementation
uses //riimitys,
  math;
const
    Days : array[0..6] of string =
    (
      'Sun', 'Mon', 'Tue', 'Wed',
      'Thu', 'Fri', 'Sat'
    ) ;
const
    uusnum: array  [0..33] of word=( //(0,1,2,3,4,5,6,7,8,9,10,11,12,13,25,26,27,28,29,14,32,33,15,16,17,30,31,18,19,20,21,22,23,24);
            0, //nom
            2,  // part
            1,3,4,6,7,8,10,11,//gen+paikallis+abe+tra
            9,     //ess, pari poikkeamaa paikeista.. (luokat joissa muotojen astevaihteluita on kirjoittu kaavoihin
            5, // illat,
            //monikot:
            12, // mon nom , sama kuin yks paikallis
            15,16,18,19,20,22,23,24, //paik yms (ei gen
            21,   //ess, pari poikkeamaa paikeista.. (luokat joissa muotoja on kirjoittu kaavoihin
            14,32,33, //partit
            17,30,31, //illatiiveja, keskenään erilaisia
            13,25,26,27,28,29  //monikon genetiivejä, erilaisia, joku turha joukossa
     );

procedure tsanasto.luomuodot;
 var mfile:text;i,j,jj:word;  //järjestää muodot uudestaan
 begin
  writeln('<pre>');
  write(^j,'Endings:');
  //for j:=0 to 33 do    write(''''+reversestring(nominit.sijat[uusnum[j]].ending)+''',');
  write(^j,'Esims:');
  for j:=0 to 33 do    write(''''+nominit.sijat[uusnum[j]].esim+''',');
  write(^j,'Nams:');
  for j:=0 to 33 do    write(''''+replacestr(nominit.sijat[uusnum[j]].name,' ','')+''',');
  write(^j,'vahvanvahvat:');  for j:=0 to 33 do if nominit.sijat[j].vv then write('''',uusnum[j],''',');
  write(^j,'heikonheikot:');  for j:=0 to 33 do if not nominit.sijat[j].hv then write('''',uusnum[j],''',');
    assign(mfile,'nommuodot.csv');
    rewrite(mfile);
    for j:=0 to 33 do    write(mfile,inttostr(uusnum[j])+'('+inttostr(j)+')':11);
    write(mfile,^j,'          ');
    for j:=0 to 33 do    write(mfile,reversestring(nominit.sijat[uusnum[j]].ending):11);
    write(mfile,^j,'          ');
    for j:=0 to 33 do    write(mfile,(nominit.sijat[uusnum[j]].esim):11);
    write(mfile,^j,'          ');
    for j:=0 to 33 do    write(mfile,(replacestr(nominit.sijat[uusnum[j]].name,' ','')):11);

    for i:=1 to 49 do
    begin
      write(mfile,^j,inttostr(i),lks[i].esim:9);
      for jj:=0 to 33 do
      begin
       j:=uusnum[jj];
       write(mfile,reversestring(nominit.lmmids[i,j]):10,' ');
      end;
    end;
    close(mfile);
end;
procedure tsanasto.luokaavat;
   function aste(koodi:char):string;
   var av:string;
   begin
   case koodi of
   'A': av:='k*' ;
   'B':  av:='p*' ;
   'C':   av:='t*' ;
   'D':  av:='k*' ;
   'E':  av:='pv' ;
   'F':  av:='td' ;
   'G': av:='kg' ;
   'H':  av:='pm' ;
   'I':  av:='tl' ;
   'J':  av:='tn' ;
   'K':  av:='tr' ;
   'L':  av:='kj' ;
   'M':  av:='kv' ;
   '0': av:='--';
   else av:='xxx';
  end;
   result:=av;
 end;
   var osat,tavut,sanakaavat,muotokaavat:tstringlist;  lkasofar:word;prevlka,rivi:string;
   //mfile:text;
   procedure testt(st:string);
   begin
      writeln('<li>',st,hyphenfi(st,osat),osat.commatext);
   end;
   var f:text;s,ss,prevav:string; n:word;    i,j,k,jj,slen:word;olia,prevlk,ordo:word;
         pALA:WORD; sanaav,muotoav,myvok,mymid,color:string;
     var s_v,s_a,s_s,s_x:string;
          outo,vahva:boolean;
          xxx:tstringlist;
   begin
   osat:=tstringlist.create;
   muotokaavat:=tstringlist.create;
   tavut:=tstringlist.create;
   sanakaavat:=tstringlist.create;
   sanakaavat.sorted:=true;
   //testt('aiettakaan');    testt('traiettakaan');   testt('hauilla');   testt('aieoittra');  exit;
//////////test;
//assign(f,'nomsall.lst');
writeln('<table border="1"><tr>');
ordo:=0;
assign(f,'uussanat.nom');
 reset(f);
 while not eof(f) do
 begin
   readln(f,s);
   osat.commatext:=s;
   try
   n:=strtointdef(osat[0],999);
   if n<>prevlk then begin prevav:='x';writeln('<hr>');end;
   prevlk:=n;
   if n>49 then break;
   if nominit.lmmids[n,0]<>'' then //continue;
   if pos(reversestring(nominit.lmmids[n,0]),osat[2])<=0 then
   begin writeln('<li>NOGO:(',red(osat[2]+'/'+nominit.lmmids[n,0]),n,')');continue;
   end;
  // writeln(n,nominit.lmmids[n,0],pos(nominit.lmmids[n,0],osat[2]));
   ss:=reversestring(copy(osat[2],1,length(osat[2])-length(nominit.lmmids[n,0])));
   slen:=length(ss);
   PALA:=0;
   s_v:=ss[1];s_a:='';s_s:='';s_x:='';
   //if pos(s_v,konsonantit)>0 then writeln(s);
   try
   for i:=2 to slen  DO
     if (pos(ss[i],konsonantit)>0) then begin s_a:=ss[i];s_s:=copy(ss,i+1);break;end
     else if (isvokraja(s_v[i-1],ss[i])) or (i>2) then begin s_s:=copy(ss,i);break;end
     else s_v:=s_v+ss[i];
  //   write(' ',s_v[1],ss[2],isvokraja(s_v[1],ss[2]));
   except writeln('fAILPILKO:',S);END;
  //if n<>17 then continue;//if length(s_v)<2 then continue;
  //if osat[1]='0' then continue;
  //if length(s_v)<2 then continue;
  sanaav:=aste(osat[1][1]);
  outo:=false;
  //if (n<33) then begin if  (sanaav[1]<>s_a) then outo:=true;myav:=myav[1];end
  //else begin if (myav[1]<>s_a) then outo:=true;myav:=myav[2];end
  except writeln('<li>failx');end;
  //if not outo then continue;
   try

   sanakaavat.Add(AddChar('0', inttostr(n), 2)+','+(s_v+','+s_a+','+s_s)+' \'+s);
   //if osat[1]=prevav then continue;
   //if osat[1]='0' then continue;
   if sanaav[2]<>'*' then continue; //TMP debug!
   writeln('<li>',AddChar('0', inttostr(n), 2)+',<b>'+(s_v+','+s_a+'</b>,'+s_s)+' \'+s,' <b>',aste(osat[1][1]),'</b>');

   for j:=1 to 33 do
   begin
     if not (j in [17,30,31])then continue;
     mymid:=nominit.lmmids[n,j];
     //if mymid='!' then continue;
     //myav:=aste(osat[1][1]);
     try
     if osat[1]='0' then muotoav:=s_a
     else
     begin
         if n<33 then if nominit.sijat[j].vv then muotoav:=sanaav[1] else muotoav:=sanaav[2];
         if n>32 then if nominit.sijat[j].hv then
         begin
           if sanaav[2]='*' then muotoav:=sanaav[1]+sanaav[1] else muotoav:=sanaav[1]  // heikon vahvat
         end
         else
         begin //heikon heikot
            if sanaav[2]='*' then muotoav:='' else muotoav:=sanaav[2];
         end;
     end;
     except writeln('!!!!!!',mymid,n,j);end;
     myvok:=s_v;
     //if yav='_' then myav:='';
     if pos('-',mymid)=1 then begin myvok:=copy(myvok,2,9);mymid:=copy(mymid,2,99);end;
     if mymid='!' then
     writeln('<small style="color:#ccc">',reversestring(nominit.sijat[j].ending+mymid+myvok+muotoav+s_s),'</small>')
     else writeln(reversestring(nominit.sijat[j].ending+mymid+myvok+muotoav+s_s));

   end;
   prevav:=osat[1];

   continue;
  // if osat[0]+<>prevlka then
  if osat[0]<>prevlka then begin writeln('',s);olia:=0;end;
  prevlka:=osat[0];
  if length(osat[2])>15 then continue;

  if  (osat[1]='D') and (length(s_a)=1)
    //(n<33) and (length(s_a)=0))  or ((n>32) and (length(s_a)=1))
  then
    else continue;//if (osat[1]='A') and (olia<2)  then inc(olia) else  continue;
  //olid:=false;
  //if (s_v='') or (hyphenfi(s_v,nil)<2) then continue;
  //for i:=1 to 3 do
  //if not isdifto(s_s[length(s_s)],s_v[1]) then
   continue;
  WRITE('<LI>',s,':');
  write('=',s_s+'<b>'+s_a+'</b>'+s_v,' ?',s_x);
  write('  /<b>',aste(osat[1][1]),'</b> ',osat[1][1],' ',hyphenfi(s_v,tavut),' ',tavut.commatext,'  ',s_s[length(s_s)],s_v[1] );
  except writeln(' <li>FAIL::',s,' ',n,' voks:',s_v,'!');end;
 end;
 sanakaavat.SaveToFile('uuskaavas.lst');
 prevlka:='';
 exit;
 for i:=0 to sanakaavat.count-1 do
 begin
    osat.commatext:=sanakaavat[i];
    if osat[0]+osat[1]=prevlka then inc(lkasofar) else lkasofar:=1;
    prevlka:=osat[0]+osat[1];
    if lkasofar>1 then continue;
    writeln('<li>',sanakaavat[i]);
 end;
end;

{ tutkittiin osaa muodoista (sisäpikallis + pari muuta)

for i:=1 to 48 do
begin
  writeln('<table border="1"><tr><td>',i,'</td>');
  for j in [1,3,4,6,7,8,10..12] do
    writeln('<td>',j,reversestring(nominit.sijat[i].ending),'</td>');
  for jj in [1,3,4,6,7,8,10..12] do
  begin
   j:=uusnum[jj];
   writeln('<tr><td>',j,'</td>');
   for k in [1,3,4,6,7,8,10..12] do
   begin
     writeln('<td> ');
      if nominit.lmmids[i,j]<>nominit.lmmids[i,k] then writeln('['+nominit.lmmids[i,j]+'\'+nominit.lmmids[i,k]+inttostr(j)+inttostr(k)+']');
      writeln('</td>');
   end;
   writeln('<tr>');
end;
writeln('</table><h1>here</h1>');
}

procedure tsanasto.tutkimuotoerot;
var difs:array[0..33] of array[0..33] of word;i,j,k,jj:word;
begin
fillchar(difs,sizeof(difs),0);
for i:=1 to 48 do
begin
  for j:=0 to 33  do
  for k:=0 to 33  do
      if nominit.lmmids[i,j]<>nominit.lmmids[i,k] then inc(difs[j,k]);
end;
begin
  writeln('DIFS<table border="1"><tr><td>',i,'</td>');
  for j:=0 to 33  do
    writeln('<td>',uusnum[j],reversestring(nominit.sijat[uusnum[j]].ending),'</td>');
  for j:=0 to 33  do
  begin
   writeln('<tr><td>',uusnum[j],' ',reversestring(nominit.sijat[uusnum[j]].ending),'</td>');
   for k:=0 to 33  do
   begin
    if difs[uusnum[j],uusnum[k]]<10 then
     writeln('<td> ',difs[uusnum[j],uusnum[k]],'</td>')
    else writeln('<td> </td>');
   end;
   writeln('<tr>');
 end;
end;
writeln('</table>');
writeln('<pre>');
 write(^j,'          ');
end;
function tsanasto.generateone(sana,muoto:word):string;
//function tsanasto.generateone(luokka:tlka;sana:tsan;muoto:tsija):string;
var luokka:tlka;sis:tsis;san:tsan;sij:tsija;
 var hakutakvok,emp:boolean;st,mysis,myav,mymid,recon:string;
    av:tav;sisu:tsis;
begin
 result:='';
 sij:=nominit.sijat[muoto];
  san:=sans[sana];
  av:=avs[san.av];
  sis:=siss[av.sis];
  mysis:=sis.sis;
  mymid:=nominit.lmmids[sis.lk,muoto];
  emp:=pos('_',mymid)+pos('_',mymid)+pos('#',mymid)+pos('*',mymid)>0;

  //if sana=652 then writeln('<b>',mymid,'_',mysis,'_',san.san,san.akon,'!!!',siss[sans[652].av].sis,'</b>');
  if mymid='!' then exit;
  //if emp then write('***');
  //if pos('_',mymid)+pos('_',mymid)+pos('#',mymid)+pos('*',mymid)=0 then exit;
  myav:=av.v;
  //myav:=ifs(sij.hv,av.h,av.v);
  //if mysis[length(mysis)]='-' then write('???');
  luokka:=lks[sis.lk];
  if luokka.vahva then   myav:=ifs(sij.vv,av.v,av.h)
   else   myav:=ifs(sij.hv,av.v,av.h);
  if pos('-',mymid)=1 then begin delete(mymid,1,1);delete(mysis,1,1); end;
  if pos('_',mymid)=1 then begin delete(mymid,1,1);mymid:=mymid+mysis; end;
  if pos('*',mymid)=1 then begin delete(mymid,1,1);mymid:=mymid+san.san[1]; end;
 // st:=sij.ending+'.>b/<'+mymid+'>b<'+mysis+'.'+myav+'.'+san.san+san.akon+'!';
  st:=sij.ending+mymid+mysis+myav+san.san+san.akon;
  result:=reversestring(st);
  if not(san.takavok) then result:=etu(result);
 // if muoto<>5 then if emp then result:='<em style="background:#add">'+result+'</em>';
  if pos('t',mysis)>0 then result:=result+'xxxxx';
 // st:=st+sana.san;

end;
procedure tsanasto.parsewend(anend:string);
var i,j:word; tavs:tstringlist;
begin
  tavs:=tstringlist.create;
  //eatvok //vokvex,   if (mid<>'') and (length(koita)>0) then if (mid[1]='-') then     begin vokvex:=true;delete(mid,1,1);delete(koita,1,0);; end
  //nogo //if mid='!' then exit;
  //kondbl   if (mid<>'') and (mid[1]='*') then LUOKASSA 67&28 * tuplaa, muissa meinaa S:ää
        //if luokka=67 then begin delete(koita,1,0);delete(mid,1,1);kondbl:=true;end  PUHEL > PUHELL
        //else begin write('');delete(mid,1,1);konvex:=true;end; // SUOTAA SOUSI
      //KANSi KANNen

  //* KONVEX nyt joko tuplaa (lkat 28,67) tai syö (S-imperfektit verbeissä) konsonantin, yhdenmukaista jotenkin
  //#  eatdif1 "#"  Suo Soita Tuo Toi
  //_  vokdbl:  verbs vain lka 52 sanOO paisUU  ; nomineissa Illatiivin vokaalin kahdennus tai 41,48 useissa muodoissa kirvEs->kirvEEn,ruumIs ruumIIsta
  //-     SYÖ VOKAALIN LOPPUVOKeista- Useimmiten vois hoitaa laittamalla sen vokaalin vain niihin muotihin kuin tarvitaan
            // tutki missä luokissa loppvokit vakioita 1,2 (3 muuT kuin O turhia), 5i +turhia,


 // KONVEX lka 60: lähteä -> läksi ainoa sana , hoidetaan muuten

  //eatkon,konvex
  {if (anend<>'') then
        begin
         if (anend='_') then begin if d then write('????',mid);vokdbl:=true;delete(anend,1,1);end;
        if (mid[1]='-') then
        begin vokvex:=true;delete(mid,1,1);; end
        else if mid[1]='#' then begin difmuu:=true;delete(mid,1,1);delete(koita,1,0); end;
             tatu o i da
             pu      i da}
  //ASTERIX n
  //endi:string[16];
  //st:string[4];
  //eatkon,eatdif1,eatvok,dblcon:boolean;
  writeln('<table border="1"><tr><td></td>');
  for i:=0 to 33 do  writeln('<td>',nominit.sijat[i].esim,'<br>',nominit.sijat[i].ending,'</td>');
  writeln('</tr>');
  for i:=1 to 48 do
  begin
    writeln('<tr><td>',lks[i].esim,'</td>');
    for j:=0 to 33 do
      begin
       try
       //writeln('<td>',nominit.lmmids[i,j],'<br>',hyphenfi(nominit.sijat[j].ending+nominit.lmmids[i,j],tavs),'</td>');
       writeln('<td>',nominit.sijat[j].ending,'.<em style="color:red">',nominit.lmmids[i,j],'</em><br> ',hyphenfi(nominit.sijat[j].ending+nominit.lmmids[i,j],tavs),'</td>');
        except writeln('(?',nominit.lmmids[i,j],')</td>');end;

      end;
      writeln('</tr>');

  end;
  writeln('<pre>');




end;
procedure tsanasto.readadjbin;
var
     ch:char;adf:text;c:word;   //   cat san_ana.tmp |grep -n " A Pos Nom Sg"|gawk ' BEGIN { FS=":"} { for (i=prev;i<$1;i++) { print i;s=s "0"}; s=s "1" ;print $0; prev=$1+1} END { print s>"adj.bins"}'
begin
   c:=0;
   writeln('adj:');
   assign(adf,'adj.bins');
   reset(adf);
   while not eof(adf) do
   begin
      read(adf,ch);
      c:=c+1;
      if ch='1' then isad[c]:=true else isad[c]:=false;
       //if ch='1' then writeln(c,slist[c]);
   end;
   for c:=1 to 27545 do
    if isad[c] then writeln(c,slist[c]);
end;

procedure tsinfotaulu.add(gsana:string31;nnum,nlka,nsija:word;nsanalka:byte);
var rec:tsanainfo;
begin
 try
  wcount:=wcount+1;  //note: 1.base
// rec:=
taulu[wcount].sana:=gsana;
 taulu[wcount].num:=nnum;
 taulu[wcount].lka:=nlka;taulu[wcount].sija:=nsija;taulu[wcount].sanalka:=nsanalka;
 except writeln('failadd:',wcount,'!!');end;
end;
constructor tsinfotaulu.create(n:word);
begin
  setlength(taulu,n);
  wcount:=0;
end;
procedure tsanasto.rajat;
var i,pi:word;
begin


end;
procedure tsanasto.concor;
var numsf:textfile;line,s:ansistring;
    cc:array [0..35000] of array [0..15] of word;
    lnums:array[0..15] of word;
    function setnum(num:word):word;
     var j:word;
    begin
      for j:=0 to 15 do
       if lnums[j]=num then continue
       else if lnums[j]=0 then
       begin
         lnums[j]:=num;
         //lnums[0]:=lnums[0]+1;
         break;
       end;
    end;
    function setmat(a,b:word):word;
     var j:word;
    begin
      for j:=0 to 15 do
       if cc[a,j]=b then break
       else if cc[a,j]=0 then
       begin
         cc[a,j]:=b;
         break;
       end;
    end;
    procedure listmat;
    var row,col:word;
    begin
      for row:=0 to 30000 do
       if cc[row,0]<>0 then
       begin
         writeln('<li>',row,': ');
        for col:=0 to 15 do
        if cc[row,col]=0 then break else
         writeln(slist[cc[row,col]],',');
      end;
    end;

var i,j,n,b:word;c:integer;
begin
//SAN sanoja:=tstringlist.create;
  //SAN sanoja.loadfromfile('sanat_ok.ansi');
assign(numsf,'syn_ok.num');  //synonyymit/liittyvät sanat
//assign(inf,'sanatall.arev');  //kaikki ei-yhdyssanat
reset(numsf);
//writeln('<pre>');
for i:=0 to 30000 do cc[i,0]:=i;
c:=0;
 while not eof(numsf) do
 begin
  c:=c+1;//if c>5000 then break;
  readln(numsf,line);
  b:=1;
  //writeln('<li>');;
  for i:=1 to length(line) do
     if line[i]=',' then begin setnum(strtointdef(s,-999));s:='';end
    else s:=s+line[i];
  for i:=0 to 15 do if lnums[i]=0 then break else
   for j:=0 to 15 do if lnums[j]=0 then break else
    setmat(lnums[i],lnums[j]);
   //writeln(lnums[i]);
  fillchar(lnums,sizeof(lnums),0);
  cc[1]:=lnums;
 end;
 close(numsf);
 listmat;
end;

function tsanasto.generateverb(snum:word):ansistring;

var runko,sisu,astva:str16;aresu:tstringlist;hakutakvok:boolean;
 lukn,sijax,prlim,x:integer;
sija,sikanum,ha,lkx:integer;
sika:tsija;
 d,vahvaluokka,vahvasija:boolean;
 gsana:string;
 luokka,sis,av,san:word;
 //curlkacursis,cura
  { $H-}
  //((lk,sis,av,san,sija:integer;
  //hakunen:tvhaku;
  //mymid,mysis,myav,mysana,mysija,lopvok,myend:str16;
   sikalauma, riimit,xxresu:tstringlist;

function sijaa(sija:word;curlka:tlka;cursis:tsis;curav:tav;cursan:tsan):ansistring;
var vokdbl,vokvex,konvex,kondbl:boolean;mid,myav,mysis,myend:ansistring;
begin
  try
   if sija=6 then exit;
   mysis:=cursis.sis;
   vahvasija:=true;

   myend:=reversestring(verbit.sijat[sija].ending);
   mid:=''+verbit.lmmids[luokka-52,sija];
   if curlka.vahva then begin if  sija in vvahvanheikot then vahvasija:=false;end
     else  if  sija in vheikonheikot then vahvasija:=false;
   myav:=ifs(vahvasija,avs[av].v,avs[av].h);     //writeln('[',mid,']');
   if (mid<>'') and (mid[1]='_') then
   begin
        if mysis='' then mysis:='ii' else begin MYSIS:=MYSIS[1]+MYSIS;delete(mid,1,1);end;
   end;
   if mid='!' then exit;
   if (mid<>'') and (mid[1]='*') then //mid[1]:=mysis[1];
      if mysis='' then  begin delete(mid,1,1);myav:=''; end else mid[1]:=mysis[1];  //loppuii vierasp sanoissa lka 5
   //if mysis=''then   mysis:='ii' else mySIS:=mySIS[1]+mySIS;  //loppuii vierasp sanoissa lka 5

   if (mid<>'')  and (mid[1]='-') then
   begin
       if (mid='--') then //writeln('<li>64:mid:',mid,'/sis:',mysis,sija);
       begin  mid:='';mysis:=mysis[1]; end    ///speak
       else
       begin
           delete(mid,1,1);
           delete(mysis,1,1);
       end;
   end;
  except writeln('failvs:<b>',cursan.san,cursan.akon,sija,'</b>');end;

   try
   gsana:=reversestring(myend)+string(mid+mysis+myav+sans[snum].san+sans[snum].akon);
  // writeln('<li>vvv_',reversestring(gsana));
   //if curlka.kot=64 then writeln('<li>zzx ',reversestring(gsana));
           //san; if konvex then if curlka.kot=60 then  delete(sana,1,1);  //vain "lähteä" monikot läksin
           //av:   if konvex then if myav<>'' then begin delete(myav,1,1);end;
           //sis;          if kondbl then if curlka.kot=67 then mysis:=MYSIS[1]+MYSIS;
                //                 if vokdbl then if mysis='' then mysis:='ii' else MYSIS:=MYSIS[1]+MYSIS;  //loppuii vierasp sanoissa lka 5
          //           if vokvex then f curlka.kot=64 //viedä vei then       delete(mysis,2,1) else delete(mysis,1,1);
       if not sans[snum].takavok then gsana:=etu(gsana);
       //if taka(gsana)<>taka(sanoja[snum]) then
       //resus.addobject(gsana,tobject(pointer(snum)));
       resutaulu.add(gsana,snum,curlka.kot,sija,2);
       //if reversestring(gsana)='jöittä' then
        // writeln('<li>:',reversestring(gsana),':',sija,vahvasija,verbit.lmmids[luokka-52,sija],'/',mid);
  except writeln('failverb!!!',gsana);end;
end;

var curlka:tlka;
begin     //kaikki yhden verbin sijamuodot resutauluun
  sija:=0;
  for lUOKKA:=0 to 78 do
  begin
    //writeln('<h4>LKA:',luokka,'</h4>');
    if lks[luokka].vikasana>=snum then
    begin
      curlka:=lks[luokka];
      vahvasija:=true;
      //if luokka<52 then
      begin
        for sis:=lks[lUOKka].ekasis to lks[lUOKka].vikasis do
        if siss[sis].vikasana>=snum then
         for AV:=SISS[SIS].ekaAV to SISS[SIS].VIKAAV do
          if avs[av].vikasana>=snum then
          //if avs[av].v<>avs[av].h then
          begin
           //writeln('<li>',snum,'#',luokka,sanoja[snum],';',curlka.kot,'.',reversestring(siss[sis].sis+'_'+avs[av].v+avs[av].h+'.'+sans[snum].san+sans[snum].akon),' ',luokka,curlka.vahva,siss[sis].sis,':');
           for sija:=0 to 66 do //sikoja do
           //for sija in [0,5,12,13,16,23,36,37,39,45] do //sikoja do
              sijaa(sija,curlka,siss[sis],avs[av],sans[snum]);
              //function sijaa(curlka:tlka;cursis:tsis;curav:tav;cursan:tsan):ansistring;
              exit;
          end;
          exit;
        end;
      end;
         //for SAN:=AVS[AV].ekasana to avs[av].VIKAsana do
     //writeln('<hr>');
    end;
  end;

//function tsanasto.generatenom(resus:tstringlist;snum:word;all:boolean):ansistring;
function tsanasto.generatenom(snum:word;all:boolean):ansistring;
  function red(st:string):string; begin result:='<b style="color:red">'+st+'</b>';  end;
  function blue(st:string):string;  begin result:='<b style="color:blue">'+st+'</b>';  end;

var runko,sisu,astva:str16;aresu:tstringlist;hakutakvok:boolean;
 lukn,sijax,prlim,x:integer;
sija,sikanum,ha,lkx:integer;
sika:tsija;
 d,vahvaluokka,vahvasija:boolean;
 gsana:string;
 luokka,sis,av,san:word;
 //curlkacursis,cura
  { $H-}
  //((lk,sis,av,san,sija:integer;
  //hakunen:tvhaku;
  //mymid,mysis,myav,mysana,mysija,lopvok,myend:str16;
   sikalauma, riimit,xxresu:tstringlist;

function sijaa(sija:word;curlka:tlka;cursis:tsis;curav:tav;cursan:tsan):ansistring;
var vokdbl,vokvex,konvex,kondbl:boolean;mid,myav,mysis,myend:ansistring; newrec:tsanainfo;
   pvok:char;
begin
  try
   mysis:=cursis.sis;
   vahvasija:=true;
   begin
     //writeln('<li>SIJA',sija);
     myend:=(nominit.sijat[sija].ending);
     try
     mid:=nominit.lmmids[luokka,sija];
      except writeln('FAILN:',snum,':',myend,',',mid,',',vahvasija,',',myav,'!',luokka,'/',sija);end;
     if curlka.vahva then if  not (sija in nvahvanvahvat) then vahvasija:=false;  //vv hh
     if not curlka.vahva then if  (sija in nheikonheikot) then vahvasija:=false;  //vv hh
     myav:=ifs(vahvasija,avs[av].v,avs[av].h);     //writeln('[',mid,']');
     if (mid<>'') and (mid[1]='_') then
     begin
        if mysis='' then mysis:='ii' else begin MYSIS:=MYSIS[1]+MYSIS;delete(mid,1,1);end;
     end;
     if mid='!' then exit;
     if (mid<>'') and (mid[1]='*') then //mid[1]:=mysis[1];
        if mysis='' then  begin delete(mid,1,1);mysis:=myav; end else mid[1]:=mysis[1];  //loppuii vierasp sanoissa lka 5
     //if mysis=''then   mysis:='ii' else mySIS:=mySIS[1]+mySIS;  //loppuii vierasp sanoissa lka 5
     if curlka.kot=19 then begin if mid<>'' then if mid[1]='-' then begin delete(mid,1,1);delete(mysis,length(mysis),1);end; end else
     while (mid<>'')  and (mid[1]='-') do  begin delete(mid,1,1);if mysis='' then myav:=''  else delete(mysis,1,1); end;
     //gsana:=reversestring(myend)+string(mid+mysis+myav+sans[snum].san+sans[snum].akon);
     gsana:=(myend)+string(mid+mysis+myav+sans[snum].san+sans[snum].akon);
     if not sans[snum].takavok then gsana:=etu(gsana);
     resutaulu.add(gsana,snum,curlka.kot,sija,1);
     //if snum=8035 then
      pvok:='_';
      writeln(' ',reversestring(gsana),tavuluku(gsana,pvok));//,'  ',snum,':',myend,',',mid,',',vahvasija,',',myav,'!',luokka,'/',sija,'=',resutaulu.taulu[resutaulu.wcount-1].num);

   end;
  except writeln('FAILnom!!!',snum);end;
end;
var curlka:tlka;
begin    //hae kaikki yhden nominin sijamuodot resutaulu
  sija:=0;
  for lUOKKA:=0 to 49 do
  begin
     if not luokka in [14,17,25] then
    if lks[luokka].vikasana>=snum then
    begin
      curlka:=lks[luokka];
      vahvasija:=true;
      //if luokka<52 then
      begin
        for sis:=lks[lUOKka].ekasis to lks[lUOKka].vikasis do
        if siss[sis].vikasana>=snum then
         for AV:=SISS[SIS].ekaAV to SISS[SIS].VIKAAV do
          if avs[av].vikasana>=snum then
          //if avs[av].v<>avs[av].h then
          begin
           //writeln('<li>',luokka,sanoja[snum],';',curlka.kot,'.',reversestring(siss[sis].sis+'_'+avs[av].v+avs[av].h+'.'+sans[snum].san+sans[snum].akon),' ',luokka,curlka.vahva,siss[sis].sis,':');
           //for sija:=0 to 9 do //sikoja do
           writeln('<li>:',curlka.kot);
           for sija:=0 to  32 do //in [0,5,12,13,16,23,36,37,39,45] do //sikoja do
           //if (all) or (not (sija in [3,4,6..12,15..20,22,23,24])) then
//           if (not (sija in [13,14,17])) then
              sijaa(sija,curlka,siss[sis],avs[av],sans[snum]);
              //function sijaa(curlka:tlka;cursis:tsis;curav:tav;cursan:tsan):ansistring;
            exit;
          end;
          exit;
        end;
      end;
         //for SAN:=AVS[AV].ekasana to avs[av].VIKAsana do
     //writeln('<hr>');
    end;
  end;
procedure tsanasto.addtolist(sana:ansistring);
begin
 resutaulu.add(sana,9999,99,1,3);
 //resutaulu.
 //listaa;
end;

procedure tsanasto.generatelist(wlist:tlist;all:boolean);
var j,snum:integer;  parts:tstringlist;
begin
  for j:=0 to wlist.count-1 do
  begin
    snum:=integer(wlist[j]);
    if (snum<0) or (snum>65000) then continue;
    if snum<19547 then
     generatenom(snum,false)
    else if snum<25484 then
     generateverb(snum)
    else
    begin
       resutaulu.add(reversestring(slist[snum]),snum,99,1,3);
    end;
  end;
  //parts:=tstringlist.create; parts.loadfromfile('partikkelit.lst'); for j:=0 to parts.count-1 do  resutaulu.add(string(parts[j]),60000,99,0,3);
end;


procedure tsanasto.tutkimuodot;
var tavut,sijatavut:tstringlist;
   sijatavux:array[0..48] of array[0..33] of byte;
 function cell(sa,si,ref,lu:word):integer;
 var  w,color:string;tc:word;eritavu:boolean;
 begin
  try
 w:=generateone(sa,si);
 if w='' then exit;
 hyphenfi(w,tavut);
 tc:=tavut.count;
      if lu<33 then color:=ifs(nominit.sijat[si].vv,'green','red');
      if lu>32 then color:=ifs(nominit.sijat[si].hv,'green','red');
      if si=0 then if tc=0 then write('OUTO:',sa);
      //if si=5 then if lu=1 then write(' %',w,' ',tavut.commatext,tc-ref);
 //if si<>0 then if tc-ref+1<>sijatavux[lu,si] then writeln('<td>',SI,'\',tc-ref,' ',tavut.commatext,' <b>',sans[sa].av.v  ,sans[sa].av.h  '</td>');
      //if tc-ref+1<>sijatavux[lu,si] then
    eritavu:=tc-ref+1<>sijatavux[lu,si];
    if si<>0  then
    if eritavu then
    //  if (avs[sans[sa].av].v='k') and (avs[sans[sa].av].h='') then
    // if pos(sans[sa].san[1],konsonantit)<1 then
    if avs[sans[sa].av].v+avs[sans[sa].av].h='k' then
    begin
       writeln('<span style="color:',color,'">',w,'</span>', ' ',sans[sa].san[1],'/',nominit.lmmids[lu,si]);
       write('[<b>',siss[avs[sans[sa].av].sis].sis,'</b>]');
       //write(,'[<b>',avs[sans[sa].av].v,'.',avs[sans[sa].av].h,'</b>]');

   end;
   // ,ifs(nominit.sijat[si});
   // else if avs[sans[sa].av].v +avs[sans[sa].av].h='k' then writeln(si,w,sijatavux[lu,si]);
  //writeln(' [',reversestring(siss[avs[sans[sa].av].sis].sis  +'*'+sans[sa].san+sans[sa].akon)+'] ',w,' <b>',avs[sans[sa].av].v  ,avs[sans[sa].av].h, '</b>',sa);// else if avs[sans[sa].av].v +avs[sans[sa].av].h='k' then writeln(si,w,sijatavux[lu,si]);
                         //  siss[sans[sa].av].sis
 if (tc-ref+1>=0)  and (tc-ref+1<10) then inc(tcounts[lu,si,tc-ref+1]);// else writeln('%%%%',w);
 result:=tc;
 except write(' fail!!!!',lu,'.',si,'/',tc,'-',ref,w,'/');end;
 end;
 var i,j,otto,tref,eka:word; oli,olisan,avk:boolean;lu,vo,av,san:word;
    oliav:array[0..4] of word;
    avst,allav,alleka,alsan:string;  avtype,mak,makv,nonz:word;
begin
 //fillchar(tcounts,sizeof(tcounts),0);
 tavut:=tstringlist.create;
 sijatavut:=tstringlist.create;
 sijatavut.loadfromfile('sijatavut.x');
 writeln('<li>tutkimuodot');
 for i:=0 to sijatavut.count-1 do
 begin
   writeln('<li>');
   for j:=1 to 33 do sijatavux[i+1,j]:=strtointdef(sijatavut[i][j],9);
   for j:=1 to 33 do write(sijatavux[i+1,j]);
  end;
 //writeln('kokosis:',siss[avs[sans[652].av].sis].sis,'!');
 //for j:=1 to 33 do  writeln(' ',generateone(652,j));
 //for j:=1 to 33 do  writeln(' ',generateone(27393,j));
  //writeln('<h1>TUTKI</h1><table border="1">');
  eka:=1;
  for i:=1 to 48 do
  //for i:=2 to 2 do
  begin      // if i>2 then continue;
    writeln('<li>lka:',i);
    //writeln('<tr>');//<td>',i,' ',generateone(lks[i].vikasana,0),tutkimuodot(),'</td>');
    //writeln('<tr><td>',i,'</td>');//<td>',i,' ',generateone(lks[i].vikasana,0),tutkimuodot(),'</td>');

    for otto:=eka to lks[i].vikasana do
    begin
      //writeln('<li>');
      tref:=cell(otto,0,0,i);
      for j:=1 to 33 do cell(otto,j,tref,i);
      //writeln('</tr>');
    end;
    eka:=lks[i].vikasana+1;
    //Writeln('<tr>');//<tr><td>',i+1,'</td>');//<td>',i,' ',generateone(lks[i].vikasana,0),tutkimuodot(),'</td>');
    //tref:=cell(lks[i].vikasana+1,0,0,i);
    //for j:=1 to 33 do cell(lks[i].vikasana+1,j,tref,i);
    //for j:=1 to 33 do writeln('<td>',generateone(lks[i].vikasana,j),'</td>');
    //writeln('</tr><tr><td>',i+1,' ',generateone(lks[i].vikasana+1,0),'</td>');
    //for j:=1 to 33 do cell(i,j,tref);
    //for j:=1 to 33 do writeln('<td>',generateone(lks[i].vikasana+1,j),'</td>');
    //writeln('</tr>');
  end;
 //writeln('</table>');
  writeln('"""""""""""<table border=1><tr><td></td>');
  for j:=1 to 33 do  writeln('<td>',nominit.sijat[j].esim,'<br>',nominit.sijat[j].ending,'</td>');
  for i:=999 to 48 do
  begin
     writeln('</tr><tr><td>',i,lks[i].esim,'</td>');
     for j:=1 to 33 do
     begin
      write('<td>');
      oli:=false;
      makv:=0;mak:=0;nonz:=0;
      for otto:=0 to 10 do if tcounts[i,j,otto]>0 then inc(nonz);
      if nonz>1 then for otto:=1 to 10 do if tcounts[i,j,otto]>0 then begin writeln(otto-1,'/',tcounts[i,j,otto],' ');if oli then write('.');oli:=true;end;
      write('</td>');
     end;
     writeln('</tr>');
  end;
    writeln('</table>maxes');
    //for j:=0 to 10 do writeln(j,'/',tcounts[1,5,j],' ');
    for i:=1 to 48 do
    //for i:=2 to 2 do
    begin
      writeln('<li>');//,lks[i].esim,' ');//,nominit.sijat[j].ending,' ');
      for j:=1 to 33 do
      //for j:=5 to 5 do
       begin
         oli:=false;
         makv:=0;mak:=0;nonz:=0;
         //if i=2 then
          for otto:=0 to 10 do if tcounts[i,j,otto]>makv then begin mak:=otto;makv:=tcounts[i,j,otto];end;
         //for otto:=1 to 10 do begin write(' (',otto,'=',tcounts[i,j,otto],'->',makv,'/',mak,')'); if tcounts[i,j,otto]>makv then begin mak:=otto;makv:=j;end;end;
         //write(' ',mak-1,reversestring(nominit.sijat[j].ending),makv,'.');
         write(mak);
    end;
    end;
    for lu:=1 to 48 do
    begin
         writeln('<li>',lu,lks[lu].esim,'<ul>');
         for vo:=lks[lu].ekasis to lks[lu].vikasis do
         begin
              writeln('<li>',siss[vo].sis,' ',avs[siss[vo].vikaav].vikasana-avs[siss[vo].ekaav].ekasana+1);
              continue;
              writeln('<li>',siss[vo].sis,'<ul><li>');
              writeln('<li>',siss[vo].sis);//,'<ul><li>');
              for i:=0 to 4 do oliav[i]:=0;
              for av:=siss[vo].ekaav to siss[vo].vikaav do
              begin                      //a,,  =0tyhjät  a,x,=1eivaihdu  t,d=2aitoav  ,,k=3katoava kon
                 avst:=avs[av].v+','+avs[av].h;
                 avtype:=8;
                if avs[av].v='' then
                begin if avs[av].h='' then avtype:=0 else  avtype:=4;  // ei pitäis olla--vahva='' / heikko jotain
                end
                else begin if avs[av].h='' then avtype:=3 else if avs[av].v=avs[av].h then avtype:=1
                else  avtype:=2;
                end;
                //if (length(avst)=3) then continue;
                //if length(avst)=2 then if avst[1]=avst[2] then avk:=true;
                if  oliav[avtype]=0 then
                 write('<li><b>(',avst,')</b>') else continue;
                 if avtype>3 then writeln('cccccccccccccccccc');
                 inc(oliav[avtype]);
                 //write(' #',avs[av].v,avs[av].h,avtype,'/',oliav[avtype]);
                 //if pos(',',allav) then oliav:=true else allav:=allav+avs[av].v+avs[av].h;
                  // writeln('<li>',avs[av].v,'/',avs[av].h,' :');
                  alsan:='';
                  writeln(reversestring(sans[avs[av].ekasana].san+sans[avs[av].ekasana].akon),' :');
                  writeln(reversestring(sans[avs[av].vikasana].san+sans[avs[av].vikasana].akon),' :');
                  for i:=avs[av].ekasana to avs[av].vikasana do if pos(sans[i].san[1],alsan)<1 then alsan:=alsan+sans[i].san[1]+'.';
                   write(alsan);
              end;
              writeln('</ul>');
         end;
         writeln('</ul>');
    end;
    writeln('</ul>');

end;

constructor tsanasto.create;
var i,j,tc1,tcj:word;h:thakunen;
   hakusanat:tstringlist;
begin
hakusanat:=tstringlist.create;
//hakusanat.loadfromfile('haku.lst');
hakusanat.loadfromfile('vtest.tmp');
//hakusanat.loadfromfile('ntest.lst');
resutaulu:=tsinfotaulu.create(65535);
writeln('kaavas.LST');
luesanat('kaavas.lst');
writeln('LUETTU kaavat.csv');
//readadjbin;
verbit:=tverbit.create('kaavas.lst','vmids.csv','vsijat.csv');
writeln('<li>verbikaavat luettu');
nominit:=tnominit.create('nomkaavat.csv','nmids.csv'); //'nomsall.csv'
writeln('<li>nominikaavat luettu');
//function generateone(luokka:tlka;sana:tsan;muoto:tsija):string;
luomuodot;
luokaavat;exit;
tutkimuodot;
exit;
parsewend('');exit;
//haenumero('sanoa',false,false);
//generatenom(2000,false);
for i:=199 to 48 do
 //if (nominit.lmmids[i,13]<>'!') then //nominit.lmmids[i,25]) then
 if (pos('!',nominit.lmmids[i,28])<1) then
begin
   writeln('<li>',i,':',nexamples[i],',(<b>',
  reversestring(nominit.lmmids[i,13]),')en</b> (',
  //reversestring(nominit.lmmids[i,25]),')en',' (',
  reversestring(nominit.lmmids[i,26]),')iden',' (',
  reversestring(nominit.lmmids[i,27]),'itten)',' (',
  reversestring(nominit.lmmids[i,28]),')in',' (',
  reversestring(nominit.lmmids[i,29]),')ten');
  //reversestring(nominit.lmmids[i,29])')ten'
end;
writeln('<ul>');

for i:=199 to 48 do
begin
  generatenom(lks[i].vikasana,false);
  generatenom(lks[i].vikasana+1,false);
end;//exit; generateverb(30000);
try
for i:=1 to 48 do
begin
   writeln('<li>',i,lks[i].esim,':', lks[i].vikasis-lks[i].ekasis+1,'/',lks[i].vikasana-lks[i-1].vikasana+1, ': ');
   for j:=siss[lks[i].ekasis].ekaav to siss[lks[i].vikasis].vikaav do
     if avs[j].v=avs[j].h then write('.') else write(avs[j].v,avs[j].h,' ');
  write(avs[siss[lks[i].vikasis].vikaav].h,avs[siss[lks[i].vikasis].vikaav].v,'<b>: ');
   for j:=lks[i].ekasis to lks[i].vikasis do write(siss[j].sis,'/ ' );
   write('</b>');
end;
writeln('<hr>');
for i:=52 to 78 do
begin
   writeln('<li>',i,lks[i].esim,':', lks[i].vikasis-lks[i].ekasis+1,'/',lks[i].vikasana-lks[i-1].vikasana+1, ': ');
   for j:=siss[lks[i].ekasis].ekaav to siss[lks[i].vikasis].vikaav do
     if avs[j].v=avs[j].h then write('.') else write(avs[j].v,avs[j].h,' ');
  write(avs[siss[lks[i].vikasis].vikaav].h,avs[siss[lks[i].vikasis].vikaav].v,'<b>: ');
   for j:=lks[i].ekasis to lks[i].vikasis do write(siss[j].sis,'/ ' );
   write('</b>');
end;
//exit;
for i:=0 to hakusanat.count-2 do
 if hakusanat[i]<>'' then if hakusanat[i]='###' then break else haenumero(hakusanat[i],false,false);
except writeln('failhaku',i);end;
//listaa;
end;


procedure tsanasto.luesanat(fn:string);
  var sl:tstringlist;ms:tsija;sanafile:textfile;sana:ansistring;
  i,j,k,l:word;differ:byte;
  prevsl:tstringlist;
  clka,csis,cav,csan,ulka:integer;
  prlim:word;
  procedure uusav;
    begin
     try
      avs[cav].vikasana:=csan;
      //writeln('<li>','<b>av:',avs[cav].v,avs[cav].h,'</b>','..',csan, '   ',sl[2],sl[3],' :::',clka,'/',csis,'/',cav,'/',csan);
      cav:=cav+1;
      avs[cav].ekasana:=csan+1;
      avs[cav].takia:=0;  //takavokaalisten määrä  .. ei käytetä vielä
      avs[cav].v:=sl[2];
      avs[cav].h:=sl[3];
      avs[cav].sis:=csis;
      if avs[cav].v='a' then avs[cav].v:=avs[cav].h  //"_" käytettiin aastevaihtelun puuttumisen merkkaamiseen. sorttautuu siistimmin
      else prlim:=0;
      //write(' >',avs[cav].v,avs[cav].h);
      EXCEPT WRITELN('fav:',cav,'/',SL.count,'/',length(avs));END;
  end;
  procedure uussis;
    begin
     TRY
    siss[csis].vikasana:=csan;
    siss[csis].vikaav:=cav;
    csis:=csis+1;
    siss[csis].ekaav:=cav+1;
    siss[csis].sis:=(sl[1]);
    siss[csis].lk:=clka;
    prlim:=0;
    //write(^J':',siss[csis].sis);
    EXCEPT WRITELN('fsis:',csis,'/',SL.count,'/',length(siss));END;
  end;
  procedure uusluokka;
    var kot:integer;
    begin
    try
       lks[clka].vikasis:=csis;
       lks[clka].vikasana:=csan;
       clka:=strtointdef(sl[0],99);
       //clka:=clka+1;
       //writeln('<li>LKA:',clka,sl.commatext);
       lks[clka].ekasis:=csis+1;
     //try lks[clka].esim:=sl[7];except lks[clka].esim:='x'+inttostr(sl.count)+'x';end;
     kot:=strtointdef(sl[0],99);//clka+51;
     lks[clka].kot:=kot;//clka+51;
     if (kot in vahvatverbiluokat+vahvatnominiluokat) then   lks[clka].vahva:=true else  lks[clka].vahva:=false;
     prlim:=0;
     //write(^J^J':',CLKA);
    except writeln('failreadlka');end;
  end;
//tverbit.luesana
var alko:boolean;
begin
   prlim:=0;
   alko:=true;
  clka:=0;csis:=0;cav:=0;csan:=0;
  writeln('lue ',fn);
  assign(sanafile,fn);//'verbsall.csv');
  reset(sanafile);
  sl:=tstringlist.create;
  prevsl:=tstringlist.create;
  prevsl.commatext:='1,x,v,h,0,x,x,x';
  // tämmönenkin toimii:  for i in vahvatverbiluokat+vahvatnominiluokat do  writeln(i);
  writeln('lue ',fn);
  while not eof(sanafile) do
  begin
     try
     readln(sanafile,sana);
     //WRITE(SANA);
     sl.commatext:=sana;
     ulka:=strtointdef(sl[0],0);
     //if ulka>58 then break;     //if ulka<52 then continue;     //if alko then begin prevsl.commatext:=sana;end;  //eka sana ei aloita luokkia
     differ:=4;
     if alko then differ:=0 else
     for i:=0 to sl.count-2 do if prevsl[i]<>sl[i] then begin differ:=i;break; end;
    if differ=3  then differ:=2;      // av-heikot erosivat mutta vahva oli sama. yhdistetään - AV ylipäätään oli eri
    if (differ<1) then uusluokka;
    if (differ<2) then  uussis;
    if (differ<4) then uusav;
     except writeln('fail:',DIFFER);end;
     TRY
     alko:=false;
         //if sl[4]='0' then avs[cav].takia:=csan+1; //pitäis olla sortattu vokaalisoinnun mukaan, eli ei tarttis laittaa  joka sanalle talteen
      csan:=csan+1;
      sans[csan].av:=cav;

    sans[csan].san:=(sl[5]);
    sans[csan].takavok:=sl[4]='0';
    sans[csan].akon:=(sl[6]);
    if sl[4]='0' then avs[cav].takia:=avs[cav].takia+1;  //lasketaan takavokaalisten määrää av-luokassa hakujen tehostamiseksi
    //csan:=csan+1;
    alko:=false;
    prevsl.commatext:=sana;  //tähän taas seuraavaa sanaa verrataan
    prlim:=prlim+1;  //just for debug to make managable listings .. not used
    except writeln('failav',differ,'@',csan,'/',length(sans));end;
   end;

    writeln('<li>Sanasto luettu ',fn,'  LKS:',CLKa,' /sis:',csis,' /av:',cav,' /w:',csan,'</h1>');
    for i:=990 to 78 do
    begin
       writeln('<li>',i, '>',lks[i].vikasana,'<ul>');
       for j:=lks[i].ekasis to lks[i].vikasis  do
       begin
          writeln('<li>',j,'si:<b>',siss[j].sis, '</b> >',siss[j].vikasana,'<ul>');
          for k:=siss[j].ekaav to siss[j].vikaav  do
          begin
             writeln('<li>',k,':av<b>',avs[k].av,avs[k].v,avs[k].h, '</b>>',avs[k].vikasana,'<ul>');
             writeln('<li>',avs[k].ekasana,reversestring(sans[avs[k].ekasana].san+sans[avs[k].ekasana].akon),' ',avs[k].vikasana,reversestring(sans[avs[k].vikasana].san+sans[avs[k].vikasana].akon));
              writeln('</ul>');
          end;
           writeln('</ul>');
       end;
       writeln('</ul>');
    end;
    for i:=1 to 49 do lks[i].esim:=nexamples[i];
    for i:=52 to 78 do lks[i].esim:=vesims[i-51];
 end;

procedure tsanasto.listaa;
  function b(st:string):string;
    begin result:='<b>'+st+'</b>';end;
var lu,sis,av,san:Integer;myav,mysis,mymid:string;
curlka:tlka;cursis:tsis;curav:tav;cursan:tsan;Skip,oliav:BOOLEAN;
tavusija, tavulksis,tavuav,tavusana:word;
tavutustila:char;//  tavun alussa, vokaalirungossa pärs-kei-tä  ai-o-taan
  function tavuluku(st:string;var olivok:char):word;
  var i:word;tulos:string;raja:boolean;
  begin
    result:=0;
   // writeln('<li>',st,': ');
    //st:=reversestring(st);
    tulos:='';
    for i:=1 to length(st) do
    begin
     raja:=false;
     if (olivok<>'_') then
     begin
          if (pos(st[i],konsonantit)>0)then
          begin
            result:=result+1;olivok:='_';raja:=true;
            //tulos:='-'+st[i]+tulos;

          end else  //vokaalin edellä vokaali
          if (not isdifto(st[i],olivok)) then
          begin //tavuraja vokaalien välillä
            olivok:=st[i];result:=result+1;raja:=true;
            //tulos:=st[i]+'-'+tulos;
          end else  //diftongi
          begin
             olivok:=st[i];//raja:=true;
          end;
     end else //ed konsonantti, nyt toinen konsonantti ei tee mitään, vokaali on avoimen tavun.loppu
       if (pos(st[i],konsonantit)<1)then olivok:=st[i];
    //tulos:=st[i]+tulos;
    if not raja then tulos:=st[i]+tulos;
   // write(' ',st[i]);if raja then write('-');
   end;
   // write('=',tulos);
  end;
  var olivok:char;
begin
  writeln('<ul>');

  tavusija:=0; //later: muut sijat ssa=1 aksemme=2
  for lu:=0 to 78 do
  begin
    tavusija:=0;
     //tmp: listaa sanat joilla av merkattu tyhjäksi, ja ne joilla av.h = av.v
    curlka:=lks[lu];      //mymid:=lmmids[lu-1,1];
    try
    olivok:='_';
    if lu<50 then writeln('<li>:',lu,nominit.lmmids[lu,0],tavuluku(nominit.lmmids[lu,0],olivok ))
    else if lu>51 then writeln('<li>:',lu,verbit.lmmids[lu-52,0])  ;//,
    except write('!',lu); end;
    //B(curlka.ESIM),' ',mymid,curlka.kot,' ');//,curlka.ekasis,'...',curlka.vikasis,' ',curlka.vikasana);
   // writeln('<ul>');
   for  sis:=curlka.ekasis to curlka.vikasis do
   begin
     cursis:=siss[sis];
     mysis:=reversestring(cursis.sis);skip:=TRUE;
     if mysis<>'' then if pos(mysis[1],vokaalit)>0 then skip:=FALSE;//begin   writeln('<li>sis:',b(mysis+'.'));skip:=FALSE;end
    // else
    writeln('<large>[[',(mysis+']]</large>'));  //vain poltergeist ragout - poistetaan!  layout myös pois . AALOE
     if length(mysis)>1 then writeln('X');
      // 24/26 sekaisin .. vähän kaikilla
     // eli SIS[1] on aina vokaali tai tyhjä
     //     if (mysis='') or (pos(mysis[1],vokaalit)>0) then continue;

     //if lmmids[lu-1,1]='*' then begin mymid:=mysis[1]+'';end;// else    mymid:=lmmids[lu-1,1];
     //writeln('<ul>');
     oliav:=false;
    for av:=cursis.ekaav to cursis.vikaav do
    begin
      curav:=avs[av];
      if (lu<33)
      or ((lu>51) and (lu<66)) then myav:=curav.v else  myav:=curav.h;
      oliav:=true;
      if curav.h+curav.v='' then begin write('|||');oliav:=false;end else
      IF CURAV.H=curav.v then begin write(' <b>',curav.v,'</b>'); oliav:=true;end
       else
       writeln(' <b style="COLOR:RED">:',curav.v,'|',curav.h,'</b>');//,myav,':');  //1-
       //if curav.h<>curav.v then
 //      if curav.h+curav.v<>'' then continue;
      //if skip then continue;
//      writeln('<li>',b(curav.v+curav.h),' ',curav.ekasana,'...',curav.vikasana,' ',sans[curav.vikasana].san,sans[curav.vikasana].akon,' ',curav.vikasana);
      if (curav.h<>'') then continue;
      write('-');
      for san:=curav.ekasana to min(curav.ekasana+15,curav.vikasana) do //write(' ',reversestring(mymid+mysis+''+myav+''+sanat[san].san+sanat[san].akon)+'a');
          if not oliav then write(' <b style="color:green">',sans[san].akon,reversestring(sans[san].san),'</b> ') else
         write(' ',sans[san].akon,reversestring(sans[san].san));
    end;
    writeln('</ul>');
   end;
   writeln('</ul>');
  end;
  writeln('</ul>');
end;

function listsija(sika:tsija):string;
begin
   with sika do  result:=' [['+inttostr(num)+' '+ name+' ('+esim+') '+inttostr(vparad)+' '+inttostr(hparad)+'):<b>'+ending+'</b>]] ';
end;

function tsanasto.etsiyks(hakusana,hakuakon,hakukoko:string;hakueietu,hakueitaka:boolean;sika:tsija;
  aresu:tstringlist;onjolist:tlist;var hits:word;d:boolean):word;
var sanasopi,sanajatko,avsopi,avjatko,sissopi,sisjatko,lkasopi,lkajatko,sikaloppu,sikasopi:string;
    curlka:tlka;cursis:tsis;curav:tav;cursan:tsan;myav,mysis:string;
    resunum:word;//mysika:tsija;
    tc,hakutc:integer;
    luokka:word;//xskipped:string;
     heikkomuoto:boolean;
     resst:string;
     isverb:boolean;
     vokvex,vokdbl,konvex,kondbl,difmuu:boolean;
     //eietu,eitaka:boolean;
      function red(st:string):string;
        begin result:='<b style="color:red">'+st+'</b>';
        end;
      function blue(st:string):string;
        begin result:='<b style="color:blue">'+st+'</b>';
        end;
      //function voksana(w:string;tak:booleen);      begin if tak thenend;
      function sijanheikkous(isv:boolean;sika:integer):boolean;
      begin //huikko:=true
      result:=false;
      if isverb then
        if curlka.vahva then begin if  sika in vvahvanheikot then result:=true;end
        else  if  sika in vheikonheikot then result:=true;
      if not isverb then
        if curlka.vahva then begin if  not (sika in nvahvanvahvat) then result:=true;end  //vv hh
      else  if  sika in nheikonheikot then result:=true;
      //writeln('<li>heikko? ',
      end;

      procedure savehit(yht,sanax,hakux:string;sa:integer);
      var thishit:thit;sana,color:string;
      begin
        sana:=sans[sa].san;
        thishit:=thit.Create;
        with thishit.sana do
        begin
          alku:=cursan.san;
          akon:=cursan.akon;
          v:=curav.v;
          h:=curav.h;
          sis:=cursis.sis;
          luokka:=curlka.kot;
          if isverb  then  sloppu:=verbit.lmmids[luokka-52,0] else   sloppu:=nominit.lmmids[luokka,0];
          takvok:=not hakueitaka;
          sananum:=sa;   if d then writeln('xxx:',alku,'.',akon,'.',v,h,'.',sis,'.',luokka);
        end;
        //hitlist.add(thishit);
      end;

      function sana_f(san:integer;koita,skipped:string):boolean;
      var i,j:word;hakujatko,sana,kokos:string;yhtlen,slen,hlen:word;//osataka,osaetu:boolean;
      begin
      try
       try // write('.');
         //try
       //d:=false;
      //if skipped<>'' then write
       result:=false;
      cursan:=sans[san];
      sana:=cursan.san;
      //if san=24567 then
      //if d=d then writeln(sana);
      TRY
       //if   curlka.kot=60 then writeln('<li>:::',luokka, blue(sana+'|'+cursan.akon),'/',red(koita+'|'+hakuakon+'!'));
       //,(skipped+sana+cursan.akon=koita+hakuakon),'/ ',kondbl,konvex,' ',hakusana,(skipped+sana+cursan.akon)<>(koita+hakuakon),'/',san);
       //,'/taka:',cursan.takavok,' /ä:',hakueitaka,'/a:',hakueietu,(cursan.takavok) and (hakueitaka),skipped,']',cursan.takavok);
       EXCEPT WRITE('<H1>xXXX</H1>');END;
       if cursan.takavok then begin if hakueitaka then exit;end else if hakueietu then exit;
        //  lypsää                      lup true
     //LYPSÄÄ lupsaa  cursan.takavok,hakueitaka,hakueietu FAL FAL TRU  lupsaa ei salli etu (äöy)
       if d then if konvex then writeln('<li>::konvex:',luokka,'#',sika.num,sika.ending, blue(sana+'|'+cursan.akon),'/',red(koita+'|'+hakuakon+'!'));

        if konvex then
          if curlka.kot=60 then
           delete(sana,1,1)  //vain "lähteä" monikot läksin
        ;// else begin konvex:=false; sana:=sana[1]+sana;if d=d then  writeln('/DBL:',red(koita),blue(sana+'.'+cursan.akon),konvex,'/'); end; //vain "lähteä" monikot läksin
      kokos:=cursan.akon+reversestring(sikasopi+lkasopi+''+sissopi+''+myav+''+sana);
      if not cursan.takavok then kokos:=etu(kokos);
      if d then writeln('/SN:',red(koita),blue(sana+'.'+cursan.akon)+'/',skipped,'?');
      if sana+cursan.akon=koita+hakuakon then //if cursan.akon=hakuakon then
      begin
         //fullhit(sana,koita,san);
         hits:=hits+1;
         result:=true;
          if d=d then writeln('<em style="color:green">+',reversestring(sikasopi+lkasopi+''+sissopi+''+avsopi+''+sana+cursan.akon) ,san,' ',sika.num,'</em> ');
          //write(' <b style="color:green">',reversestring(sikasopi+lkasopi+''+sissopi+''+avsopi+''+sana+cursan.akon),'</b>');
          resst:=resst+'+++'+(reversestring(sikasopi+lkasopi+''+sissopi+''+avsopi+''+sana+cursan.akon)+inttostr(san));
          //resst:=resst+' <b style="color:red">['+kokos+']</b>';
          hitcount:=hitcount+1;
          hits:=hits+1;
           hitlist[hitcount]:=san;
           resunum:=san;
         //resst:=' ';
         //writeln('HIT,exiting for now');
         exit;
      end;
         //try
      //if pos(sana,koita)=1 then writeln('<li>short?',curlka.kot,red(cursan.akon+'.'+reversestring(cursan.san)),'/',blue(koita));
      if (skipped+sana+cursan.akon=koita+hakuakon) // then exit;
      or (skipped+sana+cursan.akon='') or // (sana='') and          uull  amdma
       ((pos(skipped+sana+cursan.akon,koita+hakuakon)=1) ) then
      begin          //sikasopi+''+ lkasopi+'' +sissopi+''+avsopi+''+sans[sa].san+sans[sa].akon
          //kokos:=sikasopi+lkasopi+''+sissopi+''+avsopi+''+sana+cursan.akon;
          begin
            if not cursan.takavok then kokos:=etu(kokos);
            //if ((cursan.takavok) and (hakueitaka)) or (not(cursan.takavok) and (hakueietu)) then
            begin  ///etutakahässäkkä:kiirastorstai/t/                                                   eit:FALSE         /eie:TRUE         /t:FALSE kiirastorstai=torstai 1593!2
             if d then writeln('<li>ET:'+reversestring(hakusana),'/',reversestring(sana),cursan.akon,'/eit:',hakueitaka,'/eie:',hakueietu,'/t:',cursan.takavok
             , ' <b>'+reversestring(skipped+sana+cursan.akon)+'!='+reversestring(koita+'.'+hakuakon),'\</b>',san,kokos,'???',hakukoko );
              //if cursan.takavok then for i:=1 to length(kokos) do if pos(hakukoko[i+length(kokos)],'äöy')>0 then exit;
              //if (cursan.takavok) then for i:=0 to length(kokos)-1 do if pos(hakukoko[length(hakukoko)-i],'äöy')>0 then exit;// else write(length(hakukoko)-i,hakukoko[length(hakukoko)-i]);
              //if not(cursan.takavok) then for i:=0 to length(kokos)-1 do if pos(hakukoko[length(hakukoko)-i],'aou')>0 then exit;// else write(length(hakukoko)-i,hakukoko[length(hakukoko)-i]);
               if d then        writeln('***</li>')
            end;// else hits:=hits+1;
            if (skipped+sana+cursan.akon<>koita+hakuakon)  then
             hits:=hits+0 //   mitä vittua
            else hits:=hits+0;
          end;// else hits:=hits+1;
          //if hits>0 then resst:=resst+'(! <b style="color:purple"> '+kokos+'!</b>!)';//+'"/'+copy(hakusana+hakuakon,length(kokosana)+1)+'/s:'+inttostr(san)+' '+inttostr(luokka)+'#'+inttostr(sika.num)+'['+skipped+']';
      end;
      //write('=',cursan.akon,'+',reversestring(sana));
      if (koita='') or (pos(koita,sana)=1) then //writeln(      //'<li>longhit '       ,'/',skipped+'-'+sana+'   '+cursis.sis,'.',curav.v,curav.h,'_',cursan.san,cursan.akon,' #',san,'  ',reversestring(cursis.sis+'.'+curav.v+curav.h+'_'+cursan.san+cursan.akon))
       begin
       //write(' ,<b>',reversestring(sikasopi+lkasopi+''+sissopi+''+myav+''+sana+cursan.akon),'</b>' );
        //resst:=resst+' ('+cursan.akon+reversestring(sikasopi+lkasopi+''+sissopi+''+myav+''+sana)+'?)';
        //writeln('<b style="color:brown"> ('+ifs(cursan.takavok,kokos,etu(kokos))+'!?</b>)',mysis,'_',myav,'!');
        //+'"/'+copy(hakusana+hakuakon,length(kokosana)+1)+'/s:'+inttostr(san)+' '+inttostr(luokka)+'#'+inttostr(sika.num)+'['+skipped+']';
   //if d then
   resst:=resst+' ('+kokos+'?)';
       // writeln('****',kokos,'***');
        //RIIMIHAUSSA LATETAAN PÄÄLLE, nyt vain tarkka haku  result:=true;
       end;
      ;//else if cursan.san='' then
      ;//writeln('<span  style="color:green">%%%','/<em><b style="color:blue">',
      //reversestring(sikasopi+''+lkasopi+''+sissopi+''+avsopi+''+sans[san].san+sans[san].akon),'/</b></em>',san,'</span>')//,kokohaku[2]);
      //else //if curlka.kot=9 then
      //writeln('<b style="color:purple">',
      ;//reversestring(sikasopi+''+lkasopi+''+sissopi+''+avsopi+''+sans[san].san+sans[san].akon),'/</b></em>',san,'</span>');//,kokohaku[2]);
       except writeln('<li>!failsanax',san);raise;  end;
       finally writeln('');
       END;
      end;

      function av_f(av:integer;koita,skipped:string):boolean;
      var san,i,j:integer;//
           procedure x;begin if d then write('!');end;
      begin
       try if d then writeln('<ul>');
       try
        //d:=curlka.vikasana=16256;
        //if skipped<>'' then writeln('<li>skipav:',skipped);
        result:=false;;
        curav:=avs[av];
        //d:=(curlka.kot=67) and (curav.v='l');
        myav:=ifs(heikkomuoto,curav.h,curav.v);
        if d  then   writeln('<li>AV:{',blue(myav),'}',curav.v,'/',curav.h,curlka.kot,konvex,red('try:'+koita));//+xvok),blue(myav),' :',curav.v,curav.h,'/',curav.ekasana,'-',curav.vikasana);
        //if d then writeln('<li>(',myav,'=',curav.v+'/',curav.h,'\',konvex,')',red(koita),blue(myav),'/hm:',heikkomuoto,'/ver:',isverb,'/vah:',curlka.vahva);
        if konvex then if myav<>'' then begin if curlka.kot<>60 then konvex:=konvex;writeln('');delete(myav,1,1);end;
        //vitun LÄHDIN LÄKSI
        if koita='' then begin avsopi:='';skipped:=skipped+myav;avjatko:='';if d then writeln('<li>AVX:',red(koita),blue(myav)) end;// else
        begin
          //write('#');
          if (myav='') or (koita='') or  (pos(myav,koita)=1)  then  //result:=true
          else  begin  write('');if pos(myav,koita)=1 then if d then writeln('<li>AVLOPPU:',koita,myav);  exit;
                end;


          avjatko:=copy(koita,length(myav)+1,99);//xvok;
          avsopi:=copy(koita,1,length(myav));//xvok;
        end;
        //avsopi:=myav;

        if d then writeln('/AV:',myav,red(avjatko),blue(avsopi),' ',curav.ekasana,'..',curav.vikasana,heikkomuoto,' ');
          //if d then  for san:=curav.ekasana to min(curav.vikasana,curav.ekasana+50) do write('/',sanat[san].san);
           //writeln('?',result);
          for san:=curav.ekasana to curav.vikasana do //!!!
                if sana_f(san,avjatko,skipped) then begin result:=true;end;// else write('z');
        except writeln('faILAV!!!');end;
       finally if d then writeln('</ul>');end;
      end;

      function sis_f(sis:integer;koita,skipped:string):boolean;
      var av,i,j:integer;//mysis:string;
      begin
        try
        try
        if d then writeln('<ul>');
        //  if skipped<>'' then writeln('<li>skipsis:',skipped);
        result:=false;
        cursis:=siss[sis];
        mysis:=cursis.sis;
         if kondbl then if curlka.kot=67 then mysis:=MYSIS[1]+MYSIS;// else mysis:=MYSIS[1]+MYSIS;
        //if curlka.kot=37 then mysis:=mysis[2]; //VIRHE NMIDS korjaa tiedostoon, ei tänne koodiin
        if vokdbl then if mysis='' then mysis:='ii' else MYSIS:=MYSIS[1]+MYSIS;  //loppuii vierasp sanoissa lka 5

        //writeln('<li> ', curlka.kot,' ',sis,cursis.sis,'#',cursis.ekaav,'-',cursis.vikaav);
        if koita='' then BEGIN sisjatko:='';skipped:=skipped+mysis; sissopi:='';sisjatko:='';END     //sijapääte oli pidempi kuin hakusana
        ELSE
        begin
          //while (cut>0) and (mysis<>'') {and(koita<>'')} do
          if difmuu then
           delete(mysis,2,1);
         if vokvex then
         begin
            delete(mysis,1,1);
             //if curlka.kot in [19,64] then
             //if d then write('<li>?/',koita,'/',mysis,vokvex);
            //delete(mysis,1,1);
          END;
          //if cutvok=2 then  begin mysis:=mysis[1];END;

          if (mysis='') or (pos(mysis,koita)=1) then //begin writeln('<li>SHORTHIT:',curlka.kot,'/',red(koita),'|',blue(mysis));result:=true;end
          else
          begin  if pos(koita,mysis)=1 then if d then writeln('<li>EOW:',curlka.kot,'/',red(koita),'|',blue(mysis));  exit;
          end;
          sisjatko:=copy(koita,length(mysis)+1,99);
          sissopi:=copy(koita,1,length(mysis));
          //writeln(red(koita),'qqq');
          //if sisjatko='' then writeln('<hr>','<hr>');
          //sissopi:=mysis;
        END;
         //if curlka.kot=60 then write('<li>?/',koita,'/',mysis,vokvex);
         if d then writeln('<li>SisHit:',red(koita),'/',blue(mysis+'!'),vokvex);
         if  d then   write('<b>/sovita:',sisjatko,'|sopi:',sissopi,'\</b>');
          //if d then for av:=cursis.ekaav to cursis.vikaav do write('[',avs[av].v,'.',avs[av].h,']');
          //if d then writeln('<ul>');
          //for av:=cursis.ekaav to cursis.vikaav do write('|',avs[av].v,avs[av].h);
           for av:=cursis.ekaav to cursis.vikaav do
         if av_f(av,sisjatko,skipped) then begin result:=true;end;
          //if d then writeln('</ul>');
          except writeln('failsis(',mysis,')/',red(koita),'/',blue(mysis+'/!'),curlka.kot);end;
       finally if d then writeln('</ul>');end;
      end;

      function lka_f(lka,sija:integer;koita,skipped:string;olivok:char;tavuc:byte):boolean;  //vain verbille
      var sisus,i,j:integer;mid:string;repeats:integer;
      begin
       try
       result:=false;
       //if skipped<>'' then
       //d:=lka=19;
      luokka:=lka;
       d:=true;
        d:=false;//lka=10;
        //if lka<>70 then exit;
        curlka:=lks[lka];
        isverb:=lka>51;
        repeats:=1;
        //if curlka.kot<>62 then exit;
        if isverb then
        begin
          if luokka=68 then repeats:=2;  //kaikilla 68'illa on kaksi taivutustapaa  aterioi aterioitsee
          if (luokka in [55,57,60]) then if sika.vparad=6 then repeats:=2;
          if (luokka in [76]) then if sika.vparad=5 then repeats:=2;
          if (luokka in [74,75]) then if sika.vparad=9 then repeats:=2;
        end;
        if (luokka=11) then if sika.num>13 then repeats:=2;
       repeat
        if isverb then
          mid:=verbit.lmmids[lka-52,sija] else
          mid:=nominit.lmmids[lka,sija];
         //if lka=36 then

         if mid='!' then exit;
        //dbl:=false;    eatkon:=false;

        vokvex:=false;vokdbl:=false;konvex:=false;kondbl:=false;  difmuu:=false;
        //if curlka.vahva then para:=sika.vparad else para:=mysika.hparad ;
        if (luokka=71) then
        { if ((sika.hparad=2) and  (sija>10)) then     //ifs(isverb,verbit.lmmids[lka-52,sija],nominit.lmmids[lka,sija])
        begin IF D THEN writeln('<li>!!!???');//mid:='k'
        end else    }
          if  (sija in [23,28]) then begin IF D THEN writeln('!!!--');mid:='k'; end;
        //aint it pretty?
        if isverb then
        begin    //lu 30 peistä peitseä?
        if luokka=68 then if repeats=2 then begin mid:=verbit.lmmids[10,sija];end;
        if repeats=2 then if luokka=55 then if sika.vparad=6 then  begin mid:='';end; //soutaa sousi
        if repeats=2 then if luokka=57 then if sika.vparad=6 then  begin mid:='o';end; //kaatoi kaasi
        if repeats=2 then if luokka=60 then if sika.vparad=6 then  begin mid:='sk';konvex:=true;end; //huom vain luokan 60  - vain "läksin läksit"
        if repeats=2 then if luokka=76 then if sika.vparad=5 then  begin mid:='*nn';;end;  //tiennen, tainnen
        if repeats=2 then if luokka in [74,75] then if sika.vparad=9 then  begin mid:='';end; //katketa nimetä katkeisi/katkeaisi
        if repeats=2 then if luokka in [74,75] then if sika.vparad=9 then  begin mid:='';end; //katketa nimetä katkeisi/katkeaisi
        end;
        //if lu=71 then if (sija in [23,28]) then mid:='-k';
        repeats:=repeats-1;
        //if (luokka=11) then  Writeln('<li>REPEAT',repeats);
        IF REPEATS=0 THEN if (luokka=11) then MID:=MID+'o';
        try
        if d then
        //if lka=60 then
         write('<li>TRY:',lka,'#',sija,' <b>[',repeats,blue(mid),'/',red(koita), '] </b>(');//,nominit.lmmids[lka,sija],')');//,curlka.kot,'!',isverb,verbit.lmmids[15,0]);
        except write('FAILSIKA[[[',sija,']]]');end;
        //result:=false;
        if (mid<>'') and (mid[1]='*') then
           if luokka=67 then begin delete(koita,1,0);delete(mid,1,1);kondbl:=true;end
           else begin write('');delete(mid,1,1);konvex:=true;end;

        if (mid<>'') and (mid[1]='_') then begin if d then write('????',mid);vokdbl:=true;delete(mid,1,1);end;
        if (mid<>'') and (length(koita)>0) then
         if (mid[1]='-') then
         begin vokvex:=true;delete(mid,1,1);delete(koita,1,0);; end
         else if mid[1]='#' then begin difmuu:=true;delete(mid,1,1);delete(koita,1,0); end;

         //av-konsonanttia ei vaadita kun on joku vakkari (s) tilalla
        if d then writeln(' DOTRY:',vokvex,lka,curlka.esim,' #',sija,' <b>(mid:',blue(mid),')/',//lmmids[lka,sija],
          '/</b>'    ,ifs(curlka.vahva,'v:'+inttostr(sika.vparad),'h'+inttostr(sika.vparad))
          ,'=',' koita::',red(koita),' / ', blue(mid),tavuluku(mid,olivok));//,vokvex,vokdbl,konvex,kondbl,'///',curlka.ekasis,'-',curlka.vikasis,'::',repeats,'');
        //exit;
        //writeln('--',lka);
        try
        if (mid='') OR (pos(mid,koita)=1) then //result:=true
        else
        begin
           if  (koita='') or (pos(koita,mid)=1) then
            begin  if d then writeln('//EOW:',koita,mid);
               skipped:=skipped+copy(mid,length(koita)+1);
               koita:='';
            end else
            begin  if d then write('/ei:',blue(koita),red(mid));
             continue;
           end;
        end;
        except writeln('faillka-koita::',lka,'#',sija,sika.onverbi);end;
        if d then for sisus:=curlka.ekasis to curlka.vikasis do writeln('!!!!',siss[sisus].sis);
        lkajatko:=copy(koita,length(mid)+1,99);
        lkasopi:=copy(koita,1,length(mid));
        heikkomuoto:=sijanheikkous(isverb,sija);
        if d then   writeln('<ul>');
        for sisus:=curlka.ekasis to curlka.vikasis do
         if sis_f(sisus,lkajatko,skipped) then begin result:=true;
          //writeln('<b style="color:red">(',curlka.kot,')</b>') ;
         end;

        if result then if d then write(' <span style="color:green">',lka,resst,'!?!</span>');
        if d then writeln('</ul>');
       until repeats<=0;
       except writeln('faillka:',lka,'#',sija,sika.onverbi);end;
      end;

      //ETSIYKS(
var i,j,lu,tavuc:word;olivok:char;
 begin
   try
  try
  //writeln('<h1>',lks[19].vikasana,'****</h1>');exit;
   //for i:=1 to 50000 do write(sans[i].san,' '); exit;
   //writeln('XXXXXXXXXXXXXXX ',hakusana,' ',sika.name,sika.num,d);
  // listaa;exit;
  //d:=false;
  result:=100;
  //writeln('<li>sika;',sika.num,'__');
  //if sika.num<>13 then exit;

  resunum:=0;
  //d:=true;  write(d);
  //for i:=1 to jotain do
  //voksointu(hakusana,eietu,eitaka);
  //for i:=1 to 27 do write('<li>',i,sans[i].san,sans[i].takavok);  exit;
  //write('nnnnnnnnnnnnnnnnnnnnnnnnnn');
  sikaloppu:= copy(hakusana,length(sika.ending)+1);
  sikasopi:= sika.ending;
  resst:='';
  //writeln('<hr><li><b>ETSI:',hakusana,hakuakon,'</b>','[',sika.ending,']',sika.name,' ',sika.esim,'  :: ');
  //if d then writeln('<li><li>HAE:',reversestring(hakusana),'-',sika.ending,sika.num,': ');//,'..[',sika.ending,']',sika.num,'@',sika.onverbi,'');
  if sika.onverbi then
   begin
      if sika.num=0 then sikasopi:='a';
      for lu:=52 to 78 do
       begin
         try
         //if d then write('+[',lu,']');
         olivok:='_';
         tavuc:=tavuluku(sika.ending,olivok);
         //if d then writeln('#(((',lu,'#',sika.num,sika.ending,tavuc,'))):');
         if lka_f(lu,sika.num,sikaloppu,'',olivok,tavuc) then
          begin
            aresu.add(sika.name+inttostr(lu)+resst);
            resst:='';
            result:=lu;
            //if d=d then writeln('!HIT!',lu,aresu.text);continue;result:=resunum;break;
          end;// else writeln('<h2>!!'+hakusana+'</h2>');
         if d then write('{/',lu,'}');
         except writeln('VERBITOHIä'); end;
       end;
     end
  else
  for lu:=1 to 49 do
   begin     exit;
      //if lu<>11 then continue;
       olivok:='_';
       tavuc:=tavuluku(sika.ending,olivok);
       if d then writeln('#',lu,'#',sika.num,sika.ending,tavuc,':');
       //if d then write(' [',lu,sika.ending,']');

      if lka_f(lu,sika.num,sikaloppu,'',olivok,tavuc) then
     begin
         result:=lu;
         //write('?<b style="color:green">',result,'</b>');
//         aresu.add(sika.name+resst);
         resst:='';
         exit;
      //if d=d then write('NHIT',lu);result:=resunum;if 1=0 then break;
     end; // skipped vois olls <> ''
   end;
  except writeln('<li>failetsi:', hakusana);end;
    finally if d then writeln ('</ul>',reversestring(hakusana),result);//result:=resunum;//writeln('<li>res:',resunum);
    end;
end; //etsi


function tsanasto.haenumero(sana:ANSISTRING;debug,rev:boolean):word;
var eitaka,eietu:boolean;akon,koko,loppu,kokosana:ansistring;aresu,haku:tstringlist;onjolista:tlist;
   hits,myhit:word;i,j:word;vfound,nfound:boolean;
begin
akon:='';
if rev then sana:=reversestring(sana);
kokosana:=sana;
aresu:=tstringlist.create;
voksointu(sana,eietu,eitaka);
if debug=debug then writeln(' <li><em>?',sana,'</em>');
koko:=sana;
while pos(sana[1],konsonantit)>0 do begin akon:=sana[1]+akon;   delete(sana,1,1); end;
loppu:=taka(reversestring(sana));
//continue;

//exit;
//resst:=resst+
//writeln(verbit.sanat[0].san);
//verbikama.
//writeln(verbit.sijat[10].ending+'!');
//writeln('numeroi;',sana,loppu+akon,koko,'',eietu,eitaka);//,verbit.vesims[1]);//,verbit.sijat[0],aresu,onjolista,hits);
try
  //myhit:=etsiyks(loppu+akon,koko,'',eietu,eitaka,verbit.sijat[0],aresu,onjolista,hits);
except writeln('!-!',reversestring(akon+sana),myhit,koko);end;
// function etsiyks(hakusana,hakuakon,hakukoko:string;hakueietu,hakueitaka:boolean;sika:tsija;aresu:tstringlist;onjolist:tlist;var hits:word):word;
  try

  //if myhit=0 then
  //for i:=0 to 66 do   writeln('<li>VERBISIJA',i,reversestring(verbit.sijat[i].ending),'.! ',pos(verbit.sijat[i].ending,loppu));
  nfound:=false;
  vfound:=false;
  debug:=true;
  //if debug then   writeln('<li>nominit:',debug);
  for i:=9990 to 33 do //33 do
   begin
      //if i<13 then continue;
      //write(i);continue;
        try
        //continue;
        //writeln(i,'/sija:',nominit.sijat[i].ending,'/ ');
        if (nominit.sijat[i].ending<>'') then if
         (pos(nominit.sijat[i].ending,loppu)<>1) then     continue;
          //if (nominit.sijat[i].ending<>'') and (pos(nominit.sijat[i].ending,loppu)<>1) then continue;
          //write(nominit.sijat[i].name,' ', nominit.sijat[i].ending,'\');
          //writeln('<hr><li><b>',kokosana,'</b>','[',nominit.sijat[i].ending,']',pos(nominit.sijat[i].ending,loppu),'.! ',nominit.sijat[i].name,' ',nominit.sijat[i].esim);
          //writeln('<li>HAESIJA',i,'<b>[',nominit.sijat[i].ending,']</b>',pos(nominit.sijat[i].ending,loppu),'.! ',nominit.sijat[i].name,' ',nominit.sijat[i].esim,'  ',loppu);
          debug:=true;
          myhit:=etsiyks (loppu,AkoN,'',eietu,eitaka,nominit.sijat[i],aresu,onjolista,hits,debug);
          //write('{',myhit,'}');
          if myhit<>100 then nfound:=true;
          //if nfound then writeln('+',myhit,' ') else writeln('<b style="color:red">-',sana,'</b>');
          if nfound then break;
        except writeln('<li>NFAIL??',reversestring(akon+sana),i);end;
       end;

   //writeln('<li>verbit:');
    for i:=0 to 65 do
    begin
         try
         //writeln('match:',i,'/',verbit.sijat[i].ending,'/',loppu,'?');
         //write('<li>',i,'  pos(',verbit.sijat[i].ending,',',loppu,')=',pos(verbit.sijat[i].ending,loppu), '}::',verbit.sijat[i].esim,' ',verbit.sijat[i].name);
         //if (length(verbit.sijat[i].ending)=0) then
         if (verbit.sijat[i].ending<>'') then if (pos(verbit.sijat[i].ending,loppu)<>1) then continue;
         //pos(verbit.sijat[i].ending,loppu)<>1) and (pos(loppu,verbit.sijat[i].ending)<>1) then continue;
        // write('<li>',i,'_',verbit.sijat[i].name,' ', verbit.sijat[i].ending,'\');

           //writeln('<hr><li><b>',loppu,kokosana,'</b>','[',verbit.sijat[i].ending,']'
           //,pos(verbit.sijat[i].ending,loppu),'.! ',verbit.sijat[i].name,' ',verbit.sijat[i].esim,'  :: ');
           myhit:=etsiyks (loppu,AkoN,'',eietu,eitaka,verbit.sijat[i],aresu,onjolista,hits,debug);
           if myhit<>100 then vfound:=true;
           //write(' "',myhit);
           except writeln('<li>FAILV??',reversestring(akon+sana),i);end;
      //exit;
     // for i:=1 to 33 do
     end;
    //writeln('<li>',vfound,nfound);
  except writeln('<li>FAIL??',reversestring(akon+sana),myhit);end;
//etsiyks (HAKUNEN.loppu+hakunen.akon,hakunen.koko,'',hakunen.eietu,hakunen.eitaka,sika,aresu,onjolist,hits);
     //if debug then  writeln('<li><b>::',koko,'</b> ',aresu.text,'</li><hr>');
     if (nfound or vfound) then writeln('<small style="color:green">',kokosana,'</small>') else writeln('<b  style="color:red">!!',kokosana,'</b>');
     result:=myhit;
     aresu.free;
end;

end.

