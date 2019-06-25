unit verbikama;
interface
uses
  Classes, SysUtils,strutils,riimiutils,math;

{$mode objfpc}{$H+}
type tverlka=record esim:string;kot,ekasis,vikasis:word;vahva:boolean;w:word;end;
type tversis=record ekaav,vikaav:word;sis:string[8];w:word;end;
//type tverlvok=record ekaav,vikaav:word;vok:array[0..1] of char;end;
type tverav=record ekasana,takia,vikasana:word;h,v,av:string[1];w:word;end;  //lis�� viek� .takia - se josta etuvokaalit alkavat
type tvsana=record san:string[15];akon:string[4];takavok:boolean;end;
type thitrec=record lk,si,av,san:word;end;
type tverbit=class(tobject)
    lmmids:array[0..49] of array[0..67] of string[15];
    protomids:array[0..49] of array[0..12] of string[15];
     // lk:60 lv:130 av:683
     lkat,lktot,latot,avtot,taivluokkia:word;
     //sijat:array[0..49] of array[0..75] of string[8];
     sijat:array[0..66] of tsija;
     lUOKS:array[0..30] of tverlka; //78-52+1:+USEAMPI varalla
     sisS:array[0..127] of tversis;
     AVS:array[0..1023] of tverav;
     vhitlist:tlist;
     //VOKS:a   rray[0..160] of tverlvok;
     //sanat:array[0..27816] of tsana;
     sanat:array[0..16383] of tvsana;
     procedure genlist;
    procedure listaa;
    //procedure siivoosijat;
    //procedure luesanat(fn:string);
    procedure luesijat(fn:string);
    procedure listsijat;
    procedure luemids(fn:string);
    procedure createsija(si:integer;ss:ansistring);
    constructor create(wfile,midfile,sijafile:string);
    function numtowrd(snum:integer):thitrec;
    //procedure listaasanat;
    procedure listaasijat;
    procedure haesijat(haku:ansistring;sikalauma:tstringlist);
end;
const luokkia=24;sikoja=66;
const VprotoLOPut:array[0..11] of ansistring =('a','a', 'n', '', 'a', 'ut', 'i', 'tu', 'en', 'isi', 'kaa', 'emme');
const
vvahvanheikot =[5,6,7,8,9,10,13,14,15,24,25,26,27,29,30,31,32,33,34,35,36];
//VVAHVAT: ,0,1,2,3,4,11,12,16,17,18,19,20,21,22,23,28,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65
vheikonheikot=[0,1,2,3,4,13,14,15,16,17,18,19,20,21,22,29,30,31,32,33,34,35,36,37,38,62,63,64,65];
vsijanimet:array[0..66] of ansistring =
  ('V Inf1 Lat ','V Inf1 Act Tra Sg PxPl1 ','V Inf1 Act Tra Sg PxSg1 ','V Inf1 Act Tra Sg PxPl2 ','V Inf1 Act Tra Sg PxSg2 ','V Impv Act Sg2 ','V Prs Act ConNeg ','V Prs Act Pl1 ','V Prs Act Sg1 ','V Prs Act Sg2 ','V Prs Act Pl2 ','V Prs Act Pl3 ','V Prs Act Sg3 ','V Prs Pass ConNeg ','V Prs Pass Pe4 ','PrfPrc Pass Pos Nom Pl ','V Pot Act Sg3 ','V Pot Act Pl1 ','V Pot Act Sg1 ','V Pot Act Sg2 ','V Pot Act Pl2 ','V Pot Act Pl3 ','PrfPrc Act Pos Nom Sg ','V Pst Act Sg3 ','V Pst Act Pl1 ','V Pst Act Sg1 ','V Pst Act Sg2 ','V Pst Act Pl2 ','V Pst Act Pl3 ','V Inf2 Pass Ine ','V Cond Pass Pe4 ','V Impv Pass Pe4 ','V Inf3 Pass Ins ','V Pot Pass Pe4 ','PrsPrc Pass Pos Nom Sg ','V Pst Pass Pe4 ','V Pst Pass ConNeg ','V Inf2 Act Ins ','V Inf2 Act Ine Sg ','V Cond Act Sg3 ','V Cond Act Pl1 ','V Cond Act Sg1 ','V Cond Act Sg2 ','V Cond Act Pl2 ','V Cond Act Pl3 ','AgPrc Pos Nom Sg ','AgPrc Pos Ill Sg ','V Act Inf5 Px3 ','V Act Inf5 PxPl1 ','V Act Inf5 PxSg1 ','V Act Inf5 PxPl2 ','V Act Inf5 PxSg2 ','V Inf3 Ade ','V Inf3 Man ','V Inf3 Ine ','V Inf3 Ela ','V Inf3 Abe ','V N Nom Sg ','V N Par Sg ','V N Par Sg ','PrsPrc Act Pos Nom Sg ','PrsPrc Act Pos Nom Pl ','V Impv Act Pl2 ','V Impv Act Pl1 ','V Impv Act Sg3','V Impv Act Pl3','V Act Inf5 Px3 ');
vsijaesim:array[0..66] of ansistring = ('kehua','kehuaksemme','kehuakseni','kehuaksenne','kehuaksesi','kehu',{'VIRHE',}'kehu','kehumme','kehun','kehut','kehutte','kehuvat','kehuu','kehuta','kehutaan','kehutut','kehunee','kehunemme','kehunen','kehunet','kehunette','kehunevat','kehunut','kehui','kehuimme','kehuin','kehuit','kehuitte','kehuivat','kehuttaessa','kehuttaisiin','kehuttakoon','kehuttaman','kehuttaneen','kehuttava','kehuttiin','kehuttu','kehuen','kehuessa','kehuisi','kehuisimme','kehuisin','kehuisit','kehuisitte','kehuisivat','kehuma','kehumaan','kehumaisillaan','kehumaisillamme','kehumaisillani','kehumaisillanne','kehumaisillasi','kehumalla','kehuman','kehumassa','kehumasta','kehumatta','kehuminen','kehumista','kehumista','kehuva','kehuvat','kehukaa','kehukaamme','koon','koot','kehumaisillansa');
vesims: array[1..27] of ansistring =('hioa', 'sulaa', 'pieks��', 'soutaa', 'jauhaa', 'kaataa', 'laskea', 'tuntea', 'l�hte�', 'kolhia', 'naida', 'saada', 'vied�', 'k�yd�', 'p��st�', 'puhella', 'aterioida', 'suudita', 'piest�', 'n�hd�', 'parata', 'niiata', 'kasketa', 'nimet�', 'taitaa', 'kumajaa', 'kaikaa');
//'trio', 'plasebo', 'oboe', 'viidakko', 'kebab', 'diesel', 'hanhi', 'boutique', 'dia', 'kahdeksan', 'apaja', 'idea', 'orkidea', 'kahakka', 'juohea', 'taempi', 'vajaa', 'maa', 'tie', 'nugaa', 'reggae', 'buffet', 'lohi', 'ruuhi', 'loimi', 'huoli', 'paasi', 'kansi', 'lapsi', 'peitsi', 'kaksi', 's�vel', 'morsian', 'hapan', 'l�mmin', 'parahin', 'vasen', 'sulhanen', 'iskias', 'vakaus', 'uskalias', 'mies', 'ohut', 'kev�t', 'nelj�s', 'tuhat', 'kuollut', 'poikue', 'sammal');


{$mode objfpc}{$H+}



implementation
//uses riimiuus;
function red(st:string):string;
begin result:='<b style="color:red">'+st+'</b>';
end;
function blue(st:string):string;
begin result:='<b style="color:blue">'+st+'</b>';
end;
function listsija(sika:tsija):string;
begin
   with sika do  result:=' {'+inttostr(num)+'('+inttostr(vparad)+' '+inttostr(hparad)+'):'+ending+'} ';
end;
function tverbit.numtowrd(snum:integer):thitrec;
var lu,si,av,sa:integer;
var lux,six,avx,sax:integer;
 koko:string;
begin
   fillchar(result,sizeof(result),0);
    for lux:=0 to luokkia do
    begin
      //writeln('<li>LKA:',lux+52,' ',luoks[lux].kot,'/',luoks[lux].w,' (',snum,') <ul>');//,luoks[lux].ekasis,' to ',luoks[lux].vikasis );
      if luoks[lux].w>=snum then
      begin
        //writeln('<li>',lux,luoks[lux].kot,'<ul>');
        result.lk:=lux;
        for six:=luoks[lux].ekasis to luoks[lux].vikasis do
          if siss[six].w>=snum then
          begin
            //writeln('<li>Sis:',six,' ',siss[six].sis,' ',siss[six].w,'<ul>');
            result.si:=six;
            for avx:=siss[six].ekaav to siss[six].vikaav do
            if avs[avx].w>=snum then
              begin
                //writeln('<li>AV:',avx,' ',avs[avx].v,avs[avx].h,'');
                result.av:=avx;
                for sax:=avs[avx].ekasana to avs[avx].vikasana do
                 if sax=snum then begin
                  result.san:=sax;
                 // writeln(sax);
                  //writeln('<li>',sax,' ',sanat[sax].san,sanat[sax].akon,'');
                  break;
                end;
                break;
              end;
            //writeln('</ul>');
            break;
          end;
         //writeln('</ul>');
         break;
      end;
      //writeln('</ul>');
    end;
  koko:='a'+lmmids[result.lk,0]+'.'+siss[result.si].sis+'.'+ifs(result.lk+52<63,avs[result.av].v,avs[result.av].h)+'.'+sanat[result.san].san+sanat[result.san].akon;
   if not sanat[result.san].takavok then koko:=etu(koko);
  if random(55555) mod 55555=1 then
  writeln('<li>',reversestring(koko),'   ',snum,' ',result.lk+52,':<hr/>');
end;
procedure tverbit.genlist;
var sija,lu,si,av,sa,cut:integer;
  sijast,lust,sist,avst,sofar:string;
  mylust,mysist,myavst:string;
  vahvaluokka,vahvasija:boolean;yksvaan:integer;
  cutav:boolean;
procedure dosana(snum:integer);
//var san:integer;
begin
  //if length(sanat[snum].san)>3 then exit;
   if cutav then writeln('zxc');
sofar:=sijast+lust+sist+avst;
writeln('<li><span title:="'+inttostr(lu+51),'"> ',reversestring(sijast+''+mylust+''+mysist+''+myavst+''+sanat[snum].san+sanat[snum].akon)+'</span>');
end;

procedure doav(avn:integer);
var i:integer;
begin
 av:=avn;
 if lu+51=58 then if (avs[av].v<>'k') or (avs[av].h<>'') then exit;
   avst:=ifs(vahvasija,avs[av].v,avs[av].h);
   myavst:=ifs(cutav,'',avst);
   if avs[av].v<>avs[av].h then writeln('<b>',avst,':</b>');
   for i:=avs[av].ekasana to avs[av].ekasana do dosana(i);exit;
   for i:=avs[av].ekasana to min(avs[av].ekasana+1,avs[av].vikasana) do dosana(i);
end;
procedure dosis(snum:integer);//;mylust:string);
var i:integer;
begin
   mysist:=siss[snum].sis;
   mylust:=lust;
   try
   if lust='*' then mylust:=mysist[1];
   except writeln('<h3>(',mysist,'/',lust,')');end;
   if cut>0 then writeln('CUT:',cut,sist,'!');
   if cut>0 then
   begin
     if cut=2 then delete(mysist,length(mysist),1)
     else  delete(mysist,1,1);
     mylust:=stringreplace(mylust,'_','',[rfReplaceAll]);
     //mylust:='';//mysis:='';
     writeln('<b>[',sist,'/',mysist,']</b>');
   end;
   yksvaan:=0;

   //for i:=max(siss[snum].ekaav,siss[snum].vikaav-1) to siss[snum].vikaav do doav(i);
   for i:=siss[snum].ekaav to siss[snum].ekaav  do doav(i);
   exit;
   for i:=siss[snum].ekaav to siss[snum].vikaav  do doav(i);
end;
procedure dolka(lnum:integer);
var i:integer; mylust:string;
begin
   lu:=lnum;
   cut:=0;
   if not (lu+51 in [52]) then exit;
   lust:=lmmids[lu,sija];
   if (lu+51=71) then if ((sijat[sija].hparad=2) and  (sija>10)) then lust:='-ek' else if  (sija in [23,28]) then lust:='-k';
   vahvaluokka:=(lu+51<63) or (lu+51 in [76]);
   if vahvaluokka then vahvasija:=not (sija in vheikonheikot)
   else vahvasija:=not (sija in vheikonheikot);
   if pos('_',lust)>0 then if pos('__',lust)>0 then cut:=2 else cut:=1;
      cutav:=pos('-',lust)>0;
   //if cut=0 then exit;
//   writeln('<li><b>',lu+51,'</b>',lust,cut,': ');
   for i:=luoks[lu].ekasis+1 to luoks[lu].vikasis  do dosis(i);
end;
var i:integer;
begin
   for sija:=0 to 66 do
   begin
      sijast:=(sijat[sija].ending);
      for i:=1 to 27 do dolka(i);
   end;

end;

procedure tverbit.listsijat; var i,j,k:integer;
function td(st:string):string;
  begin   result:= '<td>'+st+'</td>';end;

function vh(lu,si:integer):string;
 Var tag:string;
  begin
   //  if lu+51<64 then result:=ifs(si in vHEIKOtsijat,'','!!')
   //ELSE  result:=ifs(si in HHEIKOtsijat,'','!!')
   if lu+51<64 then TAG:=ifs(si in vvahvanheikot,'SPAN','B')
 ELSE  tAG:=ifs(si in vHEIKOnheikot,'SPAN','B');
WRITELN('<TD><',TAG,'>'+reversestring(LMMIDS[LU,SI])+'</'+TAG+'></td>');
  end;
begin
   writeln('<table border="1"><tr><td/>');
   for j:=0 to sikoja do     if (j in  [0,5,10,11,12,13,16,23,25,36,37,39,45]) then writeln(td(inttostr(j)+reversestring(sijat[j].ending)));
   writeln('</tr>');
   for i:=1 to 26 do
   begin
    writeln('<tr>',td(inttostr(i+51)+luoks[i].esim));
     for j:=0 to sikoja do   if (j in  [0,5,10,11,12,13,16,23,25,36,37,39,45]) then
    VH(I,J);//(;;writeln(td(reversestring(lmmids[i,j])+vh(i,j)));
   end;
end;

procedure tverbit.listaa;
  function b(st:string):string;
    begin result:='<b>'+st+'</b>';end;
var lu,sis,av,san:Integer;myav,mysis,mymid:string;
curlka:tverlka;cursis:tversis;curav:tverav;cursan:tvsana;
begin
  writeln('<ul>');
  for lu:=0 to 27 do
  begin
    curlka:=luoks[lu];
    mymid:=lmmids[lu-1,1];
    writeln('<li>',lu+52,B(curlka.ESIM),' ',mymid,curlka.kot,' ',curlka.ekasis,'...',curlka.vikasis,' ',curlka.w);
    writeln('<ul>');
   for  sis:=curlka.ekasis to curlka.vikasis do
   begin
     cursis:=siss[sis];
     mysis:=cursis.sis;
     //if lmmids[lu-1,1]='*' then begin mymid:=mysis[1]+'';end;// else    mymid:=lmmids[lu-1,1];
     writeln('<li>sis:',b(mysis+'.'),' ',cursis.ekaav,'...',cursis.vikaav,' ',cursis.w);
     //if lu=1 then continue;
     writeln('<ul>');
    for av:=cursis.ekaav to cursis.vikaav do
    begin
      curav:=avs[av];
      if (lu+52<63) or (lu+52>76) then myav:=curav.v else  myav:=curav.h;
      writeln('<li>',b(curav.v+curav.h),' ',curav.ekasana,'...',curav.vikasana,' ',sanat[curav.vikasana].san,sanat[curav.vikasana].akon,' ',curav.w);
      //for san:=curav.ekasana to curav.vikasana do writeln(' ',reversestring(mymid+mysis+''+myav+''+sanat[san].san+sanat[san].akon)+'a');
    end;
    writeln('</ul>');
   end;
   writeln('</ul>');
  end;
  writeln('</ul>');
  writeln('<li>ss',sanat[4908].san);
end;


procedure tverbit.haesijat(haku:ansistring;sikalauma:tstringlist);
var i,lk,hitpos:integer;mylop:ansistring;j:ptrint;a:ansichar;
begin
 // KAIKKI ON JO REVERSOITU
for j:=0 to 66 do
 begin
    mylop:=sijat[j].ending; ////VERBEISSA EI loppup��tteiss� toistoja kuten nominien illatiiviss�
    if mylop='*' then writeln('<li>sija:',j);
    if (mylop='') or (mylop='*') then hitpos:=1 else
    //if matchendvar(haku,mylop,alku) then  //EIIK� T�T� KUN KAIKKI ON K��NNETTU
    hitpos :=POS(mylop,haku);
    //if hitpos>0 then writeln(' [:<b>',j,reversestring(mylop),':</b>', hitpos,haku,'] ');
    if hitpos=1 then  //EIIK� T�T� KUN KAIKKI ON K��NNETTU
   begin
        sikalauma.addobject(mylop,tobject(@sijat[j]));//@sijat[j]));  //TEMPPUILUA, NUMERO PANNAAN OBJEKTIPOINTTERIIN
  //ei etsit� riimej� suoraa p��tteist� (grillasi/ maisillasi)
   end else if pos(haku,mylop)=1 then
   sikalauma.addobject('',tobject(@sijat[j]));//@sijat[j]));  //TEMPPUILUA, NUMERO PANNAAN OBJEKTIPOINTTERIIN
 end;
end;




constructor tverbit.create(wfile,midfile,sijafile:string);
begin
writeln('<li>verbit:',wfile,'/',midfile,'/',sijafile,'/');
vhitlist:=tlist.create;
luemids(midfile);//'vmids.csv');
//writeln('luettu,luesijat');
//writeln('lue');
luesijat('vsijat.csv');
writeln('verbit luettu');
//listaasijat;
//writeln('listattu');
//luesanat(wfile);
//writeln('<hr>etsi<hr>');
//etsi;
//  writeln('<hr>listaakaikki<hr>');
//   listaasanat;
//writeln('<hr>listaSkaikki<hr>');
end;


procedure tverbit.createsija(si:integer;ss:ansistring);
var sl:tstringlist;ms:tsija; j:word;num:integer;
begin
try
sl:=tstringlist.create;
sl.commatext:=ss;
num:=strtointdef(sl[0],98);
sijat[si].num:=num;
sijat[si].vparad:=strtointdef(sl[1],255);
sijat[si].hparad:=strtointdef(sl[3],255);
sijat[si].vv:=sl[2]='v1';
sijat[si].hv:=sl[4]='h1';
sijat[si].ending:=reversestring(sl[5]);
sijat[si].onverbi:=true;
sijat[si].name:=vsijanimet[num];
sijat[si].esim:=vsijaesim[num];
//writeln('<li>',si,'/',num,':',sl.commatext,':<b>',sijat[si].name,'</b>',vsijanimet[num]);

for j:=0 to 27 do
  //if (j<12) or (j>24) then
  if (j+52<64) or (j+52=76) then
   lmmids[j,si]:=reversestring(protomids[j,sijat[si].vparad])
 else //for j:=12 to 27 do
   lmmids[j,si]:=reversestring(protomids[j,sijat[si].hparad]);
//luoks[0].vahva:=true;
except
writeln('<li>fail:',si,':',sl.count,'!</li>');
end;
//writeln(',',si,lmmids[0,si]);
sl.free;
end;
procedure tverbit.listaasijat;
var lu,si:word;
begin
  writeln('<table border="1"><tr><td></td>');
  for si:=0 to 64 do
   writeln('<td>',si,'</td>');
  writeln('</tr><tr><td></td>');
  for si:=0 to 64 do
   writeln('<td>',sijat[si].vparad,' ',sijat[si].hparad,'</td>');
  writeln('</tr>');
  writeln('<tr><td>xx</td> ');
  for si:=0 to 64 do
   writeln('<td>',si,reversestring(sijat[si].ending),'</td>');
  writeln('</tr>');
  for lu:=0 to luokkia do
  begin
    //if (lu+52>59) or (lu>63)  then continue;
    writeln('<tr><td>',ifs(luoks[lu].vahva,'<b>'+inttostr(lu+52)+'</b>',inttostr(lu+52)),'</td>');
    for si:=0 to 64 do
    writeln('<td>',lu,si,reversestring(lmmids[lu,si]),'</td>');
    //writeln('<td>',lmmids[lu,sijat[si].vparad],'</td>');
    writeln('</tr>');
   end;
   writeln('</table');



end;

procedure tverbit.luesijat(fn:string);
var i,j:integer;
 vheikot,hheikot,vvahvat,hvahvat:string;
 slist:tstringlist;
begin
 slist:=tstringlist.create;
 slist.loadfromfile(fn);//'vsijat.csv');
 for i:=0 to slist.count-1 do     // 0..65
 begin
  createsija(i,slist[i]);
  if sijat[i].vv then  vvahvat:=vvahvat+','+inttostr(i) else vheikot:=vheikot+','+inttostr(i);
  if sijat[i].hv then hvahvat:=hvahvat+','+inttostr(i) else hheikot:=hheikot+','+inttostr(i);
  //writeln('<li>',i,':',vsijanimet[i],'/',vsijaesim[i],' <b>',reversestring(sijat[i].ending),'</b>');
 end;
 {for i:=1 to 11 do
  begin
   for j:=1 to slist.count-1 do
     if (sijat[j].vparad=i) or (sijat[j].hparad=i) then write('*');
  end;
 //writeln('<table><tr>'); for i:=0 to slist.count for i:=0 to slist count
 }
 slist.free;
end;

procedure tverbit.luemids(fn:string);
var i,j:integer;
 slist,mlist:tstringlist;
begin
//writeln('<h1>tverbit.luemids</h1>');
slist:=tstringlist.create;
mlist:=tstringlist.create;
  slist.loadfromfile(fn);//'vmids.csv');
  //writeln('<pre>',slist.commatext,'</pre>');
  slist.delete(0);  slist.delete(0);  slist.delete(0);  slist.delete(0);
  for i:=0 to slist.count-1 do
  begin
     mlist.commatext:=slist[i];
     //writeln('<pre>',mlist.commatext,'</pre>');
     mlist.delete(4);
     //writeln('<pre>',mlist.commatext,'</pre>');
     for j:=1 to 10 do
     begin
      protomids[i,j]:=trim(mlist[j+1]);
     end;
  end;
end;




end.

