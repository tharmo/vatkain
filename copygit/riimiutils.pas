unit riimiutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,strutils;
const konsonantit ='bcdfghjklmnpqrstvwxz'''; vokaalit='aeiouyäö';
  //used in sanataulu

  type tsan=record san:string[15];akon:string[4];takavok:boolean;av:word;end;
  type tnod=record ru_lyh,ru_pit,ru_lyly,yhted,jump,ed,lev,len,tavucount,snum:word;xreftavs:integer;etu,tie:boolean;letter:ansichar;sanainf:pointer;end;
  type tlet=record c:ansichar;w:word;end;

type tvsana=record san:string[15];akon:string[4];takavok:boolean;end;
type tsija=record
  vv,hv:boolean;
  num,vparad,hparad:byte;
  vmuomids,hmuomids:array[1..16] of byte;
  ending:string[15];
  onverbi:boolean;
  name,esim:string[32];
end;

type tvhaku=record hakunen:string[15];sija:byte;eietu,eitaka:boolean;end;
//type tvhakuset=array of tvhaku;
//type thakunen=record koko,akon,loppu:string;lk,sija:word;eietu,eitaka:boolean;end;
type thakunen=class(tobject) koko,akon,loppu:string;lk,sija:word;eietu,eitaka,isverb:boolean;end;

procedure tsekkaa(haku:ansistring;etuko:boolean;hits,riimit:tstringlist);
//procedure tsekkaatavut(haku:ansistring;etuko:boolean;hits,riimit:tstringlist);
function hyphenfi(w:ansistring;tavus:tstringlist):word;
//function hyphenfirev(w:ansistring;tavus:tstringlist):word;
function isdifto(c1,c2:ansichar):boolean;
function isvokraja(c1,c2:ansichar):boolean;
function matchtavut(s1,s2:tstringlist):boolean;
function taka(st:ansistring):ansistring;
function etu(st:ansistring):ansistring;
function matchalkutavu(s1,s2:ansistring):boolean;
function IFs(cond:boolean;st1,st2:ansistring):ansistring;
type tnsana=record san:string[15];akon:string[3];takavok:boolean;end;
type str16=string[16];
type tkokosana=record takvok:boolean;akon,alku,sis,h,v,sloppu:str16;luokka,sananum:word;end;
 type thit=class(tobject) sana:tkokosana;
 function list:string;
end;
procedure voksointu(sana:string;vAR eietu,eitaka:boolean);
function red(st:string):string;
function blue(st:string):string;
function tavucountx(w:string):integer;
function tavusrev(st:ansistring):word;
procedure savebin(rows,cols,usiz:integer;arr:array of word;fn:string) ;
procedure readbin(rows,cols,usiz:integer;var arr:array of word;fn:string) ;
function tavuluku(st:string;var olivok:char):word;
function takax(st:string;var x:word):string;

implementation

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

procedure savebin(rows,cols,usiz:integer;arr:array of word;fn:string) ;
   var i,j:integer;  a:array[0..999] of word;p:word;
   binf:file;
  begin
  try
  AssignFile(binf, fn);  //ei onnistu tstrinlistillä varmaan blockwrite
  Rewrite(binf, cols*usiz);
  writeln('binsave:');
   for i:=0 to rows-1 do //writeln(i*syncols,syns[i*syncols]);exit;
    begin // do
     try
      BlockWrite(binf, arr[i*cols], 1);
     except on e:exception do writeln(e.message);end;
    end;
    writeln('binsaved');
  finally
    Closefile(binf);
    writeln('binclosed:',fn);

   end;
  end;
procedure readbin(rows,cols,usiz:integer;var arr:array of word;fn:string) ;
   var i,j:integer;  a:array[0..999] of word;p:word;
   binf:file;
  begin
  try
  AssignFile(binf, fn);  //ei onnistu tstrinlistillä varmaan blockwrite
  Reset(binf, cols*usiz);
  writeln('binread:',fn,' ',rows,'*',cols,'*',usiz,'=',rows*cols*usiz);
   for i:=0 to rows-1 do //writeln(i*syncols,syns[i*syncols]);exit;
    begin // do
     try
      Blockread(binf, arr[i*cols], 1);
       //if i>5700 then write(^j^j,i);
       //if i>5700 then     for j:=0 to cols do write(' /',arr[i*cols+j],':',arr[i*cols+j+1],' ');
      //end;
     except on e:exception do writeln(e.message);end;
    end;
    writeln('binread');
  finally
    Closefile(binf);
    writeln('binclosed:',fn);

   end;
    exit;
      for i:=5726 to 5728 do
      begin
        try
       // for j:=0 to 31 do if bigvars[j]>0 then IF BIGVARS[J]< 5730 THEN write(' ',gutlist[bigvars[j]],bigvals[j]);
        write(^j^j,i);
        //if i>2800 then writeln(' ?',i,'/',sparmat[i*cols],':',sparmat[i*cols+1],'.',sparmat[i*cols+2],':',sparmat[i*cols+3],'.',sparmat[i*cols+4],':',sparmat[i*cols+5],' ');
        for j:=0 to cols do write(' ///',arr[i*cols+j],':',arr[i*cols+j+1],' ');

        //for j:=0 to 31 do begin      ((if arr[i*64+(j*2)]>0 then // writeln(i*64+(j*2),'/',length(arr)) else
        // write(' ',arr[(i*64)+(j*2)],'=',  arr[i*64+(j*2)+1]);
        //end;
       except writeln('!!',i,':',j,'=',(i*64)+(j*2),'/',length(arr),'fail');end;
       end;
  end;

function red(st:string):string; begin result:='<b style="color:red">'+st+'</b>';  end;
function blue(st:string):string;  begin result:='<b style="color:blue">'+st+'</b>';  end;
//type tnod=record rlen,yhted,jump,ed,lev,len,tavucount:word;reftavs:integer;etu,tie:boolean;letter:ansichar;end;

function tavuraja(a,b:ansichar):boolean;  //huom käänteinen suunta
begin  //
   result:=false;
   if pos(a,vokaalit)>0 then //a on normaalijärjesyksessä seuraava kirjain
   begin  // vailla -> av
     if pos(b,konsonantit)>0 then result:=true
     else if not (isdifto(b,a)) then begin result:=true;;end;
     //if pos(b,vokaalit)>0 then writeln(a,b,result);
    end;
   // writeln(' /',ifs(result,'<b>'+a+b+'</b>',a+b));
end;


function tavusrev(st:ansistring):word;
var i:word;turha:ansistring;pvok,isvok:boolean;
begin                                   //  ae ao ea eo ia io oa oe ua ue
   pvok:=fALSE;
   result:=0;turha:='';
   for i:=1 TO length(st) do
   begin
    isvok:=pos(st[i],vokaalit)>0;
    if pvok then
    begin
      if (isvok) and (not isdifto(st[i],st[i-1])) then result:=result+1
      else  if not isvok then  result:=result+1;
    end;
    pvok:=isvok;
    //writeln(' ',st[i-1],'<b>',st[i],'</b>',result);
   end;
   if pvok then result:=result+1;
    //for i:=2 to length(st) do begin  turha:=turha+st[i];if (tavuraja(st[i-1],st[i])) then begin  turha:=turha+'-';result:=result+1;end;end;
     //writeln(' ',result,pvok);
    //writeln('<li>',string(turha),st[length(st)],result);
end;
function tavucountx(w:string):integer;
var i,tc,voks:word;ch,nch:char;pvok:boolean;
begin
 voks:=0;tc:=0;
 ;nch:=w[length(w)];pvok:=pos(nch,vokaalit)>0;

  for i:=length(w) downto 1 do
  begin
     ch:=w[i];                              //vaah-do-ta  ai-o-ta  hau-ista ha-uis-ta
     if pos(ch,konsonantit)>0 then
     begin
        if voks>0 then begin tc:=tc+1;//write('_');
        end;
        voks:=0;pvok:=false;
     end
     else
     begin voks:=voks+1;
           if (voks>2) or (pvok and (not isdifto(nch,ch)))
           then begin //write('-');
             tc:=tc+1;//writeln(voks,pvok);
             voks:=0; pvok:=false;end
           else pvok:=true;
     end;
      nch:=ch;
  end;         //     akat aito  aietta
  if voks>0 then if pos(w[1],vokaalit)>0 then tc:=tc+1;
  //writeln(tc,'}}');
  result:=tc;
  //writeln('{{',w,':',tc)
end;
procedure voksointu(sana:string;vAR eietu,eitaka:boolean);
var i:word;
BEGIN
try
  eietu:=false;eitaka:=false;
  for i:=1 to length(sana) do
  begin
      if pos(sana[i],'aou')>0 then eietu:=true;
      if pos(sana[i],'äöy')>0 then eitaka:=true;
  end;
  //if eitaka and eietu then begin  eietu:=false;eitaka:=false;end;
except writeln('failcvoksointu');raise;end;
 //writeln('%%',sana,eietu,eitaka);
end;
function thit.list:string;
begin
    with sana do
    result:=akon+reversestring(alku)+'['+v+h+']'+reversestring(sis)+sloppu;
    //with sana do    writeln('<LI>',ALKU+AKON,',',SIS,',',V+H,',',LUOKKA,'</LI>');
    //result:=akon+reversestring(sloppu+'['+v+h+']'+sis+alku);
     //xxx:se.p.tt.i.4 !sesni
    //writeln('xxx:',alku,'.',akon,'.',v,h,'.',sis,'.',luokka);
    //xxx:kki.p..iu.37 !kki
  end;

//procedure luehaku(var hakusetti:tvhakuset;hakufile:string);  //braindead? thaku useless class?

function hyphenfi(w:ansistring;tavus:tstringlist):word;
  var i,k,len,vpos:integer;hy,vocs,alkkon:ansistring;ch,chprev:ansichar;lasttag:ansistring;haddif:boolean;
begin
  if tavus<>nil then tavus.clear;
  len:=length(w);
  result:=0;//w[len];
 if len=0 then exit;
 chprev:=' ';//w[len];   //o
 haddif:=false;
 alkkon:='';
 if (pos(w[1],konsonantit)>0) then
 for i:=2 to 3 do if (pos(w[i],konsonantit)>0) then alkkon:=alkkon+w[i-1] else break;
 if alkkon<>'' then w:=ansilowercase(copy(w,length(alkkon)+1,99));
 len:=length(w);
 for i:=len downto 1 do
 begin
    ch:=w[i]; //d  kons alkaa uuden tavun paitsi jos ed=kons
    if (pos(chprev,vokaalit)>0) then  //jos ed on vokaali
    begin        //a ie  prev:e nyt:i  ie on dift
      if (pos(ch,konsonantit)>0)  then
       begin haddif:=false;
         //result:=result+'-'+ch+hy;
         inc(result);
         if tavus<>nil then tavus.insert(0,ch+hy);hy:='';ch:=' ';
       end
       else  if haddif then begin haddif:=false;//result:=result+'-'+hy;
        inc(result);
        if tavus<>nil then tavus.insert(0,hy);
        hy:=ch
      end
      else
      if (not isdifto(ch,chprev)) then //or  ((i>1) and (pos(w[i-1],vokaalit)>0)) then //kolm vok pÃ¤tks aina kaksi vikaa?                     //  as-il a a is | os
      begin
        //result:=result+'+'+hy+'+';
        inc(result);
        if tavus<>nil then tavus.insert(0,hy);
        hy:=ch; //chprev:='y';//                     //  as-il a a is | os
      end else  //diftongi, mutta tulossa vielÃ¤ yksi vokaali
       if ((i>1) //and (pos(w[i-1],vokaalit)>0))
        and (isdifto(w[i-1],ch))) then begin
          inc(result);
         //result :=result+'+'+hy+'+';
         if tavus<>nil then tavus.insert(0,hy);
         hy:=ch; //chprev:='y';//                     //  as-il a a is | os
       end else
       begin haddif:=true;hy:=ch+hy;
       end; //ignoroi kolmoiskons - mukana vain ei-yhd.sanojen perusmuodot,
   end else hy:=ch+hy;
    if i=1 then begin //result:=result+'_';
      if hy<>'' then begin inc(result);if tavus<>nil then tavus.insert(0,hy);end;
    end else
   chprev:=ch;
 end;
 //HUOM: NÄÄ ON JOSKUS HALUTTU / YLIM ALKUKONONANTIT ERI TAVUNA
// if alkkon<>'' then tavus.insert(0,alkkon+'')
 //else tavus.insert(0,'');
 //result:=alkkon+'_'+result;
 // result:=tavus.commatext;
end;

function hyphenfirev(w:ansistring;tavus:tstringlist):ansistring;
  var i,k,len,vpos:integer;hy,vocs,alkkon:ansistring;ch,chprev:ansichar;lasttag:ansistring;haddiftong:boolean;
begin
  //tAvus:=tstringlist.create;//just for test
  tavus.clear;
  //write('<pre>',w,':=',reversestring(w));//test
//  w:=reversestring(w);//test
 write('[',w,']');
 alkkon:='';
  len:=length(w);
 result:='';//w[len];
 if len=0 then exit;
 chprev:=' ';//w[len];   //o
 haddiftong:=false;
 //alkkon:='';
 //if (pos(w[1],konsonantit)>0) then for i:=2 to 3 do if (pos(w[i],konsonantit)>0) then alkkon:=alkkon+w[i-1] else break; //ekan tavun konsonantit omaksi kokon.
 //if alkkon<>'' then w:=ansilowercase(copy(w,length(alkkon)+1,99));
 len:=length(w);
 for i:=1 to len do
 begin
    ch:=w[i]; // kons alkaa uuden tavun paitsi jos ed=kons
    if (pos(chprev,vokaalit)>0) then  //jos ed on vokaali (muuten aina jatketaan ja pannaaan vain edellinen muistiiin)
    begin
      if (pos(ch,konsonantit)>0)  then
      begin                     ///konsonantti vokaalin edellä päättää tavun
         haddiftong:=false;result:=result+'-'+ch+hy;tavus.insert(0,ch+hy);hy:='';ch:=' ';
      end
      else  if haddiftong then
      begin
         haddiftong:=false;result:=result+'-'+hy;tavus.insert(0,hy);hy:=ch
      end
      else                       // vokaalin edellä vokaali
      if (not isdifto(ch,chprev)) then  //kolm vok pätks aina kaksi vikaa?                     //  as-il a a is | os
      begin                      //eivät muodost diftongia, eli tavuraja väliin
        result:=result+'+'+hy+'+';tavus.insert(0,hy);hy:=ch; //chprev:='y';//                     //  as-il a a is | os
      end else  //diftongi, mutta edellä vielä yksi vokaali
      if ((i<len)
        and (isdifto(w[i+1],ch))) then
        begin
         result :=result+'*'+hy+'*';tavus.insert(0,hy);hy:=ch;     //
       end else
       begin  //oli diftongi, päätetään edellisen merkin per seuraavalla kierroksella
         haddiftong:=true;hy:=ch+hy;
       end; //ignoroi kolmoiskons - mukana vain ei-yhd.sanojen perusmuodot,
   end else hy:=ch+hy;
  if i=len then begin result:=result+'_'; if hy<>'' then tavus.insert(0,hy);end
  else
   chprev:=ch;
 end;
 if alkkon<>'' then tavus.insert(0,alkkon+'')
 else tavus.insert(0,'');
 result:=alkkon+'_'+result;
//  result:=tavus.commatext;
 // writeln('<li>tavut;',tavus.commatext,'</li>');
end;

procedure tsekkaa(haku:ansistring;etuko:boolean;hits,riimit:tstringlist);
var   hittavut,tofindtavut:tstringlist; etuhit:boolean;
i,tavucount:integer;
hitw:ansistring;
begin
  hittavut:=tstringlist.create;
  tofindtavut:=tstringlist.create;
  hyphenfi(haku,tofindTAVUT);
  //writeln('<li>tutkitavut:',haku,'|||',hits.commatext,hits.count,'</li>');
  //writeln('<li>tutkittavat:',haku,'|||',hits.commatext,hits.count);
  tavucount:=tofindtavut.count;
  for i:=0 to hits.count-1 do
  begin
      hitw:=hits[i];//(trim(copy(hits[i],1)));
      hyphenfi(hitw,hittavut);   //tavus pitäis olla käänt.järj. ja pakattuina ttavuiksi. myöhemmin
      //write('?<li>haku:',haku,'/kohde:',hitw,'#', hittavut.count,tavucount,'-' );
      if abs(hittavut.count-tavucount) mod 2=0 then
      begin
        //writeln('/tav?',hittavut.count,hittavut.commatext,tofindtavut.count,tofindtavut.commatext);
        if  matchtavut(hittavut,tofindtavut) then
        begin
          //if etuhit then hitw:=etu(hitw);
          writeln('++',hitw);
          riimit.add(hitw);
          hittavut.clear;
          //writeln('++',hitw);
        end;// else writeln('!!!!!!!!!!!!',etuhit,etuko);
      end ;// else writeln('<strike>::',hittavut.commatext,'</strike>')
    end;
  hittavut.free;tofindtavut.free;
end;

procedure tsekkaatavut(haku:ansistring;etuko:boolean;hits,riimit:tstringlist);
var   hittavut,tofindtavut:tstringlist; etuhit:boolean;
i,tavucount:integer;
hitw:ansistring;
begin
  hittavut:=tstringlist.create;
  tofindtavut:=tstringlist.create;
  //takako(haku,takahaku);
  hyphenfi(haku,tofindTAVUT);
  //writeln('<h1>tutkitavut:',haku,'|||',tofindtavut.commatext,tofindtavut.count,'</h1>');
  //writeln('<li>tutkittavat:',haku,'|||',hits.commatext,hits.count);
  //writeln('<li>hits:<b>',hits.commatext,'</b><li>',tofindtavut.commatext,',</li>');
  tavucount:=tofindtavut.count;
  for i:=0 to hits.count-1 do
  begin
      //tofind.addstrings(tavus);
      if hits[i][1]='1' then etuhit:=true else etuhit:=false;
      //write('?',haku,'/',hits[i],etuko,etuhit);
      if etuko<>etuhit then continue;
      hitw:=trim(copy(hits[i],2));
      //write('=',haku,'/',hitw,etuko,etuhit);
      hyphenfi(hitw,hittavut);   //tavus pitäis olla käänt.järj. ja pakattuina ttavuiksi. myöhemmin
      //writeln('<li>X ',hits[i]);
      //writeln('<li><em>VERTAA:',hittavut.commatext,hittavut.count,'</em>');
      //writeln('<li>',hittavut.commatext,hittavut.count,'/',tavucount);
      if abs(hittavut.count-tavucount) mod 2=0 then
      begin
        //mismat:=false;
         //writeln('<hr>');
        if  matchtavut(hittavut,tofindtavut) then
        begin
          //if etuhit then writeln('<li style="color:green">:::',etu(hittavut.commatext),'</li>')
         // else  writeln('<b style="color:blue">;;;',(hittavut.commatext),'</b>');
          if etuhit then hitw:=etu(hitw);

          riimit.add(hitw);
          hittavut.clear;
        end;// else writeln('!!!!!!!!!!!!',etuhit,etuko);
        //writeln(';; ',tavus.commatext,tavus.count,'/',tavucount,' ',abs(tavus.count-tavucount) mod 2)
      end  else writeln('<strike>::',hittavut.commatext,'</strike>')
    end;
  //writeln('<h4>kaikki:',riimit.commatext,'!!</h4>');
  hittavut.free;tofindtavut.free;
end;

function isdifto(c1,c2:ansichar):boolean;
 begin
  result:=false;
  if c1=c2 then result:=true else                       //arv  i o i da                ae ao ea eo ia io oa oe ua ue
  case c1 of
   'a': if pos(c2,'iu')>0 then result:=true;
   'e','i': if pos(c2,'ieuy')>0 then result:=true;
   'o','u': if pos(c2,'iuo')>0 then result:=true;
   'y','ö': if pos(c2,'iyö')>0 then result:=true;
   'ä': if pos(c2,'iy')>0 then result:=true;
  end;
 // if not result then write('*');
 end;
  function isvokraja(c1,c2:ansichar):boolean;
   begin
    result:=true;
    if pos(c2,konsonantit)>0 then result:=false else
    if pos(c1,konsonantit)>0 then result:=false else
    if c1=c2 then result:=false else                       //arv  i o i da                ae ao ea eo ia io oa oe ua ue
    case c1 of
     'a': if pos(c2,'iu')>0 then begin result:=false;;end;
     'e','i': if pos(c2,'ieuy')>0 then result:=false;
     'o','u': if pos(c2,'iuo')>0 then result:=false;
     'y','ö': if pos(c2,'iyö')>0 then result:=false;
     'ä': if pos(c2,'iy')>0 then result:=false;
    end;
 // if not result then;
 end;
function matchtavut(s1,s2:tstringlist):boolean;
var i,j,ldif,end1:integer;p:boolean;       //onnettomat listat ovat tavuja oikeassa järjestyksessä, helpompaa ois käänteiset
begin
  RESULT:=TRUE;
 // writeln('<li>MMMM:',s1.commatext,'///',s2.commatext);
 // p:=s1.text=s2.text;
  ldif:=s1.count-s2.count;
    for j:=(s1.count-1) downto 2 do
      if j-ldif<2  then
      begin //writeln('(',j-ldif,'!)');
        break;
      end else
      begin
          //if s1[j]<>taka(s2[j-ldif]) then
          if s1[j]<>s2[j-ldif] then
           begin //write('{',J,s1[j],'!=',J-LDIF,s2[j-ldif],'}');
             result:=fALSE;
               //if p then
               //writeln('<li style="color:red">xxyy',s1.commatext,'/',s2.commatext,result);
             EXIT;end;
          // write(' [',J,s1[j],'=',J-LDIF,(s2[j-ldif]),']');
          end1:=j;
      end;
    try
     //writeln('<li>',s1.commatext,'///',s2.commatext);
     if (end1>1) and (end1-ldif-1>0) then
      result:=matchalkutavu(s1[end1-1],s2[end1-ldif-1]);
    //writeln('((',end1, s1[end1-1],'/',s2[end1-ldif-1],'))',result);
    except   writeln('fail:',end1-1,'[[',end1, ldif,']]',end1-ldif-1);
   end;
    //FOR J:=LENGTH(S1[)
end;

function matchalkutavu(s1,s2:ansistring):boolean;
var i,j:integer;
begin
try
  result:=true;
  if (s1='') or (s2='') then exit;
  while pos(s1[1],konsonantit)>0 do delete(s1,1,1);
  while pos(s2[1],konsonantit)>0 do delete(s2,1,1);
  result:=s1=s2;
  //writeln(result,s1,s2, '/');
  except   writeln('!!FAILEN:',s1,s2);end;
end;

function IFs(cond:boolean;st1,st2:ansistring):ansistring;
     begin
      if cond then result:=st1 else result:=st2;
     end;
function etu(st:ansistring):ansistring;
var i:byte;
begin
 for i:=1 to length(st) do
  if st[i]='a' then st[i]:='ä' else
 if st[i]='o' then st[i]:='ö' else
 if st[i]='u' then st[i]:='y';// else st[i]:=st[i];
 result:=st;
end;
function takax(st:string;var x:word):string;
 var i:byte;st2:ansistring;
 function c(s:ansichar):ansichar;
 begin

     if pos(s,'aou')>0 then begin result:=s;x:=0;end else
     if s='ä' then  result:='a' else
     if s='ö' then result:='o' else
     if s='y' then result:='u'  else result:=s;
  end;
 begin
  x:=1;
  result:='';
  for i:=1 to length(st) do st[i]:=c(st[i]);
  result:=st;
 end;

function taka(st:ansistring):ansistring;
var i:byte;st2:ansistring;
function c(s:ansichar):ansichar;
begin
    if pos(s,'aou')>0 then begin result:=s;end else
    if s='ä' then result:='a' else
    if s='ö' then result:='o' else
    if s='y' then result:='u'  else result:=s;
 end;
begin
 result:='';
 for i:=1 to length(st) do st[i]:=c(st[i]);
 result:=st;
end;



end.

