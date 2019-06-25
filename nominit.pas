unit nominit;
interface
uses
  Classes, SysUtils,strutils,riimiutils,math,etsi;

{$mode objfpc}{$H+}
{type tnomlka=record kot,ekasis,vikasis:word;vahva:boolean;end;
type tnomsis=record ekaav,vikaav:word;sis:string[8];end;
//type tverlvok=record ekaav,vikaav:word;vok:array[0..1] of char;end;
type tnomav=record ekasana,takia,vikasana:word;h,v,av:string[1];end;  //lisää viekä .takia - se josta etuvokaalit alkavat
}
type tnomlka=record esim:string;kot,ekasis,vikasis:word;vahva:boolean;end;

type tnomsis=record ekaav,vikaav:word;sis:string[8];end;
//type tverlvok=record ekaav,vikaav:word;vok:array[0..1] of char;end;
type tnomav=record ekasana,takia,vikasana:word;h,v,av:string[1];end;  //lisää viekä .takia - se josta etuvokaalit alkavat
type tnsana=record san:string[15];akon:string[4];takavok:boolean;end;

{type tsija=record
  vv,hv:boolean;
  num:byte;//vparad,hparad:byte;
  //vmuomids,hmuomids:array[1..12] of byte;
  ending:string[15];
end;
}

type tnominit=class(tobject)
    lmmids:array[0..49] of array[0..33] of string[15];
    clka,csis,cav,csan:integer;
    nhitlist:tlist;
    //protomids:array[0..49] of array[0..11] of string[15];
     // lk:60 lv:130 av:683
     lkat,lktot,latot,avtot,taivluokkia:word;
     //sijat:array[0..49] of array[0..75] of string[8];
     sijat:array[0..33] of tsija;
     lUOKS:array[0..49] of tnomlka; //78-52+1:+1 varalla
     sisS:array[0..255] of tnomsis;
     AVS:array[0..1023] of tnomav;
     //lks:49 sis:130 av:617 /w:27809
     //VOKS:array[0..160] of tverlvok;
     //sanat:array[0..27816] of tnsana;
     //hakueitaka,hakueietu:boolean;
     sanat:array[0..30000] of tnsana;
    procedure generate(runko,sis,astva:str16;luokka:integer;aresu:tstringlist;hakutakvok:boolean);
    //procedure luesanat(fn:string);
    procedure luemids(fn:string);
    procedure listaa;
    constructor create(kaavafile,midfile:string);
    procedure haesijat(haku:ansistring;sikalauma:tstringlist);
end;

const nbaseendings:array[0..11] of ansistring =('a','a', 'n', '', 'a', 'ut', 'i', 'tu', 'en', 'isi', 'kaa', 'emme');
scount=33;
const nendings:array[0..33] of ansistring=('','n','a','ssa','sta','n','lla','lta','lle','na','ksi','tta','t','en','a','issa','ista','in','illa','ilta','ille','ina','iksi','itta','in','en','iden','itten','in','ten','in','ihin','a','ita');
//const navexamples:array[1..49] of ansistring=('abo','hiomo','avio','elikko','cup','agar','ovi','byte','eka','boa','itara','urea','aluna','urakka','upea','kumpi','keruu','jää','suo','bukee','gay','buffet','lohi','uni','liemi','veri','mesi','kansi','lapsi','peitsi','yksi','aamen','astin','alaston','lämmin','alin','vasen','öinen','ajos','etuus','oras','mies','ohut','kevät','sadas','tuhat','mennyt','ane','huhmar');
//const navexamples:array[1..49] of ansistring=('ukko','hiomo','avio','elikko','cup','agar','kaikki','byte','eka','boa','itara','urea','aluna','urakka','upea','kumpi','keruu','jää','suo','bukee','gay','buffet','lohi','uni','liemi','veri','mesi','jälsi','lapsi','peitsi','yksi','tytär','astin','alaston','lämmin','alin','vasen','öinen','ajos','etuus','rakas','mies','immyt','kevät','sadas','tuhat','mennyt','hake','kinner');

const nexamples:array[1..49] of ansistring=('ukko','hiomo','avio','elikko','häkki','agar','kaikki','nukke','ankka','fokka','itara','urea','aluna','ulappa','upea','kumpi','keruu','jää','suo','bukee','gay','buffet','lohi','uni','liemi','veri','mesi','jälsi','lapsi','peitsi','yksi','tytär','asetin','hapan','lämmin','alin','vasen','öinen','ajos','etuus','rakas','mies','immyt','kevät','sadas','tuhat','mennyt','hake','kinner');
const nsijnams:array[0..34]of ansistring =('N Nom Sg','N Gen Sg','N Par Sg','N Ine Sg','N Ela Sg','N Ill Sg','N Ade Sg','N Abl Sg','N All Sg','N Ess Sg','N Tra Sg','N Abe Sg','N Nom Pl','N Gen Pl','N Par Pl','N Ine Pl','N Ela Pl','N Ill Pl','N Ade Pl','N Abl Pl','N All Pl','N Ess Pl','N Tra Pl','N Abe Pl','N Ins Pl','N Gen Pl','N Gen Pl','N Gen Pl','N Gen Pl','N Gen Pl','N Ill Pl','N Ill Pl','N Par Pl','N Par Pl','xx');

const nsijesims:array[0..34]of ansistring =('ilo','ilon','iloa','ilossa','ilosta','iloon','ilolla','ilolta','ilolle','ilona','iloksi','ilotta','ilot','ilojen','iloja','iloissa','iloista','iloihin','iloilla','iloilta','iloille','iloina','iloiksi','iloitta','iloin','ilojen','omenoiden','omenoitten','ulappain','unten','uniin','iloihin','iloja','omenoita','xx');



const
nvahvanvahvat =[0,2,5,9,13,14,17,21,25,28,31,32];
nheikonheikot= [0,2,29];
//if (lka<32) then if j in [0,2,5,9,13,14,17,21] then thisvahva:=true else thisvahva:=false;

{$mode objfpc}{$H+}

procedure fixlista(fnin,fnout:string);



implementation
//uses riimitys;

procedure fixlista(fnin,fnout:string);  //just once as a fast cure ... gotta rewrite the creator of old listfile later, now just quick cure;
var i,j,jj:integer;fin,fout:textfile;
 rivi,uus,sisus,v,h,vs,sana,alkkon:string;w:tstringlist;
 lka:integer;
  lopvok,lopkon:string;
begin
  assign(fin,fnin);
  reset(fin);
  assign(fout,fnout);
  rewrite(fout);
  w:=tstringlist.create;
  writeln('<pre>');
  i:=0;
  while not eof(fin) do
  begin
        readln(fin,rivi);
        w.commatext:=rivi;
        alkkon:='';
       {if w[0]='042' then w[2]:='ei';  ///pelkkä MIES, loppu-s pois .. temppuilua, mutta ok?
       if w[0]='044' then w[2]:='a';  ///pelkät kevät, venät, loppu-t pois .. temppuilua, mutta ok?
       if w[0]='045' then w[1]:='';  ///ordinaalilukuja, loppu-s pois ..
       if w[0]='046' then w[1]:='';  ///vain TUHAT, loppu-t pois ..
       if w[0]='047' then begin w[1]:='';w[2]:='';end;  ///,-ut päät. partisiippejä loppu-ut pois ..    //liittoutuneet virheenä .. korjaantuu samalla
       }
       lka:=strtointdef(w[0],999);
       if lka=42 then writeln('<li>',rivi, ' ',fnin,'  mies on jotenkin tuhrittu tiedostossa. käsin korjattu, mutta nomit.luolista rikki');
       if w[0]='038' then if w[3]<>'nn' then continue //pois: minunlaiseni.. voi muokatakin > minunlainen
        else
        begin //shitty thing to have to do at this stage, should be handled previously .. well, the whole fixlista is shitty
         lopvok:='';lopkon:='';
           for j:=length(w[5]) downto 1 do
            if (pos(w[5][j],vokaalit)<1) then /// or (j+2<length(w[5])) then
            begin
               jj:=j;
                if j+2<length(w[5]) then jj:=j+1;

              //w[1]:=reversestring(copy(w[5],j+1))+'';
              w[1]:=copy(w[5],jj+1)+'';
              w[2]:=''; w[3]:='';//w[5][j]+w[5][j];
              w[5]:=copy(w[5],1,jj);
              if j<>jj then writeln(':::',w.commatext);
              break;
            end;// else writeln(w[5][j],j);
        end; //  nen-päätteet sanasta pois
        if w[5]='' then sana:='' else if pos(w[5][1],konsonantit)>0 then begin alkkon:=w[5][1];sana:=copy(w[5],2) end else sana:=w[5];
        sana:=reversestring(sana);
        sisus:=reversestring(w[2]+ifs(w[1]='_','',w[1]));
        if length(w[3])=2 then begin v:=w[3][1]; h:=w[3][2];if h=v then v:='a';end;
        if length(w[3])=1 then begin v:=w[3][1]; h:='';end;
        if length(w[3])=0 then begin h:='';v:='a';end;
        vs:=w[4];
       uus:=w[0]+','+sisus+','+v+','+h+','+vs+','+sana+','+alkkon;
        if lka=42 then writeln('<li>',uus);
       if  w[0]='005' then if w[5]='rem' then writeln('<h1>:::',rivi,' //',uus,'</h1>');
       if lka<32 then if w[1]<>'_' then writeln(rivi,'   ',uus);
       writeln(fout,uus);
   end;
  closefile(fin);closefile(FOUT);
end;

procedure tnominit.haesijat(haku:ansistring;sikalauma:tstringlist);
var i,lk,hitpos:integer;mylop:ansistring;j:ptrint;a:ansichar;
begin
 // KAIKKI ON JO REVERSOITU
 // writeln('haesija:',haku,scount);
//for j:=0 to scount do
for j:=0 to scount do
 begin

    mylop:=reversestring(sijat[j].ending); ////VERBEISSA EI loppupäätteissä toistoja kuten nominien illatiivissä
    if mylop='' then hitpos:=1 else
    hitpos :=POS(mylop,haku);
     if pos('*',mylop)>0 then if haku[1]='n' then begin hitpos:=1;mylop:='n';end;
    if hitpos=1 then  //EIIKÄ TÄTÄ KUN KAIKKI ON KÄÄNNETTU
   begin

    sikalauma.addobject((mylop),tobject(@sijat[j]));//@sijat[j]));  //TEMPPUILUA, NUMERO PANNAAN OBJEKTIPOINTTERIIN
   end;
 end;
end;

procedure tnominit.listaa;
var lk,sis,av,san,sija,prlim,x,i,j,k:integer;
  //((lk,sis,av,san,sija:integer;
  mysis,myav,mysana,mysija:string;
  sames:array[0..33] of array[0..33] of byte;
begin
 fillchar(sames,sizeof(sames),0);
 write('<pre>');
 luoks[0].ekasis:=0;
 writeln('<style type="text/css">  .g1 {background-color:green} </style>');
 for lk:=0 to 48 do //clka do
 for i:=0 to 33 do
  for j:=0 to 33 do
  begin
       if LMMIDS[LK,i]=LMMIDS[LK,j] then sames[i,j]:=sames[i,j]+1;
  end;
 writeln('<table border="1"><tr>');
 for i:=0 to 33 do
      write( '<td>', sijat[i].ending,'</td>');
   write( '</tr>');
 for i:=0 to 33 do
     begin
        write( '<tr>');
        write( '<td>',i,' ' ,sijat[i].ending,'</td>');
      for j:=0 to 33 do
      write( '<td>', sames[i,j],'</td>');
      write( '</tr>');
     end;
 writeln('</table>');
 exit;
for lk:=0 to 48 do //clka do
begin
    write(^j,' <b class="g1">lk: ',lk+1,' ',luoks[lk].ekasis,'...',luoks[lk].vikasis,' ',luoks[lk].kot,'</b>');//,'<ul>');
    //for sis:=luoks[lk].ekasis to luoks[lk].vikasis  do write(sis,siss[sis].sis,' /');
    for sis:=luoks[lk].ekasis to luoks[lk].vikasis  do
      begin     prlim:=0;
        //if lk=38 then mysis:='' else
        mysis:=siss[sis].sis;
        //writeln('<li> ',reversestring(mysis),'<ul><pre>');
        for av:=siss[sis].ekaav to siss[sis].vikaav  do
        begin   // if avs[av].v=avs[av].h then if prlim>2 then if lk<>38 then continue;
          //if lk=38 then begin avs[av].v:='';avs[av].h:='';end; //pitää muuttaa lähtötiedostoon
          //writeln('<li>',avs[av].v,avs[av].h,'<hr><pre>');//'<ul><li>');

          for san:=avs[av].ekasana to avs[av].vikasana   do
          //for san:=avs[av].ekasana to min(avs[av].ekasana+3,avs[av].vikasana)   do
          begin  if san>avs[av].ekasana+10 then continue;
            if (prlim<4) or  (avs[av].h<>avs[av].v) or (lk=0) then
            begin
            //        writeln('<li>',reversestring(sanat[san].san+sanat[san].akon),', ');
              // writeln;
            for sija:=0 to scount do
            begin

              if (lk<31) or (lk+1=489) then myav:=ifs(sijat[sija].vv,avs[av].v,avs[av].h)
              else myav:=ifs(sijat[sija].hv,avs[av].v,avs[av].h);
              //else myav:=ifs(sijat[sija].hv,avs[av].hxxx,avs[av].vxxx);
              mysana:=sanat[san].san;
              try
              if pos('*',mysija)>0 then
              begin //write(':',mysija,'[',myav,']');
                //delete(mysija,length(mysija),1);
                mysija[length(mysija)]:='-';
                if mysana<>'' then myav:=mysana[1];//write(':',mysija,'[',myav,']');
              end;
              except write('xxxxxxx');end;
              mysana:=mysis+''+myav+''+mysana+sanat[san].akon;
              //writeln(mysana+' ');continue;
              //myssis:=sanat[san].san;
              //while pos('-',mySIJA)=1 do begin write('*');delete(mysija,1,1);delete(mysana,1,1);end;
              if (lk+1=19) and (pos('-',mysija)>0) then begin delete(mysana,2,1);delete(mysija,length(mysija),1);
              // tie / suo syödäänkin eka vokaali, ei vikaa kuten muissa
              end  else
              while (mysija<>'') and (mySIJA[length(mysija)]='-') do begin write('.');delete(mysija,length(mysija),1);delete(mysana,1,1);end;
              //while (mysija<>'') and (pos('-',mySIJA)>0) do begin x:=pos('-',mySIJA);  write(x);delete(mysija,x,1);delete(mysana,x,1);end;
              //while pos('-',mysis)=length(mysi) do begin delete(mysis,,1)end;
              //write(' ',reversestring(mysana),'<B>',mysija,'</B>'+sijat[sija].ending,',');
              write(copy(reversestring(mysija+mysana)+'                      ',1,8)); //''+sijat[sija].ending+
              //writeln(' ',reversestring(mysana+sanat[san].akon),'',myav,siss[sis].sis,'<B>',LMMIDS[LK-1,SIJA],'</B>'+sijat[sija].ending);
              prlim:=prlim+1;
            end;
            end;
          end; //writeln('</pre><hr>');
        end; // writeln('</pre></ul>');
      end;
end;  writeln('</ul></pre>');
end;


constructor tnominit.create(kAAVAFILE,midfile:string);
begin
//writeln(' ');
luemids('nmids.csv');
//luesanat('nomsall.csv');
//???luesanat(wfile);
//writeln('luettu,luesanat');
nhitlist:=tlist.create;
//writeln('<li>nominit luettu:',midfile,' ',wfile);
//listaa;
exit;
writeln('<hr>etsi<hr>');
//etsi;
//  writeln('<hr>listaakaikki<hr>');
//   listaasanat;
writeln('<hr>listaSkaikki<hr>');
end;


procedure tnominit.luemids(fn:string);  //hanskaa samalla sijojen luonti luettavat sisuskalut on 1/1 sijoihin (todin kuin verbeillä, joilla on "protot")
var i,j:integer;
 slist,mlist:tstringlist;d:boolean;
begin
d:=true;
slist:=tstringlist.create;
mlist:=tstringlist.create;
  slist.loadfromfile(fn);//'nmids.csv');
  //writeln('<pre>luettu ',fn,'=',^m,slist.text,'</pre>');
  slist.delete(0); slist.delete(0);
  slist.delete(0); slist.delete(0);
  //write('<hr>',i,':',slist.commatext,'<hr><pre>');
  //if d then  writeln(slist.commatext);
  for i:=0 to slist.count-1 do
  begin
    //if i=27 then writeln('2222222222222228888888:',slist[i]);
    // if i=27 then slist[i]:='28,     ,*e  ,--tt  ,*e  ,*e  ,*-te  ,*e  ,*e  ,*e  ,*-te  ,*e  ,*e  ,*e  ,      ,      ,-     ,-     ,      ,-     ,-     ,-     ,-     ,-     ,-     ,-';

     mlist.commatext:=slist[i];
     mlist.delete(0);
     mlist.delete(0);
     for j:=0 to scount do
     begin
        try
      lmmids[i+1][j]:=reversestring(mlist[j]);
      //WRITELN(' ',J,lmmids[i][j]);
      except writeln('???fail;',i,' ',j);end;
      //sijat[j].
      //protomids[i,j]:=trim(mlist[j]);
     end;
  end;
 // IF D THEN writeln('<hr>LISTSIJAT:::');
  for i:=0 to scount do
  begin
    sijat[i].vv:=i in nvahvanvahvat;
    sijat[i].hv:=not (i in nheikonheikot);
    sijat[i].ending:=reversestring(trim(nendings[i]));
    sijat[i].num:=i;
    sijat[i].onverbi:=false;
    sijat[i].name:=nsijnams[i];
    sijat[i].esim:=nsijesims[i];

    //if d then
   // writeln('<li>sija:',i,' /end:',sijat[i].ending,' /v:',sijat[i].vv, '/h:',sijat[i].hv,' ',sijat[i].name,' (',sijat[i].esim,')');
  end;
 // writeln('<hr>LISTedSIJAT');
 // writeln('<hr><pre>');
 // if d then for i:=990
 for i:=099 to  49 do
  begin
     write(i);
     for j:=0 to 33 do write(',*',copy(lmmids[i,j]+'              ',1,6));
     writeln;
  end;
  writeln(', ',fn,' luettu');

end;
function red(st:string):string;
begin result:='<b style="color:red">'+st+'</b>';
end;
function blue(st:string):string;
begin result:='<b style="color:blue">'+st+'</b>';
end;


{taivutus edellyttää että on kaivettu sanan perusmuoto, taivutusluokka ja astevaihtelu. Sen voi tehdä käsin, tai etsi-funktiolla
(jolle tarvitaan parametri jo kertoo että halutaan tarkka haku, ei riimihakua. Tai ottamalla inputiksi perusmuoto, genetiivi ja partitiivi)
tehdään alkuun taivutus tunnetusta muodosta.
}

procedure tnominit.generate(runko,sis,astva:str16;luokka:integer;aresu:tstringlist;hakutakvok:boolean);
var lukn,sijax,prlim,x:integer;
si,sikanum,ha:integer;sika:tsija;
 d,vahvaluokka,vahvasija:boolean;         sofar:string;
  {$H-}
  //((lk,sis,av,san,sija:integer;
   mymid,mysis,myav,mysana,mysija,lopvok,myend:str16;
   sikalauma, riimit,xxresu:tstringlist;
   function red(st:string):string;
   begin result:='<b style="color:red">'+st+'</b>';
   end;
   function blue(st:string):string;
   begin result:='<b style="color:blue">'+st+'</b>';
   end;

begin
   d:=true;
   d:=false;
    writeln('</ul>');
    writeln('<li>G:',luokka);
    //for si:=0 to scount-1 do writeln('/',lmmids[luokka,si]);
    vahvaluokka:=luokka+1<32;
    for si:=0 to scount do
    begin
      mymid:=lmmids[luokka,si];
      if pos('?',mymid)>0 then continue;
      myend:=nendings[si];
      sofar:=runko;
      sika:=sijat[si];
      if vahvaluokka then vahvasija:=si in nvahvanvahvat
      else vahvasija:=not (si in nheikonheikot);
      if astva='' then myav:='' else
      if vahvasija then  myav:=astva[1] else
      if length(astva)>1 then myav:=astva[2] else myav:='';
      sofar:=sofar+myav+sis;
      //if lka+1=19 then begin if (mymid[length(mymid)]='-') then end else
      while (mymid<>'') and (mymid[length(mymid)]='-') do
        // begin end else
        begin delete(mymid,length(mymid),1);if luokka+1=19 then delete(sofar,length(sofar)-1,1) else delete(sofar,length(sofar),1);end;
      if pos('!',mymid)=1 then continue;
      if pos('_',mymid)=1 then mymid:=sofar[length(sofar)];
      sofar:=sofar+reversestring(mymid);
      if pos('*',myend)=1 then if pos(sofar[length(sofar)],vokaalit)>0 then myend[1]:=sofar[length(sofar)] else myend[1]:=sofar[length(sofar)-1];//writeln('<li>tupla:',runko+'_'+myend);end;

      sofar:=sofar+myend;
      if not hakutakvok then sofar:=etu(sofar);
      writeln('<li>gene:',luokka+1,'#',si,':',sofar,'/',lmmids[luokka,si],'[',myav,'|',astva,vahvasija,vahvaluokka,']</li>');
      aresu.add(sofar);
       //if not hakutakvok then sofar:=etu(sofar);

     end;
     writeln('</ul>');
   //end;
end;

end.

