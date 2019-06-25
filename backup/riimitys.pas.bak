unit riimitys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,nominit,verbikama,//nomutils,
  strutils,riimiutils,riimiuus;
 procedure nomforms;
 procedure riimei;
procedure tsekkaatavut(hakunen:ansistring;etuko:boolean;hits,riimit:tstringlist);

type triimitin=class(tobject)
 verbit:tverbit;nominit:tnominit;//eitait:teitaivu;

 //hakuset,hakuset2:tvhakuset;
 allresus,riimit:tstringlist;
 procedure etsi(resux:tstringlist);
 procedure tulokset;


 constructor create;
 end;
 PROCEDURE PILKO;

implementation
 procedure verbforms;
 var i,j:integer;st,res:tstringlist;verbit:tverbit;
 begin
   verbit:=tverbit.create('sanatuus.csv','vmids.csv','vsijat.csv');
  // verbikama.
  //for i:=0 to 65 do begin writeln('<li>',i, ',', sijanimet[i],',',sijaesim[i],verbit.sijat[i].vparad,'/',verbit.sijat[i].hparad,reversestring(verbit.sijat[i].ending));       if (i in  [0,5,9,12,13,16,23,36,37,39,45]) then writeln(' ************');end;

  st:=tstringlist.create;
  res:=tstringlist.create;
  st.loadfromfile('esim.tmp');
  for i:=0 to st.count-1 do
  if i>51 then
   for j:=0 to 65 do
        if (j in  [0,5,9,12,13,16,23,36,37,39,45]) then
          res.add(st[i]+' '+trim(vsijanimet[j]));
    res.savetofile('esim.gen');

   //writeln(st[i]);
 end;
 procedure nomforms;
 var lu,i,j,k:integer;st,res:tstringlist;noms:tnominit;
 tabu:array[0..33]of array[0..33] of integer;
 begin
   fillchar(tabu,sizeof(tabu),0);
   writeln('ttttttttttttttttttttttemp');
   noms:=tnominit.create('sanatuus.csv','nmids.csv');
   writeln('<table border="1"><tr><td/>');
  //for lu:=1 to 50 do
    for i:=0 to 33 do
    writeln('<td>',noms.sijat[i].ending,'</td>');
    for i:=0 to 33 do
    begin
      writeln('<tr><td>',nsijnams[i+1],':<b>',noms.sijat[i].ending,'</b></td>');
      for j:=0 to 33 do
      begin
        for k:=1 to 50 do
        begin
            if noms.lmmids[k,i]=noms.lmmids[k,j] then tabu[i,j]:=tabu[i,j]+1;
        end;
          writeln('<td title="',j+1,nsijnams[j+1],'">', tabu[i,j],'</td>');
      end;
      writeln('</tr>');
    end;
  writeln('</table>');
  for k:=0 to 49 do if noms.lmmids[k,8]<>noms.lmmids[k,9] then
    begin writeln('<li>',k+1,' ',reversestring(noms.lmmids[k,9]));
      for i:=noms.siss[noms.luoks[k].ekasis].ekaav to noms.siss[noms.luoks[k].vikasis].vikaav do
       writeln('[',noms.avs[i].v,noms.avs[i].h,']');
    end;
   for k:=1 to 49 do if k in [27,31,36,37,45,46] then
   begin
    writeln('<li>lka:',k,' ',nexamples[k],'<ul>');
    for i:=0 to 33 do
      begin writeln('<li>',i,' '+reversestring(noms.lmmids[k-1,i])+'|'+(noms.sijat[i].ending),ifs(i in nvahvanvahvat,'*',''),ifs(i in nheikonheikot,'!!',''));end;
    writeln('</ul>');
   end;
  exit;
  st:=tstringlist.create;
  res:=tstringlist.create;
  st.loadfromfile('esim.tmp');
  for i:=0 to st.count-1 do
  if i>51 then
   for j:=0 to 65 do
        if (j in  [0,5,9,12,13,16,23,36,37,39,45]) then
          res.add(st[i]+' '+trim(vsijanimet[j]));
    res.savetofile('esim.gen');
 // 27 [t/d],  ->S/S NomSG + melk kaikki monikkomuodot
 // 31 yksi yksikössä td/vahvlka yhden yhtä, -> S/S nom sg + monikot ks
 // 36 alin  vahvojen luokkien mp/mm -vaihtelu, poikk: aliN aliNTa aliNTen
  // 37 vasen, vahv MP/MM, poikkeukset vase-N vaseNTa vaseNTen
  // 45 sadas vanv T/D yksikössä, monikossa t ja d -> S
  //46 tuhat nt/nn  , monikossa NS  ,poik: tuhat tuhatta
   //writeln(st[i]);
 end;
procedure riimei;
var riimii:triimitin;verbit:tverbit;nominit:tnominit;sanasto:tsanaSTO;//eitait:teitaivu;
begin
 exit;
 //ILKO;
// nomforms;    exit;
 //nominit:=tnominit.create('nomsall.csv','nmids.csv');
 //verbit:=tverbit.create('sanatuus.csv','vmids.csv','vsijat.csv');
 //luesanat(wfile);
 //nomforms;
 //nominit.siivoosijat;exit;
 //etsi(allresus);
 //eitait.etsilista;
 //eitait.listaa;
 //exit;
SANASTO:=tsanasto.create;
//sanasto.luesanat('sanatuus.csv');
//sanasto.listaa;
exit;
 riimii:=triimitin.create;
 riimii.tulokset;

end;
constructor triimitin.create;
 var h:thitrec;r:integer;
begin
//pilko;exit;
allresus:=tstringlist.create;
riimit:=tstringlist.create;
//exit;
//verbit:=tverbit.create('verbsall.csv','vmids.csv','vsijat.csv');
//nominit:=tnominit.create('nomsall.csv','nmids.csv');
verbit:=tverbit.create('sanatuus.csv','vmids.csv','vsijat.csv');
//verbit.listaa;
//verbit.numtowrd(4907);verbit.numtowrd(4908);verbit.numtowrd(4909);verbit.numtowrd(4910);
//randomize;
 //for r:=0 to 1000000 do  h:=verbit.numtowrd(random(5935));
   //writeln(h.lk,'/',h.si,'/',h.av,'/',h.san,'  ',h.san+19548);
   //h:=verbit.numtowrd(10);
exit;
//verbit.listaasijat;
//nominit:=tnominit.create('sanatuus.csv','nmids.csv');
  writeln('created verbit');
  etsi(allresus);
  //verbit.genlist;
  //exit;
  //  verbit.listaasijat; //   exit;
  //verbit.listaasijat;
end;

function Custom(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if reversestring(taka(list[index1]))
    > reversestring(taka(list[index2]))
    then result:=1 else result:=-1;
end;


procedure triimitin.etsi(resux:tstringlist);
var //hakunen:tvhaku;
     ha,i,ohits,hs,hm,hakuluokka:integer;hakusana:string;
     hakunen:thakunen;
     aresulist,aresulist2,HAKULISTA,uushAKUlista,ahaku:tstringlist;
     onjolist:tlist;
begin
   //exit;
 writeln('ETTSSII');
 hakulista:=tstringlist.create;
 ahaku:=tstringlist.create;
    hakulista.loADFROMFILE('haku.lst');
    //!!!TEEhakulista(hakuset,HAKULISTA);
   onjolist:=tlist.create;
   //for ha:=0 to length(hakuset)-1 do
   for hA:=0 TO hakuLISTA.count-1 do
   begin
     onjolist.clear;
     //if pos('isi',hakulista[ha])>0 then continue;
     if trim(hakulista[ha])='' then continue;
     ahaku.commatext:=hakulista[ha];
     hakusana:=ahaku[0];
     if hakusana='###' then break;
     TRY HAKULUOKKA:=strtoint(ahaku[1]); except hakuluokka:=0;end;
     //writeln('<li><em style="color:#000">HAKU:',hakusana,ha,'/',hakulista.count,'</em> ');
     if hakusana='' then continue;
     teehaku(hakunen,hakusana,hakuluokka);
     try
     //if hakuset[ha].hakunen='' then continue;
     hakusana:=reversestring(hakunen.koko);
     if length(hakusana)>24 then continue;

     if pos('''',hakusana)>0 then continue;
     aresulist:=tstringlist.create;
     onjolist.clear;
     //writeln('<li>haku:',reversestring(hakusana),'::');
    //writeln('<ul>');
     //nominit.etsi(hakunen,aresulist,onjolist);
     verbit.etsi(hakunen,aresulist,onjolist);
     //writeln('</ul>');
     //writeln('ykshakunen');
     //if resux.indexof(hakunen.koko)<0 then
     for i:=11110 to nominit.nhitlist.count-1 do
     begin
        uushakulista:=tstringlist.create;
        onjolist.clear;
      if i=0 then        WITH THIT(nominit.nhitlist[i]).SANA DO
         begin
          nominit.generate(reversestring(ALKU+AKON),reversestring(SIS),v+h,LUOKKA,uusHAKULISTA,takvok);
         writeln('<li>nomgens:',uushakulista.text);
         end;
        //if uushakulista.count>0 then
        resux.addobject(hakunen.koko,uushakulista);
     end;
     except if 1=0 then writeln('<h3>eietsinom:',hakusana,'</h3>');end;
    // writeln('etsiverbi;',hakusana);
     uushakulista:=tstringlist.create;
     onjolist.Clear;
     //exit;
     continue;
//     verbit.etsi(hakunen,aresulist,onjolist);
    // writeln('<li>didverbit:',verbit.vhitlist.count,aresulist.text,'<hr>');
     //except writeln('zzx',aresulist.Text);end;
     for i:=9990 to verbit.vhitlist.count-1 do
     begin
       onjolist.clear;
      try
      //WITH THIT(verbit.vhitlist[i]).SANA DO
//      verbit.generate(reversestring(ALKU+AKON),reversestring(SIS),v+h,LUOKKA,sananum,uusHAKULISTA,takvok);
      except writeln('<h3>nogo:;',hakusana,'/',verbit.vhitlist.Count,i ,'</h3>');end;
      writeln('<li>verbgens;;',uushakulista.Text);
      //if uushakulista.count>0 then
      resux.addobject(hakunen.koko,uushakulista);
     end;
     //     resux.addobject(hakuset[ha].hakunen,aresulist);
     //except writeln('<h3>eietsiverb:',hakusana,'</h3>');end;
   end;
   try
    writeln('<hr>HITSxxx::',resux=nil,resux.count);//,verbit.vhitlist.count,'/',aresulist.count,'</h2>');
    writeln('<li>etsitty ',tstringlist(resux.objects[0]).text);
   except writeln('<li>ppoblems resux');end;
   try
   for ha:=0 to resux.count-1 do
     begin
       writeln('<li>etsiii');
       onjolist.Clear;
       //if hakulista[ha]='' then continue;
       hakulista:=tstringlist(resux.objects[ha]);
       writeln('<li>ettipä:',resux[ha],'::',hakulista.text,'<ul>');
       for hm:=0 to hakulista.count-1 do
       begin
        //for hm:=0 to resux.count-1 do
         //begin
       //ohits:=aresulist2.count;
       //if d then writeln('<li>hae::',ha,hakulista.count,hakulista[ha]);//,ptrint(hakulista.objects[ha]),'/',aresulist2.count);
       //verbit.etsi(reversestring(hakulista[ha],aresulist2,onjolist);
        //onjolist.clear;
        aresulist2:=tstringlist.create;
        teehaku(hakunen,hakulista[hm],0);
        try
        //write('eetsverb',hakuset[ha].hakunen,'#');
       verbit.etsi(hakunen,aresulist2,onjolist);except writeln('failfind');end;
       //onjolist.clear;
       nominit.etsi(hakunen,aresulist2,onjolist);
       //writeln('<li>LÖYDETTY:',aresulist2.text);
       hakulista.objects[hm]:=tstringlist(aresulist2);
       //aresulist2.clear;
       //write('eetnom',hakuset[ha].hakunen,'#',ohits,'/',aresulist2.commatext,'::',hakulista[ha],'¤¤¤');
       end;
       try
       //if ohits<aresulist2.count then
      // resux.addobject(hakunen.koko,aresulist2);
//     resux.add('<b>'+hakulista[ha]+'</b>');
       //writeln(' <b>'+hakulista[ha]+'</b>');
       except writeln('<li>eieieie:',aresulist2.text+'___');end;
     end;
   except writeln('<li>eieieie:',aresulist2.text+'___');end;
end;

procedure triimitin.tulokset;
var i,j:integer;haksu,sss:string;aresu,ariimi:tstringlist;
begin
  writeln('<hr>tulos',allresus.count,allresus.commatext);
  writeln('<dl>');
  ariimi:=tstringlist.create;
  for i:=0 to allresus.count-1 do
  begin
      haksu:=allresus[i];
      aresu:=tstringlist(allresus.objects[i]);
      if aresu=nil then writeln('NILX');//continue;
      //if aresu.indexof(haksu)>=0 then continue;
      //writeln('<dt><b>',haksu,'</b></dt>');
      //writeln('<dd>',aresu.commatext,'</dd>');
      writeln('<dt>',haksu,'');//,aresu.commatext);
      for j:=0 to aresu.count-1 do
      if tstringlist(aresu.objects[j]).count>0 then
       writeln('<dd><b>',aresu[j],'</b>: ',tstringlist(aresu.objects[j]).text);
      //continue;
      //if aresu.indexof(haksu)<0 then writeln('<li>',haksu,'//',aresu.commatext) else
      {if 1=1 then
      begin
         tsekkaatavut(haksu,true,aresu,ariimi);
        if ariimi.count>2 then
        begin
          writeln('<dt><b>',haksu,'</b></dt>');
          riimit.addstrings(aresu);  //why?
          //for j:=0 ariimi.count-1 do
           writeln('<dd>',ariimi.commatext,'</dd>');
        end;
        ariimi.clear;
      end;}
  end;
  writeln('</dl><hr>');
  exit;
  for i:=0 to allresus.count-1 do
  begin
      haksu:=allresus[i];
      ariimi:=tstringlist.create;
      aresu:=tstringlist(allresus.objects[i]);
      //writeln('<li>',i,aresu.commatext);
      //continue;
      if aresu.count>1 then
      begin
      tsekkaatavut(haksu,true,aresu,ariimi);
      if ariimi.count>2 then  begin // riimit.addstrings(ariimi);
      writeln('<li>',haksu,': ');
          for j:=0 to ariimi.count-1 do writeln(aresu[j]+' ');
      end;
      ariimi.clear;
      aresu.Clear;
      end;
      continue;
      //procedure tsekkaatavut(hakunen:ansistring;etuko:boolean;hits,riimit:tstringlist);
      //writeln('<li>Löytyi:',(haksu),aresu.count,':<ul>');
      for j:=0 to aresu.count-1 do
      begin
        sss:=aresu[j];
        writeln('<li>:',sss,':</li>');
      end;
      writeln('</ul></li>');
  end;
   exit;
   riimit.customsort(@custom);
  writeln('<li>RIIMIT:',riimit.text,':<ul>');
  for i:=0 to riimit.count-1 do
  begin
      //haku:=riimit[i];
     if sss=riimit[i] then continue;
      writeln('<li>',riimit[i]);//reversestring(haku),':<ul>');
      sss:=riimit[i];
  end;
end;

procedure pilko;
var hf,hout:textfile;ch:ansichar;sana:ansistring;
begin
 try
assign(hf,'haku.txt');
reset(hf);
assign(hout,'haku.lst');
rewrite(hout);
 //writeln('<pre>');
 while not eof(hf) do
 begin
   read(hf,ch);
   if pos(ansilowercase(ch),'abcdefghijklmnopqrstuvxyzåäö')>0 then sana:=sana+ch else
    begin  if length(sana)>3 then writeln(hout,ansilowercase(sana));sana:='';end;
   end;

 closefile(hf);
 closefile(hout);
 except writeln('nogo');end;
end;

 procedure tsekkaatavut(hakunen:ansistring;etuko:boolean;hits,riimit:tstringlist);
var   hittavut,tofindtavut:tstringlist; etuhit:boolean;
i,tavucount:integer;
hitw:ansistring;
begin
 //writeln('%',hakunen,hits.commatext);
  hittavut:=tstringlist.create;
  tofindtavut:=tstringlist.create;
  //takako(haku,takahaku);
  hyphenfi(hakunen,tofindTAVUT);
  //writeln('<h1>tutkitavut:',haku,'|||',tofindtavut.commatext,tofindtavut.count,'</h1>');
  //writeln('<li>tutkittavat:',haku,'|||',hits.commatext,hits.count);
  //writeln('<li>hits:<b>',hits.commatext,'</b><li>',tofindtavut.commatext,',</li>');
  tavucount:=tofindtavut.count;
  for i:=0 to hits.count-1 do
  begin
      //tofind.addstrings(tavus);
//      if hits[i][1]='1' then etuhit:=true else etuhit:=false;
//      write('?',hakunen,'/',hits[i],etuko,etuhit);
//      if etuko<>etuhit then continue;
      hitw:=trim(hits[i]);//,2));
      //write('=',haku,'/',hitw,etuko,etuhit);
      if riimit.indexof(hitw)>=0 then continue;
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
          //writeln('+++++++++++',riimit.commatext);
          hittavut.clear;
        end;// else writeln('!!!!!!!!!!!!',etuhit,etuko);
        //writeln(';; ',tavus.commatext,tavus.count,'/',tavucount,' ',abs(tavus.count-tavucount) mod 2)
      end;//d  else writeln('<strike>::',hittavut.commatext,'</strike>')
    end;
  //writeln('<h4>kaikki:',riimit.commatext,'!!</h4>');
  hittavut.free;tofindtavut.free;
end;



end.

