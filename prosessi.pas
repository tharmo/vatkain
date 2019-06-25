unit prosessi;

{$mode objfpc}{$H+}
interface
uses riimiuus,
  riimiutils, Classes, SysUtils,verbikama,nominit,sanataulu,ngrams,math;
procedure uustemppu;
procedure teetemput;
var //riimii:triimitin;
  //verbs:tverbit;
  noms:tnominit;
  //muuts:tstaulu;  //muissa sanaluokissa kaikki muodot luetellaan listassa, ei generoida
procedure teegut;

implementation


//uses verbit,nominit;
uses syno,strutils;

procedure teegut;
var syns:tsynonyms;  I:INTEGER;
begin
  //OR I:=0 TO 16 DO IF I<>809 THEN WRITELN(I,' ',1+(8 - (abs(i-8))) DIV 3);EXIT;
  getngrams;exit;
  syns:=tsynonyms.create;
  // did this at first passa:
  //syns.readgutansi;
  //syns.countguts;
  //syns.gutextract;   exit;
  //syns.gutmatrix;exit;
  syns.gutsparse;
  //syns.gutkerro;

end;
type st17=string[17];

function sortto(kohdevars,kohdevals:tlist;vari,vali,vlen:byte):word;
var i,j,k:integer;//pp,vp:pointer;
begin
  try
  result:=999;
  for j:=1 to vlen do
    if integer(kohdevals[j])<vali then  //requiring 2-byte numbers
    begin
     move(kohdevars[j],kohdevars[j+1]^,(vlen-j)*8);
     move((kohdevals[j]),kohdevals[j+1]^,(vlen-j)*8);
     kohdevars[j]:=pointer(vari);
     kohdevals[j]:=pointer(vali);
     result:=j;
     exit;
    end;
  except write(^j^j,'FAIL(','/',vari,'=',vali,')-topos:',j,'  ');end;
 end;

function onksiso(pp,pv:PWORD;vari,vali:integer;veclen,wnum:word):word;//;       bsiz,bpair: array of word);
var i,j,k:integer;//pp,vp:pointer;
begin
  try
  result:=999;
  if wnum=vari then exit;
  for j:=1 to veclen do
    if (pv+j)^<vali then  //requiring 2-byte numbers
    begin
      move((pp+(j))^,(pp+((j+1)))^,(veclen-j)*2);
      move((pv+(j))^,(pv+((j+1)))^,(veclen-j)*2);
      word((pp+j)^):=vari;
      word((pv+j)^):=vali;
      //if vali>10 then write('=',j);
      //if vali>10 then for k:=0 to 3 do write('/',word((pp+k)^),'.',word((pv+k)^));
      result:=j;
      exit;
    end;
  except write(^j^j,'FAIL(','/',vari,'=',vali,')-topos:',j,'  ');end;
//    onksiso(@aclusts[0],@vclusts[0],i,vali,2,20);

end;
type twcounter=class(tobject)
   words:array[1..200] of word;
   vals:array[1..200] of word;
   tmpwords:array[1..200] of word;
   tmpvals:array[1..200] of word;
   cnt:word;
   procedure sort;
   procedure print;
   procedure clear;
   procedure add(w,val:word);
   function strength:word;
end;
  function twcounter.strength:word;
  begin
  end;
procedure twcounter.sort;
var i:integer;
begin
  for i:=1 to cnt do
        onksiso(@words[1],@vals[1],tmpwords[i],tmpvals[i],40,i);
 //for i:=3 to 39 do if vals[i]>vals[i-1] then writeln('****',words[i],'.',i,' ');
end;
procedure twcounter.clear;
var i:word;
begin
  //for i:=1 to cnt do begin words[i]:=0;
  fillchar(tmpwords,sizeof(words),0);
  fillchar(tmpvals,sizeof(vals),0);
  fillchar(words,sizeof(words),0);
  fillchar(vals,sizeof(vals),0);
  cnt:=0;
end;
   procedure twcounter.print;
   var i:integer;
   begin
     for i:=1 to cnt do
       begin
         write(words[i],vals[i],' ');
       end;
   end;

   procedure twcounter.add(w,val:word);
   var i,olijo:integer;
   begin
     if w=0 then exit;
     try
     for i:=1 to cnt do if tmpwords[i]=w then
       begin
         tmpvals[i]:=tmpvals[i]+val;
         exit;
       end;
       if cnt>39 then exit;
       cnt:=cnt+1; tmpvals[cnt]:=val;tmpwords[cnt]:=w;
      except write('failadd',w,'=',val, '/',cnt, '/');end;
   end;
procedure uustemppu;
var vees,aas,palat,vclu:tstringlist;
    vfreq,afreq:aRRAY of longword;
    vcount,acount:longword;
    tottot:longword;
    vc:twcounter;
    vst,ast:array of st17;
    vcoocs,acoocs,vacoocs:array of integer;
    var a,v,i,j,k,c,kk,ap1,ap2,ai,vi,vn,an,n,sf,v2,a2,ap1val,ap2val,vtot,round,tmp,posi,jvar,jval:integer;
     pala:string;
    f: text;
    vari,vali:word;
    clusvars,clusvals,clusvars2,clusvals2,clusvars3,clusvals3:ARRAY OF word;
    maxclusw,maxclusa:word;//array of word;
    onlist:tlist;
    cluvals,cluvars,acluvar,acluvat:tlist;
    newclus,vclusts:tlist;
    tmpp:pointer;
   // myc,myv:array of wor;
begin
  //cluvals:=tlist.create;
  //cluvars:=tlist.create;
  vees:=tstringlist.create;
  aas:=tstringlist.create;
  vees.loadfromfile('verb.n');
  aas.loadfromfile('adv.n');
  vcount:=vees.count;
  acount:=aas.count;
  setlength(vfreq,vcount);
  //setlength(maxclusw,vcount);
  //setlength(maxclusa,acount);
  writeln('huhhuh...');
  setlength(afreq,acount);
  setlength(vst,vcount);
  setlength(ast,acount);
  setlength(vcoocs,vcount*vcount);
  //setlength(acoocs,acount*acount);
  //setlength(vacoocs,acount*vcount);
  for v:=0 to vees.count-1 do
  begin
     ap1:=pos(' ',vees[v]);
     //if ap1>17 then writeln('***',vees[v]);
     vst[v]:=copy(vees[v],1,ap1-1);
     vfreq[v]:=strtointdef(copy(vees[v],ap1+1),0);
     vees[v]:=vst[v];
  end;
  for a:=0 to aas.count-1 do
  begin
     ap1:=pos(' ',aas[a]);
     if ap1>17 then writeln('***',aas[a]);
     ast[a]:=copy(aas[a],1,ap1-1);
     afreq[a]:=strtointdef(copy(aas[a],ap1+1),0);
     aas[a]:=ast[a];
  end;
  //for i:=0 to acount-1 do write('[',ast[i],']',afreq[i],' ');
  //for i:=0 to acount-1 do write(ast[i],afreq[i],' ');
  palat:=tstringlist.create;
  palat.delimiter:=' ';
  palat.strictdelimiter:=true;
  assign(f,'va.isot');
  reset(f);
  while not eof(f) do  //  ymmätään jokaiselle sanalle (adv) kaikki yhteisesiintymät (verbien) anssa
  begin
    readln(f,pala);
    palat.delimitedtext:=pala;
    ai:=aas.indexof(palat[2]);
    vi:=vees.indexof(palat[0]);
    vn:=strtointdef(palat[1],0);
    an:=strtointdef(palat[3],0);
    n:=strtointdef(palat[4],0);
    sf:=n*10 div ( 1+(vn*an div 1000000));
    vacoocs[vi*acount+ai]:=min(255,sf);

    //if sf>1500 then writeln(vst[vi],' ',ast[ai],' ',vacoocs[vi*acount+ai],'  ',sf);
  end;

  writeln('LASKETAAN YHTEISESIINTYMÄT AD*AD<-VERBS');
  for v:=0 to vcount-1 do   //   //"kerrotaan" v*v adverbeilla
    for v2:=0 to vcount-1 do //if v2<>v then
    begin
      vtot:=0;
      for a:=0 to acount-1 do
        vtot:=vtot+min(vacoocs[v*acount+a],vacoocs[v2*acount+a]);
        //sf:=n*10 div ( 1+(vn*an div 1000000));
      TRY
      vcoocs[v*vcount+v2]:=min(255,vtot);
       //if vtot>50 then  writeln(vst[v],' ',vst[v2],' ',vtot);
      EXCEPT WRITELN('ZZZ');END;
    end;
  writeln(^j^j);
  for v:=1 to vcount-1 do if vcoocs[v*vcount+v]<20 then write(vst[v],' ',vcoocs[v*vcount+v],' ');
  writeln(^j^j);
  for v:=1 to vcount-1 do if vcoocs[v*vcount+v]>70 then write(vst[v],' ',vcoocs[v*vcount+v],' ');
  writeln('ggggggggggggggggggggg');
  for a:=0 to acount-1 do //kerrotaan" a*a verbeillä
  begin
    tottot:=0;
    for a2:=0 to acount-1 do //if a2<>a then
    begin
      vtot:=0;  //po. atot
      for v:=0 to vcount-1 do  vtot:=vtot+min(vacoocs[v*acount+a],vacoocs[v*acount+a2]);
      acoocs[a*acount+a2]:=min(255,vtot);
      tottot:=tottot+vtot;
        //if vtot>20 then  writeln(ast[a],' ',ast[a2],' ',vtot,' ',acoocs[a*acount+a2]);

    end;
    //writeln(ast[a],' ',acoocs[a*acount+a], ' ',tottot,'                  ',1000*acoocs[a*acount+a] div (tottot+1));
  end;
  maxclusa:=0;
  setlength(clusvars,acount*40);
  setlength(clusvals,acount*40);
  setlength(clusvars2,acount*40);
  setlength(clusvals2,acount*40);
  setlength(clusvars3,40*40);
  setlength(clusvals3,40*40);



  write(^j,^j,'VALITAAN ISOIMMAT YHTEISESIINTYMÄT HARVAMATRIISIIIN' );
  //for round:=1 to 50 do
  for i:=0 to acount-1 do  //tehdään sparce matrix
  begin
    tmp:=0;
    try
    //write(^j,^j,'::' ,ast[i]);
    for j:=0 to acount-1 do
    begin
       onksiso(@clusvars[i*40],@clusvals[i*40],j,acoocs[i*acount+j],20,i);
     end;
    clusvars[i*40]:=i;
    for j:=1 to 10 do tmp:=tmp+clusvals[i*40+j];
    clusvals[i*40]:=tmp;
 except write('%');end;
  end;
  try
  except writeln('nomax:',maxclusw);end;


  writeln(^j,' KLUSTERINVATKAUSTA ///////////////////////////////////////');
  onlist:=tlist.create;
  vc:=twcounter.create;
  onlist.clear;
  for round:=1 to 70 do
  begin
    //writeln(round);//(^j,^j,round);
    maxclusa:=0;maxclusw:=0;
    //for j:=0 to onlist.count-1 do write('-',ast[integer(onlist[j])]);
    for i:=0 to acount-1 do
    begin
       try
        if acoocs[i*acount+i]<50 then continue;
        if onlist.indexof(pointer(clusvars[i*40]))>=0 then
        begin //write('--',ast[clusvars[i*40]]);
         continue;end; //ei käytettyjä
         //  onlist.indexof(pointer(clusvars[maxclusw*40+j])
        //if clusvals[i*20]<30 then continue;
        //for j:=0 to 5 do
        //writeln;
        vc.clear;
        except writeln('noiii:',vc.cnt,'.',j);end;
        for j:=0 to 15 do
        begin
          try
           ap1:=clusvars[i*40+j];

          except writeln('noapclus',ap1,'.',i,' ',j);end;
           if onlist.indexof(pointer(ap1))>=0 then
                   begin //write('{',ast[clusvars[ap1*40]],'}');
                    continue;end; //ei käytettyjä
          try
          ap1val:=clusvals[i*40+j];
           if ap1=0 then continue;
           vc.add(ap1,ap1val);// clusvals[i*20+j]);
           except writeln('noadd',ap1,'.',ap1val);end;
           try
           for k:=0 to 8 do
           begin
             ap2:=clusvars[ap1*40+k];
              if onlist.indexof(pointer(ap2))>=0 then begin continue;write('..');continue;end;
             ap2val:=min(clusvals[ap1*40+k],ap1val);
             vc.add(clusvars[ap1*40+k],ap2);//clusvals[ap1*20+k]);
             try
             for kk:=1 to 3 do
              if onlist.indexof(pointer(clusvars[ap2*40+kk]))>=0 then //ei oteta enää uusia 3-tasolta?
               vc.add(clusvars[ap2*40+kk],min(clusvals[ap2*40+kk],ap2val));//clusvals[ap1*20+k]);
             except writeln('nokk ',ap2,', ',ap2val);end;
           end;
          except writeln('nojii',ap1,'.',k);end;
        end;
        try
        if vc.cnt<5 then continue;
        vc.sort;
        vtot:=0;
        //for k:=1 to 10 do vtot:=vtot
        tmp:=0;
        for j:=1 to min(10,vc.cnt-1) do
          tmp:=tmp+clusvals[i*40+j];
        { writeln(^j);
         //if vc.words[1]<>i then write(^j,'-------------',ast[i]);
         write(ast[clusvars[40*i]],'  ' ,clusvals[i*40],'  :: ');
        //if vc.vals[j]>4 then
        begin
           // write(ast[clusvars[40*i+j]],'  ' ,clusvals[i*40+j],' ');
           //for k:=1 to 10 do
        end;
        write('  (',tmp,')  ');
        }
        if maxclusa<tmp then
        begin
            maxclusw:=i;maxclusa:=tmp;
            //writeln(^j, '**************max:',ast[clusvars[i*40]],tmp,' ',onlist.indexof(pointer(clusvars[i*40])));
            //write(' ',onlist.indexof(pointer(clusvars[i*40]);             // for j:=1 to 3 do write(ast[clusvars[i*40+j]],clusvals[i*40+j],' ');
            // writeln(^j,'MAX:',ast[maxclusw],maxclusw,':');
        end;
            //tmp:=vc.strength;
            //if maxclusw<tmp then begin write('!!MAX',i);maxclusw:=i;maxclusa:=tmp;end;
        except writeln('no:',vc.cnt,'.',j);end;
    end;
    //writeln(^j,'max=',ast[maxclusw],maxclusw,' ', '@round ',round);
    tmp:=0;
    for j:=0 to 39 do
    begin
       if onlist.indexof(pointer(clusvars[maxclusw*40+j]))>=0 then continue;//write('***');
       //write(ast[clusvars[maxclusw*40+j]],' ');//,clusvals[maxclusw*40+j],' ');
       if tmp<5 then
         onlist.add(pointer(clusvars[maxclusw*40+j]));
        clusvars2[round*40+tmp]:=clusvars[maxclusw*40+j];
        clusvals2[round*40+tmp]:=clusvals[maxclusw*40+j];

      inc(tmp);
       if tmp>10 then break;
    end;
    //writeln;
   //    if maxclusv<vcoocs[i*vcount+j] then clustv[round]
  end;
  fillchar(clusvars[0],length(clusvars)*2,0);
  fillchar(clusvals[0],length(clusvals)*2,0);
  writeln('cccccccccc222222222222222222222222222');
  newclus:=tlist.create;
  for i:=0 to 40 do newclus.add(tstringlist.create);
  vclusts:=tlist.create;
  for i:=0 to 40 do vclusts.add(tstringlist.create);
    //for i:=0 to 40 do //jokaiselle sanalle     WRITE(   ast[CLUSVARS2[i*40]],ast[CLUSVARS2[i*40+1]],' ');
  for i:=0 to acount-2 do //jokaiselle sanalle
  begin
    for j:=1 to 39 do //tutkitaan kuinka se sopii kuhunkin klusteriin
    begin
      jvar:=clusvars2[j*40];
      if jvar=0 then continue;
      jval:=clusvals2[j*40];
      vtot:=0;
      for a:=0 to 20 do //tutkittavan klusterin jäsenet
        if clusvars2[j*40+a]<>0 then
          vtot:=vtot+acoocs[i*acount+clusvars2[j*40+a]];//niiden korrelaatiot i:n kanssa
      vtot:=(100*vtot) div (1+jval);
      posi:=onksiso(@clusvars[i*40],@clusvals[i*40],j,vtot,20,i);
      //if posi<>999 then write(posi,ast[clusvars2[clusvars[i*40+posi]*40]],vtot,'.','  ');
    end;
    for k:=1 to 3 do
    begin
      if clusvals[I*40+k]*2<clusvals[I*40+1] then continue; //SELVÄSTI PIENEMPI KUIN PARAS
      tmp:=clusvars[i*40+k]; //parhaan, toiseks kolm parhaan nykysanan klusterin mumero
      tstringlist(newclus[clusvars[i*40+k]]).addobject(ast[i],tobject(pointer(i)));
      if (tmp>39) then writeln(^J,'LIIANISOKLUSTERINNUMERO');
      onksiso(@clusvars3[tmp*40],@clusvals3[tmp*40],i,CLUSVALS[I*40+K],20,99);
      //if k=1 then WRITE(^J,ast[I],'->',ast[clusvars3[tmp*40+1]],ast[clusvars[i*40+k]],' ',CLUSVALS[I*40+K]);
    end;
  end;


  for i:=1 to acount-1 do vclusts.add(tstringlist.create);

  {begin
   for j:=1 to 39 do
     if clusvals[i*40+j]<1 then break else
     begin
      tmp:=clusvars[i*40+j];  //sanan klusterit
      for k:=1 to 3 do      //kolme parasta
        posi:=onksiso(@clusvars3[tmp+k],@clusvals3[tmp+k],clusvars2[i*40+j],clusvals[i*40+j],20,i);
       //sortto(tlist(cluvars[j]),tlist(cluvals[j]),clusvars2[i*40+k],clusvars2[i*40+k],39);
     end;
   end;
   }
  fillchar(clusvars2[0],length(clusvars2)*2,0);
  fillchar(clusvals2[0],length(clusvals2)*2,0);
  {WRITELN('LISTAA KLUSTERIT');
  for i:=1 to 39 do
  begin
    write(^j,i,'***');
    for j:=1 to 10 do //tutkitaan kuinka se sopii kuhunkin klusteriin
    begin
     write(ast[clusvars3[i*40+j]],clusvals3[i*40+j],' ');
    end;
  end;
  }


  writeln(^J,'ADVERBIJENGEILLE VERBIKAVERIT');
  for c:=1 to 39 do //kullekin klusterille etsitään parhaat verbit
  begin
    vtot:=0;
    writeln(^j,'**',tstringlist(newclus[c]).commatext);
    palat:=tstringlist(vclusts[c]);
    //for a:=1 to acount-1 do
    //for a:=1 to 10 do write(clusvars[c*40+a],ast[clusvars[c*40+a]],' ');
    //writeln(^j,tstringlist(newclus[c]).commatext);
    for v:=1 to vcount-1 do
    begin //WRITE(V,' ');
     TRY
     vtot:=0;
     for a:=1 to 10 do //tstringlist(newclus.count-1 do    //vtot:=vtot+acoocs[i*acount+clusvars2[j*40+a]];//niiden korrelaatiot i:n kanssa
        vtot:=vtot+vacoocs[v*acount+clusvars3[c*40+a]];//niiden korrelaatiot i:n kanssa
     //vtot:=(vtot * 10) div vcoocs[v*vcount+v2];
     posi:=onksiso(@clusvars2[c*40],@clusvals2[c*40],v,vtot,20,0);
     //if vtot>30 then write(' (',vst[v],vtot,')');//,clusvars2[c*40+1],':',clusvals2[c*40+1],')')
     EXCEPT WRITELN('nocountverbs');end;
    end;
    for a:=1 to 10 do
    begin
      tmp:=clusvars2[c*40+a];
      if a<5 then write('[',vst[tmp],tmp,'] ');
      if tmp>0 then
      palat.addobject(vst[tmp],tobject(pointer(tmp)));
    end;
  end;
   for i:=1 to 38 do //jokaiselle sanalle
   begin
     writeln(^j,tstringlist(newclus[i]).commatext);
     writeln('*',tstringlist(vclusts[i]).commatext);
   end;
   exit;
  for i:=9990 to 60 do //jokaiselle sanalle
  begin
    write(^j,i,'***');
    for j:=0 to 10 do //tutkitaan kuinka se sopii kuhunkin klusteriin
    begin
     //write(ast[clusvars2[i*40+j]],clusvals2[i*40+j],' ');
    end;
  end;

end;

procedure teetemput;
var   riiminaiheet,riimaavat:tstaulu;sl:tlist;muolist,riilist,nums:tstringlist;syns:tsynonyms;synarr:tsynarray;
   i,siz:integer;
   sans:tstringlist;
   sims:tsimmat;
   //wf:tfilestream;
   hlist,hplist,glist,glist2,gplist,gplist2:tlist;
   asani:tsanainfo;
   function sltext:string;
   var i:word;
   begin
      result:='';
      for i:=0 to sl.count-1 do result:=result+' '+sans[integer(sl[i])];
      writeln('///',integer(sl[i]),sans[integer(sl[i])]);
   end;
   procedure aho(w:string);
   begin hlist.add(pointer(sans.indexof(w)));
       hplist.add(pointer(10));
     writeln('///',w,sans.indexof(w));
   end;
   var inf:text;ss,sp:string;
    var ocs:array of shortint;anum:double;aint,rows,cols,j,w:integer;
       coocs:array of integer;   asho:shortint;
       //advst,verbst:
       rownames,colnames:  tstringlist;
       bsiz,bpair: array of word;
   procedure bigs(ii,jii,vv:integer);//;       bsiz,bpair: array of word);
  // function big64(bigs,bigvals:pword;bwrd,wrd,freq:word;sl:tstringlist):word;
   var i,j,k:integer;pp,vp:pointer;
       z:byte;
   begin
     try
     z:=2;
     pp:=@bpair[ii*50]; //ignore pos zero
     vp:=@bsiz[ii*50];
     for j:=1 to 49 do
       if bsiz[ii*50+j]<vv then
       begin
         move((pp+z*(j-1))^,(pp+z*j)^,(50-j)*z);
         move((vp+z*(j-1))^,(Vp+z*j)^,(50-j)*z);
         bpair[ii*50+j]:=jii;
         bsiz[ii*50+j]:=vv;
         exit;
       end;
     except write(^j^j,'FAIL(',ii,'/',jii,'=',vv,')-topos:',j,'  ');end;
   end;



  procedure prod(kohde:string);
var i,j,k,m,nn:integer;inu,isi,jnu,jsi,knu,maxval,maxvar,round:word;
    abig,avar,asiz,msiz:word;
   // bsiz,bpair: array of word;
    inbigs,bigatts,mybigs,mybigs2,mysizs,mysizs2:tlist;
    clusters,clusters2:tstringlist;
begin
  clusters:=tstringlist.create;
  clusters2:=tstringlist.create;
  inbigs:=tlist.create;
  bigatts:=tlist.create;
  //advst.loadfromfile('ava.names');
 // havaintojen sanat talletettu erikseen listaan jota kutsutaan advst:ksi oli kyse adverbeizta tai verbeistä.. lazy, bad
  writeln(^j,'rows;',rows,'cols:',cols);
  setlength(bsiz,rows*50);
  setlength(bpair,rows*50);
  setlength(coocs,rows*rows);
 // procedure trybigs(ii,jii,vv:integer;pvars,psiz:pointer;isiz:byte);//;       bsiz,bpair: array of word);
 //ybigs(1,2,3,pointer(bpair),pointer(bsiz),2); trybigs(1,3,1,pointer(bpair),pointer(bsiz),2); trybigs(1,4,5,pointer(bpair),pointer(bsiz),2); trybigs(1,7,2,pointer(bpair),pointer(bsiz),2); for i:=1 to 4 do write(bpair[1*50+i],':',bsiz[1*50+i],'   ');  readbin(n,50,2,bpair,kohde+'var.bin');
 readbin(rows,50,2,bsiz,kohde+'siz.bin');
 readbin(rows,50,2,bpair,kohde+'var.bin');
  writeln('gogogog ',kohde,' ',rows);
  //readbin(n,50,2,bpair,'aaa.bin');
  //readbin(n,50,2,bsiz,'aas.bin');
  for i:=9991 to rows-1 do
   begin
    try
    write(^j^j,rownames[i],i,': ');
    for j:=1 to 50 do
    if bsiz[i*50+j]>0 then write(' ',rownames[bpair[i*50+j]],bpair[i*50+j],'=',bsiz[i*50+j]);
    except writeln('fail:',i,'@',bpair[i*50],' ',i*50);raise;end;

  end;
  for i:=1 to rows-1 do
  begin
    //inum:=5
    for j:=1 to 49 do
    begin
      try
      jnu:=bpair[i*50+j];jsi:=bsiz[i*50+j];
      except write('?',i*50+j);raise;end;
      coocs[i*rows+j]:=0;
      if jnu<>0 then
      for k:=1 to 49 do   //kaikki  j:n korrelaatit
      begin
        try
        knu:=bpair[jnu*50+k]; //yks jiin
        for m:=1 to 49 do  //kaikki (muut) i:n korrelaatin
        begin
          try
          if i<>j then
          if bpair[i*50+m]=knu then  // oli myös i:n korrelaatti
          //write(k);

          begin
            coocs[i*rows+jnu]:=coocs[i*rows+jnu]+min(bsiz[j*50+k],bsiz[i*50+m] div 10);
          end;
          except write('???',i*rows+jnu,' =',i,';',jnu,' #',coocs[i*rows+jnu],'+',min(jsi,bsiz[i*50+k]),'!');raise;end;
        end;
        except write('??',i*50+jnu,' =',i,';',jsi);raise;end;

      end;
    end;
  end;
  msiz:=0;
  for i:=0 to rows-1  do write('  ', coocs[i*rows+i]);
  writeln;
  for i:=0 to rows-1  do
  for j:=0 to rows-1  do
   if coocs[i*rows+j]>msiz then
    begin msiz:=coocs[i*rows+j];write(msiz,rownames[i],rownames[j],'@',i,' ');end;
  exit;
  writeln('KERRO itsellään: rows*rows  .. ',rows);
  for round:=1 to 100 do
  begin
    maxval:=0;
    maxvar:=0;
    setlength(bsiz,0);
    setlength(bpair,0);
    setlength(bsiz,rows*50);
    setlength(bpair,rows*50);
    for i:=1 to rows-2 do
    if inbigs.indexof(pointer(i))<0 then
     begin
      try
      //write(i,'.');
      for j:=1 to rows-2 do
      if inbigs.indexof(pointer(j))<0 then
       bigs(i,j,coocs[i*rows+j]);//,bpair,bsiz);
      except writeln(' :',i,'@',bpair[i*50],' ',i*50);raise;end;
      if bsiz[i*50+1]>maxval then
      begin
        maxval:=bsiz[i*50+1];
        maxvar:=i;
      end;
    end;
    // writeln('yksveivausohi;max;',maxvar);
    for j:=1 to 10 do
      if  bsiz[maxvar*50+j]<bsiz[maxvar*50+1] div 2 then
       inbigs.add(pointer(0)) else
       inbigs.add(pointer(bpair[maxvar*50+j]));
    //for j:=0 to inbigs.count-1  do write('++',rownames[integer(inbigs[j])]);
    //write(^j^j,rownames[maxvar],'::');
    mybigs:=tlist.create;
    for j:=1 to 50 do //if bsiz[maxvar*50+j]>0 then
         if  bsiz[maxvar*50+j]>=bsiz[maxvar*50+1] div 2 then
         begin
          MYBIGS.add(pointer(bpair[maxvar*50+j]));
          MYBIGS.add(pointer(bsiz[maxvar*50+j]));
         // write(' ',rownames[bpair[maxvar*50+j]],' ');//,bsiz[maxvar*50+j]);
         end;
    clusters.addOBJECT(rownames[maxvar],MYBIGS);
    //ideana että veivataan vielä kaikki sanat klustereiden kautta

  end;
  //koitetaan veiviä, ei ihan helppo
  setlength(bsiz,0);
  setlength(bpair,0);
  setlength(bsiz,rows*50);
  setlength(bpair,rows*50);
  inbigs.clear;
  for round:=1 to 1 do
  begin
    for i:=0 to CLUSTERS.COUNT-1 do //downto 0  do
    begin
      mysizs:=tlist.create;
      //MYSizs.add(pointer(bpair[maxvar*50+j]));
      msiz:=integer(mybigs[1]);
      write(^j^j,clusters[i],msiz,' ',coocs[integer(mybigs[2])*rows+integer(mybigs[4])]);
      MYBIGS:=tlist(Clusters.objects[i]);
      for j:=1 to (mybigs.count div 2) -1 do write(' .',rownames[integer(mybigs[j*2])],'.',integer(mybigs[j*2+1]));
      write(^j,'->',clusters[i],msiz);
      for j:=1 to rows-2 do
      begin
        isi:=0;
       for k:=0 to (mybigs.count div 2) -1 do
       begin
        try
        abig:=integer(mybigs[k*2])+10;
        if abig<>j then
        if integer(abig)>0 then  //tyhjä
        if inbigs.indexof(mybigs[k*2])<0 then
        begin
          asiz:=integer(mybigs[k*2]);
          isi:=isi+1000*(coocs[j*rows+abig] div (msiz));

        end;
       except write('nogo ',isi,' ',integer(mybigs[k*2]));end;
       end;
       bigs(i,j,isi);//,bpair,bsiz);
      end;
      //for j:=1 to 10 do cluster2.addobject(rownames[bpair[i*50+j]]);
      for j:=1 to 20 do write(' .',rownames[bpair[i*50+j]],bsiz[i*50+j]);
      for j:=1 to 10 do inbigs.add(pointer(bpair[i*50+j]));
    end;

  end;
  exit;
  for i:=CLUSTERS.COUNT-1 downto 0 do
  begin
     write(^j,clusters[i]);
    MYBIGS:=tlist(Clusters.objects[i]);
    for j:=0 to mybigs.count-1 do   write(' ',rownames[integer(mybigs[j])]);
  end;
  exit;
  for i:=0 to (inbigs.count-1) div 15 do
  begin
    abig:=(i div 15);
    for j:=0 to 14 do
    begin
      avar:=integer(inbigs[abig]);
      isi:=0;
      for k:=1 to rows-2 do isi:=isi+coocs[50*avar+k];
      //bigats.add(pointer(bpair[maxvar*50+j]));
      //trybigs(ii,jii,vv:integer;pvars,psiz:pointer;isiz:byte);//;       bsiz,bpair: array of word);

    end;
  end;
  exit;
  //for i:=1 to n-1 do
  for i:=maxvar to maxvar do
   begin
    try
    if bsiz[i*50+1]<2600 then continue;
    write(^j^j,rownames[i],i,':::: ');

    for j:=1 to 100 do
    if bsiz[i*50+j]>1000 then
    write(' ',j,rownames[bpair[i*50+j]],'==',bsiz[i*50+j]);
    except writeln('fail:',i,'@',bpair[i*50],' ',i*50);raise;end;
  end;

end;
 var kohde:string;
begin
  //wf.create('substantiivit.iso', fmsharedenynone);

  //assign(inf,'klk_tofiana2.lst');
  //reset(inf);
 //n:=100;
  write('alkaany');
  rownames:=tstringlist.create;
  kohde:='vas'; //avs - adverbit havaintoina, verbit muuttujina
  colnames:=tstringlist.create;
  rownames.LoadFromFile(kohde+'.lst');
  if kohde='avs' then  colnames.loadfromfile('vas.lst') else
  colnames.loadfromfile('avs.lst');
  rows:=rownames.count;
  cols:=colnames.count;
  prod(kohde);  exit;
 // pihalla: rows.cols? kohde?
  if kohde='avs' then  colnames.loadfromfile('avs.lst') else
  colnames.loadfromfile('vas.lst');
  if kohde='avs' then  rownames.loadfromfile('vas.lst') else
  rownames.loadfromfile('avs.lst');
  rows:=rownames.count;
  cols:=colnames.count;
  // havaintojen sanat talletettu erikseen listaan jota kutsutaan advst:ksi

  if kohde='vas' then assign(inf,'avs.iso')  //pihalöla kuin lumiukko .. cols ja rows???
  else  assign(inf,'vas.iso');
  writeln(kohde+'.iso');
  reset(inf);
  readln(inf,ss);
 // advst.commatext:=ss; advst.Delimiter:=^j; advst.SaveToFile('avs.lst');   writeln(advst.delimitedtext);exit;


  i:=1;
  nums:=tstringlist.create;
  readln(inf,ss);

  //colnames.commatext:=ss;  //ekalla rivillä muuttujanimet (adverbit)
  //writeln(advst.commatext);exit;
  //verbst.loadfromfile(kohde+'st.lst');
  //cols:=colnames.Count;
  setlength(ocs,cols);
  setlength(coocs,cols*cols);  //kerrotaan muuttujat keskenään
  setlength(bsiz,cols*50);
  setlength(bpair,cols*50);
  w:=0;
  while not eof(inf) do
  begin
   readln(inf,ss);
   nums.commatext:=ss;
   //verbs.add(nums[0]);
   //write(^j^j,nums[0],': ',verbst[w]);
   for i:=1 to nums.Count-1 do
   begin
     //try     anum:=int(100*strtofloat(nums[i]));except writeln(nums[i],'!',w);end;
     try
     //aint:=round(anum);
     asho:=(round(2*(strtofloat(nums[i]))));
     if asho<0 then continue;
     except writeln('Ii:',i,'/',nums[i],'_');end;
     try
     //coocs[w*n+i]:=asho;
     //if asho>0 then write(advst[i],asho,' ');
     //ocs[i]:=asho;
     //writeln(cols,' ',nums.count);
     for j:=1 to nums.Count-1 do //nums=cols  or should be
      begin
        try
       coocs[i*cols+j]:=coocs[i*cols+j]+abs(asho*(round(2*(strtofloat(nums[j])))));
       //if i=j then write(^j,i,'  ',advst[i],':',coocs[i*n+j],' = ',asho,' /  ',nums[i],'=',nums[j]);

         //write(coocs[i*n+j],'_',asho, '   ');
         except writeln('jii:',w,'/',i,'_',asho,'  ',j,'.',nums[j],'  ',coocs[i*cols+j]);end;
      end;
     except writeln('nogo:',w,'/',i,'_',asho,'  ',j,'.');end;
     //if asho>50 then     write(vnams[i],aint, ' ');
   end;
   //write (' ',vnams[w]);
   inc(w);
   //if w>=50 then break;
   END; //continue;
   writeln('*****************');
   FOR I:=1 TO cols-2 DO
   begin
      write(^j^j,colnames[i],i,'::: ');
      for j:=1 to cols-2 do
      begin
        //write(' ',vnams[j], coocs[i*n+j]);
        if coocs[i*cols+j]>10 then
        begin
          bigs(i,j,coocs[i*cols+j]);//,bpair,bsiz);
          //write(advst[j],coocs[i*n+j],'. ');
        end;
      end;
      for j:=1 to 10 do write(' ',colnames[bpair[i*50+j]],'=',bsiz[i*50+j]);
   end;
   //procedure savebin(rows,cols,usiz:integer;arr:array of word;fn:string) ;
   savebin(cols,50,2,bpair,kohde+'var.bin');
   savebin(cols,50,2,bsiz,kohde+'siz.bin');
   //advst.savetofile('ava.names');
   //savebin(n,50,2,bpair,'aaa.bin');
   //savebin(n,50,2,bsiz,'aas.bin');
   //advst.savetofile('aaa.names');
   exit;
   {BEGIN
   if (lowercase(ss)<>ss) then continue;
   if pos(' ',ss)>0 then continue;
   if pos('-',ss)>0 then continue;
   if ss<sp then writeln(sp,' / ',ss);
   sp:=ss;
   //if i mod 1000=1 then write(sp);
  end;}
  exit;
  assign(inf,'klk_tofiana2.lst');
  reset(inf);
  sans:=tstringlist.create;  //pitäis laittaa classiin
  sans.Duplicates:=dupignore;
  sans.sorted:=true;
  i:=0;
  while not eof(inf) do  //erillinen tmp testi
  begin
     readln(inf,ss);
     //ss:=copy(ss,1,pos(' ',ss)-1);
     sans.add(ss);
     i:=i+1;//if i mod 5000=1 then write(sans.count,ss, ', ');
  end;
  writeln(sans.commatext);
  exit;
  sans.loadfromfile('sanatvaan.ansi');
  //sans.insert(0,'');
  glist:=tlist.create;
  glist2:=tlist.create;
  gplist:=tlist.create;
  gplist2:=tlist.create;
  hplist:=tlist.create;
  hlist:=tlist.create;
  //aho(690);
  sans.insert(0,'vaarinhousut');
  aho('sauna');
  aho('kiuas');
  aho('löyly');
  //aho('kuuma');
  aho('helvetti');
  aho('kärsimys');
  aho('nautinto');
  aho('autuus');
  //aho('olut');
  aho('kärsimys');
  aho('pätsi');
  //aho('olla');
  //aho('sisäänlämpiävä');
  {aho('helvetti');
  aho('kärsimys');
  aho('nauttia');
  aho('helvetti');
  aho('kärsimys');
  aho('synti');
    aho('ihana');
  aho('nautinto');
  aho('palella');
  aho('jää');
  //aho('maksaa');
  aho('sauna');
  aho('kuolla');
  aho('nauttia');
  aho('pakkanen');
  aho('kuuma');
  aho('helvetti');
  aho('ikuinen');
  aho('autuus');
  aho('tuska');
  Aho('löyly');
  aho('kiuas');
  aho('alaston');
  //aho('kaunis');

  //aho('kasvo');
  //aho('kasvi');
  //aho('kasvaa');
  //aho('kissa');
  //aho('pissa');
  //aho('tähti');
  }
 // sims:=tsimmat.create(sans.count-1,64,'wvars',sans);
 // sims:=tsimmat.create(sans.count-1,64,'wvars',sans);
  sims:=tsimmat.create(27550,64,'wvars1',sans);
  //sims.kerro(64,glist2,sans,nil);  exit;
  //listgrams;
  //sims:=tsimmat.create(27550,64,'wvars',sans);
{  writeln('<hr>garmlöist');
  sims.haegramlist(64,hlist,hplist,sans,glist,gplist);
  //sims.list(sans);
  for i:=0 to glist.count-1 do writeln('>',sans[integer(glist[i])],integer(gplist[i]));//,integer(glist2[i]));
  writeln('<<hr>tuttuja:',glist.count,'/',gplist.count);
}  //glist2:=glist;
{  sims.haegramlist(16,glist,gplist,sans,glist2,gplist2);
  writeln('<<hr>tututuu:',glist2.count,'/',gplist2.count);
  for i:=0 to glist2.count-1 do writeln('+',sans[integer(glist2[i])],integer(gplist2[i]));
}  if 1=0 then
  for i:=0 to glist2.count-1 do
  begin
      siz:=integer(gplist2[i]);
      if siz>10 then
      writeln('|<b style="color:red;font-size:',min(8,siz div 200),'em;margin:-4em;margin-left:0em;opacity:0.5">',sans[integer(glist2[i])],'</b>') //,integer(glist2[i]));
      else  if siz>5 then
        writeln('<em style="font-size:',min(80,max(12,siz div 5)),'px;">',sans[integer(glist2[i])],'</em>') //,integer(glist2[i]));
      {else if integer(gplist2[i])>100 then
      writeln('|<b style="color:green;font-size:',siz div 100,'em">',sans[integer(glist2[i])],siz,'</b>') //,integer(glist2[i]));
      else if siz>50 then
      writeln('|<b>',sans[integer(glist2[i])],siz,'</b>') //,integer(glist2[i]));
      else if integer(gplist2[i])>0 then
      writeln('|',sans[integer(glist2[i])],siz);//,integer(glist2[i]));}
  end;
  //exit;
  //sims.list(sans);
  writeln('<hr>tutuntuttuja:',hlist.count);
  glist.clear;gplist.clear;
  sims.kerro(64,hlist,hplist,sans,glist,gplist);
  sims.kerro(64,glist,gplist,sans,glist2,gplist2);
  //GLIST:=GLIST2;gplist2:=gplist;
  writeln('<hr>');
  //for i:=0 to glist.count-1 do if integer(gplist[i])>10 then writeln('&lt;',sans[integer(glist[i])],integer(gplist[i]));//,integer(glist2[i]));
  writeln('<hr>');
  //exit;
 // for i:=0 to glist.count-1 do writeln(sans[integer(glist[i])]);//,integer(glist[i]));
  writeln('<hr>');
//!  syns.gutcoocs;//exit;
//!  writeln(sans[0],'<li>createsyns');
  //tmp_num;exit;
  //!  syns:=tsynonyms.create;
  //syns.makelist;writeln('list saved to syn.bin');exit;
  //syns.coocs;exit;
  //syns.gutenberg;exit;
  //syns.luegut;exit; //gutenberg concordance is very low quality. Redo completely .. later! proceed with wiktionary related words & synonyms
 // SYNS.read('synmul.bin');  // binääri sanat*32 listaa kullekin sanalle liittyvät sanat sanatuus.csv (taivutuskaavat)-järjestysnumeroilla
  //SYNS.kerro; //lisätään liittyvien liittyvät
  writeln('<li>luo sanasto');
  SANASTO:=tsanasto.create;  //luo myös globaalit verbit, nominit, muodot
  sanasto.slist:=sans;
  sanasto.readadjbin;
  glist.clear;
  glist.add(pointer(19982));
  writeln('generoi:');
  sanasto.generatelist(glist,true);
  writeln('gggggg');
  for i:=0 to sanasto.resutaulu.wcount-1 do
  begin
    asani:=sanasto.resutaulu.taulu[i];

    writeln('<li>',reversestring(asani.sana));//,' ',asani.sija);
  end;
  exit;
  exit;
  //jostain syystä käyttää tiedostoa NOMSALL.CSV vaikka kaikki on mukana SANATUUS.CSV'ssä.. EI KÄYTÄ
  //******'sanatuus.csv','vmids.csv','vsijat.csv';
  //****  'nomsall.csv','nmids.csv');
  //writeln('<li>lue hakusanat',verbikama.vesims[1]);
//!!  riiminaiheet:=tstaulu.create('haku.lst',nil);  //mitä vittua, vain muutamaa sanaa varten
  //*** SIEMENSANAT TIEDOSTOSTA HAKU.LST
  writeln('<h3>//*hae sananumerot perussmuodoille:</h3>');
//!  sl:=riiminaiheet.numeroi;
//  writeln('<hr><h3>haku:</h3>',sltext,'<hr>');
//!!  syns.haesynolist(sl,sans);
  //writeln('<h3>s1:</h3><hr>',sltext);
//!!    syns.haesynolist(sl,sans);

  //writeln('<h3>s2:</h3><hr>',sltext);
 //! writeln('<h1>gotsyns:',sl.count,'</h1>');
 //! for i:=0 to sl.count-1 do writeln((sans[integer(sl[i])]));
  //muolist:=tstringlist.create;

  riilist:=tstringlist.create;
  writeln('<h3>generatelist:',glist2.count,'</h3>');
  sanasto.generatelist(glist2,false);  //taivuttaa kaikki ja laittaa taivutetut sanat muolistiin, sananumerot objekteihin
  writeln('<li>got:',sanasto.resutaulu.wcount);
  //sanasto.addtolist('koskaan');
  //for i:=0 to sanasto.resutaulu.wcount-1 do writeln('<li>resta:',reversestring(sanasto.resutaulu.taulu[i].sana),'#',sanasto.resutaulu.taulu[i].sija);
  //writeln('zxczzzzzzzzz');
  //EXIT;
  //for i:=0 to muolist.count-1 do try writeln(' ',reversestring(muolist[i]),':',sans[integer(pointer(muolist.objects[i]))],'!!');except writeln('#'); end;
  //for i:=0 to muolist.count-1 do writeln('<sub>',reversestring(muolist[i]),integer(pointer(muolist.objects[i])),'</sub>');exit;
   // writeln('<hr><small>taivutettuja:',muolist.text,'</small>');
   //muolist.insert(0,'');
   //riimaavat:=tstaulu.create('',muolist);
   riimaavat:=tstaulu.create('',muolist,sans);
  //riimaavat.listaa;
  //tstaulu on otus joka sisältää trien sanoista (vs. lemma) joiden keskenäisiä riimejä haetaan. Myös taipumattomat sanat (adverbit yms) hoidetaan tstauluna.
  writeln('<h3>//* riimaa muolist &lt; riimaavat.slista</h3>');
  riimaavat.riimaa;  //hae keskenäiset riimit
  //writeln('<h3>riimattu > miköä vitun riilista=</h3>');
  //writeln('<hr>',riilist.text);
  writeln('<li>//*hae synonyymit');
  writeln('<li>//*generoi taivutusmuodot');
  writeln('<li> //* tee hakutaulu');

  writeln('<li>// ... valitse hyvin riimautuvat sanat, hae niille yhdessä esiintyviä sanoja, niille synonyymeja, generoi muodot, riimitä uudestaan');
  writeln('<li>// .. karsi taas sanoja, hae alkusointuja ja puolisoituja, riimitä uudestaan');
  writeln('<li>// ... sitten lauseiden rakentaminen ja runojalan polkeminen, mutta se lienee ensi vuoden juttuja...');

end;
end.

