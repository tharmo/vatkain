unit otusmat;

{$mode objfpc}{$H+}



interface


uses
Classes, SysUtils;

type st17=string[17]; st16=string[15];
 type tintlist=class(tlist)  //crude simple list
  //function add(item:pointer):integer;
 end;
var     vnames,anames:array of st17;   maxcor:longword; onjo:tlist;    rownames,varnames:tstringlist;



type totus=class(tobject)
//apurowv,apucolv:array of word; //apuvektoreita, ei haluta allokoida aina uudestaan
   varcount,obscount,rlen:word;
   //maxcor:longword;
vars,vals,vmax,vmaxval:array of word;
var     vst,ast:array of st17;

procedure test;
procedure print(msg:string);
function sumup:word;
function karuselli:totus;
function getpos(r,posi:word;out thevar:word):word;
function addsort(row,avar,aval:word):word;
function addtorow(var varvec,valvec:array of word;avar,aval:word;var entries:word):word;
function multimat(other:totus):totus;
function transponoi:totus;
procedure harvatverbit;
function veivaa:totus;
function skarppaa(i:integer):totus;
constructor readfromfile(mfiln,rfiln,vfiln:string);
function klusteroi:totus;
procedure readnou_adj;
constructor create(mrows,mcols,scar:word);
//constructor createfromcsv(mrows,mcols,scar:word);
end;

var testmat:totus;

implementation
uses math,Bufstream ;
function ifn(test:boolean;v1, v2:word):word; begin if test then result:=v1 else result:=v2;end;

function totus.skarppaa(i:integer):totus;
var ii,j1,j2,k,j1var,j2var,j1val,j2val,kval,kvar,hits,misses:word;sum:longword;
begin
  //print('a*a');
  //for i:=1 to obscount do
  begin
    write(^j^j,i,ast[i],':;');
    for j1:=1 to rlen-1 do  //kaikkien korrelattien
    begin
      j1var:=vars[i*rlen+j1];
      j1val:=vals[i*rlen+j1];
      sum:=0;
      hits:=0;
      for j2:=1 to rlen-1 do
      begin
        j2var:=vars[i*rlen+j2];
        j2val:=vals[i*rlen+j2];
        if j1var=j2var then continue;  //??
        for k:=1 to rlen-1 do   // korrelaatin korrelaatit .. korreloiko se muiden i:n korrelaattien kanssa
        begin
           kvar:=vars[j1var*rlen+k];
           if kvar<>j2var then continue;
           kval:=vals[j1var*rlen+k];
           inc(sum,min(j1val,min(kval,j2val)));
           inc(hits);
           end;

        end;
        //j1 on käyty läpi
        if sum>10*j1val then
        begin
        write(' [',ast[j1var],'.',sum*10 div j1val,'_',hits,'] ');//, j1val,'>',sum,'_',hits,'] ');
        onjo.add(pointer(j1var));
        end;
    end;

  end;
end;
function totus.veivaa:totus;
var veivi,paras,uuson,vr,j,k:word;tpose,vatkuli:totus;uusons:tlist;
begin
 uusons:=tlist.create;
 tpose:=transponoi;
 for veivi:=1 to 41 do
 begin
   //uuson.clear;
   writeln('***********************************************************',veivi,' ',onjo.count);
   //try for j:=0 to onjo.count-1 do write(ast[integer(onjo[j])],'_'); except writeln('!!!',onjo.count,j);end;
   vatkuli:=multimat(tpose);
   vatkuli:=vatkuli.multimat(vatkuli.transponoi);
   vatkuli:=vatkuli.multimat(vatkuli.transponoi);
   vatkuli.sumup;
   //vatkuli.print('vatkuli');
   paras:=vatkuli.sumup;
   write(^j^j,paras,ast[paras],maxcor,'::');
   //for j:=1 to obscount do write(ast[j],vatkuli.vals[j*rlen], ' ');
   vatkuli.skarppaa(paras);
   //if veivi=40 then vatkuli.print('vatkuli6');
   write(^j,ast[paras],' ',maxcor,': ',onjo.count,' ::');
   for j:=1999 to 10 do
   if vatkuli.vars[paras*rlen+j]=0 then break else
   if onjo.indexof(pointer(vatkuli.vars[paras*rlen+j]))<0 then
   begin
     onjo.add(pointer(vatkuli.vars[paras*rlen+j]));
     write(ast[vatkuli.vars[paras*rlen+j]],vatkuli.vals[paras*rlen+j],' ');
   end;
 end;

end;


{procedure turha;
var w:tstringlist;v:tlist;
begin
 w:=tstringlist.create;v:=tlist.create;
 w.add('turha');w.add('eka');w.add('toga');w.add('koli');w.add('neli');w.add('viis');
 v.add(pointer(0));v.add(pointer(5)); v.add(pointer(4));v.add(pointer(3));v.add(pointer(2));v.add(pointer(6));
 sortrow(w,v);

end;}



function totus.klusteroi:totus;


  procedure clustersum(mak:word);   //kertoo klu adv (v) keskenään,, pitäis sort..
var i,j,k,yksmem,yksjii,memmem,vees:word; mava:longword;
var resvw,resvv:array of word;
    begin
     setlength(resvw,rlen*rlen);
     setlength(resvv,rlen*rlen);

     vees:=0;
     //write(^j,ast[mak],'===');
     for i:=1 to rlen-1 do   //käydään kaikki klusteri-emon kimppaajat
     begin
       yksmem:=vars[mak*rlen+i];
       if yksmem=0 then break;
       if vals[mak*rlen+i]*3<vals[mak*rlen+1] then break;
       for j:=1 to rlen-1 do
       begin
         yksjii:=vars[yksmem*rlen+j];//write('?',vst[yksjii]);

         if onjo.indexof(pointer(yksjii))>=0 then continue;
         //if yksjii=yksmem then continue;
         if yksjii=0 then break;
         vees:=addtorow(resvw,resvv,yksjii,100*vals[yksmem*rlen+j] div 100 {vals[yksmem*rlen]},vees);
         //write('?',vees);
            //addtorow(varvec,valvec:array of word;avar,aval:word;var entries:word):word;
          //write('*',yksjii,'_',vals[yksmem*rlen+j],' ');
       end;
     end;
     mava:=0;
     for i:=1 to vees do if mava<resvv[i] then mava:=resvv[i];
     //write(^j^j,'@@@',vst[mak],vees,'/',mava,' ::  ');
     for i:=1 to vees do //if mava<3*resvv[i] then
     begin
       if 2*resvv[i]>mava then begin write(' ,',i,vst[resvw[i]],resvv[i]);
       onjo.add(pointer(resvw[i])) ;end;// else write(' -',resvv[i],' ');
     end;
 end;
var  i,j,i2,j2,k,yksjii,yksval:word;maxr,eka:longint;mul:word;uusonjo:tlist;
begin
  //sumup;
  //print('tokluster:');

   writeln('CLUSTEROI obsB:',obscount,' vars::',varcount,' row:',vst[162],'*',ast[162]);
   //xmat2:=xmat.multimat(tpose);  //sumup merkkaa jo käytetyt, joten tulos aina eri vaikka samoja matriiseja kerrotaan
   uusonjo:=tlist.create;  result:=totus.create(50,obscount,rlen);
  result.ast:=anames;
  result.vst:=anames;
  onjo.clear;
  for i:=1 to 50 do
  begin   //valitaan 100 parasta riviä, poistetaan aina pelistä ed kirros valitut
        //write(^j,'this:'); for j:=1 to 20 do write(vst[clusmat.vars[rlen+j]], ' ');
     maxr:=sumup;  //etsitään alkper matriisista seuraavaksi isoin rivi
     //skarppaa(maxr);
     uusonjo.clear;
     writeln(^j^j,vst[maxr],' ',maxcor,'*****************************************************',vals[i*rlen]);
     for k:=1 to 15 do if (vars[maxr*rlen+k]=0) then break else
      write(vst[vars[maxr*rlen+k]],' ',vals[maxr*rlen+k],'. ');
     writeln(^j);
     eka:=0;
     for j:=1 to 30 do if (vars[maxr*rlen+j]=0) then break else
     begin
       yksjii:=vars[maxr*rlen+j];
       if onjo.indexof(pointer(yksjii))>=0 then continue;
       yksval:=vals[maxr*rlen+j];
       mul:=0;
       //write(^j,'     ', vst[yksjii],vals[maxr*rlen+j],'\',vals[yksjii*rlen],': ');
       for i2:=1 to 30 do if (vars[maxr*rlen+i2]=0) then break else
       if onjo.indexof(pointer(vars[maxr*rlen+i2]))>=0 then continue else
       for j2:=1 to 30 do if (vars[yksjii*rlen+j2]=0) then break else
       if vars[maxr*rlen+i2]=vars[yksjii*rlen+j2] then
       begin
         mul:=mul+min(vals[maxr*rlen+i2],vals[yksjii*rlen+j2]);
        // write(vst[vars[maxr*rlen+i2]],vals[yksjii*rlen+j2],'/',vals[maxr*rlen+i2],' ')
         //write(' ',ast[vars[maxr*rlen+i2]],vals[maxr*rlen+i2]);
       end;
       if (j=1) or (2*mul>eka) then
       begin write(^j,'', mul,'  ',vst[yksjii],'  ',eka);
         uusonjo.add(pointer(yksjii));
         if j=1 then eka:=mul;
       end
       ;//else        write(^j,'---', mul,'  ',vst[yksjii],'  ',yksval)
;
       //onjo.add(pointer(yksjii));
     end;
     onjo.addlist(uusonjo);
     continue;
     {for j:=1 to obscount-1 do
     begin
       for k:=
     end;}
     //if onjo.indexof(pointer(maxr))>=0 then continue;
     //write(^j,^j,'##', ' ',maxr,' ',vst[maxr],' ',vals[maxr*rlen],':::');//,vals[maxr*rlen+1],'  onjo:',onjo.count,'####');
     //for j:=1 to 10 do write(' ',ast[vars[maxr*rlen+j]],vals[maxr*rlen+j]);
     k:=1;
     for j:=1 to rlen-1 do if (vars[maxr*rlen+j]=0) then break else
     begin
      try
     //if vmaxvals[vmax[xmat.vars[maxr*rlen+j]]<>i then continue;
      if onjo.indexof(pointer(vars[maxr*rlen+j]))>=0 then  //write('?',vst[vars[maxr*rlen+j]])
      else
      begin
        //if j<6 then write(' ?',vst[vars[maxr*rlen+j]],vmax[vars[maxr*rlen+j]]);
        try
        //write(' \',ast[vars[maxr*rlen+j]],vals[maxr*rlen+j]);
        if vals[maxr*rlen+j]*2<vals[maxr*rlen+2] then continue;
        result.vars[i*rlen+k]:=vars[maxr*rlen+j];
        result.vals[i*rlen+k]:=vals[maxr*rlen+j];
        //if onjo.indexof(pointer(vars[maxr*rlen+j]))<0 then  //write('-')//--',vst[vars[maxr*rlen+j]]); end
        //onjo.add(pointer(vars[maxr*rlen+j]));
        k:=k+1;
        //if j<>k then writeln('MMMMMMMMMMMMMMMMMMMMMMMM ',j,' ',k);
        {if result.vals[i*rlen+k]*2>result.vals[i*rlen+2] then
        begin
         try
         onjo.add(pointer(vars[maxr*rlen+j]));
         write(' +',ast[vars[maxr*rlen+k]],vals[maxr*rlen+k]);
         except writeln('NOADD');end;
        end else write(' -',ast[vars[maxr*rlen+j]],vals[maxr*rlen+k]);
        }
       except write('??',vars[maxr*rlen+j]);end;
      end;
      except write('!!!!!!!!!!!!',vars[maxr*rlen+j]);end;
     end;
     //writeln('_:_:');
    // clustersum(maxr);
   //write('***',maxr,'.',xmat.vals[maxr*rlen],'***');
   //move(xmat.vars[maxr*rlen],clusmat.vars[i*rlen],39);//rlen*2);
   //move(xmat.vals[maxr*rlen],clusmat.vals[i*rlen],39);//rlen*2);
   //write(^j,i,' :: ');//,xmat.vals[maxr*rlen],'/', clusmat.vals[i*rlen],' ');
   //for j:=0 to 10 do write('\',xmat.vals[maxr*rlen+j],'/', clusmat.vals[i*rlen+j],' ');

   //writeln(^j^j,'mplied',xmat.vst[i]);
 end;
  {for i:=1 to obscount do
  begin
    for j:=1 to rlen-1 do
    begin
      sum:=0;
      for k:=1 to rlen-1 do
      begin
         if result.vars[vars[i*rlen+j]*rlen+k]=vars[j*rlen]
      end;
    end;
  end;}
 //clusmat.print('50 klueria');
end;

function totus.karuselli:totus;
var i,j,v,c,k:integer;tpose,amat,apose,xmat,xmat2:totus; maxlist:tlist;maxr,myclus:word;clusmat:totus;
begin
  sumup;
        //tä vielä verb kertaa adv
 //turha;exit;
 //sumup;
 for i:=1999 to obscount do //tpose.obscount do
 begin
   try
   write(^j^j,vst[i]:12,vals[i*rlen]:4,':');except write('!!',i*rlen);end;
   for j:=1 to rlen-1 do //tpose.obscount do
   if  vals[i*rlen+j]>2*(vals[i*rlen+rlen-1]+1)  then write(' ',vals[i*rlen+j]:4) else  write(' -',vals[i*rlen+j]:3);
   write(^j,ast[vars[i*rlen+1]]:16,':');
   for j:=1 to rlen-1 do //tpose.obscount do
   //if  vals[i*rlen+j]>(vals[i*rlen])  then
   write(' ',vals[i*rlen+j]*100 div (vals[i*rlen+j-1]+1):4) // else  write(' -',vals[i*rlen+j]);
 end;
 //print('alku');exit;
 //print('ORIGINAL');
 maxcor:=256;
 apose:=transponoi;   // ad * v
 //atmat:=totus.readfromfile('va.isot','verb.n','adv.n');
 //apose.print('A*V'); exit;
 writeln(^j^j,'tposed',apose.vst[1]);
 //xmat:=multimat(apose);  // *v*v
 xmat:=apose.multimat(self);  // *v*v
 writeln(^j^j,'Mplied',xmat.vst[1]);
 xmat.veivaa;
 exit;
 xmat.sumup;
 //xmat2:=xmat.multimat(xmat.transponoi);  // *v*v
 //writeln(^j^j,'Mplied',xmat2.vst[1]);
 //xmat2.sumup;
 //writeln(^j^j,'summed',xmat.vst[1]);
 //xmat2.print('adVERBIT*adVERBIT');
 //apose:=xmat.transponoi;
 //xmat:=xmat.multimat(apose);  // *v*v
 //xmat.print('VERBIT*VERBIT');
 clusmat:=xmat.klusteroi;
 onjo.clear;
 clusmat.sumup;
 //clusmat.print('clustered verbs'); exit;
 //onjo.clear;
 tpose:=clusmat.transponoi;  //kullekin verbille klusteri johon kuuluu
 tpose.sumup;
 //tpose.print('xxxxxxxxxxxxxxxxxxxx');
 //tpose.print('mikälie');

 //xit;
 xmat2:=clusmat.multimat(apose);
 tpose:=xmat2.transponoi;
 //xmat2:=tpose.multimat(clusmat);     //apose: adv: v,,v,..
 for i:=991 to varcount do //tpose.obscount do
  begin
     write(^j,vals[i*rlen],'/',vals[i*rlen+1],'/',vals[i*rlen+rlen]);
    for j:=2 to rlen-1 do if tpose.vals[i*rlen+j]*1.2>tpose.vals[i*rlen+1] then write(' _',i) else  tpose.vars[i*rlen+j]:=0;
   //myclus:=tpose.vars[i*rlen+1];
   //if myclus>0 then write(^J,tpose.vst[I],myclus,
   //'>',CLUSMAT.ast[clusmat.vars[myclus*rlen+1]],'_',CLUSMAT.ast[clusmat.vars[myclus*rlen+2]],' ');
  end;
 apose:=tpose.transponoi;//.print('klusterit VerVeille');
 for i:=1 to 50 do //tpose.obscount do
  begin
    write(^j,^j,'V: ');
    for j:=2 to rlen-1 do if apose.vars[i*rlen+j]=0 then break
    else  write(vst[apose.vars[i*rlen+j]],' ');
    write(^j,'A:');
    for j:=2 to rlen-1 do if clusmat.vars[i*rlen+j]=0 then break
    else  write(ast[clusmat.vars[i*rlen+j]],' ');
   //myclus:=tpose.vars[i*rlen+1];
   //if myclus>0 then write(^J,tpose.vst[I],myclus,
   //'>',CLUSMAT.ast[clusmat.vars[myclus*rlen+1]],'_',CLUSMAT.ast[clusmat.vars[myclus*rlen+2]],' ');
  end;

 exit;
 //writeln(^j,i,' isot poimittu ************************',length(clusmat.vars),'*');
 //
 //tpose.print;  klusterit riveinä verbit muuttujina
 //apose.print('verbien klusterit (rivinimet sotukussa');  //apose.print('');

// xmat2:=clusmat.multimat(self);  //xmat uusiokäytössä: apose: adv riveinä, verbit muuuttujina clusmat=clus*v xmat:rivit adp
  xmat2:=apose.multimat(tpose);  //>clus*v xmat:rivit adp
  xmat2.sumup;

  //for i:=1 to 40 do for j:=0 to 2 do if j=0 then writeln(^j,i) else write(' ',vst[clusmat.vars[i*rlen+j]]);
  for i:=1 to obscount do
   for j:=2 to rlen do //if j<3 then write(vst[clusmat.vars[xmat2.vars[i*rlen+j]*rlen+1]],' ');
    if  xmat2.vals[i*rlen+1]>1.5*xmat2.vals[i*rlen+j] then
     xmat2.vars[i*rlen+j]:=0;
  {for i:=1 to varcount do
  //if  xmat2.vals[i*rlen+1]>2*xmat2.vals[i*rlen+2] then
  write(^j,ast[i],xmat2.vars[i*rlen+1],':',xmat2.vars[i*rlen+2],'*',
  vst[clusmat.vars[xmat2.vars[i*rlen+1]*rlen+1 ]],
  xmat2.vals[i*rlen+1],
  ' ',vst[clusmat.vars[xmat2.vars[i*rlen+2]*rlen+1 ]],
  xmat2.vals[i*rlen+2]);
  xmat2.print('Adverbien yhteydet verbiklustereihin. 2 per rivi?');
  }
  xmat:=xmat2.transponoi;

  xmat.sumup;
  //xmat.print('Adverbien yhteydet verbiklustereihin. 2 per rivi?');
 // xmat:=xmat.multimat(xmat2);
  //xmat.print('zzzzzzzzzzzzzzzzzz');
  //xmat2.transponoi.print('aaposessa');
  xmat.harvatverbit;        exit;

 //xmat2.print('adverbit clustereissa');
 //tpose:=xmat.transponoi;
 //tpose.print;
 //halutaan jokaiselle sanalle korrelaatio kuhunkin klusteriin
 //tpose:=clusmat.transponoi;
 { TODO karuselli} //ToDo
 //pose.print;
  {xmat2.sumup;
  xmat:=xmat2.multimat(apose);
  xmat.sumup;
  xmat2:=xmat.multimat(self);}
  for i:=1 to 100 do
  begin
     write(^j^j,'        ');
     for j:=1 to 20 do if  xmat.vars[i*rlen+j]=0 then break else //if xmat.vals[i*rlen+j]*2>xmat.vals[i*rlen+2] then
     write(' ',ast[xmat.vars[i*rlen+j]]);
     write(^j,'         ');
     for j:=1 to clusmat.rlen do
       if clusmat.vars[i*rlen+j]=0 then break else if clusMAT.vals[i*rlen+j]*3>clusmat.vals[i*rlen+2] then write(' _',vst[clusmat.vars[i*rlen+j]]);
  end;
 exit;
 //maxlist:=tlist.create;
 writeln(^j,i,' listaa ekat ************************',onjo.count);
 for i:=1 to 100 do //write(' ',clusmat.vars[J*rlen+1]);
  begin
    write(^j,i,': ');
    for j:=1 to obscount do try
    begin
      if tpose.vars[j*rlen+1]=I then write(tpose.vst[j],' ');
      if tpose.vars[j*rlen+2]=I then write('-',tpose.vst[j],' ');

     end;
     except writeln(i,'****',j,'*',length(clusmat.vars));end;
  end;
 //xmat2.print;
  //xmat.sumup;
  //xmat.print;
  writeln(^j,'_____________________',onjo.count,':::');
end;

function totus.multimat(other:totus):totus;
var
    //i,j,ik,jm:word;
 v1var,a1n,a1var,a1val,v2n,v3,v3n,v2val,v2var,a2,a2var,a2val,i,sikoja:word;
 //ivar,jvar,ikvar,jmvar:word;
 sum:longword;
 tutki,atot:longword;
 vv:array of word; //oisko parempi allokoida pysyvästi ja aina täyttääå nollilla?
begin
 tutki:=rlen-1;
 // if obscount<>omat.varcount then
 //rite(^j,'this:'); for v2n:=1 to 20 do write(vst[vars[rlen+v2n]], ' ');

  setlength(vv,600);//obscount+1);
  result:=totus.create(obscount,other.varcount,rlen);
  result.vst:=vst;
  result.ast:=other.ast;
  //writeln(^j,'KERRO;:',obscount,';',varcount,' * ',other.obscount,'*',other.varcount,' to: ',result.obscount,'*',result.varcount,' len:',length(result.vals),' len:',length(result.vars));
  // writeln(^j,'KERRO',^j,'obsA:',obscount,' vars::',varcount,' row:',rlen);
  // writeln('obsB:',other.obscount,' vars::',other.varcount,' row:',rlen);
  // writeln('RES:',result.obscount,' vars::',result.varcount,' row:',rlen);
 for v1var:=1 to obscount do  //yksi verbi (havainto) kerrallaan
  begin
    try
    //write('.,',ast[v1var*rlen+1]);
    for i:=0 to length(vv)-1 do vv[i]:=0;
    for a1n:=1 to tutki do  //tutkittavan verbin ADVERBIT ..
    begin
      a1var:=vars[v1var*rlen+a1n];
      //write('\',ast[v1var]);
      a1val:=vals[v1var*rlen+a1n];
      if a1var=0 then break; //verbin harvamatriisin rivi päättyy
      for v2n:=1 to tutki do           //kullekin adverbille kaikki verbit
      begin  //adverbin verbit
        try
        v2var:=other.vars[a1var*other.rlen+v2n]; //verbi joka korreloin saman adv. kanssa kuin
        if onjo.indexof(pointer(v2var))>=0 then continue;
        except writeln(^j,'NONOother:',a1var,' ',a1var*other.rlen,' ',v2n,' #',v2n,'=',v2var,'##',length(other.vars));end;
        try
        if v2var=0 then break;
        //if v2var=v1var then continue;
        v2val:=other.vals[a1var*other.rlen+v2n];
        for a2:=1 to tutki do //tutkittavan kanssa korreloivat muut ADVERBIT ..
         begin
           a2var:=vars[v1var*rlen+a2]; //alkuperäinen rivi uudestaan
          if a2var=0 then break;
          if a2var<>a1var then continue;
          if onjo.indexof(pointer(a1var))>=0 then continue;
          vv[v2var]:=vv[v2var]+min(a1val,vals[v1var*rlen+a2]);  // pistetään sarkakkeille v1-riville arvoja kolmansille muuttujille
          //vv[v2var]:=vv[v2var]+10;  // pistetään sarkakkeille v1-riville arvoja kolmansille muuttujille
          //if vv[v2var]>10000 then write('|',vv[v2var],vst[v1var],vst[v2var]);
           break;end;
        except writeln(^j,'NONOmult:',v1var,' ',length(vv),' ',vst[v1var],' ',a1var,ast[a1var],' #',v2n,'=',v2var,'##',length(vv));end;
          try
        if v2val=0 then break;
        //vv[v2var]:=vv[v2var]+min(a1val,v2val);  // pistetään sarkakkeille v1-riville arvoja kolmansille muuttujille
        //if vv[v2var]>0 then if v1var=531 then
        //write('  ',v2val,'>',vv[v2var]);
        except writeln(^j,'multiNONO:',v2var,'!',length(vv),vv[v2var],' ',vst[v1var],' ',a1var,ast[a1var],' #',v2n,'=',v2val,'/',a1val,'##',length(vv)); end;
       end;  //yhden sanan kaikki kaverit käyty läpi

    end;
    except writeln(^j,'NONOV2:',v1var,' ',length(vv),' ',vst[v1var],' ',a1var,ast[a1var],' #',v2n,'=',v2var,'##',length(vv));end;
    //result.vals[v1var*rlen]:=vv[v2n] div 1;
    try
    for v2n:=1 to other.varcount do //ekan matriisin rivit
     //if onjo.indexof(pointer(v2n))<0 then (rivi*rivi)
      result.addsort(v1var,v2n,vv[v2n] div 20);// div maxcor); //ne tokan matriisin sarakkeet jotka muistuttivat ekan riviä
    except writeln(^j,'NONOsrt',v1var,'/',v2n,'/',vv[v2n] div maxcor,'///:',v1var,' ',vv[v2n],' ',maxcor);end;
    //write(' ;',result.vals[v1var*rlen]);
  end;
end;

function totus.addsort(row,avar,aval:word):word;
  //function onksiso(pp,pv:PWORD;vari,vali:integer;veclen,wnum:word):word;//;       bsiz,bpair: array of word);
var i,j,k,x:integer;pvars,pvals:pword;
begin
  try
  pvals:=@vals[row*rlen];
  pvars:=@vars[row*rlen];
  result:=0;
  for j:=1 to rlen-1 do
  begin
    x:=word((pvals+j)^);
    if x<aval then  //requiring 2-byte numbers
    begin     //vals: 2 4 8, aval=3 -> j=2
      move((pvars+(j))^,(pvars+((j+1)))^,(rlen-j)*2-2);
      move((pvals+(j))^,(pvals+((j+1)))^,(rlen-j)*2-2);
      vars[row*rlen+j]:=avar;
      vals[row*rlen+j]:=aval;
      result:=j;
      //if j<2 then write('  >',aval,'@',j);
      //for k:=1 to 10 do write(' ',vars[row*rlen+k],'=',vals[row*rlen+k]);
      exit;
    end;
  end;
  except write(^j^j,'FAILSORT(','/',avar,'=',aval,')-topos:',row,':',j,'  ');end;
end;

function totus.addtorow(vAR varvec,valvec:array of word;avar,aval:word;var entries:word):word;
// yhden klusterin yhden arvon lisäysm
var i,j,k,x:integer;pvars,pvals:pword;olijo:boolean;
begin
  try
  result:=0;
  for j:=1 to entries do
  begin
    if varvec[j]=avar then  //requiring 2-byte numbers
    begin     //vals: 2 4 8, aval=3 -> j=2
      valvec[j]:=valvec[j]+aval;
      result:=entries;
      //write('(',valvec[j]')');
      //for k:=1 to 10 do write(' ',vars[row*rlen+k],'=',vals[row*rlen+k]);
      // write('!',vst[avar],1*aval);
      exit;
    end;
  end;
  result:=entries+1;
  varvec[result]:=avar;
  valvec[result]:=aval;
  except write(^j^j,'FAIrow(','/',avar,'=',aval,')-topos:',entries,':',j,'  ');end;
end;


function totus.getpos(r,posi:word;out thevar:word):word;
begin
 try
  thevar:=vars[r*rlen+posi];
  result:=vals[r*rlen+posi];
  except writeln('***',r, 'posi:',posi, ' @',thevar,' ');raise;end;
end;

function karsituplat(var var1,val1,var2,val2:array of word;len:word):word;
var uniks,i,j:word;isuni:boolean;
begin
  uniks:=1;
  for i:=1 to len do ///vittu, voiks tää olla näin työlästä. Vitun vittu ja pari perkelettä päälle
  begin
    isuni:=true;
    for j:=1 to uniks-1 do  //oliko jo lisätty
    begin
       //tehdään uus josta dupl poistettu/ynnätty
      if var1[i]=var2[j] then
      begin
         isuni:=false;
         val2[j]:=val2[j]+val1[i];
         break;
      end;
    end;
    if isuni then begin var2[uniks]:=var1[i];val2[uniks]:=val1[i];inc(uniks);end;
  end;
  result:=uniks;
end;

function totus.transponoi:totus;
var totvars,totvals:array of word;
   r,v,a,hits,uniks,dvals,i,j,sum:word;

begin
  result:=totus.create(varcount,obscount,rlen);
  result.vst:=ast;
  result.ast:=vst;
  result.varcount:=obscount;
  result.obscount:=varcount;

  //result.vars:=;
  //setlength(totvars,obscount*varcount);  //pitää tehdä tlläiset hillittömyydet
  setlength(totvals,(obscount+1)*(varcount+1));
  writeln(^j,'TRANSPONOI',^j,'obsA:',obscount,' vars::',varcount,' row:',rlen,vst[1],ast[1]);
  writeln('obsB:',result.obscount,' vars::',result.varcount,' row:',rlen,result.vst[1],'*',result.ast[1]);
  //writeln(vst[500],result.ast[500]);
  for i:=1 to obscount do
  begin
    hits:=0;
    //fillchar(varvec[0],length(varvec)*1,0);
    //write(' ',i);
    for r:=1 to rlen-1 do
    begin
      // write (i,'.',r,' ');
      //write('.');
      if vars[i*rlen+r]=0 then break;
      v:=vars[i*rlen+r];
      totvals[v*obscount+i]:=vals[i*rlen+r];
      //write(^j,v,':',i,' ',sum,':: ');      for j:=1 to sum do write(' +',totvars[v*varcount+j]);    write('   <', i,':' ,v,' ');
      //addtomat(vars[i*rlen+r],i,vals[i*rlen+r]);
    end;
  end;
  //write('kierrä ');
  //writeln(^j,'TRANSPOSE---------------',varcount,'->');
  for i:=1 to varcount do
  begin
   try
      //write(^j,'X:',i,' #',totvals[i*varcount],':');
      //FOR j:=1 TO RLEN do if totvars[i*varcount+j]=0 then continue
      //else write(totvars[i*varcount+j],'=',totvaLs[i*varcount+j],' ');
      for j:=1 to obscount do
      begin //write(j,' ');
         if totvals[i*obscount+j]=0 then continue else
            result.addsort(i,j,totvals[i*obscount+j]);
      end;
   except write('FAIL:',i,' ',i*obscount+j,' ',i*rlen+1,'!!',' ',totvals[i*rlen+1],'%%%%%'); end;
   //write(^j,i,':');
   result.vaLs[i*RLEN]:=totvals[i*obscount];
      //write(^j,vst[i],' (',result.vaLs[i*RLEN],') ');
   end;
  exit;
  for i:=1 to varcount-1 do
  begin
   write(^j,ast[i],result.vaLs[i*RLEN],': ');
   FOR j:=1 TO 10 do write(vst[result.vars[i*RLEN+j]],'=',result.vaLs[i*RLEN+j],' ');
   end;
end;

procedure totus.print(msg:string);
var i,j,npos:word;sum,tot:longword; muniso:word;
begin
  writeln(^j^j^j,'PRINTMAT ',msg,':rows:',obscount,' cols:',varcount,',max=',maxcor,vst[1]);
   // for j:=0 to onjo.count-1 do write(vst[integer(onjo[j])],'_');

   for i:=1 to  obscount do
   begin
      muniso:=vals[i*rlen+2];
      //if munisoin<500 then continue;
     try
      tot:=0;
     for j:=1 to rlen-1 do if (vars[i*rlen+j]=0) or (vals[i*rlen+j]=0) then break else tot:=tot+vals[i*rlen+j];
     //s[i*rlen+j];
     //if tot>7000 then continue;
     //if onjo.indexof(pointer(vars[i*rlen]))>=0 then continue;
     write(^j^j,i,':::',vst[i],tot,' (', vals[i*rlen],'): ');
     for j:=1 to rlen-1 do if vars[i*rlen+j]=0 then break
     else //if vals[i*rlen+j]=0 then break else //*2<muniso then break else
     begin
        try
        if onjo.indexof(pointer(vars[i*rlen+j]))>=0 then write(' ---- ') else
        write(' .',ast[vars[i*rlen+j]],'=',vals[i*rlen+j],' ');
        except writeln('eieieFailprint',i,' ',i*rlen+j,' ',length(vars),' ',length(vals),' V:',vars[i*rlen+j],'=',vals[i*rlen+j]);raise;end;
        //onjo.add(pointer(integer(vars[i*rlen+j])));
     end;
   except writeln('Failprint',i,' ',i*rlen+j,' ',length(vars));end;
  end;
end;

function totus.sumup:word;
var i,j,maxrow:word; sum:longword;X:INTEGER;
begin
   try
   WRITELN('SUMUP:',onjo.count);
   //for X:=0 to onjo.count-1 do BEGIN try write('__',vst[integer(onjo[X])]);except write('_!!!',integer(onjo[X]),I)end;END;
   except writeLN(^J,'NONONONONONONno  onjo',ONJO.COUNT,'???');end;
   result:=0;
   maxcor:=0;maxrow:=0;
   //write(' SUM');
   //fillchar(vmax[0],varcount*2,0);
   for i:=1 to obscount do vals[i*rlen]:=0;
   for i:=1 to obscount do
   begin
     try
     sum:=0;
     if onjo.indexof(pointer(i))<0 then
     for j:=2 to rlen-1 do  if vars[i*rlen+j]=0 then continue else if onjo.indexof(pointer(vars[i*rlen+j]))<0 then
     begin
      //if  vars[i*rlen+j]<>i then if vmaxval[vars[i*rlen+j]]<vals[i*rlen+j] then begin vmax[vars[i*rlen+j]]:=i;vmaxval[vars[i*rlen+j]]:=vals[i*rlen+j];end;
      //if j>9 then break;
      //if vals[i*rlen+j]<2*(vals[i*rlen+rlen-1]+1) then vals[i*rlen+j]:=0;
      // if  vals[i*rlen+j]<50 then  vals[i*rlen+j]:=0;
      sum:=sum+vals[i*rlen+j];// else sum:=sum+(vals[i*rlen+j]);// div 2);
     end;
     try
     sum:=sum div 5;
     vals[i*rlen]:=sum;//*1024 div maxcor;
     //write(' ',sum);
     except  writeln('FAILsum ',maxcor,'/',sum,'/',ast[vars[i*rlen+j]]); end;
     if maxcor<sum then
     begin
       maxcor:=sum;
       maxrow:=i;
       //write('+',ast[maxrow],maxcor);
     end;
     except  writeln('FAILfindMAX ',maxcor,'/',sum,'/',ast[vars[i*rlen+j]]); end;
   end;
   try
   //write(^j,'(:',maxrow,vst[maxrow],')');//,vst[maxrow],'#',maxcor,':');   for j:=1 to 3 do write(' ',ast[vars[maxrow*rlen+j]],vals[maxrow*rlen+j]);
   //for j:=1 to 8 do write(' ',vals[maxrow*rlen+j]);
   result:=maxrow;
   //onjo.add(pointer(maxrow));
   //write(^j,'MAX: ');//,maxrow,vst[maxrow],'#',maxcor);
   //for j:=1 to 10 do write(' ',vst[vars[maxrow*rlen+j]]);//,vals[maxrow*rlen+j]);

   // for j:=1 to 6 do if   onjo.indexof(pointer(vars[maxrow*rlen+j]))<0 then onjo.add(pointer(vars[maxrow*rlen+j]));
  except  write('FAILMAX ',maxcor); end;
end;

procedure totus.test;
var i,j,a,v,vnpos:word;xmat,xmat2,xmat3:totus;
begin
  karuselli;
  writeln('test scarce matrix ',obscount ,'*',varcount, ' ',rlen);
 //print;exit;
end;

constructor totus.create(mrows,mcols,scar:word);
begin
    obscount:=mrows;
    varcount:=mcols;
    rlen:=scar;  //length of scarse mat row .. hom many biggest saved
    setlength(vmax,max(varcount+1,obscount+1));
    setlength(vmaxval,max(varcount+1,obscount+1));
    setlength(vars,(obscount+1)*(rlen+1));//alkaa nollasta mutta nollat käyttämättä tai erikoistarkoituksiin
    setlength(vals,(obscount+1)*(rlen+1));//alkaa nollasta mutta nollat käyttämättä tai erikoistarkoituksiin
    vst:=vnames;
    ast:=anames;
end;

function onksiso(pp,pv:PWORD;vari,vali:integer;veclen,wnum:word):word;//;       bsiz,bpair: array of word);
var i,j,k:integer;//pp,vp:pointer;
begin
  try
  result:=999;
  // if vali>0 then
  //write('=',vali);
  //if wnum=vari then exit;
  for j:=1 to veclen do
    if (pv+j)^<vali then  //requiring 2-byte numbers
    begin
      move((pp+(j))^,(pp+((j+1)))^,(veclen-j)*2);
      move((pv+(j))^,(pv+((j+1)))^,(veclen-j)*2);
      word((pp+j)^):=vari;
      word((pv+j)^):=vali;
      //if vali>10 then for k:=0 to 3 do write('/',word((pp+k)^),'.',word((pv+k)^));
      result:=j;
      exit;
    end;
  except write(^j^j,'FAIL(','/',vari,'=',vali,')-topos:',j,'  ');end;
end;
type tww=packed record v:longword;w:st16;end;
type  tws=array of tww;

  function sortrow(vars:tstringlist;vals:tlist):word;
    //function onksiso(pp,pv:PWORD;vari,vali:integer;veclen,wnum:word):word;//;       bsiz,bpair: array of word);
  var i,j,k,x,sofar:integer;pvars,pvals:pword;w:st17;      ww:pointer;
     CNT:WORD;va:array of word; wa:array of st16;
  begin
    try
    if vals.count<2 then exit;
    CNT:=vals.count-1;
    setlength(va,vals.count+2);
    setlength(wa,vars.count+2);
    sofaR:=1;
    except writeln(cnt,'nonnogogo',vals.count);end;
    try
    for j:=0 to cnt do
    begin
       x:=integer(vals[j]);
       //write(^j,j,'zzz:',vars[j],' ',x,'/',va[j],'::');
       //continue;
      if x>0 then
      //for k:=sofar downto 1 do
      for k:=1 to sofar do
      begin
        try
        if x>va[k]  then //result[k].v then  //requiring 2-byte numbers
        begin     //vals: 2 4 8, aval=3 -> j=2
          //ww:=@result[k].v;
        // write('_');
         move(va[k],va[k+1],(sofar-k)*2);
         move(wa[k],wa[k+1],(sofar-k)*16);
         wa[k]:=copy(vars[j],1,15);
         va[k]:=x;
          //result[k].v:=x;
          //result[k].w:=copy(vars[j],1,15);
          sofar:=sofar+1;
          //write('  ',result[k].w,'=',result[k].v,'.',k);        //if j<2 then write('  >',aval,'@',j);
          break;
        end;
        except write(' failmove topos:',k,' /sf:',sofar,' /tot:',cnt,' j:',j);      end;  //if j<2 then write('  >',aval,'@',j);
      end;
    end;
    //writeln(^j^j':::::::::::::');
    for k:=1 to sofar do write(' ', wa[k]);//,va[k]);  //result[k].w,'=',result[k].v);
    except writeln(' failsort topos:',k,' /sf:',sofar,' /tot:',cnt,' j:',j);      end;  //if j<2 then write('  >',aval,'@',j);
  end;

procedure totus.harvatverbit;
var f:text;palat:tstringlist;pala,prev:string;vavars:array of longword;vavals:array of longint;c,h,a,hits:word;maksvar,maksval,mv2:integer;harva:boolean;
   yle,asum:integer;
   chits:array [0..100] of tstringlist;
   acounts,chitvals:tlist;
   vcnt:longword;oli:word;
   ex,ob:longint;hit:boolean;
begin
 acounts:=tlist.create;
 chitvals:=tlist.create;
   palat:=tstringlist.create;
   palat.loadfromfile('adv.n');
   for a:=0 to palat.count-1 do acounts.add(pointer(1+strtointdef(copy(palat[a],pos(' ',palat[a])+1),0) div 1000));
   for a:=0 to palat.count-1 do write('_',integer(acounts[a]));
   for a:=0 to 100 do chits[a]:=tstringlist.create;
   for a:=0 to 100 do chitvals.add(pointer(tlist.create));
   assign(f,'koko.vasums');
   write('koko.vasums');
   palat.clear;
   //writeln(varnames.commatext);
 reset(f);
 varnames.sort;
 varnames.sorted:=true;
 setlength(vavars,varcount);
 setlength(vavals,varcount);
 palat:=tstringlist.create;
 palat.delimiter:=' ';
 palat.strictdelimiter:=true;
 assign(f,'koko.vasums');
 reset(f);
 writeln(^j'Lue iso',varcount,' v',obscount);
 hits:=0;
 //writeln(rownames.text);
 while not eof(f) do  //  ymmätään jokaiselle sanalle (adv) kaikki yhteisesiintymät (verbien) anssa
 begin
    try
   readln(f,pala);
   palat.delimitedtext:=pala;
   if  prev<>palat[0] then
   begin //uusi verbi , rekisteröi edellinen
     try
     //write(^j,prev, vcnt,' ');
     //if prev='tutkia' then writeln(^j,'TUTKIA :',vcnt,'/',hits);

     //write(^j^j,prev,hits,'/',vcnt);
     maksval:=0;mv2:=0;
     if hits>10 then if vcnt>0 then
     for c:=1 to obscount do  //klusterit
     begin
       asum:=0;
       //if prev='tutkia' then writeln(^j^j^j,c,' [',ast[vars[c*rlen+1]],'.',ast[vars[c*rlen+2]]);
       for h:=1 to hits do  //kaikki luetun nykyverbin (ei ehkä matriisissa) yleiset adverbit
       begin  //ylverb       yladv                          isoklus
         ex:=(vcnt*integer(acounts[vavars[h]])*vals[c*rlen+1]) div 10000;
         if ex=0 then continue;
         //write('  ',ex,'::');
         try
         for a:=1 to rlen do  //kaikki klusterin adverbit
         begin
           if (a>=rlen) or (vars[c*rlen+a]=0) then
           //verbin adv ei esiintynyt klusterissa, pikkasen miinusta (paljon jos yleinen verbi tai tärkeä adv klussa)
           begin
             //try asum:=(asum-(ex)) div hits;
             break;
             //except write('%%%',asum,'%',vals[c*rlen+a]*vcnt,'*');end;
           end else
           if vars[c*rlen+a]=vavars[h] then // sekä klusterin ett' verbin listoilla
           begin
             //begin inc(asum,vavals[h]*vals[c*rlen+a] div ((vcnt *integer(acounts[vars[c*rlen+a]])+10));
               try  //yleverb       //isomaklust    //yleav
               ob:=round(100*sqrt(vavals[h]*vals[c*rlen+a] div ex));//sopii klusteriin siinä määärin kuin klusteriadverbi yliesiintyy
               //ob:=max(vals[c*rlen+a]-ex,0);//sopii klusteriin siinä määärin kuin klusteriadverbi yliesiintyy
                //ob:=vavals[h]
                //ei murehdita yliedustuksissta, niitä tulee myös kilpaileviin klustereihin
                //if vals[c*rlen+a]>500 then              write(' +',vals[c*rlen+a],'.',ex);
                //if ex-vavals[h]>150 then writeln(^j,'   +++',ast[vavars[h]],vavals[h]);
                hit:=true;//
               //inc(asum,10*vavals[h]*vals[c*rlen+a] div ((vcnt+1 )));//*integer(acounts[vars[c*rlen+a]])+10));
               //havaittu - odotettu
               //ex(1000*vavals[h] div vcnt-vals[c*rlen+a] div vcnt-vals[c*rlen+a],' ');
               inc(asum,ob);// div ((vcnt+1 )));//*integer(acounts[vars[c*rlen+a]])+10));
               //if prev='tutkia' then write('(',vavals[h],'/',ex,ast[vavars[h]],ob,' >',asum,') ');
               break;
               except write('{!}');end;
             end;
           end;
         //miinuksia joka klusterin sanasta joka ei esiintynyt .. ent
         except write('{!!!!',vals[c*rlen+a]);end;
       end;
       try
       //
       if asum>maksval then begin
         //write(' ',ast[vars[c*rlen+1]],'/',asum,';  '  );
         {for a:=1 to rlen-1 do  //kaikki klusterin adverbit
           if vars[c*rlen+a]=0 then break  //klusteri käyty loppuun
            else for h:=1 to hits do  //kaikki luetun nykyverbin (ei ehkä matriisissa) yleiset adverbit
             if vars[c*rlen+a]=vavars[h] then write(ast[vars[c*rlen+a]],vals[c*rlen+a],'*',vavals[h],
              '/',vals[c*rlen],'*',integer(acounts[vars[c*rlen+a]]),'=',1000*vavals[h]*vals[c*rlen+a] div (vals[c*rlen]*integer(acounts[vars[c*rlen+a]])+10),' ');
          }
         maksval:=asum;maksvar:=c;end
       else

       if asum>mv2 then mv2:=asum; //toisaiseksi paras klusteri
       except write('!!!',c,'!',h,'!',asum,'/max;',maksvar,'.',maksval);end;
     end;
      //write('_',maksval);
      //if maksval>5*mv2 then
      if maksval>0 then
      begin
      if maksval>100 then
       write(^j,prev,':  ',maksval,' ->',ast[vars[maksvar*rlen+1]],' '
         ,ast[vars[maksvar*rlen+2]],' ',ast[vars[maksvar*rlen+3]],' ',ast[vars[maksvar*rlen+4]]
         ,tstringlist(chits[maksvar]).count,' ',vcnt);//,ast[vars[maksvar*rlen+2]],' ',ast[vars[maksvar*rlen+3]],maksval,'.',mv2
      tstringlist(chits[maksvar]).add(prev);
      tlist(chitvals[maksvar]).add(pointer(maksval));
     // write(^j,maksvar,prev,'->',ast[vars[maksvar*rlen+1]],tstringlist(chits[maksvar]).count,' ');//,ast[vars[maksvar*rlen+2]],' ',ast[vars[maksvar*rlen+3]],maksval,'.',mv2
      // ,'vc:',vcnt,' sum:',asum,'  clu:',vals[maksvar*rlen]);
      end;
        // if prev='tutkia' then readln;

      try
      for h:=1 to varcount-1 do begin vavars[h]:=0;vavals[h]:=0;end;
      except write(^j,'/fail///');end;
      try
             //writeln('**',prev,maksval,'**');

      prev:=palat[0];
      except write('/failend',hits,pala,'/');end;
     except write('/failnew',vcnt,':(',pala,')/');end;
     hits:=0;
     vcnt:=0;
   end;
   // sama verbi toistuu eri adverbien kanssa
   yle:=varnames.indexof(palat[1]);
    except writeln('****!///!********',hits,' ');end;
    try
   if yle>0 then  //yleinen adverbi
   begin
      vcnt:=vcnt+(strtointdef(palat[2],0));
      inc(hits);
      vavars[hits]:=yle;
      vavals[hits]:=strtointdef(palat[2],0);
    //  write('*',hits);
    //harva:=
    end;
    //write('[',pala,'/',yle,']');

   except writeln('****!!********',yle,' ',pala);end;
 end;
 for a:=0 to 60 do
 begin writeLN(^j^j);
   try
   //for c:=0 to tstringlist(chits[a]).count-1 do write(tstringlist(chits[a])[c],integer(tlist(chitvals[a])[c]),' ');
   sortrow(tstringlist(chits[a]),tlist(chitvals[a]));
   write(^J'::: ');
   for c:=1 to rlen-1 do if vars[a*rlen+c]=0 then break else write(ast[vars[a*rlen+c]],' ');//vals[a*rlen+c],' ');
  except write('? ');end;
 end;
end;


procedure totus.readnou_adj;

var nounlist, adjlist:tstringlist;f:text;
    var v,a: string;
        StartPos, LineLen,sp: longword;buf:array[0..4096] of byte;
        bvirta:TReadBufStream;
        fvirta:TfileStream;
        freq:longword;
        ch:byte;
        i,recpos,FI:longword;
        scAR:ARRAY of word; //need to index the words .. 2^16 suffices
     type generic TArray <TIndex, TValue> = class
     A:STRING;END;
    type trec3=record a,b:packed array[1..16] of char;f:longword;end;
    var  recs:array of trec3;
    function isoSET(eka,vika:longword;vali,veclen:word):word;//;       cant i make a general version of this..
      var i,j,k:integer;//veclen:word;//pp,vp:pointer;
      begin
        try
        result:=999;
        for i:=eka to vika do
          for j:=1 to veclen do
          if buf[i]<vali then  //requiring 2-byte numbers
       {   begin
            move((pp+(j))^,(pp+((j+1)))^,(veclen-j)*2);
            move((pv+(j))^,(pv+((j+1)))^,(veclen-j)*2);
            word((pp+j)^):=vari;
            word((pv+j)^):=vali;
            //if vali>10 then for k:=0 to 3 do write('/',word((pp+k)^),'.',word((pv+k)^));
            result:=j;
            exit;
          end;
          }
        except write(^j^j,'FAIL(','/',vali,'=',vali,')-topos:',j,'  ');end;
      end;
  var doneall,donebuf:boolean;
    //in1,in2,in3:boolean;
     s1,s2,s3:word;
     sts:array[0..3] of longword;//array[0..15] of char;
     fld:byte;
     s1same,s2same:boolean;
     //setlength(scar,64*
     fst:string;
      bpos,spos:word;
      curfield:byte;curfieldstart,curlinestart:word;
      p,p1,p2,prevp2:pchar;   bstart:pointer;   j,newc:word;
      maxf:longword;
      //ch:char;
      procedure writerec(recpos:longword);
       var i,j:word;
         begin
         write(^j'');//,'###',recpos,' ;',sts[1]-1,'/',sts[2]-sts[1],'_',curlinestart,':',recs[recpos].a[1],^j'A:');
         for j:=1 to 16 do if recs[recpos].a[j]=#0 then break else write(recs[recpos].a[j]);
         write(' ');
         //for j:=i-1 downto curlinestart+sts[2]+sts[1]+2 do
         for j:=1 to 16 do if recs[recpos].b[j]=#0 then break else write(recs[recpos].b[j]);
         write('=',recs[recpos].f);
         maxf:=freq;
         end;

 begin
  setlength(recs,990000);
  fvirta := TfileStream.Create('nomadj.pairs',fmopenread);;
  fvirta.Position := 0;  // Ensure you are at the start of the file
  doneall:=false;
  donebuf:=true;//in1:=true;in2:=false;
  s1:=0;s2:=0;s3:=0;
  curlinestart:=4097;
  curfield:=1;
  recpos:=1;spos:=0;maxf:=0;
  s1same:=true;s2same:=true; //tottta kunnes toisin todistetaan
  while fvirta.Position<fvirta.Size do
  begin
    //if recpos>1000000 then break;
    if recpos>1 then
    begin
    end;
    if curlinestart<4097 then move(buf[curlinestart],buf[1],4097-curlinestart);
    //writeln(^j^j'****moved ',4097-curlinestart,' from:',curlinestart+1);
    fvirta.read(buf[4097-curlinestart+1],curlinestart-1);
    //writeln(^j^j'luettiin ',curlinestart-1,' merkkiä  kohtaan:',4097-curlinestart+1);
    newc:=4097-curlinestart+1;
    curlinestart:=1;

    if recpos>9999999 then
    begin
     write(recpos,' ');
   // writeln(^j,buf[4096],'NEW::');
       for i:=91 to 40 do write(chr(buf[i]),',');
       //writeln('!!field:',curfield);
       //readln;
    end;
    //fvirta.read(buf[1],4096);
    for i:=newc to 4096 do
    begin
      //write(i,'#',buf[i]);
      ch:=buf[i];
      if (ch=32) then
      begin
        //write('i:',i,' s:',curlinestart,' f:',curfield);
        if curfield=1 then sts[1]:=i-curlinestart
        else sts[2]:=i-sts[1]-curlinestart-1;
        //write('!>',curfield,'@',sts[curfield]);
        inc(curfield);
      end
      else if ch=10 then
      begin
        freq:=0;
        //for j:=0 to 4 do if buf[i-j-1]=32 then break else write(     buf[i-j-1]-48,'*',round(intpower(10,j)),'.');
        try
        for j:=0 to 4 do if buf[i-j-1]=32 then break else  inc(freq,(buf[i-j-1]-48)*round(intpower(10,j)));
        except write(j,'^',i,'/chr:',buf[i-j-1]);raise;end;
       //write('==');
        curfield:=1;
        //if not s1same then writeln(^j^j^j'************');
        if (s1same) and (s2same) then begin
          recpos:=recpos-1;
          //write(^j,'EIKU',freq,'/',recs[recpos].f);
          freq:=recs[recpos].f+freq;
        end;
        //else
        begin
         recs[recpos].f:=freq;
         move(buf[curlinestart],recs[recpos].a,sts[1]);
         move(buf[curlinestart+sts[1]+1],recs[recpos].b,sts[2]);
         //write(^j,pchar(recs[recpos].a),'+',recs[recpos].b,'=',recs[recpos].f);
         s1same:=true;s2same:=true;curfield:=1;
         inc(recpos);
        end;
        curlinestart:=i+1;
      end
      //else /f recpos=1 then continue
      else  //peruskaurat
      begin
        try
        if recpos=1 then s1same:=false else
        if curfield=1 then
        begin
          if CHAR(ch)<>recs[recpos-1].a[i-curlinestart+1] then s1same:=false;
        end
        else  if curfield=2 then if char(ch)<>recs[recpos-1].b[i-curlinestart-sts[1]] then s2same:=false;
        //if recpos>1 then if curfield=2 then    write(' /',s2same,chr(ch),i-curlinestart-sts[1],'|',recs[recpos-1].b[i-curlinestart-sts[1]],'/');
        except write(^j,curfield,' ',i-sts[2],'rec:',recpos,' i:',i,' /linest:',curlinestart,'\1:',sts[1],'/2:',sts[2],'!');end;

      end;
      //if curfield=2 then write('(',i-curlinestart-sts[1]+1,char(ch),')');
    end;
   end;
   writeln(recpos);
  //for i:=1 to recpos do writerec(i);
   //savebin:
end;

constructor totus.readfromfile(mfiln,rfiln,vfiln:string);
var i,j,v,r,a:word;
    vfreq,afreq:aRRAY of longword;
    //vcount,acount:longword;
    tottot:longword;
    //vc:twcounter;
    tietty,tiettysum:qword;
    //rownames,varnames,
      palat:tstringlist;
    sum,ap1,ap2,ci,ri,rn,cn,sf,n:qword;
    f:text;
    vacoocs:  array of qword; //,acoocs,vccoocs
    verbs:tstringlist;
    acounts,vcounts:tlist;
    pala,prev:string;tu:tww;
begin
        // write(^j,(sizeof(tu)));exit;
  palat:=tstringlist.create;
 palat.delimiter:=' ';
 palat.strictdelimiter:=true;

 //palat:=tlist.create;
 acounts:=tlist.create;
 vcounts:=tlist.create;
 palat.loadfromfile(vfiln);
 writeln('xxxxxxxxxxxxxxxxxx');
 acounts.add(pointer(0));
 vcounts.add(pointer(0));
 for a:=0 to palat.count-1 do acounts.add(pointer(1+strtointdef(copy(palat[a],pos(' ',palat[a])+1),0)));
 palat.loadfromfile(rfiln);
 for a:=0 to palat.count-1 do vcounts.add(pointer(1+strtointdef(copy(palat[a],pos(' ',palat[a])+1),0)));

 onjo:=tlist.create;
 if rlen=0 then rlen:=40;
 verbs:=tstringlist.create;
 varnames:=tstringlist.create;
 rownames:=tstringlist.create;
 //varnames.sorted:=true;
 //rownames.sorted:=true;
 rownames.loadfromfile(rfiln); //huom tyhjä eka rivi
 rownames.insert(0,'0 eiverbi');
 varnames.loadfromfile(vfiln);
 varnames.insert(0,'0 eiadv');
 varcount:=varnames.count-1;
 obscount:=rownames.count-1;
 setlength(vmax,max(varcount+1,obscount+1));
 setlength(vmaxval,max(varcount+1,obscount+1));
 //setlength(maxclusw,vcount);
 //setlength(maxclusa,acount);
 setlength(afreq,varcount+2);  //paikka [0] on ylimääräinen ..
 setlength(vnames,obscount+2);  //globaali muuttuja, ei otuksen osa
 setlength(anames,varcount+2);

 setlength(vfreq,obscount+1);
 //setlength(vcoocs,vcount*vcount); setlength(acoocs,acount*acount); setlength(vacoocs,acount*vcount);
 //huhhuh...
 writeln(^j'a',varcount,' v',obscount,' r:',rlen);
 for v:=1 to rownames.count-1 do     //tiedostossa sattuu olemaan "sana totfrekv"
 begin
  try
   //write(v,rownames[v],' ');
    ap1:=pos(' ',rownames[v]);
    //if ap1>17 then writeln('***',vees[v]);
    vnames[v]:=copy(rownames[v],1,ap1-1);
    vfreq[v]:=strtointdef(copy(rownames[v],ap1+1),0);
    rownames[v]:=vnames[v]; //vees
    except writeln('nonononon',v,' ',length(vst),' ',rownames.count);end;
 end;
 writeln(^j'READFILE******************a',varcount,' v',obscount, vnames[obscount]);
 for a:=0 to varnames.count-1 do
 begin
   try
    ap1:=pos(' ',varnames[a]);
    anames[a]:=copy(varnames[a],1,ap1-1);
    afreq[a]:=strtointdef(copy(varnames[a],ap1+1),0);
    varnames[a]:=anames[a]; //aas
    except writeln('no1nononon',v,' ',length(vst));end;
 end;
 vst:=vnames;ast:=anames;
 //assign(f,'va.isot');
 assign(f,mfiln);
 reset(f);
 setlength(vacoocs,(obscount+1)*(varcount+1));
 setlength(vars,(obscount+1)*(rlen+1));
 setlength(vals,(obscount+1)*(rlen+1));
 writeln(^j'Luedata',varcount,' v',obscount);
 //writeln(rownames.text);
 while not eof(f) do  //  ymmätään jokaiselle sanalle (adv) kaikki yhteisesiintymät (verbien) anssa
 begin
    try
   readln(f,pala);
   palat.delimitedtext:=pala;
   ri:=rownames.indexof(palat[0]);
   ci:=varnames.indexof(palat[2]);
   //rn:=strtointdef(palat[1],0);
   //cn:=strtointdef(palat[3],0);
   rn:=vfreq[ri];
   cn:=afreq[ci];
   n:=strtointdef(palat[4],0);
   sf:=round((n*1000000000)/ (0+(rn*cn) ));
    except writeln('dataonkelmaXXX');raise; end;
   if sf>10000 then write(' ',sf,palat[0],'.',palat[2]);
   //if palat[2]='tietenkin' then tiettysum:=tiettysum+sf;
   //if sf=0 then write('*',n);
    //sf:=round(n*1000/(rn));//* cn));
   //sf:=round(n*10000/(rn));
   //if sf>1000 then write(' ',sf,'/',rownames[ri],varnames[ci]);
   try
   //if sf>256 then writeln(' ',sf,'     ',pala);
   //vacoocs[ri*varcount+ci]:=min(2560,sf);   //täysmatriisia pukkaa...
   vacoocs[ri*varcount+ci]:=sf;   //täysmatriisia pukkaa...
   except writeln('dataonkelma:',sf,' ',ri,'/',ci,':',ri*obscount+ci,'/',obscount,rownames[ri],varnames[ci]); end;
 end;
 writeln(^j^j'datat ok');
 for i:=1 to obscount do
 begin
    sum:=0;
    write('.',VST[I]);
    try
    for j:=1 to varcount do
    begin
      sum:=sum+vacoocs[i*varcount+j];
      if i<>j then //if vacoocs[i*varcount+j]>1000 then   //  write(vacoocs[i*varcount+j],' ');
       //if vars[i*rlen]<>0 then
       n:=onksiso(@vars[i*rlen],@vals[i*rlen],j,vacoocs[i*varcount+j],rlen,i);
      end;
    except write(^j, j,'FAIL  ',i,';',j,'v:[',i*varcount+j,'] ',varcount);
      //if n<>999 then write(n,' ');
     //if vacoocs[i*varcount+j]>50 then writeln('*',i,';',j,'x:[',vars[i*rlen],'][',varnames[j],']',i*rlen, '  ',length(vars));
    end;
    //writeln(vst[i],' ',sum,' ');
 end;
 //print('mikämättää');
 writeln(^j,^j'donesparse');
 for i:=1 to obscount do
  vals[i*rlen]:=vfreq[i] div 1000;
 for j:=9990 to varcount do
 begin
   sum:=0;
   for i:=1 to obscount do sum:=sum+vacoocs[i*varcount+j];
   if 100*afreq[j] div sum>500 then writeln(ast[j],' ',sum,' ',anames[j],' ',afreq[j],'    ',100*afreq[j] div sum);
 end;
 //print('read');
 //varnames.Free;
 //rownames.Free;
 palat.free;
 //transponoi;
end;





end.






 function addtomat(row:word;avar,aval:word):word;    // jos avar valmiiksi listalla, lisätään avar siihen. Jos ei, niin lisätään avar:aval
    var i,j,k,x:integer;pvars,pvals:pword;olijo:boolean;
    begin
    //  if row=8 then write(' !!(',row,'.',avar,':',totvars[row*varcount+1],'=',totvals[row*varcount+1],') : ');
      try     //varm ja valm sarakkeet ovat sorttaamattomia
      for j:=1 to obscount do  //valm row ja col vaihtuneet. Parametri row uuden matriisin row
      begin
        if (totvars[row*obscount+j]=avar)  then
        begin     //vals: 2 4 8, aval=3 -> j=2
          totvals[row*obscount+j]:=totvals[row*obscount+j]+aval;
          write(^j,'********[',row,'+',avar,'] ');
          exit;
        end;
        if (totvars[row*obscount+j]=0) then  //requiring 2-byte numbers
        begin     //vals: 2 4 8, aval=3 -> j=2
          totvars[row*obscount+j]:=avar;
          totvals[row*obscount+j]:=aval;
          exit;
        end;
      end;
      except write(^j^j,'FAIxx(','/',avar,'=',aval,')-topos:',row,':',j,'  ');end;
    end;
end;
