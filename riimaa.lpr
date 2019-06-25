program riimaa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, //riimitys,
  nominit, riimiutils, verbikama, etsi,syno,
  prosessi, //sanataulu,
  //ngrams,
  RIIMIUUS,otusmat, uusotus, _ARRAYS  //, roskaa
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }
type taste=record
   a:ansichar;
   f,t:ansistring;
end;

type tasteet=array[0..15] of taste;
const aste: tasteet=((a:'_';f:'';t:''),(a:'A';f:'kk';t:'k'), (a:'B';f:'pp';t:'p'), (a:'C';f:'tt';t:'t'), (a:'D';f:'k';t:'-'), (a:'E';f:'p';t:'v'), (a:'F';f:'t';t:'d'), (a:'G';f:'nk';t:'ng'), (a:'H';f:'mp';t:'mm'), (a:'I';f:'lt';t:'ll'), (a:'J';f:'nt';t:'nn'), (a:'K';f:'rt';t:'rr'), (a:'L';f:'k';t:'j'), (a:'M';f:'k';t:'v'), (a:'N';f:'s';t:'n'), (a:'O';f:'t';t:'-'));


procedure TMyApplication.DoRun;
var
  ErrorMsg: String;noms:tnominit;verbs:tverbit;//riimitin:triimitin;
  i:word;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
  //writeln('<pre>');nomforms;terminate;exit;
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  //if HasOption('f', 'fixteet') then begin
  if paramstr(1)='test' then
  begin
    //writeln('PARAMS:',paramstr(1),' ',paramstr(2),' ',paramstr(3),' ');
    //turkuana;
       SANASTO:=tsanasto.create;  //luo myös globaalit verbit, nominit, muodot

    Terminate;
    Exit;
  end;
  if paramstr(1)='gut' then
  begin
    teegut;
    Terminate;
    Exit;
  end;
  if paramstr(1)='klk_lemmaa' then
  begin
    //writeln('lemmaa');
    //ngrams; //siivoa ngram-tiedostoa ajettavaksi hfts/figen läpi
    ngramlemmas(paramstr(2));
    Terminate;
    Exit;
  end;
  if paramstr(1)='grammat' then
  begin
    //writeln('lemmaa',paramstr(0),'\1',paramstr(1),'/2',paramstr(2),'/3',paramstr(3),'!');     //readln;
    //ngrams; //siivoa ngram-tiedostoa ajettavaksi hfts/figen läpi
    //lastenlapset;terminate;exit;

    if paramstr(3)='x' then grammat(paramstr(2)) else
      writeln('listgrams');
    Terminate;
    Exit;
  end;
  if paramstr(1)='klksiivo' then
  begin
    //ngrams; //siivoa ngram-tiedostoa ajettavaksi hfts/figen läpi
    getngrams;
    Terminate;
    Exit;
  end;
  if paramstr(1)='etsi' then
  begin

    //writeln('<li>verbeja');
    testmat:=totus.readfromfile('vauus.fix','verb.nn','adv.nn');
    //totus.create(20,20,10);
    testmat.test;
    //uustemppu;
    terminate;exit;
    teetemput;
    //if 'vana'='uusi' then riimei;
    //noms:=tnominit.create;
    //    verbs:=tverbit.create;
   //writeln('<li>Lataa');
   //noms.lataasanat('noms4.csv');
   writeln('<li>....');
   //noms.listaasanat;
   //writeln('<h1>oisko siinä?</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='testaa' then  begin
    writeln('<li>tavutusta');
     testmat:=totus.create(20,20,10);
     testmat.readnou_adj;
    Terminate;
    Exit;
  end;
  if paramstr(1)='fixlista' then  begin
    fixlista('noms4.csv','nomstosort.csv');
   // writeln(aste
    writeln('<li>vanha listaa uuteen muotoon');
    hyphenfirev('ulkoaisatintroryoksiniidihappo',nil);
    hyphenfirev('striimitin',nil);
    writeln('<li>....');
   //noms.listaasanat;
writeln('<h1>pikalista?</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='etsinom' then  begin
    writeln('<li>Luo');
    //riimitin:=triimitin.create;
  // noms:=tnominit.create;
   writeln('<li>Luotu');
   //noms.listaasanat;
   //noms.etsi;
   writeln('<li>Lataa');
   writeln('<h1>tehty?</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='listaa' then  begin
    writeln('<li>Luo');
   //noms:=tnominit.create;
   //writeln('<li>Lataa');
   //noms.lataasanat('noms4.csv');
   writeln('<li>Listaa');
   //noms.listaasanat;
writeln('<h1>pikalista?</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='fixmonikot' then  begin
    //fixmonikot;
    writeln('<h1>fiksattiiin monikkosanoja</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='listaaoudot' then  begin
    //listaaoudot;
    writeln('<h1>ei talletettu tiedostoon ""</h1>');
    Terminate;
    Exit;
  end;
  if paramstr(1)='listaa' then  begin
    //rx:=triimitin.create;
    //rx.nominit.listaa;
    Terminate;
    Exit;
  end;
  //rx:=triimitin.create;
  //rx.nominit.luolista;
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

