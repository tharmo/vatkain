unit uusotus;
{ tarkoitus tehdä matriisityyppinen luokka, vähän alemmantasoinen kuin otusmat.pas
 tarjoaa vain viittaukset matriisin soluihin


}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type tmatrow=record
 a,b,c:word;
end;

type tmatlookalike=class(tobject)
  //function
  data:array of tmatrow;
end;
implementation

end.

