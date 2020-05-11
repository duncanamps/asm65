unit uexpression;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Forward declarations

function StripQuotes(const _s: string): string;
function VariableFromHexLiteral(const _s: string): string;
function VariableFromOctLiteral(const _s: string): string;

implementation


function StripQuotes(const _s: string): string;
begin
  Result := _s;
  if Length(Result) < 2 then
    raise Exception.Create('Trying to strip quotes from string which is too short ' + _s);
  if (_s[1] <> Chr(34)) or
     (_s[Length(_s)] <> Chr(34)) then
    raise Exception.Create('Trying to strip quotes which are not present ' + _s);
  Result := Copy(_s,2,Length(_s)-2);
end;

function VariableFromHexLiteral(const _s: string): string;
var decval: int64;
begin
  decval := StrToInt(_s);
  Result := IntToStr(decval);
end;

function VariableFromOctLiteral(const _s: string): string;
var decval: int64;
    i:      integer;
begin
  decval := 0;
  for i := 1 to Length(_s) do
    decval := decval * 8 + (Ord(_s[i])-Ord('0'));
  Result := IntToStr(decval);
end;

end.

