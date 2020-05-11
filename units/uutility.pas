unit uutility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ExpandTabs(const _s: string; tabsize: integer): string;
function IsPrime(_value: integer): boolean;
function NextPrime(_value: integer): integer;

implementation

function ExpandTabs(const _s: string; tabsize: integer): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(_s) do
    begin
      if _s[i] <> #9 then
        Result := Result + _s[i]
      else
        repeat
          Result := Result + ' ';
        until (Length(Result) mod tabsize) = 0;
    end;
end;

function IsPrime(_value: integer): boolean;
var divisor: integer;
begin
  divisor := 3;
  Result := True;
  if (_value > 3) and ((_value mod 2) = 0) then
    Exit(False);  // Even numbers > 2 aren't prime
  while divisor*divisor < _value do
    if (_value mod divisor) = 0 then
      Exit(False)
    else
      divisor := divisor + 2;
end;

function NextPrime(_value: integer): integer;
begin
  if (_value mod 2) = 0 then
    Inc(_value);
  while not IsPrime(_value) do
    _value := _value + 2;
  Result := _value;
end;

end.

