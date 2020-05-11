unit uoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOutputSequence = array of byte;

  TOutput = class(TObject)
    private
      FBytes: array[UINT16] of byte;
      FUsed:  array[UINT16] of boolean;
      function Highest: integer;
      function Lowest: integer;
    public
      procedure Clear;
      procedure Write(const _array: TOutputSequence; _addr: UINT16; _len: integer);
      procedure SaveHex(const _filename: string);
      procedure SaveObject(const _filename: string);
  end;

implementation

{ TOutput }

procedure TOutput.Clear;
var i: UINT16;
begin
  for i in UINT16 do
    FUsed[i] := False;
  for i in UINT16 do
    FBytes[i] := $00;
end;

function TOutput.Highest: integer;
var i: integer;
begin
  for i := High(UINT16) downto Low(UINT16) do
    if FUsed[i] then
      break;
  Result := i;
end;

function TOutput.Lowest: integer;
var i: integer;
begin
  for i in UINT16 do
    if FUsed[i] then
      break;
  Result := i;
end;

procedure TOutput.SaveHex(const _filename: string);
var sl: TStringList;
    i:  integer;
    column: integer;
    s:      string;
begin
  if _filename = '' then
    Exit;
  sl := TStringList.Create;
  s := '';
  try
    column := 0;
    for i := Lowest to Highest do
      begin
        if column = 16 then
          begin
            sl.Add(s);
            s := '';
            column := 0;
          end;
        if column = 0 then
          s := s + Format('%4.4X:',[i]);
        s := s + Format(' %2.2X',[FBytes[i]]);
        Inc(column);
      end;
    if s <> '' then
      sl.Add(s);
    sl.SaveToFile(_filename);
  finally
    sl.Free;
  end;
end;

procedure TOutput.SaveObject(const _filename: string);
var addr: UINT16;
    size: UINT16;
    strm: TFileStream;
begin
  if _filename = '' then
    Exit;
  size := Highest-Lowest;
  addr := Lowest;
  strm := TFileStream.Create(_filename,fmCreate,fmShareDenyWrite);
  try
    strm.Write(addr,sizeof(addr));
    strm.Write(size,sizeof(addr));
    strm.Write(FBytes[addr],size);
  finally
    strm.Free;
  end;
end;

procedure TOutput.Write(const _array: TOutputSequence; _addr: UINT16; _len: integer);
var i: integer;
begin
  for i := 1 to _len do
    if FUsed[_addr] then
      raise Exception.Create(Format('Attempt to write to an address %4.4X which is already written to',[_addr]))
    else
      begin
        FUsed[_addr] := True;
        FBytes[_addr] := _array[i-1];
        Inc(_addr);
      end;
end;

end.

