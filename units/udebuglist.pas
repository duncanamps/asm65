unit udebuglist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TDebugEntry = class(TObject)
    private
      FAddress:   uint16;
      FCodeLen:   uint16;
      FCodeObj:   array of byte;
      FStringLen: uint16;
      FStringObj: string;
    public
      constructor Create(_addr: uint16; _codesize: uint16; _code: array of Byte; _str: string);
      procedure WriteStream(Strm: TStream);
  end;

  TDebugList = class(specialize TFPGList<TDebugEntry>)
    public
      procedure SaveToFile(const Filename: string);
      procedure SaveToStream(Strm: TStream);
  end;

implementation


{ TDebugEntry }

constructor TDebugEntry.Create(_addr: uint16; _codesize: uint16; _code: array of Byte; _str: string);
var i: integer;
begin
  inherited Create;
  FAddress   := _addr;
  FCodeLen   := _codesize;
  if FCodeLen > 0 then
    begin
      SetLength(FCodeObj,_codesize);
      for i := 0 to _codesize-1 do
        FCodeObj[i] := _code[i];
    end;
  FStringLen := Length(_str);
  FStringObj := _str;
end;

procedure TDebugEntry.WriteStream(Strm: TStream);
begin
  Strm.Write(FAddress,Sizeof(FAddress));
  Strm.Write(FCodeLen,SizeOf(FCodeLen));
  if FCodeLen > 0 then
    Strm.Write(FCodeObj[0],FCodeLen);
  Strm.Write(FStringLen,SizeOf(FStringLen));
  Strm.Write(FStringObj[1],FStringLen);
end;


{ TDebugList }

procedure TDebugList.SaveToFile(const Filename: string);
var strm: TFileStream;
begin
  strm := TFileStream.Create(Filename,fmCreate);
  try
    SaveToStream(strm);
  finally
    strm.Free;
  end;
end;

procedure TDebugList.SaveToStream(Strm: TStream);
var i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].WriteStream(Strm);
end;

end.

