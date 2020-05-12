unit ufilestack;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, deployment_parser_module, deployment_parser_types;

type

  TFileStackEntry = record
    Filename:  string;
    List:      TStringList;
    InputLine: integer;
    class operator = (fse1, fse2: TFileStackEntry): boolean;
  end;

  TFileStack = class(specialize TFPGList<TFileStackEntry>)
    private
      FOnMonitor: TLCGMonitorProc;
      FParser:    TLCGParser;
      function  Exists(const _fn: string): boolean;
      function  GetEOF: boolean;
      function  GetFilename: string;
      function  GetInputLine: integer;
      function  GetTop: TFileStackEntry;
    public
      constructor Create(_parser: TLCGParser);
      function  GetLine: string;
      procedure Pop;
      procedure Push(filename: string);
      procedure Monitor(LogType: TLCGLogType; const Message: string);
      property EOF:       boolean          read GetEOF;
      property Filename:  string           read GetFilename;
      property InputLine: integer          read GetInputLine;
      property OnMonitor: TLCGMonitorProc  read FOnMonitor   write FOnMonitor;
      property Top:       TFileStackEntry  read GetTop;
  end;

implementation

uses
  uutility, uassembler;

{ TFileStackEntry }

class operator TFileStackEntry.= (fse1, fse2: TFileStackEntry): boolean;
begin
  Result := (fse1.Filename = fse2.Filename);
end;



{ TFileStack }

constructor TFileStack.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
end;

function TFileStack.Exists(const _fn: string): boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    if SameFilename(Items[i].Filename,_fn) then
      Exit(True);
end;

function TFileStack.GetEOF: boolean;
begin
  Result := (Top.InputLine >= Top.List.Count);
end;

function TFileStack.GetFilename: string;
begin
  Result := Top.Filename;
end;

function TFileStack.GetInputLine: integer;
begin
  Result := Top.InputLine;
end;

function TFileStack.GetLine: string;
var rec: TFileStackEntry;
begin
  if EOF then
    Monitor(ltInternal,'Attempt to read past end of file on ' + Filename);
  Result := ExpandTabs(Top.List[InputLine],TAssembler(FParser).TabSize);
  rec := Top;
  Inc(rec.InputLine);
  Items[Count-1] := rec;
end;

function TFileStack.GetTop: TFileStackEntry;
begin
  if Count = 0 then
    Monitor(ltInternal,'Attempting to take the top of an empty file stack');
  Result := Items[Count-1];
end;

procedure TFileStack.Monitor(LogType: TLCGLogType; const Message: string);
begin
  FParser.Monitor(LogType,Message);
end;

procedure TFileStack.Pop;
begin
  Monitor(ltDebug,'Closing file ' + Top.Filename);
  Delete(Count-1);
end;

procedure TFileStack.Push(filename: string);
var rec: TFileStackEntry;
begin
  filename := ExpandFilename(filename);
  if Exists(filename) then
    Monitor(ltError,'Circular file reference in file ' + filename);
  Monitor(ltDebug,'Opening file ' + filename);
  rec.Filename  := filename;
  rec.List := TStringList.Create;
  rec.List.LoadFromFile(filename);
  rec.InputLine := 0;
  Add(rec);
end;

end.

