unit uassembler;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, deployment_parser_module, deployment_parser_types,
  ufilestack, usymbol;


type

  TAssembler = class(TLCGParser)
    private
      FBytesGenerated: integer;
      FFilenameSrc:    string;
      FFileStack:      TFileStack;
      FLineCount:      integer;
      FPass:           integer;
      FProcArray:      array of TLCGParserProc;
      FStreamLog:      TFileStream;
      FSymbols:        TSymbolTable;
      FTabSize:        integer;
      FVerbose:        boolean;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      procedure FilesClose;
      procedure FilesOpen;
      procedure InitPass;
      procedure InitStart;
      procedure ProcessFile(const _fn: string);
      procedure ProcessLine(const _line: string);
      procedure RegisterProc(const _procname: string; _proc: TLCGParserProc);
      procedure RegisterProcs;
      procedure SetFilenameSrc(const _fn: string);
      procedure SetOnMonitor(_monitor: TLCGMonitorProc);
    public
      FilenameHex:    string;
      FilenameLst:    string;
      FilenameLog:    string;
      FilenameMap:    string;
      FilenameObj:    string;
      constructor Create;
      destructor Destroy; override;
      procedure Assemble;
      procedure AssemblePass(_pass: integer);
      procedure Monitor(LogType: TLCGLogType; const Message: string); override;
      procedure Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const); override;
      function  Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
      property FilenameSrc: string          read FFilenameSrc write SetFilenameSrc;
      property OnMonitor:   TLCGMonitorProc read FOnMonitor   write SetOnMonitor;
      property TabSize:     integer         read FTabSize     write FTabSize;
  end;


implementation

uses
  uexpression;


{ TAssembler }

constructor TAssembler.Create;
begin
  inherited Create;
  LoadFromResource('ASM65');
  SetLength(FProcArray,Rules);
  FSymbols := TSymbolTable.Create;
  FFileStack := TFileStack.Create(Self);
  FPass := 0;
  FTabSize := 4;
  FVerbose := False;
  FOnReduce := @Reduce;
  RegisterProcs;
end;

destructor TAssembler.Destroy;
begin
  FFileStack.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TAssembler.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
begin
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FSymbols.Define(FPass,symbolname,expression);
  Result.Buf := '';
end;

function TAssembler.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
var parentfile: string;
    myfile:     string;
    saveddir:   string;
begin
  saveddir := GetCurrentDir;
  parentfile := FFileStack.Filename;
  SetCurrentDir(ExtractFilePath(parentfile));
  myfile := StringFromVariable(_parser.ParserStack[_parser.ParserSP-1].Buf);
  myfile := ExpandFilename(myfile);
  SetCurrentDir(saveddir);
  ProcessFile(myfile);
  Result.Buf := '';
end;

function TAssembler.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromHexLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
end;

function TAssembler.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromOctLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromStringQuoted(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

procedure TAssembler.Assemble;
begin
  FilesOpen;
  Monitor(ltVerbose,'Assembly started %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)]);
  InitStart;
  try
    AssemblePass(1);
    AssemblePass(2);
  finally
    Monitor(ltInfo,'%d lines assembled, %d bytes generated',[FLineCount, FBytesGenerated]);
    Monitor(ltVerbose,'Assembly ended %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)]);
    FilesClose;
  end;
end;

procedure TAssembler.AssemblePass(_pass: integer);
begin
  FPass := _pass;
  Monitor(ltInfo,'ASSEMBLER PASS %d',[_pass]);
  InitPass;
  ProcessFile(FFilenameSrc);
end;

procedure TAssembler.FilesClose;
begin
  if Assigned(FStreamLog) then
    begin
      Monitor(ltDebug,'Closing %s',[FilenameLog]);
      FreeAndNil(FStreamLog);
    end;
end;

procedure TAssembler.FilesOpen;
begin
  try
    Monitor(ltDebug,'Opening %s',[FilenameLog]);
    FStreamLog := TFileStream.Create(FilenameLog,fmCreate,fmShareDenyWrite);
  except
    Monitor(ltError,'Unable to create log file %s',[FilenameLog]);
  end;
end;

procedure TAssembler.InitPass;
begin
  FBytesGenerated := 0;
  FLineCount := 0;
end;

procedure TAssembler.InitStart;
begin
  Monitor(ltWarAndPeace,'HEX filename = %s',[FilenameHex]);
  Monitor(ltWarAndPeace,'LST filename = %s',[FilenameLst]);
  Monitor(ltWarAndPeace,'LOG filename = %s',[FilenameLog]);
  Monitor(ltWarAndPeace,'MAP filename = %s',[FilenameMap]);
  Monitor(ltWarAndPeace,'OBJ filename = %s',[FilenameObj]);
  Monitor(ltWarAndPeace,'tab value = %d',[TabSize]);
end;

procedure TAssembler.Monitor(LogType: TLCGLogType; const Message: string);
var s: string;
begin
  if FFileStack.Count > 0 then
    FInputLineSave := FFileStack.InputLine;
  inherited Monitor(LogType,Message);
  if Assigned(FStreamLog) and (LogType <= FLogLevel) then
    begin
      s := Message + #13 + #10;
      FStreamLog.Write(s[1],Length(s));
    end;
end;

procedure TAssembler.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TAssembler.ProcessFile(const _fn: string);
var asmline: string;
begin
  Monitor(ltWarAndPeace,'Processing file %s',[_fn]);
  FFileStack.Push(_fn);
  try
    while not FFileStack.EOF do
      begin
        asmline := FFileStack.GetLine;
        if LogLevel >= ltDebug then
          Monitor(ltDebug,'> ' + asmline);
        Inc(FLineCount);
        // Assemble the line here
        if asmline <> '' then
          ProcessLine(asmline);
      end;
  finally
    FFileStack.Pop();
  end;
end;

procedure TAssembler.ProcessLine(const _line: string);
var strm: TStringStream;
begin
  strm := TStringStream.Create(_line);
  try
    Parse(strm);
  finally
    strm.Free;
  end;
end;

function TAssembler.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  if Assigned(FProcArray[RuleIndex]) then
    Result := FProcArray[RuleIndex](Parser)
  else
    Monitor(ltInternal,'Code not defined for rule no. %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TAssembler.RegisterProc(const _procname: string; _proc: TLCGParserProc);
var i: integer;
    done_one: boolean;
begin
  done_one := False;
  for i := 0 to Rules-1 do
    if RuleProcs[i] = _procname then
      begin
        FProcArray[i] := _proc;
        done_one := True;
      end;
  if not done_one then
    Monitor(ltInternal,'Could not find procedure %s in grammar',[_procname]);
end;

procedure TAssembler.RegisterProcs;
begin
  RegisterProc('ActCopy1',          @ActCopy1);
  RegisterProc('ActDirDefineExpr',  @ActDirDefineExpr);
  RegisterProc('ActDirInclude',     @ActDirInclude);
  RegisterProc('ActHexLiteral',     @ActHexLiteral);
  RegisterProc('ActIgnore',         @ActIgnore);
  RegisterProc('ActOctLiteral',     @ActOctLiteral);
  RegisterProc('ActStringConstant', @ActStringConstant);
  RegisterProc('ActValueSymbol',    @ActValueSymbol);
end;

procedure TAssembler.SetFilenameSrc(const _fn: string);
begin
  FFilenameSrc := _fn;
  if FilenameHex  = '' then FilenameHex  := ChangeFileExt(_fn,'.hex');
  if FilenameLst  = '' then FilenameLst  := ChangeFileExt(_fn,'.lst');
  if FilenameLog  = '' then FilenameLog  := ChangeFileExt(_fn,'.log');
  if FilenameMap  = '' then FilenameMap  := ChangeFileExt(_fn,'.map');
  if FilenameObj  = '' then FilenameObj  := ChangeFileExt(_fn,'.obj');
end;

procedure TAssembler.SetOnMonitor(_monitor: TLCGMonitorProc);
begin
  FOnMonitor := _monitor;
  if Assigned(FFileStack) then
    FFileStack.OnMonitor := _monitor;
end;

end.

