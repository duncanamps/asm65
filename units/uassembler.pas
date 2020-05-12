unit uassembler;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, deployment_parser_module, deployment_parser_types,
  ufilestack, usymbol, uasmglobals, uoutput, uifstack;


type

  TAssembler = class(TLCGParser)
    private
      FAddr:           UINT16;
      FAddrMode:       TAddrMode;
      FAssemblyStart:  TDateTime;
      FBytesFromLine:  integer;
      FBytesTotal:     integer;
      FFilenameSrc:    string;
      FFileStack:      TFileStack;
      FIfStack:        TIfStack;
      FIncludeNext:    string;
      FLineCount:      integer;
      FList:           boolean;
      FListNext:       boolean;
      FListing:        TStringList;
      FOutput:         TOutput;
      FOutputArr:      TOutputSequence;
      FOpCode:         TOpCode;
      FOrg:            UINT16;
      FPass:           integer;
      FProcArray:      array of TLCGParserProc;
      FStreamLog:      TFileStream;
      FSymbols:        TSymbolTable;
      FTabSize:        integer;
      FVerbose:        boolean;
      function  ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirNolist(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprFalse(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprTrue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpA(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbs(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbsX(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbsY(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpImmed(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpImpl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndX(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndY(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      procedure FilesClose;
      procedure FilesOpen;
      procedure InitPass;
      procedure InitStart;
      procedure OutputListingLine(const _asmline: string);
      procedure ProcessFile(const _fn: string);
      procedure ProcessLine(const _line: string);
      procedure RegisterProc(const _procname: string; _proc: TLCGParserProc);
      procedure RegisterProcs;
      procedure SetFilenameSrc(const _fn: string);
      procedure SetOnMonitor(_monitor: TLCGMonitorProc);
      procedure WriteMapFile;
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
  uexpression, strutils, fileinfo;

const CodeTable: array[TOpCode,TAddrMode] of integer = (
          (109,125,121,-1,-1,113,105,-1,97,101,117,-1,-1),
          (45,61,57,-1,-1,49,41,-1,33,37,53,-1,-1),
          (14,30,-1,-1,-1,-1,-1,-1,-1,6,22,-1,10),
          (-1,-1,-1,-1,-1,-1,-1,144,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,176,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,240,-1,-1,-1,-1,-1),
          (44,-1,-1,-1,-1,-1,-1,-1,-1,36,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,208,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,16,-1,-1,-1,-1,-1),
          (-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,80,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,112,-1,-1,-1,-1,-1),
          (-1,-1,-1,24,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,216,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,88,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,184,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (205,221,217,-1,-1,209,201,-1,193,197,213,-1,-1),
          (236,-1,-1,-1,-1,-1,224,-1,-1,228,-1,-1,-1),
          (204,-1,-1,-1,-1,-1,192,-1,-1,196,-1,-1,-1),
          (206,222,-1,-1,-1,-1,-1,-1,-1,198,214,-1,-1),
          (-1,-1,-1,202,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,136,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (77,93,89,-1,-1,81,73,-1,65,69,85,-1,-1),
          (238,254,-1,-1,-1,-1,-1,-1,-1,230,246,-1,-1),
          (-1,-1,-1,232,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,200,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (76,-1,-1,-1,108,-1,-1,-1,-1,-1,-1,-1,-1),
          (32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (173,189,185,-1,-1,177,169,-1,161,165,181,-1,-1),
          (174,-1,190,-1,-1,-1,162,-1,-1,166,-1,182,-1),
          (172,188,-1,-1,-1,-1,160,-1,-1,164,180,-1,-1),
          (78,94,-1,-1,-1,-1,-1,-1,-1,70,86,-1,74),
          (-1,-1,-1,234,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (13,29,25,-1,-1,17,9,-1,1,5,21,-1,-1),
          (-1,-1,-1,72,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,104,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,40,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (46,62,-1,-1,-1,-1,-1,-1,-1,38,54,-1,42),
          (110,126,-1,-1,-1,-1,-1,-1,-1,102,118,-1,106),
          (-1,-1,-1,64,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (237,253,249,-1,-1,241,233,-1,225,229,245,-1,-1),
          (-1,-1,-1,56,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,248,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,120,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (141,157,153,-1,-1,145,-1,-1,129,133,149,-1,-1),
          (142,-1,-1,-1,-1,-1,-1,-1,-1,134,-1,150,-1),
          (140,-1,-1,-1,-1,-1,-1,-1,-1,132,148,-1,-1),
          (-1,-1,-1,170,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,168,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,186,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,138,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,154,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,152,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1));

  InstructionSizes: array[TAddrMode] of integer =
    (
      3,       // ADM_ABS
      3,       // ADM_ABSX
      3,       // ADM_ABSY
      1,       // ADM_IMPL
      3,       // ADM_IND
      3,       // ADM_INDY
      2,       // ADM_LIT
      2,       // ADM_REL
      3,       // ADM_XIND
      2,       // ADM_ZPG
      2,       // ADM_ZPGX
      2,       // ADM_ZPGY
      1        // ADM_ACC
    );

{ TAssembler }

constructor TAssembler.Create;
begin
  inherited Create;
  LoadFromResource('ASM65');
  SetLength(FProcArray,Rules);
  FSymbols := TSymbolTable.Create;
  FFileStack := TFileStack.Create(Self);
  FIfStack   := TIfStack.Create(Self);
  FOutput := TOutput.Create;
  FListing := TStringList.Create;
  FPass := 0;
  FTabSize := 4;
  FVerbose := False;
  FOnReduce := @Reduce;
  RegisterProcs;
end;

destructor TAssembler.Destroy;
begin
  FListing.Free;
  FOutput.Free;
  FIfStack.Free;
  FFileStack.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TAssembler.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) =
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
  result.Buf := '1'
else
  result.Buf := '0';
end;

function TAssembler.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <>
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
var sl: TStringList;
    i:   integer;
    bval: integer;
begin
  if not FIfStack.Allowed then
    Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := _parser.ParserStack[_parser.ParserSP-1].Buf;
    FBytesFromLine := sl.Count;
    SetLength(FOutputArr,FBytesFromLine);
    for i := 0 to sl.Count-1 do
      begin
        bval := StrToInt(sl[i]);
        if (bval > 255) or (bval < -128) then
          Monitor(ltError,'Byte value %d not in range',[bval]);
        FOutputArr[i] := bval and $00FF;
      end;
    FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  finally
    sl.Free;
  end;
  Result.Buf := '';
end;

function TAssembler.ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  if not FIfStack.Allowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,symbolname);
  if message <> '' then
    Monitor(ltError,message);
  Result.Buf := '';
end;

function TAssembler.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  if not FIfStack.Allowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
  Result.Buf := '';
end;

function TAssembler.ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  if not FIfStack.Allowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s);
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    FOutputArr[i-1] := Ord(s[i]);
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  Result.Buf := '';
end;

function TAssembler.ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  if not FIfStack.Allowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s) + 1;
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    FOutputArr[i-1] := Ord(s[i]);
  FOutputArr[FBytesFromLine-1] := 0;
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  Result.Buf := '';
end;

function TAssembler.ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.ElseSwap;
  Result.Buf := '';
end;

function TAssembler.ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.Pop;
  Result.Buf := '';
end;

function TAssembler.ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  Monitor(ltError,_parser.ParserStack[_parser.ParserSP-1].Buf);
  Result.Buf := '';
end;

function TAssembler.ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  succeeded := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0;
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := FSymbols.Exists(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := not FSymbols.Exists(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
var parentfile: string;
    myfile:     string;
    saveddir:   string;
begin
  if not FIfStack.Allowed then
    Exit;
  saveddir := GetCurrentDir;
  parentfile := FFileStack.Filename;
  SetCurrentDir(ExtractFilePath(parentfile));
  myfile := _parser.ParserStack[_parser.ParserSP-1].Buf;
  myfile := ExpandFilename(myfile);
  SetCurrentDir(saveddir);
  FIncludeNext := myfile;
  Result.Buf := '';
end;

function TAssembler.ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  FList := True;
  FListNext := True;
  Result.Buf := '';
end;

function TAssembler.ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  Monitor(ltInfo,_parser.ParserStack[_parser.ParserSP-1].Buf);
  Result.Buf := '';
end;

function TAssembler.ActDirNoList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  FListNext := False;
  Result.Buf := '';
end;

function TAssembler.ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  FOrg := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  Result.Buf := '';
end;

function TAssembler.ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not FIfStack.Allowed then
    Exit;
  Monitor(ltWarning,_parser.ParserStack[_parser.ParserSP-1].Buf);
  Result.Buf := '';
end;

function TAssembler.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) +
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) and
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-2].Buf;
end;

function TAssembler.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) div
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprFalse(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '0';
end;

function TAssembler.ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                ',' +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(-StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) mod
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) *
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) xor
                         $FFFF);
end;

function TAssembler.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) or
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) shl
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) shr
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) -
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprTrue(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '1';
end;

function TAssembler.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) xor
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  s := _parser.ParserStack[_parser.ParserSP-2].Buf;
  if Length(s) < 1 then
    Monitor(ltError,'No argument provided to ASC() function');
  Result.Buf := IntToStr(Ord(s[1]));
end;

function TAssembler.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) shr 8);
end;

function TAssembler.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
var expr:     integer;
    trueval:  integer;
    falseval: integer;
    resval:   integer;
begin
  expr     := StrToInt(_parser.ParserStack[_parser.ParserSP-6].Buf);
  trueval  := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  falseval := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if expr <> 0 then
    resval := trueval
  else
    resval := falseval;
  Result.Buf := IntToStr(resval);
end;

function TAssembler.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) and $00FF);
end;

function TAssembler.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Pos(_parser.ParserStack[_parser.ParserSP-4].Buf,
                             _parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromHexLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
end;

function TAssembler.ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
var opstr: string;
    i:     TOpCode;
    iaddr: integer;
    found: boolean;
    instr: byte;
begin
  // We have the operand type and address in FAddrMode / FAddr, find opcode
  opstr := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  found := False;
  for i in TOpCode do
    if OpCodeText[i] = opstr then
      begin
        found := True;
        FOpCode := i;
        break;
      end;
  if not found then
    Monitor(ltError,'Opcode %s not known',[opstr]);
  // There are 13 address modes, we will have identified from a range of 9 so
  // far. Need to expand out to the missing four if possible (zpg / zpg,x /
  // zpg,y / rel)
  if (FAddrMode = ADM_ABS) and (FAddr < $100) then
    FAddrMode := ADM_ZPG;
  if (FAddrMode = ADM_ABSX) and (FAddr < $100) then
    FAddrMode := ADM_ZPGX;
  if (FAddrMode = ADM_ABSY) and (FAddr < $100) then
    FAddrMode := ADM_ZPGY;
  if (FOpCode in [OPC_BCC,OPC_BCS,OPC_BEQ,OPC_BMI,OPC_BNE,OPC_BPL,OPC_BVC,OPC_BVS]) then
    begin
      FAddrMode := ADM_REL;
      iaddr := FAddr - (FOrg+2);
      if (iaddr < -128) or (iaddr > 127)  then
        Monitor(ltError,'Range error on relative instruction');
      FAddr := iaddr and $00FF;
    end;
  // Check that we have a valid instruction
  if CodeTable[FOpCode][FAddrMode] < 0 then
    Monitor(ltError,'Illegal opcode and operand combination');
  instr := CodeTable[FOpCode][FAddrMode];
  // @@@@@ Code here to write out instruction data
  if FIfStack.Allowed then
    begin
      FBytesFromLine := InstructionSizes[FAddrMode];
      SetLength(FOutputArr,FBytesFromLine);
      FOutputArr[0] := instr;
      if FBytesFromLine > 1 then FOutputArr[1] := FAddr and $00FF;
      if FBytesFromLine > 2 then FOutputArr[2] := FAddr shr 8;
      FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
    end;
  Result.Buf := '';
end;

function TAssembler.ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
begin
  symbolname := _parser.ParserStack[_parser.ParserSP-2].Buf;
  expression := IntToStr(FOrg);
  FSymbols.Define(FPass,symbolname,expression);
  Result.Buf := '';
end;

function TAssembler.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) and
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) = 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) or
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromOctLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActSetOpA(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := 0;
  FAddrMode := ADM_ACC;
  Result.Buf := '';
end;

function TAssembler.ActSetOpAbs(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FAddrMode := ADM_ABS;
  Result.Buf := '';
end;

function TAssembler.ActSetOpAbsX(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_ABSX;
  Result.Buf := '';
end;

function TAssembler.ActSetOpAbsY(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_ABSY;
  Result.Buf := '';
end;

function TAssembler.ActSetOpImmed(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FAddrMode := ADM_LIT;
  Result.Buf := '';
end;

function TAssembler.ActSetOpImpl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := 0;
  FAddrMode := ADM_IMPL;
  Result.Buf := '';
end;

function TAssembler.ActSetOpIndX(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  FAddrMode := ADM_XIND;
  Result.Buf := '';
end;

function TAssembler.ActSetOpIndY(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  FAddrMode := ADM_INDY;
  Result.Buf := '';
end;

function TAssembler.ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_IND;
  Result.Buf := '';
end;

function TAssembler.ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result.Buf := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function TAssembler.ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Chr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('yyyy-mm-dd',FAssemblyStart);
end;

function TAssembler.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Format('%X',[StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf)]);
end;

function TAssembler.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
var fmt: string;
    digits: integer;
begin
  digits := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if (digits < 1) or (digits > 4) then
      Monitor(ltError,'Range for number of hex digits is 1 to 4');
  fmt := Format('%%%d.%dX',[digits,digits]);
  Result.Buf := Format(fmt,[StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf)]);
end;

function TAssembler.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for LEFT() cannot use a negative number');
  Result.Buf := LeftStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := LowerCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
    start: integer;
begin
  start := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  len   := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if start < 0 then
    Monitor(ltError,'Start for MID() cannot use a negative number');
  if len < 0 then
    Monitor(ltError,'Length for MID() cannot use a negative number');
  Result.Buf := Copy(_parser.ParserStack[_parser.ParserSP-6].Buf,start,len);
end;

function TAssembler.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for RIGHT() cannot use a negative number');
  Result.Buf := RightStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler.ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('hh:nn:ss',FAssemblyStart);
end;

function TAssembler.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(FOrg);
end;

function TAssembler.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,FOrg);
end;

procedure TAssembler.Assemble;
begin
  FilesOpen;
  FAssemblyStart := Now;
  Monitor(ltVerbose,'Assembly started %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyStart)]);
  InitStart;
  try
    AssemblePass(1);
    AssemblePass(2);
    WriteMapFile;
    if FilenameLst <> '' then
      FListing.SaveToFile(FilenameLst);
    if FBytesTotal > 0 then
      begin
        FOutput.SaveHex(FilenameHex);
        FOutput.SaveObject(FilenameObj);
      end;
  finally
    Monitor(ltInfo,'%d lines assembled, %d bytes generated',[FLineCount, FBytesTotal]);
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
  FBytesTotal := 0;
  FLineCount := 0;
  FList := True;
  FListNext := True;
  FOrg := $0200;
  FOutput.Clear;
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
    prefix: string;
    lineinfo: string;
    line,column: integer;
    msg: string;
begin
  if LogType > FLogLevel then
    Exit;
  // Sort out prefix
  case LogType of
    ltInternal:    prefix := 'INTERNAL';
    ltError:       prefix := '   ERROR';
    ltWarning:     prefix := ' WARNING';
    ltInfo,
    ltVerbose,
    ltWarAndPeace: prefix := '    INFO';
    ltDebug:       prefix := '   DEBUG';
    otherwise      prefix := '????????';
  end;
  line := 0;
  column := 0;
  if FFileStack.Count > 0 then
    line := FFileStack.InputLine;
  if LogType <= ltWarning then
    column := FInputColumnSave;
  lineinfo := '';
  if line > 0 then
    begin
      if column > 0 then
        lineinfo := Format('[%d:%d] ',[line,column])
      else
        lineinfo := Format('[%d] ',[line]);
    end;
  msg := Format('%s: %s%s',[prefix,lineinfo,message]);
  if Assigned(FStreamLog)  then
    begin
      s := msg + #13 + #10;
      FStreamLog.Write(s[1],Length(s));
    end;
  inherited Monitor(LogType,msg);
end;

procedure TAssembler.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TAssembler.OutputListingLine(const _asmline: string);
const MAX_BYTES_PER_ROW = 4;
      HEX_WIDTH = MAX_BYTES_PER_ROW * 3 + 7;
var byteindex: integer;
    bytesleft: integer;
    bcount:    integer;
    i:         integer;
    s:         string;
begin
  if (FPass = 1) or (FFileStack.Count > 1) or (not FList) or (not FIfStack.Allowed) then
    Exit;
  if FBytesFromLine = 0 then
    FListing.Add(StringOfChar(' ',HEX_WIDTH) + ExpandTabs(_asmline,FTabSize))
  else
    begin
      byteindex := 0;
      bytesleft := FBytesFromLine;
      bcount := bytesleft;
      if bcount > MAX_BYTES_PER_ROW then
        bcount := MAX_BYTES_PER_ROW;
      bytesleft := bytesleft - bcount;
      s := Format('%4.4X:',[byteindex+FOrg]);
      for i := 0 to bcount-1 do
        s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
      s := PadRight(s,HEX_WIDTH);
      s := s + ExpandTabs(_asmline,FTabSize);
      FListing.Add(s);
      byteindex := byteindex + bcount;
      // Do any overspill lines if required
      while bytesleft > 0 do
        begin
          bcount := bytesleft;
          if bcount > MAX_BYTES_PER_ROW then
            bcount := MAX_BYTES_PER_ROW;
          bytesleft := bytesleft - bcount;
          s := Format('%4.4X:',[byteindex+FOrg]);
          for i := 0 to bcount-1 do
            s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
          FListing.Add(s);
          byteindex := byteindex + bcount;
        end;
    end;
end;

procedure TAssembler.ProcessFile(const _fn: string);
var asmline: string;
    incl:    string;
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
          begin
            ProcessLine(asmline);
            // Add to listing
            OutputListingLine(asmline);
            // Bump org
            FOrg := FOrg + FBytesFromLine;
            FBytesTotal := FBytesTotal + FBytesFromLine;
            // Check to see if listing flag has changed
            FList := FListNext;
            // Check to see if we created a new include file to use
            if FIncludeNext <> '' then
              begin
                incl := FIncludeNext;
                FIncludeNext := '';
                ProcessFile(incl); // Recursive!!!
              end;
          end
        else
          OutputListingLine(asmline);
      end;
  finally
    FFileStack.Pop();
  end;
end;

procedure TAssembler.ProcessLine(const _line: string);
var strm: TStringStream;
begin
  FAddrMode := ADM_IMPL;
  FOpCode := OPC_NOP;
  FBytesFromLine := 0;
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
  RegisterProc('ActCompEQ',         @ActCompEQ);
  RegisterProc('ActCompGE',         @ActCompGE);
  RegisterProc('ActCompGT',         @ActCompGT);
  RegisterProc('ActCompLE',         @ActCompLE);
  RegisterProc('ActCompLT',         @ActCompLT);
  RegisterProc('ActCompNE',         @ActCompNE);
  RegisterProc('ActDecLiteral',     @ActDecLiteral);
  RegisterProc('ActDirDB',          @ActDirDB);
  RegisterProc('ActDirDefine',      @ActDirDefine);
  RegisterProc('ActDirDefineExpr',  @ActDirDefineExpr);
  RegisterProc('ActDirDS',          @ActDirDS);
  RegisterProc('ActDirDSZ',         @ActDirDSZ);
  RegisterProc('ActDirElse',        @ActDirElse);
  RegisterProc('ActDirEndif',       @ActDirEndif);
  RegisterProc('ActDirError',       @ActDirError);
  RegisterProc('ActDirIf',          @ActDirIf);
  RegisterProc('ActDirIfdef',       @ActDirIfdef);
  RegisterProc('ActDirIfndef',      @ActDirIfndef);
  RegisterProc('ActDirInclude',     @ActDirInclude);
  RegisterProc('ActDirList',        @ActDirList);
  RegisterProc('ActDirMessage',     @ActDirMessage);
  RegisterProc('ActDirNolist',      @ActDirNolist);
  RegisterProc('ActDirOrg',         @ActDirOrg);
  RegisterProc('ActDirWarning',     @ActDirWarning);
  RegisterProc('ActExprAdd',        @ActExprAdd);
  RegisterProc('ActExprAnd',        @ActExprAnd);
  RegisterProc('ActExprBracket',    @ActExprBracket);
  RegisterProc('ActExprDiv',        @ActExprDiv);
  RegisterProc('ActExprFalse',      @ActExprFalse);
  RegisterProc('ActExprList',       @ActExprList);
  RegisterProc('ActExprMinus',      @ActExprMinus);
  RegisterProc('ActExprMod',        @ActExprMod);
  RegisterProc('ActExprMul',        @ActExprMul);
  RegisterProc('ActExprNot',        @ActExprNot);
  RegisterProc('ActExprOr',         @ActExprOr);
  RegisterProc('ActExprShl',        @ActExprShl);
  RegisterProc('ActExprShr',        @ActExprShr);
  RegisterProc('ActExprSub',        @ActExprSub);
  RegisterProc('ActExprTrue',       @ActExprTrue);
  RegisterProc('ActExprXor',        @ActExprXor);
  RegisterProc('ActFuncAsc',        @ActFuncAsc);
  RegisterProc('ActFuncHigh',       @ActFuncHigh);
  RegisterProc('ActFuncIif',        @ActFuncIif);
  RegisterProc('ActFuncLow',        @ActFuncLow);
  RegisterProc('ActFuncPos',        @ActFuncPos);
  RegisterProc('ActFuncValue',      @ActFuncValue);
  RegisterProc('ActHexLiteral',     @ActHexLiteral);
  RegisterProc('ActIgnore',         @ActIgnore);
  RegisterProc('ActInstruction',    @ActInstruction);
  RegisterProc('ActLabel',          @ActLabel);
  RegisterProc('ActLogAnd',         @ActLogAnd);
  RegisterProc('ActLogNot',         @ActLogNot);
  RegisterProc('ActLogOr',          @ActLogOr);
  RegisterProc('ActOctLiteral',     @ActOctLiteral);
  RegisterProc('ActSetOpA',         @ActSetOpA);
  RegisterProc('ActSetOpAbs',       @ActSetOpAbs);
  RegisterProc('ActSetOpAbsX',      @ActSetOpAbsX);
  RegisterProc('ActSetOpAbsY',      @ActSetOpAbsY);
  RegisterProc('ActSetOpImmed',     @ActSetOpImmed);
  RegisterProc('ActSetOpImpl',      @ActSetOpImpl);
  RegisterProc('ActSetOpIndX',      @ActSetOpIndX);
  RegisterProc('ActSetOpIndY',      @ActSetOpIndY);
  RegisterProc('ActSetOpInd',       @ActSetOpInd);
  RegisterProc('ActStrBuild',       @ActStrBuild);
  RegisterProc('ActStrCat',         @ActStrCat);
  RegisterProc('ActStrChr',         @ActStrChr);
  RegisterProc('ActStrDate',        @ActStrDate);
  RegisterProc('ActStrHex1',        @ActStrHex1);
  RegisterProc('ActStrHex2',        @ActStrHex2);
  RegisterProc('ActStringConstant', @ActStringConstant);
  RegisterProc('ActStrLeft',        @ActStrLeft);
  RegisterProc('ActStrLower',       @ActStrLower);
  RegisterProc('ActStrMid',         @ActStrMid);
  RegisterProc('ActStrRight',       @ActStrRight);
  RegisterProc('ActStrString',      @ActStrString);
  RegisterProc('ActStrTime',        @ActStrTime);
  RegisterProc('ActStrUpper',       @ActStrUpper);
  RegisterProc('ActValueOrg',       @ActValueOrg);
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

procedure TAssembler.WriteMapFile;
var sl: TStringList;
begin
  if FilenameMap = '' then
    Exit;
  sl := TStringList.Create;
  try
    FSymbols.SortByName;
    sl.Add('SYMBOLS BY NAME');
    sl.Add('');
    FSymbols.Dump(sl);
    FSymbols.SortByAddr;
    sl.Add('');
    sl.Add('');
    sl.Add('');
    sl.Add('SYMBOLS BY VALUE');
    sl.Add('');
    FSymbols.Dump(sl);
    sl.SaveToFile(FilenameMap);
  finally
    sl.Free;
  end;
end;

end.

