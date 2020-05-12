program asm65;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fileinfo, usymbol, uutility, uassembler,
  deployment_parser_module, deployment_parser_types, ufilestack, uexpression,
  uoutput, uifstack, umacro;

const
  DEFAULT_TAB_VALUE = 4;
  SHORT_OPTIONS = 'e::hI:l::m::o:t:v:Vx::';
  LONG_OPTIONS: array [1..10] of string =
    (
      'errorlog::',
      'help',
      'include:',
      'listing::',
      'map::',
      'object:',
      'tab:',
      'verbose:',
      'version',
      'hex::'
    );

type

  { TAsm6502 }

  TAsm6502 = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ProcessFilename(var filename: string; shortopt: char; longopt: string; defaultext: string);
    procedure WriteHelp; virtual;
    procedure WriteTitle;
    procedure WriteVersion;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Monitor({%H-}Parser: TLCGParser; {%H-}LogType: TLCGLogType; const Message: string);
  end;

{ TAsm6502 }

procedure TAsm6502.DoRun;
var
  ErrorMsg:   String;
  asm65:      TAssembler;
  taboption:  string;
  tabvalue:   integer;
  verboption: string;
  verblevel:  integer;
  nonoptions: TStringList;
  filename:   string;
begin
  WriteTitle;

  // quick check parameters
  ErrorMsg:=CheckOptions(SHORT_OPTIONS, LONG_OPTIONS);
  if ErrorMsg<>'' then begin
    WriteLn(ErrorMsg);
    Terminate;
    Exit;
  end;

  // Check filename is specified
  nonoptions := TStringList.Create;
  try
    GetNonOptions(SHORT_OPTIONS,LONG_OPTIONS,nonoptions);
    if nonoptions.Count < 1 then
      begin
        WriteLn('Filename not specified');
        Terminate;
        Exit;
      end;
    if nonoptions.Count > 1 then
      begin
        WriteLn('More than one filename specified');
        Terminate;
        Exit;
      end;
    filename := ExpandFilename(nonoptions[0]);
  finally
    nonoptions.Free;
  end;

  // parse parameters
  if HasOption('h', 'help') or (ParamCount < 1) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
  if HasOption('t', 'tab') then
    begin
      taboption := GetOptionValue('t', 'tab');
      if (Length(taboption) < 1) or
         (Length(taboption) > 2) then
        begin
          WriteLn('Illegal or missing option value for -t / --tab, must be 1 to 99');
          Terminate;
          Exit;
        end;
      try
        tabvalue := StrToInt(taboption);
      except
        WriteLn('Illegal or missing option value for -t / --tab, must be 1 to 99');
        Terminate;
        Exit;
      end;
    end
  else
    tabvalue := DEFAULT_TAB_VALUE;

  if HasOption('V', 'version') then
    begin
      WriteVersion;
      Terminate;
      Exit;
    end;

  if HasOption('v', 'verbose') then
    begin
      verboption := GetOptionValue('v', 'verbose');
      if (Length(verboption) <> 1) or
         (verboption < '0') or
         (verboption > '3') then
        begin
          WriteLn('Illegal option value for -v / --verbose, must be 0, 1, 2 or 3');
          Terminate;
          Exit;
        end;
      verblevel := StrToInt(verboption);
    end
  else
    verblevel := 0;

  { add your program here }


  // Create the assembler and run it

  asm65 := TAssembler.Create;
  try
    // Set up the initial parameters
    asm65.FilenameSrc := filename; // Has to go first!
    asm65.OnMonitor   := @Monitor;
    asm65.TabSize     := tabvalue;
    case verblevel of
      0: asm65.LogLevel := ltInfo;
      1: asm65.LogLevel := ltVerbose;
      2: asm65.LogLevel := ltWarAndPeace;
      3: asm65.LogLevel := ltDebug;
    end;
    ProcessFilename(asm65.FilenameHex,'h','hex',     '.hex');
    ProcessFilename(asm65.FilenameLst,'l','listing', '.lst');
    ProcessFilename(asm65.FilenameLog,'e','errorlog','.log');
    ProcessFilename(asm65.FilenameMap,'m','map',     '.map');
    ProcessFilename(asm65.FilenameObj,'o','object',  '.obj');
    try
      asm65.Assemble;
    except
      on E: LCGErrorException do ;    // Nothing, it's dealt with already
      on E: LCGInternalException do ; // Ditto
      on E: Exception do Monitor(asm65,ltInternal,'UNHANDLED EXCEPTION: ' + E.Message);
    end;
  finally
    asm65.Free;
  end;


  // stop program loop
  Terminate;
end;

constructor TAsm6502.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TAsm6502.Destroy;
begin
  inherited Destroy;
end;

procedure TAsm6502.Monitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
begin
  WriteLn(Message);
end;

procedure TAsm6502.ProcessFilename(var filename: string; shortopt: char; longopt: string; defaultext: string);
var newname: string;
begin
  if HasOption(shortopt,longopt) then
    begin
      newname := GetOptionValue(shortopt,longopt);
      if ExtractFileExt(newname) = '' then
        newname := newname + defaultext;
      filename := ExpandFilename(newname);
    end;
end;

procedure TAsm6502.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: asm6502 filename <options>');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('    -e <en> --errorlog=<en> Set error log to <en>');
  WriteLn('    -h      --help          Display this message');
  WriteLn('    -I <id> --include=<id>  Set the include directory to <id>');
  WriteLn('    -l <ln> --listing=<ln>  Set the listing name to <ln>');
  WriteLn('    -m <mn> --map=<mn>      Set the map filename to <mn>');
  WriteLn('    -o <on> --object=<on>   Set the object name to <on>');
  WriteLn('    -t <n>  --tab=<n>       Tab size for input file (default 4)');
  WriteLn('    -v <n>  --verbose=<n>   Verbose output while assembling');
  WriteLn('    -V      --version       Display version and other status info');
  WriteLn('    -x <hn> --hex=<hn>      Set the hex filename to <hn>');
  WriteLn('');
  WriteLn('<en>/<ln>/<mn>/<on>/<hn> default to the filename with ext changed to');
  WriteLn('.log/.hex/.lst/.obj/.hex respectively. Not specifying <en>, <ln>, <mn>');
  WriteLn('or <hn> will stop that output.');
  WriteLn('');
  WriteLn('verbose <n> options:');
  WriteLn('    0 Normal output levels (the default)');
  WriteLn('    1 Verbose output');
  WriteLn('    2 "War and Peace", lots more output');
  WriteLn('    3 Debug level output');
  WriteLn('');
  WriteLn('The include file directory <id> can contain names delimited by ;');
  WriteLn('for example --include=source/tables;source/help;/users/me/includes');
  WriteLn('');
end;




// Write the title when the program starts up

procedure TAsm6502.WriteTitle;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    WriteLn('');
    WriteLn('6502 Macro Assembler V' + FileVerInfo.VersionStrings.Values['ProductVersion']);
    WriteLn('Copyright (C)2020 Duncan Munro');
    WriteLn('');
  finally
    FileVerInfo.Free;
  end;
end;


// Write the version information

procedure TAsm6502.WriteVersion;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    WriteLn('VERSION INFORMATION');
    WriteLn('');
    WriteLn('Software version: ' + FileVerInfo.VersionStrings.Values['FileVersion']);
    WriteLn('Compiler version: ' + {$I %FPCVERSION%});
    WriteLn('Compiled:         ' + {$I %DATE%} + ' ' + {$I %TIME%});
    WriteLn('Target OS:        ' + {$I %FPCTARGETOS%});
    WriteLn('Target CPU:       ' + {$I %FPCTARGETCPU%});
    WriteLn;
  finally
    FileVerInfo.Free;
  end;
end;



var
  Application: TAsm6502;

{$R *.res}

begin
  Application:=TAsm6502.Create(nil);
  Application.Title:='6502 Macro Assembler';
  Application.Run;
  Application.Free;
end.

