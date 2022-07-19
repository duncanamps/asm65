program asm65;

{
    ASM65 - Cross Assembler for 6502 processor
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fileinfo, usymbol, uutility, uassembler,
  ufilestack, uexpression, uoutput, uifstack, umacro,
  deployment_parser_types_12, deployment_parser_module_12, udebuglist;

const
  DEFAULT_TAB_VALUE = 4;
  SHORT_OPTIONS = 'b::d:e::hI:l::m::o:t:v:Vx::';
  LONG_OPTIONS: array [1..12] of string =
    (
      'debug::',
      'define:',
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
    procedure ProcessFilename(basename: string; var filename: string; shortopt: char; longopt: string; defaultext: string);
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
  basename:   string;  // Filename without .ext on the end
begin
  WriteTitle;

  // quick check parameters
  ErrorMsg:=CheckOptions(SHORT_OPTIONS, LONG_OPTIONS);
  if ErrorMsg<>'' then begin
    WriteLn(ErrorMsg);
    Terminate;
    Exit;
  end;

  // Help if needed...
  if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;


  // parse parameters
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

  // Check filename is specified

  nonoptions := TStringList.Create;
  try
    GetNonOptions(SHORT_OPTIONS,LONG_OPTIONS,nonoptions);
    if nonoptions.Count < 1 then
      begin
        WriteLn('Use  asm65 --help  for command line options');
        WriteLn('');
        WriteLn('This program is free software: you can redistribute it and/or modify');
        WriteLn('it under the terms of the GNU General Public License as published by');
        WriteLn('the Free Software Foundation, either version 3 of the License, or');
        WriteLn('(at your option) any later version.');
        WriteLn;
        WriteLn('This program is distributed in the hope that it will be useful,');
        WriteLn('but WITHOUT ANY WARRANTY; without even the implied warranty of');
        WriteLn('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
        WriteLn('GNU General Public License for more details.');
        WriteLn;
        WriteLn('You should have received a copy of the GNU General Public License');
        WriteLn('along with this program.  If not, see <https://www.gnu.org/licenses/>.');
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
    basename := StringReplace(asm65.FilenameSrc,ExtractFileExt(asm65.FilenameSrc),'',[rfReplaceAll]);
    ProcessFilename(basename,asm65.FilenameDbg,'b','debug',   '.d65');
    ProcessFilename(basename,asm65.FilenameHex,'x','hex',     '.hex');
    ProcessFilename(basename,asm65.FilenameLst,'l','listing', '.lst');
    ProcessFilename(basename,asm65.FilenameLog,'e','errorlog','.log');
    ProcessFilename(basename,asm65.FilenameMap,'m','map',     '.map');
    ProcessFilename(basename,asm65.FilenameObj,'o','object',  '.o65');
    CmdOptionToList(Self,'d','define', asm65.CmdDefines);
    CmdOptionToList(Self,'I','include',asm65.CmdIncludes,true);
    AugmentIncludes(GetCurrentDir,asm65.CmdIncludes);
    AugmentIncludes(ExtractFilePath(filename),asm65.CmdIncludes);
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

procedure TAsm6502.ProcessFilename(basename: string; var filename: string; shortopt: char; longopt: string; defaultext: string);
var newname: string;
begin
  if HasOption(shortopt,longopt) then
    begin
      newname := GetOptionValue(shortopt,longopt);
      if newname = '' then
          newname := basename;
      if ExtractFileName(newname) = newname then // No path specified
        newname := ExtractFilePath(basename) + newname;
      if ExtractFileExt(newname) = '' then
        newname := newname + defaultext;
      filename := ExpandFilename(newname);
    end;
end;

procedure TAsm6502.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: asm65 filename <options>');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('    -b <bn> --debug=<bn>    Set the debug name to <bn>');
  WriteLn('    -d <id> --define=<id>   Define one or more symbols');
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
  WriteLn('<bn>/<en>/<hn>/<ln>/<mn>/<on> default to the filename with ext changed to');
  WriteLn('.d65/.log/.hex/.lst/.map/.o65 respectively. Not specifying <bn>, <en>,');
  WriteLn('<hn>, <ln>, <mn> or <on> will stop that output.');
  WriteLn('');
  WriteLn('verbose <n> options:');
  WriteLn('    0 Normal output levels (the default)');
  WriteLn('    1 Verbose output');
  WriteLn('    2 "War and Peace", lots more output');
  WriteLn('    3 Debug level output');
  WriteLn('');
  WriteLn('The include file directory and define list <id> can contain names or');
  WriteLn('symbols delimited by ; for example:');
  WriteLn('    --define=DEBUG;TAB_SIZE=4;CODE_NAME="Project ASM"');
  WriteLn('    --include=source/tables;source/help;/users/me/includes');
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
    WriteLn('6502 Macro Cross Assembler V' + FileVerInfo.VersionStrings.Values['ProductVersion']);
    WriteLn('Copyright (C)2020-' + FormatDateTime('YYYY',Now) + ' Duncan Munro');
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

