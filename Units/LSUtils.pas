(*
  LazSolutions, Utils unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSUtils;

{$I lazsolutions.inc}

interface

uses
{$IFDEF UNIX}
  MacUUID,
{$ENDIF}
{$IFDEF MSWINDOWS}
  DynLibs, Windows, JwaTlHelp32,
{$ENDIF}
{$IFDEF LSLCL}
 {$IFDEF LSGUI}
  Forms, Controls, StdCtrls, Graphics, LCLIntf,
 {$ENDIF}
  LCLProc,
{$ENDIF}
  LSConsts, LSMessages, LSExceptionUtils, SysUtils, Classes, Process, DateUtils,
  FPJSON, JSONParser, Math, Base64;

type
  TLSChars = set of Char;

  PLSChars = ^TLSChars;

  THDDInfo = record
    FileHandle: Cardinal;
    InfoAvailable: Boolean;
    ProductRevision: string;
    ProductId: string;
    SerialNumber: string;
    VendorId: string;
  end;

{ Get current path, e.g.: /myproject/, or C:\myproject\. }
function LSCurrentPath: string;
{ Load a file to string. }
function LSLoadFile(const AFileName: TFileName): string;
{ Save a string to file. }
procedure LSSaveFile(const AFileName: TFileName; const AString: string);
{ Execute process and get large output. }
function LSExecProcess(const ACommandLine: string): string;
{ Execute process and get short output. }
function LSExecProcessWithShortOut(const ACommandLine: string): string;
{ Get current user name. }
function LSCurrentUserName: string;
{ Y/N to boolean. }
function LSYNToBool(const S: Char): Boolean;
{ Boolean to Y/N. }
function LSBoolToYN(const B: Boolean): Char;
{$IFDEF UNIX}
{ Get the name of the current Linux distro. }
function LSGetLinuxDistro: string;
{$ENDIF}
{ Get MAC address. (see: http://en.wikipedia.org/wiki/MAC_address) }
function LSGetMACAddress: string;
{ Get process ID by process name. }
function LSGetProcessID(const AProcessName: string;
  const AIgnoreCurrent: Boolean = False): Integer;
{ List active process. }
procedure LSListProcess(const AProcess: TStringList;
  const AShowPID: Boolean = False; const ASorted: Boolean = True;
  const AIgnoreCurrent: Boolean = False);
{ Get if process is running. }
function LSProcessIsRunning(const AProcessName: string;
  const AIgnoreCurrent: Boolean = False): Boolean;
{ Kill process by name. }
function LSKillProcess(const AProcessName: string): Boolean;
{ Add a atom string. }
procedure LSGlobalAddAtom(const AAtomName: string);
{ Delete a atom string. }
procedure LSGlobalDeleteAtom(const AAtomName: string);
{ Find a atom string. }
function LSGlobalFindAtom(const AAtomName: string): Boolean;
{ Remove Cid from HTML.
  E.g.: From <img src="cid:MyImage.png"> To <img src="MyImage.png"> }
function LSRemoveCid(const AHTML: string): string;
{ Stream to base64 string (see: http://en.wikipedia.org/wiki/Base64) }
function LSStreamToBase64Str(AStream: TStream): string;
{ Base64 string to stream. }
procedure LSBase64StrToStream(const ABase64Str: string; AStream: TStream);
{ File to base64 string. }
function LSFileToBase64Str(const AFileName: TFileName): string;
{ Base64 string to file. }
procedure LSBase64StrToFile(const ABase64Str: string;
  const AFileName: TFileName);
{ Get physical serial of current disk. }
function LSGetPhysicalSerialOfCurrentDisk
{$IFDEF MSWINDOWS}(out AHDDInfo: THDDInfo){$ENDIF}: string;
{$IFDEF MSWINDOWS}
function LSGetPhysicalSerialOfCurrentDisk: string;
{$ENDIF}
{ Convert string to hex. }
function LSStrToHex(const AStr: string): string;
{ Convert hex to string. }
function LSHexToStr(const AHex: string): string;
{ Generate a UUID as string.
  (see: http://en.wikipedia.org/wiki/Universally_unique_identifier) }
function LSUUID: string;
function LSUUID(
  const AUseSeparators: Boolean): string;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Delete current executable. }
procedure LSDeleteCurrentExecutable(
{$IFDEF MSWINDOWS}const AAddBATCommands: string = ''{$ENDIF});
{ Change the date time of a file. }
function LSChangeFileDateTime(const AFileName: TFileName;
  const AFileDateTime: TDateTime): Boolean;
{ Get the position of the last occurence of a substring in string. }
function LSLastPos(const ASubString, AString: string): Integer;
{ Delete all LineEnding(sLineBreak) from a string. }
function LSDeleteLineBreaks(const AString: string;
  const ASubstitute: Char = #32): string;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Replace all char in a string. }
function LSReplaceChar(const AString: string; const AOldChar,
  ANewChar: Char): string;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Replace all chars in a string. }
function LSReplaceChar(const AString: string; const AOldChars: TLSChars;
  const ANewChar: Char): string;{$IFDEF LSINLINE}inline;{$ENDIF}
{$IFDEF LSGUI}
{ Shorten the pathname with an ellipses "...". }
function LSShortenPathName(const ARect: TRect; ACanvas: TCanvas;
  const AFileName: TFileName): TFileName;
function LSShortenPathName(const ARect: TRect; const AFont: TFont;
  const AFileName: TFileName): TFileName;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Show a hint on top of a control. }
procedure LSShowHint(const AControl: TWinControl; const AHint: string);
{ Close a hint on top of a control. }
procedure LSCloseHint(const AControl: TWinControl);
{$ENDIF}
{ Return the number of milliseconds since 1970-01-01.
  (see: http://www.w3schools.com/jsref/jsref_gettime.asp) }
function LSGetTime: Int64;
{ Remove all HTML code from a string. }
function LSStripHTMLMarkup(const ASource: string): string;
{$IFDEF LSLCL}
{ Get part of a string from a stream. }
function LSGetPartFromStream(const ASkipTo, AEnd: string; var ASource: TStream;
  const AIgnoreCase: Boolean = True; const AUpdateSource: Boolean = False): string;
{$ENDIF}
{ Load string to stream. }
procedure LSStrToStream(const AString: string; var AStream: TStream);
{ Load stream to string. }
function LSStreamToStr(const AStream: TStream): string;
{$IFDEF LSGUI}
{ Find item in a ListBox. }
function LSFindItemInListBox(var AListBox: TCustomListBox; const AText: string;
  const ACaseSensitive: Boolean = False; const AFindNext: Boolean = True): Integer;
{$ENDIF}
{ JSON array to strings. (use array: ["Item1", "Item2",.."ItemN"]) }
procedure LSJSONArrayToStrings(const AJSON: TJSONStringType; AStrings: TStrings);
{ Strings to JSON array. (to array: ["Item1", "Item2",.."ItemN"]) }
function LSStringsToJSONArray(AStrings: TStrings): TJSONStringType;
{ JSON string to JSON object and to strings. }
procedure LSJSONToJSONObjectStrings(var AJSONObject: TJSONObject;
  const AJSON: TJSONStringType; AStrings: TStrings);
{ Round an extended value.
  (see: http://www.freepascal.org/docs-html/rtl/math/tfpuroundingmode.html) }
function LSRoundAnExtended(const AValue: Extended;
  const ARoundingMode: TFPURoundingMode = rmNearest;
  const ARoundingDigits: Integer = -2): Extended;
{ Size to string. }
function LSSizeToString(const ASize: Extended): string;

implementation

const
  CSince19700101 =
    ((1970 { Year } - 1900) * 365.25) + ((1 { Month } - 1) * 30) + 1 { Day };
{$IFDEF UNIX}
  CLSAtomTableFileName = '/tmp/.lsatomtable';

var
  _LSAtomList: TStringList = nil;

function LSAtomList: TStringList;
begin
  if not Assigned(_LSAtomList) then
    _LSAtomList := TStringList.Create;
  Result := _LSAtomList;
end;

function LSAtomTableExists: Boolean;
begin
  Result := FileExists(CLSAtomTableFileName);
end;
{$ENDIF}

function LSCurrentPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function LSLoadFile(const AFileName: TFileName): string;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';
      Free;
      raise;
    end;
    Free;
  end;
end;

procedure LSSaveFile(const AFileName: TFileName; const AString: string);
begin
  with TFileStream.Create(AFileName, fmCreate) do
    try
      Write(Pointer(AString)^, Length(AString));
    finally
      Free;
    end;
end;

function LSExecProcess(const ACommandLine: string): string;
const
  READ_BYTES = 2048;
var
  VStrTemp: TStringList;
  VMemoryStream: TMemoryStream;
  VProcess: TProcess;
  I64: LongInt;
  VBytesRead: LongInt;
begin
  VMemoryStream := TMemoryStream.Create;
  VProcess := TProcess.Create(nil);
  VStrTemp := TStringList.Create;
  try
    VBytesRead := 0;
{$WARN SYMBOL_DEPRECATED OFF}
    VProcess.CommandLine := ACommandLine;
{$WARN SYMBOL_DEPRECATED ON}
    VProcess.Options := [poUsePipes, poNoConsole];
    VProcess.Execute;
    while VProcess.Running do
    begin
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64)
      else
        Sleep(100);
    end;
    repeat
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64);
    until I64 <= 0;
    VMemoryStream.SetSize(VBytesRead);
    VStrTemp.LoadFromStream(VMemoryStream);
    Result := Trim(VStrTemp.Text);
  finally
    VStrTemp.Free;
    VProcess.Free;
    VMemoryStream.Free;
  end;
end;

function LSExecProcessWithShortOut(const ACommandLine: string): string;
var
  VProcess: TProcess;
  VStrTemp: TStringList;
begin
  VProcess := TProcess.Create(nil);
  VStrTemp := TStringList.Create;
  try
    VProcess.Options := [poStderrToOutPut, poNoConsole, poUsePipes];
{$WARN SYMBOL_DEPRECATED OFF}
    VProcess.CommandLine := ACommandLine;
{$WARN SYMBOL_DEPRECATED ON}
    VProcess.Execute;
    VStrTemp.LoadFromStream(VProcess.Output);
    Result := Trim(VStrTemp.Text);
  finally
    VProcess.Free;
    VStrTemp.Free;
  end;
end;

function LSCurrentUserName: string;
begin
  Result := GetEnvironmentVariable('USERNAME');
end;

function LSYNToBool(const S: Char): Boolean;
begin
  Result := (S = #89) or (S = #121);
end;

function LSBoolToYN(const B: Boolean): Char;
begin
  Result := #110;
  if B then
    Result := #121;
end;

{$IFDEF UNIX}
function LSGetLinuxDistro: string;
var
  I, J: Integer;
  VDistros, VReleaseFiles: TStringList;
begin
  Result := SLSUtilsGetLinuxDistroError;
  VDistros := TStringList.Create;
  VReleaseFiles := TStringList.Create;
  try
    VDistros.Text := CLSUtilsDistros;
    VDistros.NameValueSeparator := ':';
    for I := 0 to Pred(VDistros.Count) do
    begin
      VReleaseFiles.Clear;
      ExtractStrings([','], [' '], PChar(VDistros.ValueFromIndex[I]), VReleaseFiles);
      for J := 0 to Pred(VReleaseFiles.Count) do
        if FileExists(VReleaseFiles.ValueFromIndex[J]) then
        begin
          Result := VDistros.Names[I];
          Break;
        end;
    end;
  finally
    VDistros.Free;
    VReleaseFiles.Free;
  end;
end;
{$ENDIF}

function LSGetMACAddress: string;
{$IFDEF MSWINDOWS}
type
  TCreateGUIDFunction = function(AGUID: PGUID): LongInt; stdcall;
{$ENDIF}
var
  VGUID1, VGUID2: TGUID;
{$IFDEF MSWINDOWS}
  VLibHandle: TLibHandle;
  VCreateGUIDFunction: TCreateGUIDFunction;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  VLibHandle := LoadLibrary('rpcrt4.dll');
  try
    if VLibHandle <> NilHandle then
    begin
      VCreateGUIDFunction := TCreateGUIDFunction(GetProcedureAddress(VLibHandle,
        'UuidCreateSequential'));
      if Assigned(VCreateGUIDFunction) then
{$ENDIF}
        if (
{$IFDEF UNIX}
          CreateGUID
{$ENDIF}
{$IFDEF MSWINDOWS}
          VCreateGUIDFunction
{$ENDIF}
          ({$IFDEF MSWINDOWS}@{$ENDIF}VGUID1) = 0) and (
{$IFDEF UNIX}
          CreateGUID
{$ENDIF}
{$IFDEF MSWINDOWS}
          VCreateGUIDFunction
{$ENDIF}
          ({$IFDEF MSWINDOWS}@{$ENDIF}VGUID2) = 0) and
          (VGUID1.D4[2] = VGUID2.D4[2]) and (VGUID1.D4[3] = VGUID2.D4[3]) and
          (VGUID1.D4[4] = VGUID2.D4[4]) and (VGUID1.D4[5] = VGUID2.D4[5]) and
          (VGUID1.D4[6] = VGUID2.D4[6]) and (VGUID1.D4[7] = VGUID2.D4[7]) then
            Result := Format(CLSUtilsFormatMACMask, [VGUID1.D4[2], VGUID1.D4[3],
                        VGUID1.D4[4], VGUID1.D4[5], VGUID1.D4[6], VGUID1.D4[7]]);
{$IFDEF MSWINDOWS}
    end;
  finally
    UnloadLibrary(VLibHandle);
  end;
{$ENDIF}
end;

function LSGetProcessID(const AProcessName: string;
  const AIgnoreCurrent: Boolean): Integer;
var
  VProcess: TStringList;
begin
  VProcess := TStringList.Create;
  try
    VProcess.NameValueSeparator := CLSUtilsProcessNameValueSeparator;
    LSListProcess(VProcess, True, False, AIgnoreCurrent);
    Result := StrToIntDef(VProcess.Values[AProcessName], -1);
  finally
    VProcess.Free;
  end;
end;

procedure LSListProcess(const AProcess: TStringList; const AShowPID: Boolean;
  const ASorted: Boolean; const AIgnoreCurrent: Boolean);
var
{$IFDEF UNIX}
  I, J: Integer;
  VOldNameValueSeparator: Char;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
{$ENDIF}
begin
{$IFDEF UNIX}
  VOldNameValueSeparator := AProcess.NameValueSeparator;
  AProcess.NameValueSeparator := CLSUtilsProcessNameValueSeparator;
  AProcess.Text := LSExecProcess('sh -c "ps -A | awk ''{ print $4 "' +
    CLSUtilsProcessNameValueSeparator + '" $1 }''"');
  J := AProcess.Count;
  for I := AProcess.Count downto 1 do
  begin
    if (I > J - 3) or (AIgnoreCurrent and (StrToIntDef(AProcess.ValueFromIndex[
      I - 1], -1) = Integer(GetProcessID))) then
    begin
      AProcess.Delete(I - 1);
      Continue;
    end;
    if not AShowPID then
      AProcess.Strings[I - 1] := AProcess.Names[I - 1];
  end;
  AProcess.NameValueSeparator := VOldNameValueSeparator;
{$ENDIF}
{$IFDEF MSWINDOWS}
  try
    VSnapshotHandle := CreateToolHelp32SnapShot(TH32CS_SNAPALL, 0);
    VProcessEntry32.dwSize := SizeOf(TProcessEntry32);
    Process32First(VSnapshotHandle, VProcessEntry32);
    repeat
      if AIgnoreCurrent and (GetProcessID = VProcessEntry32.th32ProcessID) then
        Continue;
      if AShowPID then
        AProcess.Add(VProcessEntry32.szExeFile + CLSUtilsProcessNameValueSeparator +
          IntToStr(VProcessEntry32.th32ProcessID))
      else
        AProcess.Add(VProcessEntry32.szExeFile);
    until (not Process32Next(VSnapshotHandle, VProcessEntry32));
  except
    on E: Exception do
 {$IFNDEF LSGUI}
      WriteLn(SLSUtilsListProcessError);
 {$ELSE}
      Application.MessageBox(PChar(SLSUtilsListProcessError),
        PChar(Application.Title), MB_ICONERROR + MB_OK);
 {$ENDIF}
  end;
{$ENDIF}
  if AProcess.Count > 0 then
    AProcess.Delete(0);
  AProcess.Sorted := ASorted;
end;

function LSProcessIsRunning(const AProcessName: string;
  const AIgnoreCurrent: Boolean): Boolean;
var
  VProcess: TStringList;
begin
  VProcess := TStringList.Create;
  try
    LSListProcess(VProcess, False, False, AIgnoreCurrent);
    Result := VProcess.IndexOf(AProcessName) > -1;
  finally
    VProcess.Free;
  end;
end;

function LSKillProcess(const AProcessName: string): Boolean;
{$IFDEF MSWINDOWS}
const
  CPROCESS_TERMINATE = $0001;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  VContinueLoop: BOOL;
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := LSProcessIsRunning(AProcessName, True);
  if Result then
  begin
    LSExecProcess('sh -c "kill ' + IntToStr(LSGetProcessID(
      AProcessName, True)) + '"');
    Result := not LSProcessIsRunning(AProcessName, True);
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := False;
  VSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  VProcessEntry32.dwSize := SizeOf(VProcessEntry32);
  VContinueLoop := Process32First(VSnapshotHandle, VProcessEntry32);
  while Integer(VContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(VProcessEntry32.szExeFile)) =
      UpperCase(AProcessName)) or (UpperCase(VProcessEntry32.szExeFile) =
      UpperCase(AProcessName))) and
      (VProcessEntry32.th32ProcessID <> GetProcessID) then
      Result := TerminateProcess(OpenProcess(CPROCESS_TERMINATE, BOOL(0),
                  VProcessEntry32.th32ProcessID), 0);
    VContinueLoop := Process32Next(VSnapshotHandle, VProcessEntry32);
  end;
  CloseHandle(VSnapshotHandle);
  Sleep(100);
{$ENDIF}
end;

procedure LSGlobalAddAtom(const AAtomName: string);
begin
{$IFDEF UNIX}
  if LSAtomTableExists then
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
  if LSAtomList.IndexOf(AAtomName) = -1 then
    LSAtomList.Add(AAtomName);
  LSAtomList.SaveToFile(CLSAtomTableFileName);
{$ENDIF}
{$IFDEF MSWINDOWS}
  if GlobalFindAtom(LPCSTR(AAtomName)) = 0 then
    GlobalAddAtom(LPCSTR(AAtomName));
{$ENDIF}
end;

procedure LSGlobalDeleteAtom(const AAtomName: string);
{$IFDEF UNIX}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF UNIX}
  if LSAtomTableExists then
  begin
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
    I := LSAtomList.IndexOf(AAtomName);
    if I <> -1 then
      LSAtomList.Delete(I);
    LSAtomList.SaveToFile(CLSAtomTableFileName);
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  GlobalDeleteAtom(GlobalFindAtom(LPCSTR(AAtomName)));
{$ENDIF}
end;

function LSGlobalFindAtom(const AAtomName: string): Boolean;
begin
{$IFDEF UNIX}
  Result := False;
  if LSAtomTableExists then
  begin
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
    Result := LSAtomList.IndexOf(AAtomName) <> -1;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := GlobalFindAtom(LPCSTR(AAtomName)) <> 0;
{$ENDIF}
end;

function LSRemoveCid(const AHTML: string): string;
begin
  Result := StringReplace(AHTML, 'src="cid:', 'src="', [rfIgnoreCase,
    rfReplaceAll]);
end;

function LSStreamToBase64Str(AStream: TStream): string;
var
  VOutStream: TStringStream;
  VDecoder: TBase64DecodingStream;
begin
  VOutStream := TStringStream.Create('');
  try
    VDecoder := TBase64DecodingStream.Create(AStream, bdmMIME);
    try
       VOutStream.CopyFrom(VDecoder, VDecoder.Size);
       VOutStream.Position := 0;
       Result := VOutStream.ReadString(VOutStream.Size);
    finally
      VDecoder.Free;
    end;
  finally
    VOutStream.Free;
  end;
end;

procedure LSBase64StrToStream(const ABase64Str: string; AStream: TStream);
var
  VInStream: TStringStream;
  VDecoder: TBase64DecodingStream;
begin
  VInStream := TStringStream.Create(ABase64Str);
  try
    VDecoder := TBase64DecodingStream.Create(VInStream, bdmMIME);
    try
      AStream.CopyFrom(VDecoder, VDecoder.Size);
    finally
      VDecoder.Free;
    end;
  finally
    VInStream.Free;
  end;
end;

function LSFileToBase64Str(const AFileName: TFileName): string;
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LSStreamToBase64Str(TStream(VFileStream));
  finally
    VFileStream.Free;
  end;
end;

procedure LSBase64StrToFile(const ABase64Str: string;
  const AFileName: TFileName);
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    LSBase64StrToStream(ABase64Str, TStream(VFileStream));
  finally
    VFileStream.Free;
  end;
end;

function LSGetPhysicalSerialOfCurrentDisk
{$IFDEF MSWINDOWS}(out AHDDInfo: THDDInfo){$ENDIF}: string;
{$IFDEF UNIX}
const
  CDfCommand = 'df ./';
  CLsCommand = 'ls -l /dev/disk/by-id';

  function _InternalExecuteCommand(const ACommand: string): string;
  var
    VProcess: TProcess;
    VStrTemp: TStringList;
  begin
    VProcess := TProcess.Create(nil);
    try
      VProcess.Options := VProcess.Options + [poWaitOnExit, poUsePipes];
      VProcess.CommandLine := ACommand;
      VProcess.Execute;
      VStrTemp := TStringList.Create;
      VStrTemp.LoadFromStream(VProcess.Output);
      Result := VStrTemp.Text;
    finally
      VStrTemp.Free;
      VProcess.Free;
    end;
  end;

var
  I, J, K: Integer;
  VSda, VSerialNumber, VStr: string;
  VStrTemp1, VStrTemp2: TStrings;
{$ENDIF}
{$IFDEF MSWINDOWS}
  function _SerialNumberToString(ASerialNumber: string): string;
  var
    I, VStrLen: Integer;
    VPair: string;
    VByte: Byte;
    VChar: Char absolute VByte;
  begin
    Result := '';
    VStrLen := Length(ASerialNumber);
    if Odd(VStrLen) then
      Exit;
    I := 1;
    while I < VStrLen do
    begin
      VPair := Copy(ASerialNumber, I, 2);
      HexToBin(PChar(VPair), PChar(@VByte), 1);
      Result += Chr(VByte);
      Inc(I, 2);
    end;
    I := 1;
    while I < Length(Result) do
    begin
      VChar := Result[I];
      Result[I] := Result[I + 1];
      Result[I + 1] := VChar;
      Inc(I, 2);
    end;
  end;

const
  IOCTL_STORAGE_QUERY_PROPERTY = $2D1400;

type
  TCharArray = array[0..32767] of Char;
  PCharArray = ^TCharArray;

  STORAGE_PROPERTY_QUERY = packed record
    PropertyId: DWORD;
    QueryType: DWORD;
    AdditionalParameters: array[0..3] of Byte;
  end;

  STORAGE_DEVICE_DESCRIPTOR = packed record
    Version: ULONG;
    Size: ULONG;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: ULONG;
    ProductIdOffset: ULONG;
    ProductRevisionOffset: ULONG;
    SerialNumberOffset: ULONG;
    STORAGE_BUS_TYPE: DWORD;
    RawPropertiesLength: ULONG;
    RawDeviceProperties: array[0..511] of Byte;
  end;

var
  VReturned: Cardinal = 0;
  VStatus: LongBool;
  VPropQuery: STORAGE_PROPERTY_QUERY;
  VDeviceDescriptor: STORAGE_DEVICE_DESCRIPTOR;
  VPChar: PChar;
  VCurDrv: ShortString = '';
{$ENDIF}
begin
  Result := '';
{$IFDEF UNIX}
  VStrTemp1 := TStringList.Create;
  VStrTemp2 := TStringList.Create;
  try
    VStrTemp1.Text := _InternalExecuteCommand(CDfCommand);
    VStrTemp2.Text := _InternalExecuteCommand(CLsCommand);
    if (VStrTemp1.Count < 1) or (VStrTemp2.Count < 1) then
      Exit;
    VStr := VStrTemp1[1];
    J := Pos('/dev/', VStr);
    K := Pos(' ', VStr);
    VStr := Copy(VStr, 6, K - 6);
    VSda := VStr;
    VSerialNumber := '';
    for I := 0 to Pred(VStrTemp2.Count) do
    begin
      VStr := VStrTemp2[I];
      if Pos(' -> ../../' + VSda, VStr) > 0 then
      begin
        VSerialNumber := VStr;
        repeat
          J := Pos('_', VSerialNumber);
          if J > 0 then
            VSerialNumber := Copy(VSerialNumber, Succ(J), Length(VSerialNumber));
        until J = 0;
        J := Pos('-', VSerialNumber);
        if J > 0 then
          VSerialNumber := Copy(VSerialNumber, 1, Pred(J));
        Break;
      end;
    end;
  finally
    // <bus>-<brand>-<model>-<serial#>[-<partition>]
    Result := VSerialNumber;
    VStrTemp1.Free;
    VStrTemp2.Free;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  AHDDInfo.ProductRevision := '';
  AHDDInfo.ProductId := '';
  AHDDInfo.SerialNumber := '';
  AHDDInfo.VendorId := '';
  try
    GetDir(0, VCurDrv);
    {\\.\PhysicalDrive0}
    AHDDInfo.FileHandle := CreateFile(PChar('\\.\' + VCurDrv[1] + ':'),
      0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if AHDDInfo.FileHandle = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
    ZeroMemory(@VPropQuery, SizeOf(VPropQuery));
    ZeroMemory(@VDeviceDescriptor, SizeOf(VDeviceDescriptor));
    VDeviceDescriptor.Size := SizeOf(VDeviceDescriptor);
    VStatus := DeviceIoControl(AHDDInfo.FileHandle, IOCTL_STORAGE_QUERY_PROPERTY,
      @VPropQuery, SizeOf(VPropQuery), @VDeviceDescriptor, VDeviceDescriptor.Size,
      VReturned, nil);
    if not VStatus then
      RaiseLastOSError;
    if VDeviceDescriptor.VendorIdOffset <> 0 then
    begin
      VPChar := @PCharArray(@VDeviceDescriptor)^[VDeviceDescriptor.VendorIdOffset];
      AHDDInfo.VendorId := VPChar;
    end;
    if VDeviceDescriptor.ProductIdOffset <> 0 then
    begin
      VPChar := @PCharArray(@VDeviceDescriptor)^[VDeviceDescriptor.ProductIdOffset];
      AHDDInfo.ProductId := VPChar;
    end;
    if VDeviceDescriptor.ProductRevisionOffset <> 0 then
    begin
      VPChar := @PCharArray(@VDeviceDescriptor)^
        [VDeviceDescriptor.ProductRevisionOffset];
      AHDDInfo.ProductRevision := VPChar;
    end;
    if VDeviceDescriptor.SerialNumberOffset <> 0 then
    begin
      VPChar := @PCharArray(@VDeviceDescriptor)^[VDeviceDescriptor.SerialNumberOffset];
      AHDDInfo.SerialNumber := VPChar;
    end;
  finally
    if AHDDInfo.FileHandle <> INVALID_HANDLE_VALUE then
      CloseHandle(AHDDInfo.FileHandle);
  end;
  if AHDDInfo.SerialNumber <> '' then
    Result := Trim(_SerialNumberToString(AHDDInfo.SerialNumber));
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function LSGetPhysicalSerialOfCurrentDisk: string;
var
  VHDDInfo: THDDInfo;
begin
  Result := LSUtils.LSGetPhysicalSerialOfCurrentDisk(VHDDInfo);
end;
{$ENDIF}

function LSStrToHex(const AStr: string): string;
var
  I: Integer;
begin
  SetLength(Result, Length(AStr) * 2);
  for I := 1 to Length(AStr) do
  begin
    Result[I * 2 - 1] := CLSUtilsHexCharsArray[Byte(AStr[I]) shr 4];
    Result[I * 2] := CLSUtilsHexCharsArray[Byte(AStr[I]) and $0F];
  end;
end;

function LSHexToStr(const AHex: string): string;
var
  I: Integer;
begin
  if Length(AHex) mod 2 <> 0 then
    Exit;
  SetLength(Result, Length(AHex) div 2);
  for I := 1 to Length(Result) do
  begin
    if AHex[I * 2 - 1] in ['0'..'9'] then
      Result[I] := Chr(Ord(AHex[I * 2 - 1]) - Ord('0'))
    else
      if AHex[I * 2 - 1] in ['A'..'F'] then
        Result[I] := Chr(Ord(AHex[I * 2 - 1]) - Ord('A') + 10)
    else
      Exit;
    Result[I] := Chr(Ord(Result[I]) shl 4);
{$HINTS OFF}
    if AHex[I * 2] in ['0'..'9'] then
      Result[I] := Chr(Ord(Result[I]) + Ord(AHex[I * 2]) - Ord('0'))
    else
      if AHex[I * 2] in ['A'..'F'] then
        Result[I] := Chr(Ord(Result[I]) + Ord(AHex[I * 2]) - Ord('A') + 10)
      else
        Exit;
{$HINTS ON}
  end;
end;

function LSUUID: string;
var
  VGUID:TGUID;
begin
  CreateGUID(VGUID);
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [VGUID.D1, VGUID.D2, VGUID.D3, VGUID.D4[0], VGUID.D4[1], VGUID.D4[2],
    VGUID.D4[3], VGUID.D4[4], VGUID.D4[5], VGUID.D4[6], VGUID.D4[7]]);
end;

function LSUUID(const AUseSeparators: Boolean): string;
var
  VGUID:TGUID;
begin
  if AUseSeparators then
  begin
    CreateGUID(VGUID);
    SetLength(Result, 36);
    StrLFmt(PChar(Result), 36, CLSUtilsUUIDMask, [VGUID.D1, VGUID.D2, VGUID.D3,
      VGUID.D4[0], VGUID.D4[1], VGUID.D4[2], VGUID.D4[3], VGUID.D4[4],
      VGUID.D4[5], VGUID.D4[6], VGUID.D4[7]]);
  end
  else
    Result := LSUtils.LSUUID;
end;

{$WARNINGS OFF}
procedure LSDeleteCurrentExecutable(
{$IFDEF MSWINDOWS}const AAddBATCommands: string{$ENDIF});
{$IFDEF MSWINDOWS}
const
  CLSDelCurrExeFileName = 'lsdelcurrexe.bat';
  CLSLabelLSDelCurrExe = ':LabelLSLabelLSDelCurrExe';
  SYSTEM32_PATH = '%systemroot%\System32\';

var
  VExeName: string;
  VBAT: TStringList;
{$ENDIF}
begin
{$IFDEF UNIX}
  DeleteFile(ParamStr(0));
  LSExecProcess('sh -c "kill ' + IntToStr(GetProcessID) + '"');
{$ENDIF}
{$IFDEF MSWINDOWS}
  VExeName := '"' + ParamStr(0) + '"';
  VBAT := TStringList.Create;
  try
    VBAT.Add(SYSTEM32_PATH + 'taskkill /pid ' + IntToStr(GetProcessID) + ' /f');
    VBAT.Add(SYSTEM32_PATH + 'attrib -r -s -h ' + VExeName);
    VBAT.Add(CLSLabelLSDelCurrExe);
    VBAT.Add('del ' + VExeName);
    VBAT.Add('if exist ' + VExeName + ' goto ' + CLSLabelLSDelCurrExe);
    VBAT.Add(SYSTEM32_PATH + 'attrib -h ' + CLSDelCurrExeFileName);
    VBAT.Add(CLSLabelLSDelCurrExe + '2');
    VBAT.Add('del ' + CLSDelCurrExeFileName);
    VBAT.Add('if exist ' + CLSDelCurrExeFileName + ' goto ' +
      CLSLabelLSDelCurrExe + '2');
    if AAddBATCommands <> '' then
      VBAT.Add(AAddBATCommands);
    VBAT.Add('exit');
    VBAT.SaveToFile(CLSDelCurrExeFileName);
    SetFileAttributes(PChar(CLSDelCurrExeFileName), FILE_ATTRIBUTE_HIDDEN);
  finally
    VBAT.Free;
  end;
  WinExec(CLSDelCurrExeFileName, SW_HIDE);
{$ENDIF}
end;
{$WARNINGS ON}

function LSChangeFileDateTime(const AFileName: TFileName;
  const AFileDateTime: TDateTime): Boolean;
var
  VFileHandle: THandle;
  VFileSetDateTimeResult: Integer;
begin
  VFileHandle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    try
      if VFileHandle > 0 then
      begin
        VFileSetDateTimeResult :=
          FileSetDate(VFileHandle, DateTimeToFileDate(AFileDateTime));
        Result := VFileSetDateTimeResult = 0;
      end;
    except
      Result := False;
    end;
  finally
    FileClose(VFileHandle);
  end;
end;

function LSLastPos(const ASubString, AString: string): Integer;
var
  VFound, VLen, VPos: Integer;
begin
  VPos := Length(AString);
  VLen := Length(ASubString);
  VFound := 0;
  while (VPos > 0) and (VFound = 0) do
  begin
    if Copy(AString, VPos, VLen) = ASubString then
      VFound := VPos;
    Dec(VPos);
  end;
  Result := VFound;
end;

function LSDeleteLineBreaks(const AString: string;
  const ASubstitute: Char): string;
var
  I: Integer;
begin
  Result := AString;
  for I := 1 to Length(AString) do
    case Result[I] of
      #10: Result[I] := ASubstitute;
      #13: Result[I] := ASubstitute;
    end;
end;

function LSReplaceChar(const AString: string; const AOldChar, ANewChar: Char): string;
var
  I: Integer;
begin
  Result := AString;
  for I := 1 to Length(AString) do
    if Result[I] = AOldChar then
      Result[I] := ANewChar;
end;

function LSReplaceChar(const AString: string; const AOldChars: TLSChars;
  const ANewChar: Char): string;
var
  I: Integer;
begin
  Result := AString;
  for I := 1 to Length(AString) do
    if Result[I] in AOldChars then
      Result[I] := ANewChar;
end;

{$IFDEF LSGUI}
function LSShortenPathName(const ARect: TRect; ACanvas: TCanvas;
  const AFileName: TFileName): TFileName;

  procedure _CutFirstDir(var ADir: string);
  var
    VPos: Integer;
    VRoot: Boolean;
  begin
    if ADir = DirectorySeparator then
      ADir := ''
    else
    begin
      if ADir[1] = DirectorySeparator then
      begin
        VRoot := True;
        Delete(ADir, 1, 1);
      end
      else
        VRoot := False;
      if ADir[1] = '.' then
        Delete(ADir, 1, 4);
      VPos := Pos(DirectorySeparator, ADir);
      if VPos <> 0 then
      begin
        Delete(ADir, 1, VPos);
        ADir := CLSEllipsis + DirectorySeparator + ADir;
      end
      else
        ADir := '';
      if VRoot then
        ADir := DirectorySeparator + ADir;
    end;
  end;

var
  VMaxLen: Integer;
  VDir, VName, VDrive: string;
begin
  Result := AFileName;
  try
    ACanvas.Handle := GetDC(0);
    ACanvas.Font.Assign(ACanvas.Font);
    VMaxLen := ARect.Right - ARect.Left;
    VDir := ExtractFilePath(Result);
    VName := ExtractFileName(Result);
{$IFDEF MSWINDOWS}
    if (Length(VDir) >= 2) and (VDir[2] = DriveSeparator) then
    begin
      VDrive := Copy(VDir, 1, 2);
      Delete(VDir, 1, 2);
    end
    else
      VDrive := '';
{$ENDIF}
{$IFDEF UNIX}
    VDrive := Copy(Vdir, 1, Pos(DirectorySeparator, Copy(VDir, 2, MaxInt)));
{$ENDIF}
    while ((VDir <> '') or (VDrive <> '')) and
      (ACanvas.TextWidth(Result) + 2 > VMaxLen) do
    begin
      if VDir = DirectorySeparator + CLSEllipsis + DirectorySeparator then
      begin
        VDrive := '';
        VDir := CLSEllipsis + DirectorySeparator;
      end
      else
      if VDir = '' then
        VDrive := ''
      else
        _CutFirstDir(VDir);
      Result := VDrive + VDir + VName;
    end;
  finally
    ReleaseDC(0, ACanvas.Handle);
  end;
end;

function LSShortenPathName(const ARect: TRect; const AFont: TFont;
  const AFileName: TFileName): TFileName;
var
  VCanvas: TCanvas;
begin
  VCanvas := TCanvas.Create;
  try
    VCanvas.Font.Assign(AFont);
    Result := LSUtils.LSShortenPathName(ARect, VCanvas, AFileName);
  finally
    VCanvas.Free;
  end;
end;

procedure LSShowHint(const AControl: TWinControl; const AHint: string);
var
  VRect: TRect;
begin
  if not Assigned(AControl) then
    Exit;
  LSCloseHint(AControl);
  with THintWindow.Create(AControl) do
  begin
    Name := CLSUtilsHintName;
    AutoHide := True;
    VRect := CalcHintRect(Screen.Width, AHint, nil);
    VRect.Left += AControl.ClientOrigin.X;
    VRect.Right += AControl.ClientOrigin.X;
    VRect.Top += AControl.ClientOrigin.Y - Height;
    VRect.Bottom += AControl.ClientOrigin.Y - Height;
    ActivateHint(VRect, AHint);
  end;
end;

procedure LSCloseHint(const AControl: TWinControl);
var
  VHint: THintWindow;
begin
  VHint := AControl.FindComponent(CLSUtilsHintName) as THintWindow;
  if Assigned(VHint) then
    VHint.Free;
end;
{$ENDIF}

function LSGetTime: Int64;
begin
  Result := MilliSecondsBetween(CSince19700101, Now);
end;

function LSStripHTMLMarkup(const ASource: string): string;
var
  VSource: PChar;
  VInTag: Boolean;
  I, VCount: Integer;
begin
  SetLength(Result, Length(ASource));
  VSource := PChar(Result);
  VInTag := False;
  VCount := 0;
  for I := 1 to Length(ASource) do
    if VInTag then
    begin
      if ASource[I] = '>' then
        VInTag := False;
    end
    else
      if ASource[I] = '<' then
        VInTag := True
      else
      begin
        VSource[VCount] := ASource[I];
        Inc(VCount);
      end;
  SetLength(Result, VCount);
end;

{$IFDEF LSLCL}
function LSGetPartFromStream(const ASkipTo, AEnd: string; var ASource: TStream;
  const AIgnoreCase: Boolean; const AUpdateSource: Boolean): string;
begin
  SetLength(Result, ASource.Size);
  ASource.Read(Pointer(Result)^, ASource.Size);
  Result := GetPart(ASkipTo, AEnd, Result, AIgnoreCase, AUpdateSource);
end;
{$ENDIF}

procedure LSStrToStream(const AString: string; var AStream: TStream);
begin
  AStream.Write(Pointer(AString)^, Length(AString));
  AStream.Position := 0;
end;

function LSStreamToStr(const AStream: TStream): string;
begin
  AStream.Position := 0;
  SetLength(Result, AStream.Size);
  AStream.Write(Pointer(Result)^, AStream.Size);
end;

{$IFDEF LSGUI}
function LSFindItemInListBox(var AListBox: TCustomListBox; const AText: string;
  const ACaseSensitive: Boolean; const AFindNext: Boolean): Integer;
var
  I, VIndex, VCount: Integer;
  VText, VItem: string;
begin
  VCount := AListBox.Items.Count;
  if VCount = -1 then
    Exit;
  try
    AListBox.Items.BeginUpdate;
    if AFindNext then
      VIndex := Succ(AListBox.ItemIndex)
    else
      VIndex := 0;
    for I := VIndex to Pred(VCount) do
    begin
      if ACaseSensitive then
      begin
        VText := AText;
        VItem := AListBox.Items[I];
      end
      else
      begin
        VText := LowerCase(AText);
        VItem := LowerCase(AListBox.Items[I]);
      end;
      if Pos(VText, VItem) <> 0 then
      begin
        Result := I;
        AListBox.ItemIndex := I;
        AListBox.TopIndex := I;
        Exit;
      end;
    end;
    Result := -1;
    AListBox.ItemIndex := -1;
  finally
    AListBox.Items.EndUpdate;
  end;
end;
{$ENDIF}

procedure LSJSONArrayToStrings(const AJSON: TJSONStringType; AStrings: TStrings);
const
  LSJSONARRAYTOSTRINGSNAME = 'LSJSONArrayToStrings';
var
  I: Integer;
  VJSONData: TJSONData = nil;
  VJSONArray: TJSONArray;
  VJSONParser: TJSONParser;
begin
  VJSONParser := TJSONParser.Create(AJSON);
  try
    VJSONData := VJSONParser.Parse;
    if not Assigned(VJSONData) then
      LSShowJSONEmptyDatabaseError(LSJSONARRAYTOSTRINGSNAME);
    if VJSONData.JSONType <> jtArray then
      LSShowJSONIsNotArrayError(LSJSONARRAYTOSTRINGSNAME, VJSONData.ClassName);
    if VJSONData.Count < 1 then
      LSShowJSONEmptyArrayError(LSJSONARRAYTOSTRINGSNAME);
    VJSONArray := TJSONArray(VJSONData);
    AStrings.Clear;
    for I := 0 to Pred(VJSONArray.Count) do
      AStrings.Add(VJSONArray[I].AsString);
  finally
    VJSONData.Free;
    VJSONParser.Free;
  end;
end;

function LSStringsToJSONArray(AStrings: TStrings): TJSONStringType;
var
  I: Integer;
  VJSONArray: TJSONArray;
begin
  VJSONArray := TJSONArray.Create;
  try
    for I := 0 to Pred(AStrings.Count) do
      VJSONArray.Add(AStrings[I]);
    Result := VJSONArray.AsJSON;
  finally
    VJSONArray.Free;
  end;
end;

procedure LSJSONToJSONObjectStrings(var AJSONObject: TJSONObject;
  const AJSON: TJSONStringType; AStrings: TStrings);
const
  LSJSONTOJSONOBJECTSTRINGS = 'LSJSONToJSONObjectStrings';
var
  I: Integer;
  VJSONParser: TJSONParser;
begin
  VJSONParser := TJSONParser.Create(AJSON);
  try
    AJSONObject := TJSONObject(VJSONParser.Parse);
    if not Assigned(AJSONObject) then
      LSShowJSONEmptyDatabaseError(LSJSONTOJSONOBJECTSTRINGS);
    if AJSONObject.JSONType <> jtObject then
      LSShowJSONIsNotObjectError(LSJSONTOJSONOBJECTSTRINGS,
        AJSONObject.ClassName);
    if AJSONObject.Count < 1 then
      LSShowJSONEmptyObjectError(LSJSONTOJSONOBJECTSTRINGS);
    AStrings.Clear;
    for I := 0 to Pred(AJSONObject.Count) do
      AStrings.Add(AJSONObject.Names[I]);
  finally
    VJSONParser.Free;
  end;
end;

function LSRoundAnExtended(const AValue: Extended;
  const ARoundingMode: TFPURoundingMode;
  const ARoundingDigits: Integer): Extended;
var
  VRoundingMode: TFPURoundingMode;
begin
{
  AFPURoundingMode:

  rmNearest: Round to nearest integer value;
  rmDown: Round to biggest integer smaller than value;
  rmUp: Round to smallest integer larger than value;
  rmTruncate: Cut off fractional part;
}
  VRoundingMode := GetRoundMode;
  try
    SetRoundMode(ARoundingMode);
    Result := RoundTo(AValue, ARoundingDigits);
  finally
    SetRoundMode(VRoundingMode);
  end;
end;

function LSSizeToString(const ASize: Extended): string;
var
  I: Extended;
begin
  I := Abs(ASize);
  if I < 1000 then
    Result := Format('%.0n B', [ASize / 1])
  else if I < 10235 then
    Result := Format('%.2n KB', [ASize / 1024])
  else if I < 102349 then
    Result := Format('%.1n KB', [ASize / 1024])
  else if I < 1023488 then
    Result := Format('%.0n KB', [ASize / 1024])
  else if I < 10480518 then
    Result := Format('%.2n MB', [ASize / 1048576])
  else if I < 104805172 then
    Result := Format('%.1n MB', [ASize / 1048576])
  else if I < 1048051712 then
    Result := Format('%.0n MB', [ASize / 1048576])
  else if I < 10732049531 then
    Result := Format('%.2n GB', [ASize / 1073741824])
  else if I < 107320495309 then
    Result := Format('%.1n GB', [ASize / 1073741824])
  else if I < 1073204953088 then
    Result := Format('%.0n GB', [ASize / 1073741824])
  else if I < 10989618719622 then
    Result := Format('%.2n TB', [ASize / 1099511627776])
  else if I < 109896187196212 then
    Result := Format('%.1n TB', [ASize / 1099511627776])
  else if I < 1098961871962112 then
    Result := Format('%.0n TB', [ASize / 1099511627776])
  else if I < 11253369568892027 then
    Result := Format('%.2n PB', [ASize / 1125899906842624])
  else if I < 112533695688920269 then
    Result := Format('%.1n PB', [ASize / 1125899906842624])
  else if I < 1125336956889202688 then
    Result := Format('%.0n PB', [ASize / 1125899906842624])
  else
    Result := Format('%.2n EB', [ASize / 1152921504606846976]);
end;

{$IFDEF UNIX}
initialization

finalization
  if Assigned(_LSAtomList) then
    _LSAtomList.Free;
{$ENDIF}

end.

