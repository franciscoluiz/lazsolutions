(*
  LazSolutions, Play WAV unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSPlayWAV;

{$I lazsolutions.inc}

interface

uses
{$IFDEF UNIX}
  Classes, SysUtils, FileUtil, Process, LSUtils, LSConsts, LSMessages;
{$ENDIF}
{$IFDEF MSWINDOWS}
  MMSystem;
{$ENDIF}

type

  { ILSPlayWAV }

  ILSPlayWAV = interface
    ['{5267BB1A-D21D-B211-93B2-000FEA22BE72}']
    procedure Execute(const AWAVFile: string;
      const AAsynchronous: Boolean = True);
    function GetPlayer: string;
    procedure SetPlayer(const AValue: string);
    property Player: string read GetPlayer write SetPlayer;
  end;

  { TLSPlayWAV }

  TLSPlayWAV = class(TInterfacedObject, ILSPlayWAV)
  private
    FPlayer: string;
    function GetPlayer: string;
    procedure SetPlayer(const AValue: string);
  public
{$IFDEF UNIX}
    constructor Create;
{$ENDIF}
    procedure Execute(const AWAVFile: string;
      const AAsynchronous: Boolean = True);
    property Player: string read GetPlayer write SetPlayer;
  end;

{$IFDEF UNIX}
function LSGetLinuxPlayer: string;
{$ENDIF}
procedure LSPlayWAVFile(const AWAVFile: string;
  const AAsynchronous: Boolean = True);

implementation

{$IFDEF UNIX}
function LSGetLinuxPlayer: string;
var
  S: string;
  I: Integer;
  VPlayers, VPlayersPaths: TStringList;
begin
  Result := SLSPlayWAVPlayerNotFound;
  VPlayers := TStringList.Create;
  VPlayersPaths := TStringList.Create;
  try
    VPlayers.Text := CLSUtilsDistrosPlayer;
    VPlayers.NameValueSeparator := ':';
    VPlayersPaths.Clear;
    ExtractStrings([','], [' '], PChar(VPlayers.Values[LSGetLinuxDistro]),
      VPlayersPaths);
    for I := 0 to Pred(VPlayersPaths.Count) do
    begin
      S := FindDefaultExecutablePath(VPlayersPaths.ValueFromIndex[I]);
      if FileExists(S) then
      begin
        Result := S;
        Break;
      end;
    end;
  finally
    VPlayers.Free;
    VPlayersPaths.Free;
  end;
end;
{$ENDIF}

procedure LSPlayWAVFile(const AWAVFile: string;
  const AAsynchronous: Boolean = True);
var
  VLSPlayWAV: ILSPlayWAV;
begin
  VLSPlayWAV := TLSPlayWAV.Create;
  VLSPlayWAV.Execute(AWAVFile, AAsynchronous);
end;

{ TLSPlayWAV }

function TLSPlayWAV.GetPlayer: string;
begin
  Result := FPlayer;
end;

procedure TLSPlayWAV.SetPlayer(const AValue: string);
begin
  FPlayer := AValue;
end;

{$IFDEF UNIX}
constructor TLSPlayWAV.Create;
var
  VPlayers: TStringList;
begin
  VPlayers := TStringList.Create;
  try
    VPlayers.NameValueSeparator := ':';
    VPlayers.Text := CLSUtilsDistrosPlayer;
    FPlayer := LSGetLinuxPlayer;
  finally
    VPlayers.Free;
  end;
end;
{$ENDIF}

procedure TLSPlayWAV.Execute(const AWAVFile: string;
  const AAsynchronous: Boolean);
{$IFDEF UNIX}
var
  VProcess: TProcess;
{$ENDIF}
begin
{$IFDEF UNIX}
  if FPlayer = SLSPlayWAVPlayerNotFound then
    raise Exception.Create('ERROR: Implement the path of a player of this OS.');
  VProcess := TProcess.Create(nil);
  try
    if AAsynchronous then
      VProcess.Options := [poNoConsole]
    else
      VProcess.Options := [poNoConsole, poWaitOnExit];
    VProcess.CommandLine := FPlayer + ' ' + QuotedStr(AWAVFile);
    VProcess.Execute;
  finally
    VProcess.Free;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  if AAsynchronous then
    sndPlaySound(PChar(AWAVFile), SND_ASYNC or SND_FILENAME)
  else
    sndPlaySound(PChar(AWAVFile), SND_SYNC or SND_FILENAME)
{$ENDIF}
end;

end.

