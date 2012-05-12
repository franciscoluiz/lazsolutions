(*
  LazSolutions, JSON properties storage unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSJSONPropStorage;

{$I lazsolutions.inc}

interface

uses
{$IFDEF UNIX}
  FileUtil,
{$ENDIF}
  LSCommonIntf, SysUtils, Forms, LazConfigStorage, JSONConf;

type

  { TLSCustomJSONPropStorage }

  TLSCustomJSONPropStorage = class(TFormPropertyStorage, ILSAboutComponent)
  private
    FCount: Integer;
    FFileName: TFileName;
    FJSONConfig: TJSONConfig;
    FRootPath: string;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
  protected
    function GetJSONFileName: string; virtual;
    function RootSection: string; override;
    function FixPath(const APath: string): string; virtual;
    property JSONConfig: TJSONConfig read FJSONConfig;
  public
    procedure StorageNeeded(AReadOnly: Boolean); override;
    procedure FreeStorage; override;
    function DoReadString(const ASection, AIdent,
      ADefault: string): string; override;
    procedure DoWriteString(const ASection, AIdent, AValue: string); override;
    procedure DoEraseSections(const ARootSection: string); override;
    property About: string read GetAbout write SetAbout stored False;
    property FileName: string read FFileName write FFileName;
    property RootPath: string read FRootPath write FRootPath;
  end;

  { TLSJSONPropStorage }

  TLSJSONPropStorage = class(TLSCustomJSONPropStorage)
  published
    property About stored False;
    property StoredValues;
    property FileName;
    property RootPath;
    property Active;
    property OnSavingProperties;
    property OnSaveProperties;
    property OnRestoringProperties;
    property OnRestoreProperties;
  end;

  { TLSJSONConfigStorage }

  TLSJSONConfigStorage = class(TConfigStorage)
  private
    FFreeJSONConfig: Boolean;
    FJSONConfig: TJSONConfig;
  protected
    function GetFullPathValue(const APath, ADefault: string): string; override;
    function GetFullPathValue(const APath: string; ADefault: Integer): Integer;
      override;
    function GetFullPathValue(const APath: string; ADefault: Boolean): Boolean;
      override;
    procedure SetFullPathValue(const APath, AValue: string); override;
    procedure SetDeleteFullPathValue(const APath, AValue,
      ADefValue: string); override;
    procedure SetFullPathValue(const APath: string; AValue: Integer); override;
    procedure SetDeleteFullPathValue(const APath: string;
      AValue, ADefValue: Integer); override;
    procedure SetFullPathValue(const APath: string; AValue: Boolean); override;
    procedure SetDeleteFullPathValue(const APath: string;
      AValue, ADefValue: Boolean); override;
    procedure DeleteFullPath(const APath: string); override;
    procedure DeleteFullPathValue(const APath: string); override;
  public
    procedure Clear; override;
    constructor Create(const AFileName: string;
      ALoadFromDisk: Boolean); override;
    constructor Create(AJSONConfig: TJSONConfig);
    constructor Create(AJSONConfig: TJSONConfig; const AStartPath: string);
    destructor Destroy; override;
    property JSONConfig: TJSONConfig read FJSONConfig;
    property FreeJSONConfig: Boolean read FFreeJSONConfig write FFreeJSONConfig;
    procedure WriteToDisk; override;
    function GetFileName: string; override;
  end;

implementation

{ TLSCustomJSONPropStorage }

{$HINTS OFF}
procedure TLSCustomJSONPropStorage.StorageNeeded(AReadOnly: Boolean);
var
  VDir: string;
begin
  if not Assigned(FJSONConfig) then
  begin
    FJSONConfig := TJSONConfig.Create(nil);
    FJSONConfig.FileName := GetJSONFileName;
    VDir := ExtractFilePath(FJSONConfig.FileName);
    if not DirectoryExists(VDir) then
      MkDir(VDir);
  end;
  Inc(FCount);
end;
{$HINTS ON}

procedure TLSCustomJSONPropStorage.FreeStorage;
begin
  Dec(FCount);
  if FCount <= 0 then
  begin
    FCount := 0;
    FreeAndNil(FJSONConfig);
  end;
end;

function TLSCustomJSONPropStorage.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomJSONPropStorage.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

function TLSCustomJSONPropStorage.GetJSONFileName: string;
begin
  if FFileName <> '' then
    Result := FFileName
  else
{$IFDEF UNIX}
    Result := IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME')) +
      '.' + ExtractFileName(Application.ExeName);
{$ELSE}
  Result := ChangeFileExt(Application.ExeName, '.json');
{$ENDIF}
end;

function TLSCustomJSONPropStorage.FixPath(const APath: string): string;
begin
  Result := StringReplace(APath, '.', '/', [rfReplaceAll]);
end;

function TLSCustomJSONPropStorage.RootSection: string;
begin
  if FRootPath <> '' then
    Result := FRootPath
  else
    Result := inherited RootSection;
  Result := FixPath(Result);
end;

function TLSCustomJSONPropStorage.DoReadString(
  const ASection, AIdent, ADefault: string): string;
begin
  Result := AnsiString(FJSONConfig.GetValue(WideString(FixPath(ASection) + '/' +
    AIdent), WideString(ADefault)));
end;

procedure TLSCustomJSONPropStorage.DoWriteString(
  const ASection, AIdent, AValue: string);
begin
  FJSONConfig.SetValue(WideString(FixPath(ASection) + '/' + AIdent),
    WideString(AValue));
end;

procedure TLSCustomJSONPropStorage.DoEraseSections(const ARootSection: string);
begin
  FJSONConfig.DeletePath(WideString(FixPath(ARootSection)));
end;

{ TLSJSONConfigStorage }

{$HINTS OFF}
constructor TLSJSONConfigStorage.Create(const AFileName: string;
  ALoadFromDisk: Boolean);
begin
  FJSONConfig := TJSONConfig.Create(nil);
  FJSONConfig.FileName := AFileName;
  FFreeJSONConfig := True;
end;
{$HINTS ON}

constructor TLSJSONConfigStorage.Create(AJSONConfig: TJSONConfig);
begin
  FJSONConfig := AJSONConfig;
  if not Assigned(FJSONConfig) then
    raise Exception.Create('');
end;

constructor TLSJSONConfigStorage.Create(AJSONConfig: TJSONConfig;
  const AStartPath: string);
begin
  Create(AJSONConfig);
  AppendBasePath(AStartPath);
end;

destructor TLSJSONConfigStorage.Destroy;
begin
  if FreeJSONConfig then
    FreeAndNil(FJSONConfig);
  inherited Destroy;
end;

function TLSJSONConfigStorage.GetFullPathValue(const APath,
  ADefault: string): string;
begin
  Result := AnsiString(FJSONConfig.GetValue(WideString(APath),
    WideString(ADefault)));
end;

function TLSJSONConfigStorage.GetFullPathValue(const APath: string;
  ADefault: Integer): Integer;
begin
  Result := FJSONConfig.GetValue(WideString(APath), ADefault);
end;

function TLSJSONConfigStorage.GetFullPathValue(const APath: string;
  ADefault: Boolean): Boolean;
begin
  Result := FJSONConfig.GetValue(WideString(APath), ADefault);
end;

procedure TLSJSONConfigStorage.SetFullPathValue(const APath, AValue: string);
begin
  FJSONConfig.SetValue(WideString(APath), WideString(AValue));
end;

procedure TLSJSONConfigStorage.SetDeleteFullPathValue(
  const APath, AValue, ADefValue: string);
begin
  FJSONConfig.SetDeleteValue(WideString(APath), WideString(AValue),
    WideString(ADefValue));
end;

procedure TLSJSONConfigStorage.SetFullPathValue(const APath: string;
  AValue: Integer);
begin
  FJSONConfig.SetValue(WideString(APath), AValue);
end;

procedure TLSJSONConfigStorage.SetDeleteFullPathValue(const APath: string;
  AValue, ADefValue: Integer);
begin
  FJSONConfig.SetDeleteValue(WideString(APath), AValue, ADefValue);
end;

procedure TLSJSONConfigStorage.SetFullPathValue(const APath: string; AValue: Boolean);
begin
  FJSONConfig.SetValue(WideString(APath), AValue);
end;

procedure TLSJSONConfigStorage.SetDeleteFullPathValue(const APath: string;
  AValue, ADefValue: Boolean);
begin
  FJSONConfig.SetDeleteValue(WideString(APath), AValue, ADefValue);
end;

procedure TLSJSONConfigStorage.DeleteFullPath(const APath: string);
begin
  FJSONConfig.DeletePath(WideString(APath));
end;

procedure TLSJSONConfigStorage.DeleteFullPathValue(const APath: string);
begin
  FJSONConfig.DeleteValue(WideString(APath));
end;

procedure TLSJSONConfigStorage.Clear;
begin
  FJSONConfig.Clear;
end;

procedure TLSJSONConfigStorage.WriteToDisk;
begin
  FJSONConfig.Flush;
end;

function TLSJSONConfigStorage.GetFileName: string;
begin
  Result := FJSONConfig.FileName;
end;

end.

