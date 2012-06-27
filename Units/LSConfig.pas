(*
  LazSolutions, Config unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSConfig;

{$I lazsolutions.inc}

interface

uses
  LSConsts, Classes, SysUtils, Variants;

type

  { TLSConfigVarRec }

  TLSConfigVarRec = object
  private
    FValue: string;
  public
    function AsString: string;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsBoolean: Boolean;
    function AsChar: Char;
    function AsExtended: Extended;
    function AsCurrency: Currency;
  end;

  { TLSConfigOutputValues }

  TLSConfigOutputValues = array of TLSConfigVarRec;

  { TLSCondigOperationSaveLoad }

  TLSCondigOperationSaveLoad = (opLoad, opSave);

{ Set string to a variable. }
procedure LSSetStringToVar(const AString: string; const AType;
  const AValue: Pointer);
{ Load (or Save) your configurations from a file. }
procedure LSLoadSaveConfig(var AConfigs: TStringList;
  const AOperation: TLSCondigOperationSaveLoad; const AFileName: TFileName);
{ Load your configurations from a file. }
procedure LSLoadConfig(const AOutputNames: array of ShortString;
  const AOutputTypes: array of const; const AOutputValues: array of Pointer;
  const AFileName: TFileName = '');
procedure LSLoadConfig(const AOutputNames: array of ShortString;
  var AOutputValues: TLSConfigOutputValues; const AFileName: TFileName = '');
{ Save your configurations to a file. }
procedure LSSaveConfig(const AInputNames: array of ShortString;
  const AInputValues: array of Variant; const AFileName: TFileName = '');

var
  LSConfigFileName: TFileName;

implementation

procedure LSSetStringToVar(const AString: string; const AType;
  const AValue: Pointer);
begin
  case TVarRec(AType).VType of
    vtString: string(AValue^) := AString;
    vtAnsiString: AnsiString(AValue^) := AString;
    vtInteger: Integer(AValue^) := StrToIntDef(AString, 0);
    vtInt64: Int64(AValue^) := StrToInt64Def(AString, 0);
    vtBoolean: Boolean(AValue^) := StrToBoolDef(AString, False);
    vtChar: Char(AValue^) := (AString + #0)[1];
    vtExtended: Extended(AValue^) := StrToFloatDef(AString, 0);
    vtCurrency: Currency(AValue^) := StrToCurrDef(AString, 0);
  end;
end;

procedure LSLoadSaveConfig(var AConfigs: TStringList;
  const AOperation: TLSCondigOperationSaveLoad; const AFileName: TFileName);
var
  VConfigDir: string;
  VConfigFileName: TFileName;
begin
  if AFileName <> '' then
    VConfigFileName := AFileName
  else
    VConfigFileName := LSConfigFileName;
  case AOperation of
    opLoad:
      if FileExists(VConfigFileName) then
        AConfigs.LoadFromFile(VConfigFileName);
    opSave:
      begin
        VConfigDir := ExtractFileDir(VConfigFileName);
        if not DirectoryExists(VConfigDir) then
          ForceDirectories(VConfigDir);
        AConfigs.SaveToFile(VConfigFileName);
      end;
  end;
end;

procedure LSLoadConfig(const AOutputNames: array of ShortString;
  const AOutputTypes: array of const; const AOutputValues: array of Pointer;
  const AFileName: TFileName);
var
  I: Integer;
  VOutputValue: string;
  VConfigs: TStringList;
begin
  VConfigs := TStringList.Create;
  try
    LSLoadSaveConfig(VConfigs, opLoad, AFileName);
    for I := 0 to High(AOutputNames) do
    begin
      VOutputValue := VConfigs.Values[AOutputNames[I]];
      LSSetStringToVar(VOutputValue, AOutputTypes[I], AOutputValues[I]);
    end;
  finally
    VConfigs.Free;
  end;
end;

procedure LSLoadConfig(const AOutputNames: array of ShortString;
  var AOutputValues: TLSConfigOutputValues; const AFileName: TFileName);
var
  VOutputValue: string;
  VConfigs: TStringList;
  I, VHighOutputNames: Integer;
begin
  VConfigs := TStringList.Create;
  try
    LSLoadSaveConfig(VConfigs, opLoad, AFileName);
    VHighOutputNames := High(AOutputNames);
    SetLength(AOutputValues, VHighOutputNames + 1);
    for I := 0 to VHighOutputNames do
    begin
      VOutputValue := VConfigs.Values[AOutputNames[I]];
      AOutputValues[I].FValue := VOutputValue;
    end;
  finally
    VConfigs.Free;
  end;
end;

{ TLSConfigVarRec }

function TLSConfigVarRec.AsString: string;
begin
  Result := FValue;
end;

function TLSConfigVarRec.AsInteger: Integer;
begin
  Result := StrToIntDef(FValue, 0);
end;

function TLSConfigVarRec.AsInt64: Int64;
begin
  Result := StrToInt64Def(FValue, 0);
end;

function TLSConfigVarRec.AsBoolean: Boolean;
begin
  Result := StrToBoolDef(FValue, False);
end;

function TLSConfigVarRec.AsChar: Char;
begin
  Result := FValue[1];
end;

function TLSConfigVarRec.AsExtended: Extended;
begin
  Result := StrToFloatDef(FValue, 0);
end;

function TLSConfigVarRec.AsCurrency: Currency;
begin
  Result := StrToCurrDef(FValue, 0);
end;

procedure LSSaveConfig(const AInputNames: array of ShortString;
  const AInputValues: array of Variant; const AFileName: TFileName);
var
  I: Integer;
  VConfigs: TStringList;
  VInputValues: string;
begin
  if High(AInputNames) <> High(AInputValues) then
    raise Exception.Create(
      'Number of items in "AInputNames" is different of "AInputValues".');
  VConfigs := TStringList.Create;
  try
    LSLoadSaveConfig(VConfigs, opLoad, AFileName);
    for I := Low(AInputNames) to High(AInputNames) do
    begin
      VInputValues := AInputValues[I];
      VConfigs.Values[AInputNames[I]] := VInputValues;
    end;
    LSLoadSaveConfig(VConfigs, opSave, AFileName);
  finally
    VConfigs.Free;
  end;
end;

initialization
  LSConfigFileName := GetAppConfigDir(False) + CLSCONFIGFILENAME;

end.

