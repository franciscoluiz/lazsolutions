(*
  LazSolutions, Dialogs unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSDialogs;

{$I lazsolutions.inc}

interface

uses
  LSLookupDlg, LSMessages, FPJSON, Controls, SysUtils, Dialogs, ExtDlgs,
  Variants;

type

  { TLSOpenDialogType }

  TLSOpenDialogType = (odtFile, odtPicture, odtDirectory);

  { TLSSaveDialogType }

  TLSSaveDialogType = (sdtFile, sdtPicture);

{ Open a file or directory. }
function LSOpenDialog(const ALSOpenDialogType: TLSOpenDialogType = odtFile;
  const AInitialDir: string = ''; const AFilter: string = '';
  const AFilterIndex: Integer = -1; const AMultiSelect: Boolean = False;
  const AAbortIfNotExecute: Boolean = True): string;
{ Save a file. }
function LSSaveDialog(var AFileName: TFileName;
  const ALSSaveDialogType: TLSSaveDialogType = sdtFile;
  const AInitialDir: string = ''; const AFilter: string = '';
  const AFilterIndex: Integer = -1; const AOverwritePrompt: Boolean = True;
  const AAbortIfNotExecute: Boolean = True;
  const AForceExt: string = ''): Boolean;
function LSSaveDialogS(const ADefFileName: string = '';
  const AFilter: string = ''; const AForceExt: string = '';
  const AFilterIndex: Integer = -1;
  const ALSSaveDialogType: TLSSaveDialogType = sdtFile): string;
{ JSON lookup dialog. }
function LSJSONLookupDialog(const ACaption: TCaption; var AText: TJSONStringType;
  const AJSON: TJSONStringType; var AJSONObject: TJSONObject;
  const AWidth: Integer = 298; const AHeight: Integer = 260): Boolean;
function LSJSONLookupDialog(const ACaption: TCaption; var AText: TJSONStringType;
  const AJSON: TJSONStringType; var AJSONValue: Variant;
  const AWidth: Integer = 298; const AHeight: Integer = 260): Boolean;

implementation

function LSOpenDialog(const ALSOpenDialogType: TLSOpenDialogType;
  const AInitialDir: string; const AFilter: string;
  const AFilterIndex: Integer; const AMultiSelect: Boolean;
  const AAbortIfNotExecute: Boolean): string;
var
  VOpenDialog: TOpenDialog;
begin
  case ALSOpenDialogType of
    odtFile: VOpenDialog := TOpenDialog.Create(nil);
    odtPicture: VOpenDialog := TOpenPictureDialog.Create(nil);
    odtDirectory: VOpenDialog := TSelectDirectoryDialog.Create(nil);
  end;
  try
    if AInitialDir <> '' then
      VOpenDialog.InitialDir := AInitialDir;
    if AFilter <> '' then
      VOpenDialog.Filter := AFilter;
    if AFilterIndex > -1 then
      VOpenDialog.FilterIndex := AFilterIndex;
    if AMultiSelect then
      VOpenDialog.Options := VOpenDialog.Options + [ofAllowMultiSelect]
    else
      VOpenDialog.Options := VOpenDialog.Options - [ofAllowMultiSelect];
    if VOpenDialog.Execute then
    begin
      if AMultiSelect then
        Result := VOpenDialog.Files.Text
      else
        Result := VOpenDialog.FileName;
    end
    else
      if AAbortIfNotExecute then
        Abort;
  finally
    VOpenDialog.Free;
  end;
end;

function LSSaveDialog(var AFileName: TFileName;
  const ALSSaveDialogType: TLSSaveDialogType; const AInitialDir: string;
  const AFilter: string; const AFilterIndex: Integer;
  const AOverwritePrompt: Boolean; const AAbortIfNotExecute: Boolean;
  const AForceExt: string): Boolean;
var
  VEmptyFileExt: Boolean;
  VSaveDialog: TOpenDialog;
begin
  case ALSSaveDialogType of
    sdtFile: VSaveDialog := TSaveDialog.Create(nil);
    sdtPicture: VSaveDialog := TSavePictureDialog.Create(nil);
  end;
  try
    VSaveDialog.FileName := AFileName;
    if AInitialDir <> '' then
      VSaveDialog.InitialDir := AInitialDir;
    if AFilter <> '' then
      VSaveDialog.Filter := AFilter;
    if AFilterIndex > -1 then
      VSaveDialog.FilterIndex := AFilterIndex;
    if AOverwritePrompt then
      VSaveDialog.Options := VSaveDialog.Options + [ofOverwritePrompt]
    else
      VSaveDialog.Options := VSaveDialog.Options - [ofOverwritePrompt];
    Result := VSaveDialog.Execute;
    if Result then
    begin
      AFileName := VSaveDialog.FileName;
      VEmptyFileExt := ExtractFileExt(AFileName) = '';
      if (AForceExt <> '') and VEmptyFileExt then
        AFileName += '.' + AForceExt;
      if (ALSSaveDialogType = sdtPicture) and VEmptyFileExt then
        AFileName += '.' + TOpenPictureDialog(VSaveDialog).GetFilterExt;
    end
    else
      begin
        AFileName := '';
        if AAbortIfNotExecute then
          Abort;
      end;
  finally
    VSaveDialog.Free;
  end;
end;

function LSSaveDialogS(const ADefFileName: string; const AFilter: string;
  const AForceExt: string; const AFilterIndex: Integer;
  const ALSSaveDialogType: TLSSaveDialogType): string;
var
  VFileName: TFileName = '';
begin
  Result := '';
  VFileName := ADefFileName;
  if LSSaveDialog(VFileName, ALSSaveDialogType, '', AFilter, AFilterIndex, True,
    True, AForceExt) then
    Result := VFileName;
end;

function LSJSONLookupDialog(const ACaption: TCaption; var AText: TJSONStringType;
  const AJSON: TJSONStringType; var AJSONObject: TJSONObject;
  const AWidth: Integer; const AHeight: Integer): Boolean;
var
  VJSONObject: TJSONObject = nil;
  VForm: TLSLookupDialogForm = nil;
begin
  VForm := TLSLookupDialogForm.Create(nil);
  try
    if ACaption <> '' then
      VForm.Caption := ACaption
    else
      VForm.Caption := SLSLookupDialogCaption;
    VForm.FindLabel.Caption := SLSLookupDialogFindLabelCaption;
    VForm.OKButton.Caption := SLSLookupDialogOKButton;
    VForm.CancelButton.Caption := SLSLookupDialogCancelButton;
    VForm.Width := AWidth;
    VForm.Height := AHeight;
    VForm.FindEdit.Text := AText;
    VForm.ItemsListBox.LoadJSON(AJSON);
    Result := VForm.ShowModal = mrOK;
    if Result then
    begin
      VJSONObject := VForm.ItemsListBox.Item;
      Result := Assigned(VJSONObject);
      if Result then
      begin
        AJSONObject := TJSONObject(VJSONObject.Clone);
        AText := VForm.ItemsListBox.GetSelectedText;
      end;
    end;
  finally
    VForm.Free;
  end;
end;

function LSJSONLookupDialog(const ACaption: TCaption; var AText: TJSONStringType;
  const AJSON: TJSONStringType; var AJSONValue: Variant; const AWidth: Integer;
  const AHeight: Integer): Boolean;
var
  VJSONObject: TJSONObject = nil;
begin
  Result := LSJSONLookupDialog(ACaption, AText, AJSON, VJSONObject, AWidth,
    AHeight);
  if Result then
  begin
    AJSONValue := VJSONObject.Value;
    FreeAndNil(VJSONObject);
  end;
end;

end.

