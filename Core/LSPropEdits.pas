(*
  LazSolutions, Property editors unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSPropEdits;

{$I lazsolutions.inc}

interface

uses
  LSConsts, LSAboutFrm, LSRegExList, LSRegExPropertyEditorFrm, PropEdits,
  GraphPropEdits, TypInfo, ImgList, Classes, SysUtils;

const
  LS_ABOUT_PROP = '(About)';
  LS_TIP_PROP = '(Tip)';

type

  { TLSAboutPropertyEditor }

  TLSAboutPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  { TLSPatternPropertyEditor }

  TLSPatternPropertyEditor = class(TStringPropertyEditor)
  protected
    function GetRegExList: TLSCustomRegExList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

  { TLSExpressionPropertyEditor }

  TLSExpressionPropertyEditor = class(TStringPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TLSRegExTipPropertyEditor }

  TLSRegExTipPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  { TLSImageIndexPropertyEditor }

  TLSImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

  { TLSDateEditDisplayFrmtPropertyEditor }

  TLSDateEditDisplayFrmtPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

implementation

{ TLSAboutPropertyEditor }

function TLSAboutPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

procedure TLSAboutPropertyEditor.Edit;
begin
  TLSAboutForm.Execute;
end;

function TLSAboutPropertyEditor.GetValue: AnsiString;
begin
  Result := LS_ABOUT_PROP;
end;

{ TLSPatternPropertyEditor }

function TLSPatternPropertyEditor.GetRegExList: TLSCustomRegExList;
var
  VObject: TObject;
  VPropInfo: PPropInfo;
  VPersistent: TPersistent;
  VComponent: TComponent absolute VPersistent;
begin
  Result := nil;
  VPersistent := GetComponent(0);
  if not (VPersistent is TComponent) then
    Exit;
  VPropInfo := TypInfo.GetPropInfo(VComponent, 'Patterns');
  if VPropInfo = nil then
    Exit;
  VObject := GetObjectProp(VComponent, VPropInfo);
  if VObject is TLSCustomRegExList then
    Exit(TLSCustomRegExList(VObject));
end;

function TLSPatternPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TLSPatternPropertyEditor.GetValues(AProc: TGetStrProc);
var
  I: Integer;
  VRegExList: TLSCustomRegExList;
begin
  VRegExList := GetRegExList;
  if Assigned(VRegExList) then
    for I := 0 to Pred(VRegExList.Count) do
      AProc(VRegExList.Items[I].Name);
end;

{ TLSExpressionPropertyEditor }

procedure TLSExpressionPropertyEditor.Edit;
var
  VRegEx: TLSRegEx;
begin
  VRegEx := GetComponent(0) as TLSRegEx;
  if TLSRegExPropertyEditorForm.Execute(VRegEx) then
    Modified;
end;

function TLSExpressionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable];
end;

{ TLSRegExTipPropertyEditor }

procedure TLSRegExTipPropertyEditor.Edit;
begin
  TLSRegExPropertyEditorForm.ShowTip;
end;

function TLSRegExTipPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

function TLSRegExTipPropertyEditor.GetValue: AnsiString;
begin
  Result := LS_TIP_PROP;
end;

{ TLSImageIndexPropertyEditor }

function TLSImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  VObject: TObject;
  VPropInfo: PPropInfo;
  VPersistent: TPersistent;
  VComponent: TComponent absolute VPersistent;
begin
  Result := nil;
  VPersistent := GetComponent(0);
  if not (VPersistent is TComponent) then
    Exit;
  VPropInfo := TypInfo.GetPropInfo(VComponent, 'Images');
  if VPropInfo = nil then
    Exit;
  VObject := GetObjectProp(VComponent, VPropInfo);
  if VObject is TCustomImageList then
    Exit(TCustomImageList(VObject));
end;

function TLSDateEditDisplayFrmtPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TLSDateEditDisplayFrmtPropertyEditor.GetValues(AProc: TGetStrProc);
var
  VDefDateFrmt: string;
begin
  VDefDateFrmt := LowerCase(DefaultFormatSettings.ShortDateFormat);
  if VDefDateFrmt <> '' then
    AProc(VDefDateFrmt);
  if CLSDateEditDisplayFrmtMDY <> VDefDateFrmt then
    AProc(CLSDateEditDisplayFrmtMDY);
  if CLSDateEditDisplayFrmtDMY <> VDefDateFrmt then
    AProc(CLSDateEditDisplayFrmtDMY);
  if CLSDateEditDisplayFrmtYMD <> VDefDateFrmt then
    AProc(CLSDateEditDisplayFrmtYMD);
end;

end.

