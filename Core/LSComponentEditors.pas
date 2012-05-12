(*
  LazSolutions, Component editors unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSComponentEditors;

{$I lazsolutions.inc}

interface

uses
{$IFNDEF LSNEWLAZARUS}
  Classes,
{$ENDIF}
  LSAboutFrm, LSRegExList, LSControls, LSGrids, LSRegExPropertyEditorFrm,
  ComponentEditors, PropEdits, MaskPropEdit, CheckListboxEditorDlg,
  LazStringGridEdit, CheckLst, Controls, Forms, Grids;

const
  LS_ABOUT_COMPONENT_EDITOR = 'About ...';
  LS_REGEX_COMPONENT_EDITOR = 'LSRegExList Editor ...';
  LS_TIP_COMPONENT_EDITOR = 'Tip ...';

type

  { TLSComponentEditor }

  TLSComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor; virtual;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TLSDefaultComponentEditor }

  TLSDefaultComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DoShowEditor; virtual;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TLSRegExListComponentEditor }

  TLSRegExListComponentEditor = class(TLSComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TLSCheckListBoxComponentEditor }

  TLSCheckListBoxComponentEditor = class(TCheckListBoxComponentEditor)
  private
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TLSMaskEditEditor }

  TLSMaskEditEditor = class(TMaskEditEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TLSStringGridComponentEditor }

  TLSStringGridComponentEditor = class(TStringGridComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$IFNDEF LSNEWLAZARUS}
procedure LSRegisterComponentEditor(AComponentClasses: array of TComponentClass;
  AComponentEditor: TComponentEditorClass); overload;
{$ENDIF}

implementation

{$IFNDEF LSNEWLAZARUS}
procedure LSRegisterComponentEditor(AComponentClasses: array of TComponentClass;
  AComponentEditor: TComponentEditorClass);
var
  I: Integer;
begin
  for I := 0 to High(AComponentClasses) do
    RegisterComponentEditor(AComponentClasses[I], AComponentEditor);
end;
{$ENDIF}

function EditStringGrid(AStringGrid: TStringGrid): Boolean;
var
  VStringGridEditorDlg: TStringGridEditorDlg;
begin
  VStringGridEditorDlg := TStringGridEditorDlg.Create(Application);
  try
    VStringGridEditorDlg.LoadFromGrid(AStringGrid);
    if VStringGridEditorDlg.ShowModal = mrOk then
      VStringGridEditorDlg.SaveToGrid;
    Result := VStringGridEditorDlg.Modified;
  finally
    VStringGridEditorDlg.Free;
  end;
end;

{ TLSDefaultComponentEditor }

procedure TLSDefaultComponentEditor.DoShowEditor;
begin
  TLSAboutForm.Execute;
end;

{$HINTS OFF}
procedure TLSDefaultComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    DoShowEditor
  else
    inherited;
end;
{$HINTS ON}

{$HINTS OFF}
function TLSDefaultComponentEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := LS_ABOUT_COMPONENT_EDITOR
  else
    Result := inherited;
end;
{$HINTS ON}

function TLSDefaultComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

{ TLSComponentEditor }

procedure TLSComponentEditor.DoShowEditor;
begin
  TLSAboutForm.Execute;
end;

{$HINTS OFF}
procedure TLSComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  DoShowEditor;
end;
{$HINTS ON}

{$HINTS OFF}
function TLSComponentEditor.GetVerb(AIndex: Integer): string;
begin
  Result := LS_ABOUT_COMPONENT_EDITOR;
end;
{$HINTS ON}

function TLSComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TLSRegExListComponentEditor }

procedure TLSRegExListComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VHook: TPropertyEditorHook;
  VComponent: TLSRegExList;
begin
  case AIndex of
    0:
      begin
        GetHook(VHook);
        VComponent := GetComponent as TLSRegExList;
        EditCollection(VComponent, VComponent.Items, 'Items');
        if Assigned(VHook) then
          VHook.Modified(Self);
      end;
    1: inherited;
    2: TLSRegExPropertyEditorForm.ShowTip;
  end;
end;

function TLSRegExListComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := LS_REGEX_COMPONENT_EDITOR;
    1: Result := inherited;
    2: Result := LS_TIP_COMPONENT_EDITOR;
  end;
end;

function TLSRegExListComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TLSCheckListBoxComponentEditor }

procedure TLSCheckListBoxComponentEditor.DoShowEditor;
var
  VDlg: TCheckListBoxEditorDlg;
begin
  VDlg := TCheckListBoxEditorDlg.Create(nil);
  try
    if GetComponent is TLSCustomCheckListBox then
    begin
      VDlg.ACheck := TCheckListBox(GetComponent);
      if not HasHook then
        Exit;
      AssignCheckList(VDlg.FCheck, VDlg.aCheck);
      if VDlg.ShowModal = mrOK then
      begin
        AssignCheckList(VDlg.aCheck, VDlg.FCheck);
        Modified;
      end;
      if VDlg.Modified then
        Modified;
    end;
  finally
    VDlg.Free;
  end;
end;

procedure TLSCheckListBoxComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: DoShowEditor;
    1: TLSAboutForm.Execute;
  end;
end;

function TLSCheckListBoxComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited;
    1: Result := LS_ABOUT_COMPONENT_EDITOR;
  end;
end;

function TLSCheckListBoxComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TLSMaskEditEditor }

procedure TLSMaskEditEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: inherited;
    1: TLSAboutForm.Execute;
  end;
end;

function TLSMaskEditEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited;
    1: Result := LS_ABOUT_COMPONENT_EDITOR;
  end;
end;

function TLSMaskEditEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TLSStringGridComponentEditor }

procedure TLSStringGridComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VComponent: TComponent;
  VHook: TPropertyEditorHook;
begin
  case AIndex of
    0:
      begin
        GetHook(VHook);
        VComponent := GetComponent;
        if VComponent is TLSStringGrid then
          if EditStringGrid(TStringGrid(VComponent)) then
            if Assigned(VHook) then
              VHook.Modified(Self);
      end;
    1: TLSAboutForm.Execute;
  end;
end;

function TLSStringGridComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited;
    1: Result := LS_ABOUT_COMPONENT_EDITOR;
  end;
end;

function TLSStringGridComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

