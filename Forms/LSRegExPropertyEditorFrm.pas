(*
  LazSolutions, RegEx property editor unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSRegExPropertyEditorFrm;

{$I lazsolutions.inc}

interface

uses
  LSConsts, LSRegEx, LSRegExList, LSJSONPropStorage, LSLazIDEIntf, Forms,
  Controls, StdCtrls, ExtCtrls, Spin, Classes, Dialogs, LCLType;

type

  { TLSRegExPropertyEditorForm }

  TLSRegExPropertyEditorForm = class(TForm)
    ConfigLSJSONPropStorage: TLSJSONPropStorage;
    TipButton: TButton;
    CancelButton: TButton;
    ModifiersCheckGroup: TCheckGroup;
    ExecuteButton: TButton;
    ExpressionEdit: TEdit;
    ExpressionLabel: TLabel;
    RegExGroupBox: TGroupBox;
    MatchLabel: TLabel;
    OKButton: TButton;
    MatchSpinEdit: TSpinEdit;
    StringEdit: TEdit;
    StringLabel: TLabel;
    procedure ExecuteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TipButtonClick(Sender: TObject);
  private
    FRegEx: TLSRegEx;
    procedure UpdateRegEx(var ARegEx: TLSRegEx);
  public
    class function Execute(var ARegEx: TLSRegEx): Boolean;
    class procedure ShowTip;
  end; 

implementation

{$R *.lfm}

{ TLSRegExPropertyEditorForm }

{$HINTS OFF}
procedure TLSRegExPropertyEditorForm.ExecuteButtonClick(Sender: TObject);
var
  VRegEx: TLSRegEx;
begin
  VRegEx := TLSRegEx.Create(FRegEx.Collection);
  try
    UpdateRegEx(VRegEx);
    if Assigned(VRegEx) and (VRegEx.Expression <> '') then
      ShowMessage(LSExtractStringUsingRegEx(StringEdit.Text, VRegEx.Expression,
        VRegEx.Match, VRegEx.Modifiers));
  finally
    VRegEx.Free;
  end;
end;

procedure TLSRegExPropertyEditorForm.FormCreate(Sender: TObject);
begin
  ConfigLSJSONPropStorage.FileName := LSGetExpertsConfigFileName;
end;
{$HINTS ON}

{$HINTS OFF}
procedure TLSRegExPropertyEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F1 then
  begin
    TipButton.Click;
    Key := 0;
  end;
  if Key = VK_F9 then
  begin
    ExecuteButton.Click;
    Key := 0;
  end;
end;
{$HINTS OFF}

procedure TLSRegExPropertyEditorForm.TipButtonClick(Sender: TObject);
begin
  TLSRegExPropertyEditorForm.ShowTip;
end;

procedure TLSRegExPropertyEditorForm.UpdateRegEx(var ARegEx: TLSRegEx);
begin
  ARegEx.Expression := ExpressionEdit.Text;
  ARegEx.Modifiers := [];
  if ModifiersCheckGroup.Checked[0] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierG];
  if ModifiersCheckGroup.Checked[1] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierI];
  if ModifiersCheckGroup.Checked[2] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierM];
  if ModifiersCheckGroup.Checked[3] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierR];
  if ModifiersCheckGroup.Checked[4] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierS];
  if ModifiersCheckGroup.Checked[5] then
    ARegEx.Modifiers := ARegEx.Modifiers + [rmModifierX];
  ARegEx.Match := MatchSpinEdit.Value;
end;

class function TLSRegExPropertyEditorForm.Execute(var ARegEx: TLSRegEx): Boolean;
var
  VIsDummy: Boolean;
  VDummy: TLSRegExs;
begin
  VIsDummy := not Assigned(ARegEx);
  if VIsDummy then
    VDummy := TLSRegExs.Create(nil);
  with Self.Create(nil) do
  try
    ConfigLSJSONPropStorage.Restore;
    if ConfigLSJSONPropStorage.StoredValue['FirstOpening'] = '' then
      Position := poDesktopCenter;
{$IFDEF LCLGtk2}
    Width := 412;
{$ENDIF}
{$IFDEF MSWINDOWS}
    Width := 394;
{$ENDIF}
    if VIsDummy then
    begin
      ARegEx := VDummy.Add;
      Caption := 'RegEx validator';
    end;
    FRegEx := ARegEx;
    CancelButton.Visible := not VIsDummy;
    ExpressionEdit.Text := ARegEx.Expression;
    ModifiersCheckGroup.Checked[0] := rmModifierG in ARegEx.Modifiers;
    ModifiersCheckGroup.Checked[1] := rmModifierI in ARegEx.Modifiers;
    ModifiersCheckGroup.Checked[2] := rmModifierM in ARegEx.Modifiers;
    ModifiersCheckGroup.Checked[3] := rmModifierR in ARegEx.Modifiers;
    ModifiersCheckGroup.Checked[4] := rmModifierS in ARegEx.Modifiers;
    ModifiersCheckGroup.Checked[5] := rmModifierX in ARegEx.Modifiers;
    MatchSpinEdit.Value := ARegEx.Match;
    Result := ShowModal = mrOK;
    if not VIsDummy and Result then
    begin
      UpdateRegEx(ARegEx);
      if not ARegEx.Active then
        ARegEx.Active := MessageDlg('Enable RegRex?', mtConfirmation,
          mbYesNo, 0) = mrYes;
    end;
  finally
    ConfigLSJSONPropStorage.StoredValue['FirstOpening'] := 'false';
    ConfigLSJSONPropStorage.Save;
    if VIsDummy then
      VDummy.Free;
    Free;
  end;
end;

class procedure TLSRegExPropertyEditorForm.ShowTip;
begin
  ShowMessage(
    'Best websites about RegEx' + LineEnding + LineEnding +
    'Wiki:' + LineEnding +
    'http://en.wikipedia.org/wiki/Regular_expression' + LineEnding + LineEnding +
    'RegExp Studio:' + LineEnding +
    'http://regexpstudio.com' + LineEnding + LineEnding +
    'Regular Expression Library:' + LineEnding +
    'http://regexlib.com' + LineEnding + LineEnding +
    'Regular-Expressions.info:' + LineEnding +
    'http://www.regular-expressions.info' + LineEnding + LineEnding +
    'regexpal:' + LineEnding +
    'http://regexpal.com' + LineEnding + LineEnding +
    'Expressões Regulares - Aurelio Marinho Jargas:' + LineEnding +
    'http://aurelio.net/regex');
end;

end.

