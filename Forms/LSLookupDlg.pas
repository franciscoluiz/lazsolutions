(*
  LazSolutions, Lookup dialog unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSLookupDlg;

{$I lazsolutions.inc}

interface

uses
  Forms, ExtCtrls, StdCtrls, Buttons, LSControls, Classes;

type

  { TLSLookupDialogForm }

  TLSLookupDialogForm = class(TForm)
    CancelButton: TBitBtn;
    FindEdit: TEdit;
    FindLabel: TLabel;
    BottomPanel: TPanel;
    ItemsListBox: TLSListBox;
    OKButton: TBitBtn;
    TopPanel: TPanel;
    procedure FindEditChange(Sender: TObject);
    procedure FindEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ItemsListBoxDblClick(Sender: TObject);
    procedure ItemsListBoxSelectionChange(Sender: TObject; User: Boolean);
  end;

implementation

{$R *.lfm}

uses
  LSUtils, LCLType;

{ TLSLookupDialogForm }

{$HINTS OFF}
procedure TLSLookupDialogForm.FindEditChange(Sender: TObject);
begin
  if FindEdit.Focused then
    LSFindItemInListBox(TCustomListBox(ItemsListBox), FindEdit.Text,
      False, False);
end;

procedure TLSLookupDialogForm.FindEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
    begin
      if ItemsListBox.ItemIndex > 0 then
        ItemsListBox.ItemIndex := Pred(ItemsListBox.ItemIndex);
      FindEdit.Text := ItemsListBox.GetSelectedText;
      FindEdit.SelectAll;
      Key := VK_UNKNOWN;
    end;
    VK_DOWN:
    begin
      Key := VK_UNKNOWN;
      if ssCtrl in Shift then
      begin
        LSFindItemInListBox(TCustomListBox(ItemsListBox), FindEdit.Text);
        Exit;
      end;
      if ItemsListBox.ItemIndex < Pred(ItemsListBox.Count) then
        ItemsListBox.ItemIndex := Succ(ItemsListBox.ItemIndex);
      FindEdit.Text := ItemsListBox.GetSelectedText;
      FindEdit.SelectAll;
    end;
  end;
end;

procedure TLSLookupDialogForm.ItemsListBoxDblClick(Sender: TObject);
begin
  OKButton.Click;
end;

procedure TLSLookupDialogForm.ItemsListBoxSelectionChange(Sender: TObject;
  User: Boolean);
begin
  if ItemsListBox.Focused then
  begin
    FindEdit.Text := ItemsListBox.GetSelectedText;
    FindEdit.SelectAll;
    FindEdit.SetFocus;
  end;
end;
{$HINTS ON}

end.

