unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddAtomButton: TButton;
    DeleteAtomButton: TButton;
    FindAtomButton: TButton;
    AtomNameEdit: TEdit;
    StatusLabel: TLabel;
    procedure AddAtomButtonClick(Sender: TObject);
    procedure DeleteAtomButtonClick(Sender: TObject);
    procedure FindAtomButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSUtils;

{ TMainForm }

procedure TMainForm.AddAtomButtonClick(Sender: TObject);
begin
  LSGlobalAddAtom(AtomNameEdit.Text);
end;

procedure TMainForm.DeleteAtomButtonClick(Sender: TObject);
begin
  LSGlobalDeleteAtom(AtomNameEdit.Text);
end;

procedure TMainForm.FindAtomButtonClick(Sender: TObject);
begin
  if LSGlobalFindAtom(AtomNameEdit.Text) then
    StatusLabel.Caption := 'Atom "' + AtomNameEdit.Text + '" exists.'
  else
    StatusLabel.Caption := 'Atom "' + AtomNameEdit.Text + '" don''t exists.';
end;

end.

