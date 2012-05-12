unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    CalcHashButton: TButton;
    PassGenButton: TButton;
    ChackPasswordButton: TButton;
    CalcSumButton: TButton;
    PassGenEdit: TEdit;
    PasswordEdit: TEdit;
    SumEdit: TEdit;
    HashEdit: TEdit;
    SumHashTypeRadioGroup: TRadioGroup;
    MainPageControl: TPageControl;
    MD5SumSHA1SumTabSheet: TTabSheet;
    PasswordTabSheet: TTabSheet;
    PasswordGeneratorTabSheet1: TTabSheet;
    MD5SHA1TabSheet: TTabSheet;
    HashTypeRadioGroup: TRadioGroup;
    procedure CalcHashButtonClick(Sender: TObject);
    procedure ChackPasswordButtonClick(Sender: TObject);
    procedure CalcSumButtonClick(Sender: TObject);
    procedure HashTypeRadioGroupClick(Sender: TObject);
    procedure PassGenButtonClick(Sender: TObject);
    procedure SumHashTypeRadioGroupClick(Sender: TObject);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSHashs, LSDialogs;

{ TMainForm }

procedure TMainForm.SumHashTypeRadioGroupClick(Sender: TObject);
begin
  SumEdit.Clear;
end;

procedure TMainForm.CalcSumButtonClick(Sender: TObject);
begin
  case SumHashTypeRadioGroup.ItemIndex of
    0: SumEdit.Text := LSMD5Sum(LSOpenDialog);
    1: SumEdit.Text := LSSHA1Sum(LSOpenDialog);
  end;
end;

procedure TMainForm.HashTypeRadioGroupClick(Sender: TObject);
begin
  HashEdit.Clear;
end;

procedure TMainForm.PassGenButtonClick(Sender: TObject);
begin
  PassGenEdit.Text := LSPasswordGenerator;
end;

procedure TMainForm.ChackPasswordButtonClick(Sender: TObject);
begin
  if LSMD5(PasswordEdit.Text) = 'bde934a7331cd706882318693be7bc67' then
    ShowMessage('Valid password.')
  else
    ShowMessage('Invalid password.');
end;

procedure TMainForm.CalcHashButtonClick(Sender: TObject);
begin
  case HashTypeRadioGroup.ItemIndex of
    0: HashEdit.Text := LSMD5(HashEdit.Text);
    1: HashEdit.Text := LSSHA1(HashEdit.Text);
  end;
end;

end.

