unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, StdCtrls, SysUtils, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    GenerateButton: TButton;
    CaptchaTypeRadioGroup: TRadioGroup;
    ValidateButton: TButton;
    CodeEdit: TEdit;
    CaptchaImage: TImage;
    MainPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure ValidateButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;
  Code: ShortString = 'CODE';

implementation

{$R *.lfm}

uses
  LSCaptcha;

{ TMainForm }

procedure TMainForm.GenerateButtonClick(Sender: TObject);
const
  CCaptchaType: array[Boolean] of TLSCaptchaType = (ctNoise, ctLine);
var
  VLSFPMemoryImage: TLSFPMemoryImage;
begin
  if not LSFontExists(ftLiberationSerifRegularTTF) then
    Exit;
  VLSFPMemoryImage := TLSFPMemoryImage.Create;
  try
    Code := LSCaptchaGenerator(VLSFPMemoryImage,
      CCaptchaType[CaptchaTypeRadioGroup.ItemIndex = 1]);
    CaptchaImage.Picture.Assign(VLSFPMemoryImage);
  finally
    VLSFPMemoryImage.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
{  CaptchaImage.Width := 90;
  CaptchaImage.Height := 30;
  CaptchaImage.Stretch := True;}
end;

procedure TMainForm.ValidateButtonClick(Sender: TObject);
begin
  if SameText(Code, CodeEdit.Text) then
    ShowMessage('Valid Captcha! :)')
  else
    ShowMessage('Invalid Captcha! :(');
end;

end.

