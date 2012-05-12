unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExatractEmailButton: TButton;
    ExatractURLButton: TButton;
    ExtractURLTabSheet: TTabSheet;
    TextWithEmailMemo: TMemo;
    ExtractEmailTabSheet: TTabSheet;
    TextWithURLMemo: TMemo;
    ValidateIPButton: TButton;
    IPEdit: TEdit;
    PutIPLabel: TLabel;
    MainPageControl: TPageControl;
    ValidateIPTabSheet: TTabSheet;
    procedure ExatractURLButtonClick(Sender: TObject);
    procedure ExatractEmailButtonClick(Sender: TObject);
    procedure ValidateIPButtonClick(Sender: TObject);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSRegEx, LSConsts;

{ TMainForm }

procedure TMainForm.ValidateIPButtonClick(Sender: TObject);
begin
  if LSExtractStringUsingRegEx(IPEdit.Text, CLSConstsExtractIPRegEx) <> '' then
    ShowMessage('Valid IP.')
  else
    ShowMessage('Invalid IP.');
end;

procedure TMainForm.ExatractEmailButtonClick(Sender: TObject);
begin
  ShowMessage(LSExtractStringUsingRegEx(TextWithEmailMemo.Text,
    CLSConstsExtractEmailRegEx));
end;

procedure TMainForm.ExatractURLButtonClick(Sender: TObject);
begin
  ShowMessage(LSExtractStringUsingRegEx(TextWithURLMemo.Text,
    CLSConstsExtractURLRegEx));
end;

end.

