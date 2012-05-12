unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Buttons, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    GetBitBtn: TBitBtn;
    procedure GetBitBtnClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSHTTPSend;

{ TMainForm }

procedure TMainForm.GetBitBtnClick(Sender: TObject);
const
  CLSIPType: array[0..1] of TLSIPType = (iptLocal, iptExternal);
  CIPTypeMessage: array[0..1] of string = ('Local IP:', 'External IP:');
var
  I: Integer;
begin
  for I := 0 to 1 do
    ShowMessage(CIPTypeMessage[I] + ' ' + LSGetIP(CLSIPType[I]));
end;

end.

