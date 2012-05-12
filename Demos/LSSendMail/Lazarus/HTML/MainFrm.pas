unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Buttons, Ipfilebroker, IpHtml,
  Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseBitBtn: TBitBtn;
    IpFileDataProvider: TIpFileDataProvider;
    IpHtmlPanel: TIpHtmlPanel;
    MailBottomPanel: TPanel;
    SendBitBtn: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure SendBitBtnClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSConsts, LSUtils;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
const
  CTempFileName = '~test.html';
var
  VStrTemp: TStringList;
begin
  VStrTemp := TStringList.Create;
  try
    VStrTemp.LoadFromFile('test.html');
    VStrTemp.Text := LSRemoveCid(VStrTemp.Text);
    VStrTemp.SaveToFile(CTempFileName);
    IpHtmlPanel.OpenURL(ExpandLocalHtmlFileName(CTempFileName));
  finally
    DeleteFile(CTempFileName);
    VStrTemp.Free;
  end;
end;

procedure TMainForm.SendBitBtnClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  VSubject: string;
{$ENDIF}
begin
  if not FileExists(CLSSendMailPath) then
    raise Exception.Create(
      'ERROR: LSSendMail not installed. It will be impossible to send an e-mail.');
{$IFDEF MSWINDOWS}
  VSubject := 'Your subject.';
  ShowMessage(LSExecProcess(CLSSendMailPath + ' -from="Your name" ' +
    '-to="destination@gmail.com" -subject="' + Utf8ToAnsi(VSubject) + '" ' +
    '-messagefile="test.html" -attached="image.jpg" -messagetype="html" ' +
    '-user="example@gmail.com" -password="YOURPASSWORD" ' +
    '-host="smtp.gmail.com" -port="465" -ssl="y" -tls="y"'));
{$ENDIF}
{$IFDEF UNIX}
  ShowMessage(LSExecProcess(CLSSendMailPath + ' -from=''Your name'' ' +
    '-to=''destination@gmail.com'' -subject=''Your subject.'' ' +
    '-messagefile=''test.html'' -attached=''image.jpg'' -messagetype=''html'' ' +
    '-user=''example@gmail.com'' -password=''YOURPASSWORD'' ' +
    '-host=''smtp.gmail.com'' -port=''465'' -ssl=''y'' -tls=''y'''));
{$ENDIF}
end;

end.

