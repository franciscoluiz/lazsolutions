unit MainFrm;

interface

uses
  Windows, Classes, Controls, Forms, Dialogs, StdCtrls, Registry;

type
  TMainForm = class(TForm)
    SendButton: TButton;
    procedure SendButtonClick(Sender: TObject);
  private
    FSendResult: string;
  protected
    procedure DoStart;
    procedure DoStop;
  end;

var
  MainForm: TMainForm;

implementation

uses 
  SysUtils;

{$R *.dfm}

function GetProgramFilesDir: string;
var
  VRegistry: TRegistry;
begin
  VRegistry := TRegistry.Create;
  try
    VRegistry.RootKey := HKEY_LOCAL_MACHINE;
    VRegistry.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion', False);
    Result := VRegistry.ReadString('ProgramFilesDir');
  finally
    VRegistry.Free;
  end;
end;

function GetDOSOutput(ACommandLine: string; AWork: string = 'C:\'): string;
var
  VSecurityAttributes: TSecurityAttributes;
  VStartupInfo: TStartupInfo;
  VProcessInformation: TProcessInformation;
  VStdOutPipeRead, VStdOutPipeWrite: THandle;
  VWasOK: Boolean;
  VBuffer: array[0..255] of AnsiChar;
  VBytesRead: Cardinal;
  VWorkDir: string;
  VHandle: Boolean;
begin
  Result := '';
  with VSecurityAttributes do
  begin
    nLength := SizeOf(VSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(VStdOutPipeRead, VStdOutPipeWrite, @VSecurityAttributes, 0);
  try
    with VStartupInfo do
    begin
      FillChar(VStartupInfo, SizeOf(VStartupInfo), 0);
      cb := SizeOf(VStartupInfo);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := VStdOutPipeWrite;
      hStdError := VStdOutPipeWrite;
    end;
    VWorkDir := AWork;
    VHandle := CreateProcess(nil, PChar('cmd.exe /C ' + ACommandLine),
      nil, nil, True, 0, nil, PChar(VWorkDir), VStartupInfo, VProcessInformation);
    CloseHandle(VStdOutPipeWrite);
    if VHandle then
      try
        repeat
          VWasOK := ReadFile(VStdOutPipeRead, VBuffer, 255, VBytesRead, nil);
          if VBytesRead > 0 then
          begin
            VBuffer[VBytesRead] := #0;
            Result := Result + VBuffer;
          end;
        until not VWasOK or (VBytesRead = 0);
        WaitForSingleObject(VProcessInformation.hProcess, INFINITE);
      finally
        CloseHandle(VProcessInformation.hThread);
        CloseHandle(VProcessInformation.hProcess);
      end;
  finally
    CloseHandle(VStdOutPipeRead);
  end;
end;

function Send: Integer;
begin
  Result := 0;
  with MainForm do
  begin
    TThread.Synchronize(nil, DoStart);
    FSendResult := GetDOSOutput('lssendmail.exe -from="Your name" ' +
      '-to="destination@gmail.com" -subject="Your subject." ' +
      '-message="Your message." -user="example@gmail.com" ' +
      '-password="abcd1234" -host="smtp.gmail.com" -port="465" -ssl="y" ' +
      '-tls="y"', GetProgramFilesDir + '\LazSolutions\LSSendMail\');
    TThread.Synchronize(nil, DoStop);
  end;  
  ExitThread(0);
end;

procedure TMainForm.DoStart;
begin
  SendButton.Enabled := False;
end;

procedure TMainForm.DoStop;
begin
  ShowMessage(FSendResult);
  SendButton.Enabled := True;
end;

procedure TMainForm.SendButtonClick(Sender: TObject);
var
  VID: Cardinal;
begin
  if not FileExists(GetProgramFilesDir +
    '\LazSolutions\LSSendMail\lssendmail.exe') then
    raise Exception.Create(
      'ERROR: LSSendMail not installed. It will be impossible to send an e-mail.');
  VID := 0;
  BeginThread(nil, 0, Addr(Send), nil, 0, VID);
end;

end.
 