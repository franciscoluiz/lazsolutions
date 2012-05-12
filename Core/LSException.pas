(*
  LazSolutions, Exception unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSException;

{$I lazsolutions.inc}

interface

uses
  LSMessages, LSExceptionUtils, LSExceptionDlg, LSConsts, LSGraphics, LSUtils,
  LSSMTPSend, LSZCompress, LCLType, Classes, Forms, Clipbrd, SysUtils, Controls,
  Menus, Graphics;

type

  { TLSExceptionBtns }

  TLSExceptionBtns = set of (ebCopy, ebTerminate);

  { TLSException }

  TLSException = class
  private
    FFrom: string;
    FHost: string;
    FPassword: string;
    FPort: string;
    FSendMail: Boolean;
    FShowCallStack: Boolean;
    FShowBtnCaptions: Boolean;
    FLSExceptionBtns: TLSExceptionBtns;
    FLSExceptionDialog: TLSExceptionDialog;
    FSendMailJPEGFileName: string;
    FSSL: Boolean;
    FTLS: Boolean;
    FTo_: string;
    FUser: string;
    FUUID: ShortString;
    FIgnoredMsgsDir: string;
    FIgnoredMsgsFileName: string;
    FMessage: string;
    FStoredMessage: string;
    FPrintScreenNarrow: Double;
    FCompressionQuality: TJPEGQualityRange;
    FIgnoredMsgsList: TStringList;
    function LoadIgnoredMsgs: Boolean;
    procedure SaveIgnoredMsgs;
    function IsIgnoredMsg: Boolean;
  protected
    procedure DeleteSendMailJPEGFile;
    function InternalShow(const AMsg: string): Integer;
    procedure DoCopyActionClick(Sender: TObject); virtual;
    procedure DoEmailActionClick(Sender: TObject); virtual;
    procedure DoTerminateActionClick(Sender: TObject); virtual;
    property ShowCallStack: Boolean read FShowCallStack write FShowCallStack;
    property ShowBtnCaptions: Boolean read FShowBtnCaptions write FShowBtnCaptions;
    property LSExceptionBtns: TLSExceptionBtns
      read FLSExceptionBtns write FLSExceptionBtns;
    property SendMail: Boolean read FSendMail write FSendMail;
    property From: string read FFrom write FFrom;
    property To_: string read FTo_ write FTo_;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property SSL: Boolean read FSSL write FSSL;
    property TLS: Boolean read FTLS write FTLS;
  public
    procedure Init; virtual;
    procedure Finit; virtual;
    class procedure Register(const AShowCallStack: Boolean = True;
      const AShowBtnCaptions: Boolean = True;
      const ALSExceptionBtns: TLSExceptionBtns = [ebCopy, ebTerminate];
      AIgnoredMsgsFileName: TFileName = '';
      const APrintScreenNarrow: Double = 1.4;
      const ACompressionQuality: TJPEGQualityRange = 40); virtual;
    class procedure Register(const ASendEmail: Boolean; const AFrom, ATo,
      AUser, APassword, AHost, APort: string; const ASSL, ATLS: Boolean;
      const AShowCallStack: Boolean = True;
      const AShowBtnCaptions: Boolean = True;
      const ALSExceptionBtns: TLSExceptionBtns = [ebCopy, ebTerminate];
      AIgnoredMsgsFileName: TFileName = '';
      const APrintScreenNarrow: Double = 1.4;
      const ACompressionQuality: TJPEGQualityRange = 40); virtual;
    class procedure UnRegister; virtual;
    class function Instance: TLSException;
    class procedure ShowException(const AMsg: string);
    procedure OnExceptionHandler(ASender: TObject; AException: Exception);
  end;

implementation

var
  _LSException: TLSException;

function FormatMessage(const AMsg, ACallStack: string): string;
begin
  Result := Format(SLSExceptionDlgMsg,
    [AMsg, FormatDateTime(CLSConstsDateFormat +
    ' ' + CLSConstsTimeFormat, Now), ACallStack]);
end;

{ TLSException }

procedure TLSException.Init;
begin
  if FSendMail then
  begin
    if FIgnoredMsgsFileName = '' then
    begin
      FIgnoredMsgsDir := GetUserDir + '.' + ApplicationName + DirectorySeparator;
      FIgnoredMsgsFileName := FIgnoredMsgsDir + '.lsexception';
    end;
    FIgnoredMsgsList := TStringList.Create;
  end;
  FLSExceptionDialog := TLSExceptionDialog.Create(nil);
  with FLSExceptionDialog do
  begin
    Caption := SLSExceptionDlgCaption;
    CopyAction.OnExecute := @DoCopyActionClick;
    EmailAction.OnExecute := @DoEmailActionClick;
    TerminateAction.OnExecute := @DoTerminateActionClick;
    CopyAction.Caption := SLSExceptionDlgCopyActCaption;
    EmailAction.Caption := SLSExceptionDlgEmailActCaption;
    TerminateAction.Caption := SLSExceptionDlgTerminateActCaption;
    CloseBitBtn.Caption := SLSExceptionDlgCloseActCaption;
    CopyAction.Hint := SLSExceptionDlgCopyActHint;
    EmailAction.Hint := SLSExceptionDlgEmailActHint;
    TerminateAction.Hint := SLSExceptionDlgTerminateActHint;
    CloseBitBtn.Hint := SLSExceptionDlgCloseActHint;
    if Assigned(Application.MainForm) then
      ShowHint := Application.MainForm.ShowHint
    else
      ShowHint := True;
  end;
end;

procedure TLSException.Finit;
begin
  FLSExceptionDialog.Free;
  if Assigned(FIgnoredMsgsList) then;
    FIgnoredMsgsList.Free;
end;

class procedure TLSException.Register(const AShowCallStack: Boolean;
  const AShowBtnCaptions: Boolean; const ALSExceptionBtns: TLSExceptionBtns;
  AIgnoredMsgsFileName: TFileName; const APrintScreenNarrow: Double;
  const ACompressionQuality: TJPEGQualityRange);
var
  VInstance: TLSException;
begin
  VInstance := TLSException.Instance;
  VInstance.FShowCallStack := AShowCallStack;
  VInstance.FShowBtnCaptions := AShowBtnCaptions;
  VInstance.FLSExceptionBtns := ALSExceptionBtns;
  VInstance.FIgnoredMsgsFileName := AIgnoredMsgsFileName;
  VInstance.FPrintScreenNarrow := APrintScreenNarrow;
  VInstance.FCompressionQuality := ACompressionQuality;
  Application.Flags := Application.Flags + [AppNoExceptionMessages];
  Application.AddOnExceptionHandler(@VInstance.OnExceptionHandler);
  VInstance.Init;
end;

class procedure TLSException.Register(const ASendEmail: Boolean; const AFrom,
  ATo, AUser, APassword, AHost, APort: string; const ASSL, ATLS: Boolean;
  const AShowCallStack: Boolean; const AShowBtnCaptions: Boolean;
  const ALSExceptionBtns: TLSExceptionBtns; AIgnoredMsgsFileName: TFileName;
  const APrintScreenNarrow: Double; const ACompressionQuality: TJPEGQualityRange
  );
var
  VInstance: TLSException;
begin
  VInstance := TLSException.Instance;
  if Assigned(VInstance.Instance) then
  begin
    VInstance.FSendMail := ASendEmail;
    VInstance.FFrom := AFrom;
    VInstance.FTo_ := ATo;
    VInstance.FUser := AUser;
    VInstance.FPassword := APassword;
    VInstance.FHost := AHost;
    VInstance.FPort := APort;
    VInstance.FSSL := ASSL;
    VInstance.FTLS := ATLS;
    VInstance.Register(AShowCallStack, AShowBtnCaptions, ALSExceptionBtns,
      AIgnoredMsgsFileName, APrintScreenNarrow, ACompressionQuality);
  end;
end;

class procedure TLSException.UnRegister;
var
  VInstance: TLSException;
begin
  VInstance := TLSException.Instance;
  if Assigned(VInstance.Instance) then
  begin
    VInstance.Finit;
    VInstance.Free;
  end;
end;

class function TLSException.Instance: TLSException;
begin
  if not Assigned(_LSException) then
    _LSException := TLSException.Create;
  Result := _LSException;
end;

function TLSException.LoadIgnoredMsgs: Boolean;
begin
  Result := FileExists(FIgnoredMsgsFileName);
  if Result then
    FIgnoredMsgsList.LoadFromFile(FIgnoredMsgsFileName);
end;

procedure TLSException.SaveIgnoredMsgs;
begin
  if not DirectoryExists(FIgnoredMsgsDir) then
    MkDir(FIgnoredMsgsDir);
  FIgnoredMsgsList.SaveToFile(FIgnoredMsgsFileName);
end;

function TLSException.IsIgnoredMsg: Boolean;
begin
  if LoadIgnoredMsgs then
    Result := FIgnoredMsgsList.IndexOf(FStoredMessage) > -1
  else
    Result := False;
end;

procedure TLSException.DeleteSendMailJPEGFile;
begin
  if FileExists(FSendMailJPEGFileName) then
    DeleteFile(FSendMailJPEGFileName);
end;

function TLSException.InternalShow(const AMsg: string): Integer;
var
  VIsIgnoredMsg: Boolean;
begin
  FStoredMessage := LSZCompressString(AMsg);
  VIsIgnoredMsg := IsIgnoredMsg;
  if FSendMail and not VIsIgnoredMsg then
  begin
    FUUID := LSUUID;
    FSendMailJPEGFileName := GetTempDir + FUUID + '.jpg';
    LSPrintScreenJPEG(FSendMailJPEGFileName, FLSExceptionDialog.Handle,
      FPrintScreenNarrow, FCompressionQuality);
  end;
  with FLSExceptionDialog do
  begin
    CopyAction.Visible := ebCopy in FLSExceptionBtns;
    EmailAction.Visible := FSendMail and not VIsIgnoredMsg;
    TerminateAction.Visible := ebTerminate in FLSExceptionBtns;
    if FShowCallStack then
      FMessage := FormatMessage(AMsg, 'Stack trace: ' + LineEnding +
        '----------------------------------------' + LineEnding +
        LSDumpExceptionCallStack)
    else
      FMessage := TrimRight(FormatMessage(AMsg, ''));
    MessageMemo.Text := FMessage;
    MessageMemo.PopupMenu := TPopupMenu.Create(MessageMemo);
    Result := ShowModal;
    if FSendMail then
      DeleteSendMailJPEGFile;
  end;
end;

{$HINTS OFF}
procedure TLSException.DoCopyActionClick(Sender: TObject);
begin
  Clipboard.AsText := FMessage;
end;

procedure TLSException.DoEmailActionClick(Sender: TObject);
var
  VAttached: TStringList;
  VEmailSentResult: string;
begin
  Screen.Cursor := crHourGlass;
  VAttached := TStringList.Create;
  try
    if FSendMail then
    begin
      VAttached.Text := FSendMailJPEGFileName;
      VEmailSentResult := LSSendMail(FFrom, FTo_, FUUID, FMessage, 'normal', '',
      '', FUser, FPassword, FHost, FPort, VAttached, False, FSSL, FTLS);
    end;
  finally
    VAttached.Free;
    if VEmailSentResult = SLSSMTPSendEmailSentSuccessfully then
    begin
      LoadIgnoredMsgs;
      FIgnoredMsgsList.Add(FStoredMessage);
      SaveIgnoredMsgs;
      FLSExceptionDialog.EmailAction.Visible := False;
      DeleteSendMailJPEGFile;
    end;
    Screen.Cursor := crDefault;
    Application.MessageBox(PChar(VEmailSentResult), PChar(Application.Title),
      MB_ICONINFORMATION + MB_OK);
  end;
end;

procedure TLSException.DoTerminateActionClick(Sender: TObject);
begin
  if Application.MessageBox(PChar(SLSExceptionDlgTerminateConfirmMsg),
    PChar(SLSExceptionDlgTerminateConfirmCaption), MB_ICONQUESTION + MB_YESNO) =
    mrYes then
  begin
    if FSendMail then
      DeleteSendMailJPEGFile;
    Application.Terminate;
  end;
end;

procedure TLSException.OnExceptionHandler(ASender: TObject; AException: Exception);
begin
  TLSException.Instance.InternalShow(AException.Message);
end;
{$HINTS ON}

class procedure TLSException.ShowException(const AMsg: string);
begin
  raise Exception.Create(AMsg);
end;

initialization

finalization
  TLSException.UnRegister;

end.

