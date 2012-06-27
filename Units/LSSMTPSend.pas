(*
  LazSolutions, SMTP unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSSMTPSend;

{$I lazsolutions.inc}

interface

uses
{$IFDEF LSOPENSSL}
  SSL_OpenSSL,
{$ENDIF}
  LSConsts, LSMessages, SysUtils, Classes, SMTPSend, MimePart, MimeMess,
  SynaChar;

type

  { TLSMessageType }

  TLSMessageType = (mtTXT, mtHTML);

  { TMimeMessEx }

  TMimeMessEx = class(TMimeMess)
  public
    function AddPartText(const Value: TStrings;
      const PartParent: TMimePart): TMimepart;
    function AddPartHTML(const Value: TStrings;
      const PartParent: TMimePart): TMimepart;
  end;

  { TLSSendMail }

  TLSSendMail = class
  private
    FAttached: TStringList;
    FAttempt: Byte;
    FBCCList: TStringList;
    FCCList: TStringList;
    FConfirmReading: Boolean;
    FDebugMode: Boolean;
    FFrom: string;
    FHost: string;
    FMessage: TStringList;
    FMessageType: TLSMessageType;
    FPassword: string;
    FPort: string;
    FPriority: TMessPriority;
    FSSL: Boolean;
    FSubject: string;
    FTLS: Boolean;
    FToList: TStringList;
    FUser: string;
  public
    function Send: string;
    constructor Create;
    destructor Destroy; override;
    property From: string read FFrom write FFrom;
    property ToList: TStringList read FToList write FToList;
    property CCList: TStringList read FCCList write FCCList;
    property BCCList: TStringList read FBCCList write FBCCList;
    property Subject: string read FSubject write FSubject;
    property Message: TStringList read FMessage write FMessage;
    property Attached: TStringList read FAttached write FAttached;
    property Priority: TMessPriority read FPriority write FPriority;
    property ConfirmReading: Boolean read FConfirmReading write FConfirmReading;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property SSL: Boolean read FSSL write FSSL;
    property TLS: Boolean read FTLS write FTLS;
    property Attempt: Byte read FAttempt write FAttempt;
    property MessageType: TLSMessageType read FMessageType write FMessageType;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

  { TLSSendMailConnection }

  TLSSendMailConnection = class
  private
    FAttempt: Byte;
    FCCList: TStrings;
    FFrom: string;
    FMessage: TStringList;
    FSMTP: TSMTPSend;
    FToList: TStrings;
  protected
    function Send: string;
  public
    constructor Create;
    destructor Destroy; override;
    property SMTP: TSMTPSend read FSMTP write FSMTP;
    property From: string read FFrom write FFrom;
    property ToList: TStrings read FToList write FToList;
    property CCList: TStrings read FCCList write FCCList;
    property Message: TStringList read FMessage write FMessage;
    property Attempt: Byte read FAttempt write FAttempt;
  end;

{$IFDEF LSOPENSSL}
{ Check if OpenSSL is available. }
function IsOpenSSLAvailable: Boolean;
{$ENDIF}
{ Send a e-mail (TXT/HTML). }
function LSSendMail(const AFrom, ATo, ASubject, AMessage, APriority, ACC, ABCC,
  AUser, APassword, AHost, APort: string; const AAttached: TStrings;
  const AConfirmReading, ASSL, ATLS: Boolean;
  const AMessageType: TLSMessageType = mtTXT; const AAttempt: Byte = 3;
  const ADebugMode: Boolean = False): string;

implementation

{$IFDEF LSOPENSSL}
function IsOpenSSLAvailable: Boolean;
var
  VSSLOpenSSL: TSSLOpenSSL;
begin
  VSSLOpenSSL := TSSLOpenSSL.Create(nil);
  try
    Result := VSSLOpenSSL.LibVersion <> '';
  finally
    VSSLOpenSSL.Free;
  end;
end;
{$ENDIF}

function LSSendMail(const AFrom, ATo, ASubject, AMessage, APriority, ACC, ABCC,
  AUser, APassword, AHost, APort: string; const AAttached: TStrings;
  const AConfirmReading, ASSL, ATLS: Boolean;
  const AMessageType: TLSMessageType; const AAttempt: Byte;
  const ADebugMode: Boolean): string;
var
  VAttached: TStringList;
  VLSSendMail: TLSSendMail;
begin
  VLSSendMail := TLSSendMail.Create;
  VAttached := TStringList.Create;
  try
    VLSSendMail.From := AFrom;
    ExtractStrings([',', ';'], [' '], PChar(ATo), VLSSendMail.ToList);
    VLSSendMail.Subject := ASubject;
    VLSSendMail.User := AUser;
    VLSSendMail.Password := APassword;
    VLSSendMail.Host := AHost;
    VLSSendMail.Port := APort;
    if AMessage <> '' then
      VLSSendMail.Message.Text := AMessage;
    VAttached.Sorted := True;
    VAttached.Duplicates := dupIgnore;
    if Assigned(AAttached) then
    begin
      VAttached.AddStrings(AAttached);
      VLSSendMail.Attached.AddStrings(VAttached);
    end;
    VLSSendMail.SSL := ASSL;
    VLSSendMail.TLS := ATLS;
    if APriority = 'unknown' then
      VLSSendMail.Priority := MP_unknown
    else
      if APriority = 'low' then
        VLSSendMail.Priority := MP_low
      else
        if APriority = 'normal' then
          VLSSendMail.Priority := MP_normal
        else
          if APriority = 'high' then
            VLSSendMail.Priority := MP_high;
    ExtractStrings([',', ';'], [' '], PChar(ACC), VLSSendMail.CCList);
    ExtractStrings([',', ';'], [' '], PChar(ABCC), VLSSendMail.BCCList);
    VLSSendMail.ConfirmReading := AConfirmReading;
    VLSSendMail.Attempt := AAttempt;
    VLSSendMail.MessageType := AMessageType;
    VLSSendMail.DebugMode := ADebugMode;
    Result := VLSSendMail.Send;
  finally
    VAttached.Free;
    VLSSendMail.Free;
  end;
end;

{ TMimeMessEx }

function TMimeMessEx.AddPartText(const Value: TStrings;
  const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'plain';
    Description := 'Message text';
    CharsetCode := IdealCharsetCoding(Value.Text, TargetCharset, IdealCharsets);
    EncodingCode := ME_7BIT;
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMessEx.AddPartHTML(const Value: TStrings;
  const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'html';
    Description := 'HTML text';
    CharsetCode := IdealCharsetCoding(Value.Text, TargetCharset, IdealCharsets);
    EncodingCode := ME_7BIT;
    EncodePart;
    EncodePartHeader;
  end;
end;

{ TLSSendMail }

constructor TLSSendMail.Create;
begin
  inherited Create;
  FToList := TStringList.Create;
  FCCList := TStringList.Create;
  FBCCList := TStringList.Create;
  FMessage := TStringList.Create;
  FAttached := TStringList.Create;
  FPriority := MP_unknown;
  FConfirmReading := False;
  FDebugMode := False;
end;

destructor TLSSendMail.Destroy;
begin
  FMessage.Free;
  FCCList.Free;
  FBCCList.Free;
  FAttached.Free;
  FToList.Free;
  inherited Destroy;
end;

function TLSSendMail.Send: string;
var
  I: Integer;
  VMimeMessEx: TMimeMessEx;
  VMimePart: TMimePart;
  VLSSendMailConnection: TLSSendMailConnection;
begin
  VMimeMessEx := TMimeMessEx.Create;
  VLSSendMailConnection := TLSSendMailConnection.Create;
  try
    case FMessageType of
      mtTXT:
        begin
          if FAttached.Count > 0 then
            VMimePart := VMimeMessEx.AddPartMultipart('mixed', nil);
          if FMessage.Count > 0 then
          begin
            if FAttached.Count > 0 then
              VMimeMessEx.AddPartText(FMessage, VMimePart)
            else
              VMimePart := VMimeMessEx.AddPartText(FMessage, nil);
          end;
          for I := 0 to Pred(FAttached.Count) do
            VMimeMessEx.AddPartBinaryFromFile(FAttached.Strings[I], VMimePart);
        end;
      mtHTML:
        begin
          if FAttached.Count > 0 then
            VMimePart := VMimeMessEx.AddPartMultipart('related', nil);
          if FMessage.Count > 0 then
          begin
            if FAttached.Count > 0 then
              VMimeMessEx.AddPartHTML(FMessage, VMimePart)
            else
              VMimePart := VMimeMessEx.AddPartHTML(FMessage, nil);
          end;
          for I := 0 to Pred(FAttached.Count) do
            VMimeMessEx.AddPartHTMLBinaryFromFile(FAttached.Strings[I],
              '<' + ExtractFileName(FAttached.Strings[I]) + '>', VMimePart);
        end;
    end;
    VMimeMessEx.Header.XMailer := 'LazSolutions and Synapse framework.';
    VMimeMessEx.Header.ToList.AddStrings(FToList);
    VMimeMessEx.Header.CCList.AddStrings(FCCList);
    VMimeMessEx.Header.From := FFrom;
    VMimeMessEx.Header.Subject := FSubject;
    VMimeMessEx.Header.Priority := FPriority;
    if FConfirmReading then
      VMimeMessEx.Header.CustomHeaders.Add(CLSSMTPSendDispNotificationTo + FUser);
    VMimeMessEx.EncodeMessage;
    if FDebugMode then
    begin
      Result := VMimeMessEx.Lines.Text;
      Exit;
    end;
    VLSSendMailConnection.From := FFrom;
    VLSSendMailConnection.ToList.AddStrings(FToList);
    VLSSendMailConnection.CCList.AddStrings(FCCList);
    VLSSendMailConnection.CCList.AddStrings(FBCCList);
    VLSSendMailConnection.Message.AddStrings(VMimeMessEx.Lines);
    VLSSendMailConnection.Attempt := FAttempt;
    VLSSendMailConnection.SMTP.UserName := FUser;
    VLSSendMailConnection.SMTP.Password := FPassword;
    VLSSendMailConnection.SMTP.TargetHost := FHost;
    VLSSendMailConnection.SMTP.TargetPort := FPort;
    VLSSendMailConnection.SMTP.FullSSL := FSSL;
    VLSSendMailConnection.SMTP.AutoTLS := FTLS;
    Result := VLSSendMailConnection.Send;
  finally
    VMimeMessEx.Free;
    VLSSendMailConnection.Free;
  end;
end;

{ TLSSendMailConnection }

constructor TLSSendMailConnection.Create;
begin
  inherited Create;
  FSMTP := TSMTPSend.Create;
  FToList := TStringList.Create;
  FMessage := TStringList.Create;
  FCCList := TStringList.Create;
  FFrom := '';
  FAttempt := 3;
end;

destructor TLSSendMailConnection.Destroy;
begin
  FMessage.Free;
  FCCList.Free;
  FToList.Free;
  FSMTP.Free;
  inherited Destroy;
end;

function TLSSendMailConnection.Send: string;

  function _FormatErrorMsg(const AMsg: string): string;
  begin
    Result := Format(SLSSMTPSendError, [AMsg, FSMTP.EnhCodeString,
      FSMTP.FullResult.Text]);
  end;

var
  I, VAttempt: Integer;
begin
  Result := '';
  for VAttempt := 1 to FAttempt do
  begin
    if FSMTP.Login then
      Break;
    if VAttempt >= FAttempt then
    begin
      Result := _FormatErrorMsg('Login');
      Exit;
    end;
  end;
  for VAttempt := 1 to FAttempt do
  begin
    if FSMTP.MailFrom(FFrom, Length(FFrom)) then
      Break;
    if VAttempt >= FAttempt then
    begin
      Result := _FormatErrorMsg('MailFrom');
      Exit;
    end;
  end;
  for I := 0 to Pred(FToList.Count) do
    for VAttempt := 1 to FAttempt do
    begin
      if FSMTP.MailTo(FToList.Strings[I]) then
        Break;
      if VAttempt >= FAttempt then
      begin
        Result := _FormatErrorMsg('MailTo');
        Exit;
      end;
    end;
  for I := 0 to Pred(FCCList.Count) do
    for VAttempt := 1 to FAttempt do
    begin
      if FSMTP.MailTo(FCCList.Strings[I]) then
        Break;
      if VAttempt >= FAttempt then
      begin
        Result := _FormatErrorMsg('MailCC');
        Exit;
      end;
    end;
  for VAttempt := 1 to FAttempt do
  begin
    if FSMTP.MailData(FMessage) then
      Break;
    if VAttempt >= FAttempt then
    begin
      Result := _FormatErrorMsg('MailData');
      Exit;
    end;
  end;
  for VAttempt := 1 to FAttempt do
  begin
    if FSMTP.Logout then
      Break;
    if VAttempt >= FAttempt then
    begin
      Result := _FormatErrorMsg('Logout');
      Exit;
    end;
  end;
  Result := SLSSMTPSendEmailSentSuccessfully;
  FSMTP.Sock.CloseSocket;
end;

end.

