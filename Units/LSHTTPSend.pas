(*
  LazSolutions, Network unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSHTTPSend;

{$I lazsolutions.inc}

interface

uses
{$IFDEF LSOPENSSL}
  SSL_OpenSSL, // Use with HTTPS and OpenSSL.
{$ENDIF}
{$IFDEF UNIX}
  BaseUnix,
  Sockets,
{$ENDIF}
{$IFDEF MSWINDOWS}
  WinSock,
{$ENDIF}
{$IFDEF LSLCL}
 {$IFDEF LSGUI}
  LSHTTPSendProxyCfgDlg, LSHTTPSendUPCfgDlg, Graphics,
 {$ENDIF}
  LCLProc,
{$ENDIF}
  LSConsts, LSMessages, LSHashs, HTTPSend, SynaUtil, BlckSock, Classes,
  SysUtils, DOM, DOM_HTML, SAX_HTML, XMLRead, FPJSON, JSONParser;

type
  TLSIPType = (iptLocal, iptExternal);

  TLSHTTPConfigCryptType = (ctMD5, ctSHA1, ctNone);

  ELSShotMethodError = class(Exception);

{ Global function to execute THTTPSend.HTTPMethod. }
function LSShotMethod(var AHTTPSend: THTTPSend; const AMethod, AURL: string;
  const AUseHTTPResultCodeOK: Boolean): Boolean;
{ Get content from HTTP host. }
function LSHTTPGetContent(const AURL: string; const AUTF8: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): string;
{ Delete content from HTTP host. }
function LSHTTPDeleteContent(const AURL: string; const AUTF8: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): string;
{ Get your local or external IP. (extended) }
function LSGetIPEx(var AHTTPSend: THTTPSend; const ALSIPType: TLSIPType): string;
{ Get text from HTTP host. (extended) }
function LSHTTPGetTextEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStrings; const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Delete text from HTTP host. (extended) }
function LSHTTPDeleteTextEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStrings; const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Get DOM HTML interface from HTTP host. (extended) }
function LSHTTPGetHTMLDocumentEx(var AHTTPSend: THTTPSend;
  var AResponse: THTMLDocument; const AURL: string;
  const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Get DOM XML interface from HTTP host. (extended) }
function LSHTTPGetXMLDocumentEx(var AHTTPSend: THTTPSend;
  var AResponse: TXMLDocument; const AURL: string;
  const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Get JSON object from HTTP host. (extended) }
function LSHTTPGetJSONEx(var AHTTPSend: THTTPSend; var AResponse: TJSONData;
  const AURL: string;{$IFDEF LSNEWLAZARUS}const AStrict: Boolean = False;{$ENDIF}
  const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Get stream from HTTP host. (extended) }
function LSHTTPGetBinaryEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStream; const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Get file from HTTP host. (extended) }
function LSHTTPGetFileEx(var AHTTPSend: THTTPSend; const AURL: string;
  AFileName: TFileName = ''; const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{$IFDEF LSLCL}
{ Get picture (GIF/PNG/JPG/BMP...) from HTTP host. (extended) }
function LSHTTPGetPictureEx(var AHTTPSend: THTTPSend; const AURL: string;
  const APicture: TPicture; var AFileExtensions, AMimeType: string;
  const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{$ENDIF}
{ Post a URL data in a HTTP host. (extended) }
function LSHTTPPostURLEx(var AHTTPSend: THTTPSend; const AURL, AURLData: string;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AContentType: string = CLSHTTPSendContentType_x_www_form_urlencoded): Boolean;
{ Post a file in a HTTP host. (extended) }
function LSHTTPPostFileEx(var AHTTPSend: THTTPSend;
  const AURL, AFieldName, AFileName: string; const AData: TStream;
  const AResultData: TStrings;
  const AContentType: ShortString = CLSHTTPSendContentType_octet_string;
  const AUseHTTPResultCodeOK: Boolean = True): Boolean;
{ Put a URL data in a HTTP host. (extended) }
function LSHTTPPutURLEx(var AHTTPSend: THTTPSend; const AURL, AURLData: string;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AContentType: string = CLSHTTPSendContentType_x_www_form_urlencoded): Boolean;
{$IFDEF LSLCL}
{ Get the value of a tag. (extended) }
function LSHTTPGetPartEx(var AHTTPSend: THTTPSend;
  const ASkipTo, AEnd: array of string; const AURL: string;
  const AIgnoreCase: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True): string;
function LSHTTPGetPartEx(var AHTTPSend: THTTPSend;
  const ASkipTo, AEnd: string; const AURL: string;
  const AIgnoreCase: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True): string;{$IFDEF LSINLINE}inline;{$ENDIF}
{$ENDIF}
{ Get your local or external IP. }
function LSGetIP(const ALSIPType: TLSIPType; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): ShortString;
{ Get text from HTTP host. }
function LSHTTPGetText(const AURL: string; const AResponse: TStrings;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Delete text from HTTP host. }
function LSHTTPDeleteText(const AURL: string; const AResponse: TStrings;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get DOM HTML interface from HTTP host. }
function LSHTTPGetHTMLDocument(const AURL: string; var AResponse: THTMLDocument;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get DOM XML interface from HTTP host. }
function LSHTTPGetXMLDocument(const AURL: string; var AResponse: TXMLDocument;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get JSON object from HTTP host. }
function LSHTTPGetJSON(const AURL: string; var AResponse: TJSONData;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get stream from HTTP host. }
function LSHTTPGetBinary(const AURL: string; const AResponse: TStream;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get file from HTTP host. }
function LSHTTPGetFile(const AURL: string; AFileName: TFileName = '';
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{$IFDEF LSLCL}
{ Get picture (GIF/PNG/JPG/BMP...) from HTTP host. }
function LSHTTPGetPicture(const AURL: string; const APicture: TPicture;
  var AFileExtensions, AMimeType: string;
  const AUseHTTPResultCodeOK: Boolean = True; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Get picture (GIF/PNG/JPG/BMP...) from HTTP host. }
function LSHTTPGetPicture(const AURL: string;
  const APicture: TPicture; const AUseHTTPResultCodeOK: Boolean = True;
  const AProxyHost: ShortString = ''; const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{$ENDIF}
{ Post a URL data in a HTTP host. }
function LSHTTPPostURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean = True; const AUserName: ShortString = '';
  const APassword: ShortString = ''; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Post a URL data with cookies in a HTTP host. }
function LSHTTPPostURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AUserName: ShortString = ''; const APassword: ShortString = '';
  const AProxyHost: ShortString = ''; const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Put a URL data in a HTTP host. }
function LSHTTPPutURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean = True; const AUserName: ShortString = '';
  const APassword: ShortString = ''; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Put a URL data with cookies in a HTTP host. }
function LSHTTPPutURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AUserName: ShortString = ''; const APassword: ShortString = '';
  const AProxyHost: ShortString = ''; const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Post a URL data in a HTTP host. (content-type JSON) }
function LSHTTPPostJSONURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean = True; const AUserName: ShortString = '';
  const APassword: ShortString = ''; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Post a URL data with cookies in a HTTP host. (content-type JSON) }
function LSHTTPPostJSONURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AUserName: ShortString = ''; const APassword: ShortString = '';
  const AProxyHost: ShortString = ''; const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Put a URL data in a HTTP host. (content-type JSON) }
function LSHTTPPutJSONURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean = True; const AUserName: ShortString = '';
  const APassword: ShortString = ''; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{ Put a URL data with cookies in a HTTP host. (content-type JSON) }
function LSHTTPPutJSONURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean = True;
  const AUserName: ShortString = ''; const APassword: ShortString = '';
  const AProxyHost: ShortString = ''; const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Post a file in a HTTP host. }
function LSHTTPPostFile(const AURL, AFieldName, AFileName: string;
  const AData: TStream; const AResultData: TStrings;
  const AContentType: ShortString = CLSHTTPSendContentType_octet_string;
  const AUseHTTPResultCodeOK: Boolean = True; const AUserName: ShortString = '';
  const APassword: ShortString = ''; const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = ''; const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): Boolean;
{$IFDEF LSLCL}
{ Get the value of a tag. }
function LSHTTPGetPart(const ASkipTo, AEnd: array of string;
  const AURL: string; const AIgnoreCase: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True;
  const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): string;
function LSHTTPGetPart(const ASkipTo, AEnd: string; const AURL: string;
  const AIgnoreCase: Boolean = True;
  const AUseHTTPResultCodeOK: Boolean = True;
  const AProxyHost: ShortString = '';
  const AProxyPort: ShortString = '';
  const AProxyUser: ShortString = '';
  const AProxyPass: ShortString = ''): string;
{$ENDIF}
{ Generate HTTP Boundary. (see: http://tools.ietf.org/html/rfc1341) }
function LSGenerateBoundary: ShortString;
{ Configure global proxy. }
function LSGlobalProxyDialog(var AHost, APort, AUser, APass: string;
  const ASaveConfig: Boolean): Boolean;
function LSGlobalProxyDialog: Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}
{ Configure global user/password. }
function LSGlobalUPDialog(var AUser, APass: string; const AHost: string;
  const ASaveConfig: Boolean): Boolean;
function LSGlobalUPDialog: Boolean;{$IFDEF LSINLINE}inline;{$ENDIF}

var
  { Configure your global proxy. }
  LSGlobalProxyConfig: record
    ProxyHost,
    ProxyPort,
    ProxyUser,
    ProxyPass: ShortString;
    Active, AutoOpenConfigInput: Boolean;
    ConfigFileName: string;
    CryptType: TLSHTTPConfigCryptType;
  end;

  { Configure your global user/password. }
  LSGlobalUPConfig: record
    User,
    Pass: ShortString;
    Active, AutoOpenConfigInput: Boolean;
    ConfigFileName: string;
    CryptType: TLSHTTPConfigCryptType;
  end;

implementation

procedure LoadGlobalProxyFile;
var
  VProxySettings: TStringList;
begin
  VProxySettings := TStringList.Create;
  try
    if not FileExists(LSGlobalProxyConfig.ConfigFileName) then
    begin
      LSGlobalProxyConfig.ProxyHost := '';
      LSGlobalProxyConfig.ProxyPort := '';
      LSGlobalProxyConfig.ProxyUser := '';
      LSGlobalProxyConfig.ProxyPass := '';
      Exit;
    end;
    VProxySettings.LoadFromFile(LSGlobalProxyConfig.ConfigFileName);
    LSGlobalProxyConfig.ProxyHost :=
      VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGHOST];
    LSGlobalProxyConfig.ProxyPort :=
      VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGPORT];
    LSGlobalProxyConfig.ProxyUser :=
      VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGUSER];
    LSGlobalProxyConfig.ProxyPass :=
      VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGPASS];
  finally
    VProxySettings.Free;
  end;
end;

procedure LoadGlobalUPFile;
var
  VUPSettings: TStringList;
begin
  VUPSettings := TStringList.Create;
  try
    if not FileExists(LSGlobalUPConfig.ConfigFileName) then
    begin
      LSGlobalUPConfig.User := '';
      LSGlobalUPConfig.Pass := '';
      Exit;
    end;
    VUPSettings.LoadFromFile(LSGlobalUPConfig.ConfigFileName);
    LSGlobalUPConfig.User :=
      VUPSettings.Values[CLSHTTPSENDGLOBALUPCONFIGUSER];
    LSGlobalUPConfig.Pass :=
      VUPSettings.Values[CLSHTTPSENDGLOBALUPCONFIGPASS];
  finally
    VUPSettings.Free;
  end;
end;

function CryptPassword(const Pass: string;
  const CryptType: TLSHTTPConfigCryptType): string;
begin
  case CryptType of
    ctMD5: Result := LSMD5(Pass);
    ctSHA1: Result := LSSHA1(Pass);
    ctNone: Result := Pass;
  end;
end;

function NormalizeURL(const URL: string): string;

  function _IsFileName: Boolean;
  var
    I: Integer;
    URLSep: set of Char;
  begin
    I := Length(URL);
    URLSep := [':'] + ['/'] + ['.'];
    while (I > 0) and not (URL[I] in URLSep) do
      Dec(I);
    Result := (I > 0) and (URL[I] = '.');
  end;

var
  C: Char;
  S: string;
  I: Integer;
begin
  Result := URL;
  I := Pos('//', URL);
  if I > 0 then
    I += 2;
  S := Copy(URL, I, MaxInt);
  if (Pos('/', S) <> 0) and _IsFileName then
    Exit;
  if (Result = '') or (Pos('?', Result) <> 0) or (Pos('#', Result) <> 0) or
    (Pos('|', Result) <> 0) then
    Exit;
  C := Result[Length(Result)];
  if C <> '/' then
    Result += '/';
end;

function ExtractFileURL(const URL: string): string;
var
  I: Integer;
begin
  Result := NormalizeURL(URL);
  I := LastDelimiter('/', Result);
  Result := StringReplace(Result, Copy(Result, I + 1, Length(Result) - (I)), '',
    [rfReplaceAll]);
end;

function LSShotMethod(var AHTTPSend: THTTPSend; const AMethod, AURL: string;
  const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  VOldNameValSep: Char;
  VProxyAuthRequired: Boolean;
  VProxyHost, VProxyPort, VProxyUser, VProxyPass, VURL, VOldURL: string;

  function _CheckUnauthorized: Boolean;
  var
    VUser, VPass: string;
  begin
    Result :=
      (AHTTPSend.ResultCode = CLSHTTPSendResultCodeUnauthorized) or
      (Pos('unauthorized', LowerCase(AHTTPSend.Headers.Text)) <> 0);
    if Result then
    begin
      Result := LSGlobalUPConfig.Active and LSGlobalUPConfig.AutoOpenConfigInput;
      if Result then
      begin
        VUser := '';
        VPass := '';
        Result := LSGlobalUPDialog(VUser, VPass, VURL, False);
        if Result then
        begin
          AHTTPSend.UserName := VUser;
          AHTTPSend.Password := VPass;
        end;
      end;
    end;
  end;

begin
  if not Assigned(AHTTPSend) then
    raise ELSShotMethodError.Create('AHTTPSend parameter can''t be nil.');
  if AMethod = '' then
    raise ELSShotMethodError.Create('AMethod parameter can''t be empty.');
  if AURL = '' then
    raise ELSShotMethodError.Create('AURL parameter can''t be empty.');
  if LSGlobalProxyConfig.Active then
  begin
    LoadGlobalProxyFile;
    AHTTPSend.ProxyHost := LSGlobalProxyConfig.ProxyHost;
    AHTTPSend.ProxyPort := LSGlobalProxyConfig.ProxyPort;
    AHTTPSend.ProxyUser := LSGlobalProxyConfig.ProxyUser;
    AHTTPSend.ProxyPass := LSGlobalProxyConfig.ProxyPass;
  end;
  if LSGlobalUPConfig.Active then
  begin
    LoadGlobalUPFile;
    AHTTPSend.UserName := LSGlobalUPConfig.User;
    AHTTPSend.Password := LSGlobalUPConfig.Pass;
  end;
  if ((CompareText(AMethod, CLSHTTPSendPOSTMethod) = 0) or
    (CompareText(AMethod, CLSHTTPSendPUTMethod) = 0)) and
    (AHTTPSend.Document.Size = 0) then
    AHTTPSend.Headers.Insert(0, 'Content-Length: 0');
  // checking proxy...
  repeat
    Result := AHTTPSend.HTTPMethod(AMethod, AURL);
    if AUseHTTPResultCodeOK then
    begin
      Result := AHTTPSend.ResultCode = CLSHTTPSendResultCodeOK;
      if Result then
        Exit;
    end
    else
      Result := True;
    VProxyAuthRequired :=
      (AHTTPSend.ResultCode = CLSHTTPSendResultCodeProxyAuthenticationRequired) or
      (Pos('proxy', LowerCase(AHTTPSend.Headers.Text)) <> 0);
    if VProxyAuthRequired then
    begin
      VProxyAuthRequired := LSGlobalProxyConfig.Active and
        LSGlobalProxyConfig.AutoOpenConfigInput;
      if VProxyAuthRequired then
      begin
        VProxyHost := '';
        VProxyPort := '';
        VProxyUser := '';
        VProxyPass := '';
        VProxyAuthRequired := LSGlobalProxyDialog(VProxyHost, VProxyPort,
          VProxyUser, VProxyPass, False);
        if VProxyAuthRequired then
        begin
          AHTTPSend.ProxyHost := VProxyHost;
          AHTTPSend.ProxyPort := VProxyPort;
          AHTTPSend.ProxyUser := VProxyUser;
          AHTTPSend.ProxyPass := VProxyPass;
        end
        else
          Exit;
      end
      else
        Exit;
    end;
  until not VProxyAuthRequired;
  // checking redirection...
  VURL := AURL;
  VOldNameValSep := AHTTPSend.Headers.NameValueSeparator;
  AHTTPSend.Headers.NameValueSeparator := ':';
  try
    while (AHTTPSend.ResultCode = CLSHTTPSendResultCodeRedirect) or
      (AHTTPSend.ResultCode = CLSHTTPSendResultCodeTemporaryRedirect) or
      _CheckUnauthorized do
    begin
      VOldURL := VURL;
      VURL := Trim(AHTTPSend.Headers.Values['location']);
      if Pos('://', VURL) = 0 then
        VURL := ExtractFileURL(VOldURL) + VURL;
      AHTTPSend.Clear;
      Result := AHTTPSend.HTTPMethod(CLSHTTPSendGETMethod, VURL);
    end;
  finally
    AHTTPSend.Headers.NameValueSeparator := VOldNameValSep;
  end;
  if AUseHTTPResultCodeOK then
    Result := AHTTPSend.ResultCode = CLSHTTPSendResultCodeOK
  else
    Result := True;
end;

function LSHTTPGetContent(const AURL: string; const AUTF8: Boolean;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): string;
var
  VContent: TStrings;
begin
 VContent := TStringList.Create;
 try
   LSHTTPGetText(AURL, VContent, AUseHTTPResultCodeOK, AProxyHost, AProxyPort,
     AProxyUser, AProxyPass);
   if AUTF8 then
     Result := VContent.Text
   else
     Result := Utf8ToAnsi(VContent.Text);
 finally
   VContent.Free;
 end;
end;

function LSHTTPDeleteContent(const AURL: string; const AUTF8: Boolean;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): string;
var
  VContent: TStrings;
begin
 VContent := TStringList.Create;
 try
   LSHTTPDeleteText(AURL, VContent, AUseHTTPResultCodeOK, AProxyHost, AProxyPort,
     AProxyUser, AProxyPass);
   if AUTF8 then
     Result := VContent.Text
   else
     Result := Utf8ToAnsi(VContent.Text);
 finally
   VContent.Free;
 end;
end;

function LSGetIPEx(var AHTTPSend: THTTPSend; const ALSIPType: TLSIPType): string;
{$IFDEF UNIX}
const
  CGDNSADDR = '8.8.8.8';
  CGDNSPORT = 53;
{$ENDIF}
var
  VAttempts: Integer;
  VJSONParser: TJSONParser;
  VJSONObject: TJSONObject;
{$IFDEF UNIX}
  S: string;
  VHostAddr: TSockAddr;
  VLength: Integer;
  VInetSockAddr: TInetSockAddr;
  VSock, VError: LongInt;
  VIPBuf: array[0..255] of Char = #0;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VWSAData: TWSAData;
  VHostEnt: PHostEnt;
  VName: string;
{$ENDIF}
begin
  Result := '';
  case ALSIPType of
    iptLocal:
    begin
{$IFDEF UNIX}
      VError := 0;
      FillChar(VIPBuf, SizeOf(VIPBuf), #0);
      VSock := FpSocket(AF_INET, SOCK_DGRAM, 0);
      VInetSockAddr.family := AF_INET;
      VInetSockAddr.port := htons(CGDNSPORT);
      VInetSockAddr.addr := StrToHostAddr(CGDNSADDR).s_addr;
      if (FpConnect(VSock, @VInetSockAddr, SizeOf(VInetSockAddr)) = 0) then
        try
          VLength := SizeOf(VHostAddr);
          if (FpGetSockName(VSock, @VHostAddr, @VLength) = 0) then
          begin
            S := NetAddrToStr(VHostAddr.sin_addr);
            StrPCopy(PChar(VIPBuf), S);
          end
          else
            VError := SocketError;
        finally
          if (FpClose(VSock) <> 0) then
            VError := SocketError;
        end
      else
        VError := SocketError;
      if (VError <> 0) then
        Result := '127.0.0.1'
      else
        Result := StrPas(VIPBuf);
{$ENDIF}
{$IFDEF MSWINDOWS}
{$HINTS OFF}
      WSAStartup(2, VWSAData);
{$HINTS ON}
      SetLength(VName, 255);
      GetHostName(PChar(VName), 255);
      SetLength(VName, StrLen(PChar(VName)));
      VHostEnt := GetHostByName(PChar(VName));
      with VHostEnt^ do
        Result := Format(CLSHTTPSendFormatIPMask, [Byte(h_addr^[0]),
          Byte(h_addr^[1]), Byte(h_addr^[2]), Byte(h_addr^[3])]);
      WSACleanup;
{$ENDIF}
    end;
    iptExternal:
    begin
      for VAttempts := 0 to CLSHTTPSendAttempts do
      begin
        AHTTPSend.Clear;
        AHTTPSend.Timeout := 3000;
        Result := LSHTTPGetContent(CLSHTTPSendGetIPAPIURL);
        if Result <> '' then
        begin
          VJSONParser := TJSONParser.Create(Result);
          try
            VJSONObject := TJSONObject(VJSONParser.Parse);
            Result := VJSONObject.Strings['externalIP'];
          finally
            VJSONObject.Free;
            VJSONParser.Free;
          end;
        end;
      end;
      if Result = '' then
        raise Exception.Create(SLSHTTPSendGetIPError);
    end;
  end;
end;

function LSHTTPGetTextEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStrings; const AUseHTTPResultCodeOK: Boolean): Boolean;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
    AResponse.LoadFromStream(AHTTPSend.Document);
end;

function LSHTTPDeleteTextEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStrings; const AUseHTTPResultCodeOK: Boolean): Boolean;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendDELETEMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
    AResponse.LoadFromStream(AHTTPSend.Document);
end;

function LSHTTPGetHTMLDocumentEx(var AHTTPSend: THTTPSend;
  var AResponse: THTMLDocument; const AURL: string;
  const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  VStream: TStream;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    VStream := AHTTPSend.Document;
    ReadHTMLFile(AResponse, VStream);
  end;
end;

function LSHTTPGetXMLDocumentEx(var AHTTPSend: THTTPSend;
  var AResponse: TXMLDocument; const AURL: string;
  const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  VStream: TStream;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    VStream := AHTTPSend.Document;
    ReadXMLFile(AResponse, VStream);
  end;
end;

function LSHTTPGetJSONEx(var AHTTPSend: THTTPSend; var AResponse: TJSONData;
  const AURL: string;{$IFDEF LSNEWLAZARUS}const AStrict: Boolean;{$ENDIF}
  const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  VJSONParser: TJSONParser;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    VJSONParser := TJSONParser.Create(AHTTPSend.Document);
    try
{$IFDEF LSNEWLAZARUS}
      VJSONParser.Strict := AStrict;
{$ENDIF}
      AResponse := VJSONParser.Parse;
    finally
      VJSONParser.Free;
    end;
  end;
end;

function LSHTTPGetBinaryEx(var AHTTPSend: THTTPSend; const AURL: string;
  const AResponse: TStream; const AUseHTTPResultCodeOK: Boolean): Boolean;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    AResponse.Seek(0, soFromBeginning);
    AResponse.CopyFrom(AHTTPSend.Document, 0);
  end;
end;

function LSHTTPGetFileEx(var AHTTPSend: THTTPSend; const AURL: string;
  AFileName: TFileName; const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  VFileStream: TFileStream;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    if AFileName = '' then
      AFileName := ExtractFileName(AURL);
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      VFileStream.CopyFrom(AHTTPSend.Document, AHTTPSend.DownloadSize);
    finally
      VFileStream.Free;
    end;
  end;
end;

{$IFDEF LSLCL}
function LSHTTPGetPictureEx(var AHTTPSend: THTTPSend; const AURL: string;
  const APicture: TPicture; var AFileExtensions, AMimeType: string;
  const AUseHTTPResultCodeOK: Boolean): Boolean;
begin
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendGETMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
  begin
    APicture.LoadFromStream(AHTTPSend.Document);
    AFileExtensions := APicture.Graphic.GetFileExtensions;
    AMimeType := APicture.Graphic.MimeType;
  end;
end;
{$ENDIF}

function LSHTTPPostURLEx(var AHTTPSend: THTTPSend; const AURL, AURLData: string;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean;
  const AContentType: string): Boolean;
begin
  WriteStrToStream(AHTTPSend.Document, AURLData);
  AHTTPSend.MimeType := AContentType;
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendPOSTMethod,
    AURL, AUseHTTPResultCodeOK);
  if Result then
    AData.CopyFrom(AHTTPSend.Document, 0);
end;

function LSHTTPPostFileEx(var AHTTPSend: THTTPSend; const AURL, AFieldName,
  AFileName: string; const AData: TStream; const AResultData: TStrings;
  const AContentType: ShortString; const AUseHTTPResultCodeOK: Boolean): Boolean;
var
  S, VBoundary: string;
begin
  VBoundary := LSGenerateBoundary;
  S := '--' + VBoundary + CRLF + 'Content-Disposition: form-data; name="' +
    AFieldName + '";' + ' filename="' + AFileName + '"' + CRLF +
    'Content-Type: ' + AContentType + CRLF + CRLF;
  WriteStrToStream(AHTTPSend.Document, S);
  AHTTPSend.Document.CopyFrom(AData, 0);
  S := CRLF + '--' + VBoundary + '--' + CRLF;
  WriteStrToStream(AHTTPSend.Document, S);
  AHTTPSend.MimeType := 'multipart/form-data; boundary=' + VBoundary;
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendPOSTMethod, AURL,
    AUseHTTPResultCodeOK);
  if Result then
    AResultData.LoadFromStream(AHTTPSend.Document);
end;

function LSHTTPPutURLEx(var AHTTPSend: THTTPSend; const AURL, AURLData: string;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean;
  const AContentType: string): Boolean;
begin
  WriteStrToStream(AHTTPSend.Document, AURLData);
  AHTTPSend.MimeType := AContentType;
  Result := LSShotMethod(AHTTPSend, CLSHTTPSendPUTMethod,
    AURL, AUseHTTPResultCodeOK);
  if Result then
    AData.CopyFrom(AHTTPSend.Document, 0);
end;

{$IFDEF LSLCL}
function LSHTTPGetPartEx(var AHTTPSend: THTTPSend;
  const ASkipTo, AEnd: array of string; const AURL: string;
  const AIgnoreCase: Boolean; const AUseHTTPResultCodeOK: Boolean): string;
var
  S: string;
  VResponse: TStringList;
begin
  VResponse := TStringList.Create;
  try
    LSHTTPGetTextEx(AHTTPSend, AURL, VResponse, AUseHTTPResultCodeOK);
    S := VResponse.GetText;
    Result := GetPart(ASkipTo, AEnd, S, AIgnoreCase, False);
  finally
    VResponse.Free;
  end;
end;
{$ENDIF}

{$IFDEF LSLCL}
function LSHTTPGetPartEx(var AHTTPSend: THTTPSend; const ASkipTo, AEnd: string;
  const AURL: string; const AIgnoreCase: Boolean;
  const AUseHTTPResultCodeOK: Boolean): string;
begin
  Result := LSHTTPGetPartEx(AHTTPSend, [ASkipTo], [AEnd], AURL, AIgnoreCase,
    AUseHTTPResultCodeOK);
end;
{$ENDIF}

function LSGetIP(const ALSIPType: TLSIPType; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): ShortString;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSGetIPEx(VHTTPSend, ALSIPType);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetText(const AURL: string; const AResponse: TStrings;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetTextEx(VHTTPSend, AURL, AResponse, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPDeleteText(const AURL: string; const AResponse: TStrings;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPDeleteTextEx(VHTTPSend, AURL, AResponse, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetHTMLDocument(const AURL: string; var AResponse: THTMLDocument;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetHTMLDocumentEx(VHTTPSend, AResponse, AURL,
      AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetXMLDocument(const AURL: string; var AResponse: TXMLDocument;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetXMLDocumentEx(VHTTPSend, AResponse, AURL,
      AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetJSON(const AURL: string; var AResponse: TJSONData;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetJSONEx(VHTTPSend, AResponse, AURL,
      {$IFDEF LSNEWLAZARUS}False, {$ENDIF} AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetBinary(const AURL: string; const AResponse: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetBinaryEx(VHTTPSend, AURL, AResponse, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetFile(const AURL: string; AFileName: TFileName;
  const AUseHTTPResultCodeOK: Boolean; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetFileEx(VHTTPSend, AURL, AFileName, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

{$IFDEF LSLCL}
function LSHTTPGetPicture(const AURL: string; const APicture: TPicture;
  var AFileExtensions, AMimeType: string; const AUseHTTPResultCodeOK: Boolean;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetPictureEx(VHTTPSend, AURL, APicture, AFileExtensions,
      AMimeType, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetPicture(const AURL: string;
  const APicture: TPicture; const AUseHTTPResultCodeOK: Boolean;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): Boolean;
var
  VFileExtensions: string = '';
  VMimeType: string = '';
begin
  Result := LSHTTPGetPicture(AURL, APicture, VFileExtensions, VMimeType,
    AUseHTTPResultCodeOK, AProxyHost, AProxyPort, AProxyUser, AProxyPass);
end;
{$ENDIF}

function LSHTTPPostURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPPostURLEx(VHTTPSend, AURL, AURLData, AData, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPostURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean;
  const AUserName: ShortString; const APassword: ShortString;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    VHTTPSend.Cookies.AddStrings(ACookies);
    Result := LSHTTPPostURLEx(VHTTPSend, AURL, AURLData, AData, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPutURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPPutURLEx(VHTTPSend, AURL, AURLData, AData, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPutURL(const AURL, AURLData: string; const ACookies: TStrings;
  const AData: TStream; const AUseHTTPResultCodeOK: Boolean;
  const AUserName: ShortString; const APassword: ShortString;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    VHTTPSend.Cookies.AddStrings(ACookies);
    Result := LSHTTPPutURLEx(VHTTPSend, AURL, AURLData, AData, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPostJSONURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPPostURLEx(VHTTPSend, AURL, AURLData, AData,
      AUseHTTPResultCodeOK, CLSHTTPSendContentType_json);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPostJSONURL(const AURL, AURLData: string;
  const ACookies: TStrings; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    VHTTPSend.Cookies.AddStrings(ACookies);
    Result := LSHTTPPostURLEx(VHTTPSend, AURL, AURLData, AData,
      AUseHTTPResultCodeOK, CLSHTTPSendContentType_json);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPutJSONURL(const AURL, AURLData: string; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPPutURLEx(VHTTPSend, AURL, AURLData, AData,
      AUseHTTPResultCodeOK, CLSHTTPSendContentType_json);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPutJSONURL(const AURL, AURLData: string;
  const ACookies: TStrings; const AData: TStream;
  const AUseHTTPResultCodeOK: Boolean; const AUserName: ShortString;
  const APassword: ShortString; const AProxyHost: ShortString;
  const AProxyPort: ShortString; const AProxyUser: ShortString;
  const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    VHTTPSend.Cookies.AddStrings(ACookies);
    Result := LSHTTPPutURLEx(VHTTPSend, AURL, AURLData, AData,
      AUseHTTPResultCodeOK, CLSHTTPSendContentType_json);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPPostFile(const AURL, AFieldName, AFileName: string;
  const AData: TStream; const AResultData: TStrings;
  const AContentType: ShortString; const AUseHTTPResultCodeOK: Boolean;
  const AUserName: ShortString; const APassword: ShortString;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): Boolean;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.UserName := AUserName;
    VHTTPSend.Password := APassword;
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPPostFileEx(VHTTPSend, AURL, AFieldName, AFileName,
      AData, AResultData, AContentType, AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

{$IFDEF LSLCL}
function LSHTTPGetPart(const ASkipTo, AEnd: array of string; const AURL: string;
  const AIgnoreCase: Boolean; const AUseHTTPResultCodeOK: Boolean;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): string;
var
  VHTTPSend: THTTPSend;
begin
  VHTTPSend := THTTPSend.Create;
  try
    VHTTPSend.ProxyHost := AProxyHost;
    VHTTPSend.ProxyPort := AProxyPort;
    VHTTPSend.ProxyUser := AProxyUser;
    VHTTPSend.ProxyPass := AProxyPass;
    Result := LSHTTPGetPartEx(VHTTPSend, ASkipTo, AEnd, AURL, AIgnoreCase,
      AUseHTTPResultCodeOK);
  finally
    VHTTPSend.Free;
  end;
end;

function LSHTTPGetPart(const ASkipTo, AEnd: string; const AURL: string;
  const AIgnoreCase: Boolean; const AUseHTTPResultCodeOK: Boolean;
  const AProxyHost: ShortString; const AProxyPort: ShortString;
  const AProxyUser: ShortString; const AProxyPass: ShortString): string;
begin
  Result := LSHTTPGetPart([ASkipTo], [AEnd], AURL, AIgnoreCase,
    AUseHTTPResultCodeOK, AProxyHost, AProxyPort, AProxyUser, AProxyPass);
end;
{$ENDIF}

function LSGenerateBoundary: ShortString;
begin
  Result := IntToHex(Random(MaxInt), 8) + CLSHTTPSendBoundaryEndPart;
end;

{$IFNDEF LSGUI}
{$HINTS OFF}
{$ENDIF}
function LSGlobalProxyDialog(var AHost, APort, AUser, APass: string;
  const ASaveConfig: Boolean): Boolean;
{$IFDEF LSGUI}
var
  VProxyConfigDir: string;
  VProxySettings: TStringList;
  VLSHTTPSendProxyConfigDialog: TLSHTTPSendProxyConfigDialog;
{$ENDIF}
begin
{$IFDEF LSGUI}
  VLSHTTPSendProxyConfigDialog := TLSHTTPSendProxyConfigDialog.Create(nil);
  try
    VLSHTTPSendProxyConfigDialog.Caption := SLSHTTPSendProxyDlgCaption;
    VLSHTTPSendProxyConfigDialog.ProxyGroupBox.Caption :=
      SLSHTTPSendProxyDlgProxyGBoxCaption;
    VLSHTTPSendProxyConfigDialog.AuthenticationGroupBox.Caption :=
      SLSHTTPSendProxyDlgAuthenticationGBox;
    VLSHTTPSendProxyConfigDialog.HostLabel.Caption :=
      SLSHTTPSendProxyDlgHostLblCaption;
    VLSHTTPSendProxyConfigDialog.PortLabel.Caption :=
      SLSHTTPSendProxyDlgPortLblCaption;
    VLSHTTPSendProxyConfigDialog.UserLabel.Caption :=
      SLSHTTPSendProxyDlgUserLblCaption;
    VLSHTTPSendProxyConfigDialog.PasswordLabel.Caption :=
      SLSHTTPSendProxyDlgPasswordLblCaption;
    VLSHTTPSendProxyConfigDialog.OKButton.Caption :=
      SLSHTTPSendProxyDlgOKBtnCaption;
    VLSHTTPSendProxyConfigDialog.CancelButton.Caption :=
      SLSHTTPSendProxyDlgCancelBtnCaption;
    LoadGlobalProxyFile;
    VLSHTTPSendProxyConfigDialog.HostEdit.Text := LSGlobalProxyConfig.ProxyHost;
    VLSHTTPSendProxyConfigDialog.PortEdit.Text := LSGlobalProxyConfig.ProxyPort;
    VLSHTTPSendProxyConfigDialog.UserEdit.Text := LSGlobalProxyConfig.ProxyUser;
    if LSGlobalProxyConfig.CryptType = ctNone then
      VLSHTTPSendProxyConfigDialog.PasswordEdit.Text :=
        LSGlobalProxyConfig.ProxyPass;
    Result := VLSHTTPSendProxyConfigDialog.ShowModal = 1 { mrOK };
    if Result then
    begin
      AHost := VLSHTTPSendProxyConfigDialog.HostEdit.Text;
      APort := VLSHTTPSendProxyConfigDialog.PortEdit.Text;
      AUser := VLSHTTPSendProxyConfigDialog.UserEdit.Text;
      APass := CryptPassword(VLSHTTPSendProxyConfigDialog.PasswordEdit.Text,
        LSGlobalProxyConfig.CryptType);
      if not ASaveConfig then
        Exit;
      VProxySettings := TStringList.Create;
      try
        VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGHOST] := AHost;
        VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGPORT] := APort;
        VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGUSER] := AUser;
        VProxySettings.Values[CLSHTTPSENDGLOBALPROXYCONFIGPASS] := APass;
        VProxyConfigDir := ExtractFilePath(LSGlobalProxyConfig.ConfigFileName);
        if not DirectoryExists(VProxyConfigDir) then
          MkDir(VProxyConfigDir);
        VProxySettings.SaveToFile(LSGlobalProxyConfig.ConfigFileName);
        LSGlobalProxyConfig.ProxyHost := AHost;
        LSGlobalProxyConfig.ProxyPort := APort;
        LSGlobalProxyConfig.ProxyUser := AUser;
        LSGlobalProxyConfig.ProxyPass := APass;
      finally
        VProxySettings.Free;
      end;
    end;
  finally
    VLSHTTPSendProxyConfigDialog.Free;
  end;
{$ELSE}
  Result := False;
  WriteLn('LSHTTPSend ERROR: Please configure your proxy.');
{$ENDIF}
end;
{$IFNDEF LSGUI}
{$HINTS OFF}
{$ENDIF}

function LSGlobalProxyDialog: Boolean;
var
  H, P, U, PW: string;
begin
  H := '';
  P := '';
  U := '';
  PW := '';
  Result := LSGlobalProxyDialog(H, P, U, PW, True);
end;

{$IFNDEF LSGUI}
{$HINTS OFF}
{$ENDIF}
function LSGlobalUPDialog(var AUser, APass: string; const AHost: string;
  const ASaveConfig: Boolean): Boolean;
{$IFDEF LSGUI}
var
  VUPConfigDir: string;
  VUPSettings: TStringList;
  VLSHTTPSendUPConfigDialog: TLSHTTPSendUPConfigDialog;
{$ENDIF}
begin
{$IFDEF LSGUI}
  VLSHTTPSendUPConfigDialog := TLSHTTPSendUPConfigDialog.Create(nil);
  try
    VLSHTTPSendUPConfigDialog.Caption := SLSHTTPSendUPDlgCaption;
    VLSHTTPSendUPConfigDialog.AuthenticationGroupBox.Caption :=
      SLSHTTPSendUPDlgAuthenticationGBox;
    VLSHTTPSendUPConfigDialog.UserLabel.Caption :=
      SLSHTTPSendUPDlgUserLblCaption;
    VLSHTTPSendUPConfigDialog.PasswordLabel.Caption :=
      SLSHTTPSendUPDlgPasswordLblCaption;
    VLSHTTPSendUPConfigDialog.OKButton.Caption :=
      SLSHTTPSendUPDlgOKBtnCaption;
    VLSHTTPSendUPConfigDialog.CancelButton.Caption :=
      SLSHTTPSendUPDlgCancelBtnCaption;
    VLSHTTPSendUPConfigDialog.HostLabel.Caption := AHost;
    LoadGlobalUPFile;
    VLSHTTPSendUPConfigDialog.UserEdit.Text := LSGlobalUPConfig.User;
    if LSGlobalUPConfig.CryptType = ctNone then
      VLSHTTPSendUPConfigDialog.PasswordEdit.Text := LSGlobalUPConfig.Pass;
    Result := VLSHTTPSendUPConfigDialog.ShowModal = 1 { mrOK };
    if Result then
    begin
      AUser := VLSHTTPSendUPConfigDialog.UserEdit.Text;
      APass := CryptPassword(VLSHTTPSendUPConfigDialog.PasswordEdit.Text,
        LSGlobalUPConfig.CryptType);
      if not ASaveConfig then
        Exit;
      VUPSettings := TStringList.Create;
      try
        VUPSettings.Values[CLSHTTPSENDGLOBALUPCONFIGUSER] := AUser;
        VUPSettings.Values[CLSHTTPSENDGLOBALUPCONFIGPASS] := APass;
        VUPConfigDir := ExtractFilePath(LSGlobalUPConfig.ConfigFileName);
        if not DirectoryExists(VUPConfigDir) then
          MkDir(VUPConfigDir);
        VUPSettings.SaveToFile(LSGlobalUPConfig.ConfigFileName);
        LSGlobalUPConfig.User := AUser;
        LSGlobalUPConfig.Pass := APass;
      finally
        VUPSettings.Free;
      end;
    end;
  finally
    VLSHTTPSendUPConfigDialog.Free;
  end;
{$ELSE}
  Result := False;
  WriteLn('LSHTTPSend ERROR: Please configure your HTTP user/pass.');
{$ENDIF}
end;
{$IFNDEF LSGUI}
{$HINTS OFF}
{$ENDIF}

function LSGlobalUPDialog: Boolean;
var
  U, PW, H: string;
begin
  U := '';
  PW := '';
  H := '';
  Result := LSGlobalUPDialog(U, PW, H, True);
end;

initialization
  LSGlobalProxyConfig.Active := True;
  LSGlobalUPConfig.Active := True;
  LSGlobalProxyConfig.AutoOpenConfigInput := True;
  LSGlobalUPConfig.AutoOpenConfigInput := True;
  LSGlobalProxyConfig.CryptType := ctNone;
  LSGlobalUPConfig.CryptType := ctNone;
  LSGlobalProxyConfig.ConfigFileName := GetAppConfigDir(False);
  LSGlobalUPConfig.ConfigFileName := LSGlobalProxyConfig.ConfigFileName;
  LSGlobalProxyConfig.ConfigFileName += CLSHTTPSENDGLOBALPROXYCONFIGFILENAME;
  LSGlobalUPConfig.ConfigFileName += CLSHTTPSENDGLOBALUPCONFIGFILENAME;

end.

