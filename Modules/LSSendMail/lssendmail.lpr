(*
  LazSolutions, Send mail module
  Copyright (C) 2010-2012 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program lssendmail;

{$I lssendmail.inc}

uses
{$IFDEF HEAPTRC}
  heaptrc,
{$ENDIF}
{$IFDEF LSOPENSSL}
  SSL_OpenSSL,
{$ENDIF}
  LSSendMailConsts,
  LSSendMailMessages,
  LSSMTPSend,
  SysUtils,
  Classes,
  SynaCode
{$IFDEF UNIX}
  , SynaUtil
{$ENDIF};

  function YNToBool(const S: string): Boolean;
  begin
    Result := LowerCase(S) = 'y';
  end;

  function MessageTypeFromParam(const AParam: string): TLSMessageType;
  begin
    Result := mtTXT;
    if LowerCase(AParam) = 'html' then
      Result := mtHTML;
  end;

  function RecognizedParam(const AParam: string): Boolean;
  begin
    Result := (AParam = CLSAboutParam) or (AParam = CLSHelpParam1) or
      (AParam = CLSHelpParam2) or (AParam = CLSFromParam) or
      (AParam = CLSToParam) or (AParam = CLSCCParam) or (AParam = CLSBCCParam) or
      (AParam = CLSSubjectParam) or (AParam = CLSMessageParam) or
      (AParam = CLSMessageFileParam) or (AParam = CLSMessageTypeParam) or
      (AParam = CLSAttachedParam) or (AParam = CLSPriorityParam) or
      (AParam = CLSConfirmReadingParam) or (AParam = CLSUserParam) or
      (AParam = CLSPasswordParam) or (AParam = CLSHostParam) or
      (AParam = CLSPortParam) or (AParam = CLSSSLParam) or
      (AParam = CLSTLSParam) or (AParam = CLSAttemptParam) or
{$IFDEF LSOPENSSL}
      (AParam = CLSCheckOpenSSLParam) or
{$ENDIF}
      (AParam = CLSVersionParam) or (AParam = CLSDebugModeParam);
  end;

  function InfoFromParam(const AParam: string): string;
  begin
    Result := '';
    if (ParamCount = 1) and (ParamStr(1) = AParam) then
    begin
      if Boolean(Pos(CLSAboutParam, AParam)) then
        Result := SLSAboutMsg;
      if Boolean(Pos(CLSHelpParam1, AParam)) or
        Boolean(Pos(CLSHelpParam2, AParam)) then
        Result := SLSHelpMsg;
      if Boolean(Pos(CLSVersionParam, AParam)) then
        Result := CLSVersion;
{$IFDEF LSOPENSSL}
      if Boolean(Pos(CLSCheckOpenSSLParam, AParam)) then
        if IsOpenSSLAvailable then
          Result := 'yes'
        else
          Result := 'no';
{$ENDIF}
    end;
  end;

  function LoadFile(const AFileName: TFileName): string;
  begin
    with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
    begin
      try
        SetLength(Result, Size);
        Read(Pointer(Result)^, Size);
      except
        Result := '';
        Free;
        raise;
      end;
      Free;
    end;
  end;

  function SendMail: string;
  var
    I, J: Integer;
    VSufficientParameters: Byte = 0;
    VInfo, VParam, VDebugFileName, VMessage: string;
    VParams, VDebug, VAttachedList: TStringList;
  begin
    VDebug := TStringList.Create;
    VAttachedList := TStringList.Create;
    VParams := TStringList.Create;
    VParams.NameValueSeparator := '=';
    try
      for I := 1 to ParamCount do
      begin
        J := Pred(I);
        VParams.Add(DecodeURL(
{$IFDEF UNIX}
          UnquoteStr(
{$ENDIF}
          ParamStr(I)
{$IFDEF UNIX}
          , '''')
{$ENDIF}
          ));
        if VParams.Names[J] <> '' then
          VParam := VParams.Names[J]
        else
          VParam := ParamStr(I);
        if not RecognizedParam(VParam) then
        begin
          Result := Format(SLSUnrecognizedParamError, [VParam]);
          Exit;
        end;
        VInfo := InfoFromParam(VParam);
        if VInfo <> '' then
        begin
          Result := VInfo;
          Exit;
        end;
        if VParam = CLSToParam then
          Inc(VSufficientParameters);
        if VParam = CLSUserParam then
          Inc(VSufficientParameters);
        if VParam = CLSPasswordParam then
          Inc(VSufficientParameters);
        if VParam = CLSHostParam then
          Inc(VSufficientParameters);
        if VParam = CLSPortParam then
          Inc(VSufficientParameters);
        if VParams.ValueFromIndex[J] = '' then
        begin
          Result := Format(SLSParamEmptyError, [VParam]);
          Exit;
        end;
      end;
      if VSufficientParameters < 5 then
      begin
        Result := SLSInsufficientParameters;
        Exit;
      end;
      ExtractStrings([',', ';'], [' '], PChar(VParams.Values[CLSAttachedParam]),
        VAttachedList);
      VDebugFileName := VParams.Values[CLSDebugModeParam];
      VMessage := VParams.Values[CLSMessageParam];
      if (VMessage = '') and (VParams.Values[CLSMessageFileParam] <> '') then
        VMessage := LoadFile(VParams.Values[CLSMessageFileParam]);
      Result := LSSMTPSend.LSSendMail(VParams.Values[CLSFromParam],
        VParams.Values[CLSToParam], VParams.Values[CLSSubjectParam],
        VMessage, VParams.Values[CLSPriorityParam], VParams.Values[CLSCCParam],
        VParams.Values[CLSBCCParam], VParams.Values[CLSUserParam],
        VParams.Values[CLSPasswordParam], VParams.Values[CLSHostParam],
        VParams.Values[CLSPortParam], VAttachedList,
        YNToBool(VParams.Values[CLSConfirmReadingParam]),
        YNToBool(VParams.Values[CLSSSLParam]),
        YNToBool(VParams.Values[CLSTLSParam]),
        MessageTypeFromParam(VParams.Values[CLSMessageTypeParam]),
        StrToIntDef(VParams.Values[CLSAttemptParam], 3), VDebugFileName <> '');
      if VDebugFileName <> '' then
      begin
        VDebug.Text := Result;
        try
          VDebug.SaveToFile(VDebugFileName);
        except
        end;
      end;
    finally
      VParams.Free;
      VDebug.Free;
      VAttachedList.Free;
    end;
  end;

  procedure Output(const AOutput: string);
  begin
{$IFDEF UNIX}
    WriteLn(AOutput);
{$ENDIF}
{$IFDEF MSWINDOWS}
    Write(AOutput);
{$ENDIF}
  end;

var
  VOutput: string;

begin
{$IFDEF HEAPTRC}
{$IFDEF UNIX}
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'LSSENDMAIL.TRC');
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + 'LSSENDMAIL.TRC');
{$ENDIF}
{$ENDIF}
  if ParamCount < 1 then
  begin
    Output(SLSAboutMsg);
    Exit;
  end;
  VOutput := SendMail;
  if VOutput <> '' then
    Output(VOutput)
  else
    Output(SLSUnknownError);
end.

