unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  LSConsts, LSHTTPSend, LSMessages, Classes, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, SysUtils, Controls, FileUtil, LCLProc, LCLIntf, HTTPSend, DOM,
  DOM_HTML, FPJSON;

type

  { TMainForm }

  TMainForm = class(TForm)
    JSONGetButton: TButton;
    CurrencyConvertButton: TButton;
    CancelDownloadButton: TButton;
    CurrencyFromComboBox: TComboBox;
    CurrencyToComboBox: TComboBox;
    DownloadProgressBar: TProgressBar;
    DownloadLogMemo: TMemo;
    DownloadBtnsPanel: TPanel;
    CurrencyValueEdit: TEdit;
    GetPartButton: TButton;
    ImagePropLabel: TLabel;
    DownloadTabSheet: TTabSheet;
    CurrencyValueLabel: TLabel;
    CurrencyFromLabel: TLabel;
    CurrencyToLabel: TLabel;
    CurrencyResultLabel: TLabel;
    JSONIdLabel: TLabel;
    JSONNameLabel: TLabel;
    LongURLEdit: TEdit;
    LongURLLabel: TLabel;
    PostURLResultMemo: TMemo;
    PauseDownloadButton: TButton;
    ShortenButton: TButton;
    StartDownloadButton: TButton;
    CurrencyConverterTabSheet: TTabSheet;
    JSONTabSheet: TTabSheet;
    TimeOfBrazilLabel: TLabel;
    GetPartTabSheet: TTabSheet;
    DownloadTimer: TTimer;
    ValidatePostFileButton: TButton;
    PictureImage: TImage;
    PictureBottomPanel: TPanel;
    GetBinaryButton: TButton;
    GetPictureButton: TButton;
    HostEdit: TEdit;
    GetHTMLButton: TButton;
    MainPageControl: TPageControl;
    HTMLBottomPanel: TPanel;
    HTMLMemo: TMemo;
    HTMLTabSheet: TTabSheet;
    BinaryTabSheet: TTabSheet;
    PictureScrollBox: TScrollBox;
    PictureTabSheet: TTabSheet;
    PostURLTabSheet: TTabSheet;
    PostFileTabSheet: TTabSheet;
    procedure CancelDownloadButtonClick(Sender: TObject);
    procedure CurrencyConvertButtonClick(Sender: TObject);
    procedure DownloadTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetPartButtonClick(Sender: TObject);
    procedure JSONGetButtonClick(Sender: TObject);
    procedure PauseDownloadButtonClick(Sender: TObject);
    procedure ShortenButtonClick(Sender: TObject);
    procedure StartDownloadButtonClick(Sender: TObject);
    procedure ValidatePostFileButtonClick(Sender: TObject);
    procedure GetBinaryButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GetHTMLButtonClick(Sender: TObject);
    procedure GetPictureButtonClick(Sender: TObject);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure CurrencyValueEditKeyPress(Sender: TObject; var Key: char);
  private
    FFirstRequest: Boolean;
    FCanDownload: Boolean;
    FHTTPSend: THTTPSend;
    FFileStream: TFileStream;
    FHost: string;
    FFileName: string;
    FTotalDownloadSize: Int64;
    FHTMLDocument: THTMLDocument;
  public
    procedure AddToLog(const AInfo: string);
    procedure Download;
    procedure OnMonitor(ASender: TObject; AWriting: Boolean;
      const ABuffer: Pointer; ALength: Integer);
  end;

const
  CUnableHTMLMsg = 'Unable to get the HTML.';
  CDownloadURL =
    'http://lazsolutions.googlecode.com/files/lssendmail_1.6_win32_all.exe';
  CDownloadKBBySec = 50 * 1024; // Use High(Int64) to download direct.
  CDOMDocumentURL = 'http://www.google.com/finance/converter';
  CGooGlAPIURL = 'https://www.googleapis.com/urlshortener/v1/url';
  CGooGllongUrlJSON = '{"longUrl": "%s"}';
  CGooGlErrorMsg = 'It wasn''t possible to shorten the URL "%s".';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

 procedure TMainForm.GetHTMLButtonClick(Sender: TObject);
{$IFDEF UNIX}
var
  VStrTemp: TStringList;
{$ENDIF}
begin
{$IFDEF UNIX}
  VStrTemp := TStringList.Create;
  try
{$ENDIF}
    if not LSHTTPGetText(HostEdit.Text,
{$IFDEF UNIX}
      VStrTemp
{$ENDIF}
{$IFDEF MSWINDOWS}
      HTMLMemo.Lines
{$ENDIF}) then
      ShowMessage(CUnableHTMLMsg);
{$IFDEF UNIX}
    HTMLMemo.Text := AnsiToUtf8(VStrTemp.Text);
  finally
    VStrTemp.Free;
  end;
{$ENDIF}
end;

procedure TMainForm.GetPictureButtonClick(Sender: TObject);
var
  VFileExtensions: string = '';
  VMimeType: string = '';
begin
  if LSHTTPGetPicture(
    'http://www.google.com.br/intl/en_com/images/srpr/logo1w.png',
    PictureImage.Picture, VFileExtensions, VMimeType) then
  begin
    PictureImage.AutoSize := True;
    ImagePropLabel.Caption := 'FileExtensions: ' + VFileExtensions +
      '; MimeType: ' + VMimeType + '.';
  end
  else
    ShowMessage('Unable to get the picture.');
end;

procedure TMainForm.GetBinaryButtonClick(Sender: TObject);
const
  CBinary = 'logo1w.png';
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(CBinary, fmCreate);
  try
    if LSHTTPGetBinary('http://www.google.com.br/intl/en_com/images/srpr/' +
      CBinary, VFileStream) then
      OpenDocument(CBinary)
    else
      ShowMessage('Unable to get the "' + CBinary + '".');
  finally
    VFileStream.Free;
  end;
end;

procedure TMainForm.ValidatePostFileButtonClick(Sender: TObject);
const
  CPostFileTest = 'PostFileTest.html';
  CW3ResultFile = 'W3Result.html';
var
  VStrTemp: TStringList;
  VFileStream: TFileStream;
begin
  VStrTemp := TStringList.Create;
  VFileStream := TFileStream.Create(CPostFileTest, fmOpenRead);
  try
    if LSHTTPPostFile('http://validator.w3.org/check', 'uploaded_file',
      CPostFileTest, VFileStream, VStrTemp, 'text/html') then
    begin
      VStrTemp.SaveToFile(CW3ResultFile);
      OpenURL(CW3ResultFile);
    end
    else
      ShowMessage('Was not possible to post file.')
  finally
    VFileStream.Free;
  end;
end;

procedure TMainForm.GetPartButtonClick(Sender: TObject);
var
  S: string;
begin
  S := LSHTTPGetPart('<td align="center" bgcolor=#ffff00><b>', '</b>',
    'http://pcdsh01.on.br/HoraLegalBrasileira.asp');
  if S <> '' then
    ShowMessage(S)
  else
    ShowMessage('Unable to get the time of Brazil.');
end;

procedure TMainForm.JSONGetButtonClick(Sender: TObject);
var
  VData: TJSONData = nil;
begin
  LSHTTPGetJSON(
    'http://lazsolutions.googlecode.com/svn/trunk/Demos/LSHTTPSend/test.json',
    VData);
  JSONIdLabel.Caption := 'ID: ' + IntToStr(VData.Items[0].AsInteger);
  JSONNameLabel.Caption := 'Name: ' + VData.Items[1].AsString;
end;

procedure TMainForm.PauseDownloadButtonClick(Sender: TObject);
const
  CPauseStatus: array[Boolean] of ShortString = ('Paused', 'Pause');
  CPauseLogStatus: array[Boolean] of ShortString = ('Paused...', 'Resume...');
begin
  DownloadTimer.Enabled := not DownloadTimer.Enabled;
  PauseDownloadButton.Caption := CPauseStatus[DownloadTimer.Enabled];
  AddToLog(CPauseLogStatus[DownloadTimer.Enabled]);
end;

procedure TMainForm.ShortenButtonClick(Sender: TObject);
var
  VResult: TStringStream;
begin
  VResult := TStringStream.Create('');
  try
    if LSHTTPPostJSONURL(CGooGlAPIURL, Format(CGooGllongUrlJSON,
      [LongURLEdit.Text]), TStream(VResult)) then
      PostURLResultMemo.Text := VResult.DataString
    else
      ShowMessage(Format(CGooGlErrorMsg, [LongURLEdit.Text]));
  finally
    VResult.Free;
  end;
end;

procedure TMainForm.StartDownloadButtonClick(Sender: TObject);
begin
  FFirstRequest := True;
  FCanDownload := True;
  DownloadLogMemo.Clear;
  DownloadTimer.OnTimer(Self);
  DownloadTimer.Enabled := True;
end;

procedure TMainForm.DownloadTimerTimer(Sender: TObject);
begin
  Download;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHTTPSend.Free;
  if Assigned(FHTMLDocument) then
    FHTMLDocument.Free;
end;

procedure TMainForm.CancelDownloadButtonClick(Sender: TObject);
var
  VOldDownloadTimerEnabled: Boolean;
begin
  try
    VOldDownloadTimerEnabled := DownloadTimer.Enabled;
    DownloadTimer.Enabled := False;
    if MessageDlg('Cancel download?', mtConfirmation, mbYesNo, 0) <> mrYes then
      Exit;
  finally
    DownloadTimer.Enabled := VOldDownloadTimerEnabled;
  end;
  if DownloadTimer.Enabled then
  begin
    DownloadTimer.Enabled := False;
    DownloadLogMemo.Text := 'Download cancelled.';
  end;
  if FileExistsUTF8(FFileName) then
  begin
    DeleteFileUTF8(FFileName);
    DownloadProgressBar.Position := 0;
    DownloadLogMemo.Text := 'File "' + FFileName + '" deleted.';
  end;
  StartDownloadButton.Enabled := True;
  PauseDownloadButton.Enabled := False;
  PauseDownloadButton.Caption := 'Pause';
  CancelDownloadButton.Enabled := False;
end;

procedure TMainForm.CurrencyConvertButtonClick(Sender: TObject);
var
  I: Integer;
  VResult: THTMLDivElement;
  VDOMResult: THTMLDocument = nil;
  VDOMNodeList: TDOMNodeList;
  VHTMLSelectElement, VDOMFromElement, VDOMToElement: THTMLSelectElement;
begin
  if Assigned(FHTMLDocument) then
  begin
    VDOMFromElement := THTMLSelectElement(
      CurrencyFromComboBox.Items.Objects[CurrencyFromComboBox.ItemIndex]);
    VDOMToElement := THTMLSelectElement(
      CurrencyToComboBox.Items.Objects[CurrencyToComboBox.ItemIndex]);
    if Assigned(VDOMFromElement) and Assigned(VDOMToElement) then
      try
        if not LSHTTPGetHTMLDocument(
          'http://www.google.com/finance/converter?a=' +
          CurrencyValueEdit.Text + '&from=' +
          VDOMFromElement.Attributes[0].NodeValue + '&to=' +
          VDOMToElement.Attributes[0].NodeValue, VDOMResult) then
          ShowMessage(CUnableHTMLMsg)
        else
          begin
            VDOMNodeList := VDOMResult.GetElementsByTagName('div');
            if VDOMNodeList.Length >= 6 then
            begin
              VResult := THTMLDivElement(VDOMNodeList.Item[5]);
              if Assigned(VResult) then
                CurrencyResultLabel.Caption := VResult.TextContent
              else
                ShowMessage('Unable to convert, try again.');
            end;
          end;
        finally
          VDOMResult.Free;
        end;
    Exit;
  end;
  if not LSHTTPGetHTMLDocument(CDOMDocumentURL, FHTMLDocument) then
    ShowMessage(CUnableHTMLMsg)
  else
  begin
    VDOMNodeList := FHTMLDocument.GetElementsByTagName('select');
    if (VDOMNodeList.Count >= 2) and
      (LowerCase(VDOMNodeList.Item[0].Attributes[0].NodeValue) = 'from') then
    begin
      CurrencyFromComboBox.Clear;
      for I := 0 to Pred(VDOMNodeList.Item[0].ChildNodes.Count) do
      begin
        VHTMLSelectElement := THTMLSelectElement(
          VDOMNodeList.Item[0].ChildNodes[I]);
        CurrencyFromComboBox.Items.AddObject(VHTMLSelectElement.TextContent,
          VHTMLSelectElement);
      end;
    end;
    if (VDOMNodeList.Count >= 2) and
      (LowerCase(VDOMNodeList.Item[1].Attributes[0].NodeValue) = 'to') then
    begin
      CurrencyToComboBox.Clear;
      for I := 0 to Pred(VDOMNodeList.Item[1].ChildNodes.Count) do
      begin
        VHTMLSelectElement := THTMLSelectElement(
          VDOMNodeList.Item[1].ChildNodes[I]);
        CurrencyToComboBox.Items.AddObject(VHTMLSelectElement.TextContent,
          VHTMLSelectElement);
      end;
    end;
  end;
  if (CurrencyFromComboBox.Items.Count > 0) and
    (CurrencyToComboBox.Items.Count > 0) then
  begin
    CurrencyConvertButton.Tag := 1;
    CurrencyValueEdit.Enabled := True;
    CurrencyFromComboBox.Enabled := True;
    CurrencyFromComboBox.ItemIndex := 0;
    CurrencyToComboBox.Enabled := True;
    CurrencyToComboBox.ItemIndex := 0;
    CurrencyConvertButton.Caption := '&Convert';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
//  LSGlobalProxyConfig.AutoConfig := False; << To no use auto-config proxy
  FHTTPSend := THTTPSend.Create;
  FHTTPSend.Sock.OnMonitor := @OnMonitor;
  FFirstRequest := True;
  FCanDownload := True;
  PictureImage.SetBounds(0, 0, 48, 48);
end;

procedure TMainForm.HostEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    GetHTMLButton.Click;
  end;
end;

procedure TMainForm.CurrencyValueEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8]) then
    Key := #0
end;

procedure TMainForm.AddToLog(const AInfo: string);
begin
  DownloadLogMemo.Lines.Add(AInfo);
end;

procedure TMainForm.Download;

  function _FormatHTTPError(const S: string): string;
  begin
    Result := Format(SLSHTTPSendUnableToGetError, [S]);
  end;

  function _DownloadCompleteMsg(const AURL, AFileName: string): string;
  begin
     Result := Format(SLSHTTPSendDownloadCompleteMsg, [AURL, AFileName]);
  end;

  function _TotalDownloadSizeMsg(const ASize: Int64): string;
  begin
    Result := Format(SLSHTTPSendTotalDownloadSizeMsg, [ASize]);
  end;

var
  S: string;
  VFileSize, VContentLength: Int64;
  VOldNameValueSeparator: Char;
begin
  if not FCanDownload then
    Exit;
  StartDownloadButton.Enabled := False;
  PauseDownloadButton.Enabled := True;
  CancelDownloadButton.Enabled := True;
  try
    FCanDownload := False;
    FHTTPSend.Clear;
    VOldNameValueSeparator := FHTTPSend.Headers.NameValueSeparator;
    FHTTPSend.Headers.NameValueSeparator := ':';
    if FFirstRequest then
    begin
      FHTTPSend.RangeEnd := 1;
      Application.ProcessMessages;
      if LSShotMethod(FHTTPSend, CLSHTTPSendGETMethod, CDownloadURL, False) then
      begin
        if FHTTPSend.ResultCode = 200 then
        begin
          DownloadTimer.Enabled := False;
          FFileStream := TFileStream.Create(FFileName, fmCreate);
          try
            FFileStream.CopyFrom(FHTTPSend.Document, FHTTPSend.Document.Size);
            AddToLog(_TotalDownloadSizeMsg(FHTTPSend.Document.Size));
            AddToLog(_DownloadCompleteMsg(CDownloadURL, FFileName));
            StartDownloadButton.Enabled := True;
            PauseDownloadButton.Enabled := False;
            CancelDownloadButton.Enabled := False;
            DownloadProgressBar.Max := 100;
            DownloadProgressBar.Position := 100;
          finally
            FFileStream.Free;
          end;
          Exit;
        end;
        if FHTTPSend.ResultCode = 206 then
        begin
          FFirstRequest := False;
          AddToLog('Host: ' + FHost);
          AddToLog('File name: ' + FFileName);
          S := FHTTPSend.Headers.Values['content-range'];
          FTotalDownloadSize := StrToInt64Def(GetPart('/', '', S, False, False), 0);
          FCanDownload := True;
        end
        else
        begin
          AddToLog(_FormatHTTPError(CDownloadURL));
          Exit;
        end;
      end
      else
      begin
        AddToLog(_FormatHTTPError(CDownloadURL));
        Exit;
      end;
    end
    else
    begin
      if FileExistsUTF8(FFileName) then
        FFileStream := TFileStream.Create(FFileName, fmOpenReadWrite)
      else
        FFileStream := TFileStream.Create(FFileName, fmCreate);
      try
        VFileSize := FFileStream.Size;
        if VFileSize >= FTotalDownloadSize then
        begin
          DownloadTimer.Enabled := False;
          AddToLog(_DownloadCompleteMsg(CDownloadURL, FFileName));
          StartDownloadButton.Enabled := True;
          PauseDownloadButton.Enabled := False;
          CancelDownloadButton.Enabled := False;
          Exit;
        end
        else
        begin
          FFileStream.Position := VFileSize;
          FHTTPSend.RangeStart := VFileSize;
          if VFileSize + CDownloadKBBySec < FTotalDownloadSize then
            FHTTPSend.RangeEnd := VFileSize + CDownloadKBBySec;
          Application.ProcessMessages;
          if LSShotMethod(FHTTPSend, CLSHTTPSendGETMethod, CDownloadURL,
            False) then
          begin
            if FHTTPSend.ResultCode = 200 then
            begin
              DownloadTimer.Enabled := False;
              FFileStream.CopyFrom(FHTTPSend.Document, FHTTPSend.Document.Size);
              AddToLog(_TotalDownloadSizeMsg(FHTTPSend.Document.Size));
              AddToLog(_DownloadCompleteMsg(CDownloadURL, FFileName));
              StartDownloadButton.Enabled := True;
              PauseDownloadButton.Enabled := False;
              CancelDownloadButton.Enabled := False;
              DownloadProgressBar.Max := 100;
              DownloadProgressBar.Position := 100;
              Exit;
            end;
            if FHTTPSend.ResultCode = 206 then
            begin
              VContentLength := VFileSize;
              VContentLength +=
                StrToInt64Def(FHTTPSend.Headers.Values['content-length'], 0);
              FFileStream.CopyFrom(FHTTPSend.Document, FHTTPSend.RangeEnd);
              AddToLog('Downloaded: ' + IntToStr(VContentLength));
              DownloadProgressBar.Max := FTotalDownloadSize;
              DownloadProgressBar.Position := VContentLength;
              FCanDownload := True;
              Exit;
            end
            else
            begin
              DownloadTimer.Enabled := False;
              AddToLog(_FormatHTTPError(CDownloadURL) + FHTTPSend.Headers.Text);
              Exit;
            end;
          end
          else
          begin
            DownloadTimer.Enabled := False;
            AddToLog(_FormatHTTPError(CDownloadURL));
            Exit;
          end;
        end;
      finally
        FFileStream.Free;
      end;
    end;
  finally
    FHTTPSend.Headers.NameValueSeparator := VOldNameValueSeparator;
  end;
end;

{$HINTS OFF}
procedure TMainForm.OnMonitor(ASender: TObject; AWriting: Boolean;
  const ABuffer: Pointer; ALength: Integer);
var
  S: string;
begin
  if AWriting and FFirstRequest then
  begin
    FHost := Trim(FHTTPSend.Headers.Values['host']);
    S := FHTTPSend.Headers.Text;
    S := GetPart('get ', ' http', S, True, False);
    FFileName := ExtractFilePath(ParamStrUTF8(0)) + ExtractFileName(S);
  end;
end;
{$HINTS ON}

end.

