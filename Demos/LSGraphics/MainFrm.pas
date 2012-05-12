unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, Spin, Buttons,
  Controls;

type

  { TMainForm }

  TMainForm = class(TForm)
    OpenBase64PictureButton: TButton;
    Base64PictureImage: TImage;
    PrintScreenBitBtn: TBitBtn;
    PrintScreenNarrowFloatSpinEdit: TFloatSpinEdit;
    PreviewImage: TImage;
    PrintScreenNarrowLabel: TLabel;
    CompressionQualityLabel: TLabel;
    MainPageControl: TPageControl;
    PreviewScrollBox: TScrollBox;
    CompressionQualitySpinEdit: TSpinEdit;
    Base64PictureTabSheet: TTabSheet;
    TopPanel: TPanel;
    PrintScreenTabSheet: TTabSheet;
    procedure OpenBase64PictureButtonClick(Sender: TObject);
    procedure PrintScreenBitBtnClick(Sender: TObject);
  private
    FImageFileName: TFileName;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSGraphics, LSDialogs, LSUtils, LCLIntf;

{ TMainForm }

procedure TMainForm.PrintScreenBitBtnClick(Sender: TObject);
begin
  if FImageFileName = '' then
  begin
    FImageFileName := 'PrintScreen.jpg';
    LSSaveDialog(FImageFileName, sdtFile, '', '', -1, True, True);
  end;
  Screen.Cursor := crHourGlass;
  try
    LSPrintScreenJPEG(FImageFileName, Handle,
      PrintScreenNarrowFloatSpinEdit.Value, CompressionQualitySpinEdit.Value);
  finally
    Screen.Cursor := crDefault;
  end;
  PreviewImage.Picture.LoadFromFile(FImageFileName);
end;

procedure TMainForm.OpenBase64PictureButtonClick(Sender: TObject);
var
  VURL: string;
begin
  try
    VURL := ExtractFilePath(ParamStr(0)) + 'image.html';
    LSSaveFile(VURL, Format(
      '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' +
      LineEnding + '<html>' + LineEnding + '<head>' + LineEnding +
      '<title>LSGraphics</title>' + LineEnding + '</head>' +
      LineEnding + '<body>' + LineEnding +
      '<img src="data:image/png;base64,%s" alt="LSGraphics">' +
      LineEnding + '</body>' + LineEnding + '</html>',
      [LSPictureToBase64(Base64PictureImage.Picture)]));
    OpenURL(VURL);
    Sleep(1000);
  finally
    DeleteFile(VURL);
  end;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FImageFileName := '';
end;

end.

