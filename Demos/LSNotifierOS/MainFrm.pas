unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Buttons, Controls, Classes, Graphics, ExtCtrls, GraphUtil, Dialogs,
  StdCtrls, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    BottomRightSpeedButton: TSpeedButton;
    BottomLeftSpeedButton: TSpeedButton;
    PlaySoundCheckBox: TCheckBox;
    ImageList: TImageList;
    BottomPanel: TPanel;
    TipLabel: TLabel;
    ThemeImageList: TImageList;
    TopPanel: TPanel;
    TopLeftSpeedButton: TSpeedButton;
    TopRightSpeedButton: TSpeedButton;
    procedure TopLeftSpeedButtonClick(Sender: TObject);
    procedure TopRightSpeedButtonClick(Sender: TObject);
    procedure BottomRightSpeedButtonClick(Sender: TObject);
    procedure BottomLeftSpeedButtonClick(Sender: TObject);
  private
    FBitmap: TBitmap;
    FTheme: TBitmap;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure HintClick(Sender: TObject);
    procedure PlaySound;
  end;

const
  CInfo = 'LSNotifierOS 1.0, Shows a notification window for short messages.';
  CInfoBkColor = $00B9FFFF;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSNotifierOS, LSPlayWAV;

{ TMainForm }

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBitmap := TBitmap.Create;
  FTheme := TBitmap.Create;
  ImageList.GetBitmap(0, FBitmap);
  ThemeImageList.GetBitmap(0, FTheme);
end;

destructor TMainForm.Destroy;
begin
  FTheme.Free;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TMainForm.HintClick(Sender: TObject);
begin
  ShowMessage('Hello! :)');
end;

procedure TMainForm.PlaySound;
begin
  if PlaySoundCheckBox.Checked then
    LSPlayWAVFile(ExtractFilePath(ParamStr(0)) + 'sound.wav');
end;

procedure TMainForm.TopRightSpeedButtonClick(Sender: TObject);
begin
  TLSNotifierOS.Execute(Self, Caption, 'Click me please!', FBitmap, npTopRight,
    True, 61, 10, $003848DC, GetShadowColor($003848DC), clWhite, @HintClick);
  PlaySound;
end;

procedure TMainForm.BottomRightSpeedButtonClick(Sender: TObject);
begin
  TLSNotifierOS.Execute(Self, Caption, CInfo, FBitmap);
  PlaySound;
end;

procedure TMainForm.BottomLeftSpeedButtonClick(Sender: TObject);
begin
  TLSNotifierOS.Execute(Self, FTheme, Caption, CInfo, FBitmap, npBottomLeft);
  PlaySound;
end;

procedure TMainForm.TopLeftSpeedButtonClick(Sender: TObject);
begin
  TLSNotifierOS.Execute(Self, Caption, CInfo, nil, npTopLeft, False, 5, 20,
    $00AEFFFF, clGray, $00282520);
  PlaySound;
end;

end.

