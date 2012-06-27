(*
  LazSolutions, Captcha unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSCaptcha;

{$I lazsolutions.inc}

interface

uses
  FPCanvas, FPImage, FPImgCanv, FTFont, SysUtils;

const
  colDkTeal: TFPColor = (Red: $0000; Green: $2000; Blue: $5000;
    Alpha: AlphaOpaque);
  ftLiberationSerifRegularTTF = 'LiberationSerif-Regular.ttf';

type

  { TLSFPMemoryImage }

  TLSFPMemoryImage = class(TFPMemoryImage)
  public
    constructor Create; overload;
  end;

  { TLSCaptchaType }

  TLSCaptchaType = (ctNoise, ctLine);

{ Check if font file exists. }
function LSFontExists(const AFileName: TFileName): Boolean;
{ Generate Captcha code. (see: http://en.wikipedia.org/wiki/Captcha) }
function LSCaptchaGenerator(var AImage: TLSFPMemoryImage;
  const ACaptchaType: TLSCaptchaType = ctLine): string;
{ Generate Captcha code. (extended) }
function LSCaptchaGeneratorEx(var AImage: TLSFPMemoryImage;
  const AFontColor: TFPColor; const AFontName: ShortString =
  ftLiberationSerifRegularTTF; const AFontSize: Byte = 18;
  const AEasyRead: Byte = 3; const ABackground: Boolean = True;
  const ACaptchaType: TLSCaptchaType = ctLine): string;

implementation

function LSFontExists(const AFileName: TFileName): Boolean;
{$IFDEF UNIX}
const
  VFontPath = '/usr/share/fonts';
{$ENDIF}
begin
  Result := FileExists(AFileName) or
{$IFDEF UNIX}
   (FileExists(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
     '.fonts/' + AFileName)) or
   (FileExists(VFontPath + AFileName)) or
   (FileExists(VFontPath + '/truetype/' + AFileName)) or
   (FileExists(VFontPath + '/truetype/freefont/')) ;
{$ENDIF}
{$IFDEF MSWINDOWS}
    FileExists(IncludeTrailingPathDelimiter(GetEnvironmentVariable('windir')) +
      'Fonts' + DirectorySeparator + AFileName);
{$ENDIF}
end;

{$WARNINGS OFF}
function LSCaptchaGeneratorEx(var AImage: TLSFPMemoryImage;
  const AFontColor: TFPColor; const AFontName: ShortString;
  const AFontSize: Byte; const AEasyRead: Byte; const ABackground: Boolean;
  const ACaptchaType: TLSCaptchaType): string;

  function _Min(A, B: Integer): Integer;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

  function _RandomRange(const AFrom, ATo: Integer): Integer;
  begin
    Result := Random(Abs(AFrom - ATo)) + _Min(ATo, AFrom);
  end;

const
  CNumbers = '0123456789';
  CAlphaNumeric = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' + CNumbers;
var
  I: Integer;
  VFreeTypeFont: TFreeTypeFont;
  VFPImageCanvas: TFPImageCanvas;
begin
  VFreeTypeFont := TFreeTypeFont.Create;
  VFPImageCanvas := TFPImageCanvas.Create(AImage);
  AImage.SetSize(80, 20);
  try
    VFreeTypeFont.Name := AFontName;
    VFreeTypeFont.Size := AFontSize;
    VFreeTypeFont.FPColor := AFontColor;
    VFPImageCanvas.Font := VFreeTypeFont;
    if ABackground then
    begin
      VFPImageCanvas.Brush.FPColor := colWhite;
      VFPImageCanvas.Brush.Style := bsSolid;
{$IFDEF LSNEWFPC}
      VFPImageCanvas.FillRect(0, 0, VFPImageCanvas.Width, VFPImageCanvas.Height);
{$ENDIF}
    end;
    VFPImageCanvas.Clear;
    VFPImageCanvas.Pen.FPColor := VFreeTypeFont.FPColor;
    if ACaptchaType = ctNoise then
    begin
      for I := 0 to (AImage.Width * AImage.Height) div AEasyRead do
        VFPImageCanvas.EllipseC(_RandomRange(0, AImage.Width),
          _RandomRange(0, AImage.Height), 0, 0);
      for I := 1 to 6 do
        Result += Copy(CNumbers, _RandomRange(0, Length(CNumbers) - 1), 1);
      VFPImageCanvas.TextOut((VFPImageCanvas.Width div 2) -
        (VFPImageCanvas.GetTextWidth(Result) div 2),
        (VFPImageCanvas.Height div 2) + (VFPImageCanvas.GetTextHeight(Result) div 2),
        Result);
    end
    else
    begin
      for I := 0 to 3 do
        Result += CAlphaNumeric[Random(Length(CAlphaNumeric) - 1) + 1];
      for I := 0 to 3 do
      begin
        VFPImageCanvas.Font.Size := _RandomRange(4, 12) + 6;
        VFPImageCanvas.TextOut(I * 22, 16, Result[I + 1]);
      end;
      for I := 0 to AEasyRead do
      begin
        VFPImageCanvas.MoveTo(Random(AImage.Width), 0);
        VFPImageCanvas.LineTo(Random(AImage.Width), AImage.Height);
      end;
    end;
  finally
    VFreeTypeFont.Free;
    VFPImageCanvas.Free;
  end;
end;
{$WARNINGS ON}

function LSCaptchaGenerator(var AImage: TLSFPMemoryImage;
  const ACaptchaType: TLSCaptchaType): string;
begin
  Result := LSCaptchaGeneratorEx(AImage, colDkTeal, ftLiberationSerifRegularTTF,
    18, 3, True, ACaptchaType);
end;

{ TLSFPMemoryImage }

constructor TLSFPMemoryImage.Create;
begin
  inherited Create(100, 100);
end;

initialization
  Randomize;

end.

