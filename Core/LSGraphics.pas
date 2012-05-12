(*
  LazSolutions, Graphics unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSGraphics;

{$I lazsolutions.inc}

interface

uses
  LSUtils, LCLIntf, LCLType, IntfGraphics, FPImage, Classes, SysUtils, Graphics;

{ Capture desktop area to TFPImageBitmap image. (extended) }
procedure LSPrintScreenEx(var AImage: TFPImageBitmap; const AHandle: THandle;
  const APrintScreenNarrow: Double = 1.4);
{ Capture desktop area to TPortableNetworkGraphic image file. }
procedure LSPrintScreenPNG(const AImageFileName: TFileName;
  const AHandle: THandle; const APrintScreenNarrow: Double = 1.4);
{ Capture desktop area to TJPEGImage image file. }
procedure LSPrintScreenJPEG(const AImageFileName: TFileName;
  const AHandle: THandle; const APrintScreenNarrow: Double = 1.4;
  const ACompressionQuality: TJPEGQualityRange = 40);
{ Base64 string to picture. }
procedure LSBase64ToPicture(const ABase64Str: string; APicture: TPicture);
{ Picture to base64 string. }
function LSPictureToBase64(const APicture: TPicture): string;
{ Get file extension from a picture. }
function LSGetFileExtFromPicture(const APicture: TPicture): ShortString;
{ Increment value in a color. }
function LSIncColor(const AColor: TColor; const AQuantity: Byte): TColor;
{ Decrement value in a color. }
function LSDecColor(const AColor: TColor; const AQuantity: Byte): TColor;
{ Get the width of the text of a source. }
function LSGetTextWidth(const AText: string; const AFont: TFont): Integer;
{ Make image in grayscale. }
procedure LSMakeImageGrayscale(const ASrc, ADest: TFPImageBitmap);

implementation

procedure LSPrintScreenEx(var AImage: TFPImageBitmap; const AHandle: THandle;
  const APrintScreenNarrow: Double);
var
  VImage: TFPImageBitmap;
  VHDC: HDC;
  VRect: TRect;
begin
  if not Assigned(AImage) then
    raise Exception.Create('AImage parameter can''t be nil.');
  if (APrintScreenNarrow < 0.1) or (APrintScreenNarrow > 2) then
    raise Exception.Create(
      'Use APrintScreenNarrow parameter value between 0.1 to 2.');
  if AImage is TPortableNetworkGraphic then
    VImage := TPortableNetworkGraphic.Create;
  if AImage is TJPEGImage then
    VImage := TJPEGImage.Create;
  if not Assigned(VImage) then
    Exit;
  VHDC := GetDC(0);
  try
    VImage.LoadFromDevice(VHDC);
    VRect.Top := 0;
    VRect.Left := 0;
    VRect.Right := Round(VImage.Width / APrintScreenNarrow);
    VRect.Bottom := Round(VImage.Height / APrintScreenNarrow);
    AImage.SetSize(VRect.Right, VRect.Bottom);
    AImage.Canvas.StretchDraw(VRect, VImage);
  finally
    VImage.Free;
    ReleaseDC(AHandle, VHDC);
  end;
end;

procedure LSPrintScreenPNG(const AImageFileName: TFileName;
  const AHandle: THandle; const APrintScreenNarrow: Double);
var
  VPortableNetworkGraphic: TPortableNetworkGraphic;
begin
  VPortableNetworkGraphic := TPortableNetworkGraphic.Create;
  try
    LSPrintScreenEx(TFPImageBitmap(VPortableNetworkGraphic), AHandle,
      APrintScreenNarrow);
    VPortableNetworkGraphic.SaveToFile(AImageFileName);
  finally
    VPortableNetworkGraphic.Free;
  end;
end;

procedure LSPrintScreenJPEG(const AImageFileName: TFileName;
  const AHandle: THandle; const APrintScreenNarrow: Double;
  const ACompressionQuality: TJPEGQualityRange);
var
  VJPEGImage: TJPEGImage;
begin
  VJPEGImage := TJPEGImage.Create;
  try
    LSPrintScreenEx(TFPImageBitmap(VJPEGImage), AHandle, APrintScreenNarrow);
    VJPEGImage.CompressionQuality := ACompressionQuality;
    VJPEGImage.SaveToFile(AImageFileName);
  finally
    VJPEGImage.Free;
  end;
end;

procedure LSBase64ToPicture(const ABase64Str: string; APicture: TPicture);
var
  VMemoryStream: TMemoryStream;
begin
  VMemoryStream := TMemoryStream.Create;
  try
    LSBase64StrToStream(ABase64Str, TStream(VMemoryStream));
    APicture.LoadFromStream(VMemoryStream);
  finally
    VMemoryStream.Free;
  end;
end;

function LSPictureToBase64(const APicture: TPicture): string;
var
  VMemoryStream: TMemoryStream;
begin
  VMemoryStream := TMemoryStream.Create;
  try
    APicture.SaveToStream(VMemoryStream);
    Result := LSStreamToBase64Str(TStream(VMemoryStream));
  finally
    VMemoryStream.Free;
  end;
end;

function LSGetFileExtFromPicture(const APicture: TPicture): ShortString;
var
  I: Integer;
begin
  Result := APicture.Graphic.GetFileExtensions;
  I := Pos(';', Result);
  if I > 0 then
    Delete(Result, I, MaxInt);
end;

function LSIncColor(const AColor: TColor; const AQuantity: Byte): TColor;

  function _Min(A, B: Integer): Integer;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

var
  R, G, B : Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R := _Min(255, Integer(R) + AQuantity);
  G := _Min(255, Integer(G) + AQuantity);
  B := _Min(255, Integer(B) + AQuantity);
  Result := RGBToColor(R, G, B);
end;

function LSDecColor(const AColor: TColor; const AQuantity: Byte): TColor;
begin
  Result := DecColor(AColor, AQuantity);
end;

function LSGetTextWidth(const AText: string; const AFont: TFont): Integer;
var
  VCanvas: TCanvas;
begin
  VCanvas := TCanvas.Create;
  try
    VCanvas.Handle := GetDC(0);
    VCanvas.Font.Assign(AFont);
    Result := VCanvas.TextWidth(AText);
  finally
    ReleaseDC(0, VCanvas.Handle);
    VCanvas.Free;
  end;
end;

procedure LSMakeImageGrayscale(const ASrc, ADest: TFPImageBitmap);
var
  X, Y: Integer;
  VCurColor: TFPColor;
  VLazIntfImage: TLazIntfImage;
begin
  VLazIntfImage := ASrc.CreateIntfImage;
  try
    for Y := 0 to Pred(VLazIntfImage.Height) do
      for X := 0 to Pred(VLazIntfImage.Width) do
      begin
        VCurColor := VLazIntfImage.Colors[X, Y];
        VCurColor.Green := VCurColor.Red;
        VCurColor.Blue := VCurColor.Red;
        VLazIntfImage.Colors[X, Y] := VCurColor;
      end;
    ADest.LoadFromIntfImage(VLazIntfImage);
  finally
    VLazIntfImage.Free;
  end;
end;

end.

