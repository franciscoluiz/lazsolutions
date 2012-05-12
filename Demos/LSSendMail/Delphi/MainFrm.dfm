object MainForm: TMainForm
  Left = 246
  Top = 199
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 202
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SendButton: TButton
    Left = 124
    Top = 88
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 0
    OnClick = SendButtonClick
  end
end
