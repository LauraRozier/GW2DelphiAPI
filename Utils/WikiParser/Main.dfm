object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal To Wiki Parser'
  ClientHeight = 561
  ClientWidth = 785
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    785
    561)
  PixelsPerInch = 96
  TextHeight = 13
  object txtParserOutput: TMemo
    Left = 0
    Top = 111
    Width = 785
    Height = 450
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyPress = txtParserOutputKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 673
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings '
    TabOrder = 1
    DesignSize = (
      673
      97)
    object Label1: TLabel
      Left = 11
      Top = 19
      Width = 47
      Height = 13
      Caption = 'Input file:'
    end
    object Label4: TLabel
      Left = 11
      Top = 61
      Width = 93
      Height = 13
      Caption = 'Output file Actions:'
    end
    object edtInputFile: TEdit
      Left = 112
      Top = 16
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtOnTextChange
    end
    object edtOutputFile: TEdit
      Left = 112
      Top = 58
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtOnTextChange
    end
  end
  object btnGenerate: TButton
    Left = 687
    Top = 47
    Width = 90
    Height = 58
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = btnGenerateClick
  end
  object Button1: TButton
    Left = 696
    Top = 16
    Width = 25
    Height = 25
    Hint = 'Configuration file 1'
    Anchors = [akTop, akRight]
    Caption = '1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 744
    Top = 16
    Width = 25
    Height = 25
    Hint = 'Configuration file 2'
    Anchors = [akTop, akRight]
    Caption = '2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = Button2Click
  end
end
