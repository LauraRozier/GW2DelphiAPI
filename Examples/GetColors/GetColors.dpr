program GetColors;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  GW2DA_Defaults in '..\..\src\GW2DA_Defaults.pas',
  GW2DA_Types in '..\..\src\GW2DA_Types.pas',
  GW2DA_Utils in '..\..\src\GW2DA_Utils.pas',
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas',
  GW2DA_Misc in '..\..\src\Misc\GW2DA_Misc.pas',
  GW2DA_Authentication in '..\..\src\Networking\GW2DA_Authentication.pas',
  GW2DA_WebHandlers in '..\..\src\Networking\GW2DA_WebHandlers.pas';

var
  fGW2API:              TGW2API;       // Main API class object
  fParams:              TUrlParams;    // Array of parameter objects
  fColorIDs:            TIntegerArray; // Array of Integers
  fColorList:           TGW2ColorList; // Array of color class objects
  fColor:               TGW2Color;     // Single color class object
  fStrValue, fBuildStr: string;
  fIntValue:            Integer;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    WriteLn;
    WriteLn('All Color IDs: ');
    fColorIDs := fGW2API.Misc.GetColorIDs(fGW2API.WebHandler);
    fBuildStr := '';

    for fIntValue in fColorIDs do
      if fBuildStr = '' then
        fBuildStr := IntToStr(fIntValue)
      else
        fBuildStr := fBuildStr + ', ' + IntToStr(fIntValue);

    WriteLn(fBuildStr);

    WriteLn;
    WriteLn;
    WriteLn('Colors:');
    SetLength(fParams, 1);
    fParams[0].Name  := 'ids';
    fParams[0].Value := '1,2,3';
    fColorList       := fGW2API.Misc.GetColors(fGW2API.WebHandler, fParams);

    for fColor in fColorList do
    begin
      WriteLn(#9 + 'ID: ' + IntToStr(fColor.id));
      WriteLn(#9 + 'Name: ' + fColor.Name);
      WriteLn(#9 + Format('Base_RGB: %d,%d,%d', [fColor.Base_RGB[0],
                                                 fColor.Base_RGB[1],
                                                 fColor.Base_RGB[2]]));
      WriteLn(#9 + 'Cloth: ');
      WriteLn(#9#9 + Format('Brightness: %d', [fColor.Cloth.Brightness]));
      WriteLn(#9#9 + Format('Contrast: %n',   [fColor.Cloth.Contrast]));
      WriteLn(#9#9 + Format('Hue: %d',        [fColor.Cloth.Hue]));
      WriteLn(#9#9 + Format('Saturation: %n', [fColor.Cloth.Saturation]));
      WriteLn(#9#9 + Format('Lightness: %n',  [fColor.Cloth.Lightness]));
      WriteLn(#9#9 + Format('RGB: %d,%d,%d',  [fColor.Cloth.RGB[0],
                                               fColor.Cloth.RGB[1],
                                               fColor.Cloth.RGB[2]]));
      WriteLn(#9 + 'Leather: ');
      WriteLn(#9#9 + Format('Brightness: %d', [fColor.Leather.Brightness]));
      WriteLn(#9#9 + Format('Contrast: %n',   [fColor.Leather.Contrast]));
      WriteLn(#9#9 + Format('Hue: %d',        [fColor.Leather.Hue]));
      WriteLn(#9#9 + Format('Saturation: %n', [fColor.Leather.Saturation]));
      WriteLn(#9#9 + Format('Lightness: %n',  [fColor.Leather.Lightness]));
      WriteLn(#9#9 + Format('RGB: %d,%d,%d',  [fColor.Leather.RGB[0],
                                               fColor.Leather.RGB[1],
                                               fColor.Leather.RGB[2]]));
      WriteLn(#9 + 'Metal: ');
      WriteLn(#9#9 + Format('Brightness: %d', [fColor.Metal.Brightness]));
      WriteLn(#9#9 + Format('Contrast: %n',   [fColor.Metal.Contrast]));
      WriteLn(#9#9 + Format('Hue: %d',        [fColor.Metal.Hue]));
      WriteLn(#9#9 + Format('Saturation: %n', [fColor.Metal.Saturation]));
      WriteLn(#9#9 + Format('Lightness: %n',  [fColor.Metal.Lightness]));
      WriteLn(#9#9 + Format('RGB: %d,%d,%d',  [fColor.Metal.RGB[0],
                                               fColor.Metal.RGB[1],
                                               fColor.Metal.RGB[2]]));
      WriteLn(#9 + 'Item: ' + IntToStr(fColor.Item));
      fBuildStr := '';

      for fStrValue in fColor.Categories do
        if fBuildStr = '' then
          fBuildStr := fStrValue
        else
          fBuildStr := fBuildStr + ', ' + fStrValue;

      WriteLn(#9 + 'Categories: ' + fBuildStr);
      WriteLn;
    end;

    FreeAndNil(fGW2API);

    WriteLn('Press the Enter key to continue...');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.