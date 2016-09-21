program Development_Test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  // System units
  SysUtils, Classes,
  // GW2 Delphi API Units
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas',
  GW2DA_Defaults in '..\..\src\GW2DA_Defaults.pas',
  GW2DA_Types in '..\..\src\GW2DA_Types.pas',
  GW2DA_Authentication in '..\..\src\Networking\GW2DA_Authentication.pas',
  GW2DA_WebHandlers in '..\..\src\Networking\GW2DA_WebHandlers.pas',
  GW2DA_Misc in '..\..\src\Misc\GW2DA_Misc.pas',
  GW2DA_Utils in '..\..\src\GW2DA_Utils.pas';

var
  fGW2API:        TGW2API;
  fAPIRequestUrl: string;
  fVersion:       TGW2Version;
  fStringList:    TStringList;
  fTmpString:     string;
  fString:        string;
  fInteger:       Integer;
  fColorIDs:      TIntegerArray;
  fColorList:     TGW2ColorList;
  fColor:         TGW2Color;
  fParams:        TUrlParams;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    WriteLn(fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));
    WriteLn;
    WriteLn;
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    WriteLn(fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));

    WriteLn;
    WriteLn;
    fVersion := TGW2Version.Create;
    fGW2API.Misc.GetBuild(fGW2API.WebHandler, APIv1, fVersion);
    WriteLn('Build API v1: ' + IntToStr(fVersion.id));
    fGW2API.Misc.GetBuild(fGW2API.WebHandler, APIv2, fVersion);
    WriteLn('Build API v2: ' + IntToStr(fVersion.id));
    FreeAndNil(fVersion);

    WriteLn;
    WriteLn;
    WriteLn('Quaggans:');
    fStringList := TStringList.Create;
    fGW2API.Misc.GetQuagganIDs(fGW2API.WebHandler, fStringList);

    for fTmpString in fStringList do
      WriteLn(fTmpString);

    FreeAndNil(fStringList);

    WriteLn;
    WriteLn;
    WriteLn('All Color IDs: ');
    fColorIDs := fGW2API.Misc.GetColorIDs(fGW2API.WebHandler);
    fTmpString := '';

    for fInteger in fColorIDs do
      if fTmpString = '' then
        fTmpString := IntToStr(fInteger)
      else
        fTmpString := fTmpString + ', ' + IntToStr(fInteger);

    WriteLn(fTmpString);

    WriteLn;
    WriteLn;
    WriteLn('Colors:');
    SetLength(fParams, 1);
    fParams[0].Name  := 'ids';
    fParams[0].Value := '1,2,3';
    fColorList := fGW2API.Misc.GetColors(fGW2API.WebHandler, fParams);

    for fColor in fColorList do
    begin
      WriteLn('ID: ' + IntToStr(fColor.id));
      WriteLn('Name: ' + fColor.Name);
      WriteLn(Format('Base_RGB: %d,%d,%d', [fColor.Base_RGB[0],
                                            fColor.Base_RGB[1],
                                            fColor.Base_RGB[2]]));
      WriteLn('Cloth: ');
      WriteLn(#9 + 'Brightness' + IntToStr(fColor.Cloth.Brightness));
      WriteLn(#9 + Format('Contrast: %n', [fColor.Cloth.Contrast]));
      WriteLn(#9 + 'Hue' + IntToStr(fColor.Cloth.Hue));
      WriteLn(#9 + Format('Saturation: %n', [fColor.Cloth.Saturation]));
      WriteLn(#9 + Format('Lightness: %n', [fColor.Cloth.Lightness]));
      WriteLn(#9 + Format('RGB: %d,%d,%d', [fColor.Cloth.RGB[0],
                                            fColor.Cloth.RGB[1],
                                            fColor.Cloth.RGB[2]]));
      WriteLn('Leather: ');
      WriteLn(#9 + 'Brightness' + IntToStr(fColor.Leather.Brightness));
      WriteLn(#9 + Format('Contrast: %n', [fColor.Leather.Contrast]));
      WriteLn(#9 + 'Hue' + IntToStr(fColor.Leather.Hue));
      WriteLn(#9 + Format('Saturation: %n', [fColor.Leather.Saturation]));
      WriteLn(#9 + Format('Lightness: %n', [fColor.Leather.Lightness]));
      WriteLn(#9 + Format('RGB: %d,%d,%d', [fColor.Leather.RGB[0],
                                            fColor.Leather.RGB[1],
                                            fColor.Leather.RGB[2]]));
      WriteLn('Metal: ');
      WriteLn(#9 + 'Brightness' + IntToStr(fColor.Metal.Brightness));
      WriteLn(#9 + Format('Contrast: %n', [fColor.Metal.Contrast]));
      WriteLn(#9 + 'Hue' + IntToStr(fColor.Metal.Hue));
      WriteLn(#9 + Format('Saturation: %n', [fColor.Metal.Saturation]));
      WriteLn(#9 + Format('Lightness: %n', [fColor.Metal.Lightness]));
      WriteLn(#9 + Format('RGB: %d,%d,%d', [fColor.Metal.RGB[0],
                                            fColor.Metal.RGB[1],
                                            fColor.Metal.RGB[2]]));
      WriteLn('Item: ' + IntToStr(fColor.Item));
      fTmpString := '';

      for fString in fColor.Categories do
        if fTmpString = '' then
          fTmpString := fString
        else
          fTmpString := fTmpString + ', ' + fString;

      WriteLn('Categories: ' + fTmpString);
    end;

    FreeAndNil(fGW2API);

    WriteLn('Press the Enter key to continue...');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
