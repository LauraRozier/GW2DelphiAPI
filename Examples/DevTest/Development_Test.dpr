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
  fRequestParams: TUrlParams;
  fVersion:       TGW2Version;
  fStringList:    TStringList;
  fTmpString:     string;

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
    fGW2API.Misc.GetBuild(fGW2API.WebHandler, fVersion);
    WriteLn('Build: ' + IntToStr(fVersion.id));
    FreeAndNil(fVersion);

    WriteLn;
    WriteLn;
    WriteLn('Quaggans:');
    fStringList := TStringList.Create;
    fGW2API.Misc.GetQuagganIDs(fGW2API.WebHandler, fStringList);

    for fTmpString in fStringList do
      WriteLn(fTmpString);

    FreeAndNil(fStringList);
    FreeAndNil(fGW2API);

    WriteLn('Press the Enter key to continue...');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
