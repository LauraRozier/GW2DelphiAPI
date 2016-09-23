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

CONST
  { These keys only exist for development purposes and will be deleted ASAP! }
  CONST_DEV_API_KEY = '9AA549AC-E2C5-994C-86D0-F49D255886DFB57AE877-A650-474A-8EC0-33FB67B3D12B';
                      //'C94A01D8-5B6E-A044-B6BA-0040B4E177BEC3F9C17B-FD49-43BB-87EC-282DB2C11493';

var
  fGW2API:        TGW2API;
  fAPIRequestUrl: string;
  fStringList:    TStringList;
  fTmpString:     string;
  fString:        string;
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
