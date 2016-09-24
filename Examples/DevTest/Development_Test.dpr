program Development_Test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

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
//  fParams:        TUrlParams;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    WriteLn('Using API version: ' +
            fGW2API.Utils.EnumToString(
              fGW2API.Utils.StringToEnum<TAPIVersion>('APIv1')
            ) + sLineBreak +
            fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));
    WriteLn;
    WriteLn;
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    WriteLn('Using API version: ' +
            IntToStr(fGW2API.Utils.EnumToInt(APIv2)) + sLineBreak +
            fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));

    WriteLn;
    WriteLn;
    WriteLn('Authentication:');
    fTmpString := fGW2API.Authenticate(CONST_DEV_API_KEY);
    WriteLn(fTmpString);
    fTmpString := '';
    WriteLn;
    WriteLn('ID: '   + fGW2API.State.AuthToken.Id);
    WriteLn('Name: ' + fGW2API.State.AuthToken.Name);

    for fString in fGW2API.State.AuthToken.Permissions do
      if fTmpString = '' then
        fTmpString := fString
      else
        fTmpString := fTmpString + ', ' + fString;

    WriteLn('Permissions: ' + fTmpString);

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
