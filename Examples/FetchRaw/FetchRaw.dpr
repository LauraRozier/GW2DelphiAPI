program FetchRaw;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  // System units
  SysUtils,
  // GW2 Delphi API Units
  GW2DA_Defaults in '..\..\src\GW2DA_Defaults.pas',
  GW2DA_Types in '..\..\src\GW2DA_Types.pas',
  GW2DA_Utils in '..\..\src\GW2DA_Utils.pas',
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas',
  GW2DA_Authentication in '..\..\src\Networking\GW2DA_Authentication.pas',
  GW2DA_WebHandlers in '..\..\src\Networking\GW2DA_WebHandlers.pas',
  GW2DA_Misc in '..\..\src\Misc\GW2DA_Misc.pas';

var
  fGW2API:        TGW2API;
  fAPIRequestUrl: string;
  fAPIReply:      string;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    // Create the API object
    FGW2API := TGW2API.Create;

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    // Fetch the server's answer for API v1
    fAPIReply := FGW2API.WebHandler.FetchRawEndpoint(FAPIRequestUrl);
    WriteLn('Raw endpoint response API v1:' + sLineBreak + fAPIReply);

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string again
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    // Fetch the server's answer for API v2
    fAPIReply := FGW2API.WebHandler.FetchRawEndpoint(FAPIRequestUrl);
    WriteLn('Raw endpoint response API v2:' + sLineBreak + fAPIReply);
    // Free the object and clean up
    FreeAndNil(FGW2API);
    WriteLn('Press the Enter key to continue...');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
